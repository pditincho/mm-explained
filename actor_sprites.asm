#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "render_actor.asm"

.label sprite_iter_idx           = $19    // Countdown index for remaining sprites to rank in draw-order pass
.label next_rel_sprite_idx       = $17    // Next available relative draw-order index to assign when filling gaps
.label sprite_in_use_flag_tbl    = $CB72  // Per-sprite usage flags (0=free, 1=used) for allocation and cleanup
.label sprite_owner_actor_idx    = $CB76  // Sprite → actor backreference table (SPRITE_OWNER_UNASSIGNED if none)
.label tmp_sprite_idx            = $404C  // Scratch: holds current sprite index while clearing both banks
.label sprites_in_use_count      = $CB7A  // Current number of sprites allocated to visible actors (0..SPRITE_COUNT_TOTAL)
.label candidate_sprite          = $1D    // Scratch: sprite index of the best draw-order candidate this pass
.label candidate_actor           = $1E    // Scratch: actor index owning the best draw-order candidate sprite
.label candidate_position_y      = $1B    // Scratch: Y coordinate of the current best candidate (smaller = farther/back)

.const SPRITE_OWNER_UNASSIGNED    = $FF    // Sentinel: sprite slot has no owning actor
.const ACTOR_OFFSCREEN            = $00    // Actor is fully outside the viewport
.const ACTOR_ONSCREEN             = $01    // Actor is within the viewport and should be drawn
.const X_ALIGN_ADD3               = $03    // Horizontal bias added before ×8 scaling for proper sprite centering
.const Y_ALIGN_ADD2               = $02    // Vertical bias added after ×2 scaling to align sprite baseline
.const ACTOR_EDGE_REVEAL_TICKS    = $03    // Frames to pre-seed animation counter on edge-based re-entry
.const SIGNED_MINUS_2             = $FE    // Two’s-complement −2 used for Sub < FE fast-path checks

/*
================================================================================
  map_room_to_sprite_coords
================================================================================
Summary
	Convert the current actor’s room-space coordinates into sprite-space
	screen coordinates, producing target X (lo/hi) and Y suitable for hardware
	sprite registers, with camera scroll and alignment biases applied.

Arguments
	actor                   	Global index of the actor to map
							
Global Inputs
	actor                       Current actor index used for all lookups
	actor_pos_x[]               Actor X position in room-column units
	actor_pos_y[]               Actor Y position in room-row units
	viewport_left_col           Leftmost visible room column (camera X)

Global Outputs
	actor_tgt_sprite_x_lo[]     Target sprite X low byte (bits 7-0 of X in VRAM)
	actor_tgt_sprite_x_hi[]     Target sprite X high bit (bit 8 of X in VRAM)
	character_sprite_y[]        Target sprite Y position (pixel units, biased)

Description
	- Refresh the current actor’s visibility state via update_actor_visibility
	so offscreen actors do not cause visual glitches.
	- Compute ΔX := actor_pos_x − viewport_left_col, apply a +3 bias, and
	scale by 8 to convert room columns into screen coordinates.
	- Rotate the 9th bit of the scaled X into actor_tgt_sprite_x_hi to support
	values beyond 255.
	- Store the scaled low byte into actor_tgt_sprite_x_lo and apply a final
	−1 adjustment for fine horizontal alignment.
	- Compute sprite Y as actor_pos_y × 2 + 2, converting rows to pixels and
	applying a small vertical bias, then store into character_sprite_y.

Biases

	* “+3 then *8” aligns the sprite’s X to the pixel grid derived from screen
	  columns, compensating for half-cell visual centering.
	* The extra ROL into the X-high byte preserves the pixel 9th bit so sprites can
	  scroll smoothly past 255 px without snapping.
================================================================================
*/
* = $3015
map_room_to_sprite_coords:
		// ------------------------------------------------------------
		// Update visibility state for the current actor
		// ------------------------------------------------------------
		jsr     update_actor_visibility

		// X := actor index (set by caller)
		ldx     actor
		

		// ------------------------------------------------------------
		// Compute screen X from room X
		//
		// - Calculate room coordinate relative to left edge (factoring horizontal scroll)
		// - Apply horizontal bias
		// - Convert to screen coordinates
		// ------------------------------------------------------------
		// Initialize X-high byte (clear carry-in target)
		lda     #$00
		sta     actor_tgt_sprite_x_hi,x
		
		// Resolve actor's X room coordinate
		lda     actor_pos_x,x                

		// Subtract viewport's left column to get ΔX from left visible column
		sec
		sbc     viewport_left_col            
		
		// Apply bias to align with visible grid
		clc
		adc     #X_ALIGN_ADD3                
		
		// Scale from room coordinates to screen coordinates (* 8)
		asl                                  
		asl                                  
		asl                                  
		
		// Hold low byte in Y
		tay 		
		iny                               
		
		// Move bit8 → sprite X-high (via carry in)
		tya
		rol     actor_tgt_sprite_x_hi,x      

		// Latch X low byte and apply −1 nibble alignment
		// (wrap-safe DEC to fine-tune horizontal placement)
		sta     actor_tgt_sprite_x_lo,x
		dec     actor_tgt_sprite_x_lo,x

		// ------------------------------------------------------------
		// Compute screen Y from room Y
		//
		// - No vertical scroll, so there's no need to calculate relative to top edge
		// - Convert to screen coordinates
		// - Apply vertical bias
		// ------------------------------------------------------------
		// Resolve actor's Y room coordinate
		lda     actor_pos_y,x          
		
		// Scale from room coordinates to screen coordinates (* 2)
		asl                                 
		
		// Apply bias to align with visible grid
		clc
		adc     #Y_ALIGN_ADD2               
		
		// Latch sprite Y coordinate
		sta     character_sprite_y,x
		rts
/*
================================================================================
  hide_all_actors
================================================================================
Summary
	Hides all actors by resetting their visibility flags and then reassigns
	sprites so that any previously visible actors release their sprite slots.

Global Inputs
	actor_visible[]            per-actor visibility flags

Global Outputs
	actor_visible[]            all set to ACTOR_OFFSCREEN
	sprite assignments         refreshed via sync_sprites_with_actors

Description
	- Iterates through all actors, setting their visibility to OFFSCREEN.
	- Calls sync_sprites_with_actors afterward to release any sprites
	that were assigned to actors now offscreen.
================================================================================
*/
* = $3BCE
hide_all_actors:
		// Prepare to set all actors offscreen
		lda     #ACTOR_OFFSCREEN
		ldx     #ACTOR_MAX_INDEX              

clear_visibility:
		// Set actor offscreen
		sta     actor_visible,x               
		dex                                   
		bpl     clear_visibility              

		// Reconcile sprites: free any now-orphaned sprite assignments
		jsr     sync_sprites_with_actors      
		rts                                   
/*
================================================================================
  sync_sprites_with_actors
================================================================================
Summary
	Reconcile sprite ownership with actor visibility, clear unused sprites, 
	and build a stable back-to-front draw order for all	visible actors.

Global Inputs
	actor_visible[]             Per-actor visibility flags
	actor_sprite_index[]        Per-actor sprite index
	sprite_in_use_flag_tbl[]    Per-sprite usage flags
	sprites_in_use_count        Number of sprites currently allocated
	sprite_owner_actor_idx[]    Per-sprite owning actor index 
	costume_for_actor[]         Per-actor costume id (used to special-case the plant)
	actor_pos_y[]               Per-actor Y coordinate (room-space rows)

Global Outputs
	actor_sprite_index[]        Cleared for offscreen actors; filled for visible actors
	sprite_in_use_flag_tbl[]    Updated when sprites are allocated or freed
	sprites_in_use_count        Incremented/decremented as pool usage changes
	sprite_owner_actor_idx[]    Maintained as sprite→actor backreference or unassigned
	relative_sprite_for_actor[] Filled with dense 0..N−1 per-actor draw order

Pass A
	For every actor that is offscreen but still owns a sprite:
		- release that sprite
			- Decrement sprites_in_use_count.
			- Mark the sprite free in sprite_in_use_flag_tbl.
			- Clear the sprite’s graphic rows in the current bank, blit a minimal
			update, toggle sprite_bank_sel, repeat the clear/blit in the other
			bank, then toggle back.
			- Clear actor_sprite_index for that actor so later passes do not see a
			stale mapping.
		
Pass B		
	For every visible actor:
		- ensure it has a sprite
			- If actor already has a sprite, refresh sprite_owner_actor_idx so the
			sprite backreference matches the actor.
			- If actor has no sprite and sprites_in_use_count < SPRITE_COUNT_TOTAL,
			scan for the first free sprite, mark it used, link actor↔sprite, and
			increment sprites_in_use_count.
			- If no free sprite exists, halt
Pass C
	For each sprite marked free:
		- reset sprite_owner_actor_idx to SPRITE_OWNER_UNASSIGNED
		- reset relative_sprite_index as well
		This is done so unused slots cannot	pollute ordering logic.
	
Pass D
	While there are sprites left to order (sprite_iter_idx ≥ 0):
		- Repeatedly choose a “best” candidate and assign a rank
			- Scan all sprites that still have an owner; for each, look up the
			owning actor and its costume.
			- If the costume is COSTUME_ID_PLANT, treat its Y as 0 to force the
			plant to the extreme “back” position (always selected earliest).
			- Otherwise, use actor_pos_y as the comparison key.
			- Keep the smallest Y (topmost/farthest) as the current candidate,
			updating on ties to keep order deterministic.
		- After the scan
			- mark the best sprite’s owner as unassigned
			- store current sprite_iter_idx into relative_sprite_index[sprite]
			- decrement sprite_iter_idx 
		- Repeat scan until all owned sprites are ranked
	
	
Pass E
	Finalize relative_sprite_for_actor so each actor’s sprite has a
	dense back-to-front index:
		- For each actor index, read the corresponding rank (or a sentinel) and
		either copy the precomputed rank or assign the next_rel_sprite_idx
		sequentially for any missing entries.
		- The result is a contiguous 0..N−1 ordering that higher-level drawing
		code can use to render actors from back to front.

Notes
	• Actor-level visibility is the sole driver for allocating/freeing sprites;
	this routine never changes actor_visible[], only reacts to it.
	• The special-case plant costume is always forced to the extreme back of
	the draw order so actors climbing it are drawn “in front” of it.
================================================================================
*/
* = $3ED3
sync_sprites_with_actors:
        // ============================================================
        // Pass A: release sprites assigned to offscreen actors
        //
        // Any actor marked offscreen releases its sprite slot.
        // The sprite is cleared in both sprite buffers and unlinked.
        // ============================================================
        ldx     #$00                              // X := actor index
		
free_sprites_for_offscreen_actors:
		// Actor visible? If so, skip
        lda     actor_visible,x                   
        bne     next_actor_freepass_or_done       

		// Actor has a sprite assigned? If not, skip
        ldy     actor_sprite_index,x              
        cpy     #NO_SPRITE                        
        beq     next_actor_freepass_or_done       

        // ------------------------------------------------------------
		// Actor is not visible and has a sprite assigned
		//
		// -Update sprite use count
		// -Mark sprite as unused
		// -Clear sprite in both sprite banks
		// -Clear actor sprite index
        // ------------------------------------------------------------
		// Decrement total sprites in use
        dec     sprites_in_use_count     
        
		// Mark this sprite as not in use
        lda     #FALSE                              
        sta     sprite_in_use_flag_tbl,y         
        sty     tmp_sprite_idx                   // save current sprite index in temp

		// Clear sprite in this bank and blit changes
		jsr     set_actor_sprite_base             // point to this sprite’s memory base
        ldy     tmp_sprite_idx                    // restore sprite index into Y
        jsr     clear_sprite_visible_rows         // clear visible rows in current bank
        jsr     blit_sprite_vthird                // update partial display (top 1/3)

		// Toggle sprite bank
        lda     #$01                              
        eor     sprite_bank_sel                   
        sta     sprite_bank_sel                   

		// Clear sprite in this bank and blit changes
        ldy     tmp_sprite_idx                    
        jsr     set_actor_sprite_base             
        ldy     tmp_sprite_idx
        jsr     clear_sprite_visible_rows         
        jsr     blit_sprite_vthird                

        // Toggle sprite bank again to go back to the original one
        lda     #$01
        eor     sprite_bank_sel                   
        sta     sprite_bank_sel                   

		// Clear actor’s sprite reference so later passes won’t try to
        // manipulate a stale sprite slot.
        lda     #NO_SPRITE                        
        sta     actor_sprite_index,x              
		
next_actor_freepass_or_done:
		// Next actor
        inx                                       
        cpx     #ACTOR_COUNT_TOTAL                
        bne     free_sprites_for_offscreen_actors 


        // ============================================================
        // Pass B: assign or refresh sprites for visible actors
        //
        // Ensures every visible actor has a sprite assigned.
        // Existing mappings are refreshed, missing ones are allocated.
        // ============================================================
        ldx     #$00                              // restart actor index at 0
		
assign_or_refresh_visible_actor_sprite:
		// Actor visible? If not, skip
        lda     actor_visible,x                   
        beq     next_actor_assignpass_or_done     
		
		// Actor has sprite assigned? If so, skip and refresh ownership
        lda     actor_sprite_index,x              
        cmp     #NO_SPRITE                        
        bne     refresh_sprite_owner_link         

		// Is there a sprite available? If so, continue
        ldy     sprites_in_use_count              
        cpy     #SPRITE_COUNT_TOTAL               
        bcc     assign_available_sprite           

        // ------------------------------------------------------------
        // Sprite pool exhausted → enter debug halt loop
        // ------------------------------------------------------------
        lda     #$0B                              
        sta     debug_error_code                  
        ldy     #MAP_IO_IN                        
        sty     cpu_port                          
debug_halt_no_sprites_left:
        sta     vic_border_color_reg              
        jmp     debug_halt_no_sprites_left        


        // ------------------------------------------------------------
        // Allocate an unused sprite slot for the visible actor
        // ------------------------------------------------------------
assign_available_sprite:
        ldy     #$00                              // start sprite scan Y=0
		
        // ------------------------------------------------------------
        // Scan for a free sprite slot and claim it for actor X
        // ------------------------------------------------------------
find_free_sprite_slot:
		// Sprite in use? If so, skip and check the next one
        lda     sprite_in_use_flag_tbl,y              
        bne     advance_sprite_scan_or_break      

        // ------------------------------------------------------------
		// Sprite available
        // ------------------------------------------------------------
		// Mark sprite in use
        lda     #TRUE                              
        sta     sprite_in_use_flag_tbl,y           

		// Link actor to sprite index
        tya                                       
        sta     actor_sprite_index,x              

		// Link sprite to actor index
        txa                                       
        sta     sprite_owner_actor_idx,y          

		// Update sprite use count
        inc     sprites_in_use_count              
		
		// Break loop
        ldy     #SPRITE_COUNT_TOTAL - 1           
		
advance_sprite_scan_or_break:
		// Next sprite
        iny                                       
        cpy     #SPRITE_COUNT_TOTAL               
        bne     find_free_sprite_slot             
        jmp     next_actor_assignpass_or_done     // next actor


refresh_sprite_owner_link:
        // ------------------------------------------------------------
        // Actor already had a sprite
		// Refresh the ownership link
        // ------------------------------------------------------------
        ldy     actor_sprite_index,x              // Y := sprite index used by actor
        txa                                       // A := actor index
        sta     sprite_owner_actor_idx,y          // refresh backreference
		
next_actor_assignpass_or_done:
		// Next actor
        inx                                       
        cpx     #ACTOR_COUNT_TOTAL
        bne     assign_or_refresh_visible_actor_sprite


        // ============================================================
        // Pass C: clear stale sprite→actor links and relative indices
        //
        // Any sprite not marked “in use” is reset to SPRITE_OWNER_UNASSIGNED
		// so it can’t corrupt ordering calculations later.
        // ============================================================
        lda     sprites_in_use_count              // load count of used sprites
        sta     sprite_iter_idx                   // store count as starting iterator index
		
        ldy     #$00                              // start sprite index at 0
        lda     #SPRITE_OWNER_UNASSIGNED          // prepare the unassigned owner sentinel
clear_unused_sprite_links:
		// Sprite in use? If so, skip
        ldx     sprite_in_use_flag_tbl,y              
        bne     next_sprite_cleanup_or_done       
		
		// Sprite not in use, clear owner actor and relative order slot
        sta     sprite_owner_actor_idx,y          
        sta     relative_sprite_index,y           
		
next_sprite_cleanup_or_done:
		// Next sprite
        iny                                       
        cpy     #SPRITE_COUNT_TOTAL               
        bne     clear_unused_sprite_links         


        // ============================================================
        // Pass D: determine back-to-front draw order using the Y coordinate
        //
        // An actor with a smaller Y room coordinate is farther from the camera
		// (and closer to the back wall) than an actor with a larger coordinate.
        // ============================================================
        dec     sprite_iter_idx                   // prepare countdown index
		
		// Any sprites to order? If not, skip this pass
        lda     sprite_iter_idx
        bpl     select_topmost_actor_for_draw_order 
        jmp     finalize_relative_sprite_indices   

select_topmost_actor_for_draw_order:
        ldx     #$00                              // start scanning actors from 0
        lda     #$FF                              // initial Y candidate (max)
        sta     candidate_position_y              // reset candidate Y tracker
		
        // ------------------------------------------------------------
        // Consider sprite X as a draw-order candidate (if owned)
        // ------------------------------------------------------------
choose_draw_order_candidate:
		// Sprite has an owner? If not, skip
        lda     sprite_owner_actor_idx,x          
        cmp     #SPRITE_OWNER_UNASSIGNED          
        beq     next_sprite_owner_or_done         
		
		// Y := actor index
        tay                                       
		
        // ------------------------------------------------------------
        // Determine if actor belongs to the special case (plant)
        // The man-eating plant is used as a ladder. Actors climbing it will be 
		// always shown closer to the camera. So the plant is always sorted to the back.
        //
        // Load the costume assigned to the actor owning this sprite.
        // If the costume ID matches the plant, the actor is forced to
        // the top. Otherwise, load its vertical Y coordinate for comparison.
        // ------------------------------------------------------------
		// Resolve costume for actor
        lda     costume_for_actor,y               
		
		//Is it the man eating plant? If so, force it to the back
        cmp     #COSTUME_ID_PLANT                 
        beq     force_plant_to_the_back     
		
		// Not the plant, resolve actor's Y coordinate
        lda     actor_pos_y,y                     
        jmp     compare_actor_y_to_candidate       
		
force_plant_to_the_back:
		// It's the plant, force its Y coordinate to 0
        lda     #$00                              
		
        // ------------------------------------------------------------
        // Compare candidate’s Y against current best
        //
        // We keep the smallest Y (topmost/farthest on screen) as the winner.
        // If equal, we still update to ensure deterministic ordering.
        // ------------------------------------------------------------
compare_actor_y_to_candidate:		
        cmp     candidate_position_y              // compare with smallest Y seen
        beq     update_candidate_with_actor       // equal → replace candidate
        bcs     next_sprite_owner_or_done         // greater Y → skip (lower priority)
		
        // ------------------------------------------------------------
        // Update current candidate with this actor
        //
        // The actor under evaluation has a smaller (higher) Y position
        // or ties with the previous best. Store its Y coordinate and
        // remember both the corresponding sprite and actor indices as
        // the new best candidate for this draw-order pass.
        // ------------------------------------------------------------
update_candidate_with_actor:
        sta     candidate_position_y              // store as new best Y
        stx     candidate_sprite                  // record sprite ID
        sty     candidate_actor                   // record actor ID
		
next_sprite_owner_or_done:
		// Next sprite
        inx                                       
        cpx     #SPRITE_COUNT_TOTAL
        bne     choose_draw_order_candidate

        // ------------------------------------------------------------
		// Ordering round finished
		//
        // Consume chosen candidate: mark its sprite as processed
        //
        // After selecting the topmost candidate for this pass, clear
        // its sprite→actor owner entry so it won’t be reconsidered in
        // subsequent iterations of the ordering pass.
        // ------------------------------------------------------------
        ldx     candidate_sprite                  // X := chosen sprite
        lda     #SPRITE_OWNER_UNASSIGNED
        sta     sprite_owner_actor_idx,x          // mark sprite as processed (owner unassigned)
		
        // ------------------------------------------------------------
        // Assign relative draw order to the chosen candidate
        //
        // Map the selected actor’s sprite to the current draw-order slot
        // (sprite_iter_idx), which encodes back-to-front layering.
        // ------------------------------------------------------------
        ldx     candidate_actor                   // X := chosen actor
        ldy     actor_sprite_index,x              // Y := sprite index assigned
        lda     sprite_iter_idx                   // A := current ordering depth
        sta     relative_sprite_index,y           // assign draw order rank
		
		// Decrement ordering iteration counter
        dec     sprite_iter_idx         
		
		// Iterate again until finishing all iterations
        bpl     select_topmost_actor_for_draw_order 


        // ============================================================
        // Pass E: finalize relative sprite indices
        //
        // For all actors, assign a continuous sprite index order.
        // Any missing ones (FF) are filled incrementally.
        // ============================================================
finalize_relative_sprite_indices:
        ldx     #$00                              // reset actor index
        lda     sprites_in_use_count              // load count of used sprites
        sta     next_rel_sprite_idx               // seed for next available order slot
assign_next_relative_index:
        lda     relative_sprite_index,x           // read current order index
        cmp     #SPRITE_OWNER_UNASSIGNED
        bne     use_existing_relative_index       // skip if already valid
		
        // ------------------------------------------------------------
        // Assign or preserve final relative sprite index
        //
        // If the sprite had no assigned draw order (FF), give it the next
        // available sequential index and advance the counter. Otherwise,
        // copy its pre-assigned relative index directly.
        // ------------------------------------------------------------
        lda     next_rel_sprite_idx               // assign next available order
        sta     relative_sprite_for_actor,x
        inc     next_rel_sprite_idx               // advance order counter
        jmp     next_actor_relidx_or_done
use_existing_relative_index:
        sta     relative_sprite_for_actor,x       // copy existing order value
		
next_actor_relidx_or_done:
        inx                                       // next actor index
        cpx     #ACTOR_COUNT_TOTAL
        bne     assign_next_relative_index        // loop until all done
        rts                                       // return to caller
/*
================================================================================
  update_actor_visibility
================================================================================
Summary
    Determine whether the actor mapped to the active costume is onscreen or
    offscreen based on its horizontal position relative to the current viewport.
    Seeds a short reveal counter when the actor enters from either viewport edge.

Global Inputs
    active_costume               Costume index whose actor is being evaluated
    actor                        Actor index corresponding to active_costume
    actor_for_costume[]          Maps costume → actor index (negative = none)
    actor_pos_x[]                Actor X position in room-column units
    viewport_left_col            Viewport left boundary in room columns
    actor_visible[]              Prior visibility state

Global Outputs
    actor_visible[]              Updated visibility flag (onscreen/offscreen)
    actor_anim_counter[]         Seeded to a short countdown on edge entry
    
Description
    • Verifies a valid actor is mapped to active_costume; otherwise marks
      offscreen and exits.
    • Computes Sub := actor_x − viewport_left_col using 8-bit wraparound.
    • Handles three major visibility regions:
        - Sub in {$FE,$FF} hex or (-2, -1) decimal and previously 
			offscreen → left-edge re-entry:
			seed reveal counter and mark visible.
        - Sub == VIEW_COL_MAX_IDX and previously offscreen → right-edge
          re-entry: seed reveal counter and mark visible.
        - Sub within interior region (0..VIEW_COL_MAX_IDX−1) → visible.
        - Sub ≥ VIEW_COL_MAX_IDX+1 → fully right → offscreen.
    • Stores final visibility state to actor_visible[actor].

Notes
    • Routine relies on global ‘actor’ matching the actor mapped from
      active_costume; callers must keep these synchronized.
    • The left-edge test uses an FE/FF band rather than only FF; both values
      trigger edge-entry behavior.
================================================================================
*/
* = $3FF7
update_actor_visibility:
		// Resolve actor for the active costume
		ldx     active_costume                
		lda     actor_for_costume,x           
		bpl     compute_viewport_delta        
		
		// No actor assigned for the costume → return offscreen
		// Note: X here is active_costume, not actor - potential bug when 
		// writing to actor_visible,X later?
		lda     #ACTOR_OFFSCREEN               
		jmp     write_visibility_and_rts      

compute_viewport_delta:
		// Resolve actor's X position
		ldx     actor                         
		lda     actor_pos_x,x                 
		
		// ------------------------------------------------------------
		// Check actor's X position vs viewport's left column
		//
		// Compute Sub := actor_x − viewport_left_col.
		//   Sub < -2        				→ fully left of viewport 
		//										(but will be handled as fully right)
		//   Sub == -1 or -2 				→ left-edge re-entry
		//   Sub in 0..VIEW_COL_MAX_IDX-1   → inside viewport (inclusive edges)
		//   Sub == VIEW_COL_MAX_IDX     	→ right-edge re-entry
		//   Sub ≥ VIEW_COL_MAX_IDX + 1     → fully right of viewport
		// ------------------------------------------------------------
		sec                                   
		sbc     viewport_left_col             // A := Sub

		// ------------------------------------------------------------
		// Fast left-edge check:
		//   if Sub < -2 → definitely not -1, go test offscreen-left vs. inside.
		//   else Sub ∈ {-2, -1} → handle potential edge re-entry.
		// ------------------------------------------------------------
		cmp     #SIGNED_MINUS_2               // Sub ? -2
		bcc     test_offscreen_left           // Sub <= -3 → left/inside path

		// ------------------------------------------------------------
		// Sub == $FE or $FF (actor 2 or 1 columns left of the viewport's left edge)
		// ------------------------------------------------------------
		// Was actor previously offscreen? 
		lda     actor_visible,x               
		bne     test_offscreen_left           // If it was already onscreen → continue

		// ------------------------------------------------------------
		// Actor was offscreen and now onscreen (entering left)
		// ------------------------------------------------------------
		// Seed a short animation counter
		lda     #ACTOR_EDGE_REVEAL_TICKS                          
		sta     actor_anim_counter,x          
		
		// Set actor as onscreen
		lda     #ACTOR_ONSCREEN               
		sta     actor_visible,x               
		rts                                   

test_offscreen_left:
		// ------------------------------------------------------------
		// Fully-left test: Sub < 0 → offscreen left.
		// ------------------------------------------------------------
		cmp     #$00                          // Sub ? 0
		bcc     set_offscreen_and_return      // Sub negative → offscreen

		// ------------------------------------------------------------
		// Right-edge test
		// If Sub == VIEW_COL_MAX_IDX → at right edge.
		// Handle possible edge-entry seeding if coming from offscreen.
		// ------------------------------------------------------------
		cmp     #VIEW_COL_MAX_IDX        
		beq     mark_visible_in_view          // if equal → handle edge entry

test_offscreen_right:
		// ------------------------------------------------------------
		// Fully-right test: Sub ≥ VIEW_COL_MAX_IDX + 1 → offscreen right.
		// ------------------------------------------------------------
		bcs     set_offscreen_and_return_b    // if beyond right edge → offscreen

mark_visible_in_view:
		// ------------------------------------------------------------
		// In-range: 0..VIEW_COL_MAX_IDX. If equal edge, may seed reveal.
		// ------------------------------------------------------------
		bne     set_visible_and_return        // if strictly inside → onscreen

		// ------------------------------------------------------------
		// Equal to right edge: if previously offscreen, seed reveal counter.
		// ------------------------------------------------------------
		// Was actor previously offscreen? 
		lda     actor_visible,x               
		bne     write_visibility_after_edge_enter // already visible → continue
		
		// ------------------------------------------------------------
		// Actor was offscreen and now onscreen (entering right)
		// ------------------------------------------------------------
		// Seed a short animation counter
		lda     #ACTOR_EDGE_REVEAL_TICKS                         
		sta     actor_anim_counter,x          
		
		// Set actor as onscreen
		lda     #ACTOR_ONSCREEN                  

write_visibility_after_edge_enter:
		jmp     write_visibility_and_rts      

set_visible_and_return:
		lda     #ACTOR_ONSCREEN               // A := onscreen
		jmp     write_visibility_and_rts      

set_offscreen_and_return_b:
		lda     #ACTOR_OFFSCREEN              // A := offscreen (right of the viewport)
		jmp     write_visibility_and_rts      

set_offscreen_and_return:
		lda     #ACTOR_OFFSCREEN              // A := offscreen (left of the viewport)

write_visibility_and_rts:
		// Store visibility
		sta     actor_visible,x               
		rts                                   

/*
Pseudo-code

function map_room_to_sprite_coords():
    // 1. Refresh visibility for the current actor
    update_actor_visibility()

    i = actor                          // actor index

    // 2. Compute sprite X from room X and viewport scroll
    //    ΔX = actor_pos_x[i] - viewport_left_col
    //    X_pixels = (ΔX + 3) * 8, with bit 8 stored in a separate hi bit
    //    Then apply a small -1 tweak to the low byte.

    actor_tgt_sprite_x_hi[i] = 0

    room_x = actor_pos_x[i]
    dx = (room_x - viewport_left_col)     // 8-bit wrap semantics in code

    dx_biased = dx + 3                    // X_ALIGN_ADD3
    x_scaled = dx_biased * 8              // low 8 bits in x_scaled, bit 8 in carry

    // In code, low byte is in A/Y, high bit in carry; here we model it explicitly:
    low = x_scaled & 0xFF
    high_bit = (x_scaled >> 8) & 1

    // There is an additional +1/-1 shuffle in the 6502 implementation;
    // net effect is a one-pixel horizontal adjustment of the low byte.
    low = (low - 1) & 0xFF

    actor_tgt_sprite_x_lo[i] = low
    actor_tgt_sprite_x_hi[i] = high_bit

    // 3. Compute sprite Y from room Y
    //    Y_pixels = actor_pos_y[i] * 2 + 2

    room_y = actor_pos_y[i]
    y_pixels = room_y * 2 + 2             // Y_ALIGN_ADD2
    character_sprite_y[i] = y_pixels
end function


function hide_all_actors():
    // Mark all actors as offscreen
    for i in ACTOR_MAX_INDEX down to 0:
        actor_visible[i] = ACTOR_OFFSCREEN

    // Reconcile sprites: free any tied to now-offscreen actors
    sync_sprites_with_actors()
end function


function sync_sprites_with_actors():
    //------------------------------------------------------------------
    // Pass A: free sprites belonging to offscreen actors
    //------------------------------------------------------------------
    for actor_idx in 0 .. ACTOR_COUNT_TOTAL - 1:
        if actor_visible[actor_idx] != ACTOR_OFFSCREEN:
            continue

        sprite_idx = actor_sprite_index[actor_idx]
        if sprite_idx == NO_SPRITE:
            continue

        // Actor is offscreen and owns a sprite → free it
        sprites_in_use_count -= 1
        sprite_in_use_flag_tbl[sprite_idx] = FALSE

        // Clear sprite graphics in both sprite banks
        // Bank 0:
        set_actor_sprite_base(sprite_idx)
        clear_sprite_visible_rows(sprite_idx)
        blit_sprite_vthird(sprite_idx)

        // Toggle to other bank
        sprite_bank_sel = sprite_bank_sel XOR 1

        // Bank 1:
        set_actor_sprite_base(sprite_idx)
        clear_sprite_visible_rows(sprite_idx)
        blit_sprite_vthird(sprite_idx)

        // Toggle back to original bank
        sprite_bank_sel = sprite_bank_sel XOR 1

        // Remove actor→sprite link
        actor_sprite_index[actor_idx] = NO_SPRITE

    //------------------------------------------------------------------
    // Pass B: ensure each visible actor has a sprite (allocate or refresh)
    //------------------------------------------------------------------
    for actor_idx in 0 .. ACTOR_COUNT_TOTAL - 1:
        if actor_visible[actor_idx] == ACTOR_OFFSCREEN:
            continue

        sprite_idx = actor_sprite_index[actor_idx]

        if sprite_idx == NO_SPRITE:
            // Actor needs a sprite; check pool capacity
            if sprites_in_use_count >= SPRITE_COUNT_TOTAL:
				hangup

            // Find first free sprite slot
            for s in 0 .. SPRITE_COUNT_TOTAL - 1:
                if sprite_in_use_flag_tbl[s] == FALSE:
                    sprite_in_use_flag_tbl[s] = TRUE
                    actor_sprite_index[actor_idx] = s
                    sprite_owner_actor_idx[s] = actor_idx
                    sprites_in_use_count += 1
                    break
            // (scan always finds a free sprite if invariants hold)
        else:
            // Actor already has a sprite; refresh sprite→actor backreference
            sprite_owner_actor_idx[sprite_idx] = actor_idx

    //------------------------------------------------------------------
    // Pass C: clear sprite→actor links and ranks for unused sprites
    //------------------------------------------------------------------
    sprite_iter_idx = sprites_in_use_count   // starting count for ordering

    for s in 0 .. SPRITE_COUNT_TOTAL - 1:
        if sprite_in_use_flag_tbl[s] != FALSE:
            continue

        sprite_owner_actor_idx[s] = SPRITE_OWNER_UNASSIGNED
        relative_sprite_index[s] = SPRITE_OWNER_UNASSIGNED

    //------------------------------------------------------------------
    // Pass D: determine back-to-front draw order using Y coordinate
    //
    // Goal: assign an ordering rank to each sprite used by some actor.
    // Smaller Y (higher on screen) → farther back; special-case plant.
    //------------------------------------------------------------------
    sprite_iter_idx -= 1
    if sprite_iter_idx >= 0:
        while sprite_iter_idx >= 0:
            best_y = 0xFF
            best_sprite = SPRITE_OWNER_UNASSIGNED
            best_actor = SPRITE_OWNER_UNASSIGNED

            // Scan all sprites that still have owners
            for s in 0 .. SPRITE_COUNT_TOTAL - 1:
                owner = sprite_owner_actor_idx[s]
                if owner == SPRITE_OWNER_UNASSIGNED:
                    continue

                actor_idx = owner

                // If this actor’s costume is the plant, force Y=0
                if costume_for_actor[actor_idx] == COSTUME_ID_PLANT:
                    candidate_y = 0
                else:
                    candidate_y = actor_pos_y[actor_idx]

                // Keep smallest Y, updating on ties to keep deterministic order
                if candidate_y <= best_y:
                    best_y = candidate_y
                    best_sprite = s
                    best_actor = actor_idx

            // Use the best candidate for this ranking slot
            if best_sprite != SPRITE_OWNER_UNASSIGNED:
                // Mark sprite as consumed so it’s not reconsidered
                sprite_owner_actor_idx[best_sprite] = SPRITE_OWNER_UNASSIGNED

                // Map this actor’s sprite to the current rank
                sprite_idx = actor_sprite_index[best_actor]

                // NOTE: relative_sprite_index is indexed by sprite_idx here.
                relative_sprite_index[sprite_idx] = sprite_iter_idx

            // Next rank
            sprite_iter_idx -= 1
            // Loop until sprite_iter_idx < 0

    //------------------------------------------------------------------
    // Pass E: finalize relative sprite indices per actor
    //
    // As implemented:
    //   - Reads relative_sprite_index using actor index as an index.
    //   - If entry is SPRITE_OWNER_UNASSIGNED, assigns sequential
    //     indices starting at sprites_in_use_count.
    //------------------------------------------------------------------
    next_rel_sprite_idx = sprites_in_use_count

    for actor_idx in 0 .. ACTOR_COUNT_TOTAL - 1:
        rank = relative_sprite_index[actor_idx]   // NOTE: actor_idx is used here

        if rank == SPRITE_OWNER_UNASSIGNED:
            // Assign new sequential rank
            rank = next_rel_sprite_idx
            relative_sprite_for_actor[actor_idx] = rank
            next_rel_sprite_idx += 1
        else:
            // Use existing rank
            relative_sprite_for_actor[actor_idx] = rank

    // done
end function


function update_actor_visibility():
    // 1. Map costume to actor; detect “no actor mapped to this costume”
    costume = active_costume
    mapped_actor = actor_for_costume[costume]

    if mapped_actor < 0:
        // No actor mapped: mark visibility offscreen using costume index as array index
        // (as the code currently does)
        actor_visible[costume] = ACTOR_OFFSCREEN
        return

    // 2. Compute horizontal delta vs viewport
    i = actor                        // active actor index (must correspond to mapped_actor)
    actor_x = actor_pos_x[i]

    // Sub := actor_x - viewport_left_col (8-bit wraparound in code)
    Sub = (actor_x - viewport_left_col) & 0xFF

    // 3. Separate the special left-edge band (Sub == $FE or $FF) from others
    if Sub == 0xFE or Sub == 0xFF:
        // Left-edge band: possible left-edge re-entry

        if actor_visible[i] == ACTOR_OFFSCREEN:
            // Was offscreen, now inside edge band → seed reveal and mark onscreen
            actor_anim_counter[i] = ACTOR_EDGE_REVEAL_TICKS
            actor_visible[i] = ACTOR_ONSCREEN
            return
        // If already onscreen fall through to further tests below
    else:
        // Other values (including Sub in 0..0xFD and < -2 represented as 0x80..0xFD)
        // fall through to general left/right tests.

        // 4. Fully-left vs interior vs right logic

        // Test for Sub < 0 (offscreen left)
        if Sub is negative in signed sense:
            // In the actual 6502 implementation this flows through unsigned compares,
            // but conceptually: Sub < 0 means left of viewport.
            actor_visible[i] = ACTOR_OFFSCREEN
            return

        // Check against right-edge column
        if Sub == VIEW_COL_MAX_IDX:
            // At right edge: handle possible right-edge entry
            goto handle_at_right_edge
        else if Sub > VIEW_COL_MAX_IDX:
            // Fully right of viewport
            actor_visible[i] = ACTOR_OFFSCREEN
            return

        // Inside region: 0 .. VIEW_COL_MAX_IDX - 1
        actor_visible[i] = ACTOR_ONSCREEN
        return

    // 5. Right-edge re-entry handling when Sub == VIEW_COL_MAX_IDX
handle_at_right_edge:
    if Sub == VIEW_COL_MAX_IDX:
        if actor_visible[i] == ACTOR_OFFSCREEN:
            // Was offscreen, now at right edge → seed reveal
            actor_anim_counter[i] = ACTOR_EDGE_REVEAL_TICKS

        // Either way, mark as onscreen
        actor_visible[i] = ACTOR_ONSCREEN
        return

    // (Sub inside interior region, not equal to edge)
    actor_visible[i] = ACTOR_ONSCREEN
end function
*/

