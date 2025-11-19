
/*
================================================================================
  Actor system: lifecycle and frame pipeline
================================================================================

	Keeps the scene’s actors in sync with the camera and with limited
	hardware sprites. It decides who is onscreen, which actor owns which sprite,
	how sprites are layered, and when to redraw.

High-level flow per frame
-------------------------

1) Motion and limbs:
	step_actor_motion_and_limbs
		• For each costume that has an actor and a loaded resource:
			- apply_waypoint_update_if_needed
			- update_actor_motion
			- animate_actor_limbs
		• No drawing here. It only advances state.


2) Visibility + sprite pool:
   refresh_visibility_and_tick_sprites
     • For each costume → compute_actor_visibility to flag
       onscreen vs hidden using horizontal delta vs camera.
     • assign_and_order_sprites reconciles the 4-sprite pool:
		   A) free sprites from hidden actors and clear both buffers
		   B) allocate or refresh sprites for onscreen actors
		   C) clear stale sprite→actor links and order slots
		   D) rank sprites by Y (topmost first; plant costume forced to the top)
		   E) finalize per-actor relative draw order
     • Tick per-actor animation counters. If counter hits 0 and the actor
       is stopped, copy fresh sprite X by calling actor_room_to_sprite_coords.

3) Redraw only what changed:
   redraw_flagged_actors
     • For actors with the “needs refresh” bit:
       - recompute visibility and on-screen coords
       - assign_and_order_sprites to ensure a valid sprite
       - draw_actor if onscreen
       - if stopped, set “needs draw” for a stable follow-up refresh
     • Flip sprite_bank_sel to present the new frame.


Key conversion routine
----------------------

actor_room_to_sprite_coords
	• Recomputes sprite X/Y from room coords + camera:
		X := (actor_x − viewport_left + +3) * 8 with carry into X-hi, then −1
		Y := actor_y * 2 + 2
	• Calls compute_actor_visibility up front so off-screen
	actors do not cause visual glitches.

Visibility rule
---------------

compute_actor_visibility
	• Sub := actor_x − viewport_left_col
		Sub < 0      → off left  → offscreen
		0..SPAN−1    → inside    → onscreen (edge entries may seed counter)
		≥ SPAN       → off right → offscreen
	• Special cases:
		- Sub == $FF and previously offscreen → entering from left → set
		actor_anim_counter := 3 and mark onscreen
		- Right edge equality also seeds the counter if coming from offscreen

Sprite assignment and order
---------------------------

assign_and_order_sprites
	• Runs in five passes to keep “which actor owns which sprite” coherent
	and to build a stable back-to-front draw order. Plant costume wins the
	top slot. Ties on Y are resolved deterministically.

Lifecycle helpers
-----------------

bind_actor_to_costume_and_init
	• Links costume↔actor, snaps spawn to a valid walkbox, ensures the
	costume resource is resident, clears stale pointers, seeds clip/limb
	state, primes motion/counters, resolves current box, and applies the
	initial clip.

assign_costume_to_free_actor
    • Finds a free actor slot and calls the binder above. If no slot is
      free it falls through unchanged.

detach_actor_from_costume
    • Snapshots the actor’s last position into the costume, clears its
      visibility and mouth, breaks the mapping, sets a standing clip from
      facing, and re-runs sprite assignment.

hide_all_actors_and_release_sprites
    • Zeros all actor_visible flags and then reassigns the sprite pool so
      no hidden actor keeps a sprite.

map_dir_mask_to_standing_clip
    • Direction mask → standing clip ID used when motion stops.


Data flow and invariants
------------------------
	• actor_for_costume[] and costume_for_actor[] are the ground truth links.
	• actor_visible[] is only camera-derived
	• actor_sprite_index[] is valid iff the actor is onscreen and capacity
	permits; otherwise it is $FF.
	• sprites_in_use_count and sprite_in_use_flag[] must match after Pass A/B.
	• relative_sprite_index[] and relative_sprite_for_actor[] form a dense 0..N−1
	ordering after Pass E.

Simplified
----------
	1) Move actors and advance animations.
	2) Decide who can be seen and give them one of four sprite “tickets” in
	the correct drawing order.
	3) Redraw only those who asked for it or just became stable.

================================================================================
*/

#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "actor_motion.asm"
#import "pathing.asm"
#import "rsrc_mgmt.asm"

.label sprite_iter_idx           = $19  // Sprite loop index (0..3) when scanning/assigning sprites
.label next_rel_sprite_idx       = $17  // Next free relative sprite order index to assign
.label sprite_in_use_flag        = $CB72  // Per-sprite usage flag table; $00=free, $01=used
.label sprite_owner_actor_idx    = $CB76  // Map: sprite index → owning actor index ($FF if none)
.label tmp_sprite_idx            = $404C  // Scratch storage for current sprite index during buffer ops
.label sprites_in_use_count      = $CB7A  // Count of sprites currently assigned (0..4)
.label candidate_sprite          = $1D  // Working: best sprite candidate for draw-order pass
.label candidate_actor           = $1E  // Working: actor index of current best candidate
.label candidate_position_y      = $1B  // Working: candidate’s Y position (room rows; lower=closer to top)

.label actor_index               = $3043  // Loop index over actors (0..3) in animation pass
.label costume_scan_idx          = $335E  // Loop index over costumes (0..$18) in update pass

.const ACTOR_RENDER_FLAGS_TICK        = ACTOR_RENDER_VISIBLE_AND_REFRESH    // Actor render flags each tick when counter > 0
.const MOTION_MASK_7F            = $7F    // Mask to clear bit7 when testing for MOTION_STOPPED_CODE
.const UNASSIGNED 				 = $FF
.const ANIM_COUNTER_INIT         = $01    // Initial per-actor animation counter value
.const ACTOR_OFFSCREEN           = $00    // Visibility flag: not visible in scene
.const ACTOR_ONSCREEN            = $01    // Visibility flag: visible in scene

// Screen coordinate conversion
.const X_ALIGN_ADD3              = $03    // +3 before *8 to align actor X from columns to pixels
.const Y_ALIGN_ADD2              = $02    // +2 after *2 to bias sprite Y placement

/*
================================================================================
  actor_room_to_sprite_coords
================================================================================

Summary
	Converts an actor’s in-room coordinates into the correct sprite coordinates
	on screen, adjusting for the current camera viewport and tile-to-pixel
	scaling.

Arguments
	.X   				actor index
	actor_x_pos[x]      actor’s room-space X position (in character columns)
	actor_y_pos[x]      actor’s room-space Y position (in row units)
	viewport_left_col   leftmost visible room column

Updates:
	actor_tgt_sprite_x_lo[x]
	actor_tgt_sprite_x_hi[x]
	character_sprite_y[x]

Description
	- Refreshes the actor’s visibility state.
	- Subtracts the visible room offset to obtain on-screen coordinates.
	- Applies X alignment bias (+3) and multiplies by 8 for pixel scaling.
	- Rotates the carry-out into the sprite X high byte (for positions >255 px).
	- Adjusts the X low byte by −1 for subpixel alignment.
	- Doubles Y (row→pixel) and adds +2 bias for correct vertical placement.

Notes
	The alignment constants (+3, −1, +2) are empirical offsets ensuring the
	sprite visually aligns with the actor’s feet in the scene projection.
================================================================================

	This routine takes the current actor’s room coordinates and converts them into
	on-screen sprite coordinates. It first refreshes the actor’s visibility state,
	then computes where the actor’s sprite should be drawn in the viewport.

What it computes

	* Sprite X: It subtracts the room’s visible left column from the actor’s X,
	  adds a small bias (+3) to align to the tile grid, then multiplies by 8 to
	  convert columns to pixels. The 9th bit of this product (the carry out of the
	  shifts) is rotated into the sprite’s X-high byte so wide screens can be
	  represented. The low byte is written to the sprite and then decremented by 1
	  for a one-pixel alignment tweak.
	* Sprite Y: It doubles the actor’s room-row Y (row→pixel scale) and adds a
	  small bias (+2) so the sprite’s baseline sits correctly relative to the room
	  graphics, then stores that as the sprite’s Y.

Why these tweaks exist

	* “+3 then *8” aligns the sprite’s X to the pixel grid derived from character
	  columns, compensating for half-cell visual centering.
	* The extra ROL into the X-high byte preserves the pixel 9th bit so sprites can
	  scroll smoothly past 255 px without snapping.
	* The “−1” on the X low byte and the “+2” on Y are empirical offsets that make
	  the sprite visually sit where the actor’s feet should appear.

Notes

* If the actor is fully off-screen, visibility logic (called up front) prevents
  any visible artifact; the computed coordinates may wrap but won’t be drawn.
================================================================================
*/
* = $3015
actor_room_to_sprite_coords:
		// ------------------------------------------------------------
		// Refresh visibility state for the current actor/sprite
		// ------------------------------------------------------------
		jsr     compute_actor_visibility

		// ------------------------------------------------------------
		// Initialize X-high byte (clear carry-in target)
		// X := actor index (set by caller)
		// ------------------------------------------------------------
		ldx     actor
		lda     #$00
		sta     actor_tgt_sprite_x_hi,x

		// ------------------------------------------------------------
		// Compute screen X from room X:
		//   tmp = (actor_x - viewport_left_col + X_ALIGN_ADD3) << 3
		// Carry from final ASL is rotated into sprite X-high
		// ------------------------------------------------------------
		lda     actor_x_pos,x                // A = actor room X (column units)
		sec
		sbc     viewport_left_col            // A := ΔX from left visible column
		clc
		adc     #X_ALIGN_ADD3                // bias so sprite aligns to tile grid
		asl                                  // *2
		asl                                  // *4
		asl                                  // *8  (pixel-scale)
		tay                                  // hold low byte in Y
		iny                                  // pre-bias to feed ROL carry correctly
		tya
		rol     actor_tgt_sprite_x_hi,x      // bit8 → sprite X-high (carry in)

		// ------------------------------------------------------------
		// Latch X low byte and apply −1 nibble alignment
		// (wrap-safe DEC to fine-tune horizontal placement)
		// ------------------------------------------------------------
		sta     actor_tgt_sprite_x_lo,x
		dec     actor_tgt_sprite_x_lo,x

		// ------------------------------------------------------------
		// Compute screen Y from room Y:
		//   sprite_y = actor_y * 2 + Y_ALIGN_ADD2
		// (room Y uses row units; *2 scales to pixels, +2 biases vertically)
		// ------------------------------------------------------------
		lda     actor_y_pos,x               // A = actor room Y (row units)
		asl                                 // *2 (pixel-scale)
		clc
		adc     #Y_ALIGN_ADD2               // vertical bias for sprite baseline
		sta     character_sprite_y,x
		rts
/*
================================================================================
  refresh_visibility_and_tick_sprites
================================================================================

Summary
	Two-phase per-frame update:
		1) Scan all costumes and refresh which actors are visible; assign/unassign
		sprites to match visibility.
		2) Iterate all actors with sprites; tick animation counters and, when a
		stopped actor’s counter expires, recompute and push its sprite X
		coordinates from the actor’s room position.

Global Inputs
	actor_for_costume[]               costume → actor index (or NO_SPRITE/FF)
	actor_sprite_index[]              actor → sprite index (NO_SPRITE if none)
	actor_anim_counter[]              per-actor countdown to next sprite move
	actor_motion_state[]              per-actor motion bits (MOTION_MASK_7F)
	actor_render_flags[]           	  per-actor render flags

Global Outputs
	active_costume                    used as pass-1 loop index (0..COSTUME_MAX_INDEX)
	actor_index                       used as pass-2 loop index (0..ACTOR_COUNT_TOTAL-1)
	actor_render_flags[]           	  |="ACTOR_RENDER_FLAGS_TICK" when ticking; unchanged on move
	actor_anim_counter[]              decremented when >0; left at 0 when move occurs
	actor_sprite_x_lo/hi[]            updated when a stopped actor’s counter hits 0

Description
	- Pass 1:
		• For each costume with an assigned actor, call compute_actor_visibility.
		• Reconcile sprite ownership with visibility via assign_and_order_sprites.
	- Pass 2:
		• For each actor that has a sprite:
			– If actor_anim_counter == 0 and (actor_motion_state & MOTION_MASK_7F)
			equals MOTION_STOPPED_CODE, call actor_room_to_sprite_coords and
			copy the computed X lo/hi to the assigned sprite.
			– Otherwise decrement actor_anim_counter and OR ACTOR_RENDER_FLAGS_TICK into
			actor_render_flags to request a redraw.

Notes
	- Sprite Y is produced inside actor_room_to_sprite_coords; this routine
	only propagates X lo/hi to the hardware sprite slot when moving.
	- No pathfinding or limb animation occurs here; those are handled elsewhere.
================================================================================
*/
* = $3044
refresh_visibility_and_tick_sprites:
		// ------------------------------------------------------------
		// Pass 1: refresh actor visibility for all active costumes
		// ------------------------------------------------------------
		// Initialize costume loop. We'll scan all costumes to determine
		// which actors are visible within the current viewport.
		// ------------------------------------------------------------
		lda     #$00                          // A := 0
		sta     active_costume                // active_costume = 0

scan_costume_for_visibility:
		ldx     active_costume                // X := current costume index
		lda     actor_for_costume,x           // A := actor index mapped to this costume
		bmi     next_costume_or_done          // if FF (no actor) → skip
		sta     actor                         // actor := current actor index
		jsr     compute_actor_visibility  // update visibility flag for this actor

next_costume_or_done:
		inc     active_costume                // active_costume++
		lda     active_costume                // load next costume index
		cmp     #COSTUME_MAX_INDEX + 1        // reached last costume?
		bne     scan_costume_for_visibility   // if not, loop again

		// ------------------------------------------------------------  
		// Pass 1b: reconcile sprites with actor visibility  
		// ------------------------------------------------------------  
		// Assign sprites to newly visible actors and free any that  
		// belong to hidden actors. Keeps sprite pool and scene sync’d.  
		// ------------------------------------------------------------  
		jsr     assign_and_order_sprites  

		// ------------------------------------------------------------  
		// Pass 2: iterate through all actors (0..ACTOR_COUNT_TOTAL−1)  
		// ------------------------------------------------------------  
		// Each actor’s animation counter is decremented. When it reaches  
		// zero and the actor is stationary, the sprite X position is  
		// updated to match the actor’s coordinates.  
		// ------------------------------------------------------------  
		ldx     #$00                          // X := 0  
		stx     actor_index                   // actor_index = 0  

scan_actor_loop:
		ldx     actor_index                   // X := current actor index
		stx     actor                         // actor := X (used by subroutines)

		// ------------------------------------------------------------  
		// Skip if actor has no assigned sprite (FF sentinel).  
		// ------------------------------------------------------------  
		lda     actor_sprite_index,x          // A := sprite index for actor  
		cmp     #NO_SPRITE                    // compare against FF  
		beq     next_actor_or_done            // if no sprite, skip  

		// ------------------------------------------------------------  
		// Animation counter handling:  
		//   counter = 0 → check if actor stopped (apply move)  
		//   counter > 0 → decrement and mark for refresh  
		// ------------------------------------------------------------  
		lda     actor_anim_counter,x          // A := current counter  
		beq     apply_sprite_move_if_stopped  // if 0, jump to move logic  

		dec     actor_anim_counter,x          // otherwise decrement counter  
		lda     actor_render_flags,x       	  // load animation flags  
		ora     #ACTOR_RENDER_FLAGS_TICK      // make visible and refresh
		sta     actor_render_flags,x       	  // write updated flags  
		jmp     next_actor_or_done            // skip to next actor  


apply_sprite_move_if_stopped:
		// ------------------------------------------------------------
		// Apply new sprite X position only if actor is stationary.
		// (motion_state & $7F == MOTION_STOPPED_CODE)
		// ------------------------------------------------------------
		lda     actor_motion_state,x          // read motion state
		and     #MOTION_MASK_7F               // mask out facing bit
		cmp     #MOTION_STOPPED_CODE          // compare against “stopped” code
		bne     next_actor_or_done            // if moving, skip

		// ------------------------------------------------------------  
		// Convert actor’s room coordinates → sprite coordinates  
		// and update hardware sprite X registers.  
		// ------------------------------------------------------------  
		jsr     actor_room_to_sprite_coords  	 // recalc sprite X/Y for actor  
		lda     actor_tgt_sprite_x_lo,x          // load computed sprite X low  
		ldy     actor_sprite_index,x             // Y := sprite slot  
		sta     actor_sprite_x_lo,y              // update hardware sprite X low  
		lda     actor_tgt_sprite_x_hi,x          // load computed sprite X high  
		sta     actor_sprite_x_hi,y              // update hardware sprite X high  


next_actor_or_done:
		// ------------------------------------------------------------
		// Loop control: move to next actor or exit when finished.
		// ------------------------------------------------------------
		inc     actor_index                   // actor_index++
		lda     actor_index                   // A := current actor index
		cmp     #ACTOR_COUNT_TOTAL            // reached total actor count?
		bne     scan_actor_loop               // if not, repeat loop
		rts                                   // done with update pass
/*
================================================================================
  step_actor_motion_and_limbs
================================================================================

Summary
	Iterate all costumes; for each with an assigned actor and a loaded costume
	resource, update the actor’s waypoint, advance motion, and animate limbs.

Global Inputs
	actor_for_costume[]       costume → actor index (FF if none)
	costume_ptr_hi_tbl[]      nonzero if costume resource is resident

Global Outputs
	actor                     scratch: current actor index
	active_costume            scratch: current costume index
	(side effects) Path/position/motion/limb state updated for each active actor

Description
	- Scan costumes 0..COSTUME_MAX_INDEX.
	- If a costume maps to an actor and its resource is resident:
		• apply_waypoint_update_if_needed
		• update_actor_motion
		• animate_actor_limbs
	- Repeat until all costumes processed; no drawing or sprite assignment here.
================================================================================
*/		
* = $3361
step_actor_motion_and_limbs:
		// ------------------------------------------------------------
		// Initialization
		// ------------------------------------------------------------
		// Start by resetting the costume scan index. The loop will iterate
		// through every costume (0..COSTUME_MAX_INDEX), each potentially tied
		// to one active actor.
		// ------------------------------------------------------------
		lda     #$00                              // A := 0 (reset index)
		sta     costume_scan_idx                  // costume_scan_idx = 0

		// ------------------------------------------------------------  
		// Costume scan loop  
		// ------------------------------------------------------------  
		// Main iteration: for each costume, check if it’s linked to an actor  
		// and whether its resource is loaded. Only then perform per-frame  
		// updates for that actor.  
		// ------------------------------------------------------------  
scan_costume_for_updates:
		ldx     costume_scan_idx                  // X := current costume index
		lda     actor_for_costume,x               // A := actor index assigned to this costume
		bmi     next_costume_or_exit              // if A == $FF → no actor → skip
		sta     actor                             // actor := actor index (cache for routines)
		stx     active_costume                    // active_costume := current costume
		lda     costume_ptr_hi_tbl,x              // check hi-byte of resource pointer
		beq     next_costume_or_exit              // 0 → not loaded → skip actor updates


		// ------------------------------------------------------------  
		// Actor updates  
		// ------------------------------------------------------------  
		// Perform sequential logic for this actor:  
		//  1. Apply or confirm the next movement waypoint.  
		//  2. Update motion state  
		//  3. Trigger limb animation for the current action (walk, etc.).  
		// ------------------------------------------------------------  
		jsr     apply_waypoint_update_if_needed   // handle pending waypoint updates  
		jsr     update_actor_motion               // update position and motion state  
		jsr     animate_actor_limbs               // animate limbs based on motion state  

		// ------------------------------------------------------------  
		// Loop control  
		// ------------------------------------------------------------  
		// Advance the scan index and repeat until all costumes have been  
		// processed. Exits when costume_scan_idx exceeds COSTUME_MAX_INDEX.  
		// ------------------------------------------------------------  
next_costume_or_exit:
		inc     costume_scan_idx                  // costume_scan_idx++
		lda     costume_scan_idx                  // load next costume index
		cmp     #COSTUME_MAX_INDEX + 1            // done with all costumes?
		bne     scan_costume_for_updates          // no → continue loop
		rts                                       // yes → return to caller
/*
================================================================================
  redraw_flagged_actors
================================================================================

Summary
	Iterates all active actors and redraws only those flagged for animation
	refresh. For each such actor, it clears the refresh bit, updates visibility
	and sprite positioning, reassigns sprites if necessary, and calls the draw
	routine if visible. If the actor is stopped, the “needs draw” bit is set to
	ensure a stable pose is redrawn next frame.

Global Inputs
	costume_for_actor[]           actor → assigned costume index (or $FF)
	actor_render_flags[]       	  animation state flags
	actor_motion_state[]          low nibble encodes motion code
	actor_visible[]               current scene visibility state
	sprite_bank_sel               sprite buffer toggle flag

Global Outputs
	actor_render_flags[]       bit0 cleared when refreshed, bit5 set if stopped
	sprite_bank_sel               toggled at end of pass

Description
	- For each actor (0..ACTOR_COUNT_TOTAL−1):
		• Skip if no costume assigned.
		• Skip if “needs refresh” bit not set.
		• Clear the refresh bit.
		• Update viewport visibility, convert actor room coordinates to screen
		sprite coordinates, and reassign sprites.
		• If actor is visible, draw it.
		• If actor is stopped (motion code = MOTION_STOPPED_CODE), set the
		“refresh” bit to mark for further refresh.
	- After scanning all actors, toggle the sprite buffer to display the newly
	drawn frame.

Notes
	This routine does not advance animation timing; it only redraws based on
	state changes or stops. Actual animation counters are updated elsewhere.
================================================================================
*/
* = $338C
redraw_flagged_actors:
		// ------------------------------------------------------------
		// Initialize actor scan (0 .. ACTOR_COUNT_TOTAL-1)
		// ------------------------------------------------------------
		lda     #$00                          // start from actor #0
		sta     actor                         // actor := 0

scan_actor_for_refresh:
		// ------------------------------------------------------------
		// Skip if this actor has no active costume assigned
		// ------------------------------------------------------------
		ldx     actor                         // X := actor index
		lda     costume_for_actor,x           // load costume mapped to actor
		bmi     next_actor_or_exit            // $FF → no costume → skip
		sta     active_costume                // cache costume as “active”


		// ------------------------------------------------------------
		// Refresh gate: proceed only if refresh flag is set
		// ------------------------------------------------------------
		lda     actor_render_flags,x       	  // read animation flags
		and     #ACTOR_RENDER_REFRESH         // test refresh bit
		beq     next_actor_or_exit            // 0 → nothing to do

		// ------------------------------------------------------------
		// Clear the refresh bit before we update/draw
		// ------------------------------------------------------------
		lda     actor_render_flags,x       		// reload flags
		and     #ACTOR_RENDER_CLEAR_REFRESH     // clear refresh flag
		sta     actor_render_flags,x       		// write back

		// ------------------------------------------------------------
		// Bring actor’s screen state up to date
		//   - recompute visibility within viewport
		//   - convert room coords → sprite coords
		//   - reconcile sprite ownership vs. visibility
		// ------------------------------------------------------------
		jsr     compute_actor_visibility   // sets actor_visible
		jsr     actor_room_to_sprite_coords    // updates sprite X/Y targets
		jsr     assign_and_order_sprites           // ensures sprite is bound

		// ------------------------------------------------------------
		// Draw if actor is visible after updates
		// ------------------------------------------------------------
		ldx     actor                         // restore actor index
		lda     actor_visible,x               // 1 → visible, 0 → hidden
		beq     next_actor_or_exit            // hidden → skip draw
		jsr     draw_actor                    // render actor this frame

		// ------------------------------------------------------------
		// If actor is stopped, set visible
		//   (low nibble == MOTION_STOPPED_CODE)
		// ------------------------------------------------------------
		ldx     actor                         // restore actor index
		lda     actor_motion_state,x          // read motion state
		and     #MSK_LOW_NIBBLE               // isolate low nibble
		cmp     #MOTION_STOPPED_CODE          // stopped?
		bne     next_actor_or_exit            // moving → no extra flag
		lda     #ACTOR_RENDER_VISIBLE         // bit mask: visible
		ora     actor_render_flags,x       // set bit
		sta     actor_render_flags,x       // commit


next_actor_or_exit:
		// ------------------------------------------------------------
		// Advance to next actor or exit when all processed
		// ------------------------------------------------------------
		inc     actor                         // actor := actor + 1
		lda     actor                         // A := actor
		cmp     #ACTOR_COUNT_TOTAL            // done all actors?
		bne     scan_actor_for_refresh        // no → loop


		// ------------------------------------------------------------
		// Flip sprite buffer for double buffering
		// ------------------------------------------------------------
		lda     #$01                          // toggle bit
		eor     sprite_bank_sel               // flip bank select
		sta     sprite_bank_sel               // commit
		rts                                   // return
/*
================================================================================
  detach_actor_from_costume
================================================================================

Summary
	Releases the link between an actor and its assigned costume.
	The actor’s last known position is stored in the costume’s destination
	fields, its visibility and mouth states are cleared, the mutual mapping
	tables are reset to $FF, and the costume’s standing animation is updated
	to match the actor’s facing direction. Finally, sprite assignments are
	refreshed to reflect the change.

Arguments
	.X   costume index to unassign

Global Inputs
	actor_for_costume[]           actor index mapped to each costume
	actor_x_pos[], actor_y_pos[]  actor position tables
	path_direction_for_actor[]    current facing direction bitmask

Global Outputs
	costume_dest_x[], costume_dest_y[]  		snapshot of actor’s last position
	actor_visible[], actor_mouth_state[]  		cleared to zero
	actor_for_costume[], costume_for_actor[]  	set to $FF (unlinked)
	costume_clip_set[]  						updated standing clip ID
	sprite assignments  						recalculated by assign_and_order_sprites

Description
	1. Look up the actor currently assigned to costume X.
	2. Copy the actor’s X/Y position into the costume’s destination slots.
	3. Clear visibility and mouth animation (actor no longer shown).
	4. Break the mapping between actor and costume (set both to $FF).
	5. Convert the actor’s direction bitmask into a standing clip ID and
	store it as the costume’s current animation set.
	6. Call assign_and_order_sprites to reconcile sprite ownership.

Notes
	The routine does not free memory or modify actor resources  only logical
	mappings and runtime state. It effectively “parks” the costume for later
	reuse or teleportation.
================================================================================
*/
* = $38E4
detach_actor_from_costume:
		// ------------------------------------------------------------
		// Resolve actor linked to costume
		//
		// Load the actor currently assigned to costume X and transfer
		// it to Y for subsequent operations.
		// ------------------------------------------------------------
		lda     actor_for_costume,x              // A := actor index linked to this costume
		tay                                     // Y := actor index


		// ------------------------------------------------------------  
		// Snapshot actor’s current position into the costume record  
		//  
		// Used so when the costume is re-assigned later, it will reappear  
		// where the actor was last located.  
		// ------------------------------------------------------------  
		lda     actor_x_pos,y                    // A := actor’s X position  
		sta     costume_dest_x,x                 // save as costume’s destination X  
		lda     actor_y_pos,y                    // A := actor’s Y position  
		sta     costume_dest_y,x                 // save as costume’s destination Y  

		// ------------------------------------------------------------  
		// Clear actor visibility and mouth animation  
		//  
		// The actor is about to be released, so ensure it is not visible  
		// in the scene and reset its mouth state to closed.  
		// ------------------------------------------------------------  
		lda     #$00                             // clear value  
		sta     actor_visible,y                  // mark actor as not visible  
		sta     actor_mouth_state,y              // mouth state := closed  

		// ------------------------------------------------------------  
		// Break costume↔actor mapping  
		//  
		// Set both mapping tables to $FF (no link).  
		// This effectively releases the actor and costume.  
		// ------------------------------------------------------------  
		lda     #$FF                             // sentinel for “none”  
		sta     actor_for_costume,x              // unassign actor from costume  
		sta     costume_for_actor,y              // unassign costume from actor  

		// ------------------------------------------------------------  
		// Store standing animation based on actor direction  
		//  
		// Converts the actor’s facing direction into its equivalent  
		// standing clip ID and stores it in the costume entry.  
		// ------------------------------------------------------------  
		lda     path_direction_for_actor,y       // A := actor facing direction mask  
		jsr     map_dir_mask_to_standing_clip  // A := clip ID for that direction  
		sta     costume_clip_set,x               // save as costume’s default clip  

		// ------------------------------------------------------------  
		// Refresh sprite assignments  
		//  
		// Reassign sprite ownership to reflect the change in actor list.  
		// ------------------------------------------------------------  
		jsr     assign_and_order_sprites  
		rts                                     // return to caller

/*
================================================================================
  assign_costume_to_free_actor
================================================================================

Summary
	Searches for an unassigned actor slot and initializes it for the currently
	active costume. If no actor slot is free, the routine falls through without
	performing any assignment. This is an implementation bug.

Global Inputs
	current_room               current active room ID
	costume_for_actor[]        table mapping actors to assigned costumes

Global Outputs
	actor_for_costume[]        updated when a free actor is found
	costume_for_actor[]        updated with the new costume–actor mapping
	actor state tables         initialized by bind_actor_to_costume_and_init

Description
	- If current_room = 0, no valid room context exists, so the routine exits.
	- Otherwise, scan all actors (0..ACTOR_COUNT_TOTAL−1):
		• If costume_for_actor[Y] < 0 (FF), the slot is free.
		• Call bind_actor_to_costume_and_init to configure it for the
		currently active costume, then exit.
		• If all actor slots are occupied, return without assigning.

Notes
	This routine is used when a costume is entering the current room.
	It ensures that visible costumes have a corresponding actor in memory.
	If no free actor is found, the costume remains inactive.
================================================================================
*/
* = $3911
assign_costume_to_free_actor:
		// ------------------------------------------------------------
		// Entry point: verify current room validity
		//
		// If current_room = 0, no valid scene is loaded, so exit early.
		// Otherwise, begin scanning for a free actor slot to assign.
		// ------------------------------------------------------------
		lda     current_room                    // A := current room ID
		bne     begin_scan_free_actor           // skip ahead if valid room
		rts                                     // no room → return immediately

begin_scan_free_actor:
		// ------------------------------------------------------------
		// Initialize scan for available actor slots (0..ACTOR_COUNT_TOTAL−1)
		//
		// Search for an actor that currently has no assigned costume.
		// When found, initialize it for the current active costume.
		// ------------------------------------------------------------
		ldy     #$00                            // Y := 0 (start with actor #0)

find_free_actor_slot:
		lda     costume_for_actor,y             // A := costume index assigned to actor Y
		bpl     advance_actor_scan_or_exit      // if >=0, actor is occupied → continue scan


		// ------------------------------------------------------------  
		// Found a free actor slot  initialize and assign it  
		//  
		// Calls bind_actor_to_costume_and_init to set up actor state,  
		// coordinates, and links to the active costume.  
		// ------------------------------------------------------------  
		jsr     bind_actor_to_costume_and_init    // prepare actor Y for current costume  
		rts                                     // exit after assigning one actor  


advance_actor_scan_or_exit:
		// ------------------------------------------------------------
		// Advance to next actor slot or terminate when done
		//
		// Increments Y to test next actor index; stops when all
		// actors have been scanned without finding a free one.
		// ------------------------------------------------------------
		iny                                     // Y := Y + 1
		cpy     #ACTOR_COUNT_TOTAL              // reached actor limit?
		bne     find_free_actor_slot            // no → continue scan


		// ------------------------------------------------------------  
		// Fallback: no available actor found - BUG (it shouldn't fall through)
		//  
		// The routine intentionally falls through here without  
		// taking further action. 
		// ------------------------------------------------------------  
/*
================================================================================
  bind_actor_to_costume_and_init
================================================================================

Summary
	Bind an actor to a costume, snap its starting coordinates to a valid
	walkbox, seed path/box state, ensure the costume resource is loaded, and
	initialize animation/limb metadata before applying the initial clip.

Arguments
	.X  Costume index
	.Y  Actor index

Returns
	None

Global Inputs
	costume_dest_x[], costume_dest_y[]     desired spawn coords for costume
	current_room                           room id for walkbox context
	nearest_box_index                      box chosen by prior proximity pass

Global Outputs
	actor_for_costume[], costume_for_actor[]    bidirectional mapping
	test_x, test_y                               snap inputs (temporary)
	walkbox_room                                 snap context
	actor_x_pos[], actor_y_pos[]                 actor current position
	actor_x_dest[], actor_y_dest[]               actor destination
	actor_active_wpt_x[], actor_active_wpt_y[]   active waypoint
	costume_dest_x[], costume_dest_y[]           snapped costume coords
	actor_mouth_state[]                          reset to closed
	active_costume_rsrc_lo/hi[]					 cleared pointers
	actor_current_clip[], actor_target_clip[]    set to $FF (none)
	limb_current_cel_seq[], limb_target_cel_seq[]  set to $FF per limb
	actor_motion_state[]                         set to stopped ($02)
	limb_tgt_loop_cnt[]                          cleared
	actor_anim_counter[]                         set to $01
	actor_search_depth[]                         set to $FF
	actor_path_update_flag[]                     set to $01
	actor_box_cur[], actor_box_prev[], actor_destination_box[]  seeded
	target_clip_id, clip_loop_cnt                initial clip + loop count

Description
	- Link costume↔actor in both tables.
	- Copy costume_dest_x/y and current_room into snap inputs and snap to a
	valid walkbox; write snapped coords back to costume and actor state.
	- Reset mouth, ensure costume resource is resident, clear stale resource
	pointers, and setup costume graphics metadata.
	- Initialize clip ids and all limb base cel sequences to $FF.
	- Seed baseline motion/counters and path/box tracking fields.
	- Load walkbox tables and resolve the actor’s current box.
	- Apply the initial clip with a one-shot loop to establish pose.
================================================================================
*/
* = $3927
bind_actor_to_costume_and_init:
		// ------------------------------------------------------------
		// Bind actor and costume together
		//
		// Establish bidirectional mapping between the actor and costume.
		// X = costume index, Y = actor index.
		// ------------------------------------------------------------
		txa                                     // A := costume index
		sta     costume_for_actor,y              // link costume → actor
		sty     actor                            // actor := Y (cache for later)
		tya                                     // A := actor index
		sta     actor_for_costume,x              // link actor → costume
		stx     active_costume                   // active_costume := X


		// ------------------------------------------------------------  
		// Prepare snap input coordinates  
		//  
		// Copy the costume’s destination coordinates and current room  
		// into the walkbox snapping inputs (test_x/test_y).  
		// ------------------------------------------------------------  
		lda     costume_dest_x,x                 // A := destination X  
		sta     test_x                           // store as snap X input  
		lda     costume_dest_y,x                 // A := destination Y  
		sta     test_y                           // store as snap Y input  
		lda     current_room                     // A := current room index  
		sta     walkbox_room                     // define walkbox context  

		// ------------------------------------------------------------  
		// Snap coordinates to nearest valid walkbox  
		//  
		// Aligns input coordinates to a valid walkable area.  
		// Restores X and Y afterward to resume proper table indexing.  
		// ------------------------------------------------------------  
		jsr     snap_coords_to_walkbox           // clamp to walkbox limits  
		ldx     active_costume                   // restore costume index  
		ldy     actor                            // restore actor index  

		// ------------------------------------------------------------  
		// Write snapped coordinates to costume and actor state tables  
		//  
		// Synchronizes costume and actor position, destination,  
		// and active waypoint data.  
		// ------------------------------------------------------------  
		lda     test_x                           // A := snapped X  
		sta     costume_dest_x,x                 // update costume X  
		sta     actor_x_pos,y                    // update actor current X  
		sta     actor_x_dest,y                   // update actor destination X  
		sta     actor_active_wpt_x,y             // update actor waypoint X  
		lda     test_y                           // A := snapped Y  
		sta     costume_dest_y,x                 // update costume Y  
		sta     actor_y_pos,y                    // update actor current Y  
		sta     actor_y_dest,y                   // update actor destination Y  
		sta     actor_active_wpt_y,y             // update actor waypoint Y  

		// ------------------------------------------------------------  
		// Reset mouth animation state  
		// ------------------------------------------------------------  
		lda     #$00                             // closed mouth  
		sta     actor_mouth_state,y              // clear mouth state  

		// ------------------------------------------------------------  
		// Ensure costume resource is loaded  
		//  
		// Loads the graphics and metadata for this costume.  
		// Clears any stale resource pointer afterward.  
		// ------------------------------------------------------------  
		jsr     rsrc_cache_costume     // ensure loaded  
		ldx     active_costume                   // restore costume index  
		lda     #$00  
		sta     active_costume_rsrc_lo,x         // clear low byte of ptr  
		lda     #$00  
		sta     active_costume_rsrc_hi,x         // clear high byte of ptr  

		// ------------------------------------------------------------  
		// Setup costume rendering and animation metadata  
		//  
		// Initializes cel and limb data structures for rendering.  
		// ------------------------------------------------------------  
		jsr     setup_costume_for_actor          // parse tables for actor  

		// ------------------------------------------------------------  
		// Initialize clip IDs and limb state  
		//  
		// Sets actor’s current/target clip to FF (none) and each limb’s  
		// base cel sequence to FF to indicate uninitialized.  
		// ------------------------------------------------------------  
		ldy     actor                            // Y := actor index  
		lda     #$FF                             // sentinel for none  
		sta     actor_current_clip,y             // clear current clip  
		sta     actor_target_clip,y              // clear target clip  

		lda     actor                            // A := actor index  
		asl                                     // ×2  
		asl                                     // ×4  
		asl                                     // ×8 → offset into limb table  
		tay                                     // Y := limb base offset  
		ldx     #MAX_LIMB_IDX                    // X := limb counter  

init_each_limb_base_cel:
		lda     #$FF                             // uninitialized limb state
		sta     limb_current_cel_seq,y           // clear current cel seq
		sta     limb_target_cel_seq,y            // clear target cel seq
		iny                                     // next limb slot
		dex                                     // decrement limb counter
		bpl     init_each_limb_base_cel          // loop until all cleared


		// ------------------------------------------------------------  
		// Restore actor/costume indices  
		// ------------------------------------------------------------  
		ldy     actor                            // Y := actor index  
		ldx     active_costume                   // X := costume index  

		// ------------------------------------------------------------  
		// Initialize baseline motion and counters  
		//  
		// Defines initial animation timing and idle state.  
		// ------------------------------------------------------------  
		lda     #$02                             // motion: stopped  
		sta     actor_motion_state,y             // set stopped motion  
		lda     #$00  
		sta     limb_tgt_loop_cnt,y              // clear limb loop counter  
		lda     #$01  
		sta     actor_anim_counter,y             // reset animation timer  

		// ------------------------------------------------------------  
		// Initialize pathfinding and walkbox tracking  
		//  
		// Seeds the actor’s box references for spatial logic.  
		// ------------------------------------------------------------  
		lda     #$FF  
		sta     actor_search_depth,y             // no depth (uninitialized)  
		lda     #$01  
		sta     actor_path_update_flag,y         // flag: update path needed  
		lda     nearest_box_index                // current valid walkbox index  
		sta     actor_box_cur,y                  // set current box  
		sta     actor_box_prev,y                 // set previous box  
		sta     actor_destination_box,y          // set destination box  

		// ------------------------------------------------------------  
		// Load walkbox data for this costume  
		//  
		// Retrieves collision/movement boundaries and determines  
		// the actor’s box placement.  
		// ------------------------------------------------------------  
		ldx     actor                            // X := actor index  
		jsr     get_walkboxes_for_costume        // load walkbox tables  
		jsr     begin_box_scan                   // resolve current box  

		// ------------------------------------------------------------  
		// Apply initial animation clip  
		//  
		// Activates the standing/walking animation assigned to  
		// the costume’s default clip set.  
		// ------------------------------------------------------------  
		ldx     active_costume                   // restore costume index  
		lda     costume_clip_set,x               // A := default clip id  
		sta     target_clip_id                   // set target clip  
		lda     #$01                             // one loop  
		sta     clip_loop_cnt                    // init loop counter  
		jsr     apply_clip_set                   // apply animation  
		rts                                     // done initializing actor
/*
================================================================================
  hide_all_actors_and_release_sprites
================================================================================

Summary
	Hides all actors by resetting their visibility flags and then reassigns
	sprites so that any previously visible actors release their sprite slots.

Global Inputs
	actor_visible[]            per-actor visibility flags
	ACTOR_MAX_INDEX            maximum actor index in current scene

Global Outputs
	actor_visible[]            cleared (all set to 0)
	sprite assignments         refreshed via assign_and_order_sprites

Description
	- Iterates through all actors, clearing their visibility flag to zero
	(not visible).
	- Calls assign_and_order_sprites afterward to release any sprites
	that were assigned to actors now hidden.
	- Used when switching rooms or performing a global redraw reset.

Notes
	This routine does not modify actor positions or state beyond visibility.
	It purely resets the render list before a fresh assignment cycle.
================================================================================
*/
* = $3BCE
hide_all_actors_and_release_sprites:
		// ------------------------------------------------------------
		// Initialize: prepare to clear visibility for all actors
		// ------------------------------------------------------------
		lda     #$00                          // A := 0 (not visible flag)
		ldx     #ACTOR_MAX_INDEX              // X := highest actor index

clear_visibility:
		// ------------------------------------------------------------
		// Loop: clear per-actor visibility from X down to 0
		// ------------------------------------------------------------
		sta     actor_visible,x               // actor_visible[X] := 0 (hidden)
		dex                                   // X := X - 1
		bpl     clear_visibility              // repeat while X >= 0


		// ------------------------------------------------------------
		// Reconcile sprites: free any now-orphaned sprite assignments
		// ------------------------------------------------------------
		jsr     assign_and_order_sprites      // release sprites from hidden actors
		rts                                   // done
/*
================================================================================
  map_dir_mask_to_standing_clip
================================================================================

Summary
	Translates an actor’s directional bitmask (as used in pathfinding and
	movement) into the corresponding standing clip set index used by the
	costume animation system.

Arguments
	A  		Direction bitmask
			$00 : facing left
			$01 : facing right
			$80 : facing down
			$81 : facing up

Returns
	A  		Animation clip set index
			CLIP_SET_LEFT, CLIP_SET_RIGHT, CLIP_SET_DOWN, or CLIP_SET_UP

Description
	This lookup routine maps the low/high bits of the actor’s path direction
	mask into a discrete animation set constant. It is used when transitioning
	from motion-based states back to idle/standing poses so that the costume
	faces the correct direction after movement stops.

Notes
	- Bit7 of the direction mask distinguishes vertical (down/up) vs.
	horizontal (left/right).
	- Default case (unmatched code) maps to “down” standing animation.
================================================================================
*/
* = $3BDF
map_dir_mask_to_standing_clip:
		cmp     #DIR_RIGHT_MASK
		bne     check_dir_left_3
		lda     #CLIP_SET_RIGHT
		jmp     return_clip_set
check_dir_left_3:
		cmp     #DIR_LEFT_MASK
		bne     check_dir_up_3
		lda     #CLIP_SET_LEFT
		jmp     return_clip_set
check_dir_up_3:
		cmp     #DIR_UP_MASK
		bne     use_dir_down_default
		lda     #CLIP_SET_UP
		jmp     return_clip_set
use_dir_down_default:
		lda     #CLIP_SET_DOWN
return_clip_set:
		rts
/*
================================================================================
  assign_and_order_sprites
================================================================================

Summary
	Reconciles sprite ownership with actor visibility, then establishes
	a stable back-to-front draw order. Runs in five passes:
		A) Free sprites from offscreen actors and clear both sprite buffers
		B) Ensure each visible actor owns a sprite (allocate or refresh link)
		C) Clear stale sprite→actor links and uninitialized order slots
		D) Build draw order by selecting topmost (smallest Y) candidates,
		forcing the plant costume to the top, until all ranks are filled
		E) Finalize relative indices, filling any remaining gaps sequentially

Global Inputs
	actor_visible[]                  per-actor visibility flags
	actor_sprite_index[]             sprite index per actor (NO_SPRITE=$FF)
	costume_for_actor[]              costume id per actor (plant special-case)
	actor_y_pos[]                    actor Y positions (screen space)
	sprite_in_use_flag[]             sprite slot usage flags (0/1)
	sprites_in_use_count             number of sprites currently allocated
	sprite_bank_sel                  active sprite VRAM bank selector

Global Outputs
	actor_sprite_index[]             updated assignments for visible actors
	sprite_in_use_flag[]             flags updated when allocating/freeing
	sprite_owner_actor_idx[]         back-references sprite→actor (or $FF)
	sprites_in_use_count             incremented/decremented as needed
	relative_sprite_index[]          per-sprite draw rank (temporary)
	relative_sprite_for_actor[]      final per-actor draw order index
	sprite_bank_sel                  toggled during buffer clears

Description
	• Pass A: For each offscreen actor with a sprite, decrement usage count,
	mark the sprite free, clear its graphics in both banks (set_actor_sprite_base,
	clear_sprite_visible_rows, blit_sprite_vthird), and unlink from actor.
	• Pass B: For each visible actor, if no sprite and capacity permits,
	allocate the first free sprite and link both directions; otherwise,
	refresh sprite→actor ownership. If pool exhausted, enter debug halt.
	• Pass C: For any sprite not in use, reset owner and relative order to $FF.
	• Pass D: Repeatedly choose the topmost (smallest Y) owned sprite as the next
	draw rank; the plant costume is forced to the top. Mark chosen sprite as
	processed ($FF) and store its rank in relative_sprite_index.
	• Pass E: For all actors, copy existing ranks or assign sequential ones to
	fill gaps, producing continuous relative_sprite_for_actor indexes.

Notes
	• Uses a stable tie-break (equal Y updates candidate) for deterministic order.
	• The debug halt writes error code ($0B) to VIC border with I/O mapped on.
================================================================================
*/
* = $3ED3
assign_and_order_sprites:
        // ============================================================
        // Pass A: release sprites assigned to offscreen actors
        //
        // Any actor marked offscreen releases its sprite slot.
        // The sprite is cleared in both sprite buffers and unlinked.
        // ============================================================
        ldx     #$00                              // X := actor index = 0
		
		// ------------------------------------------------------------
        // Check if actor’s sprite should be released
        //
        // If the actor is offscreen and currently has a sprite assigned,
        // prepare to free that sprite and clear its graphics buffers.
        // Otherwise, skip to the next actor.
        // ------------------------------------------------------------
free_sprites_for_offscreen_actors:
        lda     actor_visible,x                   // load visibility flag for actor X
        bne     next_actor_freepass_or_done       // skip release if actor still visible
		
        ldy     actor_sprite_index,x              // Y := sprite index assigned to this actor
        cpy     #NO_SPRITE                        // compare with sentinel “no sprite”
        beq     next_actor_freepass_or_done       // skip if none assigned

        // ------------------------------------------------------------
        // Release the sprite slot and mark it as free
        //
        // Decrement the active sprite counter, clear the sprite’s
        // “in use” flag, and store its index for subsequent clearing
        // operations in both sprite buffers.
        // ------------------------------------------------------------
        dec     sprites_in_use_count              // decrement total sprites in use
        lda     #$00                              // value 0 = sprite free
        sta     sprite_in_use_flag,y              // mark this sprite as not in use
        sty     tmp_sprite_idx                    // save current sprite index in temp

		// ------------------------------------------------------------
        // Clear sprite graphics in the current buffer
        //
        // Set the sprite’s memory base, then erase its visible rows and
        // push a minimal (top-third) blit to ensure the cleared state
        // is reflected on screen for this bank.
        // ------------------------------------------------------------
		jsr     set_actor_sprite_base             // point to this sprite’s memory base
        ldy     tmp_sprite_idx                    // restore sprite index into Y
        jsr     clear_sprite_visible_rows         // clear visible rows in current bank
        jsr     blit_sprite_vthird                // update partial display (top 1/3)

        // ------------------------------------------------------------
        // Toggle to the other sprite buffer and clear it too
        //
        // Flip the double-buffer selector, reselect the sprite’s VRAM
        // base for the new bank, then erase its visible rows and push
        // a minimal blit so the cleared state is reflected on-screen.
        // ------------------------------------------------------------
        lda     #$01                              // constant used for buffer toggle
        eor     sprite_bank_sel                   // flip sprite bank selector bit
        sta     sprite_bank_sel                   // commit new bank selection
        ldy     tmp_sprite_idx                    // restore sprite index again
        jsr     set_actor_sprite_base             // select sprite base in other bank
        ldy     tmp_sprite_idx
        jsr     clear_sprite_visible_rows         // clear visible rows in other buffer
        jsr     blit_sprite_vthird                // blit cleared data to display

        // ------------------------------------------------------------
        // Restore original sprite buffer
        //
        // Flip the bank selector again to return to the initial buffer.
        // ------------------------------------------------------------
        lda     #$01
        eor     sprite_bank_sel                   // toggle back to original bank
        sta     sprite_bank_sel                   // restore bank selector

        // ------------------------------------------------------------
        // Unlink sprite from actor
        //
        // Mark “no sprite” for this actor so later passes won’t try to
        // manipulate a stale sprite slot.
        // ------------------------------------------------------------
        lda     #NO_SPRITE                        // sentinel = unassigned
        sta     actor_sprite_index,x              // clear actor’s sprite reference
next_actor_freepass_or_done:
        inx                                       // increment actor index
        cpx     #ACTOR_COUNT_TOTAL                // done all 4 actors?
        bne     free_sprites_for_offscreen_actors // if not, continue scanning


        // ============================================================
        // Pass B: assign or refresh sprites for visible actors
        //
        // Ensures every visible actor has a sprite assigned.
        // Existing mappings are refreshed, missing ones are allocated.
        // ============================================================
        ldx     #$00                              // restart actor index at 0
		
        // ------------------------------------------------------------
        // Assign or refresh sprite for a visible actor
        //
        // Visible actors must own a valid sprite slot. If one is already
        // assigned, refresh the sprite→actor backreference; otherwise,
        // proceed to allocation.
        // ------------------------------------------------------------
assign_or_refresh_visible_actor_sprite:
        lda     actor_visible,x                   // load visibility flag for actor X
        beq     next_actor_assignpass_or_done     // skip if offscreen
        lda     actor_sprite_index,x              // load sprite index for this actor
        cmp     #NO_SPRITE                        // check if already has sprite
        bne     refresh_sprite_owner_link         // yes → just refresh ownership

        // ------------------------------------------------------------
        // Capacity check before allocating a new sprite
        //
        // If the number of sprites currently in use is below the total
        // available, we can allocate a new slot to this actor.
        // ------------------------------------------------------------
        ldy     sprites_in_use_count              // Y := count of sprites currently assigned
        cpy     #SPRITE_COUNT_TOTAL               // compare against max sprite capacity
        bcc     assign_available_sprite           // if in-use < max → go allocate a sprite

        // ------------------------------------------------------------
        // Sprite pool exhausted → enter debug halt loop
        //
        // Emit an error code, map in I/O, and spin forever while
        // writing the code to the border color for visual diagnosis.
        // ------------------------------------------------------------
        lda     #$0B                              // debug code: 0B = out of sprites
        sta     debug_error_code                  // record error for inspection
        ldy     #MAP_IO_IN                        // Y := $25 → enable I/O map
        sty     cpu_port                          // map in hardware registers
debug_halt_no_sprites_left:
        sta     vic_border_color_reg                  // flash border with code
        jmp     debug_halt_no_sprites_left        // infinite loop (halt)


        // ------------------------------------------------------------
        // Allocate an unused sprite slot for the visible actor
        // ------------------------------------------------------------
assign_available_sprite:
        ldy     #$00                              // start sprite scan Y=0
		
        // ------------------------------------------------------------
        // Scan for a free sprite slot and claim it for actor X
        // ------------------------------------------------------------
find_free_sprite_slot:
        lda     sprite_in_use_flag,y              // A := usage flag for sprite Y (0=free,1=used)
        bne     advance_sprite_scan_or_break      // if used, advance to next Y

        lda     #$01                              // A := 1 (mark as used)
        sta     sprite_in_use_flag,y              // sprite Y → now reserved

        tya                                       // A := Y (sprite index)
        sta     actor_sprite_index,x              // actor X → remembers its sprite index

        txa                                       // A := X (actor index)
        sta     sprite_owner_actor_idx,y          // sprite Y → remembers owning actor
		
        inc     sprites_in_use_count              // one more sprite is now allocated (in-use++)
        ldy     #SPRITE_COUNT_TOTAL - 1           // load break sentinel so Y scan loop terminates
		
advance_sprite_scan_or_break:
        iny                                       // Y++
        cpy     #SPRITE_COUNT_TOTAL               // reached 4 sprites?
        bne     find_free_sprite_slot             // if not, continue scanning
        jmp     next_actor_assignpass_or_done     // next actor


        // ------------------------------------------------------------
        // Refresh ownership link for actors already holding sprites
        // ------------------------------------------------------------
refresh_sprite_owner_link:
        ldy     actor_sprite_index,x              // Y := sprite index used by actor
        txa                                       // A := actor index
        sta     sprite_owner_actor_idx,y          // refresh backreference
next_actor_assignpass_or_done:
        inx                                       // move to next actor
        cpx     #ACTOR_COUNT_TOTAL
        bne     assign_or_refresh_visible_actor_sprite


        // ============================================================
        // Pass C: clear stale sprite→actor links and relative indices
        //
        // Any sprite not marked “in use” is reset to FF so it can’t
        // corrupt ordering calculations later.
        // ============================================================
        lda     sprites_in_use_count              // load count of used sprites
        sta     sprite_iter_idx                   // store as iterator
        ldy     #$00                              // start sprite index at 0
        lda     #UNASSIGNED                       // prepare reset byte
clear_unused_sprite_links:
        ldx     sprite_in_use_flag,y              // X := usage flag for sprite Y
        bne     next_sprite_cleanup_or_done       // skip if sprite still in use
        sta     sprite_owner_actor_idx,y          // clear owner actor ID
        sta     relative_sprite_index,y           // clear relative order slot
next_sprite_cleanup_or_done:
        iny                                       // Y++
        cpy     #SPRITE_COUNT_TOTAL               // processed all 4 sprites?
        bne     clear_unused_sprite_links         // if not, continue


        // ============================================================
        // Pass D: determine draw order by actor Y coordinate
        //
        // The actor lowest on screen (largest Y) must be drawn last.
        // The “plant” actor is forced to always be drawn first.
        // ============================================================
        dec     sprite_iter_idx                   // prepare countdown index
        lda     sprite_iter_idx
        bpl     select_topmost_actor_for_draw_order // skip exit if >=0
        jmp     finalize_relative_sprite_indices   // none left → done

select_topmost_actor_for_draw_order:
        ldx     #$00                              // start scanning actors from 0
        lda     #$FF                              // initial Y candidate (max)
        sta     candidate_position_y              // reset candidate Y tracker
		
        // ------------------------------------------------------------
        // Consider sprite X as a draw-order candidate (if owned)
        // ------------------------------------------------------------
choose_draw_order_candidate:
        lda     sprite_owner_actor_idx,x          // load actor ID for sprite X
        cmp     #UNASSIGNED                       // FF = unused slot
        beq     next_sprite_owner_or_done         // skip if unused
        tay                                       // Y := actor index
		
        // ------------------------------------------------------------
        // Determine if actor belongs to special case (plant) or normal
        //
        // Load the costume assigned to the actor owning this sprite.
        // If the costume ID matches the plant, the actor is forced to
        // the top. Otherwise, load its vertical Y coordinate for comparison.
        // ------------------------------------------------------------
        lda     costume_for_actor,y               // load costume assigned
        cmp     #COSTUME_ID_PLANT                 // is this the plant actor?
        beq     candidate_is_forced_top_plant      // yes → force topmost
        lda     actor_y_pos,y                     // load Y position of actor
        jmp     compare_actor_y_to_candidate       // compare to current candidate
candidate_is_forced_top_plant:
        lda     #$00                              // treat as top (Y=0)
		
        // ------------------------------------------------------------
        // Compare candidate’s Y against current best
        //
        // We keep the smallest Y (topmost on screen) as the winner.
        // If equal, we still update to ensure deterministic ordering.
        // ------------------------------------------------------------
compare_actor_y_to_candidate:
        cmp     candidate_position_y              // compare with smallest Y seen
        beq     update_candidate_with_actor        // equal → replace candidate
        bcs     next_sprite_owner_or_done          // greater Y → skip (lower priority)
		
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
        inx                                       // next sprite
        cpx     #SPRITE_COUNT_TOTAL
        bne     choose_draw_order_candidate

        // ------------------------------------------------------------
        // Consume chosen candidate: mark its sprite as processed
        //
        // After selecting the topmost candidate for this pass, clear
        // its sprite→actor owner entry so it won’t be reconsidered in
        // subsequent iterations of the ordering pass.
        // ------------------------------------------------------------
        ldx     candidate_sprite                  // X := chosen sprite
        lda     #UNASSIGNED
        sta     sprite_owner_actor_idx,x          // mark sprite as processed
		
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
		
        dec     sprite_iter_idx                   // decrement ordering counter
        bpl     select_topmost_actor_for_draw_order // continue until <0


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
        cmp     #UNASSIGNED
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
  compute_actor_visibility
================================================================================

Summary
	Set an actor’s visibility based on horizontal position relative to the
	current viewport. Seeds a short reveal counter on edge entry.

Arguments
	active_costume
	actor                    (used after mapping from active_costume)
	viewport_left_col

Global Inputs
	actor_for_costume[]      (maps costume → actor index; negative = none)
	actor_x_pos[]            (actor X position in pixels)
	actor_visible[]          (prior visibility state)

Global Outputs
	actor_visible[]          (updated to visible/offscreen)
	actor_anim_counter[]     (seeded to $03 on edge entry)

Description
	• If no actor is mapped to the active costume, mark offscreen and return.
	• Compute Sub := actor_x − viewport_left_col.
	• Sub < 0            → offscreen left → offscreen.
	• 0..VIEW_FULL_SPAN_MINUS1
		→ inside view; right-edge equality may seed reveal.
	• Sub ≥ VIEW_FULL_SPAN_MINUS1+1
		→ offscreen right → offscreen.
	• Special case Sub == $FF with prior offscreen
		→ entering from left edge → seed counter and show.
	• On right-edge equality with prior offscreen
		→ seed counter and show.
================================================================================
*/
* = $3FF7
compute_actor_visibility:
		// ------------------------------------------------------------
		// Update visibility from horizontal position vs viewport.
		// Inputs: active_costume, actor_x_pos[X], viewport_left_col.
		// Writes: actor_visible[X]; seeds actor_anim_counter[X] on edge entry.
		// ------------------------------------------------------------
		ldx     active_costume                // X := costume index
		lda     actor_for_costume,x           // A := actor index for this costume
		bpl     compute_viewport_delta        // if A >= 0 → actor exists
		lda     #ACTOR_OFFSCREEN              // no actor mapped → offscreen
		jmp     write_visibility_and_rts      // commit and return

compute_viewport_delta:
		// ------------------------------------------------------------
		// Compute Sub := actor_x − viewport_left_col.
		// Sub domain:
		//   Sub < 0        → fully left of view
		//   0..$27         → inside view (inclusive edges)
		//   ≥ $28          → fully right of view
		// Also special-case Sub == $FF for left-edge re-entry.
		// ------------------------------------------------------------
		ldx     actor                         // X := active actor index
		lda     actor_x_pos,x                 // A := actor X position (pixels)
		sec                                   // prepare subtract
		sbc     viewport_left_col             // A := Sub

		// ------------------------------------------------------------
		// Fast left-edge check:
		//   if Sub < $FE → definitely not $FF, go test offscreen-left/inside.
		//   else Sub ∈ {$FE,$FF} → handle potential edge re-entry.
		// ------------------------------------------------------------
		cmp     #$FE                          // Sub ? $FE
		bcc     test_offscreen_left           // Sub <= $FD → left/inside path

		// ------------------------------------------------------------
		// Sub == $FF and previously offscreen → entering from left edge.
		// Seed a short animation counter and mark visible.
		// ------------------------------------------------------------
		lda     actor_visible,x               // prior visibility
		bne     test_offscreen_left           // already visible → normal path
		lda     #$03                          // small reveal counter
		sta     actor_anim_counter,x          // start edge-entry ticks
		lda     #ACTOR_ONSCREEN               // set visible now
		sta     actor_visible,x               // commit immediate show
		rts                                   // done

test_offscreen_left:
		// ------------------------------------------------------------
		// Fully-left test: Sub < 0 → offscreen left.
		// ------------------------------------------------------------
		cmp     #$00                          // Sub ? 0
		bcc     set_offscreen_and_return      // Sub negative → offscreen

		// ------------------------------------------------------------
		// Right-edge equality: Sub == VIEW_FULL_SPAN_MINUS1 → at right edge.
		// Handle possible edge-entry seeding if coming from offscreen.
		// ------------------------------------------------------------
		cmp     #VIEW_FULL_SPAN_MINUS1        // Sub ? right-edge delta
		beq     mark_visible_in_view          // equal → handle edge entry

test_offscreen_right:
		// ------------------------------------------------------------
		// Fully-right test: Sub ≥ VIEW_FULL_SPAN_MINUS1+1 → offscreen right.
		// ------------------------------------------------------------
		bcs     set_offscreen_and_return_b    // beyond right edge → offscreen

mark_visible_in_view:
		// ------------------------------------------------------------
		// In-range: 0..VIEW_FULL_SPAN_MINUS1. If equal edge, may seed reveal.
		// ------------------------------------------------------------
		bne     set_visible_and_return        // strictly inside → visible


		// ------------------------------------------------------------
		// Equal to right edge: if previously offscreen, seed reveal counter.
		// ------------------------------------------------------------
		lda     actor_visible,x               // prior visibility
		bne     write_visibility_after_edge_enter // already visible → skip seed
		lda     #$03                          // small reveal counter
		sta     actor_anim_counter,x          // store
		lda     #ACTOR_ONSCREEN                  // prepare visible


write_visibility_after_edge_enter:
		jmp     write_visibility_and_rts      // commit and return

set_visible_and_return:
		lda     #ACTOR_ONSCREEN               // A := visible
		jmp     write_visibility_and_rts      // commit and return

set_offscreen_and_return_b:
		lda     #ACTOR_OFFSCREEN              // A := offscreen (right side)
		jmp     write_visibility_and_rts      // commit and return

set_offscreen_and_return:
		lda     #ACTOR_OFFSCREEN              // A := offscreen (left side)

write_visibility_and_rts:
		sta     actor_visible,x               // actor_visible[X] := A
		rts                                   // return

/*
================================================================================
  rsrc_unlock_or_unassign_costume
================================================================================
Summary:
	Unlock one locked costume.	
	If no costumes were locked, detach one of the kids costumes.
   
	This is done in two phases:
	
	-Phase 1 scans all costumes from highest index down and unlocks at most one.	
	-If no locked costumes found, Phase 2 takes place.
	It scans all kids costumes until it finds a loaded costume that is not the current kid.
	Once found, it detaches it from its actor, resets default destination coords, 
	and “parks” the costume in a holding room. If no candidate found, the code hangs.
	
Uses / Globals:
	costume_liveness_tbl[X]    	Attribute byte; bit7=1 ⇒ locked, bit7=0 ⇒ unlocked.
	costume_ptr_hi_tbl[X]  		Non-zero ⇒ resource resident in memory.
	current_kid_idx      		Index of current kid; never detached.
	costume_dest_x[X]  			Set to COSTUME_DFLT_X_DEST on unassignment.
	costume_dest_y[X]  			Set to COSTUME_DFLT_Y_DEST on unassignment.
	room_for_costume[X]  		Set to COSTUME_HOLDING_ROOM on unassignment.

Description:
	1) Unlock pass:
		-Scan all costumes downward, using X as index.
		-If costume_liveness_tbl[X].bit7 == 1 → clear with AND #$7F and exit

	2) Unassignment pass (only if no locks found):
		-Scan all kids costumes downward, using X as index (X = 8..1)
		-Skip if X == current_kid_idx, or not resident (ptr.hi==0)
		-On hit: 
			-costume_liveness_tbl := 0
			-detach_actor_from_costume
			-(dest_x,dest_y) := (COSTUME_DFLT_X_DEST,COSTUME_DFLT_Y_DEST)
			-room_for_costume[X] := COSTUME_HOLDING_ROOM
			-exit
	3) Failure:
		-diag_code := #$05
		-hang
================================================================================
*/
* = $567B
rsrc_unlock_or_unassign_costume:
		// Start at the highest costume index; scan downward one-by-one
        ldx #COSTUME_MAX_INDEX            

scan_locked_costumes:
        // ------------------------------------------------------------
        // Costume unlocked? If so, skip
		//
        //   	bit7 = 1 → locked (N=1)
		//		bit7 = 0 → unlocked (N=0)
        // ------------------------------------------------------------
        lda costume_liveness_tbl,x
        bpl advance_lock_scan                 

        // ------------------------------------------------------------
        // Costume locked - unlock it and return
        // ------------------------------------------------------------
        and #RSRC_CLEAR_LOCK_MASK                         
        sta costume_liveness_tbl,x          
        rts                              

advance_lock_scan:
        dex
        bne scan_locked_costumes        // continue scan while X != 0 (note: index 0 is not checked)

        // ------------------------------------------------------------		
        // No locked costumes found - switch to detachment pass over the kids' costumes
        // ------------------------------------------------------------
        ldx #$08
scan_for_evict:
        // ------------------------------------------------------------
        // Costume is the active kid? Then don't detach it
        // ------------------------------------------------------------
        cpx current_kid_idx
        beq advance_costume_evict_scan

        // ------------------------------------------------------------
        // Costume resident in memory?
        // costume_ptr_hi_tbl[X] == 0 ⇒ not resident → skip
        // ------------------------------------------------------------
        lda costume_ptr_hi_tbl,x
        beq advance_costume_evict_scan

		// ------------------------------------------------------------
		// Costume resident - Detach it from its actor
		// - Clear refcount before
		// ------------------------------------------------------------
        lda #$00
        sta costume_liveness_tbl,x         // clear refcount

        // Preserve loop index
        txa                               
        pha                               

		// Detach actor from costume
        jsr detach_actor_from_costume               

        // Restore loop index
        pla                               
        tax                               

        // ------------------------------------------------------------
        // Reset default destination for costume
        // ------------------------------------------------------------
        lda #COSTUME_DFLT_X_DEST        // default X destination
        sta costume_dest_x,x
        lda #COSTUME_DFLT_Y_DEST        // default Y destination
        sta costume_dest_y,x

        // ------------------------------------------------------------
        // Park the costume in the holding room and return.
        // This de-associates it from the active room until explicitly reloaded.
        // ------------------------------------------------------------
        lda #COSTUME_HOLDING_ROOM       
        sta room_for_costume,x
        rts                               

advance_costume_evict_scan:
        dex
        bne scan_for_evict            	// more candidates (X != 0) → continue first-8 scan

		// ------------------------------------------------------------
		// No eligible costume among kids - hangup
		// ------------------------------------------------------------
        lda #$05
        sta debug_error_code             

        ldy #MAP_IO_IN                   
        sty cpu_port      
costume_evict_hangup:
        sta vic_border_color_reg      	// set border color
        jmp costume_evict_hangup        // loop forever
