#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "walkbox_waypoint_planner.asm"
#import "rsrc_mgmt.asm"
#import "actor_costume.asm"

.label actor_index               = $3043  // Loop index over actors (0..3) in animation pass
.label costume_scan_idx          = $335E  // Loop index over costumes (0..$18) in update pass

.const ACTOR_RENDER_FLAGS_TICK   = ACTOR_RENDER_VISIBLE_AND_REFRESH    // Actor render flags each tick when counter > 0
.const MOTION_MASK_7F            = MSK_LOW7BITS    // Mask to clear bit7 when testing for MOTION_STOPPED_CODE
/*
================================================================================
  step_actor_visibility_and_sprites
================================================================================

Summary
	Two-phase per-frame update:
		1) Scan all costumes and refresh which actors are visible; assign/unassign
		sprites to match visibility.
		2) Iterate all actors with sprites; tick animation counters and, when a
		stopped actor’s counter expires, recompute and push its sprite X
		coordinates from the actor’s room position.

Global Inputs
	actor_for_costume[]               costume → actor index
	actor_sprite_index[]              actor → sprite index
	actor_anim_counter[]              per-actor countdown to next sprite move
	actor_motion_state[]              per-actor motion bits
	actor_render_flags[]           	  per-actor render flags

Global Outputs
	active_costume                    used as pass-1 loop index
	actor_index                       used as pass-2 loop index
	actor_render_flags[]           	  |="ACTOR_RENDER_FLAGS_TICK" when ticking; unchanged on move
	actor_anim_counter[]              decremented when >0; left at 0 when move occurs
	actor_sprite_x_lo/hi[]            updated when a stopped actor’s counter hits 0

Description
	- Pass 1:
		• For each costume with an assigned actor, call update_actor_visibility.
		• Reconcile sprite ownership with visibility via sync_sprites_with_actors.
	- Pass 2:
		• For each actor that has a sprite:
			– If actor_anim_counter == 0 and (actor_motion_state & MOTION_MASK_7F)
			equals MOTION_STOPPED_CODE, call map_room_to_sprite_coords and
			copy the computed X lo/hi to the assigned sprite.
			– Otherwise decrement actor_anim_counter and OR ACTOR_RENDER_FLAGS_TICK into
			actor_render_flags to request a redraw.

Notes
	- Sprite Y is produced inside map_room_to_sprite_coords; this routine
	only propagates X lo/hi to the hardware sprite slot when moving.
	- No pathfinding or limb animation occurs here; those are handled elsewhere.
================================================================================
*/
* = $3044
step_actor_visibility_and_sprites:
		// ------------------------------------------------------------
		// Pass 1: refresh actor visibility for all active costumes
		// ------------------------------------------------------------
		// Use active_costume as index to scan all costumes
		lda     #$00                          
		sta     active_costume                

scan_costume_for_visibility:
		// Costume has actor assigned? If not, skip
		ldx     active_costume                
		lda     actor_for_costume,x           
		bmi     next_costume_or_done          
		
		// Update visibility
		sta     actor                         
		jsr     update_actor_visibility  

next_costume_or_done:
		// Next costume
		inc     active_costume                
		lda     active_costume                
		cmp     #COSTUME_MAX_INDEX + 1        
		bne     scan_costume_for_visibility   

		// ------------------------------------------------------------  
		// Reconcile sprites with actor visibility  
		// ------------------------------------------------------------  
		jsr     sync_sprites_with_actors  

		// ------------------------------------------------------------  
		// Pass 2: tick animation counter and render flags 
		// ------------------------------------------------------------  
		// Use actor_index to scan all actors
		ldx     #$00                          
		stx     actor_index                   

scan_actor_loop:
		ldx     actor_index                  
		stx     actor                        

		// Actor has a sprite assigned? If not, skip
		lda     actor_sprite_index,x          
		cmp     #NO_SPRITE                    
		beq     next_actor_or_done            

		// ------------------------------------------------------------  
		// Animation counter handling:  
		//   counter = 0 → check if actor stopped (apply move)  
		//   counter > 0 → decrement and mark for refresh  
		// ------------------------------------------------------------  
		// Animation counter ongoing? If not, skip
		lda     actor_anim_counter,x          
		beq     apply_sprite_move_if_stopped  

		// Tick the counter
		dec     actor_anim_counter,x          
		
		// Set render flags for this tick, continue with next actor
		lda     actor_render_flags,x       	  
		ora     #ACTOR_RENDER_FLAGS_TICK      
		sta     actor_render_flags,x       	  
		jmp     next_actor_or_done            


apply_sprite_move_if_stopped:
		// Actor is stopped? If not, skip
		lda     actor_motion_state,x          
		and     #MOTION_MASK_7F               
		cmp     #MOTION_STOPPED_CODE          
		bne     next_actor_or_done            

		// ------------------------------------------------------------  
		// Actor stopped
		//
		// Convert actor’s room coordinates → sprite coordinates  
		// and update hardware sprite X registers.  
		// ------------------------------------------------------------  
		jsr     map_room_to_sprite_coords  	 
		lda     actor_tgt_sprite_x_lo,x          // load computed sprite X low  
		ldy     actor_sprite_index,x             // Y := sprite slot  
		sta     actor_sprite_x_lo,y              // update hardware sprite X low  
		lda     actor_tgt_sprite_x_hi,x          // load computed sprite X high  
		sta     actor_sprite_x_hi,y              // update hardware sprite X high  

next_actor_or_done:
		// Next actor
		inc     actor_index                   
		lda     actor_index                   
		cmp     #ACTOR_COUNT_TOTAL            
		bne     scan_actor_loop               
		rts                                   
/*
================================================================================
  step_actor_motion_and_animation
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
		• apply_pending_waypoint_update
		• step_actor_motion
		• step_actor_animation
	- Repeat until all costumes processed; no drawing or sprite assignment here.
================================================================================
*/		
* = $3361
step_actor_motion_and_animation:
		// Use costume_scan_idx to scan over all costumes
		lda     #$00                              
		sta     costume_scan_idx                  

		// ------------------------------------------------------------  
		// For each costume, check if it’s linked to an actor and whether 
		// its resource is loaded. Only then perform per-frame updates 
		// for that actor.  
		// ------------------------------------------------------------  
scan_costume_for_updates:
		// Actor assigned? If not, skip
		ldx     costume_scan_idx                  
		lda     actor_for_costume,x               
		bmi     next_costume_or_exit              
		
		// Save actor and active_costume
		sta     actor                             
		stx     active_costume                    
		
		// Costume cached? If not, skip
		lda     costume_ptr_hi_tbl,x              
		beq     next_costume_or_exit              

		// ------------------------------------------------------------  
		// Actor updates  
		// ------------------------------------------------------------  
		//  1. Apply or confirm the next movement waypoint.  
		//  2. Update motion state  
		//  3. Trigger limb animation for the current action (walk, etc.).  
		// ------------------------------------------------------------  
		jsr     apply_pending_waypoint_update   // handle pending waypoint updates  
		jsr     step_actor_motion               // update position and motion state  
		jsr     step_actor_animation         // animate limbs based on motion state  

next_costume_or_exit:
		// Next costume
		inc     costume_scan_idx                  
		lda     costume_scan_idx                  
		cmp     #COSTUME_MAX_INDEX + 1            
		bne     scan_costume_for_updates          
		rts                                       
/*
================================================================================
  redraw_flagged_actors
================================================================================
Summary
	Iterates all active actors and redraws only those flagged for render
	refresh. For each such actor, it clears the refresh bit, updates visibility
	and sprite positioning, reassigns sprites if necessary, and calls the draw
	routine if visible. If the actor is stopped, the “needs draw” bit is set to
	ensure a stable pose is redrawn next frame.

Global Inputs
	costume_for_actor[]           actor → assigned costume index
	actor_render_flags[]       	  animation state flags
	actor_motion_state[]          low nibble encodes motion code
	actor_visible[]               current scene visibility state

Global Outputs
	actor_render_flags[]       	  RENDER_REFRESH cleared when refreshed
								  RENDER_VISIBLE set if stopped

Description
	- For each actor:
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
		// Initialize actor scan
		lda     #$00                          
		sta     actor                         

scan_actor_for_refresh:
		// Actor has costume assigned? If not, skip
		ldx     actor                         
		lda     costume_for_actor,x           
		bmi     next_actor_or_exit            
		
		// Cache costume as “active”
		sta     active_costume                

		// Actor needs a render refresh? If not, skip
		lda     actor_render_flags,x       	  
		and     #ACTOR_RENDER_REFRESH         
		beq     next_actor_or_exit            

		// Clear the refresh flag before we draw
		lda     actor_render_flags,x       		
		and     #ACTOR_RENDER_CLEAR_REFRESH     
		sta     actor_render_flags,x       		

		// ------------------------------------------------------------
		// Bring actor’s screen state up to date
		//   - recompute visibility within viewport
		//   - convert room coords → sprite coords
		//   - reconcile sprite ownership vs. visibility
		// ------------------------------------------------------------
		jsr     update_actor_visibility   	  
		jsr     map_room_to_sprite_coords     
		jsr     sync_sprites_with_actors      

		// Actor still visible? If not, skip it
		ldx     actor                         
		lda     actor_visible,x               
		beq     next_actor_or_exit            
		
		// Actor visible and up to date - render it
		jsr     draw_actor                    

		// Resolve actor's motion state
		ldx     actor                         
		lda     actor_motion_state,x          
		
		// Is the actor stopped? If not, skip
		and     #MSK_LOW_NIBBLE               
		cmp     #MOTION_STOPPED_CODE          
		bne     next_actor_or_exit           
		
		// Actor stopped, set the "render visible" flag
		lda     #ACTOR_RENDER_VISIBLE         
		ora     actor_render_flags,x       
		sta     actor_render_flags,x       

next_actor_or_exit:
		// Next actor
		inc     actor                         
		lda     actor                         
		cmp     #ACTOR_COUNT_TOTAL            
		bne     scan_actor_for_refresh        

		// Flip sprite buffer for double buffering
		lda     #$01                          
		eor     sprite_bank_sel               
		sta     sprite_bank_sel               
		rts                                   
