/*
================================================================================
  Walkbox attribute resolution, behavior dispatch, and foreground masking
================================================================================
Summary
	Centralizes all logic for interpreting walkbox attribute bytes, resolving
	which box an actor occupies after each pixel step, and applying any
	behavior programs (ladder, slopes, foreground masking) tied to those
	attributes.

Responsibilities
	• Attribute semantics
		- Document and interpret the walkbox attribute byte (record field #4),
		splitting it into:
		· plain depth/mask attributes (bit7 = 0)
		· behavior-coded attributes (bit7 = 1, bits2..6 = handler index)
		- Distinguish between normal foreground-occluded vs non-occluded boxes
		and special behaviors like ladders and diagonal slopes.

	
	• Per-step walkbox resolution
		- resolve_walkbox_with_step_x_then_y:
			· Advance the actor one pixel along X, resolve the current walkbox,
			  apply foreground-occlusion gating, and optionally nudge the probe
			  Y coordinate toward the waypoint when resolution fails.
		- resolve_walkbox_with_step_y_then_x:
			· Advance the actor one pixel along Y and, on failure, nudge the
			  probe X coordinate toward the waypoint to disambiguate overlapping
			  or edge-adjacent boxes.
		- Both routines:
			· bracket each attempt with save_actor_path_state /
			  restore_actor_path_state so speculative positions never leak
			  into live actor state unless resolution succeeds.
			· tail-jump to set_state_to_moving on success or
			  set_actor_stopped_and_render when geometry cannot be resolved.

	• Walkbox and behavior selection
		- resolve_actor_walkbox_and_behavior:
			· Ensure the actor’s walkbox tables are resident (via
			  get_walkboxes_for_costume) and fall back to an undetermined
			  result if not.
			· Fast-path test the cached actor_box_cur; if still valid, return
			  BOX_RESULT_UNCHANGED.
			· Otherwise perform a full scan:
				· walk boxes in index order, caching vertical edge metadata and
				  aborting cleanly on EDGE_INVALID sentinels.
				· prioritize the previous box index (actor_box_prev) when
				  multiple overlapping boxes could contain the actor, to keep
				  transitions stable and visually smooth.
			· Publish the chosen box index and attribute into actor_box_cur /
			  actor_box_attr and raise actor_path_update_flag when a transition
			  occurs.
			· Inspect the attribute’s bit7 and the latched handler index to
			  decide whether to:
				· run a behavior handler (bit7 = 1, valid attribute), or
				· clear existing handler state, or
				· treat the change as purely a depth/mask update.

	• Behavior handler dispatch
		- apply_behavior:
			· Derive a handler index from attribute bits 2..6, compare to the
			  current actor_box_handler_idx, and:
				· on match: simply refresh actor_box_attr using the latched
				  selection to update depth/mask.
				· on change:
					· cache the new index in selected_box_attr.
					· call clear_actor_walkbox_attrs to reset all per-actor
					  planes (handler index, facing override, FG occlusion).
					· mask actor_box_attr with BOX_ATTR_DEPTH_MASK to keep only
					  the low depth bits.
					· look up the concrete handler address in
					  behavior_handler_lo/hi and patch behavior_handler_jsr_op.
					· invoke the handler via the patched JSR $0000; handlers
					  return by jumping to attr_update_return_stub.
		- clear_actor_walkbox_attrs:
			· Iterate three per-actor “planes” at fixed strides (handler index,
			  facing override, foreground occlusion) and zero the entry for the
			  current actor.
		- Handlers implemented in this module:
			· attr_update_return_stub:
				· shared return point for all behavior handlers.
			· set_facing_override_to_up:
				· mark the actor’s facing override as “up” (used for ladders).
			· clear_box_handler_idx / clear_box_handler_idx_b:
				· reset the actor’s box handler index to zero, providing two
				  interchangeable entry points.
			· set_box_occluded_by_fg:
				· mark the actor as occluded by the foreground layer and
				  optionally enter a blocking debug loop.

	• Foreground-masking and debug gating
		- set_box_occluded_by_fg:
			· set actor_box_fg_occlusion to FG_OCCLUDED so the renderer draws
			  the actor behind foreground tiles.
			· consult dbg_gate:
				· if bit7 is set, apply masking and return immediately.
				· otherwise, clear dbg_gate and enter a debug wait loop that:
					· maps I/O in via cpu_port,
					· flashes the VIC border to BORDER_WAIT_FG_MASK_COLOR, and
					· spins until an external agent writes a nonzero value into
					  dbg_gate, at which point I/O is unmapped and control
					  returns.
		- Foreground-occlusion gate in resolve_walkbox_with_step_x_then_y:
			· delays releasing an actor from foreground masking until the walkbox
			  transition has been acknowledged via actor_path_update_flag, so
			  box changes and occlusion flips remain synchronized.


Interactions
• Relies on pathing and motion-core modules for:
- location and probe state snapshots (save_actor_path_state,
restore_actor_path_state),
- walkbox table access and geometry tests (get_walkboxes_for_costume,
get_walkbox_offset, is_actor_pos_inside_box),
- high-level state transitions (set_state_to_moving,
set_actor_stopped_and_render).
• Feeds per-actor walkbox, handler, facing override, and foreground
occlusion state back into the actor-motion and renderer pipelines, so
pixel-level traversal decisions, layering, and special behaviors remain
consistent across frames.

================================================================================

Example of walkboxes attributes
-------------------------------

Room 1 - Front of mansion (where mansion, mailbox and part of the fence is shown)

	10 walkboxes, numbered 0 to 9
		0 L: 00 R: 07 T: 3A B: 43 Attr: 03	- Grass area, right behind the mailbox/package
		1 L: 08 R: 12 T: 3A B: 43 Attr: 00	- Grass area, between the mailbox and the left edge of the mansion
		2 L: 13 R: 2C T: 3D B: 43 Attr: 00	- Grass area, left of the steps, front of the mansion bricks
		3 L: 17 R: 5D T: 2D B: 2D Attr: 03	- Porch (one-line tall walkbox)
		4 L: 37 R: 3E T: 2E B: 42 Attr: 00	- Rectangular area on the steps
		5 L: 2D R: 47 T: 43 B: 43 Attr: 00	- Grass area, in front of steps (one-line tall walkbox)
		6 L: 48 R: 61 T: 3D B: 43 Attr: 00	- Grass area, right of the steps, front of the mansion bricks
		7 L: 62 R: 7E T: 3D B: 43 Attr: 00	- Grass area, right of the mansion bricks
		8 L: 7F R: 9D T: 3A B: 3E Attr: 03	- Grass area, behind the metal fence
		9 L: 7F R: 9D T: 41 B: 43 Attr: 00	- Graas area, in front of metal fence

		Attr: 
			00 = normal walkbox - no special behavior, actor not occluded by background
			03 = "background walkbox" - no special behavior, actor occluded by background

		Adjacency map

					|-----3------|				Porch (3)
						  4  					Steps (4)
		0 <-> 1 <-> 2 <-> 5 <-> 6 <-> 7 <-> 8	Behind fence (8)
									  ^---> 9	In front of fence (9)

		Adjacency list
			0: [1]                                                                                                                                                          
			1: [2, 0]                                                                                                                                                       
			2: [5, 1]                                                                                                                                                       
			3: [4]                                                                                                                                                          
			4: [5, 3]                                                                                                                                                       
			5: [6, 2, 4]                                                                                                                                                    
			6: [7, 5]                                                                                                                                                       
			7: [6, 8, 9]                                                                                                                                                    
			8: [7]                                                                                                                                                          
			9: [7]                                                                                                                                                          
            
Room 2 - bottom of pool

		0 L: 00 R: 04 T: 35 B: 3A Attr: 88		Next to the left wall - diagonal slope
		1 L: 00 R: 05 T: 3B B: 43 Attr: 00		Next to the left wall - square area
		2 L: 06 R: 0D T: 41 B: 43 Attr: 00		In front of floating chair + radio
		3 L: 05 R: 0D T: 35 B: 3A Attr: 03		Behind floating chair
		4 L: 0E R: 18 T: 35 B: 43 Attr: 00		In front of ladder, on top of grating
		5 L: 19 R: 27 T: 3C B: 43 Attr: 00		In front of reactor part
		6 L: 19 R: 22 T: 35 B: 37 Attr: 03		Behind reactor part
		7 L: 12 R: 12 T: 03 B: 34 Attr: 84 		One-line wide ladder box

================================================================================

Walkbox attribute byte  - semantic map and bit layout
-----------------------------------------------------
Context
	The fourth byte in each walkbox record is a mixed “attribute tag” consumed by
	the motion and layering system. The tag may either be a plain depth/masking
	value or a behavior-bearing code that also implies a depth. Code paths that
	read and act on this byte: box adoption and masking rules, and the layer
	handler dispatcher. 

Bit layout
	bit7 = 0 → plain attribute (no handler dispatch)
	bit7 = 1 → behavior handler present; index = (attr & $7C) >> 2
	bits1..0 = depth/mask sub-bits (interpretation below)

Canonical values and meanings:

	00  : normal walkbox, not occluded by foreground (FG). Depth/mask sub-bits=0.
	
	03  : normal walkbox, occluded by FG. Depth/mask sub-bits=3.

	84  : ladder behavior. Forces facing “Up” while on the box.
		  handler index = 1 → handler writes facing override = $C1,
		  which resolves to direction mask $81 (Up). Not FG-occluded.
		  Depth/mask sub-bits=0.

	88  : diagonal slope (left/down), not occluded by FG.
		  handler index = 2 → handler clears prog index flag; slope semantics
		  are provided by this handler class. Depth/mask sub-bits=0.

	8B  : diagonal slope (left/down), occluded by FG.
		  handler index = 2, FG-occluded variant. Depth/mask sub-bits=3.

	8C  : diagonal slope (right/down), not occluded by FG.
		  handler index = 3 → alternate slope handler; not FG-occluded.
		  Depth/mask sub-bits=0. 

	8F  : diagonal slope (right/down), occluded by FG.
		  handler index = 3, FG-occluded variant. Depth/mask sub-bits=3.


Operational notes

	* Plain attributes (bit7=0) do not enter the handler dispatcher; they only set
	  depth/masking for the current box. 
	* Programmed attributes (bit7=1 and attr≠$FF) extract a 5-bit index, may clear
	  or refresh per-actor box attribute flags, and then retain only the low sub-bits for
	  depth/masking after the handler returns. 
	* The ladder “Up” facing comes from writing $C1 into facing override, which masks to
	  $81 when applied as a one-shot facing override. 
	  
================================================================================
*/
#importonce
#import "globals.inc"
#import "constants.inc"
#import "actor_motion_constants.inc"
#import "walkbox_waypoint_planner.asm"
#import "actor_motion_core.asm"
#import "actor_path_dda.asm"
#import "walkbox_snap.asm"

.label box_attr_ptr              = $17  // ZP: box-attribute table base pointer (lo=$17, hi=$18)
.label dbg_gate                  = $2F  // Debug gate: bit7=1 bypass waits; write #$00 to enable waits
.label behavior_handler_jsr_op   = $2E0D  // Code-site: JSR operand for selected box-attr handler
.label selected_box_attr         = $FC3B  // Alias of shared scratch: selected box attribute value
.label selected_handler_idx      = $FC3D  // Scratch: index of chosen box-attribute handler
.label last_box_vedge            = $FC3F  // Scratch: cached vertical edge from last tested box
.label scan_box_idx              = $FDAC  // Scratch: index of box currently under consideration

/*
================================================================================
  resolve_walkbox_with_step_x_then_y
================================================================================
Summary
	Advance the actor one pixel along X, attempt to resolve the current walkbox,
	apply a foreground-occlusion gate, and if resolution fails, nudge the probe
	Y coordinate toward the waypoint and retry before either continuing motion
	or stopping the actor.

Arguments
	actor						Current actor index

Vars/State
	actor_pos_x,x				Committed actor X position in screen pixels.
	actor_dir_x_bit,x			Per-actor horizontal direction flag; 0 = left, nonzero = right.
	actor_dir_y_bit,x			Per-actor vertical direction flag; 0 = up, nonzero = down.
	actor_cur_waypoint_y,x		Target waypoint Y coordinate for the active path.
	actor_path_probe_y,x		Speculative Y probe coordinate used for walkbox tests.
	actor_box_fg_occlusion,x	Foreground-occlusion state for the actor (clear / occluded code).
	actor_box_cur,x				Cached walkbox index currently associated with the actor.
	actor_box_attr,x			Attribute/depth byte for the current walkbox.
	actor_box_handler_idx,x		Active walkbox behavior handler index for the actor.
	actor_path_update_flag,x	Flag raised when a walkbox change occurs and a path recompute is needed.

Global Outputs
	actor_pos_x,x				Updated when the horizontal pixel step is accepted.
	actor_path_probe_y,x		Adjusted by ±1 pixel when a vertical nudge toward the waypoint is applied.
	actor_box_fg_occlusion,x	Cleared when a pending path update allows the actor to pass foreground.
	actor_box_cur,x				Updated to the newly selected walkbox when a transition occurs.
	actor_box_attr,x			Updated to reflect the selected walkbox’s attribute/depth byte.
	actor_box_handler_idx,x		May be updated indirectly when behavior handlers are applied.
	actor_path_update_flag,x	Raised when the resolver commits a walkbox transition.

Returns
	A	BOX_RESULT_UNCHANGED    	when the box and handler state remain effectively stable
		BOX_RESULT_CHANGED     		When a new box or behavior handler takes effect
		BOX_RESULT_UNDETERMINED 	When geometry is invalid
	
Flow
	- On successful resolution, tail-jumps to set_state_to_moving.
	- On persistent failure after nudge, tail-jumps to set_actor_stopped_and_render.
	- Otherwise returns to the caller with A carrying the resolver’s code.

Description
	- Snapshot the actor’s path state (position, box, flags) via
	  save_actor_path_state so tentative edits can be rolled back.
	- Apply a one-pixel horizontal step to actor_pos_x based on actor_dir_x_bit:
		• 0 → move left (DEC)
		• nonzero → move right (INC)
	- Call update_actor_walkbox_state to recompute the actor’s walkbox.
	- Apply the foreground-occlusion gate:
		• If actor_box_fg_occlusion == FG_OCCLUDED:
			· If actor_path_update_flag == 0:
				– Force BOX_UNRESOLVED_CODE into A, keeping the occlusion state.
			· Else:
				– Clear actor_box_fg_occlusion and continue with the resolver’s A.
		• Otherwise, use the resolver’s A as-is.
	- If the result is resolved:
		• Restore the saved path state to re-establish the committed
		  position/probe split.
		• Tail-jump to set_state_to_moving so the motion state machine advances.
	- If the result is unresolved:
		• Restore the saved path state.
		• Compare actor_path_probe_y to actor_cur_waypoint_y:
			· If already aligned on Y, stop traversal and tail-jump to
			  set_actor_stopped_and_render.
			· If misaligned, nudge actor_path_probe_y by ±1 pixel toward the
			  waypoint using actor_dir_y_bit, resnapshot state, and retry
			  update_actor_walkbox_state:
				– If resolution now succeeds, return with the resolver’s A.
				– If resolution still fails, restore the snapshot again and
				  tail-jump to set_actor_stopped_and_render.

Notes
	- Y is treated as a secondary “edge disambiguation” axis: after a failed X
	  step, a one-pixel nudge in Y can move the probe out of ambiguous overlaps
	  and into a resolvable box.
	- Snapshot/restore bracketing around each attempt guarantees that only
	  fully validated positions and walkbox state become visible to the rest of
	  the motion system.
	- The foreground-occlusion gate provides a controlled way to hold actors
	  behind foreground elements until a path update acknowledges the new box,
	  then automatically clears the occlusion to let motion proceed.
================================================================================
*/
* = $2C36
resolve_walkbox_with_step_x_then_y:
        // Preserve path state before coordinate edits
		ldx     actor
		jsr     save_actor_path_state


        // ------------------------------------------------------------
        // Move actor horizontally one pixel based on direction bit
        // ------------------------------------------------------------
        lda     actor_dir_x_bit,x              // test horizontal direction
        bne     step_right_in_box              

        dec     actor_pos_x,x                  // move one pixel left
        jmp     resolve_box_after_horizontal_move

step_right_in_box:
        inc     actor_pos_x,x                  // move one pixel right

resolve_box_after_horizontal_move:
		// ------------------------------------------------------------
		// Try to resolve current box; A = #BOX_UNRESOLVED_CODE if unresolved
		// ------------------------------------------------------------
		jsr     resolve_actor_walkbox_and_behavior

        // ------------------------------------------------------------
        // Foreground occlusion gate logic
		//
        //   If actor_box_fg_occlusion == FG_OCCLUDED:
        //       - If actor_path_update_flag == 0 → force unresolved (A := $FF)
        //       - Else clear masking flag and continue
        //   Otherwise skip masking gate
        // ------------------------------------------------------------
		// Actor occluded?
        ldy     actor_box_fg_occlusion,x        
        cpy     #FG_OCCLUDED                	
        bne     handle_fg_masking               // not equal → skip masking logic

		// No path update yet? If so, force unresolved and stay occluded
        lda     actor_path_update_flag,x        
        beq     force_box_unresolved            
		
		// Path update already occurred - clear occlusion and let actor pass
        lda     #FG_MASKING_CLEAR               
        sta     actor_box_fg_occlusion,x
		
        jmp     handle_fg_masking               

force_box_unresolved:
		// Force unresolved state (A := $FF)
        lda     #BOX_UNRESOLVED_CODE           

handle_fg_masking:
		// Box unresolved?
        cmp     #BOX_UNRESOLVED_CODE           
        beq     handle_unresolved_box          

		// Box resolved → restore state and continue moving
        jsr     restore_actor_path_state
        jmp     set_state_to_moving			   // in actor_motion_core.asm

handle_unresolved_box:
        // ------------------------------------------------------------
		// Unresolved box
        // ------------------------------------------------------------
		// Restore path state
        jsr     restore_actor_path_state      

		// Aligned on the Y axis? If not, continue
        lda     actor_cur_waypoint_y,x             
        cmp     actor_path_probe_y,x
        bne     check_vertical_alignment         
		
        // Aligned on Y axis, we cannot resolve the box here → stop animation
        jmp     set_actor_stopped_and_render      

check_vertical_alignment:
        // ------------------------------------------------------------
        // Not aligned on Y axis - nudge one pixel in the right direction
        // ------------------------------------------------------------
        lda     actor_dir_y_bit,x                  	// test vertical direction
        bne     step_down_in_box                   	// ≠0 → nudge downward

        dec     actor_path_probe_y,x 				// nudge one pixel up
        jmp     recheck_box_after_vertical_move

step_down_in_box:
        inc     actor_path_probe_y,x 				// nudge one pixel down

recheck_box_after_vertical_move:
        // ------------------------------------------------------------
        // Retry resolution after Y nudge
        // ------------------------------------------------------------
        // Preserve path state before coordinate edits
        jsr     save_actor_path_state        
		
		// Attempt box resolution with nudged Y
        jsr     resolve_actor_walkbox_and_behavior   

        // Box resolved? If yes, exit 
        cmp     #BOX_UNRESOLVED_CODE
        bne     dummy_fht       
		
		// Box unresolved, restore path state and stop actor
        jsr     restore_actor_path_state       
        jmp     set_actor_stopped_and_render   

dummy_fht:
		//Redundant section of code - replicated from original
		// (This was an evident copy/paste of the section above)
        cmp     #BOX_UNRESOLVED_CODE
        bne     finish_horizontal_traversal       	
        jsr     restore_actor_path_state       		
        jmp     set_actor_stopped_and_render       	
		

finish_horizontal_traversal:
        rts
/*
================================================================================
  resolve_walkbox_with_step_y_then_x
================================================================================
Summary
	Advance the actor one pixel along Y, attempt to resolve the current walkbox,
	and if resolution fails, nudge the probe X coordinate toward the waypoint and
	retry before either continuing motion or stopping the actor.

Arguments
	actor						Current actor index

Global Inputs
	actor_pos_y[]				Committed actor Y position in screen pixels.
	actor_dir_y_bit[]			Per-actor vertical direction flag; 0 = up, nonzero = down.
	actor_dir_x_bit[]			Per-actor horizontal direction flag; 0 = left, nonzero = right.
	actor_cur_waypoint_x[]		Target waypoint X coordinate for the active path.
	actor_path_probe_x[]		Speculative X probe coordinate used for walkbox tests.
	actor_box_cur[]				Currently cached walkbox index for the actor.
	actor_box_attr[]			Attribute/depth byte for the current walkbox.
	actor_box_handler_idx[]		Active walkbox behavior handler index for the actor.
	actor_path_update_flag[]	Flag raised when a walkbox change occurs and a path recompute is needed.

Global Outputs
	actor_pos_y[] 				Updated to the new committed Y position when a vertical step is accepted.
	actor_path_probe_x[] 		Adjusted one pixel left or right when a horizontal nudge is attempted.
	actor_box_cur[] 			Updated to the newly selected walkbox when a transition occurs.
	actor_box_attr[] 			Updated to reflect the selected walkbox’s attribute/depth byte.
	actor_box_handler_idx[] 	May be updated indirectly when behavior handlers are applied.
	actor_path_update_flag[] 	Raised when a box transition is committed by the resolver.

Returns
	A	BOX_RESULT_UNCHANGED    Cached or resolved box is stable; no behavior change.
		BOX_RESULT_CHANGED      Walkbox or handler state changed during resolution.
		BOX_RESULT_UNDETERMINED Geometry invalid or could not be resolved.
Flow
	- On success, tail-jumps to set_state_to_moving.
	- On persistent failure, tail-jumps to set_actor_stopped_and_render.
	- Otherwise returns to the caller with A set as above.

Description
	• Save the actor’s current path state (position, box, flags) so that any
	tentative updates can be rolled back if walkbox resolution fails.
	• Apply a one-pixel vertical step to actor_pos_y based on actor_dir_y_bit:
		- 0 → move up (DEC)
		- nonzero → move down (INC)
	• Call update_actor_walkbox_state to recompute the active walkbox:
		- If the box resolves successfully:
			· Restore the saved path state to reapply the committed position/
			probe split.
			· Tail-jump to set_state_to_moving so the motion state machine
			continues normally.
		- If the box is unresolved:
			· Restore the saved path state.
			· Compare actor_path_probe_x to actor_cur_waypoint_x:
			· If already aligned, stop traversal and tail-jump to
			set_actor_stopped_and_render.
			· If misaligned, nudge actor_path_probe_x by ±1 pixel toward
			the waypoint using actor_dir_x_bit, then resnapshot state.
			· Retry update_actor_walkbox_state with the nudged X probe:
				· If resolution now succeeds, fall through with A set by the
				resolver.
				· If resolution still fails, restore the saved state again and
				tail-jump to set_actor_stopped_and_render.

Notes
	• This routine treats Y as the primary step axis and X as a secondary “edge
	disambiguation” axis to resolve ambiguous or overlapping walkbox borders.
	• Snapshot/restore pairing around each attempt ensures that only fully
	validated coordinates and box states are ever committed to live actor
	state.
================================================================================
*/
* = $2CA3
resolve_walkbox_with_step_y_then_x:
        // Preserve path state before coordinate edits
        ldx     actor                      
        jsr     save_actor_path_state      

        // ------------------------------------------------------------
        // Move actor vertically one pixel according to direction bit
        // ------------------------------------------------------------
        lda     actor_dir_y_bit,x              		// test vertical direction
        bne     step_down_in_box_vert          

        dec     actor_pos_y,x                  		// move one pixel upward
        jmp     resolve_box_after_vstep

step_down_in_box_vert:
        inc     actor_pos_y,x                  		// move one pixel downward

resolve_box_after_vstep:
		// ------------------------------------------------------------
		// Try to resolve current box; A = #BOX_UNRESOLVED_CODE if unresolved
		// ------------------------------------------------------------
		jsr     resolve_actor_walkbox_and_behavior
		
		// Unresolved box?
		cmp     #BOX_UNRESOLVED_CODE
		beq     handle_unresolved_box_vert

		//Redundant code - replicated from original
		cmp     #BOX_UNRESOLVED_CODE
		beq     handle_unresolved_box_vert
		
		// Box resolved → restore state and continue moving
		jsr     restore_actor_path_state
        jmp     set_state_to_moving			   		// in actor_motion_core.asm

handle_unresolved_box_vert:
        // ------------------------------------------------------------
        // Unresolved box
        // ------------------------------------------------------------
		// Restore path state
		jsr     restore_actor_path_state
		
		// Aligned on the X axis? If not, continue
		lda     actor_cur_waypoint_x,x
		cmp     actor_path_probe_x,x
		bne     check_horizontal_dir
		
        // Aligned on X axis, we cannot resolve the box here → stop animation
		jmp     set_actor_stopped_and_render

check_horizontal_dir:
        // ------------------------------------------------------------
        // Not aligned on X axis - nudge one pixel in the right direction
        // ------------------------------------------------------------
        lda     actor_dir_x_bit,x                  	// test horizontal direction
        bne     nudge_right_toward_waypoint        	// ≠0 → nudge right

        dec     actor_path_probe_x,x 			   	// nudge one pixel left
        jmp     recheck_box_after_horizontal_nudge

nudge_right_toward_waypoint:
        inc     actor_path_probe_x,x 			   	// nudge one pixel right

recheck_box_after_horizontal_nudge:
        // ------------------------------------------------------------
        // Retry box resolution after X nudge
        // ------------------------------------------------------------
        // Preserve path state before coordinate edits
        jsr     save_actor_path_state        
		
		// Attempt box resolution with nudged X
        jsr     resolve_actor_walkbox_and_behavior       

        // Box resolved? If yes, exit
        cmp     #BOX_UNRESOLVED_CODE
        bne     finish_vertical_traversal         
		
		// Box unresolved, restore path state and stop actor
        jsr     restore_actor_path_state       
        jmp     set_actor_stopped_and_render   

finish_vertical_traversal:
        rts
/*
================================================================================
  resolve_actor_walkbox_and_behavior
================================================================================
Summary
    Determine which walkbox the actor now occupies, apply stability and
    attribute-handler rules, and emit a result code describing whether the
    actor remained in the same box, changed boxes, or encountered invalid /
    undeterminable geometry.

Arguments
    actor                      current actor index (in zero page)

Global Inputs
    actor_box_cur[x]          cached current box index
    actor_box_prev[x]         previous committed box index
    actor_box_attr[x]         current box’s attribute/depth byte
    actor_box_handler_idx[x]  active box-attribute handler index
    actor_path_update_flag[x] flag signaling path recomputation

Global Outputs
    actor_box_cur[x]          updated to selected walkbox index
    actor_box_attr[x]         updated attribute/depth byte
    actor_path_update_flag[x] raised when transitioning to a new box

Returns
    A   BOX_RESULT_UNCHANGED    cached box is still valid; no handler change
        BOX_RESULT_CHANGED      box changed or handler state updated
        BOX_RESULT_UNDETERMINED geometry invalid (e.g., $FF vertical-edge sentinels)

Description
    • Acquire the actor’s walkbox table (via get_walkboxes_for_costume).  
      If the resource is not resident → return BOX_RESULT_UNDETERMINED immediately.

    • Fast-path containment test:
        – Compute the offset for actor_box_cur and test the actor’s position
          with is_actor_pos_inside_box.
        – If inside → return BOX_RESULT_UNCHANGED.

    • Full scan when the cached box fails:
        – Initialize scan_box_idx = $FF so the first INC targets box 0.
        – Latch prior attribute/handler into selected_box_attr / selected_handler_idx.
        – Clear actor_box_attr[x] and last_box_vedge.

        Loop over boxes:
            – Abort with BOX_RESULT_UNDETERMINED if last_box_vedge == $FF.
            – Compute walkbox offset for scan_box_idx.
            – If the actor is NOT inside the box:
                  · Read its vertical-edge byte and cache it in last_box_vedge.
                  · Continue scanning.
            – If the actor IS inside:
                  · Fetch the attribute.
                  · If attribute == $FF → treat as sentinel; break scan.
                  · Apply stability rule:
                        If scan_box_idx == actor_box_prev[x]:
                            · Select immediately; set actor_path_update_flag[x].
                        Else:
                            · Continue scanning for potential previous-box match.

    • On breaking the loop:
        – Copy attribute from the selected box into actor_box_attr[x].
        – If attr != $FF → commit actor_box_cur[x] = scan_box_idx.

    • Post-selection handler logic:
        – If bit7 of actor_box_attr[x] is set:
              · If attr != $FF → run apply_behavior and report CHANGED.
              · If attr == $FF → skip and report UNCHANGED.
        – Else if selected_handler_idx ≠ 0:
              · Clear per-actor box-attribute planes and report CHANGED.
        – Else:
              · Return UNCHANGED.

Notes
    • Stability rule ensures smooth transitions across overlapping boxes by
      prioritizing the previous box index when both could contain the actor.
    • Vertical-edge sentinels provide corruption-detection: a $FF edge byte
      indicates invalid geometry and makes the scan abort safely.
================================================================================
*/
* = $2CF4
resolve_actor_walkbox_and_behavior:
        ldx     actor                           
		
		// Load active room’s walkbox table for the active costume
        jsr     get_walkboxes_for_costume       
		
		// Walkboxes not resident? Exit
        cmp     #WALKBOX_NOT_RESIDENT           
        bne     check_existing_box              
        rts                                     

check_existing_box:
		// Resolve offset to current walkbox for this actor
        lda     actor_box_cur,x              
        jsr     get_walkbox_offset           
		
		// Actor inside? If so, box has not changed, exit
        jsr     is_actor_pos_inside_box      
        bne     begin_box_scan               
        rts                                  

begin_box_scan:
		// scan_box_idx := pre-increment start
        lda     #INIT_SCAN_IDX               
        sta     scan_box_idx                 

		// Latch last attribute for later comparison
        lda     actor_box_attr,x             
        sta     selected_box_attr            

		// Latch last handler for later comparison
        lda     actor_box_handler_idx,x      
        sta     selected_handler_idx         

		// Reset walkbox offset scratch register (Y)
        ldy     #$00                      	
		
		// Clear computed attribute/depth until a box is chosen
        lda     #$00
        sta     actor_box_attr,x          	
		
		// Reset cached vertical edge to EDGE_UNSET
        sta     last_box_vedge            	

try_next_box:
		// Increment scan_box_idx to probe next box
        inc     scan_box_idx

		// Last box vertical edge is valid? If so, continue
        lda     last_box_vedge              
        cmp     #EDGE_INVALID               
        bne     test_current_box            

		// Edge invalid, can't resolve box, exit
        lda     #BOX_RESULT_UNDETERMINED    
        rts
		jmp		dummy_tcb 

test_current_box:
		// Resolve offset to current walkbox
        lda     scan_box_idx
        jsr     get_walkbox_offset          
		
		// Actor inside the box? 
        jsr     is_actor_pos_inside_box     
dummy_tcb:		
        bne     cache_vertical_edge         // miss → cache V-edge and continue

		// ------------------------------------------------------------
		// Actor inside box
		// ------------------------------------------------------------
		// Advance Y to the attribute byte
        iny                                   
		
		// Attribute valid? If so, continue
        lda     (box_ptr),y                   
        cmp     #ATTR_INVALID                 
        bne     process_box_attribute      

        // Attribute is invalid/sentinel
		// Set A to BREAK_SCAN to break the loop
        lda     #BREAK_SCAN
        jmp     loop_control

process_box_attribute:
		// ------------------------------------------------------------
		// Process attribute and apply stability rule
		// ------------------------------------------------------------
		// Current box same as previous box? If not, continue scanning
        lda     scan_box_idx                 
        cmp     actor_box_prev,x             
        beq     select_current_box           

		// Not the previous box → continue scanning
        lda     #CONTINUE_SCAN              
        jmp     loop_control                

select_current_box:
		// ------------------------------------------------------------
		// Commit chosen box as current
		// ------------------------------------------------------------
		// Set actor_path_update_flag to trigger path recompute
        lda     #TRUE                          
        sta     actor_path_update_flag,x

		// Store scan_box_idx as new actor_box_cur
        lda     scan_box_idx                  
        sta     actor_box_cur,x               

		// Set A = BREAK_SCAN to exit loop via loop_control
        lda     #BREAK_SCAN                   
        jmp     loop_control                  

cache_vertical_edge:
        // ------------------------------------------------------------
        // Actor not in box: cache vertical edge and continue scan
        // ------------------------------------------------------------
        // Advance to vertical-edge byte
		iny
        iny
		// Cache vertical edge of box
        lda     (box_ptr),y          
        sta     last_box_vedge       

		// Continue scanning
        lda     #CONTINUE_SCAN               

loop_control:
		// ------------------------------------------------------------
		// Loop control: if A != $00 → continue scanning
		// ------------------------------------------------------------
        bne     try_next_box              

		// ------------------------------------------------------------
        // Chosen box path
		// ------------------------------------------------------------
		// Copy walkbox attribute from table
        lda     (box_ptr),y               
        sta     actor_box_attr,x          
		
		// Attribute invalid? If so, skip publish
        cmp     #ATTR_INVALID             
        beq     eval_attr_effects

		// Publish box as current box
        lda     scan_box_idx         
        sta     actor_box_cur,x      

eval_attr_effects:
		// ------------------------------------------------------------
		// Evaluate selected box attribute for handler updates
		//
		//   - If bit7 clear → skip handler path
		//   - If attr == ATTR_INVALID with bit7 set → exit unchanged
		//   - Else call apply_behavior and report BOX_RESULT_CHANGED
		// ------------------------------------------------------------
		// Bit 7 set? If not, there's no handler to apply, skip this part
        lda     actor_box_attr,x              
        bpl     check_handler_update          
		
		// Invalid attribute? If so, skip this part
        cmp     #ATTR_INVALID                 
        beq     finalize_attr_update         

        // Bit7 set and valid attribute → apply box-specific handler update
		// Apply attribute behavior
        jsr     apply_behavior     		
        ldx     actor                         // restore actor index after JSR		
		
		// Set result as box changed
        lda     #BOX_RESULT_CHANGED           
finalize_attr_update:
        jmp     exit_update_box_for_actor     

check_handler_update:
        // ------------------------------------------------------------
        //   If a previous handler index was latched (nonzero), clear current
        //   walkbox attrs to force a refresh and report “changed”.
        // ------------------------------------------------------------
        lda     selected_handler_idx          // A := prior handler index
        beq     exit_update_box_for_actor     // zero → nothing to do → unchanged

        jsr     clear_actor_walkbox_attrs     // reset per-actor walkbox attributes
        ldx     actor                         // restore X=actor after JSR
		
		// Set result as box changed
        lda     #BOX_RESULT_CHANGED           // indicate state change

exit_update_box_for_actor:
        rts                                    // exit with A as return value
/*
================================================================================
  apply_behavior
================================================================================
Summary
    Apply behavior to a walkbox via a handler.

Arguments
    A       					attribute code for the selected walkbox
    X      						actor index

Vars/State
    selected_box_attr           latched handler index / attribute selector
    behavior_handler_jsr_op     JSR operand patched with handler address

Global Inputs
    actor                       current actor index (for reloading X after JSRs)
    actor_box_handler_idx[]     current handler index for each actor
    actor_box_attr[]            current box attribute byte (low 2 bits = depth)
    behavior_handler_lo/hi[]    table of handlers

Global Outputs
    actor_box_handler_idx[]     updated handler index when it changes
    actor_box_attr[]            refreshed or depth-masked attribute byte
    selected_box_attr           set to computed handler index on change
    behavior_handler_jsr_op     patched to call the selected handler

Description
    • From the incoming attribute in A, derive a handler index by masking with
      BOX_CODE_INDEX_MASK and shifting right twice; range is nominally 0..31.
    • Compare this index against actor_box_handler_idx[X]:
        – If equal:
            · Reload selected_box_attr and copy it into actor_box_attr[X] to
              refresh the committed attribute/depth, then return.
        – If different:
            · Store the new index into selected_box_attr.
            · Call clear_actor_walkbox_attrs to zero per-actor box planes.
            · Write the new index into actor_box_handler_idx[X].
            · Mask actor_box_attr[X] with BOX_ATTR_DEPTH_MASK so only depth
              bits remain and other bits are cleared.
            · Use the handler index as an offset into behavior_handler_lo/hi[]
              to fetch the target address and patch behavior_handler_jsr_op.
            · Call the patched JSR $0000, which the handler returns from by
              jumping to attr_update_return_stub.

Notes
    This routine centralizes all transitions between box-attribute handler
    programs. Depth (low 2 bits) is preserved across handler changes; all other
    attribute semantics are reinitialized by clearing the per-actor planes and
    deferring to the handler’s logic. Handlers are indexed purely by the
    encoded bits 2..6 of the attribute.
================================================================================
*/
* = $2DD7
apply_behavior:
		// Resolve behavior handler index from attribute value
		and     #BOX_CODE_INDEX_MASK         // isolate bits 2–6 of input code  
		lsr                                  // >>1  
		lsr                                  // >>2 → normalized handler index

        // ------------------------------------------------------------
		// If resolved index == current handler index → same handler active  
        // ------------------------------------------------------------
		// Same handler as current?
		cmp     actor_box_handler_idx,x           
		bne     update_box_handler_on_prog_change // mismatch → need handler switch  
		
        // ------------------------------------------------------------
		// Same handler
		// Refresh actor_box_attr with current selection (updates depth) and exit  
        // ------------------------------------------------------------
		lda     selected_box_attr                 
		sta     actor_box_attr,x                  
		rts                                       

update_box_handler_on_prog_change:
        // ------------------------------------------------------------
		// Handler change: cache new index and reset per-actor attribute planes
        // ------------------------------------------------------------
		sta     selected_box_attr              
		jsr     clear_actor_walkbox_attrs      
		
		// Update actor_box_handler_idx[X] with selected handler index
		lda     selected_box_attr              
		sta     actor_box_handler_idx,x        
		
		// Keep only the depth bits of actor_box_attr[X], clear the other ones
		lda     actor_box_attr,x               
		and     #BOX_ATTR_DEPTH_MASK           
		sta     actor_box_attr,x               

		// Patch JSR operand using handler table entry for this actor’s handler
		ldy     actor_box_handler_idx,x          // Y := handler index (0..31)
		tya                                      // A := Y
		tax                                      // X := handler index for table lookup
		lda     behavior_handler_lo,x            // load handler low byte
		sta     behavior_handler_jsr_op          // write JSR operand low
		lda     behavior_handler_hi,x            // load handler high byte
		sta     behavior_handler_jsr_op + 1      // write JSR operand high

		// Call handler through self-modified JSR operand (target set above)
		jsr     $0000

* = $2E0F
attr_update_return_stub:               			// common return stub for box-attr handlers
		rts                             		
/*
================================================================================
  clear_actor_walkbox_attrs
================================================================================
Summary
	Zero three per-actor walkbox attribute slots for the actor in X. 

Arguments
	X                       	actor index

Global Inputs
	actor_box_handler_idx      	pointer to attribute plane 0 (base of per-actor tables)

Global Outputs
	actor_box_handler_idx[actor]  		:= 0
	actor_box_facing_override[actor]  	:= 0
	actor_box_fg_occlusion[actor]  		:= 0

Description
	- Initialize box_attr_ptr from actor_box_handler_idx.
	- Use Y=actor for (ptr),Y addressing; reuse X as a 0..2 loop counter.
	- For each of three planes:
		• store 0 at (box_attr_ptr),Y for this actor
		• advance box_attr_ptr by +4 bytes (with page carry)
	- Restore X to the actor index and exit.
================================================================================
*/
* = $2E10
clear_actor_walkbox_attrs:
        // ------------------------------------------------------------
		// Initialize attribute pointer: box_attr_ptr := actor_box_handler_idx (base of attr table)
        // ------------------------------------------------------------
		lda     #<actor_box_handler_idx             
		sta     box_attr_ptr                   
		lda     #>actor_box_handler_idx             
		sta     box_attr_ptr + 1

        // ------------------------------------------------------------
		// Prepare loop registers  
		//
		// Y := actor index (copied from X for indirect addressing)  
		// X := loop counter (0..2) for three attribute clears  
        // ------------------------------------------------------------
		txa  
		tay  
		ldx     #$00

clear_next_attr:
        // ------------------------------------------------------------
		// Clear current attribute slot for this actor
        // ------------------------------------------------------------
		lda     #$00
		sta     (box_attr_ptr),y

        // ------------------------------------------------------------
		// Advance to next attribute plane (+4 bytes)
        // ------------------------------------------------------------
		lda     box_attr_ptr                	// lo += 4
		clc
		adc     #$04
		sta     box_attr_ptr
		bcc     advance_to_next_attr_plane      // no page cross
		inc     box_attr_ptr + 1                // carry → bump hi byte

advance_to_next_attr_plane:
        // ------------------------------------------------------------
		// Loop: run three clears at offsets +0,+4,+8 (X = 0,1,2)
        // ------------------------------------------------------------
		inx                                     
		cpx     #$02                            
		bcc     clear_next_attr                 
		beq     clear_next_attr                 

        // ------------------------------------------------------------
		// Epilogue: restore X := actor index and return
        // ------------------------------------------------------------
		tya                                     
		tax                                     
		rts                                     
/*
================================================================================
  set_facing_override_to_up
================================================================================
Summary
	Force the actor’s box-facing override to “up”. This override is applied 
	exclusively during ladder traversal, ensuring the actor always faces toward 
	the ladder while climbing.

Global Outputs
	actor_box_facing_override[X]   set to FACING_OVERRIDE_UP (override = up)

Description
	Loads the current actor index into X, writes FACING_OVERRIDE_UP
	to that actor’s override slot, and returns.

Notes
	FACING_OVERRIDE_UP is the encoded value for “face up” in the box-facing override scheme.
================================================================================
*/
* = $2E35
set_facing_override_to_up:
        ldx     actor
        lda     #FACING_OVERRIDE_UP
        sta     actor_box_facing_override,x
        rts
/*
================================================================================
 clear_box_handler_idx
================================================================================
Summary
	Reset the per-actor box handler/program index to zero.

Global Inputs
	actor                          current actor index

Global Outputs
	actor_box_handler_idx[]        entry for actor set to $00
================================================================================
*/
* = $2E3D
clear_box_handler_idx:
        ldx     actor
        lda     #$00
        sta     actor_box_handler_idx,x
        rts
/*
================================================================================
  clear_box_handler_idx_b
================================================================================
Summary
	Identical in function to clear_box_handler_idx. Exists as a duplicate
	entry point with a distinct label.
================================================================================
*/
* = $2E45
clear_box_handler_idx_b:
        ldx     actor
        lda     #$00
        sta     actor_box_handler_idx,x
        rts
/*
================================================================================
  set_box_occluded_by_fg
================================================================================
Summary
    Mark the current actor as occluded by the foreground layer. Optionally
    enter a debug wait loop that changes the border color, and resumes
    when an external controller clears the gate.

Global Inputs
    actor                         current actor index
    actor_box_fg_occlusion[]        per-actor foreground-masking state
    dbg_gate                      controls whether the blocking debug loop runs

Global Outputs
    actor_box_fg_occlusion[]        entry for actor set to FG_OCCLUDED
    dbg_gate                      cleared to $00 before entering the wait loop

Description
    • Load the current actor index and set actor_box_fg_occlusion[actor] to the
      FG_OCCLUDED code so the renderer treats the actor as being behind the
      foreground layer.
    • Read dbg_gate:
        – If bit7 is set, bypass the debug wait entirely and return.
        – Otherwise, clear dbg_gate to $00 and proceed into the blocking wait.
    • In debug mode:
        – Loop:
            · Set the VIC border to BORDER_WAIT_FG_MASK_COLOR as a visual cue.
            · Poll dbg_gate until it becomes nonzero.
================================================================================
*/

* = $2E4D
set_box_occluded_by_fg:
        // ------------------------------------------------------------
		// Mark actor_box_fg_occlusion[X] := FG_OCCLUDED → actor rendered behind FG layer
        // ------------------------------------------------------------
		ldx     actor                         
		lda     #FG_OCCLUDED              	  
		sta     actor_box_fg_occlusion,x        

		// Check debug gate: if bit7 set, skip interactive debug wait  
		lda     dbg_gate                      
		bmi     debug_exit                    

        // ------------------------------------------------------------
		// Debug enabled
        // ------------------------------------------------------------
		// Reset debug gate to 0 before entering wait loop  
		lda     #$00  
		sta     dbg_gate  

		// Map I/O into memory space
		ldy     #MAP_IO_IN  
		sty     cpu_port
debug_wait_loop:
		//Set border color
		lda     #BORDER_WAIT_FG_MASK_COLOR
		sta     vic_border_color_reg              

		// Spin until external agent sets dbg_gate != 0
		lda     dbg_gate                      // poll gate
		beq     debug_wait_loop               // 0 → keep waiting

		// Map I/O out
		ldy     #MAP_IO_OUT
		sty     cpu_port

debug_exit:
		rts                                     // exit; FG masking already applied

// 0 → $2E0F  attr_update_return_stub
// 1 → $2E35  set_facing_override_to_up
// 2 → $2E3D  clear_box_handler_idx
// 3 → $2E45  clear_box_handler_idx_b
// 4 → $2E4D  set_box_occluded_by_fg
* = $2e6e
behavior_handler_lo:
        .byte   <attr_update_return_stub
		.byte	<set_facing_override_to_up
		.byte	<clear_box_handler_idx
		.byte	<clear_box_handler_idx_b
		.byte	<set_box_occluded_by_fg

* = $2e73
behavior_handler_hi:
        .byte   >attr_update_return_stub
		.byte	>set_facing_override_to_up
		.byte	>clear_box_handler_idx
		.byte	>clear_box_handler_idx_b
		.byte	>set_box_occluded_by_fg

/*
// -----------------------------------------------------------------------------
// resolve_walkbox_with_step_x_then_y
// -----------------------------------------------------------------------------
function resolve_walkbox_with_step_x_then_y(actor_id):
    x := actor_id

    // Snapshot path state so we can roll back on failure
    save_actor_path_state(x)

    // 1) Take a 1-pixel step along X
    if actor_dir_x_bit[x] == 0:
        actor_pos_x[x] -= 1         // move left
    else:
        actor_pos_x[x] += 1         // move right

    // 2) Try to resolve walkbox + behavior based on new X
    result := resolve_actor_walkbox_and_behavior(actor_id)
    // result is returned through A in real code; treat symbolically here

    // 3) Foreground occlusion gate:
    if actor_box_fg_occlusion[x] == FG_OCCLUDED:
        if actor_path_update_flag[x] == 0:
            // No path update yet → force “unresolved” and keep actor masked
            result := BOX_UNRESOLVED_CODE
        else:
            // Path update has occurred → clear occlusion flag, keep result
            actor_box_fg_occlusion[x] := FG_MASKING_CLEAR

    // 4) If box resolved, continue moving; otherwise, attempt Y-nudge
    if result != BOX_UNRESOLVED_CODE:
        // Resolution succeeded: restore snapshot and switch to moving state
        restore_actor_path_state(x)
        set_state_to_moving(actor_id)
        return

    // 5) First attempt failed: restore snapshot before trying Y-nudge
    restore_actor_path_state(x)

    // Check whether we are already aligned with the waypoint on Y
    if actor_cur_waypoint_y[x] == actor_path_probe_y[x]:
        // Cannot disambiguate via a Y nudge → stop the actor
        set_actor_stopped_and_render(actor_id)
        return

    // 6) Nudge probe Y one pixel toward waypoint
    if actor_dir_y_bit[x] == 0:
        actor_path_probe_y[x] -= 1      // nudge up
    else:
        actor_path_probe_y[x] += 1      // nudge down

    // 7) Snapshot again around the second attempt
    save_actor_path_state(x)

    // Retry resolution after Y nudge
    result := resolve_actor_walkbox_and_behavior(actor_id)

    if result == BOX_UNRESOLVED_CODE:
        // Still unresolved → roll back and stop actor
        restore_actor_path_state(x)
        set_actor_stopped_and_render(actor_id)
        return

    // Resolved after Y nudge:
    // Snapshot remains visible and caller proceeds with result in A
    return  // caller sees “resolved” result (unchanged/changed)


// -----------------------------------------------------------------------------
// resolve_walkbox_with_step_y_then_x
// -----------------------------------------------------------------------------
function resolve_walkbox_with_step_y_then_x(actor_id):
    x := actor_id

    // Snapshot path state so we can roll back on failure
    save_actor_path_state(x)

    // 1) Take a 1-pixel step along Y
    if actor_dir_y_bit[x] == 0:
        actor_pos_y[x] -= 1         // move up
    else:
        actor_pos_y[x] += 1         // move down

    // 2) Try to resolve walkbox + behavior based on new Y
    result := resolve_actor_walkbox_and_behavior(actor_id)

    // The code redundantly tests result twice; logically it’s just:
    if result != BOX_UNRESOLVED_CODE:
        // Box resolved → restore snapshot and continue moving
        restore_actor_path_state(x)
        set_state_to_moving(actor_id)
        return

    // 3) Resolution failed: restore snapshot before trying X-nudge
    restore_actor_path_state(x)

    // Check whether we are already aligned with waypoint on X
    if actor_cur_waypoint_x[x] == actor_path_probe_x[x]:
        // Cannot disambiguate via an X nudge → stop the actor
        set_actor_stopped_and_render(actor_id)
        return

    // 4) Nudge probe X one pixel toward waypoint
    if actor_dir_x_bit[x] == 0:
        actor_path_probe_x[x] -= 1      // nudge left
    else:
        actor_path_probe_x[x] += 1      // nudge right

    // 5) Snapshot again around second attempt
    save_actor_path_state(x)

    // Retry resolution after X nudge
    result := resolve_actor_walkbox_and_behavior(actor_id)

    if result == BOX_UNRESOLVED_CODE:
        // Still unresolved → roll back and stop actor
        restore_actor_path_state(x)
        set_actor_stopped_and_render(actor_id)
        return

    // Resolved after X nudge:
    // Snapshot remains visible and caller proceeds with result in A
    return  // caller sees “resolved” result (unchanged/changed)


// -----------------------------------------------------------------------------
// resolve_actor_walkbox_and_behavior
// -----------------------------------------------------------------------------
function resolve_actor_walkbox_and_behavior(actor_id) -> box_result:
    x := actor_id

    // 1) Ensure walkbox data is resident for this actor’s costume
    status := get_walkboxes_for_costume(actor_id)
    if status == WALKBOX_NOT_RESIDENT:
        return BOX_RESULT_UNDETERMINED

    // 2) Fast-path: test the actor’s cached current box
    current_box := actor_box_cur[x]
    compute_walkbox_offset(current_box)           // updates box_ptr, Y
    inside := is_actor_pos_inside_box(actor_id, box_ptr, current_box)

    if inside:
        // Cached box is still valid; nothing changed
        return BOX_RESULT_UNCHANGED

    // 3) Full scan across all boxes
    scan_box_idx := INIT_SCAN_IDX                 // typically $FF; first INC → 0

    // Latch previous attribute + handler
    selected_box_attr  := actor_box_attr[x]
    selected_handler_idx := actor_box_handler_idx[x]

    // Clear “current” attribute and cached vertical edge
    actor_box_attr[x] := 0
    last_box_vedge := EDGE_UNSET

    while true:
        // Next box index
        scan_box_idx += 1

        // If last vertical edge marked invalid, geometry is broken
        if last_box_vedge == EDGE_INVALID:
            return BOX_RESULT_UNDETERMINED

        // Resolve offset for this candidate box
        compute_walkbox_offset(scan_box_idx)      // sets box_ptr, Y
        inside := is_actor_pos_inside_box(actor_id, box_ptr, scan_box_idx)

        if not inside:
            // 3a) Actor is not in this box → cache vertical edge and continue
            // Advance Y to vertical edge field
            // (Exact layout abstracted: we just read its “vertical edge byte”.)
            vedge := read_vertical_edge(box_ptr)
            last_box_vedge := vedge

            // Continue the scan
            continue

        // 3b) Actor is inside this box — inspect its attribute
        attr := read_attribute_for_box(box_ptr)

        if attr == ATTR_INVALID:
            // Invalid/sentinel attribute → break out; no box selected yet
            break  // leaves actor_box_attr[x] as 0 (or invalid)

        // Apply stability rule:
        if scan_box_idx == actor_box_prev[x]:
            // Prefer the previous box when overlapping
            actor_path_update_flag[x] := TRUE
            actor_box_cur[x] := scan_box_idx
            // Break out; this is our chosen box
            break
        else:
            // Not the previous box → keep scanning for a previous-box match
            // Note: this logic implies we keep looping and might later
            // settle on a different candidate or hit sentinel geometry.
            continue

    // 4) Publish selected box attribute
    // At this point, box_ptr/Y still point into the current box record.
    actor_box_attr[x] := read_attribute_for_box(box_ptr)

    if actor_box_attr[x] != ATTR_INVALID:
        // Only publish index when attribute is valid
        actor_box_cur[x] := scan_box_idx

    // 5) Evaluate attribute bits and handler changes
    attr := actor_box_attr[x]

    if (attr & $80) != 0:
        // bit7 set → behavior-coded attribute
        if attr == ATTR_INVALID:
            // Treat “invalid coded attr” as no-op; leave box_result unchanged
            return BOX_RESULT_UNCHANGED

        // Valid behavior-coded attribute → apply corresponding handler
        apply_behavior(attr, x)
        // apply_behavior returns via stub, but the effect is “changed”
        return BOX_RESULT_CHANGED

    // 6) Plain attribute (no behavior code)
    // If a handler was previously active, clear per-actor handler state
    if selected_handler_idx != 0:
        clear_actor_walkbox_attrs(actor_id)
        return BOX_RESULT_CHANGED

    // No handler active and no behavior bits set → unchanged
    return BOX_RESULT_UNCHANGED


// -----------------------------------------------------------------------------
// apply_behavior
// -----------------------------------------------------------------------------
function apply_behavior(attr_code, actor_index):
    x := actor_index

    // 1) Derive handler index from attribute bits
    handler_index := (attr_code & BOX_CODE_INDEX_MASK) >> 2   // bits 2..6 → 0..31

    // 2) If same handler already active, just refresh attribute and return
    if handler_index == actor_box_handler_idx[x]:
        // selected_box_attr carries the last “committed” attribute/depth
        actor_box_attr[x] := selected_box_attr
        return

    // 3) Handler change path
    selected_box_attr := handler_index

    // Clear all per-actor walkbox attribute planes
    clear_actor_walkbox_attrs(actor_index)

    // Update current handler index
    actor_box_handler_idx[x] := selected_box_attr

    // Keep only depth bits in actor_box_attr[x]
    actor_box_attr[x] := actor_box_attr[x] & BOX_ATTR_DEPTH_MASK

    // Invoke behavior handler
    idx := actor_box_handler_idx[x]
    behavior_handler := behavior_handler_lo/hi[idx]
    call(behavior_handler)

    // Handlers must eventually return via attr_update_return_stub,
    // which returns to the caller of apply_behavior.


// -----------------------------------------------------------------------------
// clear_actor_walkbox_attrs
// -----------------------------------------------------------------------------
function clear_actor_walkbox_attrs(actor_id):
    // Base pointer to per-actor attribute planes
    base_ptr := address_of(actor_box_handler_idx)

    // Y := actor index; we conceptually index “rows” by actor
    actor_index := actor_id

    // The table is arranged as three “planes” separated by +4 bytes each:
    //   plane 0: handler index
    //   plane 1: facing override
    //   plane 2: FG occlusion
    // For each plane, we clear the entry for this actor.
    for plane from 0 to 2:
        // Clear slot at (base_ptr + plane*4)[actor]
        write_byte(base_ptr + plane*4, actor_index, 0)

    // No special return value


// -----------------------------------------------------------------------------
// set_facing_override_to_up
// -----------------------------------------------------------------------------
function set_facing_override_to_up():
    actor_box_facing_override[actor] := FACING_OVERRIDE_UP
    return


// -----------------------------------------------------------------------------
// clear_box_handler_idx
// -----------------------------------------------------------------------------
function clear_box_handler_idx():
    actor_box_handler_idx[actor] := 0
    return


// -----------------------------------------------------------------------------
// set_box_occluded_by_fg
// -----------------------------------------------------------------------------
function set_box_occluded_by_fg():
    // Mark actor as occluded by foreground
    actor_box_fg_occlusion[actor] := FG_OCCLUDED

	// Debugging part not included
    return
*/