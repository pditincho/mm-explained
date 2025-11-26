#importonce
#import "globals.inc"
#import "constants.inc"
#import "actor_motion_constants.inc"
#import "walkbox_helpers.asm"
#import "actor_animation.asm"
#import "actor_path_dda.asm"

.label motion_needed             = $FC3B  // Alias of shared scratch: #$00=motion, #$01=no motion (from init_dda_for_path)
.label axis_bit_scratch          = $FC3B  // Alias of shared scratch: holds bit7 result for axis compare
.label motion_nibble_tmp         = $FC3B  // Alias of shared scratch: temp for low-nibble motion code

/*
================================================================================
step_actor_motion
================================================================================

Summary:
    Drives actor motion toward the current target waypoint. If the target
    waypoint differs from the active one, it installs the new waypoint, resets
    the actor’s motion state, computes path deltas using DDA logic, and stages
    directional traversal and limb selection. If the actor is already at the
    waypoint, it evaluates whether animation is still required (e.g., due to
    MOTION_ABOUT_TO_STOP or mismatched position).

Arguments:
     actor                          Current actor index (ZP)
     actor_tgt_waypoint_{x,y}[]     Per-actor destination waypoint (pixels)
     actor_active_wpt_{x,y}[]       Current installed waypoint (pixels)
     actor_{x,y}_pos[]              Actor current screen position (pixels)
     actor_motion_state[]           Actor motion/turning state

Returns:
    None

Global Inputs:
     dda_dominant_axis              DDA: 0=X-dominant, ≠0=Y-dominant
     dir_x_bit                      DDA: 1=right, 0=left
     dir_y_bit                      DDA: 1=down, 0=up

Global Outputs:
     actor_active_wpt_{x,y}[]       Updated to match target waypoint
     position_to_waypoint_{x,y}_for_actor[]		Current position snapshot for DDA stepping
     actor_motion_state[]           Updated low nibble with motion-needed flag
     actor_tgt_facing_direction[]			Traversal direction mask

Description:
    • If the target waypoint differs from the current one, install it as active.
    • Clear the actor’s motion state low nibble and stage a new path:
        – Snapshot current position.
        – Initialize DDA step deltas and extract direction flags.
        – Merge result into motion state.
        – Compute and store traversal direction mask.
    • Select appropriate limbs based on facing/turning status.
        – If turning, exit.
        – If ready to walk, select walking limbs and exit.
    • If target == active:
        – If MOTION_ABOUT_TO_STOP is set, animate.
        – If current pos ≠ target pos, animate.
        – Else exit without action.

Notes:
    • Returns immediately when already at destination and no further animation needed.
    • Uses bit 7 of actor_motion_state to differentiate turning vs walking.
    • Tail-calls step_actor_along_path when animation is required.

================================================================================

This routine manages an on-screen actor’s motion toward a “target waypoint” in
a grid or walkbox-based environment. Its goal is to keep the actor moving 
smoothly and updating its animation according to whether it is walking or turning.

1. Waypoint Comparison
   - First, it checks if the actor’s target waypoint differs from the currently
     active waypoint. If so, it needs to set up a new path; otherwise, it may
     just continue animation along the existing path.

2. Motion State & DDA Setup
   - If a new waypoint is installed:
       • The actor’s motion state is prepared by clearing the low nibble (motion
         sub-state) while preserving turning/facing info (high nibble).
       • The actor’s current X/Y is copied as the origin for the DDA (Digital
         Differential Analyzer) computation.
       • init_dda_for_path calculates step deltas and determines which axis
         is dominant (X or Y) along with directional bits.

3. Traversal Mask
   - The dominant axis and direction bits are converted into a single byte
     mask that guides the actor’s movement along the path. X- or Y-dominant
     movement is encoded separately, making downstream motion logic simpler.

4. Facing and Limbs
   - The routine resolves whether the actor is turning or facing a new direction
     (resolve_turning_and_facing) and sets a neutral (standing) limb set.
   - If the actor is not turning, walking limbs are activated. Otherwise, the
     routine exits early, leaving the actor in a standing/turning state.

5. Animation Handling
   - If the actor is already at the waypoint but flagged as “about-to-stop” or
     not yet at the exact X/Y position, it jumps to step_actor_along_path
     to continue the animation without altering path data.

Summary:
	- Handles both updating active waypoints and walking animation.
	- Uses bit flags and DDA outputs to coordinate movement and facing.
	- Ensures the actor’s limbs, facing, and motion state are consistent with
	  current path progress.
	- Efficiently skips work if actor is already at the target and not moving.
================================================================================
*/
* = $29f8
step_actor_motion:
        // ------------------------------------------------------------
        // Waypoint comparison section
		//
        // Verify if the target waypoint differs from the active one.
        // The comparison proceeds X first, then Y. If any mismatch
        // is detected, control jumps to set_new_active_waypoint to
        // reinitialize motion and DDA parameters.
        // ------------------------------------------------------------
        ldx     actor                           // X := actor index
        lda     actor_tgt_waypoint_x,x          // A := target X
        cmp     actor_cur_waypoint_x,x            
        bne     set_new_active_waypoint         // X differs → install new active
        lda     actor_tgt_waypoint_y,x          // A := target Y
        cmp     actor_cur_waypoint_y,x            
        bne     set_new_active_waypoint         // Y differs → install new active


        // ------------------------------------------------------------
        // Active waypoint unchanged. Check if actor should animate:
		//
        // - If low 7 bits = MOTION_ABOUT_TO_STOP → begin animation.
		// - Else fall through to position checks
        // ------------------------------------------------------------
        lda     actor_motion_state,x            // A := actor’s motion state
        and     #MSK_LOW7BITS                   // mask off turning flag (bit7)
        cmp     #MOTION_ABOUT_TO_STOP           // compare with “about to stop” code
        beq     begin_actor_animation           // equal → trigger walking animation

        // ------------------------------------------------------------
        // Verify if actor has reached active waypoint
		//
        // - Compare Y then X; branch to animation if position differs
        // - Return if already at target and not about-to-stop
        // ------------------------------------------------------------
        lda     actor_tgt_waypoint_y,x          // A := target Y
        cmp     actor_pos_y,x                   
        bne     begin_actor_animation           // Y differs → advance animation
        lda     actor_tgt_waypoint_x,x          // A := target X
        cmp     actor_pos_x,x                   
        bne     begin_actor_animation           // X differs → advance animation
        rts                                     // At waypoint and not about-to-stop → done

begin_actor_animation:
        jmp     step_actor_along_path      		// Tail-call walking animator to advance motion; no return here

        // ------------------------------------------------------------
        // Install new active waypoint
		//
        // - Copy target waypoint into current waypoint
        // - Prepares actor for motion/DDA toward new target
        // ------------------------------------------------------------		
set_new_active_waypoint:
        lda     actor_tgt_waypoint_x,x          
        sta     actor_cur_waypoint_x,x            // active_wpt_x := target X
        lda     actor_tgt_waypoint_y,x          
        sta     actor_cur_waypoint_y,x            // active_wpt_y := target Y

        // ------------------------------------------------------------
        // Set actor base state to “moving”
		//
        // - Preserve high nibble (turning/facing)
        // - Clear low nibble to mark movement start
        // ------------------------------------------------------------		
set_state_to_moving:
        ldx     actor                           // X := actor index
        lda     actor_motion_state,x            // A := current motion state
        and     #MSK_HIGH_NIBBLE                // keep high nibble (turning/facing), zero low nibble
        sta     actor_motion_state,x            // update motion state to “moving” baseline


        // ------------------------------------------------------------
        // Snapshot current position as origin for DDA traversal
		//
        // - X/Y copied to position_to_waypoint registers
        // ------------------------------------------------------------
        lda     actor_pos_x,x                   // A := current X
        sta     actor_path_probe_x,x
        lda     actor_pos_y,x                   // A := current Y
        sta     actor_path_probe_y,x

        // ------------------------------------------------------------
        // Initialize DDA for path to active waypoint
		//
        // - Computes step deltas, dominant axis, and direction bits
        // - Returns A = $00 if motion needed, $01 if already at target
        // - Save result to motion_needed for later merge into motion state
        // ------------------------------------------------------------
        jsr     init_dda_for_path               // derive ΔX, ΔY, dir bits, dominant axis
        ldx     actor                           // restore actor index
        sta     motion_needed                   // save motion-needed flag for state merge

        // ------------------------------------------------------------
        // Merge motion_needed flag into low nibble of motion state
		//
        // - Preserve high nibble (turning/facing)
        // - Low nibble now indicates whether movement is required
        // ------------------------------------------------------------
        lda     actor_motion_state,x            // A := current state
        and     #MSK_HIGH_NIBBLE                // clear low nibble
        ora     motion_needed                   // set low nibble from computed flag
        sta     actor_motion_state,x            // commit updated state

        // ------------------------------------------------------------
        // Determine traversal mask based on dominant axis and direction
		//
        // - dda_dominant_axis = 0 → X-dominant, else Y-dominant
        // - Branch to Y-dominant handling if necessary
        // ------------------------------------------------------------
        lda     dda_dominant_axis              // A := dominant-axis flag
        bne     process_y_dominant_direction   // ≠0 → handle Y-dominant

        // ------------------------------------------------------------
        // X-dominant: select traversal mask based on horizontal direction
		//
        // - dir_x_bit = 0 → left, else right
        // ------------------------------------------------------------
        lda     dir_x_bit                      // test X direction
        beq     x_direction_left               // 0 → left

        lda     #TRV_RT_MASK                   // X right mask
        jmp     set_delta_directions_for_actor
x_direction_left:
        lda     #TRV_LT_MASK                   // X left mask
        jmp     set_delta_directions_for_actor

        // ------------------------------------------------------------
        // Y-dominant: select traversal mask based on vertical direction
		//
        // - dir_y_bit = 0 → up, else down
        // ------------------------------------------------------------
process_y_dominant_direction:
        lda     dir_y_bit                      // test Y direction bit
        beq     y_direction_up                 // 0 → moving up

        lda     #TRV_DN_MASK                   // Y down mask
        jmp     set_delta_directions_for_actor
y_direction_up:
        lda     #TRV_UP_MASK                   // Y up mask

set_delta_directions_for_actor:
        // ------------------------------------------------------------
        // Store computed traversal mask for actor
		//
        // - actor_tgt_facing_direction[x] receives the mask
        // ------------------------------------------------------------
        sta     actor_tgt_facing_direction,x   // Store traversal mask for this actor (indexed by X)

		// Redundant code
		lda actor_box_fg_occlusion,X	
		cmp #$03		
		bne dummy_dn		
dummy_dn:
		jsr do_nothing_2C22

        // ------------------------------------------------------------
        // Resolve facing/turning and apply standing limb set
		//
        // - Updates motion_state for current orientation
        // - Sets neutral limbs before determining walking animation
        // ------------------------------------------------------------
        jsr     resolve_turning_and_facing         // update facing/turning flags in motion_state
        jsr     apply_standing_clip       // apply baseline limbs before deciding to walk

        // ------------------------------------------------------------
        // Check if actor is turning/standing
		//
        // - Bit7 = MOTION_FLAG_TURNING
        // - If clear → start walking clip
        // - If set → return without changing limbs
        // ------------------------------------------------------------
        lda     actor_motion_state,x               // A := motion_state
        and     #MOTION_FLAG_TURNING               // isolate turning flag (bit7)
        beq     init_walking_clip                  // 0 → not turning → start walking animation
        rts                                        // turning/standing → exit

init_walking_clip:
        // ------------------------------------------------------------
        // Initialize walking limbs for actor
		//
        // - Sets walking clip based on current facing/direction
        // ------------------------------------------------------------
        jsr     apply_walking_clip       // Select walking clip/limbs for current facing
		// Fall through to step_actor_along_path
/*
================================================================================
  step_actor_along_path
================================================================================
Summary
	Drive per-frame walking behavior for the current actor. Handles stop and
	about-to-stop gates, stand/turn resolution, entry into walking pose, regular
	in-box DDA stepping, and mode switches into walkbox traversal (horizontal or
	vertical) when a boundary is crossed.

Arguments
	None (routine uses the global actor index and per-actor state)

Vars/State
	 actor                                 current actor index
	 actor_motion_state                    per-actor motion mode/flags
	 actor_tgt_facing_direction       per-actor seeded traverse mask
	 motion_nibble_tmp                     scratch: holds low-nibble result
	 actor_dir_x_bit                       X direction bit: 0=left, ≠0=right
	 actor_dir_y_bit                       Y direction bit: 0=up,   ≠0=down

Global Inputs
	actor, actor_motion_state[actor], actor_dir_x_bit[actor], actor_dir_y_bit[actor]

Global Outputs
	actor_motion_state[actor] low nibble set to one of:
		actor_tgt_facing_direction[actor] seeded with:
			Actor limbs/facing updated via called helpers.

Returns
	Typical fall-through “still walking” path returns with A = #$00.
	Other exits may leave A unspecified.

Description
	• If motion == MOTION_STOPPED → return.
	• If low nibble == MOTION_ABOUT_TO_STOP → set_actor_stopped_and_render and return.
	• If standing/turning (bit7 set):
		– resolve_turning_and_facing, set standing limbs;
		– if still turning → return; else fall into walking pose setup.
	• Enter walking pose → apply_walking_clip.
	• Fast-path dispatch by low nibble:
		– MOTION_TRAVERSE_HORIZ → resolve_walkbox_with_step_x_then_y.
		– MOTION_TRAVERSE_VERT  → resolve_walkbox_with_step_y_then_x.
		– MOTION_WALKING        → regular in-box step.
		– otherwise → exit.
	• Regular in-box step:
		– Step X via DDA. If reached, step Y; if either axis reports walkbox
		change, switch to corresponding traverse mode; if both reached, set
		low nibble to MOTION_ABOUT_TO_STOP.
		– When switching to traverse, seed actor_tgt_facing_direction using
		the orthogonal direction bit (Y for horizontal traverse, X for vertical),
		update facing and limbs, and jump to the traverse handler.

Notes
	• Bit layout assumptions:
		– Low nibble encodes motion sub-mode.
		– Bit7 (MSK_TURN_STAND) marks stand/turn state.
		
================================================================================

This routine manages one walking update for a single actor. It keeps three
things aligned: the motion state (what phase they are in), the facing
direction (which way the sprite should point), and the animation clip
(standing vs walking). The result is movement that looks smooth instead of
snapping.

Entry checks:

	* If the motion state says "stopped" (#$02), nothing happens and the routine
	  returns immediately.
	* If the low nibble is "about to stop" (#$01), the routine resets animation
	  to a standing pose. This is the final frame before becoming fully stopped.

Standing and turning:

	* If the "standing/turning" flag (bit7) is set, the actor is not walking yet.
	  The routine calls resolve_turning_and_facing to reconcile the intended move
	  direction with the current facing. That logic may enforce a 90° pivot when
	  the actor tries to reverse along the same axis, which prevents abrupt 180°
	  flips. After that, a standing clip is applied. If the actor is still marked
	  as turning, the routine exits; otherwise it proceeds to walking.

Walking pose and dispatch:

	* When transitioning into motion, the routine sets the walking clip that
	  matches the current facing. It then inspects the low nibble of the motion
	  state to decide what kind of step to run:
		  * #$03 → traverse horizontally across a walkbox boundary
		  * #$04 → traverse vertically across a walkbox boundary
		  * #$00 → regular walking inside the current walkbox

Regular walking (inside one box):

	* The routine advances X first via step_actor_x_dda. That function returns:
		  * #$00: still moving on X
		  * #$01: X reached its target
		  * #$03: X crossed a walkbox boundary
		  
	* If X reached its target, the routine advances Y via step_actor_y_dda:
		  * If Y also reached, it marks "about to stop" (low nibble ← #$01) and
			returns. This sets up the next frame to become fully stopped.
		  * If Y reported a walkbox change, the routine resets the animation state
			to re-enter cleanly.
		  * Otherwise it returns, indicating partial progress this frame.
		  
	* If X reported a walkbox change, the routine switches to "horizontal
	  traversal" mode (low nibble ← #$03). It seeds a Y-based direction mask
	  (up or down) into actor_tgt_facing_direction, calls
	  resolve_turning_and_facing to update the facing, reapplies the walking
	  clip, and jumps to the horizontal traversal handler.
	  
	* If X is still in progress, the routine advances Y. If Y reports a walkbox
	  change, it switches to "vertical traversal" mode (low nibble ← #$04),
	  seeds an X-based direction mask (left or right), updates facing, reapplies
	  the walking clip, and jumps to the vertical traversal handler. If not, the
	  routine returns with "still walking" (#$00 in A).

Why the axis-based masks during traversal:

	* On boundary crossings the actor must travel along a specific axis to reach
	  the next walkable region. The routine writes a constrained direction mask
	  (e.g., up or down for horizontal boundary handling) to drive facing updates
	  and animation consistently during the traversal.

State integrity:

	* When writing the low nibble to switch modes, the routine preserves the high
	  nibble of actor_motion_state. This prevents unrelated flags from being
	  clobbered while it selects between regular walking, horizontal traversal,
	  vertical traversal, and about-to-stop.

Effect:

	* Characters pivot naturally, adopt the correct walking or standing clips,
	  and cross between walkboxes without visual pops. Movement, facing, and
	  animation stay synchronized frame to frame.
 ================================================================================
*/
* = $2A9A
step_actor_along_path:
        // ------------------------------------------------------------
        // Early stop check
		//
        //   Load motion state and test for MOTION_STOPPED.
        //   If stopped, return; otherwise continue to about-to-stop gate.
        // ------------------------------------------------------------
        ldx     actor                           // X := current actor index
        lda     actor_motion_state,x            // load actor’s motion state
        cmp     #MOTION_STOPPED                 // test for fully stopped state (#$02)
        bne     check_if_about_to_stop          // if not stopped → continue walking logic
        rts                                     // stopped → no update needed this frame

        // ------------------------------------------------------------
        // About-to-stop check
		//
        //   Examine the low nibble of the motion state.
        //   If it equals MOTION_ABOUT_TO_STOP, reset animation
        //   to a standing pose; otherwise continue to the stand/turn gate.
        // ------------------------------------------------------------
check_if_about_to_stop:
        and     #MSK_LOW_NIBBLE                 // isolate low nibble of motion state
        cmp     #MOTION_ABOUT_TO_STOP           // equals #$01 → transition to standing
        bne     stand_turn_gate                 // not about to stop → handle stand/turn gate
        jmp     set_actor_stopped_and_render     	// about-to-stop → reset limbs/flags to standing

        // ------------------------------------------------------------
        // Stand/turn gate
		//
        //   Test bit7 of motion state to detect standing/turning.
        //   Set path for turn resolution; otherwise take walking fast path.
        // ------------------------------------------------------------
stand_turn_gate:
        lda     actor_motion_state,x           // fetch motion flags
        and     #MSK_TURN_STAND                // test standing/turning bit (bit7)
        beq     walking_fastpath               // clear → not standing/turning; take walking path

        // ------------------------------------------------------------
        // Standing/turning update
		//
        //   Actor is stationary or mid-turn.
        //   Resolve current facing relative to movement delta and
        //   apply the proper standing animation until the turn completes.
        // ------------------------------------------------------------
        jsr     resolve_turning_and_facing     // reconcile facing with delta; may pivot or mark turning
        jsr     apply_standing_clip   		   // apply standing clip while turning/idle

        // ------------------------------------------------------------
        // Post-turn evaluation
		//
        //   After resolving facing and pose, check if actor remains
        //   in a standing or turning state. If still turning, exit and
        //   wait for next frame; otherwise continue to walking pose.
        // ------------------------------------------------------------
        lda     actor_motion_state,x           // reload motion state after facing/pose update
        and     #MSK_TURN_STAND                // test if bit7 (turn/stand) still set
        beq     enter_walking_pose             // clear → transition to walking pose
        rts                                    // still turning → wait next frame

        // ------------------------------------------------------------
        // Enter walking pose
		//
        //   Actor finished turning and is ready to walk.
        //   Apply the appropriate walking animation clip based on
        //   current facing direction, transitioning from idle/turn
        //   to active motion.
        // ------------------------------------------------------------
enter_walking_pose:
        jsr     apply_walking_clip    // apply walking animation clip based on facing

        // ------------------------------------------------------------
        // Walking fast-path dispatch
		//
        //   Read motion state low nibble and route:
        //     #MOTION_TRAVERSE_HORIZ → resolve_walkbox_with_step_x_then_y
        //     #MOTION_TRAVERSE_VERT → resolve_walkbox_with_step_y_then_x
		//     #MOTION_WALKING → regular in-box walking step logic
        //     else → leave routine.
		//
        //   Keeps high-level walking loop tight when a traversal is active.
        // ------------------------------------------------------------
walking_fastpath:
        ldx     actor                          // X := current actor index
		
		//Redundant code - replicated from original
		lda 	actor_box_fg_occlusion,x
		cmp		#$03
		bne		dummy_wfp
		//
		
dummy_wfp:		
        lda     actor_motion_state,x           // load motion state
        and     #MSK_LOW_NIBBLE                // isolate low nibble selector

        cmp     #MOTION_TRAVERSE_HORIZ         //check for horizontal traversal mode
        bne     dispatch_traverse_vert         // no → test vertical traversal
        jmp     resolve_walkbox_with_step_x_then_y      	   // yes → run horizontal traversal handler

dispatch_traverse_vert:
        cmp     #MOTION_TRAVERSE_VERT          // check for vertical traversal mode
        bne     dispatch_regular_motion        // not vertical → test for regular walking
        jmp     resolve_walkbox_with_step_y_then_x        	   // vertical traversal → handle boundary crossing

dispatch_regular_motion:
        cmp     #MOTION_WALKING                // check for regular walking mode
        beq     step_regular_motion            // yes → run per-axis DDA stepping
        jmp     exit_saap                      // no → leave routine (no walking step this frame)

        // ------------------------------------------------------------
        // Regular in-box walking step
		//
        //   Advance X first via DDA:
        //     00 → still moving on X
        //     01 → X reached; then step Y
        //     03 → X walkbox change → handled later
		//
        //   If X reached, step Y:
        //     00 → Y still moving
        //     01 → both axes reached → mark about-to-stop elsewhere
        //     04 → Y walkbox change → handled later
        // ------------------------------------------------------------
step_regular_motion:
        jsr     step_actor_x_dda               // step X axis; A ∈ {00=cont, 01=reached, 03=wbox change}
        cmp     #XAXIS_RESULT_REACHED          // X reached target?
        bne     x_not_reached_path             // no → handle X in-progress or X wbox change

        jsr     step_actor_y_dda               // X done; step Y axis; A ∈ {00=cont, 01=reached, 04=wbox change}
        cmp     #YAXIS_RESULT_REACHED          // Y reached target too?
        bne     y_not_reached_after_x          // no → handle Y in-progress or Y wbox change

        // ------------------------------------------------------------
        // Both axes reached → transition to about-to-stop
		//
        //   Preserve high nibble of motion state and set low nibble
        //   to MOTION_ABOUT_TO_STOP (#$01). Commit and return.
        // ------------------------------------------------------------
        ldx     actor                          // X := current actor index
        sta     motion_nibble_tmp              // save Y result (#$01)
        and     #MSK_HIGH_NIBBLE               // keep high nibble of motion state
        ora     motion_nibble_tmp              // combine with low nibble = about-to-stop
        sta     actor_motion_state,x           // commit updated motion state
        rts                                    // done for this frame

        // ------------------------------------------------------------
        // Y-axis progress after X reached
		//
        //   X target reached; evaluate Y step result.
        //   If Y crossed a walkbox boundary, reset animation to reenter
        //   cleanly. Otherwise return with partial progress (Y only).
        // ------------------------------------------------------------
y_not_reached_after_x:
        // X reached, Y still in progress
        cmp     #YAXIS_RESULT_WBOX_CHANGED     // did Y cross a walkbox boundary?
        bne     return_vertical_progress_only  // no → keep current walking state
        jmp     set_actor_stopped_and_render       // yes → reenter cleanly with standing reset

return_vertical_progress_only:
        rts                                    // partial progress this frame (Y only)

        // ------------------------------------------------------------
        // X-axis evaluation and horizontal traversal switch
		//
        //   If X step reports a walkbox change, enter horizontal
        //   traversal mode (low nibble = #$03) while preserving the
        //   high nibble flags. Otherwise continue with Y updates.
        // ------------------------------------------------------------
x_not_reached_path:
        // X not yet at target
        cmp     #XAXIS_RESULT_WBOX_CHANGED     // boundary crossed on X?
        bne     no_horiz_traverse_continue_y   // no → keep updating Y within box

        // X walkbox changed → enter horizontal traversal (low nibble = #$03)
        sta     motion_nibble_tmp              // save X result code (#$03)
        lda     actor_motion_state,x           // fetch current motion state
        and     #MSK_HIGH_NIBBLE               // preserve high nibble flags
        ora     motion_nibble_tmp              // set low nibble to traverse-horiz
        sta     actor_motion_state,x           // commit new mode

        // ------------------------------------------------------------
        // Seed traversal direction from Y bit
		//
        //   Read actor_dir_y_bit to choose UP or DOWN mask, write the
        //   mask into actor_tgt_facing_direction, then proceed to
        //   update facing and continue horizontal traversal.
        // ------------------------------------------------------------
        lda     actor_dir_y_bit,x               	// read Y direction bit (0=up, ≠0=down)
        beq     set_dirmask_up                  	// 0 → set UP mask

        lda     #TRV_DN_MASK                    	// Y bit set → choose DOWN traversal mask
        sta     actor_tgt_facing_direction,x 	// seed path delta with DOWN
        jmp     commit_y_dirmask_and_walk       	// update facing and continue walking

set_dirmask_up:
        lda     #TRV_UP_MASK                   	  	// choose UP traversal mask
        sta     actor_tgt_facing_direction,x 	// seed path delta with UP

        // ------------------------------------------------------------
        // Commit Y-based traversal and continue horizontally
        //   Use the seeded UP/DOWN mask to resolve facing, apply the
        //   walking clip for the new facing, then jump to horizontal
        //   walkbox traversal to complete the boundary crossing.
        // ------------------------------------------------------------
commit_y_dirmask_and_walk:
        jsr     resolve_turning_and_facing     	// adjust facing based on new Y-axis direction
        jsr     apply_walking_clip    			// apply walking animation for updated facing
        jmp     resolve_walkbox_with_step_x_then_y      		// proceed with horizontal walkbox traversal

        // ------------------------------------------------------------
        // Continue Y update when X didn't trigger traversal
		//
        //   Step Y within current box. If Y crosses a walkbox boundary,
        //   branch to vertical traversal handler; otherwise report
        //   "still walking" status and return.
        // ------------------------------------------------------------
no_horiz_traverse_continue_y:
        // Continue with Y-axis step inside current box
        jsr     step_actor_y_dda               	// A ∈ {00=cont, 01=reached, 04=wbox change}
        cmp     #YAXIS_RESULT_WBOX_CHANGED     	// boundary crossed on Y?
        beq     y_walkbox_changed              	// yes → handle vertical traversal

        // No boundary and not at goal → report "still walking"
        lda     #$00                           	// status: continue walking
        rts

		//Unreachable code - replicated from original
		jmp 	commit_x_dirmask_and_walk
		
        // ------------------------------------------------------------
        // Y walkbox boundary crossing
		//
        //   Enter vertical traversal mode (low nibble ← #$04) while
        //   preserving high-nibble flags. Store result code, then
        //   update actor_motion_state accordingly.
        // ------------------------------------------------------------
y_walkbox_changed:
        // Y walkbox changed → enter vertical traversal (low nibble = #$04)
        sta     motion_nibble_tmp              // save Y result code (#$04)
        lda     actor_motion_state,x           // fetch current motion state
        and     #MSK_HIGH_NIBBLE               // preserve high nibble flags
        ora     motion_nibble_tmp              // set low nibble to traverse-vert
        sta     actor_motion_state,x           // commit new mode

        // ------------------------------------------------------------
        // Seed traversal direction from X bit
		//
        //   Read actor_dir_x_bit to choose LEFT or RIGHT mask, write
        //   it into actor_tgt_facing_direction, then proceed to
        //   resolve facing and continue vertical traversal.
        // ------------------------------------------------------------
        lda     actor_dir_x_bit,x               // read X direction bit (0=left, ≠0=right)
        beq     set_dirmask_left                // 0 → choose LEFT mask

        lda     #TRV_RT_MASK                    // X bit set → choose RIGHT traversal mask
        sta     actor_tgt_facing_direction,x // seed path delta with RIGHT
        jmp     commit_x_dirmask_and_walk       // update facing and continue walking

set_dirmask_left:
        lda     #TRV_LT_MASK                   // choose LEFT traversal mask
        sta     actor_tgt_facing_direction,x // seed path delta with LEFT

        // ------------------------------------------------------------
        // Commit X-based traversal and continue vertically
		//
        //   Use the seeded LEFT/RIGHT mask to resolve facing, apply the
        //   walking clip for the updated facing, then jump to vertical
        //   walkbox traversal to complete the boundary crossing.
        // ------------------------------------------------------------
commit_x_dirmask_and_walk:
        jsr     resolve_turning_and_facing     	// update facing based on new X-axis direction
        jsr     apply_walking_clip    			// apply walking animation for updated facing
        jmp     resolve_walkbox_with_step_y_then_x        		// proceed with vertical walkbox traversal
exit_saap:
        rts                                     // unreachable (kept for alignment/consistency)
/*
================================================================================
  resolve_turning_and_facing
================================================================================

Summary
	Reconcile an actor’s facing with step-delta input. Apply a one-shot
	override if present, clear turning when aligned, or set turning and
	pick a transition (snap or 90° pivot) when directions differ.

Arguments
	None

Global Inputs
	actor                                Current actor index selector
	actor_box_facing_override[*]         One-shot override: flag + dir bits
	actor_tgt_facing_direction[*]   Intended facing from path step
	actor_cur_facing_direction[*]          Current facing attribute
	actor_motion_state[*]                Motion flags (bit7 = turning)

Global Outputs
	actor_tgt_facing_direction[*]   Updated from override when used
	actor_cur_facing_direction[*]          New facing attribute committed
	actor_motion_state[*]                Turning bit set/cleared accordingly

Description
	- If override flag set: copy masked override into delta and continue.
	- If delta equals current facing: clear turning and exit.
	- If already turning: snap facing to delta and keep turning set.
	- If not turning:
		• Same axis (e.g., L↔R or U↔D): force perpendicular pivot by
		toggling axis and clearing sign, then mark turning.
		• Different axis: adopt delta directly, then mark turning.

Notes
	- Clobbers: A, X, flags.
	- Uses axis_bit_scratch as a transient compare byte.
	- Zero/invalid deltas resolve via the same mask logic.
================================================================================
*/
* = $2B70
resolve_turning_and_facing:
        // ------------------------------------------------------------
        // One-shot facing override:
        //   If FACING_OVERRIDE_BIT set in actor_box_facing_override[X],
        //   clear that bit (mask with FACING_OVERRIDE_CLEAR_MASK) and copy
        //   the masked value into actor_tgt_facing_direction[X].
        //   Otherwise skip to direction compare.
        // ------------------------------------------------------------
        ldx     actor                          // X := actor index
        lda     actor_box_facing_override,x    // read override attr
        and     #FACING_OVERRIDE_BIT           // test one-shot flag
        beq     compare_dir_masks              // flag clear → no override

        lda     actor_box_facing_override,x    // load override attr (contains one-shot flag + direction bits)
        and     #FACING_OVERRIDE_CLEAR_MASK    // clear one-shot flag; keep direction bits to propagate
        sta     actor_tgt_facing_direction,x // push masked direction into this step’s delta input


		// ------------------------------------------------------------
		// If delta equals current, clear turning bit and exit
		// ------------------------------------------------------------
compare_dir_masks:
        lda     actor_tgt_facing_direction,x 	// load intended facing (delta)
        cmp     actor_cur_facing_direction,x        // compare with current facing attribute
        beq     clear_actor_turning               	// equal → no change; clear turning flag


        // ------------------------------------------------------------
        // Directions differ:
        //   If already turning, snap facing to delta.
        //   Else choose transition and mark turning.
        // ------------------------------------------------------------
        lda     actor_motion_state,x           // load motion flags
        bmi     begin_turning_transition       // bit7=1 → already turning

        // ------------------------------------------------------------
        // Not turning yet
		//
        // Compare direction axes to decide turning transition
        //   If current and intended directions share the same axis
        //   (both horizontal or both vertical), enforce a 90° pivot
        //   by toggling the axis bit. Otherwise, adopt the new delta.
        // ------------------------------------------------------------
        lda     actor_cur_facing_direction,x     	// load current facing attribute
        and     #DIR_AXIS_BIT                  		// isolate axis (0=horizontal, 1=vertical)
        sta     axis_bit_scratch               		// save attribute’s axis for comparison
        lda     actor_tgt_facing_direction,x 	// load intended facing (delta)
        and     #DIR_AXIS_BIT                     	// isolate axis: 0=horizontal, 1=vertical
        cmp     axis_bit_scratch                  	// compare to current attribute’s axis
        beq     force_axis_flip                   	// same axis → enforce 90° pivot
        lda     actor_tgt_facing_direction,x 	// adopt delta as new facing value
        jmp     write_facing_attribute            	// commit facing and set turning flag


        // ------------------------------------------------------------
        // Force perpendicular axis flip
		//
        //   Used when actor tries to reverse along the same axis
        //   (e.g., left↔right or up↔down). Toggles DIR_AXIS_BIT to
        //   pivot 90° and clears the sign bit to ensure a consistent
        //   facing direction before committing the new attribute.
        // ------------------------------------------------------------
force_axis_flip:
        lda     actor_cur_facing_direction,x 	// load current facing attribute
        eor     #DIR_AXIS_BIT              		// flip axis bit to force perpendicular pivot
        and     #DIR_SIGN_CLEAR_MASK       		// clear sign bit → canonical positive direction
        jmp     write_facing_attribute     		// commit facing; turning flag set in callee

        // ------------------------------------------------------------
        // Handle active turning state
        //   If actor is already turning, snap facing direction directly
        //   to the delta. Then write the resulting facing attribute.
        // ------------------------------------------------------------
begin_turning_transition:
        lda     actor_tgt_facing_direction,x 	// already turning → snap facing to delta

write_facing_attribute:
        sta     actor_cur_facing_direction,x        // commit new facing attribute

        // ------------------------------------------------------------
        // Mark actor as turning
        //   Sets MOTION_FLAG_TURNING in actor_motion_state[X] to
        //   indicate the actor is mid-turn. Then jumps to the exit.
        // ------------------------------------------------------------
        lda     actor_motion_state,x           // load motion flags
        ora     #MOTION_FLAG_TURNING           // set turning bit
        sta     actor_motion_state,x           // store updated flags
        jmp     exit_update_motion_state       // unified exit

        // ------------------------------------------------------------
        // Clear turning flag and exit
        //   Resets MOTION_FLAG_TURNING in actor_motion_state[X] when
        //   facing direction already matches delta. Exits immediately
        //   afterward via a unified return point.
        // ------------------------------------------------------------
clear_actor_turning:
        lda     actor_motion_state,x           // load motion flags
        and     #MOTION_FLAG_CLEAR_TURNING     // clear turning bit
        sta     actor_motion_state,x           // store updated flags

exit_update_motion_state:
        rts                                    
/*
================================================================================
  do_nothing_2C22
================================================================================
*/
* = $2C22		
do_nothing_2C22:		
		rts		
/*
================================================================================
  set_actor_stopped_and_render
================================================================================
Summary
	Reset the current actor’s motion state to a standing, stopped state 
	and flag a redraw.

Arguments
	actor                       Current actor index (global selects X)

Global Inputs
	actor                       Actor index selector
	actor_render_flags[*]    	Read to preserve existing bits before OR

Global Outputs
	actor_motion_state[*]       Set to MOTION_STOPPED_CODE
	actor_render_flags[*]    	OR with ACTOR_RENDER_VISIBLE_AND_REFRESH to request redraw

Description
	- Select standing limbs for the current actor.
	- Set motion state to stopped.
	- Mark animation state for refresh and request a redraw via OR flags.
	- No position or box state is modified.

Notes
	- Clobbers: A, X.
	- A ends holding the updated actor_render_flags[X] value.
	- Processor flags reflect the final ORA.
================================================================================
*/
* = $2C23
set_actor_stopped_and_render:
		// ------------------------------------------------------------
		// Select standing pose for current actor
		// ------------------------------------------------------------
		ldx     actor
		jsr     apply_standing_clip


		// ------------------------------------------------------------
		// Set motion state to stopped
		// ------------------------------------------------------------
		lda     #MOTION_STOPPED_CODE
		sta     actor_motion_state,x

		// ------------------------------------------------------------
		// Mark animation refresh and request redraw
		// ------------------------------------------------------------
		lda     actor_render_flags,x
		ora     #ACTOR_RENDER_VISIBLE_AND_REFRESH
		sta     actor_render_flags,x
		rts
