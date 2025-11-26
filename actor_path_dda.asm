/*
================================================================================
  Actor path DDA: fixed-point stepping and speculative walkbox validation
================================================================================
Summary
    Provides DDA-based per-pixel stepping from an actor’s current probe position
    to its active waypoint using only integer arithmetic. Encodes direction and
    dominant axis, maintains separate X/Y accumulators, and brackets each DDA
    step with a snapshot/restore pair so walkbox classification can veto or
    accept speculative motion safely.

Responsibilities
    • Initialize DDA parameters for the current actor/waypoint pair:
        - Compute |ΔX|, |ΔY| from probe → waypoint.
        - Derive horizontal/vertical direction bits (left/right, up/down).
        - Choose a dominant axis and dominant magnitude max(|ΔX|,|ΔY|).
        - Seed X/Y accumulators with their absolute deltas.
    • Advance the actor along X or Y using fixed-point accumulators:
        - Each axis adds |Δ| into its accumulator and emits at most one ±1px step
          when the value reaches the dominant magnitude.
        - After a step, accumulators are reduced by the dominant to keep the
          |ΔX|:|ΔY| ratio stable at pixel granularity.
    • Protect world state while probing walkbox geometry:
        - Swap probe coordinates into the live actor position before resolving
          walkboxes.
        - On failure or backout, restore saved position, box index, attribute,
          and path-update flag exactly.
        - On success, the probe position becomes the new committed actor
          position.

Data flow
    • Inputs:
        - actor_path_probe_x/y[]: DDA origin / current probe coordinates.
        - actor_cur_waypoint_x/y[]: active waypoint coordinates.
        - Per-actor DDA state: actor_dir_*_bit[], actor_dx/dy_abs[],
          actor_dda_dom_abs[], actor_dx/dy_accum[].
    • Internal temporaries:
        - dir_x_bit / dir_y_bit: direction bits derived from signed deltas.
        - dda_dx_abs / dda_dy_abs: scalar absolute deltas.
        - dda_dominant_axis / dda_dominant_abs: axis/magnitude for DDA scale.
        - dda_x_accum_seed / dda_y_accum_seed: first-frame accumulator seeds.
        - saved_* slots: snapshots of position, box, and path flags.
    • Outputs:
        - Updated probe and committed positions (on successful steps).
        - Updated accumulators and per-actor DDA fields.
        - Status codes indicating “continue”, “reached”, or “walkbox changed”
          per axis.

Key routines
    • init_dda_for_path
        - Compares waypoint vs probe, computes |ΔX|/|ΔY| and direction bits,
          chooses dominant axis and magnitude, seeds accumulators, and publishes
          all DDA parameters to per-actor tables.
        - Returns PATH_RET_NO_MOVE when both deltas are zero, otherwise
          PATH_RET_MOVE_NEEDED.
    • step_actor_x_dda
        - Accumulates |ΔX|, emits at most one ±1px horizontal step on the probe
          X coordinate, runs walkbox validation via snapshot/restore, and
          reports axis status:
              XAXIS_RESULT_CONTINUE
              XAXIS_RESULT_REACHED
              XAXIS_RESULT_WBOX_CHANGED.
    • step_actor_y_dda
        - Symmetric to X: accumulates |ΔY|, emits at most one ±1px vertical
          step, performs walkbox validation, interprets a special |ΔY| == $FF
          sentinel as a vertical-only backout, and reports:
              YAXIS_RESULT_CONTINUE
              YAXIS_RESULT_REACHED
              YAXIS_RESULT_WBOX_CHANGED.
    • save_actor_path_state / restore_actor_path_state
        - Bracket speculative motion. Save restores all path-related state
          (position, box attribute/index, path-update flag) and swap probe
          coordinates into live actor_pos_*. Restore restores those fields and
          mirrors committed coordinates back into the probe copies.

Notes
    • X and Y are stepped independently but share the same dominant magnitude,
      which ensures their per-frame cadence respects the original |ΔX|:|ΔY|
      ratio.
    • All world-visible motion is mediated by walkbox classification; failed
      probes never leak into actor_pos_* thanks to the snapshot/restore pairing.
    • Axis-specific status codes allow higher-level motion code to decide when
      to stop, when to traverse along walkbox edges, and when to recompute
      paths after a box change.
	  
================================================================================

 Digital Differential Analyzer (DDA) - integer stepping for smooth motion
================================================================================

	DDA generates proportionate X/Y steps toward a waypoint using only integer
	arithmetic. It maintains per-axis accumulators seeded with the absolute
	deltas to the target. Each frame it compares the accumulator(s) against the
	dominant delta magnitude and issues a one-pixel step on an axis when the
	threshold is met, then subtracts the dominant magnitude to preserve ratio.

Why it exists
	* Smooth diagonal motion without floats.
	* Fixed cost per frame; deterministic on 8-bit CPUs.
	* Preserves the ΔX:ΔY ratio at pixel granularity.

Core quantities
	* |ΔX|, |ΔY|: absolute deltas to the waypoint.
	* dominant := max(|ΔX|, |ΔY|) for scale.
	* accum_x, accum_y: step accumulators.
	* dir_x, dir_y: 1-bit signs (X: 0=left,1=right; Y: 0=up,1=down).

Algorithm (per waypoint activation)
	1. Compute |ΔX|, |ΔY| and dir_x/dir_y from position vs target.
	2. dominant := max(|ΔX|, |ΔY|).
	3. Seed accum_x := |ΔX|, accum_y := |ΔY|.
	4. Each frame:
	   a) accum_x += |ΔX|; if accum_x ≥ dominant → step ±1 on X; accum_x -= dominant.
	   b) accum_y += |ΔY|; if accum_y ≥ dominant → step ±1 on Y; accum_y -= dominant.
	   c) Stop when position == target on both axes.

Properties
	* If |ΔX|=0 → vertical motion only; if |ΔY|=0 → horizontal only.
	* If both zero → no motion needed.
	* The larger delta defines cadence (dominant axis sets the period).
	* Order of X/Y tests can be tuned around collision/walkbox checks.

Mapping to variables
	* |ΔX| → dda_dx_abs; |ΔY| → dda_dy_abs.
	* dominant → dda_dominant_abs.
	* accum_x → actor_dx_accum; accum_y → actor_dy_accum.
	* dir_x → actor_dir_x_bit; dir_y → actor_dir_y_bit.
	* Horizontal/vertical dominance flag → dda_dominant_axis.

Edge cases
	* Reaching one axis early: continue stepping the other until both match.
	* Walkbox invalidation: revert tentative step and treat as box-change.
	* Zero-motion guard: return “no move” before seeding accumulators.

Complexity
	* O(1) time and memory per frame; all integer ops, branch-predictable on 6502.
	
================================================================================
*/
#importonce
#import "globals.inc"
#import "constants.inc"
#import "actor_motion_constants.inc"
#import "actor_walkbox_traversal.asm"

.label dda_dy_abs                = $CB61  // |ΔY| to waypoint (pixels)
.label dda_dx_abs                = $CB62  // |ΔX| to waypoint (pixels)
.label dda_dominant_abs          = $CB63  // max(|ΔX|,|ΔY|) used for DDA scaling
.label dda_x_accum_seed          = $CB64  // DDA X accumulator seed
.label dda_y_accum_seed          = $CB65  // DDA Y accumulator seed

/*
================================================================================
  step_actor_x_dda
================================================================================
Summary
    Advance the actor’s horizontal probe position using DDA. Accumulates |ΔX|
    into a fixed-point accumulator, emits at most one ±1 pixel step on X when
    the dominant threshold is reached, validates the tentative position against
    walkboxes, and reports whether X reached the waypoint or triggered a box
    change.

Vars/State
    actor                         current actor index
    dir_x_bit                     per-call horizontal direction bit (0=left, 1=right)
    dda_dx_abs                    absolute horizontal delta |ΔX|
    dda_dominant_abs              dominant DDA magnitude max(|ΔX|,|ΔY|)
    actor_dx_accum[]              per-actor X accumulator (fractional progress)
    actor_path_probe_x[]          per-actor X probe coordinate
    actor_cur_waypoint_x[]        active waypoint X coordinate
    actor_box_attr[]              current walkbox attribute tag
    actor_box_cur[]               current walkbox index
    actor_path_update_flag[]      path recompute flag

Global Inputs
    actor                         current actor index
    actor_dx_abs[]                per-actor |ΔX| to waypoint
    actor_dda_dom_abs[]           per-actor dominant magnitude max(|ΔX|,|ΔY|)
    actor_dir_x_bit[]             per-actor horizontal direction bit
    actor_cur_waypoint_x[]        per-actor waypoint X coordinate

Global Outputs
    actor_dx_accum[]              updated X accumulator after this frame
    actor_path_probe_x[]          updated probe X after any 1px step
    actor_pos_x[]                 indirectly updated via save/restore when step is accepted
    actor_box_attr[]              updated walkbox attribute after resolution
    actor_box_cur[]               updated walkbox index after resolution
    actor_path_update_flag[]      may be set if box classification changes

Returns
    A 	XAXIS_RESULT_CONTINUE     X has not yet reached waypoint; box resolved
        XAXIS_RESULT_REACHED      X probe equals waypoint X; box resolved
        XAXIS_RESULT_WBOX_CHANGED walkbox unresolved/changed on this tentative X step

Description
    • Add |ΔX| to the actor’s X accumulator to track sub-pixel progress.
    • If accumulated value is still below the dominant magnitude, store the new
      accumulator and exit with CONTINUE (no pixel step emitted).
    • If accumulated value meets or exceeds the dominant magnitude:
        – Adjust the X probe by ±1 pixel based on dir_x_bit (left/right).
        – Subtract the dominant magnitude from the accumulator and store the
          residual for the next frame.
    • Snapshot position/box/path state, then call the walkbox resolver using
      the probe coordinates as the live position.
        – If resolution is UNDETERMINED, restore the snapshot and return
          WBOX_CHANGED.
        – If resolution succeeds, compare the updated probe X against the
          waypoint X:
              · equal   → return REACHED
              · unequal → return CONTINUE

Notes
    This routine only steps and validates the X axis. Y motion is handled
    separately by step_actor_y_dda. Both use the same dominant magnitude and
    accumulator scheme so the combined motion preserves the |ΔX|:|ΔY| ratio at
    pixel granularity while respecting walkbox constraints.
	
================================================================================

DDA stepping

- Each frame adds the X delta (|ΔX|) to a running accumulator that tracks
  fractional progress between pixels. When the total equals or exceeds the
  dominant motion magnitude (the larger of |ΔX| and |ΔY|), the actor moves
  exactly one pixel left or right and keeps the leftover fraction as residual.

- After each tentative move, the routine snapshots the actor’s position and
  calls resolve_actor_walkbox_and_behavior to verify that the new position still lies
  within a valid walkbox. If it doesn’t, the previous state is restored and the
  function reports a “walkbox changed” result.

- If the new X coordinate matches the waypoint’s X, the actor has reached its
  horizontal target and the routine returns “reached.” Otherwise, it returns
  “continue” so the motion can proceed next frame.

In short:
    • Handles per-pixel horizontal stepping with sub-pixel precision.
    • Prevents invalid movement across walkbox boundaries.
    • Returns one of three possible results:
          XAXIS_RESULT_CONTINUE  		→ continue stepping
          XAXIS_RESULT_REACHED  		→ reached waypoint
          XAXIS_RESULT_WBOX_CHANGED  	→ walkbox changed or invalid position
================================================================================
*/
* = $2E8C
step_actor_x_dda:
        // ------------------------------------------------------------
        // Accumulate X progress in fixed-point: accumX += |ΔX| (held in A)
        // ------------------------------------------------------------
        ldx     actor                        
        lda     actor_dx_accum,x             // A := current X accumulator
        clc                                  
        adc     actor_dx_abs,x               // A := accumX + |ΔX| toward waypoint

        // ------------------------------------------------------------
        // If accumulated < dominant(|ΔX|,|ΔY|) → no pixel step this frame; just store residual
        // ------------------------------------------------------------
        cmp     actor_dda_dom_abs,x          
        bcc     commit_x_accumulator         

        // ------------------------------------------------------------
        // accumulated ≥ dominant → emit exactly one X pixel using dir_x_bit (0=left, 1=right)
        // ------------------------------------------------------------
        ldy     actor_dir_x_bit,x            // Y := horizontal direction flag
        bne     step_right_1px               // nonzero → move right (INC); else left (DEC)

		// ------------------------------------------------------------
		// Apply one-pixel X movement based on direction
		//
		// - dir_x_bit = 0 → step left (DEC)
		// - dir_x_bit = 1 → step right (INC)
		//
		// The update affects only the waypoint X copy; world position is
		// validated later through walkbox resolution.
		// ------------------------------------------------------------
        // One-pixel step to the left on the probe coordinate
        dec     actor_path_probe_x,x         
        jmp     adjust_accumulated_after_motion_x  

step_right_1px:
        // One-pixel step to the right on the probe coordinate
        inc     actor_path_probe_x,x         

adjust_accumulated_after_motion_x:
		// ------------------------------------------------------------
        // Convert “accumulated” into residual: residual := accumulated − dominant
		// ------------------------------------------------------------
        sec                                   
        sbc     actor_dda_dom_abs,x           
		
commit_x_accumulator:
        // Persist the updated accumulator (residual) for the next frame’s accumulation
        sta     actor_dx_accum,x              

		// ------------------------------------------------------------
        // Snapshot and validate tentative X: walkbox resolver sees probe coords as live
		// ------------------------------------------------------------
        jsr     save_actor_path_state         // save pos/box/flags and swap in probe coords
		
        jsr     resolve_actor_walkbox_and_behavior    // classify box for the tentative X
        cmp     #BOX_RESULT_UNDETERMINED      // geometry invalid or aborted scan?		
        bne     waypoint_x_compare            // resolved → go test waypoint hit
		
        jsr     restore_actor_path_state      // unresolved → rollback snapshot completely
        lda     #XAXIS_RESULT_WBOX_CHANGED    // signal that X step caused box change
        rts                                   

		// ------------------------------------------------------------
        // Compare tentative probe X against waypoint X and choose result code
		//
		// - If tentative X == waypoint X → return REACHED
		// - Else → return CONTINUE
		// ------------------------------------------------------------
waypoint_x_compare:
        lda     actor_path_probe_x,x          // A := tentative probe X after step (or unchanged)
        cmp     actor_cur_waypoint_x,x        // reached X?
        bne     continue_x_traversal          // if not → still approaching target

        lda     #XAXIS_RESULT_REACHED         // X axis has reached waypoint coordinate
        rts                                   

continue_x_traversal:
        lda     #XAXIS_RESULT_CONTINUE        // X axis still in transit toward waypoint
        rts                                   		
/*
================================================================================
  step_actor_y_dda
================================================================================
Summary
    Advance the actor’s vertical probe position using DDA. Accumulates |ΔY|
    into a fixed-point accumulator, emits at most one ±1 pixel vertical step
    when the dominant threshold is reached, validates the tentative position
    through the walkbox resolver, and reports whether Y reached the waypoint
    or triggered a walkbox change/backout.

Vars/State
    actor                         current actor index
    dir_y_bit                     per-call vertical direction bit (0=up, 1=down)
    dda_dy_abs                    absolute vertical delta |ΔY|
    dda_dominant_abs              dominant DDA magnitude max(|ΔX|,|ΔY|)
    actor_dy_accum[]              per-actor Y accumulator (sub-pixel progress)
    actor_path_probe_y[]          per-actor Y probe coordinate
    actor_cur_waypoint_y[]        active waypoint Y coordinate
    actor_box_attr[]              current walkbox attribute tag
    actor_box_cur[]               current walkbox index
    actor_path_update_flag[]      path recompute flag

Global Inputs
    actor                         current actor index
    actor_dy_abs[]                |ΔY| to waypoint
    actor_dda_dom_abs[]           dominant magnitude for X/Y DDA
    actor_dir_y_bit[]             vertical direction (0=up, 1=down)
    actor_cur_waypoint_y[]        waypoint Y coordinate

Global Outputs
    actor_dy_accum[]              updated Y accumulator after this frame
    actor_path_probe_y[]          updated probe Y after any 1px step
    actor_pos_y[]                 indirectly updated via save/restore on accept
    actor_box_attr[]              updated walkbox attribute after resolution
    actor_box_cur[]               updated walkbox index after resolution
    actor_path_update_flag[]      may be set on box classification change

Returns
    A = YAXIS_RESULT_CONTINUE     Y has not yet reached waypoint; box resolved
        YAXIS_RESULT_REACHED      Y probe equals waypoint Y; box resolved
        YAXIS_RESULT_WBOX_CHANGED walkbox unresolved/changed or vertical-only backout

Description
    • Add |ΔY| into the vertical accumulator to track fractional progress.
    • If accumulated < dominant, emit no pixel step and only store residual.
    • If accumulated ≥ dominant:
        – Move probe Y by ±1 pixel depending on dir_y_bit (up/down).
        – Subtract dominant to produce the next-frame residual.
    • Snapshot the full position/box/path state and validate tentative Y with
      resolve_actor_walkbox_and_behavior.
        – If resolution is UNDETERMINED, rollback snapshot and return
          YAXIS_RESULT_WBOX_CHANGED.
    • A special case: |ΔY| == $FF is a vertical-only backout sentinel. If seen,
      undo the speculative step and also return YAXIS_RESULT_WBOX_CHANGED.
    • If resolution succeeds, compare probe Y to the waypoint Y:
          equal   → YAXIS_RESULT_REACHED
          unequal → YAXIS_RESULT_CONTINUE

Notes
    This routine handles only the vertical axis. Combined with step_actor_x_dda,
    both axes advance in proportion to |ΔX|:|ΔY| at pixel granularity while
    respecting walkbox constraints. Vertical-only backout ($FF) supports escape
    from corner/overlap conditions in ladder/diagonal semantics.
================================================================================
*/
* = $2ECD
step_actor_y_dda:
        // ------------------------------------------------------------
        // Accumulate Y progress in fixed-point: accumY += |ΔY| (held in A)
        // ------------------------------------------------------------
        lda     actor_dy_accum,x            // A := current Y accumulator
        clc                                 
        adc     actor_dy_abs,x              // A := accumY + |ΔY| toward waypoint

        // ------------------------------------------------------------
        // If accumulated < dominant(|ΔX|,|ΔY|) → no pixel step; only store updated residual
        // ------------------------------------------------------------
        cmp     actor_dda_dom_abs,x         // compare accumulated Y vs dominant magnitude
        bcc     commit_y_accumulator        // below threshold → no 1px emission this frame

        // ------------------------------------------------------------
        // accumulated ≥ dominant → emit exactly one Y pixel based on dir_y_bit (0=up, 1=down)
        // ------------------------------------------------------------
        ldy     actor_dir_y_bit,x           // Y := vertical direction flag
        bne     step_down                   // nonzero → move down (INC); else up (DEC)

        // One-pixel step up on the probe coordinate (screen Y increases downward)
        dec     actor_path_probe_y,x        
        jmp     adjust_accumulated_after_motion  

step_down:
        // One-pixel step down on the probe coordinate (screen Y increases downward)
        inc     actor_path_probe_y,x        

adjust_accumulated_after_motion:
        // ------------------------------------------------------------
        // Convert “accumulated” into residual: residual := accumulated − dominant
        // ------------------------------------------------------------
        sec                                 
        sbc     actor_dda_dom_abs,x         // A := accumulated − dominant

commit_y_accumulator:
        // Persist updated accumulator (residual) for the next frame’s Y accumulation
        sta     actor_dy_accum,x            // actor_dy_accum := residual

        // ------------------------------------------------------------
        // Snapshot and validate tentative Y using walkbox resolver on probe coordinates
        // ------------------------------------------------------------
        jsr     save_actor_path_state       // save pos/box/path flags and swap in probe coords
		
        jsr     resolve_actor_walkbox_and_behavior  // classify box for the tentative Y
        cmp     #BOX_RESULT_UNDETERMINED    // geometry invalid or aborted scan?
        bne     validate_y_depth            // resolved → continue with sentinel/waypoint checks
		
        jsr     restore_actor_path_state    // unresolved → rollback snapshot completely
        lda     #YAXIS_RESULT_WBOX_CHANGED  // signal that Y step caused box change
        rts                                 

        // ------------------------------------------------------------
        // Check |ΔY| sentinels before testing waypoint hit:
        //   |ΔY| == 0   → normal flow, no special backout
        //   |ΔY| == $FF → vertical-only backout (abort this Y step)
        // ------------------------------------------------------------
validate_y_depth:
        lda     actor_dy_abs,x              // A := |ΔY| for sentinel evaluation
        beq     check_waypoint_y_reached    // |ΔY| == 0 → no backout, go to waypoint test
		
        cmp     #$FF                        // test vertical-only backout sentinel
        bne     check_waypoint_y_reached    // |ΔY| != $FF → normal waypoint test
		
        jsr     restore_actor_path_state    // revert tentative move for vertical-only backout
        lda     #YAXIS_RESULT_WBOX_CHANGED  // report Y-axis box change
        rts                                 

        // ------------------------------------------------------------
        // Compare tentative probe Y against waypoint Y and choose result code
        // ------------------------------------------------------------
check_waypoint_y_reached:
        lda     actor_path_probe_y,x        // A := tentative probe Y after possible 1px step
        cmp     actor_cur_waypoint_y,x      // reached Y? 
        bne     continue_y_traversal        // if not → still approaching target

        lda     #YAXIS_RESULT_REACHED       // Y axis has reached waypoint coordinate
        rts                                 

continue_y_traversal:
        lda     #YAXIS_RESULT_CONTINUE      // Y axis still in transit toward waypoint
        rts                                 		
/*
================================================================================
  save_actor_path_state
================================================================================
Summary
    Create a reversible checkpoint of the actor’s current traversal state and
    atomically swap in the path-probe coordinates so speculative walkbox tests
    operate on probe positions instead of committed world coordinates.

Vars/State
    Uses the current actor index to read/write per-actor tables:
    saved_actor_pos_x/y            snapshot of committed position
    saved_actor_box_attr           snapshot of walkbox attribute tag
    saved_actor_box_cur            snapshot of walkbox index
    saved_actor_path_update_flag   snapshot of path-update flag

Global Inputs
    actor                         current actor index
    actor_pos_x/y[]				  committed world coordinates
    actor_path_probe_x/y[]        DDA/path probe
    actor_box_attr[]              current walkbox attribute
    actor_box_cur[]               current walkbox index
    actor_path_update_flag[]      path recompute flag

Global Outputs
    actor_pos_x/y[]  			  overwritten with probe coordinates
    saved_actor_pos_x/y           committed position snapshot
    saved_actor_box_attr          saved walkbox attribute tag
    saved_actor_box_cur           saved walkbox index
    saved_actor_path_update_flag  saved path-update flag

Description
    • Save committed world coordinates for full rollback.
    • Save walkbox attribute, walkbox index, and path-update flag.
    • Overwrite live actor_pos_x/y with the DDA/path probe coordinates so the
      walkbox resolver evaluates the speculative position.
    • Paired with restore_actor_path_state, which restores all state exactly.

Notes
    This routine brackets all speculative walkbox tests. All downstream
    traversal logic must call restore_actor_path_state when the probe is found
    invalid to preserve world-state consistency.
================================================================================
*/
* = $2F1B
save_actor_path_state:
        ldx     actor                        

        // Save committed world-space position so speculative probes can be rolled back
        lda     actor_pos_x,x                
        sta     saved_actor_pos_x            
        lda     actor_pos_y,x                
        sta     saved_actor_pos_y            

        // Save the actor’s current walkbox attribute (depth / behavior tag)
        lda     actor_box_attr,x             
        sta     saved_actor_box_attr         

        // Swap in the path probe coordinates as the live position for box resolution
        lda     actor_path_probe_x,x         
        sta     actor_pos_x,x                
        lda     actor_path_probe_y,x         
        sta     actor_pos_y,x                

        // Save walkbox index and path-update flag so both can be restored after a failed probe
        lda     actor_box_cur,x              
        sta     saved_actor_box_cur          
        lda     actor_path_update_flag,x     
        sta     saved_actor_path_update_flag 

        rts                                  
/*
================================================================================
  restore_actor_path_state
================================================================================
Summary
    Restore the actor’s previously snapshotted traversal state, resynchronizing
    committed world coordinates, path-probe coordinates, and walkbox metadata
    after a speculative move or box-resolution attempt.

Vars/State
    Uses the current actor index to read/write per-actor tables:
    saved_actor_pos_x/y            snapshot of committed position
    saved_actor_box_attr           snapshot of walkbox attribute tag
    saved_actor_box_cur            snapshot of walkbox index
    saved_actor_path_update_flag   snapshot of path-update flag

Global Inputs
    actor                         current actor index
    saved_actor_pos_x/y           saved coordinate from last checkpoint
    saved_actor_box_attr          saved walkbox attribute byte
    saved_actor_box_cur           saved walkbox index
    saved_actor_path_update_flag  saved path-update flag

Global Outputs
    actor_pos_x/y[]		          restored world coordinates
    actor_path_probe_x/y[]        probe coordinates resynced to world pos
    actor_box_attr[]              restored walkbox attribute
    actor_box_cur[]               restored walkbox index
    actor_path_update_flag[]      restored path-update flag

Description
    • Reload saved X/Y and write them back to the actor’s world-space position.
    • Mirror the same coordinates into the path-probe copies to keep DDA origin
      aligned with committed position.
    • Restore the saved walkbox attribute and current box index.
    • Restore the saved path-update flag so traversal logic sees the same state
      that existed at the time of the snapshot.

Notes
    This is the required counterpart to save_actor_path_state. All speculative
    walkbox tests must call this on failure, to guarantee that world position,
    probe position, and box state remain coherent.
================================================================================
*/
* = $2F48
restore_actor_path_state:
        ldx     actor                        

        // Restore committed X coordinate and resync the DDA/path probe copy
        lda     saved_actor_pos_x            
        sta     actor_pos_x,x                
        sta     actor_path_probe_x,x         

        // Restore committed Y coordinate and resync the DDA/path probe copy
        lda     saved_actor_pos_y            
        sta     actor_pos_y,x                
        sta     actor_path_probe_y,x         

        // Restore the actor’s walkbox attribute tag
        lda     saved_actor_box_attr         
        sta     actor_box_attr,x             

        // Restore the actor’s active walkbox index
        lda     saved_actor_box_cur          
        sta     actor_box_cur,x              

        // Restore the saved path-update flag
        lda     saved_actor_path_update_flag 
        sta     actor_path_update_flag,x     

        rts                                  
/*
================================================================================
  init_dda_for_path
================================================================================
Summary
    Compute the actor’s per-axis deltas, direction bits, dominant axis, dominant
    magnitude, and initial DDA accumulator seeds using the actor’s current
    path-probe coordinates and active waypoint. Returns “no move” when both
    deltas are zero.

Vars/State
    dir_x_bit, dir_y_bit                    temporary direction bits (0=left/up, 1=right/down)
    dda_dx_abs, dda_dy_abs                  absolute |ΔX| and |ΔY|
    dda_dominant_axis                       horizontal/vertical DDA dominance flag
    dda_dominant_abs                        dominant magnitude = max(|ΔX|,|ΔY|)
    dda_x/y_accum_seed      				initial DDA accumulator seeds

Global Inputs
    actor                                   current actor index
    actor_cur_waypoint_x/y[]                active waypoint coordinates
    actor_path_probe_x/y[]                  probe coordinates used as DDA origin

Global Outputs
    actor_dir_x/y_bit[]						per-actor direction bits
    actor_dx/dy_abs[]				        stored deltas |ΔX| and |ΔY|
    actor_dda_dom_abs[]                     stored dominant magnitude
    actor_dx/dy_accum[]					    initialized accumulators

Returns
    A		PATH_RET_NO_MOVE        		when both |ΔX| and |ΔY| are zero  
     		PATH_RET_MOVE_NEEDED    		otherwise

Description
    • Compare waypoint vs probe to derive |ΔX|/|ΔY| and direction bits using
      subtract-with-carry to encode sign into bit0 of each direction byte.
    • Early-out when both deltas reduce to zero (already at waypoint).
    • Compute dominant axis (horizontal vs vertical) and dominant magnitude,
      which sets the shared DDA scale for stepping.
    • Seed X/Y accumulators with the absolute deltas, guaranteeing that the
      first emitted step has correct fractional proportion.
    • Publish all computed DDA parameters into per-actor tables, enabling the
      X/Y stepping routines to progress one pixel at a time with sub-pixel
      accumulation and walkbox validation.

Notes
    Uses the probe copy of the position (actor_path_probe_*) rather than the
    committed world position to ensure DDA setup reflects the speculative path
    coordinate origin used by all subsequent walkbox tests.
================================================================================
*/
* = $2F6F
init_dda_for_path:
        // Seed both direction bits to “left/up” and assume a horizontal dominant axis
        ldy     #$00                
        sty     dir_x_bit                  		// default X direction: left  (bit0 = 0)
        sty     dir_y_bit                  		// default Y direction: up    (bit0 = 0)
        sty     dda_dominant_axis          		// default dominant axis: horizontal

        // ------------------------------------------------------------
        // Resolve horizontal delta: compare target X vs probe X to derive |ΔX| and dir_x_bit
		//
        // If target X < probe X → actor is to the right of the waypoint,
        // so we compute (cur − path) for a leftward delta; otherwise (path − cur) for rightward.
        // ------------------------------------------------------------
        ldx     actor
        lda     actor_cur_waypoint_x,x          // target X (waypoint)
        cmp     actor_path_probe_x,x            // compare to current probe X
        bcc     dx_from_pos_minus_path         	// path < pos → moving left, compute (pos − path)

        // ------------------------------------------------------------
        // path ≥ pos: compute (path − pos) and mark rightward motion via carry → dir_x_bit
        // ------------------------------------------------------------
        sbc     actor_path_probe_x,x            // A := (target X − probe X)
        rol     dir_x_bit                       // carry set → dir_x_bit.bit0 = 1 (right)
        jmp     commit_dx_abs

dx_from_pos_minus_path:
        // ------------------------------------------------------------
        // path < pos: compute (pos − path) and keep dir_x_bit at 0 for leftward motion
        // ------------------------------------------------------------
        lda     actor_path_probe_x,x            // load current probe X
        sec                                     
        sbc     actor_cur_waypoint_x,x          // A := (probe X − target X)

commit_dx_abs:
        // ------------------------------------------------------------
        // Commit |ΔX| (absolute horizontal distance) for later DDA scaling and dominance tests
        // ------------------------------------------------------------
        sta     dda_dx_abs

        // ------------------------------------------------------------
        // Resolve vertical delta: compare target Y vs probe Y to derive |ΔY| and dir_y_bit
        // ------------------------------------------------------------
        lda     actor_cur_waypoint_y,x          // target Y (waypoint)
        cmp     actor_path_probe_y,x            // compare to current probe Y
        bcc     dy_from_pos_minus_path          // path < pos → moving up, compute (pos − path)

        // ------------------------------------------------------------
        // path ≥ pos: compute (path − pos) and mark downward motion via carry → dir_y_bit
        // ------------------------------------------------------------
        sbc     actor_path_probe_y,x            // A := (target Y − probe Y)
        rol     dir_y_bit                       // carry set → dir_y_bit.bit0 = 1 (down)
        jmp     commit_dy_abs

dy_from_pos_minus_path:
        // ------------------------------------------------------------
        // path < pos: compute (pos − path) and keep dir_y_bit at 0 for upward motion
        // ------------------------------------------------------------
        lda     actor_path_probe_y,x            // load current probe Y
        sec                                     // enable borrow for subtract
        sbc     actor_cur_waypoint_y,x          // A := (probe Y − target Y)

commit_dy_abs:
        // ------------------------------------------------------------
        // Commit |ΔY| (absolute vertical distance) for DDA calculations
        // ------------------------------------------------------------
        sta     dda_dy_abs

        // ------------------------------------------------------------
        // Early-out if both axes have zero delta: already at waypoint, no DDA setup needed
        // ------------------------------------------------------------
        lda     dda_dx_abs                      // test |ΔX|
        bne     select_dominant_axis            // nonzero X → motion required
        ldy     dda_dy_abs                      // test |ΔY|
        bne     select_dominant_axis            // nonzero Y → motion required

        // Both |ΔX| and |ΔY| are zero → actor is exactly at waypoint
        lda     #PATH_RET_NO_MOVE                           // signal “no movement needed”
        rts

select_dominant_axis:
        // ------------------------------------------------------------
        // Choose dominant axis flag: prefer horizontal when |ΔX| >= |ΔY|, else mark vertical
        // ------------------------------------------------------------
        lda     dda_dx_abs                      // A := |ΔX|
        cmp     dda_dy_abs                      // compare against |ΔY|
        bcs     select_dominant_value           // |ΔX| >= |ΔY| → keep horizontal dominance
        lda     #DOM_AXIS_V                     // otherwise, vertical is dominant
        sta     dda_dominant_axis

select_dominant_value:
        // ------------------------------------------------------------
        // Select dominant magnitude: dom := max(|ΔX|, |ΔY|) for shared DDA scale
        // ------------------------------------------------------------
        lda     dda_dx_abs                      // A := |ΔX|
        cmp     dda_dy_abs                      // compare to |ΔY|
        bcs     store_dominant_value            // keep |ΔX| when it is >= |ΔY|
        lda     dda_dy_abs                      // else use |ΔY| as dominant

store_dominant_value:
        sta     dda_dominant_abs                // cache dominant magnitude for both axes

        // ------------------------------------------------------------
        // Seed local DDA accumulators with absolute deltas (initial thresholds per axis)
        // ------------------------------------------------------------
        lda     dda_dx_abs                      // X accumulator seed := |ΔX|
        sta     dda_x_accum_seed
        lda     dda_dy_abs                      // Y accumulator seed := |ΔY|
        sta     dda_y_accum_seed

        // ------------------------------------------------------------
        // Publish direction bits to per-actor state for later stepping and facing logic
        // ------------------------------------------------------------
        ldx     actor                           // X := actor index
        lda     dir_x_bit                       // bit0: 0=left, 1=right
        sta     actor_dir_x_bit,x
        lda     dir_y_bit                       // bit0: 0=up,   1=down
        sta     actor_dir_y_bit,x

        // ------------------------------------------------------------
        // Store dominant magnitude in per-actor state for X/Y DDA comparisons
        // ------------------------------------------------------------
        lda     dda_dominant_abs
        sta     actor_dda_dom_abs,x

        // ------------------------------------------------------------
        // Store absolute deltas in per-actor state for each axis
        // ------------------------------------------------------------
        lda     dda_dy_abs                      // |ΔY| → actor’s vertical delta
        sta     actor_dy_abs,x
        lda     dda_dx_abs                      // |ΔX| → actor’s horizontal delta
        sta     actor_dx_abs,x

        // ------------------------------------------------------------
        // Initialize per-actor DDA accumulators with the seeded thresholds
        // ------------------------------------------------------------
        lda     dda_x_accum_seed                // initial X accumulator := |ΔX|
        sta     actor_dx_accum,x
        lda     dda_y_accum_seed                // initial Y accumulator := |ΔY|
        sta     actor_dy_accum,x

        // At least one axis has nonzero delta → report “movement needed”
        lda     #PATH_RET_MOVE_NEEDED
        rts

/*
procedure save_actor_path_state():
    idx = actor  // current actor index

    // Save committed world position
    saved_actor_pos_x = actor_pos_x[idx]
    saved_actor_pos_y = actor_pos_y[idx]

    // Save walkbox attribute tag
    saved_actor_box_attr = actor_box_attr[idx]

    // Swap in probe coordinates as the “live” position for subsequent tests
    actor_pos_x[idx] = actor_path_probe_x[idx]
    actor_pos_y[idx] = actor_path_probe_y[idx]

    // Save active walkbox index and path-update flag
    saved_actor_box_cur = actor_box_cur[idx]
    saved_actor_path_update_flag = actor_path_update_flag[idx]

    // No explicit return value
end procedure

procedure restore_actor_path_state():
    idx = actor  // current actor index

    // Restore X coord and reflect into probe copy
    actor_pos_x[idx]        = saved_actor_pos_x
    actor_path_probe_x[idx] = saved_actor_pos_x

    // Restore Y coord and reflect into probe copy
    actor_pos_y[idx]        = saved_actor_pos_y
    actor_path_probe_y[idx] = saved_actor_pos_y

    // Restore walkbox attribute and index
    actor_box_attr[idx] = saved_actor_box_attr
    actor_box_cur[idx]  = saved_actor_box_cur

    // Restore path-update flag
    actor_path_update_flag[idx] = saved_actor_path_update_flag

    // No explicit return value
end procedure

function init_dda_for_path() -> PATH_RET_*:
    // Defaults: dir_x = left, dir_y = up, dominant axis = horizontal
    dir_x_bit        = 0   // 0 = left,  1 = right
    dir_y_bit        = 0   // 0 = up,    1 = down
    dda_dominant_axis = HORIZONTAL

    idx = actor

    // ----- Horizontal delta -----
    target_x = actor_cur_waypoint_x[idx]
    pos_x    = actor_path_probe_x[idx]

    if target_x >= pos_x then
        // moving right: |ΔX| = target_x - pos_x, dir_x_bit = 1
        dda_dx_abs = target_x - pos_x
        dir_x_bit  = 1
    else
        // moving left: |ΔX| = pos_x - target_x, dir_x_bit remains 0
        dda_dx_abs = pos_x - target_x
    end if

    // ----- Vertical delta -----
    target_y = actor_cur_waypoint_y[idx]
    pos_y    = actor_path_probe_y[idx]

    if target_y >= pos_y then
        // moving down: |ΔY| = target_y - pos_y, dir_y_bit = 1
        dda_dy_abs = target_y - pos_y
        dir_y_bit  = 1
    else
        // moving up: |ΔY| = pos_y - target_y, dir_y_bit remains 0
        dda_dy_abs = pos_y - target_y
    end if

    // ----- Early-out if no movement needed -----
    if dda_dx_abs == 0 and dda_dy_abs == 0 then
        return PATH_RET_NO_MOVE
    end if

    // ----- Dominant axis / magnitude -----
    if dda_dx_abs >= dda_dy_abs then
        dda_dominant_axis = HORIZONTAL
        dda_dominant_abs  = dda_dx_abs
    else
        dda_dominant_axis = VERTICAL
        dda_dominant_abs  = dda_dy_abs
    end if

    // Seed accumulators with absolute deltas
    dda_x_accum_seed = dda_dx_abs
    dda_y_accum_seed = dda_dy_abs

    // ----- Publish to per-actor state -----
    actor_dir_x_bit[idx]   = dir_x_bit        // 0=left, 1=right
    actor_dir_y_bit[idx]   = dir_y_bit        // 0=up,   1=down
    actor_dda_dom_abs[idx] = dda_dominant_abs

    actor_dx_abs[idx] = dda_dx_abs
    actor_dy_abs[idx] = dda_dy_abs

    actor_dx_accum[idx] = dda_x_accum_seed
    actor_dy_accum[idx] = dda_y_accum_seed

    return PATH_RET_MOVE_NEEDED
end function

function step_actor_x_dda() -> XAXIS_RESULT_*:
    idx = actor

    // Accumulate |ΔX| into fixed-point accumulator
    accumX = actor_dx_accum[idx] + actor_dx_abs[idx]

    if accumX < actor_dda_dom_abs[idx] then
        // Not enough accumulated to emit a full pixel step
        actor_dx_accum[idx] = accumX
        // No walkbox test; no position change; still moving on X in general
        return XAXIS_RESULT_CONTINUE
    end if

    // accumX >= dominant → emit exactly one ±1 pixel X step on the probe
    if actor_dir_x_bit[idx] == 0 then
        // move left
        actor_path_probe_x[idx] -= 1
    else
        // move right
        actor_path_probe_x[idx] += 1
    end if

    // Update residual accumulator after 1px emission
    accumX = accumX - actor_dda_dom_abs[idx]
    actor_dx_accum[idx] = accumX

    // Validate tentative X via walkbox logic
    save_actor_path_state()
    result = resolve_actor_walkbox_and_behavior()

    if result == BOX_RESULT_UNDETERMINED then
        // Geometry could not be resolved → roll back entirely
        restore_actor_path_state()
        return XAXIS_RESULT_WBOX_CHANGED
    end if

    // Walkbox resolved → compare probe X against waypoint X
    probeX  = actor_path_probe_x[idx]
    targetX = actor_cur_waypoint_x[idx]

    if probeX == targetX then
        return XAXIS_RESULT_REACHED
    else
        return XAXIS_RESULT_CONTINUE
    end if
end function

function step_actor_y_dda() -> YAXIS_RESULT_*:
    idx = actor

    // Accumulate |ΔY| into fixed-point accumulator
    accumY = actor_dy_accum[idx] + actor_dy_abs[idx]

    if accumY < actor_dda_dom_abs[idx] then
        // Not enough accumulated to emit a full pixel step
        actor_dy_accum[idx] = accumY
        return YAXIS_RESULT_CONTINUE
    end if

    // accumY >= dominant → emit exactly one ±1 pixel Y step on the probe
    if actor_dir_y_bit[idx] == 0 then
        // move up (screen Y decreases)
        actor_path_probe_y[idx] -= 1
    else
        // move down (screen Y increases)
        actor_path_probe_y[idx] += 1
    end if

    // Update residual accumulator after 1px emission
    accumY = accumY - actor_dda_dom_abs[idx]
    actor_dy_accum[idx] = accumY

    // Validate tentative Y via walkbox logic
    save_actor_path_state()
    result = resolve_actor_walkbox_and_behavior()

    if result == BOX_RESULT_UNDETERMINED then
        // Unresolvable geometry → roll back and signal box change/failure
        restore_actor_path_state()
        return YAXIS_RESULT_WBOX_CHANGED
    end if

    // At this point, geometry is resolved; handle special |ΔY| sentinels
    deltaY_abs = actor_dy_abs[idx]

    if deltaY_abs == 0 then
        // No special vertical backout semantics → normal waypoint compare
        // (zero here just means no vertical distance requested)
        // fall through to compare with waypoint
        pass
    else if deltaY_abs == $FF then
        // Special sentinel: vertical-only backout
        restore_actor_path_state()
        return YAXIS_RESULT_WBOX_CHANGED
    end if

    // Compare probe Y against waypoint Y
    probeY  = actor_path_probe_y[idx]
    targetY = actor_cur_waypoint_y[idx]

    if probeY == targetY then
        return YAXIS_RESULT_REACHED
    else
        return YAXIS_RESULT_CONTINUE
    end if
end function
*/