/*========================================================
 * camera.asm — Camera control & view-window maintenance
 *
 * Summary:
 *   Core camera routines. Provides:
 *     • cam_upd_target  – per-tick camera stepper driven by cam_mode.
 *     • cam_follow_costume – switch to the room of a target costume (if needed)
 *       and begin following its actor.
 *     • cam_seek_to     – decide snap vs pan to a requested position.
 *
 *   - Modes:
 *       CAM_MODE_PAN            – step 1 column toward cam_pan_goal each tick.
 *       CAM_MODE_FOLLOW_ACTOR   – keep actor inside follow window and step toward it when exceeded.
 *
 * Coordinate model:
 *   - All positions are “columns” in room space.
 *   - The visible window is 40 columns wide (0x28), centered at cam_target_pos.
 *   - VIEW_HALF_SPAN (0x14) is the center-to-edge offset; right edge is
 *     left + VIEW_COL_MAX_IDX (0x27, inclusive).
 *
 * Behavior notes / edge cases:
 *   - cam_seek_to uses an absolute 8-bit distance (mod-256) against CAM_PAN_THRESHOLD;
 *     ≤ threshold → pan (set cam_pan_goal), > threshold → snap (set cam_target_pos & cam_pan_goal).
 *   - FOLLOW mode uses a left/right “follow window” measured from the edges of the view span.
 *     Crossing either threshold sets actor_follow_needs_pan and steps one column per tick.
 *   - Clamping guarantees the view never extends beyond room bounds:
 *     cam_target_pos ∈ [VIEW_HALF_SPAN, room_width − VIEW_HALF_SPAN].
 *
 * Conventions:
 *   - Unless stated, A/X/Y and NZCV flags are clobbered.
 *   - actor_for_costume / costume_for_actor use bit7=1 as “unassigned”.
 *
 *========================================================*/
#importonce
#import "globals.inc"
#import "constants.inc"
#import "room_loader.asm"

.label cam_request_pos = $d0

* = $0668
left_follow_threshold: .byte $0A
* = $0669
right_follow_threshold: .byte $1E


/*========================================================
 * cam_upd_target — advance camera one step based on mode, then update view edges
 *
 * Summary:
 *   Dispatches on cam_mode and adjusts cam_target_pos accordingly:
 *     - CAM_MODE_PAN: step 1 column toward cam_pan_goal (left/right).
 *     - CAM_MODE_FOLLOW_ACTOR: keep the followed actor within a horizontal
 *       window [left_follow_threshold .. right_follow_threshold] relative to
 *       viewport_left_col; when exceeded, step one column toward the actor.
 * 
 *   After movement (or if no movement), clamps cam_target_pos to room bounds
 *   and derives the visible span [viewport_left_col, viewport_right_col].
 *
 * Arguments (globals read):
 *   cam_mode                         Camera behavior selector.
 *   cam_target_pos                   Current camera center column.
 *   cam_pan_goal                     Target column for PAN mode.
 *   cam_follow_costume_id            Costume whose actor is followed (FOLLOW mode).
 *   actor_for_costume[ ]            Costume→actor slot map (FOLLOW mode).
 *   actor_pos_x[ ]         Actor X positions (FOLLOW mode).
 *   viewport_left_col             Current left edge (for FOLLOW window tests).
 * Vars:
 *   left_follow_threshold            Columns from left edge that define left window bound.
 *   right_follow_threshold           Columns from right edge that define right window bound.
 *   room_width                       Room width in columns.
 *
 * Returns / side effects (globals written):
 *   cam_target_pos                   Stepped toward goal/actor and clamped to bounds.
 *   actor_follow_needs_pan           Set/cleared when FOLLOW window is crossed/met.
 *   viewport_left_col             Updated from cam_target_pos (center - VIEW_HALF_SPAN).
 *   viewport_right_col            Updated from left_edge (+ VIEW_COL_MAX_IDX).
 *
 * Flags:
 *   Not preserved. Internal CMP/SBC/ADC modify NZCV.
 *
 * Description:
 *   1) If CAM_MODE_PAN: compare cam_target_pos vs cam_pan_goal and step ±1, then clamp.
 *   2) Else if CAM_MODE_FOLLOW_ACTOR:
 *        - Sample actor X for the followed costume.
 *        - Compute (actor_x - viewport_left_col) and compare against follow window.
 *        - If outside window, step cam_target_pos one px toward actor.
 *      Clamp afterward.
 *   3) Clamp ensures the 40-col view (centered at cam_target_pos) never exceeds room edges.
 *   4) Finally, compute [viewport_left_col, viewport_right_col] from cam_target_pos.
 *========================================================*/
* = $0671
cam_upd_target:
		// Choose behavior based on current camera mode.
		// Compare cam_mode against PAN mode; CMP sets Z=1 when equal.
		// Branch: BNE (Z=0) → not PAN → check next mode; fall-through → PAN handler.
		lda     cam_mode
		cmp     #CAM_MODE_PAN
		bne     check_mode_follow_actor

		/*---------------------------------------
		 * Camera mode: PAN TO POSITION (step 1 px toward goal each tick)
		 *  - CMP sets flags from (cam_target_pos - cam_pan_goal)
		 *  - BCS ⇒ C=1 ⇒ cam_target_pos ≥ cam_pan_goal
		 *--------------------------------------*/
		lda     cam_target_pos
		cmp     cam_pan_goal
		bcs     pan_when_ge_goal        // C=1 ⇒ A ≥ goal

		// A < goal → advance right by 1
		inc     cam_target_pos
		jmp     after_pan_step

pan_when_ge_goal:
		// Here A ≥ goal:
		bcc     after_pan_step          // unreachable (C=0 would mean A < goal); kept for symmetry
		beq     after_pan_step          // equal → no movement

		// A > goal → advance left by 1
		dec     cam_target_pos

after_pan_step:
		// Clamp camera to valid room range before computing edges
		jmp     clamp_camera_to_room

		/*---------------------------------------
		 * Next mode check: FOLLOW ACTOR
		 *  - CMP still holds cam_mode from above
		 *  - BEQ when cam_mode == CAM_MODE_FOLLOW_ACTOR
		 *--------------------------------------*/
check_mode_follow_actor:
		cmp     #CAM_MODE_FOLLOW_ACTOR
		beq     mode_follow_actor        // follow logic handles thresholding & panning
		jmp     update_visible_span      // otherwise: compute edges and finish

		/*---------------------------------------
		 * Camera mode: FOLLOW COSTUME
		 *  - Keep actor within a horizontal “follow window”
		 *  - If actor crosses left/right threshold, pan one step toward them
		 *--------------------------------------*/
mode_follow_actor:
		// Resolve followed actor’s current X
		ldx     cam_follow_costume_id         // costume id → actor slot map
		lda     actor_for_costume,x           // A := actor slot for this costume
		tax
		lda     actor_pos_x,x        // A := actor_x (world/room coordinates)

		// Left-side test:
		//   delta_left := actor_x - viewport_left_col
		//   if delta_left < left_follow_threshold → need to pan (actor too far left)
		sec
		sbc     viewport_left_col          // A := actor_x - left_edge
		cmp     left_follow_threshold
		bcs     check_follow_right_threshold   // A ≥ threshold → left OK; test right next

		// Actor is left of the follow window → flag a pan
		lda     #$01
		sta     actor_follow_needs_pan
		jmp     handle_follow_pan

check_follow_right_threshold:
		// Right-side test:
		//   if (actor_x - left_edge) ≥ right_follow_threshold → flag a pan
		// CMP sets C=1 when A ≥ right_threshold; BCC falls through when A < threshold.
		cmp     right_follow_threshold
		bcc     handle_follow_pan          // inside window on the right → no new flag
		beq     handle_follow_pan          // exactly at threshold → treat as “needs pan”

		// Actor is right of the follow window → flag a pan
		lda     #$01
		sta     actor_follow_needs_pan

handle_follow_pan:
		// Pan only if the follow flag is set; otherwise go clamp edges
		lda     actor_follow_needs_pan
		beq     clamp_camera_to_room

		// Decide pan direction from actor vs camera target:
		//   CMP sets Z=1 when equal, C=1 when actor_x ≥ cam_target_pos
		lda     actor_pos_x,x
		cmp     cam_target_pos
		bne     pan_toward_actor            // not equal → take one step toward actor

		// Equal positions → clear follow flag and continue (no step needed)
		lda     #$00
		sta     actor_follow_needs_pan
		jmp     clamp_camera_to_room

pan_toward_actor:
		// Take a single-step toward the actor:
		// After CMP: C=1 if actor_x ≥ cam_target_pos, Z=1 if equal.
		// We already handled Z=1 earlier; here Z=0.
		bcc     pan_left_toward_actor      // C=0 → actor_x < camera → step left
		beq     pan_left_toward_actor      // (defensive) treat equality as left

		// C=1 and Z=0 → actor_x > camera → step right
		inc     cam_target_pos
		jmp     clamp_camera_to_room

pan_left_toward_actor:
		// Step left (actor_x ≤ camera)
		dec     cam_target_pos

		/*---------------------------------------
		 * Clamp camera inside room bounds
		 *  - Visible view is 40 cols wide; camera targets the center.
		 *  - VIEW_HALF_SPAN (0x14) is the center-to-edge offset.
		 *  - Keep cam_target_pos such that the view never passes room edges.
		 *--------------------------------------*/
clamp_camera_to_room:
		// Left clamp: if (cam_target_pos - 0x14) underflows, pin to 0x14.
		lda     cam_target_pos
		sec                                 // prepare exact subtraction (no initial borrow)
		sbc     #VIEW_HALF_SPAN             // A = cam_target_pos - 0x14
		bcs     check_right_clamp           // C=1 → no underflow → left edge OK

		// Underflow → center at leftmost legal position.
		lda     #VIEW_HALF_SPAN
		sta     cam_target_pos
		jmp     update_visible_span

check_right_clamp:
		// Right clamp: if (cam_target_pos + 0x14) ≥ room_width, pin to (room_width - 0x14).
		lda     cam_target_pos
		clc                                 // addition may carry; that’s intended
		adc     #VIEW_HALF_SPAN             // A = cam_target_pos + 0x14
		cmp     room_width
		bcc     update_visible_span         // A < room_width → right edge OK

		// Exceeded right edge → center at rightmost legal position.
		lda     room_width
		sec
		sbc     #VIEW_HALF_SPAN
		sta     cam_target_pos

		/*---------------------------------------
		 * Compute visible room span from camera center.
		 *  left  := cam_target_pos - VIEW_HALF_SPAN (0x14)
		 *  right := left + VIEW_COL_MAX_IDX (0x27)  // inclusive 40-col window
		 *  Flags: SBC uses SEC (C=1) for exact subtract; no borrow ⇒ C=1.
		 *--------------------------------------*/
update_visible_span:
		lda     cam_target_pos
		sec                                 // exact subtract (treat as unsigned delta)
		sbc     #VIEW_HALF_SPAN             // A = center - 0x14 → left edge
		sta     viewport_left_col
		clc                                 // exact add
		adc     #VIEW_COL_MAX_IDX      // A = left + 0x27 → right edge (inclusive)
		sta     viewport_right_col
		rts

/*===========================================
 * cam_follow_costume : Follow a costume’s actor; switch rooms if needed.
 *
 * Summary:
 *   Puts the camera into “follow actor” mode for the costume ID provided by the caller.
 *   If that costume’s actor is not in the current room, transitions to the correct room,
 *   then seeks the camera toward the actor’s X position.
 *
 * Arguments:
 *   A (costume id)        Index of the costume/character to follow.
 *
 * Returns:
 *   A, X, Y               Clobbered.
 *   Flags                 Updated per last loads/stores/branches (no contract).
 *   Globals updated       cam_follow_costume_id, cam_mode, cam_current_pos;
 *                         cam_pan_goal (via cam_seek_to); may change current_room (via switch_to_room);
 *                         actor_render_flags[0..3] bit0 set for assigned actors.
 *
 * Description:
 *   1) Save the costume id and set camera mode to FOLLOW.
 *   2) Look up costume_room_idx[costume]; if it differs from current_room, prepare video
 *      state and load the target room. (Loader ABI expects room index in X.)
 *   3) Resolve the actor slot bound to that costume (actor_for_costume[costume]) and
 *      feed its X coordinate (actor_pos_x[y]) into cam_seek_to to pan/snap.
 *   4) Initialize cam_current_pos to a default baseline (prevents initial jitter).
 *   5) For actor slots 3..0: if assigned (costume_for_actor[x] bit7=0), set
 *      actor_render_flags[x].bit0 = 1 
 *   Notes:
 *     - actor_for_costume/costume_for_actor use bit7=1 as “unassigned” sentinel.
 *     - cam_seek_to performs the thresholded pan/snap decision (≤ CAM_PAN_THRESHOLD pans).
 *===========================================*/
* = $6823
cam_follow_costume:
        // Record target costume/character for follow logic (caller passed ID in A)
        sta     cam_follow_costume_id

        // Switch camera into FOLLOW mode so updates track that costume
        lda     #CAM_MODE_FOLLOW_ACTOR
        sta     cam_mode

        /*---------------------------------------
         * If the followed costume isn’t in the active room, switch rooms first
         *  - X := costume id (index into costume→room map)
         *  - A := costume_room_idx[x] (room where that costume currently lives)
         *  - If A ≠ current_room, transition to that room; else continue in-place
         *--------------------------------------*/
        ldx     cam_follow_costume_id
        lda     costume_room_idx,x
		
		// If not in current room → prepare video and load that room
        cmp     current_room
        bne     switch_to_costume_room
        jmp     room_is_current


switch_to_costume_room:
        // Transition to the room where the followed costume currently is.
        // On entry: A = target room index (from costume_room_idx[x]).
        pha                                     // keep room index across prep call (A is clobbered)
        jsr     prepare_video_for_new_room      // ready display/IO/memory banking for a room switch
        pla                                     // restore target room index
        tax                                     // loader ABI: room index must be in X
        jsr     switch_to_room                       // perform the actual room load/switch

room_is_current:
        /*---------------------------------------
         * Center/pan camera to the followed actor’s horizontal position
         *  - X := costume id to follow
         *  - Y := actor slot currently bound to that costume
         *  - A := actor’s X coordinate → feed to cam_seek_to (pan or snap logic inside)
         *--------------------------------------*/
		 
		// Fetch actor’s X position from costume→actor map
        ldx     cam_follow_costume_id
        ldy     actor_for_costume,x
        lda     actor_pos_x,y
		
		// Center camera at that position
        jsr     cam_seek_to

		// Seed default camera current position
        lda     #CAM_DEFAULT_POS
        sta     cam_current_pos


        /*---------------------------------------
         * Nudge animation state for visible actors (slots 3..0)
		 
         *  - Skip unassigned slots: costume_for_actor[x] < 0 (bit7=1) → no actor bound
         *  - Set animation_state bit0 = 1 to force an immediate update/refresh on next tick
         *    (bit0 is used as a “dirty/advance” flag by the animator)
         *--------------------------------------*/
        ldx     #$03
set_actor_render_flags:
        lda     costume_for_actor,x
        bmi     next_actor_slot          // unassigned slot → skip

        lda     actor_render_flags,x     // set actor render refresh
        ora     #ACTOR_RENDER_REFRESH
        sta     actor_render_flags,x

next_actor_slot:
        dex
        bpl     set_actor_render_flags

        rts
/*===========================================
  cam_seek_to: snap or pan camera to a target

  Summary:
    Decides between an instant jump (snap) and a smooth movement (pan)
    toward a requested camera position. Uses an absolute 8-bit distance
    and an inclusive threshold to choose the behavior.

  Arguments:
    A 		                    Requested camera destination (0..255).
  State:
    cam_target_pos           	Current/latched camera position/target.
    cam_pan_goal             	Pan system’s goal position.

  Returns:
    cam_target_pos              Updated only on snap (set to request).
    cam_pan_goal                Always set to request (snap or pan).
    Flags                       Unchanged at return.

  Description:
    Copies the requested position, computes |request − cam_target_pos|
    as an unsigned 8-bit difference (mod 256), and compares it against
    CAM_PAN_THRESHOLD. If the distance is ≤ threshold, it queues a pan by
    writing cam_pan_goal (leaving cam_target_pos for the pan system to
    advance). If the distance is > threshold, it snaps immediately by
    writing both cam_target_pos and cam_pan_goal to the requested value.
    Notes:
      - Distance is modulo-256; wraparound (e.g., 0↔255) is treated as
        a short 1-unit difference.
      - Threshold test is inclusive (≤).
      - Two’s-complement is used to absolute-value negative deltas.
===========================================*/
* = $6866
cam_seek_to:
        // Remember caller’s requested camera position (used multiple times below)
        sta     cam_request_pos

        // Measure distance to current target as an unsigned 8-bit delta:
        //   A := cam_request_pos - cam_target_pos (mod 256).
        // Carry usage: SEC sets C=1 meaning “no borrow yet”; after SBC:
        //   C=1 → no borrow → result ≥ 0 (already absolute),
        //   C=0 → borrow → result < 0  → take two’s complement to get |delta|.
        lda     cam_request_pos
        sec                                 // prime for subtraction: treat as exact difference
        sbc     cam_target_pos              // A ← desired - current (wraps modulo 256)

		// If negative, take absolute value (two’s complement)
        bcs     delta_within_threshold      // C=1 (no borrow) ⇒ non-negative; skip abs fixup
        eor     #$ff                        // two’s complement: invert…
        clc
        adc     #$01                        // …and add 1 → |desired - current|

delta_within_threshold:
        // Threshold decision (inclusive):
        //   if |delta| ≤ CAM_PAN_THRESHOLD → begin smooth pan
        //   else                           → snap immediately
        cmp     #CAM_PAN_THRESHOLD
        bcc     begin_pan                  // A <  threshold → pan
        beq     begin_pan                  // A == threshold → pan

        // Snap path: jump-cut camera to the requested position, and
        // also set the pan goal to the same value to prevent post-snap drift.
        lda     cam_request_pos
        sta     cam_target_pos
        sta     cam_pan_goal

begin_pan:
        // Smooth path: queue the requested position as the pan goal.
        // The panning subsystem will incrementally move cam_target_pos toward cam_pan_goal.
        lda     cam_request_pos
        sta     cam_pan_goal
        rts

