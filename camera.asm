/*
================================================================================
Camera horizontal control system
================================================================================
Summary
	This module controls the horizontal camera position over a side-scrolling
	room. It maintains a camera center column, clamps it to the room bounds,
	and derives the visible 40-column viewport span. The camera can either pan
	smoothly toward a target position or follow a specific actor within a
	configurable “follow window” inside the view.

Description
	- Core state:
	    • cam_target_pos holds the logical center of the viewport in room
	      columns; viewport_left_col / viewport_right_col are derived from it.
	    • cam_mode selects behavior: free pan vs actor-follow.
	    • cam_pan_goal is the destination center used by the pan stepper.
	    • cam_follow_costume_id picks which costume’s actor to track when in
	      FOLLOW_ACTOR mode.

	- Per-frame update (cam_upd_target):
	    • In pan mode:
	        - Compares cam_target_pos to cam_pan_goal.
	        - Steps the camera one column left or right per tick until the goal
	          is reached.
	    • In FOLLOW_ACTOR mode:
	        - Resolves the actor for cam_follow_costume_id and reads its X
	          position in room coordinates.
	        - Converts that X into viewport space by subtracting
	          viewport_left_col.
	        - If the actor is left of left_follow_threshold or at/beyond
	          right_follow_threshold, sets a flag to pan.
	        - When panning, moves cam_target_pos one column per tick toward the
	          actor’s X until they align.
	    • In any other mode, leaves cam_target_pos unchanged and only refreshes
	      the viewport span.
	    • After mode-specific logic, clamps cam_target_pos so the 40-column
	      viewport stays inside [0 .. room_width - 1], then recomputes
	      viewport_left_col and viewport_right_col from the clamped center.

	- Follow entry point (cam_follow_costume):
	    • Latches the requested costume id into cam_follow_costume_id and sets
	      cam_mode to FOLLOW_ACTOR.
	    • If the costume’s actor lives in a different room, calls
	      prepare_video_for_new_room and switch_to_room to load that room.
	    • Reads the actor’s X position and passes it to cam_seek_to so the
	      camera either snaps or begins a smooth pan toward the actor.
	    • Seeds cam_current_pos to a default value and marks all active actors
	      with a “refresh” flag so their sprites/tiles are redrawn under the
	      new camera alignment.

	- Seek helper (cam_seek_to):
	    • Accepts a requested camera center column in room coordinates.
	    • Computes the wrapped 8-bit distance between the request and the
	      current cam_target_pos, converting it to an absolute magnitude.
	    • If the distance is within CAM_PAN_THRESHOLD:
	        - Leaves cam_target_pos as-is and sets cam_pan_goal to the request,
	          so subsequent pan updates move the camera smoothly.
	      Otherwise:
	        - Snaps cam_target_pos immediately to the requested column and
	          also sets cam_pan_goal to that same value so there is no residual
	          panning.
================================================================================
*/
#importonce
#import "globals.inc"
#import "constants.inc"
#import "room_loader.asm"

.label cam_request_pos = $d0

* = $0668
left_follow_threshold: .byte $0A
* = $0669
right_follow_threshold: .byte $1E

/*
================================================================================
  cam_upd_target
================================================================================
Summary
	Per-frame camera update driver. Applies the current camera mode (pan or
	actor-follow) to adjust the camera center column, clamps it to the room
	width, and refreshes the visible viewport span.

Global Inputs
	cam_mode                   Current camera mode (PAN vs FOLLOW_ACTOR)
	cam_target_pos             Current camera center column in room coordinates
	cam_pan_goal               Goal camera center column for pan mode
	cam_follow_costume_id      Costume id of actor to follow in FOLLOW mode
	actor_for_costume          Map: costume id → actor slot (bit7=1 if none)
	actor_pos_x                Actor X positions in room columns
	left_follow_threshold      Left edge of horizontal follow window (view coords)
	right_follow_threshold     Right edge of horizontal follow window (view coords)
	viewport_left_col          Current leftmost visible column of the viewport
	room_width                 Total room width in columns

Global Outputs
	cam_target_pos             Stepped toward goal or actor as needed
	actor_follow_needs_pan     Flag set/cleared depending on follow logic
	viewport_left_col          Recomputed from clamped camera center
	viewport_right_col         Recomputed from clamped camera center

Description
	- Reads cam_mode to select behavior:
		• In PAN mode:
		    - Compares cam_target_pos to cam_pan_goal.
		    - Steps the camera center one column left or right toward the goal
		      until equality, then stops.
		• In FOLLOW_ACTOR mode:
		    - Resolves the followed actor via cam_follow_costume_id and
		      actor_for_costume.
		    - Computes the actor’s horizontal position relative to
		      viewport_left_col and compares it against left/right follow
		      thresholds.
		    - If the actor is left of the follow window, sets
		      actor_follow_needs_pan and steps the camera left.
		    - If the actor is right of the follow window, sets
		      actor_follow_needs_pan and steps the camera right.
		    - If the actor lies within the window, leaves the camera center
		      unchanged.
		• In any other mode:
		    - Skips stepping and just recomputes the visible span.
	- Uses clamp_camera_to_room to enforce that the 40-column viewport stays
	  fully inside the room bounds.
	- Uses update_visible_span so viewport_left_col / viewport_right_col track
	  the updated camera center.
================================================================================
*/
* = $0671
cam_upd_target:
		// Camera in pan mode?
		lda     cam_mode
		cmp     #CAM_MODE_PAN
		bne     check_mode_follow_actor

		// Pan mode (step 1 px toward goal each tick)
		// Compare target position with pan goal position
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

check_mode_follow_actor:
		// Camera following actor?
		cmp     #CAM_MODE_FOLLOW_ACTOR
		beq     mode_follow_actor        
		jmp     update_visible_span      

mode_follow_actor:
		// Camera mode: FOLLOW COSTUME
		//  - Keep actor within a horizontal “follow window”
		//  - If actor crosses left/right threshold, pan one step toward them
		// Resolve followed actor’s current X
		ldx     cam_follow_costume_id         // costume id → actor slot map
		lda     actor_for_costume,x           // A := actor slot for this costume
		tax
		lda     actor_pos_x,x        		  // A := actor_x (world/room coordinates)

		// Left-side test:
		//   delta_left := actor_x - viewport_left_col
		//   if delta_left < left_follow_threshold → need to pan (actor too far left)
		sec
		sbc     viewport_left_col          	  // A := actor_x - left_edge
		cmp     left_follow_threshold
		bcs     check_follow_right_threshold  // A ≥ threshold → left OK; test right next

		// Actor is left of the follow window → flag a pan
		lda     #TRUE
		sta     actor_follow_needs_pan
		jmp     handle_follow_pan

check_follow_right_threshold:
		// Right-side test:
		//   if (actor_x - left_edge) ≥ right_follow_threshold → flag a pan
		cmp     right_follow_threshold
		bcc     handle_follow_pan          // inside window on the right → no new flag
		beq     handle_follow_pan          // exactly at threshold → treat as “needs pan”

		// Actor is right of the follow window → flag a pan
		lda     #TRUE
		sta     actor_follow_needs_pan

handle_follow_pan:
		// Pan only if the follow flag is set; otherwise go clamp edges
		lda     actor_follow_needs_pan
		beq     clamp_camera_to_room

		// Decide pan direction from actor vs camera target
		lda     actor_pos_x,x
		cmp     cam_target_pos
		bne     pan_toward_actor            // not equal → take one step toward actor

		// Equal positions → clear follow flag and continue (no step needed)
		lda     #FALSE
		sta     actor_follow_needs_pan
		jmp     clamp_camera_to_room

pan_toward_actor:
		// Take a single-step toward the actor:
		bcc     pan_left_toward_actor      // actor_x < camera → step left
		beq     pan_left_toward_actor      // (defensive) treat equality as left

		// actor_x > camera → step right
		inc     cam_target_pos
		jmp     clamp_camera_to_room

pan_left_toward_actor:
		// Step left (actor_x ≤ camera)
		dec     cam_target_pos

clamp_camera_to_room:
		// Clamp camera inside room bounds
		//  - Visible view is 40 cols wide; camera targets the center.
		//  - VIEW_HALF_SPAN (0x14) is the center-to-edge offset.
		//  - Keep cam_target_pos such that the view never passes room edges.
		// Left clamp: if (cam_target_pos - VIEW_HALF_SPAN) underflows, pin to VIEW_HALF_SPAN.
		lda     cam_target_pos
		sec                                 // prepare exact subtraction (no initial borrow)
		sbc     #VIEW_HALF_SPAN             // A = cam_target_pos - VIEW_HALF_SPAN
		bcs     check_right_clamp           

		// Underflow → center at leftmost legal position.
		lda     #VIEW_HALF_SPAN
		sta     cam_target_pos
		jmp     update_visible_span

check_right_clamp:
		// Right clamp
		// if (cam_target_pos + VIEW_HALF_SPAN) ≥ room_width, pin to (room_width - VIEW_HALF_SPAN).
		lda     cam_target_pos
		clc                                 
		adc     #VIEW_HALF_SPAN             
		cmp     room_width
		bcc     update_visible_span         // A < room_width → right edge OK

		// Exceeded right edge → center at rightmost legal position.
		lda     room_width
		sec
		sbc     #VIEW_HALF_SPAN
		sta     cam_target_pos

update_visible_span:
		// Compute visible room span from camera center.
		// left  := cam_target_pos - VIEW_HALF_SPAN (0x14)
		lda     cam_target_pos
		sec                                 
		sbc     #VIEW_HALF_SPAN             
		sta     viewport_left_col
		
		// right := left + VIEW_COL_MAX_IDX (0x27)  // inclusive 40-col window
		clc                                 
		adc     #VIEW_COL_MAX_IDX      
		sta     viewport_right_col
		rts

/*
================================================================================
  cam_follow_costume
================================================================================
Summary
	Switch camera control into actor-follow mode for a given costume, ensure the
	corresponding room is loaded, seek or pan the camera toward the actor’s
	position, and mark all active actors for redraw.

Arguments
	.A  Costume id to follow with the camera

Global Outputs
	cam_follow_costume_id      Updated to the new costume id
	cam_mode                   Set to FOLLOW_ACTOR mode
	current_room               Updated if room switch occurs
	cam_target_pos             Adjusted or snapped via cam_seek_to
	cam_pan_goal               Adjusted or snapped via cam_seek_to
	cam_current_pos            Seeded with a default camera position
	actor_render_flags         All assigned actors tagged with “refresh needed”

Description
	- Latches the requested costume id into cam_follow_costume_id and sets
	  cam_mode to FOLLOW_ACTOR so subsequent cam_upd_target calls use follow
	  logic.
	- Looks up the room containing the costume via costume_room_idx; if it does
	  not match current_room:
	    • Calls prepare_video_for_new_room to reconfigure video/memory for the
	      target room.
	    • Calls switch_to_room to actually change rooms.
	- Resolves the actor slot for the costume via actor_for_costume and reads
	  that actor’s X position in room coordinates.
	- Passes the actor’s X position to cam_seek_to, which decides whether to
	  snap or pan the camera toward that position and updates cam_target_pos and
	  cam_pan_goal accordingly.
	- Seeds cam_current_pos with a default value so subsystems that read it
	  start from a known baseline.
	- Iterates over all actor slots; for each assigned actor, sets its
	  actor_render_flags “refresh” bit so the next render pass redraws everyone
	  under the new camera alignment.
================================================================================
*/
* = $6823
cam_follow_costume:
        // Publish target costume
        sta     cam_follow_costume_id

        // Switch camera into FOLLOW mode
        lda     #CAM_MODE_FOLLOW_ACTOR
        sta     cam_mode

		// Resolve followed costume
        ldx     cam_follow_costume_id
		
		// Resolve costume's room
        lda     costume_room_idx,x
		
		// Costume in current room?
        cmp     current_room
        bne     switch_to_costume_room
        jmp     room_is_current

switch_to_costume_room:
		// Costume not in current room
        // Transition to the room where the followed costume currently is.
        pha                                     // keep room index across prep call
        jsr     prepare_video_for_new_room      
        pla                                     // restore target room index
		
		// Switch to the target room
        tax                                     
        jsr     switch_to_room                  

room_is_current:
		// Resolve costume
        ldx     cam_follow_costume_id
		
		// Resolve actor		
        ldy     actor_for_costume,x
		
		// Resolve actor's X position
        lda     actor_pos_x,y
		
		// Center camera at that position
        jsr     cam_seek_to

		// Seed default camera current position
        lda     #CAM_DEFAULT_POS
        sta     cam_current_pos

        // Nudge animation state for onscreen actors
        ldx     #ACTOR_MAX_INDEX
set_actor_render_flags:
		// Actor is bound to a costume? If not, skip
        lda     costume_for_actor,x
        bmi     next_actor_slot          

		// Actor is bound, force a render refresh
        lda     actor_render_flags,x     
        ora     #ACTOR_RENDER_REFRESH
        sta     actor_render_flags,x

next_actor_slot:
        dex
        bpl     set_actor_render_flags

        rts
/*
================================================================================
  cam_seek_to
================================================================================
Summary
	Request a new camera center column and decide whether to snap immediately or
	begin a smooth pan toward that position, based on a configurable distance
	threshold.

Arguments
	.A  Requested camera center column in room coordinates

Global Inputs
	cam_target_pos             Current camera center column

Global Outputs
	cam_target_pos             Updated immediately when snapping
	cam_pan_goal               Goal camera center column for pan mode
	cam_request_pos            Latched requested camera center column

Description
	- Stores the requested column from A into cam_request_pos.
	- Computes an unsigned distance between cam_request_pos and cam_target_pos,
	  treating the 8-bit difference as a wrapped quantity and converting
	  negative differences to their absolute magnitude.
	- Compares |cam_request_pos - cam_target_pos| against CAM_PAN_THRESHOLD:
	    • If the distance is less than or equal to the threshold:
	        - Leaves cam_target_pos unchanged.
	        - Sets cam_pan_goal to cam_request_pos so subsequent PAN updates
	          gradually step the camera center toward the goal.
	    • If the distance is greater than the threshold:
	        - Immediately snaps cam_target_pos to cam_request_pos.
	        - Sets cam_pan_goal to cam_request_pos so no further pan movement
	          occurs after the snap.
	- Always leaves cam_pan_goal equal to cam_request_pos, ensuring that when
	  the camera arrives at the goal, pan logic naturally comes to rest.
================================================================================
*/
* = $6866
cam_seek_to:
        // Save requested camera position
        sta     cam_request_pos

        // Measure distance to current target as an unsigned 8-bit delta:
        //   A := cam_request_pos - cam_target_pos (mod 256).
        lda     cam_request_pos
        sec                                 
        sbc     cam_target_pos              

		// Result negative?
        bcs     delta_within_threshold      
		
		// Compute two's complement to take absolute value
        eor     #$ff                        
        clc
        adc     #$01                        

delta_within_threshold:
        // Threshold decision (inclusive):
        //   if |delta| ≤ CAM_PAN_THRESHOLD → begin smooth pan
        //   else                           → snap immediately
        cmp     #CAM_PAN_THRESHOLD
        bcc     begin_pan                  
        beq     begin_pan                  

		// delta > CAM_PAN_THRESHOLD
        // Snap: jump-cut camera to the requested position, and
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

/*
function cam_upd_target():
    # Step camera according to current mode
    if cam_mode == CAM_MODE_PAN:
        # Move one column toward pan goal
        if cam_target_pos < cam_pan_goal:
            cam_target_pos += 1
        elif cam_target_pos > cam_pan_goal:
            cam_target_pos -= 1
        else:
            # already at goal: no movement
            pass

    elif cam_mode == CAM_MODE_FOLLOW_ACTOR:
        # FOLLOW mode: try to keep followed actor inside a horizontal window
        costume_id = cam_follow_costume_id
        actor_index = actor_for_costume[costume_id]
        actor_x = actor_pos_x[actor_index]

        # Measure actor position relative to current left edge of view
        delta_from_left = actor_x - viewport_left_col

        # Decide whether actor is outside the follow window
        if delta_from_left < left_follow_threshold:
            # actor too far left
            actor_follow_needs_pan = TRUE
        elif delta_from_left >= right_follow_threshold:
            # actor too far right (inclusive)
            actor_follow_needs_pan = TRUE
        # otherwise leave actor_follow_needs_pan as-is

        if actor_follow_needs_pan:
            # Decide pan direction based on actor vs camera center
            if actor_x == cam_target_pos:
                # Actor already horizontally aligned with camera center
                actor_follow_needs_pan = FALSE
            else:
                # Step camera one column toward the actor
                if actor_x > cam_target_pos:
                    cam_target_pos += 1
                else:
                    cam_target_pos -= 1

    else:
        # Any other mode: do not change cam_target_pos; just recompute edges
        pass

    # Clamp camera center so 40-column view stays inside room bounds
    # center ∈ [VIEW_HALF_SPAN, room_width - VIEW_HALF_SPAN]
    if cam_target_pos - VIEW_HALF_SPAN < 0:
        cam_target_pos = VIEW_HALF_SPAN
    elif cam_target_pos + VIEW_HALF_SPAN >= room_width:
        cam_target_pos = room_width - VIEW_HALF_SPAN

    # Derive visible span from camera center
    viewport_left_col  = cam_target_pos - VIEW_HALF_SPAN
    viewport_right_col = viewport_left_col + VIEW_COL_MAX_IDX


function cam_follow_costume(costume_id):
    # Remember which costume we are following
    cam_follow_costume_id = costume_id

    # Put camera into FOLLOW-ACTOR mode
    cam_mode = CAM_MODE_FOLLOW_ACTOR

    # If the costume's actor is in another room, switch rooms
    target_room = costume_room_idx[costume_id]
    if target_room != current_room:
        # Prepare video / banking, then load/switch room
        prepare_video_for_new_room()
        switch_to_room(target_room)

    # Now we are in the correct room.
    # Center/pan camera toward the followed actor's X coordinate.
    actor_index = actor_for_costume[costume_id]
    actor_x = actor_pos_x[actor_index]

    # Let cam_seek_to decide snap vs smooth pan
    cam_seek_to(actor_x)

    # Seed camera's "current position" baseline (used elsewhere)
    cam_current_pos = CAM_DEFAULT_POS

    # Mark visible actors so their animation/render state refreshes on next tick
    for actor_slot in [3, 2, 1, 0]:
        costume_for_slot = costume_for_actor[actor_slot]
        if costume_for_slot has bit7 set:
            # slot unassigned → skip
            continue

        # Set "refresh" flag for this actor
        actor_render_flags[actor_slot] |= ACTOR_RENDER_REFRESH


function cam_seek_to(requested_pos):
    # Save requested position (used multiple times below)
    cam_request_pos = requested_pos

    # Compute distance to current target on a circular 0..255 ring
    # raw_delta = requested - current (mod 256)
    raw_delta = (cam_request_pos - cam_target_pos) mod 256

    # Convert to absolute distance on the ring:
    # If raw_delta represents a "negative" direction (i.e., > 127),
    # treat distance as 256 - raw_delta; otherwise use raw_delta directly.
    if raw_delta > 127:
        distance = 256 - raw_delta
    else:
        distance = raw_delta

    # Decide snap vs pan based on inclusive threshold
    if distance <= CAM_PAN_THRESHOLD:
        # Smooth path: queue a pan; camera stepper will move gradually
        cam_pan_goal = cam_request_pos
        # cam_target_pos is left as-is; pan logic will move it
    else:
        # Snap path: immediately move camera to requested position,
        # and also align pan goal so there is no residual panning after snap.
        cam_target_pos = cam_request_pos
        cam_pan_goal   = cam_request_pos

*/