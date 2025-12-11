/*
================================================================================
 main.asm
================================================================================
Summary
    Per-frame main loop and walking-sound controller for the Maniac Mansion
    engine. This module ties together input, scripting, actor motion, camera
    control, room redraw, flashlight effects, and audio pacing under the
    raster-IRQ–driven timing environment.

Responsibilities
    • Frame orchestration
      - Run a single “game frame” worth of work, then wait for a fixed number
        of raster IRQ ticks before advancing to the next frame.
      - Sequence keyboard input, script execution, inventory cleanup, sentence
        processing, actor movement/animation, camera updates, and rendering
        in a deterministic order.
      - Enforce an “active room” gate so most work is skipped while no room is
        loaded or during early boot transitions.

    • Rendering and camera
      - Maintain separate camera target vs. current camera position, updating
        target from game logic and committing it at the end of each frame.
      - Redraw the room view when the camera target differs from the committed
        camera position, optionally running a foreground-actor refresh pass for
        actors flagged as behind front layers.
      - Advance actor visibility, sprite states, and frame buffers, then redraw
        only those actors whose render flags request an update.
      - Coordinate with the raster IRQ via a shared video_update_signal so that
        color/graphics updates (including flashlight effects) remain synchronized
        with video timing.

    • Input, sentences, and inventory
      - Poll and process exactly one keyboard event per main-loop iteration.
      - Drive the sentence/action system by:
            • Approaching and triggering entities when conditions are met.
            • Executing the current sentence.
            • Draining the sentence stack of queued sentences.
      - Clean up inventory entries by scanning for objects whose ownership has
        been changed to a “remove/limbo” sentinel and removing them from the
        inventory table.

    • Actor expression and special effects
      - Toggle mouth animation bits for speaking actors to create a simple 
		open/closed talking effect driven by their mouth state flags.
      - Drive a shutter effect when needed, then ensure the cursor is visible afterward.
      - Maintain a dynamic flashlight beam when the global lights mode is
        “flashlight only” by:
            • Tracking cursor-relative beam coordinates.
            • Clearing/redrawing the flashlight mask when the cursor or camera
              moves.
            • Requesting a color-only redraw mode so the IRQ path applies the
              proper visual update.

    • Audio pacing – walking sounds
      - Periodically scan actors and, in eligible rooms, play a single walking
        sound effect for the first moving actor whose costume declares a step
        SFX mapping.
      - Use a signed throttle counter to avoid evaluating walking SFX every
        frame, and reset this counter only on underflow.
      - Suppress walking sounds entirely in specific exterior rooms that rely
        on ambient environmental audio (e.g., crickets).
      - Decouple walking SFX from script ownership by clearing the current task
        index before dispatching a walking sound.

    • Timing and IRQ interaction
      - Reset and monitor irq_entry_count at the start and end of each main-loop
        pass, waiting until a fixed number of IRQ entries have occurred before
        starting the next frame; this ties game speed to raster timing.
      - Use busy-wait loops on both video_update_signal and irq_entry_count as
        simple synchronization primitives between the main thread of execution
        and the raster IRQ handler.
      - Maintain debug/monitor mirrors for key runtime variables (current kid,
        camera target, room, and music state) so external debugging tools or
        monitor code can inspect live engine state without altering behavior.

Key flows
    1) Each frame:
         • Reset IRQ pacing counter.
         • Handle one keyboard event.
         • Update debug mirrors and cursor coordinate transforms.
         • Execute active scripts and perform inventory cleanup.
         • If no room is active, skip to the end and wait for next frame.
         • Run sentence processing and mouth animation.
         • Step actor motion/animation and update camera target.
         • Redraw room if camera changed; optionally refresh FG actors.
         • Step actor visibility, sprites, and flip framebuffers.
         • Redraw flagged actors and rotate the sprite shapeset bank.
         • Run shutter effect and walking-sound logic.
         • If lights == flashlight-only, update flashlight mask and request a
           color-only redraw as needed.
         • Signal the IRQ to perform its video update and wait until it clears
           the signal.
         • Perform a full costume sweep to ensure all visible actors with
           assigned sprites are drawn.
         • Commit camera position and wait for the configured IRQ pacing count.

    2) Walking-sound evaluation:
         • On each call, immediately exit in “outdoor ambience” rooms.
         • Decrement a signed throttle counter; only when it underflows:
              – Reset the counter to a configured delay.
              – Scan actors from highest index downward:
                    · Skip unused actors and those in a stopped motion state.
                    · Skip costumes with no step SFX mapping.
                    · For the first qualifying actor, clear script ownership,
                      then invoke the walking-sound handler with the mapped
                      sound index and stop scanning.

Notes
    • The module assumes that irq_entry_count and video_update_signal are
      maintained by the IRQ handler; desynchronizing that interaction will
      distort pacing or stall the main loop.
    • All per-frame work is serialized in a fixed order to avoid possible race
      conditions between sentence processing, actor movement, and rendering.
    • Room 0 is treated as a “no active room” state: the main loop continues
      running but skips most world logic until a room is assigned.
================================================================================
*/
#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "ops_sound.asm"
#import "key_handler.asm"
#import "entry_point.asm"
#import "ops_object.asm"
#import "flashlight.asm"

.label costume_index = $15

.const IRQ_PACING_THRESHOLD       = $04    // Number of IRQ entries required before main loop runs again

* = $04FD
main_loop:
        // Debug hook – no-op call
        jsr     do_nothing_077B

        // Reset IRQ-driven pacing counter (main loop runs once per 4 IRQ entries)
        lda     #$00
        sta     irq_entry_count

        // --------------------------------------------------
        // Keyboard input processing
        // --------------------------------------------------
        jsr     keyboard_handle_one_key        // Handle one keypress event

        // --------------------------------------------------
        // Update debug monitor variables
        // --------------------------------------------------
        lda     current_kid_idx
        sta     current_kid_idx                // Redundant write

        lda     cam_target_pos
        sta     var_camera_position            // Mirror camera target for debugging

        lda     topbar_mode
        sta     var_msg_flag                   // Mirror UI mode

        lda     current_room
        sta     var_current_room               // Mirror current room

        lda     music_playback_in_progress
        sta     var_music_playback_in_progress // Mirror music state

        // --------------------------------------------------
        // Cursor coordinate conversions
        // --------------------------------------------------
        lda     cursor_x_pos_quarter_relative
        clc
        adc     viewport_left_col
        sta     cursor_x_pos_quarter_absolute  // Convert to room-absolute X

        lda     cursor_y_pos_half
        sec
        sbc     #CURSOR_Y_BIAS
        sta     cursor_y_pos_half_off_by_8     // Pre-offset Y

        // --------------------------------------------------
        // Script execution
        // --------------------------------------------------
        jsr     execute_running_tasks          // Execute active scripts

        // --------------------------------------------------
        // Inventory cleanup (remove limbo objects)
        // --------------------------------------------------
        lda     remove_obj_from_inv_flag
        beq     check_active_room                     // Skip if no removals requested

        lda     #FALSE
        sta     remove_obj_from_inv_flag       // Clear request flag

        ldy     #INVENTORY_SLOTS                           // Inventory index (descending)
scan_inventory_for_limbo_objects:
        ldx     inventory_objects,y            // X := object index
        lda     object_attributes,x
        and     #OWNER_NIBBLE_MASK
        cmp     #OWNER_REMOVE_SENTINEL
        bne     next_inventory_slot          // Not limbo → skip

        tya                                     
        pha                                     // Save inventory index

        lda     #$00
        jsr     op_remove_from_inventory        // Remove object from inventory

        pla                                     
        tay                                     // Restore index

next_inventory_slot:
        dey
        bne     scan_inventory_for_limbo_objects

        // --------------------------------------------------
        // Active room check
        // --------------------------------------------------
check_active_room:
        lda     current_room
        bne     process_active_room_logic                    // Continue only if room active
        jmp     main_loop                      // Otherwise loop immediately

        // --------------------------------------------------
        // Sentence processing
        // --------------------------------------------------
process_active_room_logic:
        jsr     handle_entity_approach_and_trigger
        jsr     process_sentence
        jsr     process_sentence_stack_entry

        // --------------------------------------------------
        // Mouth animation toggling for speaking actors
        // --------------------------------------------------
        ldx     #ACTOR_MAX_INDEX
flip_mouth_state:
        lda     actor_mouth_state,x
        and     #MOUTH_STATE_SPEAK_CLOSED
        beq     next_mouth_actor

        lda     actor_mouth_state,x
        eor     #MOUTH_TOGGLE_MASK                           // Toggle #01 <-> #FF
        sta     actor_mouth_state,x

next_mouth_actor:
        dex
        bpl     flip_mouth_state

        // --------------------------------------------------
        // Actor movement, camera, room redraw
        // --------------------------------------------------
        jsr     step_actor_motion_and_animation
        jsr     cam_upd_target

        lda     #CAM_DEFAULT_POS
        sta     screen_scroll_flag             // Default: no scroll

        ldx     cam_target_pos
        cpx     cam_current_pos
        beq     update_actors                  // Skip room redraw if at target

        jsr     render_room_view               // Redraw room scene

        lda     screen_scroll_flag
        cmp     #$01
        bne     advance_animation              // Only run FG-actor logic if flag==1

        // --------------------------------------------------
        // Refresh actors behind foreground layers
        // --------------------------------------------------
        ldx     #ACTOR_MAX_INDEX
scan_actors_for_fg_refresh:
        lda     costume_for_actor,x
        bmi     next_fg_actor                    // Skip unused actor

        lda     actor_box_attr,x
        beq     next_fg_actor                    // Skip if not behind FG

        lda     #ACTOR_RENDER_REFRESH
        ora     actor_render_flags,x           // Mark “needs redraw”
        sta     actor_render_flags,x

next_fg_actor:
        dex
        bpl     scan_actors_for_fg_refresh

        // --------------------------------------------------
advance_animation:
        jsr     step_actor_visibility_and_sprites
        jsr     alternate_frame_buffer

        // --------------------------------------------------
update_actors:
        jsr     redraw_flagged_actors          // Redraw flagged actors

        lda     sprite_bank_sel
        clc
        adc     #$01
        sta     target_sprite_shapeset         // Select next sprite shapeset

        // --------------------------------------------------
        // Shutter effect
        // --------------------------------------------------
        lda     open_shutter_flag
        beq     make_walk_sounds

        lda     global_lights_state
        cmp     #LIGHTS_ENVIRONMENT_ON
        bne     show_cursor_flag

        jsr     open_shutter                   // Perform shutter animation

show_cursor_flag:
        lda     #FALSE
        sta     open_shutter_flag              // Clear request
        sta     hide_cursor_flag               // Ensure cursor visible

        // --------------------------------------------------
        // Walking sound generation
        // --------------------------------------------------
make_walk_sounds:
        jsr     update_walking_sounds

        // --------------------------------------------------
        // Flashlight-beam logic (lights==1)
        // --------------------------------------------------
        lda     global_lights_state
        cmp     #LIGHTS_FLASHLIGHT_ONLY
        bne     synchronize_on_video_update

        lda     screen_scroll_flag
        cmp     #CAM_DEFAULT_POS
        bne     flashlight_coords_changed      // Scroll in progress forces redraw

        lda     cursor_x_pos_quarter_relative
        cmp     flashlight_beam_x
        bne     flashlight_coords_changed

        lda     cursor_y_pos_half
        cmp     flashlight_beam_y
        bne     flashlight_coords_changed

        jmp     synchronize_on_video_update    // No movement → skip redraw

flashlight_coords_changed:
        jsr     clear_flashlight_area

        lda     cursor_x_pos_quarter_relative
        sta     flashlight_beam_x

        lda     cursor_y_pos_half
        sta     flashlight_beam_y

        jsr     draw_flashlight_area

        lda     #VID_SETUP_COPY_COLORS
        sta     video_setup_mode               // Mode: copy color only

        // --------------------------------------------------
        // Video-update synchronization with IRQ
        // --------------------------------------------------
synchronize_on_video_update:
        lda     #SIGNAL_SET
        sta     video_update_signal            // Signal IRQ to update video

wait_for_signal_clear:
        lda     video_update_signal
        bne     wait_for_signal_clear          // Wait until IRQ clears it

        // --------------------------------------------------
        // Second actor draw sweep (full costume scan)
        // --------------------------------------------------
        lda     #$00
        sta     costume_index

scan_costumes_for_redraw:
        ldx     costume_index
        lda     actor_for_costume,x
        bmi     next_costume_entry                   // Skip if unassigned

        sta     actor
        stx     active_costume
        ldx     actor

        lda     actor_render_flags,x
        and     #ACTOR_RENDER_VISIBLE
        beq     next_costume_entry                   // Skip if not visible

        lda     actor_render_flags,x
        and     #ACTOR_RENDER_CLEAR_VISIBLE
        sta     actor_render_flags,x           		

        lda     actor_sprite_index,x
        cmp     #NO_SPRITE
        beq     next_costume_entry                   // Skip if no sprite index

        jsr     map_room_to_sprite_coords
        jsr     draw_actor

next_costume_entry:
        inc     costume_index
        lda     costume_index
        cmp     #COSTUME_MAX_INDEX + 1
        bne     scan_costumes_for_redraw

        // --------------------------------------------------
        // Commit camera position for next frame
        // --------------------------------------------------
        lda     cam_target_pos
        sta     cam_current_pos

        // --------------------------------------------------
        // Wait for 4 IRQ cycles to pace the main loop
        // --------------------------------------------------
wait_irq_pacing_threshold:
        lda     irq_entry_count
        cmp     #IRQ_PACING_THRESHOLD
        bcc     wait_irq_pacing_threshold

        jmp     main_loop

/*
================================================================================
  update_walking_sounds
================================================================================

Summary
        Periodically checks eligible actors and triggers one walking–sound
        effect when appropriate. Uses a throttle counter to avoid playing step
        sounds every frame, and suppresses all walking sounds in specific
        exterior rooms.

Global Inputs
        current_room               Room ID used to filter out exterior areas
        walking_snd_throttle_ctr   Signed throttle counter controlling update rate
        costume_for_actor[]        Costume index for each actor (negative = unused)
        actor_motion_state[]       Motion state byte for each actor
        costume_anim_attrs[]       Per-costume animation attributes (low bits encode step SFX)
        task_cur_idx               Current task/script index

Global Outputs
        walking_snd_throttle_ctr   Reloaded after an underflow event
        task_cur_idx               Set to TASK_IDX_NONE when triggering a walking SFX
        (via call) start_sound_with_loaded_index may initiate a sound effect

Description
        - Exits immediately in rooms that suppress walking sounds.
        - Decrements a signed throttle counter and acts only when it underflows.
        - Resets the throttle interval and scans actors from highest index down.
        - Skips unused actors, actors in the excluded motion state, and costumes
          with no walking-sound identifier.
        - For the first qualifying actor, clears task context and invokes the
          walking-sound handler, then stops further scanning.
================================================================================
*/
.label  walking_snd_throttle_ctr = $066f    // Signed countdown controlling how often walking SFX are evaluated

* = $0714
update_walking_sounds:
		// ------------------------------------------------------------
		// Don't play walking sounds in the outdoor rooms that have cricket noises
        // ------------------------------------------------------------
        lda     current_room                  // A := current room id for exclusion tests
        cmp     #ROOM_FRONT_OF_MANSION        // Is this the first excluded exterior room?
        bne     check_second_room_filter      // If not, check the second excluded room
        rts                                   // In excluded room → never play walking sounds

check_second_room_filter:
        cmp     #ROOM_MANSION_ROAD_ENTRANCE   // Is this the second excluded exterior room?
        bne     throttle_counter_update       // If not, continue toward sound update logic
        rts                                   // In this room too → suppress walking sounds

throttle_counter_update:
		// ------------------------------------------------------------
        // Decrement throttle counter; only on signed underflow do we evaluate actors
		// ------------------------------------------------------------
        dec     walking_snd_throttle_ctr      // walking_snd_throttle_ctr-- (signed semantics)
        bpl     update_walking_sounds_exit    // If still ≥ 0, skip this tick and exit

		// ------------------------------------------------------------
        // Counter underflowed: reset period and scan actors this tick
		// ------------------------------------------------------------
        lda     #WALK_SND_THROTTLE_RESET      // Reload throttle interval (#calls between checks)
        sta     walking_snd_throttle_ctr      // Store new interval into throttle counter

        // Initialize actor scan from highest index down
        ldx     #ACTOR_MAX_INDEX              // X := top actor index to consider for step SFX
scan_actor_for_walksound:
		// ------------------------------------------------------------
        // Skip actors whose costume slot is unused
		// ------------------------------------------------------------
        ldy     costume_for_actor,x           // Y := costume index for actor X
        bmi     advance_to_next_actor         // If high bit set, actor slot unused → skip

		// ------------------------------------------------------------
        // Skip actors that are stopped
		// ------------------------------------------------------------
        lda     actor_motion_state,x          // A := encoded motion state for actor X
        cmp     #MOTION_STOPPED_CODE          // Compare against motion state we ignore for steps
        beq     advance_to_next_actor         // If equal, do not emit walking SFX for this actor

		// ------------------------------------------------------------
        // Skip actors that don't make walking sounds
		// ------------------------------------------------------------
        lda     costume_anim_attrs,y          // A := animation attributes for this costume
        and     #COSTUME_ATTR_STEP_SFX_MASK   // Keep low bits that encode step SFX index
        beq     advance_to_next_actor         // Zero → costume has no step SFX mapping; skip

		// ------------------------------------------------------------
        // Detach from any current task/script context and start walking SFX with index in A
		// ------------------------------------------------------------
        ldy     #TASK_IDX_NONE                // Y := sentinel meaning “no owning task/script”
        sty     task_cur_idx                  // Mark that this sound is not tied to a script index
        jsr     start_sound_with_loaded_index // Start walking sound using A as sound index

		// ------------------------------------------------------------
        // After starting one sound, force the loop to terminate on the next iteration
		// ------------------------------------------------------------
        ldx     #$00                          // Set X so that the following DEX exits the loop
advance_to_next_actor:
        dex                                   // Move to previous actor index (or 0→FF after sound)
        bpl     scan_actor_for_walksound      // Loop while X ≥ 0; X=$FF falls through to exit

update_walking_sounds_exit:
        rts                                   


/*
function main_loop():
    loop forever:
        // Optional debug hook
        call do_nothing_077B()

        // Reset IRQ pacing counter (main loop should run once per N IRQs)
        irq_entry_count = 0

        // --------------------------------------------------
        // Keyboard input
        // --------------------------------------------------
        keyboard_handle_one_key()

        // --------------------------------------------------
        // Debug/monitor mirrors (no gameplay effect)
        // --------------------------------------------------
        var_current_kid_idx             = current_kid_idx
        var_camera_position             = cam_target_pos
        var_msg_flag                    = topbar_mode
        var_current_room                = current_room
        var_music_playback_in_progress  = music_playback_in_progress

        // --------------------------------------------------
        // Cursor coordinate conversions
        // --------------------------------------------------
        // Convert cursor X from viewport-relative to room-absolute
        cursor_x_pos_quarter_absolute =
            cursor_x_pos_quarter_relative + viewport_left_col

        // Pre-bias cursor Y for later use
        cursor_y_pos_half_off_by_8 =
            cursor_y_pos_half - CURSOR_Y_BIAS

        // --------------------------------------------------
        // Script execution
        // --------------------------------------------------
        execute_running_tasks()

        // --------------------------------------------------
        // Inventory cleanup (remove limbo objects)
        // --------------------------------------------------
        if remove_obj_from_inv_flag:
            remove_obj_from_inv_flag = FALSE

            // Scan inventory slots from highest index down
            for inv_index from INVENTORY_SLOTS down to 1:
                obj_index = inventory_objects[inv_index]

                // Check object’s owner nibble
                owner_bits = object_attributes[obj_index] & OWNER_NIBBLE_MASK
                if owner_bits == OWNER_REMOVE_SENTINEL:
                    // Remove this object from inventory
                    op_remove_from_inventory(inv_index)

        // --------------------------------------------------
        // Active room gate
        // --------------------------------------------------
        if current_room == 0:
            // No active room yet → skip the rest of the frame
            continue  // jump to top of main_loop

        // --------------------------------------------------
        // Sentence processing (click/walk/use logic)
        // --------------------------------------------------
        handle_entity_approach_and_trigger()
        process_sentence()
        process_sentence_stack_entry()

        // --------------------------------------------------
        // Mouth animation toggling for speaking actors
        // --------------------------------------------------
        for actor_index from ACTOR_MAX_INDEX down to 0:
            if (actor_mouth_state[actor_index] & MOUTH_STATE_SPEAK_CLOSED):
                // Toggle mouth bit pattern (open ↔ closed)
                actor_mouth_state[actor_index] ^= MOUTH_TOGGLE_MASK

        // --------------------------------------------------
        // Actor movement, camera, and room redraw
        // --------------------------------------------------
        step_actor_motion_and_animation()
        cam_upd_target()

        // Default: assume no scroll
        screen_scroll_flag = CAM_DEFAULT_POS

        // If camera target changed, redraw the room
        if cam_target_pos != cam_current_pos:
            render_room_view()

            // Only run FG-actor refresh when a specific scroll mode is active
            if screen_scroll_flag == 1:
                // Refresh actors behind foreground layers
                for actor_index from ACTOR_MAX_INDEX down to 0:
                    if costume_for_actor[actor_index] < 0:
                        continue  // unused actor

                    if actor_box_attr[actor_index] == 0:
                        continue  // not behind foreground

                    actor_render_flags[actor_index] |= ACTOR_RENDER_REFRESH

        // --------------------------------------------------
        // Actor visibility and buffer swap
        // --------------------------------------------------
        step_actor_visibility_and_sprites()
        alternate_frame_buffer()

        // --------------------------------------------------
        // Redraw flagged actors and rotate sprite shapeset
        // --------------------------------------------------
        redraw_flagged_actors()

        target_sprite_shapeset = sprite_bank_sel + 1   // select next sprite shapeset

        // --------------------------------------------------
        // Shutter effect (fade-in / blinds)
        // --------------------------------------------------
        if open_shutter_flag:
            if global_lights_state == LIGHTS_ENVIRONMENT_ON:
                open_shutter()

            open_shutter_flag = FALSE
            hide_cursor_flag  = FALSE   // ensure cursor is visible

        // --------------------------------------------------
        // Walking sound generation
        // --------------------------------------------------
        update_walking_sounds()

        // --------------------------------------------------
        // Flashlight-beam logic (lights == flashlight-only)
        // --------------------------------------------------
        if global_lights_state == LIGHTS_FLASHLIGHT_ONLY:
            // If camera scroll is active, we force a redraw
            if screen_scroll_flag != CAM_DEFAULT_POS:
                goto flashlight_coords_changed

            // If cursor hasn’t moved, no flashlight update needed
            if cursor_x_pos_quarter_relative == flashlight_beam_x and
               cursor_y_pos_half            == flashlight_beam_y:
                goto synchronize_on_video_update

flashlight_coords_changed:
            clear_flashlight_area()

            flashlight_beam_x = cursor_x_pos_quarter_relative
            flashlight_beam_y = cursor_y_pos_half

            draw_flashlight_area()

            // Request color-only video update for flashlight effect
            video_setup_mode = VID_SETUP_COPY_COLORS

        // --------------------------------------------------
        // Video-update synchronization with IRQ
        // --------------------------------------------------
synchronize_on_video_update:
        video_update_signal = SIGNAL_SET

        // Wait until IRQ has completed the requested video update
        while video_update_signal:
            ;  // busy-wait

        // --------------------------------------------------
        // Second actor draw sweep (full costume scan)
        // --------------------------------------------------
        costume_index = 0

        while costume_index <= COSTUME_MAX_INDEX:
            x = costume_index
            actor_index = actor_for_costume[x]

            if actor_index >= 0:
                // Actor exists for this costume
                actor = actor_index
                active_costume = x

                // Only process visible actors
                if (actor_render_flags[actor] & ACTOR_RENDER_VISIBLE):
                    // Clear “clear-visible” flag bits
                    flags = actor_render_flags[actor]
                    flags &= ~ACTOR_RENDER_CLEAR_VISIBLE
                    actor_render_flags[actor] = flags

                    // If actor has a sprite assigned, update/draw it
                    if actor_sprite_index[actor] != NO_SPRITE:
                        map_room_to_sprite_coords(actor, active_costume)
                        draw_actor(actor, active_costume)

            costume_index += 1

        // --------------------------------------------------
        // Commit camera position for next frame
        // --------------------------------------------------
        cam_current_pos = cam_target_pos

        // --------------------------------------------------
        // Wait for 4 IRQ cycles to pace the main loop
        // --------------------------------------------------
        while irq_entry_count < IRQ_PACING_THRESHOLD:
            ;  // busy-wait until enough IRQ ticks have occurred

        // Loop back to top of main_loop
        // (implicit via outer "loop forever")



function update_walking_sounds():
    // ------------------------------------------------------------
    // Suppress walking sounds in specific outdoor rooms
    // ------------------------------------------------------------
    if current_room == ROOM_FRONT_OF_MANSION:
        return

    if current_room == ROOM_MANSION_ROAD_ENTRANCE:
        return

    // ------------------------------------------------------------
    // Throttle: only occasionally evaluate walking SFX
    // ------------------------------------------------------------
    walking_snd_throttle_ctr -= 1

    // If counter is still >= 0, skip this tick
    if walking_snd_throttle_ctr >= 0:
        return

    // Counter underflowed: reset it and scan actors this tick
    walking_snd_throttle_ctr = WALK_SND_THROTTLE_RESET

    // Start with highest actor index
    for actor_index from ACTOR_MAX_INDEX down to 0:
        costume_index = costume_for_actor[actor_index]

        // Skip unused actors (negative costume index)
        if costume_index < 0:
            continue

        // Skip stopped actors (no movement → no steps)
        if actor_motion_state[actor_index] == MOTION_STOPPED_CODE:
            continue

        // Check whether this costume defines a walking SFX
        attrs = costume_anim_attrs[costume_index]
        step_sfx_index = attrs & COSTUME_ATTR_STEP_SFX_MASK

        if step_sfx_index == 0:
            continue  // no walking sound defined

        // --------------------------------------------------------
        // Found an actor that should emit a walking sound
        // --------------------------------------------------------
        // This walking sound is not owned by any script/task.
        task_cur_idx = TASK_IDX_NONE

        start_sound_with_loaded_index(step_sfx_index)

        // Only play one walking sound per evaluation: break loop.
        break

*/