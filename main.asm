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

