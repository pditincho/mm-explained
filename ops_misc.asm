/*
================================================================================
Summary
        System and utility opcode handlers for the script engine. Implements
        random byte generation, task pause scheduling, lights/visual setup,
        save/load workflow, engine restart, sprite/sound and raster setup,
        sentence construction, sound/script status queries, and stopping the
        current script.

Arguments
        Operands are read from the script stream and vary per opcode. See the
        per-routine headers below for exact operand formats.

Returns
        None directly. Handlers update engine state, device registers, or write
        results into game_vars.

Global Inputs
        actor_vars                Actor variable array
        game_vars                 Script-visible variable array (read as needed)
        task_cur_idx              Current task index
        task_pause_counter_1..3   Per-task pause counters
        task_state_tbl            Per-task state table
        task_script_idx_tbl       Per-task script index table
        opcode                    Current opcode byte
        sound_liveness_tbl           Sound resource attribute bytes
        active_side_id            Current disk side identifier

Global Outputs
        game_vars                 Result bytes for many queries
        task_pause_counter_1..3   Updated from opcode #$2A
        task_state_tbl            Marked paused for opcode #$2A
        active_side_id            Set for save/load workflow
        vic_sprite_mcolor0_reg    Sprite multicolor 0 register
        vic_sprite_mcolor1_reg    Sprite multicolor 1 register
        cpu_port                  Processor port (I/O mapping)
        cam_current_pos           Camera default position after lights change

Description
        - Random: read dest and seed, generate a random byte, store to game_vars.
        - Pause: read three bytes into the current task’s pause counters and
          pause execution.
        - Lights: map I/O, clear or set view colors, unmap I/O, reset camera.
        - Save/Load: select save-game disk side, perform load or save, store
          status, fix relocations, and re-init raster IRQs.
        - Restart: load kid-select room, reset stack, jump to engine reset.
        - Init: setup sprites/sound; setup raster interrupts.
        - Sentence: parse verb and two object operands into the sentence stack.
        - Status queries: report if a sound is playing or a script is running.
        - Stop: dispatch stop for the current script.

Notes
        Relies on helpers from included modules:
        - random.asm        → generate_random_byte
        - render_room.asm          → clear_view_buffers, video/register symbols
        - save_game.asm     → load_state_from_disk, save_state_to_disk
        - ops_primitives.asm/script_primitives.asm → operand readers, utilities
================================================================================

 Opcode Table — system_ops.asm
================================================================================
 op_get_new_random              → 16                      Generate random byte using seed store to game var.
 op_set_script_pause_counters   → 2A                      Write three pause-counter bytes to current task pause.
 op_set_lights                  → 70, F0                  Set env lights and sprite multicolors reset camera.
 op_save_load_game              → 22, A2                  Save or load game; store status; fix relocations; init raster.
 op_restart_game                → 98                      Load kid-select room; reset stack; reinit engine.
 op_setup_sprites_and_sound     → 6E                      Initialize sprite and sound systems.
 op_raster_setup                → EE                      Initialize raster interrupt configuration.
 op_do_sentence                 → 03, 43, 83, C3          Queue verb, DO, IO, and placeholder preposition.
 op_is_sound_playing            → 7C, FC                  Write masked refcount; nonzero means sound is running.
 op_is_script_running           → 68, E8                  Scan task table; write 01 if script found else 00.
 op_stop_current_script         → 05, 09, 0A, 19, 23, 2C, 34, 39, 3B, 45, 49, 59, 63, 65, 6A, 6C, 79, 7A, 7B, 80, 
								  82, 85, 89, 8A, 8D, 96, 99, A3, A6, AA, AC, B5, B9, BB, C5, C9, D8, D9, E3, E6, 
								  EA, EC, F5, F9, FA, FB
														  Stop current script via dispatch_stop_by_index with X=00.
================================================================================
*/

#importonce
#import "globals.inc"
#import "constants.inc"
#import "script_primitives.asm"
#import "ops_primitives.asm"
#import "entry_point.asm"
#import "random.asm"
#import "render_room.asm"
#import "save_game.asm"

/*
================================================================================
  op_interrupt_script - Opcode #58
================================================================================

Summary
        Compute the interrupted script’s program-counter relative to the script
        base, save it, then record which script was interrupted. Discard the
        next three bytes from the stream (operand + offset).

Global Inputs
        task_pc_lo          Current script PC (lo), for computing interruption
        task_pc_hi          Current script PC (hi)
        script_base_lo      Script base address (lo), used for PC relocation
        script_base_hi      Script base address (hi)
        task_cur_idx        Active script index (which script was interrupted)

Global Outputs
        interrupted_pc_lo       Stored interrupted PC (lo), relocated
        interrupted_pc_hi       Stored interrupted PC (hi), relocated
        interrupted_script_index    Script index that was interrupted

Description
        - Subtract script_base from task_pc to obtain a relocatable interrupt PC.
        - Store the resulting low and high bytes.
        - Save the script index that was executing at interruption time.
        - Consume and discard the next three bytes in the script stream:
          • one raw byte
          • a two-byte offset (via script_skip_offset)
================================================================================
*/
* = $60B6
op_interrupt_script:
		lda     task_pc_lo
		sec
		sbc     script_base_lo
		sta     interrupted_pc_lo

		lda     task_pc_hi
		sbc     script_base_hi
		sta     interrupted_pc_hi

		// Save the interrupted script index
		lda     task_cur_idx
		sta     interrupted_script_index

		// The next 3 bytes are read and discarded
		jsr     script_read_byte
		jsr     script_skip_offset
		rts
/*
================================================================================
  op_get_new_random - Opcode #$16
================================================================================
Summary
        Read destination variable index and a seed. Call get_random_number(seed)
        and store the returned byte into the destination game variable.

Arguments
        (script) byte0  → destination game-var index
        (script) bit7   → seed index (lo from script, hi from opcode bit7)

Returns
        None (writes random byte to game_vars[dest])

Description
        - Reads destination variable index into destination_variable.
        - Loads seed index via script_load_operand_bit7, X := seed.
        - Calls get_random_number which returns a random byte in A.
        - Stores A into game_vars[destination_variable].
================================================================================
*/
* = $652F
op_get_new_random:
        // ----------------------------
        // Resolve destination variable index
        // ----------------------------
        jsr     script_read_byte              
        sta     dest_var          

        // ----------------------------
        // Resolve seed index
        // ----------------------------
        jsr     script_load_operand_bit7      
        tax                                   

        // ----------------------------
        // Generate new random byte using seed in X
        // ----------------------------
        jsr     generate_random_byte          

        // ----------------------------
        // Store result into game_vars[dest]
        // ----------------------------
        ldx     dest_var          
        sta     game_vars,x
        rts
/*
================================================================================
  op_set_script_pause_counters - Opcode #$2A
================================================================================
Summary
        Read three bytes from the script and write them into the current task’s
        pause counters (high→low order in *_3, *_2, *_1). Mark the script slot
        as paused (#$01) and jump to op_pause_task to stop execution.

Arguments
        (script) byte0 → pause_counter_3
        (script) byte1 → pause_counter_2
        (script) byte2 → pause_counter_1
================================================================================
*/
* = $659C
op_set_script_pause_counters:
        // ----------------------------
        // Read three bytes and set pause counters for current task
        // ----------------------------
        ldx     task_cur_idx                // X := current task index
        jsr     script_read_byte               
        sta     task_pause_counter_3,x      // counter[3] := byte0

        jsr     script_read_byte             
        sta     task_pause_counter_2,x      // counter[2] := byte1

        jsr     script_read_byte             
        sta     task_pause_counter_1,x      // counter[1] := byte2

        // ----------------------------
        // Mark script as paused and halt execution
        // ----------------------------
        lda     #TASK_STATE_PAUSED           
        sta     task_state_tbl,x 			// mark task paused
        jmp     op_pause_task               
/*
================================================================================
  op_set_lights - Opcodes #$70, #$F0
================================================================================
Summary
        Read global_lights_state from the script, map I/O, set sprite multicolors
        based on environment lights (off/on), then map I/O out and reset camera
        position.

Arguments
        (script) bit7 → global_lights_state (lo from script, hi from opcode bit7)

Description
        - global_lights_state = #$00: env off, flashlights off.
        - global_lights_state = #$01: env off, one flashlight on.
        - global_lights_state = #$02: env on.
        - For env off: clear scene, set mcolor0:=#00, mcolor1:=#0B.
        - For env on:  set mcolor0:=#0A, mcolor1:=#00.
        - Update VIC sprite multicolor registers, restore CPU port mapping,
          and set camera_current_position := #$C8.
================================================================================
*/
* = $69AB
op_set_lights:
        // ------------------------------------------------------------
        // Read lights status operand → global_lights_state
        // ------------------------------------------------------------
        jsr     script_load_operand_bit7
        sta     global_lights_state

        // ------------------------------------------------------------
        // Map in I/O (enable VIC/CIAs access)
        // ------------------------------------------------------------
        ldy     #MAP_IO_IN
        sty     cpu_port

        // ------------------------------------------------------------
        // Environment lights on? (status == #$02)
        // ------------------------------------------------------------
        lda     global_lights_state
        cmp     #LIGHTS_ENVIRONMENT_ON
        beq     environment_lights_on

        // ------------------------------------------------------------
        // Environment lights off → clear scene and set colors
        // mcolor0 := #$00, mcolor1 := #$0B
        // ------------------------------------------------------------
        jsr     clear_view_buffers
        ldy     #$00
        ldx     #$0b
        jmp     set_colors

environment_lights_on:
        // ------------------------------------------------------------
        // Environment lights on → set colors
        // mcolor0 := #$0A, mcolor1 := #$00
        // ------------------------------------------------------------
        ldx     #$00
        ldy     #$0a

set_colors:
        // ------------------------------------------------------------
        // Commit VIC sprite multicolors:
        //  mcolor0 → pairs '01'  (vic_sprite_mcolor0_reg)
        //  mcolor1 → pairs '11'  (vic_sprite_mcolor1_reg)
        // ------------------------------------------------------------
        sty     vic_sprite_mcolor0_reg
        stx     vic_sprite_mcolor1_reg

        // ------------------------------------------------------------
        // Map out I/O (restore RAM under I/O)
        // ------------------------------------------------------------
        ldy     #MAP_IO_OUT
        sty     cpu_port

        // ------------------------------------------------------------
        // Set default camera position and exit
        // ------------------------------------------------------------
        lda     #CAM_DEFAULT_POS
        sta     cam_current_pos
        rts
/*
================================================================================
  op_save_load_game - Opcodes #$22, #$A2
================================================================================
Summary
        Initialize sprites/sound and select disk side #$30. Read a destination
        game-var index and an operation selector. If selector == #$00, load
        state from disk; else save state. Store the callee’s status byte into
        the destination variable, then fix relocations and re-init raster.

Arguments
        (script) byte0  → destination game-var index
        (script) bit7   → operation selector (#$00=load, #$01=save)

Returns
        None (status stored to game_vars[dest])

Description
        - init_raster_and_sound_state
        - active_side_id := #GAME_DISK_ID_SAVEGAME
        - dest := script_read_byte
        - op := script_load_operand_bit7
            - op == #$00 → load_state_from_disk
            - else       → save_state_to_disk
        - game_vars[dest] := A
        - refresh_script_addresses_if_moved
        - raster_setup
================================================================================
*/
* = $6A04
op_save_load_game:
        // ------------------------------------------------------------
        // Prep hardware and select disk side
        // ------------------------------------------------------------
        jsr     init_raster_and_sound_state

        lda     #GAME_DISK_ID_SAVEGAME
        sta     active_side_id

        // ------------------------------------------------------------
        // Read destination variable index
        // ------------------------------------------------------------
        jsr     script_read_byte               
        sta     destination_variable

        // ------------------------------------------------------------
        // Read operation selector (#$00=load, #$01=save)
        // ------------------------------------------------------------
        jsr     script_load_operand_bit7       
        cmp     #$01
        bne     save_state_path                // A != #$01 → load path

        // ------------------------------------------------------------
        // Load state path (op == #$00)
        // ------------------------------------------------------------
        jsr     load_state_from_disk
        jmp     write_status_and_finalize

save_state_path:
        // ------------------------------------------------------------
        // Save state path (op == #$01)
        // ------------------------------------------------------------
        jsr     save_state_to_disk

write_status_and_finalize:
        // ------------------------------------------------------------
        // Store status byte into destination variable and finalize
        // ------------------------------------------------------------
        ldx     destination_variable           
        sta     game_vars,x                    

        jsr     refresh_script_addresses_if_moved     // fix script pointers if moved
        jsr     init_raster_irq_env                   // re-init raster configuration
        rts
/*
================================================================================
  op_restart_game - Opcode #$98
================================================================================
Summary
        Load the kid-select room, reset the CPU stack, and reinitialize the
        game engine state.
Description
        - Prepares video system for a room change.
        - Loads room #$00 (kid select).
        - Resets the 6502 stack pointer to #$FF.
        - Jumps to reset_game_engine to reinitialize global state.
================================================================================
*/
* = $6AC3
op_restart_game:
        // ------------------------------------------------------------
        // Load room #$00 (kid select)
        // ------------------------------------------------------------
        jsr     prepare_video_for_new_room
        ldx     #ROOM_IDX_START
        jsr     switch_to_room

        // ------------------------------------------------------------
        // Reset stack pointer to #$FF
        // ------------------------------------------------------------
        ldx     #$ff
        txs

        // ------------------------------------------------------------
        // Restart game state
        // ------------------------------------------------------------
        jmp     reset_game_engine
/*
================================================================================
  op_setup_sprites_and_sound - Opcode #$6E
================================================================================
Summary
        Initialize sprite and sound systems for the engine.
================================================================================
*/
* = $6AD1
op_setup_sprites_and_sound:
        jsr     init_raster_and_sound_state
        rts
/*
================================================================================
  op_raster_setup - Opcode #$EE
================================================================================
Summary
        Initialize raster interrupt configuration.
================================================================================
*/
* = $6AD5
op_raster_setup:
        jsr     init_raster_irq_env
        rts
/*
================================================================================
  op_do_sentence - Opcodes #$03, #$43, #$83, #$C3
================================================================================
Summary
        Read verb and two object operands from the script, derived by opcode
        mode. Push a new entry into the sentence stack with verb, direct object,
        indirect object, and a placeholder preposition to mark completeness.

Arguments
        (script) byte0  → verb id
        (script) next   → object operand(s) interpreted by opcode mode

Returns
        None
================================================================================
*/
* = $6724
op_do_sentence:
        // ------------------------------------------------------------
        // Advance to next sentence-stack slot and cache index in Y
        // ------------------------------------------------------------
        inc     sentstk_top_idx
        ldy     sentstk_top_idx

        // ------------------------------------------------------------
        // Read verb id and store into the queue
        // ------------------------------------------------------------
        jsr     script_read_byte
        sta     stacked_verb_ids,y

        // ------------------------------------------------------------
        // Seed helper with current opcode, then resolve direct object
        // ------------------------------------------------------------
        lda     opcode
        sta     input_opcode
        jsr     get_script_object_for_sentence     // returns low_byte/hi_byte

        // Save direct object (DO) lo/hi
        lda     low_byte
        sta     stacked_do_id_lo,y
        lda     hi_byte
        sta     stacked_do_id_hi,y

        // ------------------------------------------------------------
        // Select second object mode and resolve indirect object
        // ------------------------------------------------------------
        asl     input_opcode                        // choose IO mode
        jsr     get_script_object_for_sentence      // returns low_byte/hi_byte

        // Save indirect object (IO) lo and set preposition placeholder
        lda     low_byte
        sta     stacked_io_id_lo,y
        sta     stacked_prep_ids,y

        // Save indirect object (IO) hi
        lda     hi_byte
        sta     stacked_io_id_hi,y

        rts
/*
================================================================================
  op_is_sound_playing - Opcodes #$7C, #$FC
================================================================================
Summary
        Read destination variable index and a sound index. Load the sound’s
        attribute byte and mask with #$7F to get the reference count. Store the
        masked value; nonzero means the sound is running.

Arguments
        (script) byte0  → destination game-var index
        (script) bit7   → sound index hi selector; low byte read next

Returns
        None (writes masked refcount to game_vars[dest])
================================================================================
*/
* = $6562
op_is_sound_playing:
        // ----------------------------
        // Read destination variable index
        // ----------------------------
        jsr     script_read_byte              // A := dest var index
        sta     dest_var

        // ----------------------------
        // Read sound index (lo from script, hi from opcode bit7)
        // ----------------------------
        jsr     script_load_operand_bit7      // A := sound index
        tax                                     // X := sound index

        // ----------------------------
        // Load mem attrs and mask refcount bits (6..0)
        // ----------------------------
        lda     sound_liveness_tbl,x
        and     #$7f                          // A := refcount

        // ----------------------------
        // Store result into game_vars[dest]
        // ----------------------------
        ldx     dest_var
        sta     game_vars,x
        rts
/*
================================================================================
  op_is_script_running - Opcodes #$68, #$E8
================================================================================

Summary
        Read destination var index and a script index. Scan script slots from
        #$0F down to #$01. If any slot’s task_script_idx matches, return #$01;
        else return #$00. Store the result into game_vars[dest].

Arguments
        (script) byte0  → destination game-var index
        (script) bit7   → script index hi selector; low byte read next

Returns
        None (writes #$00 or #$01 to game_vars[dest])
================================================================================
*/
* = $6578
op_is_script_running:
        // ----------------------------
        // Read destination variable
        // ----------------------------
        jsr     script_read_byte              
        sta     dest_var

        // ----------------------------
        // Read script index to test
        // ----------------------------
        jsr     script_load_operand_bit7      

        // ----------------------------
        // Iterate script slots down to #$01
        // ----------------------------
        ldx     #TASK_MAX_INDEX

check_script:
        // Compare against task_script_idx_tbl[X]
        cmp     task_script_idx_tbl,x
        bne     next_script                 // no match → continue

        // Match found → result := #TRUE
        lda     task_state_tbl,x 			// note: redundant load (kept)
        lda     #TRUE
        jmp     oisr_set_result

next_script:
        dex
        bne     check_script                // keep scanning while X != 0

        // No matches → result := #FALSE
        lda     #FALSE

oisr_set_result:
        // ----------------------------
        // Store result into game_vars[dest]
        // ----------------------------
        ldx     dest_var
        sta     game_vars,x
        rts
/*
================================================================================
  op_stop_current_script 
================================================================================
  Opcodes 
	#$05, #$09, #$0A, #$19, #$23, #$2C, #$34, #$39, #$3B,
	#$45, #$49, #$59, #$63, #$65, #$6A, #$6C, #$79, #$7A,
	#$7B, #$80, #$82, #$85, #$89, #$8A, #$8D, #$96, #$99,
	#$A3, #$A6, #$AA, #$AC, #$B5, #$B9, #$BB, #$C5, #$C9,
	#$D8, #$D9, #$E3, #$E6, #$EA, #$EC, #$F5, #$F9, #$FA, #$FB

Summary
        Stop the current script by dispatching a stop-by-index with X := #$00.
================================================================================
*/
op_stop_current_script:
* = $6AD9
        ldx     #$00                           // X := 0 → current script slot
        jmp     dispatch_stop_by_index         // tail-call stop handler
