#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "input_scan.asm"
#import "pause.asm"
#import "ui_messages.asm"
#import "sentence_action.asm"
#import "ops_misc.asm"

/*
================================================================================
  keyboard_handle_one_key
================================================================================
Summary
	Keyboard dispatcher for gameplay. Polls one key, then handles: 
		-resume	interrupted script (F7)
		-pause/resume (Space)
		-restart (F8)
		-text speed +/−
		-save/load (F2 under guard)
		-kid switching (F1/F3/F5 in normal control)

Global Inputs
	 interrupted_pc_lo/hi    		saved PC for interrupted script
	 interrupted_script_index       target script slot index
	 script_2_mem_attr              residency flag for global script #2
	 control_mode                   engine control mode
	 text_delay_factor              current text speed
	 sid_volfilt_reg_shadow         cached SID master volume

Global Outputs
	 task_pc_ofs_lo_tbl/hi[X]       restored PC for resumed script
	 task_state_tbl[X]    			set to running when resuming
	 interrupted_pc_lo/hi    		cleared after resume attempt
	 task_cur_idx                	set to #$FF before restart/save-load
	 sentence_bar_needs_refresh     flagged TRUE after unpause
	 text_delay_factor              clamped within [MIN..MAX]
	 kbd_delay_lo/hi                forced to zero in pause loop
	 sid_master_volume              muted on pause, restored on resume

Description
	* Read key once, store for reuse, then run a linear dispatch chain.
	* F7: if an interrupted script PC exists, restore PC/state and clear saved PC.
	* Space: print “PAUSED” msg, map I/O on, mute audio, poll until Space, 
		then restore audio/map, unpause, request UI refresh, and clear pause msg.
	* F8: mark no current script in execution and jump to restart.
	* ‘+’/‘−’: adjust text speed with floor/ceiling clamps.
	* F2: start save/load script only if not in cutscene and script #2 not resident.
	* F1/F3/F5: switch active kid to 0/1/2 only in normal control mode.

Notes
	* Uses busy-wait polling during pause; kbd delays are forced to zero each tick.
================================================================================
*/
  
.label key_to_process            = $29F7

//-----------------------------
// Keyboard keycodes
//-----------------------------
.const  KEY_F1                  = $01    // F1 → kid index 0
.const  KEY_F3                  = $02    // F3 → kid index 1
.const  KEY_F5                  = $03    // F5 → kid index 2
.const  KEY_F7                  = $04    // resume interrupted script
.const  KEY_F2                  = $05    // save/load menu
.const  KEY_F8                  = $08    // restart game
.const  KEY_PLUS                = $2B    // text speed faster
.const  KEY_MINUS               = $2D    // text speed slower

//-----------------------------
// Script and slots
//-----------------------------
.const  SCRIPT_ID_SAVELOAD      = $02    // global script #2

//-----------------------------
// Text speed bounds
//-----------------------------
.const  TEXT_DELAY_MIN          = $00    // fastest
.const  TEXT_DELAY_MAX          = $0A    // slowest allowed
.const  TEXT_DELAY_MAX_PLUS1    = $0B    // compare guard for clamp

//-----------------------------
// UI and I/O
//-----------------------------
.const  SID_VOL_MUTE            = $00    // mute master volume
.const  KBD_POLL_IMMEDIATE      = $00    // force immediate keyboard poll

* = $28C2
keyboard_handle_one_key:
        // ------------------------------------------------------------
        // Read one key and keep it for later condition checks
        // ------------------------------------------------------------
        jsr     kbd_key_read                // poll keyboard; A := keycode
        sta     key_to_process              // latch key for later comparisons

        // ------------------------------------------------------------
        // F7: bypass a cutscene
		//
		// Resume an interrupted script if applicable
        // ------------------------------------------------------------
        cmp     #KEY_F7                     // F7 pressed?
        bne     space_key_check             // no → continue dispatch
		
        lda     interrupted_pc_lo  			// test saved offset lo
        ora     interrupted_pc_hi  			// merge with hi; Z=1 if both zero
        beq     bypass_cutscene_return      // none saved → nothing to resume

		//There was an interrupted script, resume it
        ldx     interrupted_script_index    // X := target script index
        lda     interrupted_pc_lo  			// restore script PC low
        sta     task_pc_ofs_lo_tbl,x
        lda     interrupted_pc_hi  			// restore script PC high
        sta     task_pc_ofs_hi_tbl,x
		
		// Set script state to running
        lda     #TASK_STATE_RUNNING
        sta     task_state_tbl,x
		// Unknown purpose for debug var
        lda     #$01                          
        sta     var_target_x
		// Clear saved script PC lo/hi
        lda     #$00                          
        sta     interrupted_pc_lo
        lda     #$00                          
        sta     interrupted_pc_hi
		
bypass_cutscene_return:
        rts                                   

space_key_check:
        // ------------------------------------------------------------
        // Space: pause until Space is pressed again
        // ------------------------------------------------------------
        cmp     #KEY_SPACE                    // Space pressed?
        bne     f8_key_check                  // no → next key
		
        jsr     pause_game                    // enter paused state

        // ------------------------------------------------------------
        // Draw "PAUSED" banner across the sentence bar
        // ------------------------------------------------------------
        ldy     #SENTENCE_BAR_LAST_IDX        // Y := last cell index
pause_banner_copy_loop:
        lda     game_paused_flag_msg,y        // read banner char
        sta     SENTENCE_BAR_BASE,y           // write to sentence bar
        dey                                   // step left
        bpl     pause_banner_copy_loop        // loop until Y wraps to $FF

        // ------------------------------------------------------------
        // Map I/O on and mute audio while paused
        // ------------------------------------------------------------
        ldy     #MAP_IO_IN                    
        sty     cpu_port                      // enable I/O space mapping
        lda     #SID_VOL_MUTE                 
        sta     sid_master_volume             // mute SID master volume

pause_wait_for_space_loop:
        // ------------------------------------------------------------
        // Force immediate keyboard polling; wait for Space to resume
        // ------------------------------------------------------------
        lda     #$00                          // zero debounce/delay lo
        sta     kbd_delay_lo
        lda     #$00                          // zero debounce/delay hi
        sta     kbd_delay_hi

        jsr     kbd_key_read                  // poll again
        cmp     #KEY_SPACE                    // Space pressed?
        bne     pause_wait_for_space_loop     // no → keep waiting

        // ------------------------------------------------------------
        // Restore audio and memory map, then unpause and refresh UI
        // ------------------------------------------------------------
        lda     sid_volfilt_reg_shadow        // restore cached volume
        sta     sid_master_volume
		
        ldy     #MAP_IO_OUT                   // normal mapping value
        sty     cpu_port

        jsr     unpause_game                  // leave paused state
		
        lda     #TRUE                         // request UI refresh
        sta     sentence_bar_needs_refresh
		
        // ------------------------------------------------------------
        // Clear the "PAUSED" banner
        // ------------------------------------------------------------
        ldy     #SENTENCE_BAR_LAST_IDX        // Y := last cell index
pause_banner_clear_loop:
        lda     #$00                          // space/blank
        sta     SENTENCE_BAR_BASE,y           // clear char cell
        dey                                   // step left
        bpl     pause_banner_clear_loop       // loop through all cells
        rts                                   // done with Space handling

f8_key_check:
        // ------------------------------------------------------------
        // F8: restart game immediately
        // ------------------------------------------------------------
        cmp     #KEY_F8                       // F8 pressed?
        bne     plus_key_check                // no → next key
		
        lda     #TASK_IDX_NONE                // sentinel "no current script"
        sta     task_cur_idx
		
        jmp     op_restart_game               // tail-call restart

plus_key_check:
        // ------------------------------------------------------------
        // '+' : faster text (dec delay) with floor clamp
        // ------------------------------------------------------------
        cmp     #KEY_PLUS                     // '+' pressed?
        bne     minus_key_check               // no → next key
		
        dec     text_delay_factor             // decrease delay
        bpl     text_speed_faster_return      // if >= 0 → within range
		
        lda     #TEXT_DELAY_MIN               // clamp to minimum
        sta     text_delay_factor
text_speed_faster_return:
        rts                                   // done with '+'

minus_key_check:
        // ------------------------------------------------------------
        // '-' : slower text (inc delay) with ceiling clamp
        // ------------------------------------------------------------
        cmp     #KEY_MINUS                    // '-' pressed?
        bne     fkeys_dispatch_check          // no → next block
		
        inc     text_delay_factor             // increase delay
		
        lda     text_delay_factor             // test against max+1
        cmp     #TEXT_DELAY_MAX_PLUS1         // reached one past max?
        bne     text_speed_slower_return      // no → within range
		
        lda     #TEXT_DELAY_MAX               // clamp to maximum
        sta     text_delay_factor
text_speed_slower_return:
        rts                                   // done with '-'

fkeys_dispatch_check:
        // ------------------------------------------------------------
        // F2 save/load: only when not in cutscene and save/load script not resident
        // ------------------------------------------------------------
        lda     control_mode                  // read current mode
        cmp     #CONTROL_MODE_CUTSCENE        // cutscene mode?
        beq     kid_switch_mode_check         // yes → can't save/load now

        lda     key_to_process                // reuse latched key
        cmp     #KEY_F2                       // F2 pressed?
        bne     kid_switch_mode_check         // no → skip
		
		// F2 pressed - run save/load script (#2)
        lda     script_2_mem_attr             // script #2 resident?
        bne     kid_switch_mode_check         // yes → we're already running it, skip
		
        lda     #TASK_IDX_NONE                // set "no current script"
        sta     task_cur_idx
		
        lda     #SCRIPT_ID_SAVELOAD           // A := script #2 id
        jmp     launch_global_script          // tail-call launcher

kid_switch_mode_check:
        // ------------------------------------------------------------
        // Kid switching only allowed in normal player control
        // ------------------------------------------------------------
        lda     control_mode                  // read mode
        cmp     #CONTROL_MODE_NORMAL          // mode == player control?
        bne     keyboard_handler_return       // no → exit

        // ------------------------------------------------------------
        // F1/F3/F5 select kids 0/1/2 respectively
        // ------------------------------------------------------------
        lda     key_to_process                // latched key
        cmp     #KEY_F1                       // F1?
        bne     f3_key_check
		
        lda     #$00                          // kid index 0
        jmp     switch_active_kid_if_different// tail-call switch

f3_key_check:
        cmp     #KEY_F3                       // F3?
        bne     f5_key_check
		
        lda     #$01                          // kid index 1
        jmp     switch_active_kid_if_different

f5_key_check:
        cmp     #KEY_F5                       // F5?
        bne     keyboard_handler_return
		
        lda     #$02                          // kid index 2
        jmp     switch_active_kid_if_different

keyboard_handler_return:
        rts                                   // default path: no action

/*
procedure handleOneKey():
    // Get at most one pending key event from the input system
    key = readNextKey()          // returns null or a symbolic key value
    if key is null:
        return

    // Keep a copy for places where we need to re-check it later
    cachedKey = key

    //-----------------------------------------------------------------
    // F7 – Resume interrupted script / bypass cutscene
    //-----------------------------------------------------------------
    if key == KEY_F7:
        if thereIsAnInterruptedScript():
            resumeInterruptedScript()   // restore program counter & state
            clearInterruptedScript()    // make resume one-shot
        return

    //-----------------------------------------------------------------
    // SPACE – Pause/unpause the game
    //-----------------------------------------------------------------
    if key == KEY_SPACE:
        enterPauseState()               // mark game as paused

        // Show a "PAUSED" banner in the UI
        showPausedBannerInSentenceBar()

        // Mute all game audio while paused
        muteMasterAudio()

        // Busy-wait loop until SPACE is pressed again
        loop:
            // Force the input system to check the keyboard immediately
            forceImmediateKeyboardPoll()

            nextKey = readNextKey()
            if nextKey == KEY_SPACE:
                break loop
            // otherwise, keep looping while game is paused

        // Restore audio and leave pause state
        restoreMasterAudioFromShadow()
        leavePauseState()

        // Tell the UI that the sentence bar needs to be redrawn
        markSentenceBarForRefresh()

        // Remove the "PAUSED" banner
        clearSentenceBarBanner()

        return

    //-----------------------------------------------------------------
    // F8 – Restart the game
    //-----------------------------------------------------------------
    if key == KEY_F8:
        clearCurrentScriptSelection()
        restartGame()                    // restart from title / initial state
        return

    //-----------------------------------------------------------------
    // '+' – Increase text speed (lower delay)
    //-----------------------------------------------------------------
    if key == KEY_PLUS:
        decreaseTextDelayFactor()        // faster text
        clampTextDelayToMinimum()        // ensure delay ≥ MIN
        return

    //-----------------------------------------------------------------
    // '-' – Decrease text speed (higher delay)
    //-----------------------------------------------------------------
    if key == KEY_MINUS:
        increaseTextDelayFactor()        // slower text
        clampTextDelayToMaximum()        // ensure delay ≤ MAX
        return

    //-----------------------------------------------------------------
    // F2 – Open save/load UI (only in allowed modes)
    //-----------------------------------------------------------------
    if gameControlMode() != CUTSCENE_MODE:
        if cachedKey == KEY_F2:
            if not isSaveLoadScriptResident():
                clearCurrentScriptSelection()
                launchGlobalSaveLoadScript()
                return

    //-----------------------------------------------------------------
    // F1 / F3 / F5 – Kid/character switching (only in normal control)
    //-----------------------------------------------------------------
    if gameControlMode() != NORMAL_CONTROL_MODE:
        // No character switching outside normal gameplay
        return

    if cachedKey == KEY_F1:
        switchActiveCharacterTo(0)       // first kid / character slot
        return

    if cachedKey == KEY_F3:
        switchActiveCharacterTo(1)       // second kid / character slot
        return

    if cachedKey == KEY_F5:
        switchActiveCharacterTo(2)       // third kid / character slot
        return

    //-----------------------------------------------------------------
    // Any other key is ignored by this handler (handled elsewhere or not at all)
    //-----------------------------------------------------------------
    return
*/