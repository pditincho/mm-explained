/*
================================================================================
  ops_cutscene.asm
================================================================================

Overview:
	Implements all logic for entering, running, and exiting cutscenes in the script
	engine. Provides supporting routines to suspend or unsuspend concurrent tasks and
	to set global control modes for gameplay or UI transitions.

Contained routines:
	- op_enter_cutscene (#$40): Saves gameplay/UI context and enters cutscene mode.
	- op_exit_cutscene (#$C0): Restores prior control mode and sentence data
	  after a cutscene finishes.
	- op_switch_control_mode (#$60, #$E0): Switches the engine’s control state (normal,
	  keypad, cutscene, disable-kid-change) and redraws or suspends tasks as needed.
	- suspend_other_tasks: Marks all other tasks as suspended.
	- resume_suspended_tasks: Clears suspended flags so background scripts resume.

Core mechanics:
	Cutscenes are treated as a separate “control mode” that halts background scripts
	and clears UI elements like the sentence bar. Before a cutscene, the system
	saves the current room, control mode, sentence composition, and task index.
	Afterward, it restores those values, re-centers the camera, and refreshes the UI.
	“Control mode” governs which input and rendering behaviors are active.

Data touched:
	- control_mode / saved_control_mode
	- current_room / saved_room_id
	- sentence_parts[0..SENTENCE_TOTAL_TOKENS]
	- stacked_io_id_hi[...] (used as sentence snapshot buffer)
	- task_state_tbl[...] (bit7 marks suspended)
	- interrupted_pc_lo/hi, interrupted_script_index
	- forced_sentence_trigger, sentence_bar_needs_refresh

Execution flow:
	1. Enter cutscene: suspend noncurrent tasks → clear sentence bar → save context.
	2. Run cutscene: game logic proceeds only in the current task.
	3. Exit cutscene: zero transient variables → restore prior state.
	   • If keypad mode, reload saved room and video state.
	   • Otherwise, restore camera, sentence, and control mode.
	   • Unsuspend all tasks to resume background activity.


|---------|------------------------|----------------------------------------------------------|
| Opcode  | Routine                | Purpose                                                  |
|---------|------------------------|----------------------------------------------------------|
|  $40    | op_enter_cutscene      | Save state, enter cutscene mode, suspend tasks.          |
|  $C0    | op_exit_cutscene       | Restore UI/control after cutscene, unsuspend tasks.      |
|  $60,$E0| op_switch_control_mode | Switch control modes and update screen regions.          |
|---------|------------------------|----------------------------------------------------------|

Notes:
- Slot $00 in task_state_tbl is intentionally skipped by suspend/unsuspend loops.
================================================================================
*/


#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "script_engine.asm"
#import "script_primitives.asm"
#import "room_loader.asm"
#import "ops_camera.asm"

.label  saved_control_mode    		= $FEA9    		// Previous control mode saved on entering cutscene
.label  saved_room_id         		= $FEAA    		// Room ID saved for keypad-mode return

.const  SENTENCE_TOTAL_TOKENS   	= $05   		// Max token index (0..5 → 6 tokens total) used by copy loops
.const  INTERACTION_AREA_LEN     	= $0118 		// Byte length of interaction region to clear
.const  INT_AREA_FILL_VALUE      	= SPACE_CHAR 	// Fill byte when clearing interaction area

/*
================================================================================
  op_enter_cutscene - Opcode #$40
================================================================================
Summary:
	Initializes and enters cutscene mode. Saves UI and task context, suspends all
	noncurrent tasks, snapshots current sentence tokens, resets the sentence system,
	and clears any forced trigger or interrupt return state.

Vars/State:
	X used as loop counter for sentence token copy.
	A used for general byte transfers.

Global Inputs:
	control_mode            – Current player control mode.
	current_room            – Current room index.
	sentence_parts          – Array of active sentence token parts.
	task_cur_idx            – Index of currently running task.

Global Outputs:
	saved_control_mode      – Stores previous control mode.
	saved_room_id           – Stores previous room ID.
	saved_task_idx          – Stores current task index for restoration.
	stacked_io_id_hi[...]   – Snapshot of sentence parts during cutscene.
	forced_sentence_trigger – Cleared to prevent unintended triggers.
	interrupted_pc_lo/hi    – Cleared to zero to mark no interrupt return.

Description:
	- Preserves gameplay context before running a scripted cutscene.
	- suspends other concurrent tasks.
	- Clears the sentence bar and resets UI.
	- Copies the current sentence composition for later restoration.
	- Resets forced-trigger and interrupt tracking variables.
	- Leaves system ready for script-driven cutscene execution.
================================================================================
*/
* = $677A
op_enter_cutscene:
		// Save control mode and room
		lda     control_mode                    
		sta     saved_control_mode
		lda     current_room                    
		sta     saved_room_id

		// Suspend all noncurrent tasks
		jsr     suspend_other_tasks       	
		
		// Clear bar + enter cutscene mode
		jsr     setup_cutscene                  

		// ------------------------------------------------------------
		// Save current sentence parts for later restoration
		// ------------------------------------------------------------
		ldx     #SENTENCE_TOTAL_TOKENS

copy_sentence_parts_loop:
		lda     sentence_parts,x
		sta     stacked_io_id_hi,x
		dex
		bpl     copy_sentence_parts_loop

		// ------------------------------------------------------------
		// Reset sentence system and clear forced trigger flag
		// ------------------------------------------------------------
		jsr     init_sentence_ui_and_stack
		lda     #FALSE
		sta     forced_sentence_trigger

		// ------------------------------------------------------------
		// Save current task context and clear interrupted task offsets
		// ------------------------------------------------------------
		lda     task_cur_idx
		sta     saved_task_idx
		
		lda     #$00
		sta     interrupted_pc_lo
		lda     #$00
		sta     interrupted_pc_hi
		rts
/*
================================================================================
  op_exit_cutscene - Opcode #$C0
================================================================================

Summary:
	Restores gameplay after a cutscene. Clears transient interrupt
	state, either returns to keypad mode with a room reload or restores normal
	mode, camera, and sentence tokens, then refreshes UI.

Vars/State:
	A used for loads/stores and as argument to set_control_mode.
	X used as descending loop counter when restoring sentence parts.
	Stack used to temporarily hold saved_control_mode (PHA/PLA).

Global Inputs:
	saved_control_mode          – Control mode prior to entering the cutscene.
	saved_room_id               – Room ID to reload when returning to keypad mode.
	current_kid_idx             – Actor index for camera re-centering.
	stacked_io_id_hi[...]       – Snapshot buffer holding prior sentence parts.

Global Outputs:
	var_destination_x           – Cleared to 0.
	saved_task_idx              – Cleared to 0.
	interrupted_script_index    – Cleared to 0.
	interrupted_pc_lo           – Cleared to 0.
	interrupted_pc_hi           – Cleared to 0.
	control_mode                – Restored directly (keypad path) or via set_control_mode.
	sentence_parts[...]         – Restored from snapshot (non-keypad path).
	sentence_bar_needs_refresh  – Set TRUE after restoring sentence/UI (non-keypad path).

Description:
	- Zeroes destination/interrupt bookkeeping created for cutscene control.
	- If previous mode was keypad:
		• Restores keypad control mode.  
		• Prepares video, reloads saved room, then unsuspends all tasks.
	- Otherwise:
		• Unsuspends tasks, re-centers camera on current kid.  
		• Restores sentence parts from snapshot using a descending inclusive loop.  
		• Restores prior control mode via set_control_mode and marks sentence bar dirty.
Notes:
	• The restore loop is inclusive (BPL) starting at SENT_STACK_LAST_INDEX.  
	• Unsuspend ordering differs by path by design to match UI/room setup needs.
================================================================================
*/
* = $67B0
op_exit_cutscene:
		// ------------------------------------------------------------
		// Clear transient destination and interrupt-return state
		//
		// Zero var_destination_x, saved_task_idx, interrupted_script_index,
		// and interrupted_pc_lo/hi to discard cutscene bookkeeping.
		// ------------------------------------------------------------
		lda     #$00
		sta     var_destination_x
		lda     #$00
		sta     saved_task_idx
		lda     #$00
		sta     interrupted_script_index
		lda     #$00
		sta     interrupted_pc_lo
		lda     #$00
		sta     interrupted_pc_hi

		// ------------------------------------------------------------
		// Check if previous control mode was keypad
		// ------------------------------------------------------------
		lda     saved_control_mode
		cmp     #CONTROL_MODE_KEYPAD
		bne     other_control_modes

		// ------------------------------------------------------------
		// Keypad mode — restore UI and room, then unsuspend tasks
		// ------------------------------------------------------------
		sta     control_mode                     // restore control mode
		jsr     prepare_video_for_new_room
		ldx     saved_room_id
		jsr     switch_to_room
		jsr     resume_suspended_tasks
		jmp     exit_return_cutscene

		// ------------------------------------------------------------
		// Non-keypad path entry
		//
		// Preserve previous control mode on stack, then resume all tasks.
		// ------------------------------------------------------------
other_control_modes:
		pha                                     
		jsr     resume_suspended_tasks              

		// ------------------------------------------------------------
		// Fix camera on current kid
		// ------------------------------------------------------------
		lda     current_kid_idx
		jsr     script_cam_follow_costume

		// ------------------------------------------------------------
		// Restore previous sentence parts
		// ------------------------------------------------------------
		ldx     #SENT_STACK_LAST_INDEX

restore_sentence_parts_loop:
		lda     stacked_io_id_hi,x
		sta     sentence_parts,x
		dex
		bpl     restore_sentence_parts_loop

		// ------------------------------------------------------------
		// Restore control mode and refresh UI
		// ------------------------------------------------------------
		pla                                     // A := saved control mode
		jsr     set_control_mode                // restore mode
		lda     #TRUE
		sta     sentence_bar_needs_refresh

exit_return_cutscene:
		rts		
/*
================================================================================
  op_switch_control_mode - Opcodes #$60, #$E0
================================================================================

Summary
	Dispatches a control-mode change from the script operand and updates UI and
	scheduler state. Modes supported: Cutscene, Keypad, Disable-kid-change, and
	Normal refresh.

Global Inputs
	(Script stream)  Next byte provides the desired mode

Global Outputs
	control_mode                 Updated to the selected mode
	sentence_bar_needs_refresh   Cleared when entering cutscene/keypad paths

Description
	* Read mode via script_load_operand_bit7, then branch:
		• Cutscene:
			* Clear the sentence bar via mem_fill_x (using fill_dest_ptr/fill_byte_cnt).
			* Set control_mode to Cutscene.
		• Keypad:
			* Perform Cutscene work, then set control_mode to Keypad.
			* suspend noncurrent tasks via suspend_other_tasks.
		• Disable kid change:
			* Perform Normal refresh work (see below), then set control_mode to Disable.
		• Normal refresh:
			* Temporarily set Cutscene, map I/O in, render_all_hotspots, map I/O out.
			* Set control_mode to Normal and unsuspend all tasks via resume_suspended_tasks.
		• Unknown mode: no-op return.
================================================================================
*/
* = $6889
op_switch_control_mode:
		// ------------------------------------------------------------
		// Resolve control mode
		// ------------------------------------------------------------
		jsr     script_load_operand_bit7        // A := control mode operand

set_control_mode:
		cmp     #CONTROL_MODE_CUTSCENE
		bne     check_mode_normal

case_mode_cutscene:
setup_cutscene:
		// ------------------------------------------------------------
		// Mode Cutscene — Clear sentence bar and enter cutscene mode
		// ------------------------------------------------------------
		lda     #FALSE
		sta     sentence_bar_needs_refresh
		
		lda     #<SENTENCE_BAR_BASE
		sta     fill_dest_ptr
		lda     #>SENTENCE_BAR_BASE
		sta     fill_dest_ptr + 1
		lda     #<INTERACTION_AREA_LEN
		sta     fill_byte_cnt
		lda     #>INTERACTION_AREA_LEN
		sta     fill_byte_cnt + 1
		ldx     #$20
		jsr     mem_fill_x
		
		lda     #CONTROL_MODE_CUTSCENE
		sta     control_mode
		rts

check_mode_normal:
		cmp     #CONTROL_MODE_NORMAL
		bne     check_mode_disable_kid

case_mode_normal:
		// ------------------------------------------------------------
		// Mode Normal — Redraw all hotspots and unsuspend tasks
		// ------------------------------------------------------------
		lda     #CONTROL_MODE_CUTSCENE
		sta     control_mode
		
		ldy     #MAP_IO_IN
		sty     cpu_port                         
		jsr     render_all_hotspots
		ldy     #MAP_IO_OUT
		sty     cpu_port                         
		
		lda     #CONTROL_MODE_NORMAL
		sta     control_mode
		
		jsr     resume_suspended_tasks
		rts

check_mode_disable_kid:
		cmp     #CONTROL_MODE_DISABLE_KID
		bne     check_mode_keypad

		// ------------------------------------------------------------
		// Mode Kid change disabled — Invoke case_mode_normal, then set mode to disable kid change
		// ------------------------------------------------------------
		jsr     case_mode_normal
		lda     #CONTROL_MODE_DISABLE_KID
		sta     control_mode
		rts

check_mode_keypad:
		cmp     #CONTROL_MODE_KEYPAD
		bne     unknown_mode		

		// ------------------------------------------------------------
		// Mode Keypad — Clear sentence bar, switch to keypad mode,
		//              suspend noncurrent tasks
		// ------------------------------------------------------------
		jsr     case_mode_cutscene
		
		lda     #CONTROL_MODE_KEYPAD
		sta     control_mode
		
		jsr     suspend_other_tasks
		rts

unknown_mode:
		rts
/*
================================================================================
  suspend_other_tasks
================================================================================
Summary:
	suspends all active tasks except the current one by setting bit7 in their
	task state entries.

Vars/State:
	A used for load/modify/store of task_state_tbl entries
	X used as descending loop counter from TASK_MAX_INDEX to $01

Global Inputs:
	task_cur_idx              – Index of currently executing task
	task_state_tbl[...]       – Current per-task state bytes

Global Outputs:
	task_state_tbl[...]       – Updated with bit7 set for all active, noncurrent tasks

Description:
	- Iterates from the highest task slot down to $01.
	- Skips the current task and any inactive slots.
	- For each other active slot, sets bit7 in the task state to mark it as suspended.
	- Slot $00 is intentionally excluded from modification.
================================================================================
*/ 
* = $6755
suspend_other_tasks:
		// ------------------------------------------------------------
		// Iterates task slots from #$0F down to #$01, skipping the
		// current task. For each active slot (state ≠ 0), sets bit7
		// in task_state_tbl[X] to mark it as suspended.
		// ------------------------------------------------------------
		ldx     #TASK_MAX_INDEX

check_task_state:
		cpx     task_cur_idx                   	// skip current task slot
		beq     next_task_2

		lda     task_state_tbl,x               	// load task state
		cmp     #TASK_STATE_INACTIVE            // inactive task?
		beq     next_task_2                    	// yes → skip

		ora     #TASK_MASK_SUSPENDED          	// set bit7 = suspended flag
		sta     task_state_tbl,x               	// update slot state

next_task_2:
		dex
		bne     check_task_state             	// loop until X == 0
		rts
/*
================================================================================
  resume_suspended_tasks
================================================================================

Summary:
	Clears the “suspended” flag from task state entries to resume execution of
	previously suspended tasks.

Vars/State:
	A used for load/modify/store of task_state_tbl entries
	X used as descending loop counter from TASK_MAX_INDEX to $01

Global Inputs:
	task_state_tbl           – Current per-task state bytes (bit7 encodes suspended)

Global Outputs:
	task_state_tbl           – Updated with bit7 cleared for slots $01..$0F


Description:
	* Iterate task slots from highest valid index down to $01.
	* For each slot, clear bit7 in the state byte to mark the task as unsuspended.
	* Slot $00 is intentionally not modified by this routine.
================================================================================
*/
* = $676C
resume_suspended_tasks:
		// ------------------------------------------------------------
		// Iterates task slots from #$0F down to #$01 and clears bit7
		// in each task_state_tbl entry, removing the “suspended” flag.
		// Slot #$00 is not touched due to the loop structure.
		// ------------------------------------------------------------
		ldx     #TASK_MAX_INDEX                 // X := last task slot index

unsuspend_task:
		lda     task_state_tbl,x            	// A := state for slot X
		and     #TASK_MASK_CLEAR_SUSPENDED    	// clear bit7 (suspended → off)
		sta     task_state_tbl,x               	// write updated state
		dex
		bne     unsuspend_task                	// loop while X != 0
		rts                                     // done

