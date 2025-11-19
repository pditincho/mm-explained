#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "rsrc_mgmt.asm"
#import "opcode_handlers.inc"

.label inlined_handler   = $5dab        // Indirect JSR target patched to handler address at runtime
.label debug_semaphore   = $2F          // Debug flag and synchronization byte (bit7 = skip wait)
.label inlined_script_pc = $5E15        // Temporary storage for inlined script PC (current read pointer)
.label script_idx        = $15          // Zero-page variable holding current script index

/*
================================================================================
  execute_running_tasks
================================================================================
Summary
	Sweep script tasks from the current index to TASK_TOTAL-1. For each task in
	RUNNING state, set base/PC and dispatch exactly one opcode via the handler
	table. After the sweep, wrap cur_script_slot to #$01 and return.
	
Global Inputs
	cur_script_slot   	starting task index for this sweep
	task_state_tbl  per-task state array used to test RUNNING

Global Outputs
	cur_script_slot   incremented during sweep; set to #$01 at end

Description
	* Read X := cur_script_slot and test task_state_tbl[X] for RUNNING.
	* If RUNNING, set script base, compute task PC, and dispatch one opcode.
	* Increment cur_script_slot and repeat until it equals TASK_TOTAL.
	* On completion, set cur_script_slot := #$01 and return.

Notes
	* Handlers must yield after one opcode to prevent starvation.
================================================================================
*/
* = $5D71
execute_running_tasks:
		ldx     cur_script_slot                    // X := current task index

		// ------------------------------------------------------------
		// Skip non-running tasks
		// ------------------------------------------------------------
		lda     task_state_tbl,x                // A := state of task X
		cmp     #TASK_STATE_RUNNING             // running?
		bne     next_task                       // no → advance to next

		// ------------------------------------------------------------
		// Execute one opcode for task X
		// ------------------------------------------------------------
		stx     task_cur_idx                    // latch X as current task
		jsr     set_script_base_from_type       // script_base := by script type
		jsr     set_current_task_pc             // task_pc := base + saved offset
		jsr     dispatch_script_ops_loop        // run one opcode (handler yields)

next_task:
		// ------------------------------------------------------------
		// Advance to next task or finish sweep
		// ------------------------------------------------------------
		inc     cur_script_slot                    // cur_script_slot := cur_script_slot + 1
		lda     cur_script_slot
		cmp     #TASK_TOTAL                     // reached end?
		bne     execute_running_tasks         // no → continue sweep

		// ------------------------------------------------------------
		// Wrap to task #$01 and return
		// ------------------------------------------------------------
		lda     #$01                            // first valid task index
		sta     cur_script_slot
		rts
/*
================================================================================
  dispatch_script_ops_loop
================================================================================
Summary
	Dispatch loop for script opcodes. Reads the next opcode byte, stores it, looks
	up the handler address from the opcode handler tables, writes that address into
	the inlined call site pointer, then JSRs through it. Execution repeats forever;
	handlers must yield control back to this loop.

Returns
	None (non-returning loop; only stops if caller replaces control flow)

Global Inputs
	opcode_handlers_lo  low-byte table mapping opcode → handler address
	opcode_handlers_hi  high-byte table mapping opcode → handler address

Description
	* Call script_read_byte to fetch the next opcode into A.
	* Store A into opcode for handler-side use.
	* Use X := opcode to index opcode_handlers_lo/hi and fetch handler address.
	* Write the address into inlined_handler (the indirect JSR call site).
	* JSR through the inlined call site at $0000 to run the handler.
	* Jump back and repeat to fetch and dispatch the next opcode.

Notes
	* Handlers must return to this loop after one operation to avoid starvation.
	* The handler tables must cover all reachable opcodes to prevent undefined jumps.
================================================================================
*/
* = $5D97
dispatch_script_ops_loop:
		// Read an operation byte → A := next opcode
		jsr     script_read_byte

		// Cache opcode in RAM for handlers that inspect it
		sta     opcode

		// Resolve handler entry point into the inlined call site ($5DAB)
		tax                                     // X := opcode (table index)
		lda     opcode_handlers_lo,x            // A := handler LO
		sta     inlined_handler                // write call-site LO
		lda     opcode_handlers_hi,x            // A := handler HI
		sta     inlined_handler + 1              // write call-site HI

		// Tail-call the resolved handler via the inlined JSR target
		jsr     $0000                           // jumps to handler at inlined_handler

		// Loop back to fetch and dispatch the next opcode
		jmp     dispatch_script_ops_loop
/*
================================================================================
  save_task_relative_offset
================================================================================

Summary
	Update the script’s saved relative offset before halting so execution can
	resume at the correct position later. Delegates to compute_task_relative_offset.

Global Inputs
	task_cur_idx   	selects which task’s offset to update
	task_pc_lo     	current absolute PC low byte
	task_pc_hi     	current absolute PC high byte
	script_base_lo  base address low byte
	script_base_hi  base address high byte

Global Outputs
	task_pc_ofs_lo_tbl  entry at task_cur_idx updated with (PC − base).lo
	task_pc_ofs_hi_tbl  entry at task_cur_idx updated with (PC − base).hi

Description
	* Call compute_task_relative_offset to store task_pc − script_base into the
	  per-task offset tables for the current task.

Notes
	* Must be called after any change to task_pc that should persist across halts.
================================================================================
*/
* = $5DB0
save_task_relative_offset:
        // ------------------------------------------------------------
        // Update the script’s relative offset before halting execution
        // so we can resume from the correct position later.
        // ------------------------------------------------------------
        jsr     compute_task_relative_offset
        rts
/*
================================================================================
  refresh_script_addresses_if_moved
================================================================================
Summary
	Refresh script addressing if the script’s memory base may have moved. If a
	current task exists and base_hi is set, immediately recompute absolute PC.
	If base_hi is clear, optionally spin until a debug semaphore is set, then
	recompute base and PC.

Global Inputs
	task_cur_idx   	current script task index used to test for NONE and for offset
	script_base_hi  nonzero indicates a valid loaded base address
	debug_semaphore bit7 skips wait; zero triggers spin until nonzero

Global Outputs
	script_base_lo       reset by set_script_base_from_type
	script_base_hi       reset by set_script_base_from_type
	task_pc_lo           recomputed by set_current_task_pc
	task_pc_hi           recomputed by set_current_task_pc

Description
	* Exit immediately if task_cur_idx == TASK_IDX_NONE.
	* If script_base_hi != 0, jump to address refresh path.
	* Else, if debug_semaphore bit7 is set, skip the wait loop.
	* Otherwise clear debug_semaphore, map I/O in, ping border color while
	  spinning until debug_semaphore becomes nonzero, then map I/O out.
	* Recompute the relative offset (compute_task_relative_offset),
	  reload base from script type (set_script_base_from_type),
	  and rebuild absolute PC = base + saved offset (set_current_task_pc).

Notes
	* The wait loop provides a different border color while polling.
	* Hand-off expects another context to set debug_semaphore nonzero.
================================================================================
*/
* = $5DB4
refresh_script_addresses_if_moved:
		// ------------------------------------------------------------
		// Guard: exit if no current script is running
		// ------------------------------------------------------------
		lda     task_cur_idx                    // A := current script index
		cmp     #TASK_IDX_NONE                  // A == NONE ?
		beq     exit_hsr                        // yes → nothing to refresh

		// ------------------------------------------------------------
		// Fast path: if base_hi is nonzero, we already have a base
		// → go refresh addresses immediately
		// ------------------------------------------------------------
		lda     script_base_hi                  // A := base_hi
		bne     refresh_script_addresses        // base set → adjust pointers

		// ------------------------------------------------------------
		// Unexpected path: script running but base_hi not set
		// If debug_semaphore bit7 is set, skip the wait loop.
		// Otherwise, clear semaphore and spin until it becomes nonzero.
		// ------------------------------------------------------------
		lda     debug_semaphore                 // read semaphore flags
		bmi     refresh_script_addresses        // bit7=1 → skip wait

		lda     #$00                            // reset semaphore
		sta     debug_semaphore

		ldy     #MAP_IO_IN                      // map I/O in for visual ping
		sty     cpu_port

spin_until_semaphore_set:
		lda     #$01                            // set border color = 1 (ping)
		sta     vic_border_color_reg
		lda     debug_semaphore                 // poll semaphore
		beq     spin_until_semaphore_set        // 0 → continue spinning

		ldy     #MAP_IO_OUT                     // restore normal memory map
		sty     cpu_port

		// ------------------------------------------------------------
		// Handle possible memory relocation:
		// 1) recompute current PC offset relative to prior base
		// 2) restore base from script type
		// 3) rebuild absolute PC = new base + saved offset
		// ------------------------------------------------------------
refresh_script_addresses:
		jsr     compute_task_relative_offset    // Y:=cur idx; save base→PC delta
												// (base and read_ptr unchanged)
		jsr     set_script_base_from_type       // reload script resource base
		jsr     set_current_task_pc             // PC := base + saved offset
exit_hsr:
		rts                                     	
/*
================================================================================
  set_current_task_pc
================================================================================
Summary
	Compute absolute PC for the current task:
	task_pc = script_base + task_pc_ofs[task_cur_idx] using a 16-bit add.

Global Inputs
	task_cur_idx            selects which offset entry to use
	script_base_lo/hi       base address of the current script resource
	task_pc_ofs_lo_tbl      per-task low-byte offsets from base
	task_pc_ofs_hi_tbl      per-task high-byte offsets from base

Global Outputs
	task_pc_lo              updated absolute PC low byte
	task_pc_hi              updated absolute PC high byte

Description
	* Load Y := task_cur_idx to choose the offset entry.
	* Add offs_lo to base_lo → task_pc_lo.
	* Add offs_hi plus carry to base_hi → task_pc_hi.
	* Result is base + offset.

Notes
	* Assumes offset tables hold valid offsets for the selected task.
	* Must be called after script_base is valid.
================================================================================
*/
* = $5DE3
set_current_task_pc:
		// ------------------------------------------------------------
		// Compute absolute PC for current task: task_pc = script_base +
		// task_pc_ofs[task_cur_idx]. 16-bit add with carry propagation.
		// ------------------------------------------------------------
		ldy     task_cur_idx                    // Y := select current task's offset entry
		
		lda     script_base_lo                  // A := base_lo
		clc                                     
		adc     task_pc_ofs_lo_tbl,y            // A := base_lo + offs_lo; C captures low-byte carry
		sta     task_pc_lo                      // write result low byte
		
		lda     script_base_hi                  // A := base_hi
		adc     task_pc_ofs_hi_tbl,y            // A := base_hi + offs_hi + prior carry
		sta     task_pc_hi                      // write result high byte
		rts                                     // return with task_pc = base + offset
/*
================================================================================
  compute_task_relative_offset
================================================================================
Summary
	Compute and store the current task’s relative PC offset:
	task_pc_ofs := task_pc − script_base (16-bit subtract).

Global Inputs
	task_cur_idx  	selects which offset slot to update
	task_pc_lo    	current absolute PC low byte
	task_pc_hi    	current absolute PC high byte
	script_base_lo  base address low byte
	script_base_hi  base address high byte

Global Outputs
	task_pc_ofs_lo_tbl  at index task_cur_idx, updated with (PC − base).lo
	task_pc_ofs_hi_tbl  at index task_cur_idx, updated with (PC − base).hi

Description
	* Load Y := task_cur_idx to choose the destination offset entry.
	* Subtract base_lo from PC_lo with SEC preset to form offs_lo and borrow.
	* Subtract base_hi and incoming borrow from PC_hi to form offs_hi.
	* Store offs_lo/offs_hi into the per-task offset tables.

Notes
	* Requires valid task_pc_* and script_base_* before invocation.
================================================================================
*/
* = $5DFA
compute_task_relative_offset:
        // ------------------------------------------------------------
        // Compute and store this task’s relative PC offset:
        // task_pc_ofs := task_pc - script_base (16-bit subtract)
        // ------------------------------------------------------------
        ldy     task_cur_idx                   // Y := current task index
		
        lda     task_pc_lo                     // A := PC.lo
        sec                                    // prepare borrow for 16-bit sub
        sbc     script_base_lo                 // A := PC.lo - base.lo
        sta     task_pc_ofs_lo_tbl,y           // offs_lo[Y] := result.lo

        lda     task_pc_hi                     // A := PC.hi
        sbc     script_base_hi                 // A := PC.hi - base.hi - borrow
        sta     task_pc_ofs_hi_tbl,y           // offs_hi[Y] := result.hi
        rts
/*
================================================================================
  script_skip_offset
================================================================================

Summary:
    Advance the script read pointer by two bytes, effectively skipping a
    16-bit value. Uses two byte reads via script_read_byte; return value is
    intentionally ignored by callers.

Global Inputs:
    inlined_script_pc        self-modified absolute address of script stream
    task_pc_lo, task_pc_hi   current script read pointer (used by callee)

Global Outputs:
    task_pc_lo, task_pc_hi   incremented by two via script_read_byte

Description:
    - Call script_read_byte to consume and discard the low byte.
    - Call script_read_byte again to consume and discard the high byte.
    - Net effect: PC := PC + 2.
================================================================================
*/
* = $5E11
script_skip_offset:
        jsr     script_read_byte               // Discard first byte; falls through to read next
/*
================================================================================
 script_read_byte
================================================================================
Summary:
    Read one byte from the current script stream and advance the task program
    counter (task_pc_lo/hi) by one. Used by the opcode dispatch loop.

Returns:
    A = byte value read from *(script_base + task_pc)
    task_pc_lo/hi incremented by 1

Global Inputs:
    inlined_script_pc    	self-modified absolute addressing of script stream
    task_pc_lo/hi			current script read pointer

Global Outputs:
    task_pc_lo/hi  			incremented by one byte position

Description:
    - Load next byte from the inlined script stream (script_base + PC).
    - Increment the task PC low byte; if it wraps, increment the high byte.
    - Return with A holding the fetched byte.
================================================================================
*/
* = $5E14
script_read_byte:
        lda     $FFFF // inlined_script_pc ; A := *(script_base + PC)
        inc     task_pc_lo                     // PC.lo++
        bne     read_done                      // if no wrap, done
        inc     task_pc_hi                     // wrap → PC.hi++
read_done:
        rts
/*
================================================================================
set_script_base_from_type
================================================================================
Summary:
    Compute script_base_lo/hi for the current task based on its script type.
    Selects the proper resource base (GLOBAL, ROOM, OBJECT). For GLOBAL, adds
    the in-memory header length to point past the 4-byte header.

Returns:
    script_base_lo/hi set to the base address for the current task’s script

Global Inputs:
    task_cur_idx
    task_type_tbl[*]
    task_script_idx_tbl[*]
    current_room
    script_ptr_lo/hi_tbl[*]
    room_ptr_lo/hi_tbl[*]
    object_ptr_lo/hi_tbl[*]

Description:
    - Read task_type_tbl[task_cur_idx] to choose the source:
        • GLOBAL  → script_ptr_*_tbl[ task_script_idx_tbl[task_cur_idx] ],
                    then add MEM_HDR_LEN to base_lo/hi.
        • ROOM    → room_ptr_*_tbl[current_room].
        • OBJECT  → object_ptr_*_tbl[ task_script_idx_tbl[task_cur_idx] ].
    - For unknown types, control transfers to deactivation logic upstream.
================================================================================
*/
* = $5E20
set_script_base_from_type:
        // ------------------------------------------------------------
        // Read task_type for the current task and select the correct resource table
        // ------------------------------------------------------------
        ldy     task_cur_idx                    // Y := current script task
        lda     task_type_tbl,y                 // A := script type for task Y
        cmp     #SCRIPT_TYPE_GLOBAL
        bne     dispatch_room_type              // not global → check room type

        //-----------------------------
        // Case Global script
        //-----------------------------
        ldx     task_script_idx_tbl,y           // X := global script resource index
        lda     script_ptr_lo_tbl,x             // base_lo
        clc
        adc     #MEM_HDR_LEN                    // skip 4-byte header
        sta     script_base_lo
        lda     script_ptr_hi_tbl,x             // base_hi
        adc     #$00                            // add carry if any
        sta     script_base_hi
        rts

dispatch_room_type:
        cmp     #SCRIPT_TYPE_ROOM
        bne     dispatch_object_type            // not room → check object/unknown

        //-----------------------------
        // Case Room script
        //-----------------------------
        ldx     current_room                    // X := current room index
        lda     room_ptr_lo_tbl,x               // base_lo
        sta     script_base_lo
        lda     room_ptr_hi_tbl,x               // base_hi
        sta     script_base_hi
        rts

dispatch_object_type:
        cmp     #SCRIPT_TYPE_OBJECT
        bne     deactivate_script_by_resource   // unknown → deactivate

        //-----------------------------
        // Case Object script
        //-----------------------------
        ldx     task_script_idx_tbl,y           // X := object resource index
        lda     object_ptr_lo_tbl,x             // base_lo
        sta     script_base_lo
        lda     object_ptr_hi_tbl,x             // base_hi
        sta     script_base_hi
        rts
/*
================================================================================
  deactivate_script_by_resource
================================================================================
Summary:
	Scan tasks from highest to lowest and deactivate the first active task whose
	script index matches A. On match: decrement the script’s memory refcount,
	clear the task’s script index, and set its state to INACTIVE. Task $00 is
	not probed.

Arguments:
	A  Target script index to match

Global Inputs:
	task_script_idx_tbl[*]  Per-task bound script index
	task_state_tbl[*]       Per-task execution state
	script_liveness_tbl[*]     Per-script memory refcount (read before decrement)

Global Outputs:
	task_script_idx_tbl[Y]  Cleared to 0 on deactivation
	task_state_tbl[Y]       Set to TASK_STATE_INACTIVE on deactivation
	script_liveness_tbl[A]     Decremented on deactivation

Vars/State:
	Uses Y as descending loop index (starts at TASK_MAX_INDEX, stops before $00)
	Uses X as a temporary to read state and script index for matched task

Description:
	- Initialize Y to the highest task index.
	- Compare A against task_script_idx_tbl[Y].
	- If equal and task_state_tbl[Y] != INACTIVE:
		• Decrement script_liveness_tbl[ task_script_idx_tbl[Y] ].
		• Clear task_script_idx_tbl[Y].
		• Write INACTIVE to task_state_tbl[Y], then return.
	- Otherwise decrement Y and continue until Y becomes $00, then return.
================================================================================
*/
* = $5E67
deactivate_script_by_resource:
        ldy     #TASK_MAX_INDEX                 // Y := highest task index

scan_tasks_for_resource:
		// Script idx matches?
        cmp     task_script_idx_tbl,y           // A == task[Y].script_idx ?
        bne     step_prev_task_or_exit          // no → check next lower task

		// Task active?
        ldx     task_state_tbl,y                // X := task[Y].state
        cpx     #TASK_STATE_INACTIVE            // inactive already?
        beq     step_prev_task_or_exit          // yes → nothing to do for this task

		// ------------------------------------------------------------
		// Deactivate task, decrement script refcount
        // ------------------------------------------------------------
        ldx     task_script_idx_tbl,y           // X := script index of matching active task
        dec     script_liveness_tbl,x              // refcount[script]--

        lda     #$00                            // A := 0
        sta     task_script_idx_tbl,y           // task[Y].script_idx := 0 (unbind)

        lda     #TASK_STATE_INACTIVE            // A := INACTIVE
        sta     task_state_tbl,y                // task[Y].state := INACTIVE
        rts                                     // done after first match

step_prev_task_or_exit:
        dey                                     // Y := Y-1 (scan next task down)
        bne     scan_tasks_for_resource         // Y != 0 → continue scanning
        rts                                     // Y == 0 → finished; no match or only task $00 left
/*
================================================================================
launch_global_script
================================================================================

Summary:
    Start a global script identified by A. If another instance is active, stop
    it. Ensure the resource is resident, bump its refcount, allocate a free
    task, initialize its context, and invoke it once before returning.

Arguments:
    A  					   Global script index to launch

Global Inputs:
    script_ptr_hi_tbl[*]   check residency (nonzero hi ⇒ already loaded)
    task_state_tbl[*]      read during deactivation scan
    task_script_idx_tbl[*] read during deactivation scan
    task_type_tbl[*]       read during deactivation scan

Global Outputs:
    script_idx             latched copy of requested script index
    script_liveness_tbl[*]    increment reference count for this script
    task_script_idx_tbl[*] bind task to script index
    task_pc_ofs_lo_tbl[*]  reset task relative PC (lo = 0)
    task_pc_ofs_hi_tbl[*]  reset task relative PC (hi = 0)
    task_state_tbl[*]      set task state to RUNNING
    task_type_tbl[*]       set task type to GLOBAL
    task_cur_idx           temporarily set during invocation

Description:
    - Deactivate any running instance that matches the requested script index.
    - If not resident, load the script resource and refresh addresses if moved.
    - Increment the script’s memory reference count.
    - Acquire a free task entry; on success, bind script index to that task.
    - Zero the task’s relative PC offset and set state/type fields.
    - Invoke the task, then restore the parent context if present.
================================================================================
*/
* = $5E8A
launch_global_script:
        // ------------------------------------------------------------
        // Latch requested script index
        // ------------------------------------------------------------
        sta     script_idx                     // script_idx := A (requested global script)

        // ------------------------------------------------------------
        // Ensure only one instance: stop an already-running copy
        // ------------------------------------------------------------
        jsr     deactivate_script_by_resource   // A=script_idx → deactivate matching running task

        // ------------------------------------------------------------
        // Ensure resource residency (load if not already resident)
        // ------------------------------------------------------------
        ldx     script_idx                      // X := script index
        lda     script_ptr_hi_tbl,x             // read resident base_hi for this script
        bne     script_ready                    // nonzero → already resident

        txa                                      // A := script index (argument for loader)
        jsr     rsrc_cache_script      // load script resource into memory
        jsr     refresh_script_addresses_if_moved // handle relocation effects if loader moved memory

script_ready:
        // ------------------------------------------------------------
        // Bump residency refcount for this script resource
        // ------------------------------------------------------------
        ldx     script_idx                      // X := script index
        inc     script_liveness_tbl,x              // refcount++

        // ------------------------------------------------------------
        // Acquire a free execution task
        // ------------------------------------------------------------
        jsr     find_free_task                  // returns X := free task id or $10 if none

        // ------------------------------------------------------------
        // Bind script to the task context
        // ------------------------------------------------------------
        lda     script_idx
        sta     task_script_idx_tbl,x           // task[X].script_idx := script_idx

        // ------------------------------------------------------------
        // Reset task PC offset so PC := base + 0 on first step
        // ------------------------------------------------------------
        lda     #$00
        sta     task_pc_ofs_hi_tbl,x            // task[X].pc_ofs_hi := 0
        sta     task_pc_ofs_lo_tbl,x            // task[X].pc_ofs_lo := 0

        // ------------------------------------------------------------
        // Mark task running and set its type to GLOBAL
        // ------------------------------------------------------------
        lda     #TASK_STATE_RUNNING
        sta     task_state_tbl,x                // task[X].state := RUNNING

        lda     #SCRIPT_TYPE_GLOBAL
        sta     task_type_tbl,x                 // task[X].type := GLOBAL

        // ------------------------------------------------------------
        // Invoke the task, then resume parent (if any)
        // ------------------------------------------------------------
        jsr     invoke_task_then_resume_parent  // call child task X with save/restore of caller
        rts                                      // return to caller
/*
============================================================================
  find_free_task
============================================================================
Find first inactive task slot

Summary:
    Scans task state table sequentially (X=$01..$0F) to locate a free slot.
    Task $00 is reserved and skipped. Returns index of first inactive task or
    sentinel TASK_TOTAL if none available.

Vars/State:
    Reads task_state_tbl,X to test activity state

Returns:
    .X = index of first inactive task, or $10 if none found
    .A = last state value read (undefined on success)

Description:
    - Starts from task index 1 and loops upward.
    - Compares each entry’s state to TASK_STATE_INACTIVE.
    - Returns immediately on the first match.
    - When all slots up to TASK_TOTAL-1 are active, exits with X=$10.
============================================================================
*/
* = $5EC0
find_free_task:
        ldx     #$01                           // X := first probe index (skip $00)

test_task_inactive:
        lda     task_state_tbl,x               // A := state for task X
        cmp     #TASK_STATE_INACTIVE           // Z=1 if task is inactive
        bne     advance_task_and_loop          // active → keep scanning
        rts                                    // inactive → return with X

advance_task_and_loop:
        inx                                    // X := X+1
        cpx     #TASK_TOTAL                    // end reached?
        bne     test_task_inactive             // no → test next task
        rts                                    // yes → none free; X=TASK_TOTAL
/*
================================================================================
invoke_task_then_resume_parent
================================================================================

Summary:
    Invoke child task X: save caller context, switch to task X, run its script
    until it yields/returns, then restore the parent task if one exists.

Arguments:
    X  		Child task index to invoke

Side effects:
	- If parent exists (id != $FF): task_cur_idx restored to parent and its
	PC rebuilt from saved relative offset.
	- If no parent (id == $FF): task_cur_idx remains the invoked child.

Global Inputs:
    task_cur_idx             current task id before invocation
    task_script_idx_tbl[*]   script resource index per task
    task_pc_lo / task_pc_hi  absolute PC of current task
    script_base_lo / hi      current script base address

Global Outputs:
    task_cur_idx             restored to parent or left as child if no parent
    task_pc_ofs_lo/hi_tbl[*] parent’s saved relative PC offset updated
    script_base_lo / hi      may be cleared on mismatch abort

Description:
    - Push caller’s relative PC = (task_pc - script_base) on stack (lo, hi).
    - Push caller’s script resource index and caller task id.
    - Switch to child task X; rebuild base and PC; run dispatch loop.
    - Pop caller id: if $FF, unwind remaining items and return (no parent).
    - Otherwise verify caller’s script resource index; on mismatch, log and
      clear script_base, drop saved offsets, and return.
    - If matched, restore caller’s saved relative PC, rebuild absolute PC, and
      return with caller as current task.
================================================================================
*/
* = $5ED0
invoke_task_then_resume_parent:
        // ------------------------------------------------------------
        // Save parent task context on stack
		//
        // Push (task_pc - script_base) as lo,hi then push parent task id and
        // its script resource index. This allows resuming after the child runs.
        // ------------------------------------------------------------
        lda     task_pc_lo                     	
        sec                                     
        sbc     script_base_lo                  // A := PC_lo - base_lo
        pha                                     // push rel_offs_lo

        lda     task_pc_hi                     	
        sbc     script_base_hi                  // A := PC_hi - base_hi - borrow
        pha                                     // push rel_offs_hi

        ldy     task_cur_idx                   	// Y := parent task index
        lda     task_script_idx_tbl,y          	// A := parent task's script resource index
        pha                                     // push parent resource index
		
        tya                                     // A := parent task index
        pha                                     // push parent task id

        // ------------------------------------------------------------
        // Switch to child task in X, rebuild its base and PC, and step it.
        // ------------------------------------------------------------
        stx     task_cur_idx                   	// current task := X
        jsr     set_script_base_from_type      	// derive script_base_{lo,hi} from task type
        jsr     set_current_task_pc            	// PC := script_base + task_pc_ofs
        jsr     dispatch_script_ops_loop       	// run until handler yields/returns here

        // ------------------------------------------------------------
        // Restore parent task id. If $FF, no parent existed → unwind stack and exit.
        // ------------------------------------------------------------
        pla                                     // A := parent task id
        cmp     #$FF                            // was there a parent?
        bne     restore_prev_context            // yes → restore it

        pla                                     // drop parent resource index
        pla                                     // drop rel_offs_hi
        pla                                     // drop rel_offs_lo
        rts                                     // done

restore_prev_context:
        // ------------------------------------------------------------
        // Reinstall parent task and verify its script resource is unchanged.
        // Mismatch implies the parent task was invalidated; abort restore.
        // ------------------------------------------------------------
        sta     task_cur_idx                   	// restore parent as current
        tay                                     // Y := parent task index
        pla                                     // A := saved parent resource index
        cmp     task_script_idx_tbl,y          	// still the same resource?
        bne     resource_mismatch_abort        	// no → abort restore

        // ------------------------------------------------------------
        // Restore parent's relative PC offset and rebuild its absolute PC.
        // ------------------------------------------------------------
        pla                                     // A := rel_offs_hi
        sta     task_pc_ofs_hi_tbl,y           	// store parent rel_offs_hi
        pla                                     // A := rel_offs_lo
        sta     task_pc_ofs_lo_tbl,y           	// store parent rel_offs_lo
        jsr     set_script_base_from_type      	// recompute script_base for parent
        jsr     set_current_task_pc            	// parent PC := base + rel_offs
        jmp     exit_ls                         // exit

resource_mismatch_abort:
        // ------------------------------------------------------------
        // Log mismatch (A=rsrc idx, Y=task id), flush saved offsets, and
        // clear script_base to a safe null before exiting.
        // ------------------------------------------------------------
        sta     $6ADE                          	// debug: mismatched rsrc index
        sty     $6ADF                          	// debug: task id

        pla                                     // drop rel_offs_hi
        pla                                     // drop rel_offs_lo

        lda     #$00
        sta     script_base_lo                 	// clear base_lo
        lda     #$00
        sta     script_base_hi                 	// clear base_hi

exit_ls:
        rts
