/*
================================================================================
 ops_primitives.asm
================================================================================

Summary:
	Core script-engine primitives and conditional operations. Implements relative
	PC displacement, task stop/pause controls, basic variable manipulation, and
	all comparison-based conditional branches used to steer script flow.

Global Inputs:
	- game_vars                  Array of game variables used by many ops
	- task_cur_idx               Index of the currently executing task
	- task_script_idx_tbl        Task→script index mapping for control ops
	- task_pc_lo, task_pc_hi     Current script program counter (for displace)
	- Script byte stream         Source of operands for all primitives

Global Outputs:
	- task_pc_lo, task_pc_hi     Updated by relative displacement
	- game_vars[]                Updated by copy/add/sub/inc/dec primitives
	- Scheduler state            Modified by stop/pause/start/deactivate ops
	- Script PC progression      Advanced or skipped by conditional branches

Description:
	- Pointer control:
		• op_displace_pointer (#$18) reads a signed 16-bit offset and adds it to PC.
		• script_skip_offset helper is used by True-branch paths to skip the offset.
	- Task control:
		• op_stop_current_task (#$00,#$A0) halts the current task, optionally
		 deactivating its script if indexed; op_pause_task (#$10) halts and saves
		 relative PC for later resume; op_start_script starts a referenced script;
		 op_stop_script_by_index stops a target script by operand or current index.
	- No-ops:
		• op_do_nothing (#$50,#$D0) and op_do_nothing_2 (#$72,#$F2).
	- Variable primitives:
		• Copy immediate/variable to var (#$1A,#$9A), increment (#$46),
		 decrement (#$C6), add (#$5A,#$DA), subtract (#$3A,#$BA).
	- Conditionals (single-offset if/then form):
		• Zero/nonzero tests (#$28, #$A8).
		• Relational/equality via script_compare_operands:
		 ≥ (#$38,#$B8), ≤ (#$04,#$84), > (#$44,#$C4), < (#$78,#$F8),
		 ≠ (#$08,#$88), == (#$48,#$C8).
		Each conditional either skips the following 16-bit offset (True) or applies
		it to PC (False), thereby selecting the next block to execute cleanly.

Notes:
	- Offsets are stored little-endian (lo,hi) and applied with carry.
	- Branch opcodes are one-way if/then constructs; “else” is formed by pairing
	complementary tests in sequence.
================================================================================

This file gives the script engine its basic “verbs.” It covers three things:
how scripts move forward, how scripts start/stop/pause, and how simple math and
comparisons on game variables work. You can think of it as the VM’s control
panel.

1. Conditionals use a single offset

	* Every “if …” reads a pair of values, compares them, then looks at the very
	  next 16-bit number in the bytecode.
	* If the test is TRUE, it *skips* over those two bytes and keeps going.
	* If the test is FALSE, it *adds* that 16-bit number to the current script
	  position (PC), jumping forward or backward. The number is signed in two’s
	  complement, so $FFxx means “go back a little.”

2. One way to read operands

	* Opcodes share a helper that decides what the next byte means:
	  bit7 of the opcode tells the engine “treat the next byte as an immediate
	  value” or “treat it as an index into the variables table.”
	* Result: all ops get their inputs in a uniform, predictable way.

3. Starting, stopping, and pausing scripts

	* Start: launches another script by index. After launching, it sanity-checks
	  that the new script’s base address is mapped; if not, it aborts cleanly.
	* Stop current: removes the current routine’s return address from the stack so
	  it cannot come back. That halts the task immediately.
	* Stop by index: targets a specific script; index #$00 is shorthand for “this
	  script.” If it refers to the running one, it behaves like “stop current.”
	* Pause: stores where we are *relative* to the script’s base (so relocation is
	  safe), then unwinds one call frame to park execution. Later a resume can
	  continue from that saved offset.

4. Variable primitives

	* Copy: source (immediate or variable) into a destination variable.
	* Inc/Dec: add or subtract one with 8-bit wraparound.
	* Add/Sub: add or subtract any 8-bit value to a variable. Carry is handled so
	  results match 8-bit arithmetic expectations.

5. The displacement helper

	* A tiny routine reads a 16-bit offset and adds it to the script PC with carry.
	  This is the workhorse behind every False branch and every manual “jump.”

================================================================================

+-----------------------------+--------------------+--------------------------------------------+
| Routine                     | Opcodes (hex)      | Purpose                                    |
+-----------------------------+--------------------+--------------------------------------------+
| op_displace_pointer         | $18                | Add signed 16-bit offset to script PC      |
| op_stop_current_task        | $00, $A0           | Stop current task                          |
| op_pause_task               | $10                | Pause task and save relative PC            |
| op_do_nothing               | $50, $D0           | No-op                                      |
| op_copy_operand_to_variable | $1A, $9A           | Copy operand → variable                    |
| op_increment_var            | $46                | Increment variable                         |
| op_decrement_var            | $C6                | Decrement variable                         |
| op_add_value_to_var         | $5A, $DA           | Add operand → variable                     |
| op_subtract_value_from_var  | $3A, $BA           | Subtract operand from variable             |
| op_if_operand_nonzero       | $28                | If operand ≠ 0                             |
| op_if_operand_zero          | $A8                | If operand == 0                            |
| op_if_operand_ge            | $38, $B8           | If op1 ≥ op2                               |
| op_if_operand_le            | $04, $84           | If op1 ≤ op2                               |
| op_if_operand_gt            | $44, $C4           | If op1 > op2                               |
| op_if_operand_lt            | $78, $F8           | If op1 < op2                               |
| op_if_operand_ne            | $08, $88           | If op1 ≠ op2                               |
| op_if_operand_eq            | $48, $C8           | If op1 == op2                              |
| op_stop_script_by_index     | $62, $E2           | Stop script by index                       |
| op_start_script             | $42, $C2           | Start script by index                      |
| op_do_nothing_2             | $72, $F2           | No-op (variant)                            |
+-----------------------------+--------------------+--------------------------------------------+
*/
#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "script_engine.asm"
#import "script_primitives.asm"


/*
================================================================================
Comparison operations
================================================================================
Comparison opcodes perform relational or equality checks between a primary variable
and a secondary operand (variable or immediate). Each result alters script flow
by conditionally advancing or displacing the instruction pointer.

Conceptually:

    if (condition) {
        ...true block...
    }

There is no built-in "else" construct. Code following an if/then block always
executes unless manually bypassed using an inverted condition. To emulate
"else", scripts could chain two complementary conditionals:

    if (condition) {
        ...true block...
    }
    if (not condition) {
        ...false block...
    }

The script interpreter determines how to continue when the condition evaluates
False by applying a relative offset to the current PC. This offset skips over
the “true” block.

A complete conditional sequence includes:

    • condition opcode
    • first operand (variable index)
    • second operand (immediate or variable)
    • 16-bit offset for False branch
    • <code for True branch>

Execution logic:

    - If the condition is True → skip the 16-bit offset and immediately execute
      the “true” code block.
    - If the condition is False → read and apply the offset, moving PC forward
      to the next instruction after the “true” block.

In effect, each conditional opcode defines a one-way branch controlled by
processor flags from the preceding comparison.
================================================================================
*/
.label var_index = $d0
.label operand_value = $d0
.label relative_offset = $d2
.label max_var_index = $d2          // Exclusive upper bound for valid var IDs
.label pointer       = $d0          // ZP base pointer to variable array
.label bitmask       = $d2          // Working mask byte (shares addr with max)
.label result_variable_index = $d2           // Destination game-var index
.const BITOP_MODE_SET      = $00    // Mode byte value: set bits (use OR)
.const BITOP_INVERT_MASK   = $FF    // Inversion mask used for clear path (~mask)
.const MAX_VAR_INDEX = $37

/*
================================================================================
  op_displace_pointer - Opcode #$18
================================================================================
Summary: 
	Read a 16-bit signed relative offset from the script stream and add it
	to the current script PC, updating task_pc_lo and task_pc_hi in place.

Global Inputs
	* task_pc_lo  current script PC low byte
	* task_pc_hi  current script PC high byte

Global Outputs
	* task_pc_lo  updated with PC + offset low byte
	* task_pc_hi  updated with PC + offset high byte and carry

Vars/State
	* relative_offset  temporary 16-bit offset (lo at $D2, hi at $D3)

Description
	* Read two bytes (lo, hi) from the script as a relative offset.
	* Add the offset to the current PC with carry propagation.
================================================================================
*/
* = $60D6
op_displace_pointer:
		// Read 16-bit relative offset
		jsr     script_read_byte               // A := offset.lo
		sta     relative_offset               // save lo

		jsr     script_read_byte               // A := offset.hi
		sta     relative_offset + 1               // save hi

		// Add offset to PC
		lda     task_pc_lo                     
		clc                                    
		adc     relative_offset               // A := PC.lo + ofs.lo
		sta     task_pc_lo                     // write PC.lo

		lda     task_pc_hi                     
		adc     relative_offset + 1               // A := PC.hi + ofs.hi + C
		sta     task_pc_hi                     // write PC.hi
		rts                                    
/*
================================================================================
  op_stop_current_task - Opcodes #$00, #$A0
================================================================================
Summary
	Stop the _current_ task. If the _current_ task index is nonzero, deactivate
	the referenced script, then unconditionally halt _this_ task by discarding its
	return address.

Returns
	* None. Control resumes at the caller’s caller; the current task halts.

Global Inputs
	* task_cur_idx  current task index being executed
	* task_script_idx_tbl  script/resource index lookup by task index

Global Outputs
	* Scheduler state via deactivate_script_by_resource  target script deactivated
	* Stack pointer  incremented by two after discarding the return address
	* Task state  current task terminates and will not resume

Description
	* If X = 0, skip external deactivation.
	* If X ≠ 0, load the script/resource index and deactivate it.
	* Pop the return address to prevent this task from returning normally.
================================================================================
*/
* = $60F2
op_stop_current_task:
		// ------------------------------------------------------------
		// If current task ≠ 0: deactivate that task. 
		// ------------------------------------------------------------
		ldx     task_cur_idx               			// X := current task
		beq     stop_this_task                  	// zero → no external target

		lda     task_script_idx_tbl,x  
		jsr     deactivate_script_by_resource       // deactivate referenced script

stop_this_task:
		// ------------------------------------------------------------
		// Terminate current task: drop return address and exit
		// ------------------------------------------------------------
		pla                                         // discard return.lo
		pla                                         // discard return.hi
		rts                                         // return to caller’s caller		
/*
================================================================================
  op_pause_task - opcode #$10
================================================================================
Summary
	Halt the current task by discarding its return address, then persist
	its relative program counter so it can resume later at the correct position.

Returns
	* None. Control transfers to the caller’s caller; the current task halts.

Global Outputs
	* task_pc_ofs_lo/hi_tbl  updated with saved relative PC for task_cur_idx

Description
	* Pop two bytes from the stack to prevent a normal RTS back into the task.
	* Tail-jump to save_task_relative_offset to record the task’s relative PC.
================================================================================
*/
* = $6100
op_pause_task:
		// Drop return address to halt current task, then save
		pla                                     // discard return.lo
		pla                                     // discard return.hi
		
		// Save the task’s relative offset for later resumption.
		jmp     save_task_relative_offset      // commit relative PC offset and exit		
/*
================================================================================
  op_do_nothing - Opcodes #$50, #$D0
================================================================================
*/
* = $6105
op_do_nothing:
		rts
/*
================================================================================
  op_copy_operand_to_variable - Opcodes #$1A (immediate), #$9A (variable)
================================================================================
Summary
	Copy an operand value into a game variable.
	Supports both immediate and variable source operands depending on opcode bit7.

Arguments
	* Byte0: destination variable index
	* Byte1: operand (immediate value or variable reference)

Returns
	* None. Value written to target variable in game_vars[].

Global Inputs
	* game_vars  array of game variables

Global Outputs
	* game_vars[var_index]  updated with resolved operand value

Description
	* Reads the destination variable index from the script.
	* Loads operand (immediate or indirect) using script_load_operand_bit7.
	* Writes the operand value into the specified game variable slot.
================================================================================
*/
* = $6106
op_copy_operand_to_variable:
		// Read variable index
		jsr     script_read_byte               // A := destination var index
		sta     var_index                      // save index

		//Resolve operand value
		jsr     script_load_operand_bit7       // A := resolved operand value

		ldx     var_index                      // X := var index
		sta     game_vars,x                    // game_vars[X] := value
		rts
/*
================================================================================
  op_increment_var - Opcode #$46
================================================================================
Summary
	Increment a game variable by 1.

Arguments
	* Operand: variable index (resolved by script_load_operand_bit7)

Global Inputs
	* game_vars  array of game variables
	* script_load_operand_bit7  resolves operand to a variable index

Global Outputs
	* game_vars[var_index]  incremented by 1

Description
	* Read operand and resolve it to a variable index.
	* Increment game_vars at that index in place.
================================================================================
*/
* = $6114
op_increment_var:
		// Resolve operand (var index) via script_load_operand_bit7.
		jsr     script_load_operand_bit7        // A := variable index
		tax                                     // X := var index
		// Increment variable by 1
		inc     game_vars,x                     // game_vars[X] := +1
		rts                                     // done		
/*
================================================================================
  op_decrement_var - Opcode #C6
================================================================================
Summary
	Decrement a game variable by 1. The operand selects the target
	variable index; the routine resolves the index and performs an in-place DEC.

Arguments
	* Operand: variable index (resolved by script_load_operand_bit7)

Description
	* Resolve operand to a variable index using script_load_operand_bit7.
	* Decrement game_vars[index] by one in place.
================================================================================
*/
* = $611C
op_decrement_var:
		// Resolve operand (var index) via script_load_operand_bit7.
		jsr     script_load_operand_bit7        // A := variable index
		tax                                     // X := var index
		// Decrement variable by 1
		dec     game_vars,x                     // game_vars[X] := -1
		rts                                     // done
/*
================================================================================
  op_add_value_to_var - Opcodes #5A (immediate), #DA (variable)
================================================================================
Summary
	Add an operand value to a destination game variable. Supports both
	immediate and variable-source operands depending on opcode bit7.

Arguments
	* Byte0: destination variable index
	* Byte1: operand (immediate value or variable reference resolved via bit7)

Global Inputs
	* game_vars  array of game variables

Global Outputs
	* game_vars[dest_index]  updated with (old_value + operand_value)

Vars/State
	* operand_value: temporary byte holding the resolved addend

Description
	* Read destination variable index.
	* Resolve operand using script_load_operand_bit7 to handle immediate vs variable.
	* Add operand to the selected game variable and store the result.
================================================================================
*/
* = $6124
op_add_value_to_var:
		// Read dest var index
		jsr     script_read_byte                // A := dest var index
		tay                                     // Y := index
		
		// Resolve operand value
		jsr     script_load_operand_bit7        // A := resolved operand value
		sta     operand_value                   // save addend

		// Add value to var
		lda     game_vars,y                     // A := current var value
		clc                                     // prepare addition
		adc     operand_value                   // A := var + addend
		sta     game_vars,y                     // write back
		rts                                     // done
/*
================================================================================
  op_subtract_value_from_var - Opcodes #3A (immediate), #BA (variable)
================================================================================
Summary
	Subtract an operand value from a destination game variable. Supports
	immediate and variable-source operands based on opcode bit7.

Arguments
	* Byte0: destination variable index
	* Byte1: operand (immediate value or variable reference resolved via bit7)

Global Inputs
	* game_vars  array of game variables to read and update

Global Outputs
	* game_vars[dest_index]  updated with (old_value − operand_value)

Vars/State
	* operand_value: temporary holding the resolved subtrahend

Description
	* Read destination variable index from the script stream.
	* Resolve operand via script_load_operand_bit7 to get subtrahend.
	* Compute var − operand and store back to the selected slot.
================================================================================
*/
* = $6137
op_subtract_value_from_var:
		// Read dest var index
		jsr     script_read_byte                // A := dest var index
		tay                                     // Y := index

		// Resolve operand value
		jsr     script_load_operand_bit7        // A := resolved operand value
		sta     operand_value                   // save subtrahend

		// Subtract value from var
		lda     game_vars,y                     // A := current var value
		sec                                     // prepare subtraction
		sbc     operand_value                   // A := var - subtrahend
		sta     game_vars,y                     // write back
		rts                                     // done
/*
================================================================================
  op_if_operand_nonzero - Opcode #$28
================================================================================
Summary
	Conditional branch if operand ≠ 0.
	Implements “IF operand ≠ 0 THEN … ELSE …” logic within script execution.

Arguments
	* Operand: immediate or variable reference resolved via bit7

Returns
	* None. Script PC advanced based on condition.

Description
	* Load operand and test if it equals zero.
	* If operand == 0, branch to False path and displace the script pointer.
	* If operand ≠ 0, skip the offset bytes and continue with the True branch.
================================================================================
*/
* = $614A
op_if_operand_nonzero:		
		// Resolve operand value
		jsr     script_load_operand_bit7       // A := operand value
		
		//Compare against zero
		cmp     #$00                           // Z=1 if operand == 0
		bne     is_not_equal_to_zero_then      // true → skip offset

		// Condition false → add displacement to script pointer
		jmp     op_displace_pointer

is_not_equal_to_zero_then:
		// Condition true → skip the displacement bytes and continue
		jsr     script_skip_offset
		rts				
/*
================================================================================
  op_if_operand_zero - Opcode #$A8
================================================================================
Summary
	Conditional branch if operand == 0.
	Implements “IF operand == 0 THEN … ELSE …” logic within script execution.

Arguments
	* Operand: immediate or variable reference resolved via bit7

Returns
	* None. Script PC advanced based on condition.

Description
	* Load operand and test if it equals zero.
	* If operand ≠ 0, branch to False path and displace the script pointer.
	* If operand == 0, skip the offset bytes and continue with the True branch.
================================================================================
*/
* = $6158
op_if_operand_zero:
		// ------------------------------------------------------------
		// Opcode #$A8  test “operand == #$00”
		// If true: skip displacement bytes. If false: apply displacement.
		// ------------------------------------------------------------
		// Resolve operand value
		jsr     script_load_operand_bit7       // A := operand value
		
		//Compare against zero
		cmp     #$00                           // Z=1 if operand == 0
		beq     is_equal_to_zero_then          // true → skip offset

		// Condition false → add displacement to script pointer
		jmp     op_displace_pointer

is_equal_to_zero_then:
		// Condition true → skip the displacement bytes and continue
		jsr     script_skip_offset
		rts		
/*
================================================================================
  op_if_operand_ge - Opcodes #$38, #$B8
================================================================================
Summary: 
	Conditional branch if operand1 ≥ operand2. Uses script_compare_operands
	to set flags, then selects the True or False path.

Returns
	* None. Script PC is advanced based on the branch outcome.

Description
	* Call script_compare_operands to compare two script operands.
	* If C=1 (operand1 ≥ operand2): skip the following 16-bit offset.
	* If C=0 (operand1 < operand2): add the 16-bit offset to the script PC.
================================================================================
*/		
* = $6166
op_if_operand_ge:
		// script_compare_operands sets C when op1 ≥ op2.
		jsr     script_compare_operands         // set flags from op1 vs op2
		bcs     is_ge_then                      // C=1 → op1 ≥ op2

		jmp     op_displace_pointer             // False → add displacement

is_ge_then:
		jmp     script_skip_offset              // True → skip displacement

/*
================================================================================
  op_if_operand_le - Opcodes #$04, #$84
================================================================================
Summary: 
	Conditional branch if operand1 ≤ operand2. Uses script_compare_operands
	to set flags, then selects the True or False path.

Returns
	* None. Script PC is advanced based on the branch outcome.

Description
	* Call script_compare_operands to compare two script operands.
	* If C=0 (operand1 < operand2) or Z=1 (operand1 == operand2),
		skip the 16-bit offset and execute the True branch.
	* Otherwise, add the offset to the script PC to follow the False branch.	
================================================================================
*/		
* = $6171
op_if_operand_le:
		// script_compare_operands:
		//   C=0 → op1 < op2, Z=1 → op1 == op2
		jsr     script_compare_operands         // set flags from op1 vs op2
		bcc     is_le_then                      // C=0 → op1 < op2
		beq     is_le_then                      // Z=1 → op1 == op2

		jmp     op_displace_pointer             // False → add displacement

is_le_then:
		jmp     script_skip_offset              // True → skip displacement

/*
================================================================================
  op_if_operand_gt - Opcodes #$44, #$C4
================================================================================
Summary: 
	Conditional branch if operand1 > operand2.
	Implements the strict “greater than” comparison using results from
	script_compare_operands.

Returns
	* None. Script PC advanced according to the branch result.

Description
	* Calls script_compare_operands to compare operand1 and operand2.
	* If Z=1 (equal), branch is False.
	* If C=1 and Z=0 (operand1 ≥ operand2 but not equal), branch is True.
	* True → skip the next 16-bit offset.
	* False → add the offset to the script PC.
================================================================================
*/
* = $617E
op_if_operand_gt:
		// script_compare_operands:
		//   Z=1 → equal → not greater
		//   C=1 with Z=0 → op1 ≥ op2 and not equal → greater
		jsr     script_compare_operands         // set flags from op1 vs op2
		beq     is_not_gt                       // equal → not greater
		bcs     is_gt_then                      // C=1 and Z=0 → greater

is_not_gt:
		jmp     op_displace_pointer             // False → add displacement

is_gt_then:
		jmp     script_skip_offset              // True → skip displacement

* = $618B
/*
================================================================================
  op_if_operand_lt - Opcodes #$78, #$F8
================================================================================
Summary: 
	Conditional branch if operand1 < operand2.
	Uses comparison flags from script_compare_operands to decide execution flow.

Returns
	* None. Script PC advanced based on branch outcome.

Description
	* Calls script_compare_operands to compare operand1 and operand2.
	* If C=0 (operand1 < operand2), the condition is True.
	* If C=1 (operand1 ≥ operand2), the condition is False.
	* True → skip next 16-bit offset.
	* False → add the offset to the script PC.
================================================================================
*/
* = $618B
op_if_operand_lt:
		// script_compare_operands: C=0 → op1 < op2.
		jsr     script_compare_operands         // set flags from op1 vs op2
		bcc     is_lt_then                      // C=0 → less than

		jmp     op_displace_pointer             // False → add displacement

is_lt_then:
		jmp     script_skip_offset              // True → skip displacement

/*
================================================================================
  op_if_operand_ne - Opcodes #$08, #$88
================================================================================
Summary: 
	Conditional branch if operand1 ≠ operand2.
	Determines inequality using the result from script_compare_operands.

Returns
	* None. Script PC advanced based on comparison outcome.

Description
	* Calls script_compare_operands to compare operand1 and operand2.
	* If Z=0 (not equal), the condition is True → skip next 16-bit offset.
	* If Z=1 (equal), the condition is False → add the offset to the script PC.
================================================================================
*/
* = $6196
op_if_operand_ne:  
		// script_compare_operands: Z=0 → not equal.
		jsr     script_compare_operands         // set flags from op1 vs op2
		bne     is_ne_then                      // Z=0 → not equal → True

		jmp     op_displace_pointer             // False → add displacement

is_ne_then:
		jmp     script_skip_offset              // True → skip displacement
/*
================================================================================
  op_if_operand_eq - Opcodes #$48, #$C8
================================================================================
Summary: 
	Conditional branch if operand1 == operand2. Compares two script
	operands via script_compare_operands, then follows the True path by skipping
	the next 16-bit offset when equal, or the False path by applying that offset.

Returns
	* None. Script PC is advanced according to the comparison outcome.

Description
	* Calls script_compare_operands to compare operand1 and operand2.
	* If Z=1 (equal), skip the following 16-bit offset and continue (True branch).
	* If Z=0 (not equal), add the 16-bit offset to the current PC (False branch).
================================================================================
*/
* = $61A1
op_if_operand_eq:  
		// script_compare_operands: Z=1 → equal.
		jsr     script_compare_operands         // set flags from op1 vs op2
		beq     is_eq_then                      // Z=1 → equal → True

		jmp     op_displace_pointer             // False → add displacement

is_eq_then:
		jmp     script_skip_offset              // True → skip displacement

/*
================================================================================
  op_stop_script_by_index - Opcodes #$62, #$E2
================================================================================
Summary: 
	Stop a specific script by index.
	If the operand is zero, the current task’s script index becomes the target.
	If the operand matches the current script index, the current task stops itself.
	Otherwise, the target script is deactivated externally.

Arguments
	* Operand: script index (immediate or variable reference resolved by bit7)

Global Inputs
	* task_cur_idx  index of the currently running task
	* task_script_idx_tbl  table mapping task indices to script indices

Global Outputs
	* Target script deactivated via deactivate_script_by_resource
	* Or current task stopped via op_stop_current_task

Description
	* Reads the operand as the script index to stop.
	* If operand == 0 → replace it with current task’s script index.
	* If current task index == 0 → deactivate the referenced script.
	* If operand ≠ current task’s script index → stop that script.
	* If operand == current script index → stop current task.
================================================================================
*/
* = $61AC
op_stop_script_by_index:
		jsr     script_load_operand_bit7       // A := script index operand

		// Entry point used by multiple opcodes (arrives with X = #$00)
dispatch_stop_by_index:
		cmp     #$00                           // operand == 0?
		bne     target_index_loaded            // no → A already target

		// Operand was #00 → target := current task’s script index
		ldx     task_cur_idx                   // X := current task
		lda     task_script_idx_tbl,x          // A := script index for task X

target_index_loaded:
		// If current task index == 0, stop the script (no valid slot)
		ldx     task_cur_idx                   // X := current task
		beq     deactivate_target_script                    // X==0 → deactivate target

		// If target ≠ current task’s script index → stop self instead
		cmp     task_script_idx_tbl,x          // match current script index?
		bne     deactivate_target_script       // no match → deactivate target

		// Target is the running script → stop the current task
		jmp     op_stop_current_task

deactivate_target_script:
		jsr     deactivate_script_by_resource  // deactivate script in A
		rts
/*
================================================================================
  op_start_script - Opcodes #$42, #$C2
================================================================================
Summary
	Start a script. Reads the script index operand (bit7 as high-byte flag), 
	calls the global script start routine, then validates that the current script 
	resource base is nonzero. If validation fails, aborts the current script by 
	removing its return address from the stack.

Arguments
	None. Operand is fetched from the script stream by script_load_operand_bit7.

Returns
	On success: returns normally to the caller.
	On failure: pops this routine’s return address and returns to the caller’s caller.

Global Inputs
	current_script_rsrc_base_hi  High byte of current script resource base; used
	to confirm a valid start before returning.

Description
	- Load script index operand; bit7 supplies the high byte.
	- Invoke launch_global_script for the referenced script.
	- If current_script_rsrc_base_hi == #$00, treat as invalid start:
	  remove this routine’s return address from the stack to halt the script.
	- Otherwise return.
================================================================================
*/
* = $61CA
op_start_script:
		// Resolve script index
        jsr     script_load_operand_bit7       
		// Launch the global script
        jsr     launch_global_script            

		//Verify script in memory
        lda     script_base_hi				   
        bne     exit_op_start_script           // ok → return normally

        // Abort current script: pop return address so caller is skipped
        pla                                     // discard return addr low
        pla                                     // discard return addr high
exit_op_start_script:
        rts
/*
================================================================================
  op_do_nothing_2 - Opcodes #$72, #$F2
================================================================================
*/
* = $61D8
op_do_nothing_2:
		rts
/*
================================================================================
  op_apply_bitmask_to_var - Opcodes #$1D, #$3D, #$5D, #$7D, #$9D, #$BD, #$DD, #$FD
================================================================================
Summary
        Configure the variable base pointer and bounds, then tail-call the
        generic bitmask applier to a variable.

Arguments
        (script) op+bit7  → variable_index
        (script) op+bit6  → bitmask
        (script) op+bit5  → mode (#$00=set, otherwise=clear) — caller here
                            intends the set path.

Returns
        None (updates *(pointer+variable_index))

Description
        - pointer := $FEAC (engine variables array).
        - max_var_index := #$37 (exclusive upper bound).
        - Jump to apply_bitmask_to_var to perform the operation.
================================================================================
*/
* = $63AE
op_apply_bitmask_to_var:
        // ------------------------------------------------------------
        // Set up base pointer → $FEAC (engine variables)
        // ------------------------------------------------------------
        lda     #<engine_vars
        sta     pointer
        lda     #>engine_vars
        sta     pointer+1

        // ------------------------------------------------------------
        // Set maximum valid variable index (exclusive) → #$37
        // ------------------------------------------------------------
        lda     #MAX_VAR_INDEX
        sta     max_var_index

        // ------------------------------------------------------------
        // Tail-call shared bitmask routine (set/clear handled there)
        // ------------------------------------------------------------
        jmp     apply_bitmask_to_var
		
/*
================================================================================
  op_test_var_bitmask - Opcodes #$31, #$71, #$B1, #$F1
================================================================================
Summary
        Configure the variable base pointer to the engine variables array
        ($FEAC) and tail-call the generic bitmask tester that ANDs a source
        variable with a bitmask and stores a boolean result (#$00/#$01).

Arguments
        (forwarded to callee test_var_bitmask)
        (script) byte0  → result_variable_index
        (script) bit7   → source_variable_index
        (script) bit6   → bitmask

Returns
        None (callee writes boolean to game_vars[result_variable_index])

Description
        - pointer := $FEAC (engine variables).
        - Jump to test_var_bitmask to perform the AND+boolean store.
================================================================================
*/
* = $63BD		
op_test_var_bitmask:
        // ------------------------------------------------------------
        // Set up base pointer → $FEAC (engine variables)
        // ------------------------------------------------------------
        lda     #<engine_vars
        sta     pointer
        lda     #>engine_vars
        sta     pointer + 1

        // ------------------------------------------------------------
        // Tail-call shared tester (AND with mask → boolean result)
        // ------------------------------------------------------------
        jmp     test_var_bitmask
/*
================================================================================
  apply_bitmask_to_var
================================================================================

Summary
        Modify one byte-sized variable selected by index using a bitmask. The
        mode selects OR-set or AND-clear. If the supplied variable index is
        out of range (≥ max_var_index), skip the next two operand bytes and
        return without changes.

Arguments
        (script) op+bit7  → variable_index
        (script) op+bit6  → bitmask
        (script) op+bit5  → mode (#$00=set, otherwise=clear)

Vars/State
        max_var_index   maximum valid variable index (exclusive upper bound)
        pointer         base pointer to the variable array
        bitmask         working byte for mask

Returns
        None (updates *(pointer+variable_index))

Description
        - Load variable_index. If variable_index ≥ max_var_index, branch to
          script_skip_offset to discard the next two operand bytes, then return.
        - Y := variable_index for indexed indirect access via (pointer),Y.
        - Load bitmask from operand (bit6).
        - Load mode from operand (bit5).
        - If mode == #$00 → set bits:   new := (old OR bitmask).
        - Else            → clear bits: new := (old AND (bitmask XOR #$FF)).
        - Store new back to (pointer),Y.
================================================================================
*/
* = $63C8
apply_bitmask_to_var:
        // Resolve variable index
        jsr     script_load_operand_bit7      

        // Bounds check against max_var_index; if invalid, skip two bytes
        cmp     max_var_index                 // A >= max? (carry set on ≥)
        bcc     index_ok          			  // valid → proceed
        jmp     script_skip_offset            // invalid → skip bitmask+mode

index_ok:
        // Use Y as the index into the variable array
        tay                                   

        // Resolve bitmask from operand; store as working mask
        jsr     script_load_operand_bit6      
        sta     bitmask

        // Resolve mode: #BITOP_MODE_SET=set, otherwise=clear
        jsr     script_load_operand_bit5      
        cmp     #BITOP_MODE_SET
        bne     set_bits_path                 // ≠0 → clear path selected

        // Clear path: invert mask then AND with current value
        lda     bitmask
        eor     #$ff
        and     (pointer),y                   // new := old AND ~mask
        jmp     commit_value

set_bits_path:
        // Set path: OR mask with current value
        lda     (pointer),y                   // old
        ora     bitmask                       // new := old OR mask

commit_value:
        // Commit the updated value
        sta     (pointer),y
        rts
/*
================================================================================
  test_var_bitmask
================================================================================
Summary
        Compute ( (pointer[source_index]) AND bitmask ) and write a boolean
        result into a destination game variable: #$00 if zero, else #$01.

Arguments
        (script) byte0  → result_variable_index  (destination game-var index)
        (script) bit7   → source_variable_index  (lo from script, hi from bit7)
        (script) bit6   → bitmask                (AND operand)

Returns
        None (writes #$00 or #$01 into game_vars[result_variable_index])

Description
        - Reads destination variable index.
        - Reads source variable index into Y.
        - Reads bitmask, ANDs it with *(pointer+Y).
        - Compares result to #$00 and selects #$00 or #$01 accordingly.
        - Stores the boolean into game_vars at the destination index.
================================================================================
*/
* = $63EF
test_var_bitmask:
        // ----------------------------
        // Resolve result variable index
        // ----------------------------
        jsr     script_read_byte              
        sta     result_variable_index         

        // ----------------------------
        // Resolve source variable index into Y
        // ----------------------------
        jsr     script_load_operand_bit7      
        tay                                   

        // ----------------------------
        // Resolve bitmask and AND with source variable
        // ----------------------------
        jsr     script_load_operand_bit6      // A := bitmask
        and     (pointer),y                   // A := *(pointer+Y) AND bitmask

        // ----------------------------
        // Convert to boolean
        // ----------------------------
        ldy     result_variable_index         // Y := dest var index
        cmp     #FALSE
        beq     commit_boolean_result         // yes → A already #$00
        lda     #TRUE                         // no  → A := #TRUE

commit_boolean_result:
        // ----------------------------
        // Store boolean into destination variable
        // ----------------------------
        sta     game_vars,y
        rts


/*
procedure op_displace_pointer()
    // Read signed 16-bit offset from script
    offset = script_read_word()        // little-endian, two’s complement

    // Add offset to current script PC
    new_pc = task_pc + offset
    task_pc = new_pc


procedure op_stop_current_task()
    current_task = task_cur_idx

    // If this task has a nonzero index, deactivate its referenced script
    if current_task != 0 then
        script_id = task_script_idx_tbl[current_task]
        deactivate_script_by_resource(script_id)
    end if

    // Drop this task’s return address so it cannot resume
    discard_return_address_for_current_call()
    // Control returns to the caller’s caller; task is effectively stopped


procedure op_pause_task()
    // Drop this call frame so execution does not return into this opcode
    discard_return_address_for_current_call()

    // Save a PC-relative offset for this task so it can be resumed later
    save_task_relative_offset()



procedure op_do_nothing()
    // No-op
    return



procedure op_copy_operand_to_variable()
    // Destination variable index
    dest_index = script_read_byte()

    // Operand value (immediate or variable, depending on opcode bit7)
    value = script_load_operand_bit7()

    game_vars[dest_index] = value



procedure op_increment_var()
    // Operand encodes which variable to increment
    index = script_load_operand_bit7()

    game_vars[index] = (game_vars[index] + 1) mod 256



procedure op_decrement_var()
    // Operand encodes which variable to decrement
    index = script_load_operand_bit7()

    game_vars[index] = (game_vars[index] - 1) mod 256



procedure op_add_value_to_var()
    dest_index = script_read_byte()
    value = script_load_operand_bit7()

    game_vars[dest_index] = (game_vars[dest_index] + value) mod 256



procedure op_subtract_value_from_var()
    dest_index = script_read_byte()
    value = script_load_operand_bit7()

    game_vars[dest_index] = (game_vars[dest_index] - value) mod 256



procedure op_if_operand_nonzero()
    value = script_load_operand_bit7()

    if value != 0 then
        // TRUE: skip over the following 16-bit offset
        script_skip_offset()
    else
        // FALSE: read and apply the offset to PC
        op_displace_pointer()
    end if



procedure op_if_operand_zero()
    value = script_load_operand_bit7()

    if value == 0 then
        // TRUE
        script_skip_offset()
    else
        // FALSE
        op_displace_pointer()
    end if



procedure op_if_operand_ge()
    // Compare two operands according to VM rules
    // script_compare_operands sets flags as: op1 vs op2
    script_compare_operands()

    // Condition: op1 ≥ op2
    if (op1_is_greater_or_equal_to_op2()) then
        script_skip_offset()
    else
        op_displace_pointer()
    end if



procedure op_if_operand_le()
    script_compare_operands()

    // Condition: op1 ≤ op2 (op1 < op2 OR op1 == op2)
    if (op1_is_less_than_op2() or op1_equals_op2()) then
        script_skip_offset()
    else
        op_displace_pointer()
    end if



procedure op_if_operand_gt()
    script_compare_operands()

    // Condition: op1 > op2 (strictly greater)
    if (op1_is_greater_than_op2()) then
        script_skip_offset()
    else
        op_displace_pointer()
    end if



procedure op_if_operand_lt()
    script_compare_operands()

    // Condition: op1 < op2
    if (op1_is_less_than_op2()) then
        script_skip_offset()
    else
        op_displace_pointer()
    end if



procedure op_if_operand_ne()
    script_compare_operands()

    // Condition: op1 ≠ op2
    if (op1_not_equal_to_op2()) then
        script_skip_offset()
    else
        op_displace_pointer()
    end if



procedure op_if_operand_eq()
    script_compare_operands()

    // Condition: op1 == op2
    if (op1_equals_op2()) then
        script_skip_offset()
    else
        op_displace_pointer()
    end if



procedure op_stop_script_by_index()
    // Operand encodes the target script index or 0 = “current script”
    script_index = script_load_operand_bit7()

    dispatch_stop_by_index(script_index)



procedure dispatch_stop_by_index(script_index)
    if script_index == 0 then
        // 0 means “use the current task’s script”
        current_task = task_cur_idx
        script_index = task_script_idx_tbl[current_task]
    end if

    current_task = task_cur_idx

    // If there is no valid current task slot, just deactivate target script
    if current_task == 0 then
        deactivate_script_by_resource(script_index)
        return
    end if

    current_script_id = task_script_idx_tbl[current_task]

    if script_index == current_script_id then
        // Target is the script currently running in this task
        op_stop_current_task()
    else
        // Target is some other script
        deactivate_script_by_resource(script_index)
    end if



procedure op_start_script()
    // Decode script id from operand (opcode bit7 may supply high part)
    script_id = script_load_operand_bit7()

    // Ask engine to start that script
    launch_global_script(script_id)

    // Validate that the current script resource is mapped
    if script_base_hi == 0 then
        // Treat as failure: drop this frame so script aborts cleanly
        discard_return_address_for_current_call()
    end if

    // Otherwise return normally



procedure op_do_nothing_2()
    // No-op
    return



procedure op_apply_bitmask_to_var()
    // Configure which variable array we operate on (engine_vars)
    pointer = &engine_vars[0]

    // Define the maximum valid variable index (exclusive)
    max_var_index = MAX_VAR_INDEX   // e.g. 0x37

    // Delegate to generic bitmask helper
    apply_bitmask_to_var()



procedure op_test_var_bitmask()
    // Set base pointer for variable tests to engine_vars
    pointer = &engine_vars[0]

    // Delegate to generic tester
    test_var_bitmask()



procedure apply_bitmask_to_var()
    // 1) Which variable?
    var_index = script_load_operand_bit7()

    // Bounds check: if out of range, skip the next two operands (mask+mode)
    if var_index >= max_var_index then
        script_skip_offset()       // skip bitmask + mode operands
        return
    end if

    // 2) Mask
    bitmask = script_load_operand_bit6()

    // 3) Mode: 0 = “clear path” in the actual code, nonzero = “set path”
    mode = script_load_operand_bit5()

    // Variable array is selected by (pointer); var_index goes in Y in asm
    old_value = pointer[var_index]

    if mode == BITOP_MODE_SET then
        inverted = bitmask XOR BITOP_INVERT_MASK   // ~bitmask
        new_value = old_value AND inverted
    else
        new_value = old_value OR bitmask
    end if

    pointer[var_index] = new_value


procedure test_var_bitmask()
    // 1) Destination game-var index for the boolean result
    dest_index = script_read_byte()

    // 2) Source variable index (relative to pointer base)
    source_index = script_load_operand_bit7()

    // 3) Bitmask
    mask = script_load_operand_bit6()

    // Compute masked value
    value = pointer[source_index] AND mask

    // Convert to boolean
    if value == 0 then
        boolean_result = FALSE
    else
        boolean_result = TRUE
    end if

    // Store into game_vars[dest_index]
    game_vars[dest_index] = boolean_result


*/