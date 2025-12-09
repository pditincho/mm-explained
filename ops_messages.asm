/*
================================================================================
 ops_messages - message-printing opcodes for costumes
================================================================================

Purpose
 Implements four script opcodes that make an actor or the current kid
 speak a text message. Each opcode decides who speaks, locates the
 message bytes in the script, prints it on screen, and updates the
 script pointer.

Key concepts
 - talking_costume: current speaking actor ID used by the UI.
 - task_pc: current script PC (points to inline or offset data).
 - message_ptr: temporary pointer the print routine consumes and advances.
 - prepare_message_for_topbar: prints the message and advances message_ptr.

Inline variants
 1) Set talking_costume (player or operand).
 2) Copy task_pc → message_ptr.
 3) Call prepare_message_for_topbar (prints + advances message_ptr).
 4) Copy message_ptr → task_pc so script resumes after the message.

Offset variants
 1) Set talking_costume (player or operand).
 2) Read a 16-bit offset from script into message_ptr (temp).
 3) Compute message_ptr = task_pc + offset.
 4) Subtract 1 from message_ptr to align with printer’s first read.
 5) Call prepare_message_for_topbar to render and advance message_ptr.
    (task_pc not updated here.)

Why subtract 1
 The print routine increments its pointer before reading the first
 character, so pre-decrementing ensures alignment to the intended start.

================================================================================
+-----------------------------------+--------------------+----------------------------------------------------------+
| Routine                           | Opcodes (hex)      | Purpose                                                  |
+-----------------------------------+--------------------+----------------------------------------------------------+
| op_print_player_message_inline    | 75                 | Current kid speaks inline message at task_pc             |
| op_print_player_message_at_offset | 0D                 | Current kid speaks message at task_pc + offset           |
| op_print_costume_msg_inline       | 14, 94             | Actor (from operand) speaks inline message               |
| op_print_costume_msg_at_offset    | 2E, AE             | Actor (from operand) speaks message at task_pc + offset  |
+-----------------------------------+--------------------+----------------------------------------------------------+
*/
#importonce
#import "globals.inc"
#import "constants.inc"
#import "ui_messages.asm"
#import "script_engine.asm"
#import "script_primitives.asm"

.label  message_ptr              = $15        // ZP pointer for message address (also temp offset)

/*
================================================================================
  op_print_player_message_inline
================================================================================

Summary
	Handles Opcode $75. Sets the speaking costume to the current kid, then falls
	through to the inline print routine to render a message that begins at the
	current task PC and advance past it.

Global Inputs
	current_kid_idx            Active player character id
	task_pc_lo / task_pc_hi    Current script execution pointer

Global Outputs
	talking_costume            Set to current_kid_idx

Description
	* Copy current_kid_idx into talking_costume to select the speaker.
	* Fall through to the inline message printer, which:
		• Uses task_pc as the start of the inline message,
		• Renders the message,
		• Updates task_pc to point to the first byte after the message.
================================================================================
*/
* = $6471
op_print_player_message_inline:
		// Set talking_costume = current_kid_idx
		lda     current_kid_idx               
		sta     talking_costume               
		
		// Fall through to print_costume_msg_inline
/*
================================================================================
 print_costume_msg_inline
================================================================================
Summary
	Prints an inline message for the current costume using the script task PC as
	the message start, then advances the task PC to the first byte after the
	message payload.

Global Inputs
	task_pc_lo / task_pc_hi   Start address of inline message

Global Outputs
	message_ptr               Temporary pointer set to the current message and
								advanced by the print routine
	task_pc_lo / task_pc_hi   Updated to point past the message data

Description
	* Copy task_pc into message_ptr to designate the message start.
	* Call prepare_message_for_topbar, which renders the message and advances
	  message_ptr to the first byte after the payload.
	* Write the advanced message_ptr back into task_pc.
================================================================================
*/
* = $6477
print_costume_msg_inline:
		// ------------------------------------------------------------
		// Copy current task PC into message_ptr
		// ------------------------------------------------------------
		lda     task_pc_lo          
		sta     message_ptr        
		lda     task_pc_hi          
		sta     message_ptr + 1

		// ------------------------------------------------------------
		// Print the message and advance message_ptr accordingly
		// ------------------------------------------------------------
		jsr     prepare_message_for_topbar             

		// ------------------------------------------------------------
		// Update task_pc to point past message data
		// ------------------------------------------------------------
		lda     message_ptr                    
		sta     task_pc_lo          
		lda     message_ptr + 1
		sta     task_pc_hi          
		rts                                     

/*
================================================================================
  op_print_player_message_at_offset
================================================================================

Summary
	Handles Opcode $0D. Sets the speaking costume to the current kid, then
	falls through to the offset-based print routine to render a message at
	task_pc plus a 16-bit offset read from the script.

Global Inputs
	current_kid_idx            Active player character id
	task_pc_lo / task_pc_hi    Current script execution pointer
	Script stream              Provides little-endian 16-bit message offset

Global Outputs
	talking_costume            Set to current_kid_idx

Description
	* Copy current_kid_idx into talking_costume to select the speaker.
	* Execution falls through to the offset-based message printer, which:
		• Reads a 16-bit offset from the script stream,
		• Computes message_ptr := task_pc + offset (with the routine’s alignment),
		• Renders the message and advances its internal pointer as defined.
================================================================================
*/
* = $648F
op_print_player_message_at_offset:
		// Set talking_costume = current_kid_idx
		lda     current_kid_idx                
		sta     talking_costume               
		// Fall through to print_costume_msg_at_offset
/*
================================================================================
  print_costume_msg_at_offset
================================================================================
Summary
	Prints a message for the current costume using a 16-bit offset relative to the
	current task PC. The offset is read from the script stream and added to
	task_pc to locate the message data.

Global Inputs
	task_pc_lo / task_pc_hi   Current script execution pointer
	Script stream              Provides 16-bit offset (little-endian) to message

Global Outputs
	message_ptr                Temporary pointer to message, updated as message
								bytes are consumed by the print routine

Description
	* Reads a 16-bit offset from the script stream.
	* Adds the offset to the current task_pc to form message_ptr.
	* Decrements message_ptr by one to align with the print routine’s internal
	increment before the first character read.
	* Calls prepare_message_for_topbar, which renders the message and advances
	message_ptr beyond it.
	* Returns without modifying task_pc itself.
================================================================================
*/
print_costume_msg_at_offset:
		// ------------------------------------------------------------
		// Read 16-bit offset and compute message pointer
		// ------------------------------------------------------------
		jsr     script_read_byte               
		sta     message_ptr                   
		jsr     script_read_byte               
		sta     message_ptr + 1

		// ------------------------------------------------------------
		// Compute message_ptr := task_pc + offset (16-bit add).
		// ------------------------------------------------------------
		clc                                    
		lda     message_ptr                   
		adc     task_pc_lo                     
		sta     message_ptr                   
		lda     message_ptr + 1
		adc     task_pc_hi                     
		sta     message_ptr + 1

		// ------------------------------------------------------------
		// Subtract 1 from message_ptr (align for print routine’s first read)
		// ------------------------------------------------------------
		lda     message_ptr                   
		bne     dec_message_ptr_lo             // if lo≠0 → only DEC lo
		dec     message_ptr + 1

dec_message_ptr_lo:
		dec     message_ptr                   

		// ------------------------------------------------------------
		// Print message at computed address
		// ------------------------------------------------------------
		jsr     prepare_message_for_topbar     // renders and advances message_ptr
		rts                                     	
/*
================================================================================
  op_print_costume_msg_inline
================================================================================

Summary
	Handles Opcode $14/$94. Loads the speaking costume from the operand
	(low 7 bits plus opcode bit7) into talking_costume, then jumps to
	print_costume_msg_inline to render an inline message that follows
	the current task PC.

Global Inputs
	Script stream   Provides 1-byte operand selecting the costume

Global Outputs
	talking_costume   Updated with selected costume id

Description
	* Decode the costume id via script_load_operand_bit7.
	* Store costume id into talking_costume.
	* Tail-call print_costume_msg_inline which:
		• Uses task_pc as the start of message data,
		• Renders the message,
		• Advances task_pc to the first byte after the message.
================================================================================
*/
* = $64BA
op_print_costume_msg_inline:
		// ------------------------------------------------------------
		// Operand gives speaker; message follows directly in script.
		// ------------------------------------------------------------
		jsr     script_load_operand_bit7        // A := costume index
		sta     talking_costume
		jmp     print_costume_msg_inline        // inline variant
/*
================================================================================
  op_print_costume_msg_at_offset
================================================================================

Summary
	Handles Opcode $2E/$AE. Loads the speaking costume from the operand into
	talking_costume, then jumps to print_costume_msg_at_offset to render a
	message located at task_pc plus a 16-bit offset read from the script.

Global Inputs
	Script stream   Provides 1-byte costume operand and a 16-bit offset

Global Outputs
	talking_costume   Updated with selected costume id

Description
	* Decode the costume id via script_load_operand_bit7.
	* Store costume id into talking_costume.
	* Tail-call print_costume_msg_at_offset which:
		• Reads a little-endian 16-bit offset,
		• Computes message_ptr := task_pc + offset, then subtracts 1 for its reader,
		• Renders the message,
		• Leaves task_pc unchanged (pointer management done by the callee as defined).
================================================================================
*/
* = $64C3
op_print_costume_msg_at_offset:
		// ------------------------------------------------------------
		// Operand gives speaker; message at PC + 16-bit offset.
		// ------------------------------------------------------------
		jsr     script_load_operand_bit7        // A := costume index
		sta     talking_costume
		jmp     print_costume_msg_at_offset  	// use offset variant


/*
procedure op_print_player_message_inline()
    // Set the speaker to the current kid
    talking_costume = current_kid_idx

    // Fall through conceptually to “inline” message printer
    print_costume_msg_inline()



procedure print_costume_msg_inline()
    // Use current script PC as the start of the message
    message_ptr = task_pc

    // Print the message and advance message_ptr past the payload
    prepare_message_for_topbar(message_ptr)

    // Resume script execution after the message data
    task_pc = message_ptr



procedure op_print_player_message_at_offset()
    // Set the speaker to the current kid
    talking_costume = current_kid_idx

    // Fall through conceptually to “offset” message printer
    print_costume_msg_at_offset()



procedure print_costume_msg_at_offset()
    // Read 16-bit offset (little-endian) from script stream
    offset_lo = script_read_byte()
    offset_hi = script_read_byte()

    // Form a 16-bit offset value
    offset = make_16bit(offset_lo, offset_hi)

    // Compute message pointer: task_pc + offset
    message_ptr = task_pc + offset

    // Pre-decrement so the printer’s initial increment lands on the first char
    message_ptr = message_ptr - 1

    // Print the message starting at message_ptr; printer advances message_ptr
    prepare_message_for_topbar(message_ptr)

    // Note: task_pc is not changed here; caller controls script flow



procedure op_print_costume_msg_inline()
    // Operand selects the speaking costume
    costume_id = script_load_operand_bit7()
    talking_costume = costume_id

    // Inline text follows at task_pc
    print_costume_msg_inline()



procedure op_print_costume_msg_at_offset()
    // Operand selects the speaking costume
    costume_id = script_load_operand_bit7()
    talking_costume = costume_id

    // Message is at task_pc + 16-bit offset in the script
    print_costume_msg_at_offset()


*/

