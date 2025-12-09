/*
  ops_object.asm
===============================================================================

This file gives scripts tools to inspect and change **objects**. It covers
attribute checks, flag set/clear, comparing against the active indirect object,
picking items up, changing owners, removing from inventory, and renaming.
	
All conditionals use the same VM rule:
	True  → skip the next 16-bit offset
	False → apply that signed offset to the script PC.

Attribute checks
	* Small opcode stubs load a fixed mask for bit7/bit6/bit5, then jump into shared
	  helpers. The helpers read the object’s attribute byte, AND with the mask, and
	  choose the True/False path by calling the VM’s skip or displace routines.
	* Result: “if bit set/clear” without bespoke branching code in each opcode.

Attribute writes
	* Separate helpers set or clear bits. The opcode stubs just load the right mask
	  and tail-call the helper, so “set bit7/6/5” or “clear bit7/6/5” share one
	  implementation.

Active-IO comparisons
	* Two “equals” opcodes and two “not-equals” opcodes compare a script-provided
	  indirect-object index to the currently active IO.
	* Modes: compare low byte only, or require hi==0 then compare low.
	* These use one subroutine that reads one script byte and returns True/False;
	  then the standard True→skip / False→displace rule applies.

Pickup flow
	* op_pickup_object resolves the room object (00 uses the active direct object),
	  allocates a heap block of that size, re-resolves and copies the object data,
	  registers the new block in the next inventory slot, sets flags “in inventory”
	  and “removed from room,” assigns ownership to the current kid, centers the
	  camera, and refreshes UI. Constants and zero-page labels at the top document
	  masks, offsets, and pointers used.

Owner get/set and removal
	* Setting the owner uses two operands: item index (00 = active object) and
	  owner. Owner==00 means “remove”: it sets a removal flag and substitutes the
	  sentinel 0x0D before committing the low-nibble owner field in the attributes
	  byte. UI refresh and relocation maintenance run afterward.
	* A separate routine can remove an item by resolving its inventory resource,
	  freeing its heap block, and clearing the slot.

Renaming objects
	* An opcode reads the object index (00 = active object), then the opcode’s bit7
	  chooses inventory vs room name storage.
	* It resolves the destination buffer and copies a zero-terminated string from
	  the script, then requests an inventory refresh.
	* No bounds checks, so scripts must provide a proper 00-terminated name.
===============================================================================

+----------------------+---------------------------+----------------------------------------------+
| Opcode(s)            | Routine                   | Function                                     |
+----------------------+---------------------------+----------------------------------------------+
| AF, EF               | op_if_obj_attr_bit7_set   | If attr bit7 is set → skip; else displace    |
| 2F, 6F               | op_if_obj_attr_bit7_clear | If attr bit7 is clear → skip; else displace  |
| 9F, DF               | op_if_obj_attr_bit6_set   | If attr bit6 is set → skip; else displace    |
| 1F, 5F               | op_if_obj_attr_bit6_clear | If attr bit6 is clear → skip; else displace  |
| 7F, BF               | op_if_obj_attr_bit5_set   | If attr bit5 is set → skip; else displace    |
| 3F, FF               | op_if_obj_attr_bit5_clear | If attr bit5 is clear → skip; else displace  |
|                      |                           |                                              |
| 97, D7               | op_set_obj_attr_bit7      | Set attr bit7                                |
| 17, 57               | op_clear_obj_attr_bit7    | Clear attr bit7                              |
| B7, F7               | op_set_obj_attr_bit6      | Set attr bit6                                |
| 37, 77               | op_clear_obj_attr_bit6    | Clear attr bit6                              |
| 8F, CF               | op_set_obj_attr_bit5      | Set attr bit5                                |
| 0F, 4F               | op_clear_obj_attr_bit5    | Clear attr bit5                              |
|                      |                           |                                              |
| A4                   | op_if_active_io_eq_lo     | If active IO == script (lo) → skip else disp |
| 24                   | op_if_active_io_eq_16     | If active IO == script (hi==0 & lo)          |
| E4                   | op_if_active_io_ne_lo     | If active IO != script (lo) → skip else disp |
| 64                   | op_if_active_io_ne_16     | If active IO != script (hi==0 & lo)          |
|                      |                           |                                              |
| 90                   | op_pickup_object          | Take room object into inventory              |
| 73, F3               | op_get_object_owner       | Read owner nibble → game var                 |
| 29, 69, A9, E9       | op_set_object_owner       | Write owner nibble; 00 ⇒ schedule removal   |
| 54, D4               | op_set_object_name        | Set object name (inv vs room by opcode bit7) |
+----------------------+---------------------------+----------------------------------------------+
*/

#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "script_engine.asm"
#import "script_primitives.asm"
#import "ops_primitives.asm"
.const IO_CMP_MODE_LO_ONLY     = $01    // Compare only low byte against script byte
.const IO_CMP_MODE_HI0_AND_LO  = $00    // Require hi == $00, then compare low byte

.const OPCODE_PICKUP_OBJECT    = $90        // Opcode ID for “pick up object” command
.const MASK_OBJ_FLAGS_SET      = $A0        // Bits 7+5: in inventory + removed from room
.const MASK_OBJ_CLEAR_OWNER   = $F0        // Mask for clearing owner in object attributes
.const CAMERA_RESET_POSITION   = $C8        // Default camera position after pickup
.const OBJ_TYPE_OBJECT         = $01        // Resource type for object block
.const OFFSET_TYPE             = $02        // Block offset for resource type field
.const OFFSET_INV_INDEX        = $03        // Block offset for inventory index
.const IDX_NONE                = $00        // Operand value → use active object
.const OBJECT_NOT_FOUND        = $FF        // Return value from get_obj_resource_in_room

.const OWNER_NONE                 = $00    // No owner / clear inventory

.label obj_bitmask = $d0
.label room_obj_index          = $15    // Object index argument for lookup; reused as low byte of returned pointer
.label room_obj_state          = $16    // Object state selector for lookup; reused as high byte of returned pointer

.label room_obj_ptr_lo         = $15    // Alias: low byte of object pointer returned by get_obj_resource_in_room
.label room_obj_ptr_hi         = $16    // Alias: high byte of object pointer returned by get_obj_resource_in_room

.label obj_index_backup        = $97    // Saved copy of object index for post-allocation re-lookup
.label obj_size_lo             = $9C    // Object size (low byte) read from resource header
.label obj_size_hi             = $9D    // Object size (high byte) read from resource header
.label obj_src_ptr 		       = $98    // Source pointer to object in room resource 
.label obj_src_ptr_lo          = $98    // Source pointer to object in room resource (low byte)
.label obj_src_ptr_hi          = $99    // Source pointer to object in room resource (high byte)
.label heap_block_ptr          = $9A    // Destination pointer to allocated heap block
.label heap_block_ptr_lo       = $9A    // Destination pointer to allocated heap block (low byte)
.label heap_block_ptr_hi       = $9B    // Destination pointer to allocated heap block (high byte)

.label variable_index = $d0        // Target game var index

.label owner = $d0
.label item  = $d1
.label object_name_pointer = $15     // Destination pointer for object name
.label resource_ptr = $15        // Temporary pointer to resource

/*
================================================================================
  script_is_object_attr_bit_clear (helper, not an opcode handler)
================================================================================
Summary
	Check whether a specified object attribute bit is clear. The bitmask is provided
	in A on entry. The routine reads the target object’s attribute byte, applies the
	bitmask, and branches to the appropriate script control routine based on the
	result.

Arguments
	A  Bitmask specifying which attribute bit(s) to test.

Returns
	No direct return value. Control flow jumps to another opcode handler:
		- If bit is clear (AND == 0): jumps to op_displace_pointer.
		- If bit is set  (AND != 0): jumps to script_skip_offset.

Global Inputs
	Implicit object context as defined by script_read_object_attributes.

Description
	- Save A as the current bitmask.
	- Read object’s attribute byte from the active script context.
	- AND the attribute with the bitmask.
	- If zero (bit clear), treat condition as false and jump to op_displace_pointer.
	- If nonzero (bit set), skip the offset and execute the “condition true” code.
================================================================================
*/
* = $61D9
script_is_object_attr_bit_clear:
		// Test bitmask of object attributes
        sta     obj_bitmask                        // save bitmask from A
        jsr     script_read_object_attributes  // A := object attributes
        and     obj_bitmask                        // test selected bit(s)
        bne     bit_is_set_then                // nonzero → bit set

        jmp     op_displace_pointer            // bit clear → take False path

bit_is_set_then:
        jmp     script_skip_offset             // bit set → skip offset, run True
/*
================================================================================
  script_is_object_attr_bit_set (helper, not an opcode handler)
================================================================================
Summary
	Check whether a specified object attribute bit is set. The bitmask is provided
	in A on entry. The routine reads the target object’s attribute byte, applies the
	bitmask, and branches to the appropriate script control routine based on the
	result.

Arguments
	A  Bitmask specifying which attribute bit(s) to test.

Returns
	No direct return value. Control flow jumps to another opcode handler:
		- If bit is clear (AND == 0): jumps to script_skip_offset.
		- If bit is set  (AND != 0): jumps to op_displace_pointer.

Global Inputs
	Implicit object context as defined by script_read_object_attributes.

Description
	- Save A as the current bitmask.
	- Read object’s attribute byte from the active script context.
	- AND the attribute with the bitmask.
	- If nonzero (bit set), treat condition as false and jump to op_displace_pointer.
	- If zero (bit clear), skip the offset and execute the “condition true” code.
================================================================================
*/
* = $61E8
script_is_object_attr_bit_set:
		// Test bitmask of object attributes
        sta     obj_bitmask                        // save bitmask from A
        jsr     script_read_object_attributes  // A := object attributes
        and     obj_bitmask                        // test selected bit(s)
        beq     bit_is_clear_then              // zero → bit clear

        jmp     op_displace_pointer            // bit set → take False path

bit_is_clear_then:
        jmp     script_skip_offset             // bit clear → skip offset, run True
/*
================================================================================
  op_if_obj_attr_bit7/6/5_set/clear
================================================================================
Summary
	Dispatch entry points for object-attribute bit tests. Each opcode-specific entry
	loads a fixed bitmask into A and uses BNE as an unconditional branch to either
	script_is_object_attr_bit_set or script_is_object_attr_bit_clear.

Returns
	Falls through into the target test routine. No direct return value here.

Dispatch (opcodes)
	- #$AF/#$EF: A := #$80 → branch to script_is_object_attr_bit_set.
	- #$2F/#$6F: A := #$80 → branch to script_is_object_attr_bit_clear.
	- #$9F/#$DF: A := #$40 → branch to script_is_object_attr_bit_set.
	- #$1F/#$5F: A := #$40 → branch to script_is_object_attr_bit_clear.
	- #$7F/#$BF: A := #$20 → branch to script_is_object_attr_bit_set.
	- #$3F/#$FF: A := #$20 → branch to script_is_object_attr_bit_clear.

Notes
	- BNE is unconditional here because A is loaded with a nonzero constant, so Z=0.
	- The target routines read object attributes and handle control flow accordingly.
================================================================================
*/
* = $61f7
op_if_obj_attr_bit7_set:
        // Opcode #$AF, #$EF → test bit 7 is SET
        lda     #MASK_BIT7
        bne     script_is_object_attr_bit_set
		
* = $61fb
op_if_obj_attr_bit7_clear:
        // Opcode #$2F, #$6F → test bit 7 is CLEAR
        lda     #MASK_BIT7
        bne     script_is_object_attr_bit_clear
		
* = $61ff
op_if_obj_attr_bit6_set:
        // Opcode #$9F, #$DF → test bit 6 is SET
        lda     #MASK_BIT6
        bne     script_is_object_attr_bit_set
		
* = $6203
op_if_obj_attr_bit6_clear:
        // Opcode #$1F, #$5F → test bit 6 is CLEAR
        lda     #MASK_BIT6
        bne     script_is_object_attr_bit_clear
		
* = $6207
op_if_obj_attr_bit5_set:
        // Opcode #$7F, #$BF → test bit 5 is SET
        lda     #MASK_BIT5
        bne     script_is_object_attr_bit_set
		
* = $620b
op_if_obj_attr_bit5_clear:
        // Opcode #$3F, #$FF → test bit 5 is CLEAR
        lda     #MASK_BIT5
        bne     script_is_object_attr_bit_clear

        // ------------------------------------------------------------
        // Clear an object attribute's bit(s) using AND with mask in A
        // A := clear bitmask (bits = 0 → cleared, bits = 1 → preserved)
        // ------------------------------------------------------------
/*
================================================================================
  script_clear_object_attr_bit
================================================================================
Summary
	Clear one bit in an object's attribute byte. 
	The caller provides a bitmask in A.

Arguments
	A  Bitmask used to clear attribute bits (0 = clear, 1 = keep).

Returns
	No direct return. Control passes to script_set_object_attributes, which writes
	the updated attributes.

Global Inputs
	Current object context as used by script_read_object_attributes.

Global Outputs
	Updates the object’s attributes field via script_set_object_attributes.

Description
	- Save bitmask from A.
	- Read the current object’s attributes.
	- Apply AND with bitmask to clear specified bits.
	- Write back the modified attributes through script_set_object_attributes.
================================================================================
*/
* = $620F
script_clear_object_attr_bit:
        sta     obj_bitmask                  		// save mask
        jsr     script_read_object_attributes   // A := current attributes
        and     obj_bitmask                  		// clear selected bit(s)
        jmp     script_set_object_attributes    // write back and return
/*
================================================================================
  script_set_object_attr_bit
================================================================================
Summary
	Set one bit in the current object's attribute byte. 
	Caller supplies a bitmask in A.

Arguments
	A  Bitmask selecting attribute bits to set (1 = set, 0 = keep).

Returns
	No direct return value. Tail-calls script_set_object_attributes to commit the
	updated attributes.

Description
	- Save the input bitmask from A.
	- Read the current object's attribute byte from the script context.
	- OR with the saved bitmask to set the selected bits.
	- Jump to script_set_object_attributes to write back the result.
================================================================================
*/
* = $6219
script_set_object_attr_bit:
        sta     obj_bitmask                   	  // save mask from A
        jsr     script_read_object_attributes // A := current attributes
        ora     obj_bitmask                   	  // set selected bit(s)
        jmp     script_set_object_attributes  // write back and return
/*
================================================================================
  op_clear/set_obj_attr_bit7/6/5
================================================================================
Summary
	Opcode entry shims for setting or clearing specific object attribute bits. Each
	entry loads a constant mask into A and uses BNE as an unconditional branch to
	either script_set_object_attr_bit (OR) or script_clear_object_attr_bit (AND).

Returns
	No direct return. Control transfers to the target routine which performs the
	attribute update.

Dispatch (opcodes)
	- #$97/#$D7 → set bit7:   A := MASK_BIT7        → BNE script_set_object_attr_bit
	- #$17/#$57 → clear bit7: A := MASK_CLEAR_BIT7  → BNE script_clear_object_attr_bit
	- #$B7/#$F7 → set bit6:   A := MASK_BIT6        → BNE script_set_object_attr_bit
	- #$37/#$77 → clear bit6: A := MASK_CLEAR_BIT6  → BNE script_clear_object_attr_bit
	- #$8F/#$CF → set bit5:   A := MASK_BIT5        → BNE script_set_object_attr_bit
	- #$0F/#$4F → clear bit5: A := MASK_CLEAR_BIT5  → BNE script_clear_object_attr_bit
================================================================================
*/
* = $6223
op_set_obj_attr_bit7:
        // Opcode #$97, #$D7 → set bit7 (mask #$80)
        lda     #MASK_BIT7
        bne     script_set_object_attr_bit
* = $6227
op_clear_obj_attr_bit7:
        // Opcode #$17, #$57 → clear bit7 (mask #$7F)
        lda     #MASK_CLEAR_BIT7
        bne     script_clear_object_attr_bit

* = $622B
op_set_obj_attr_bit6:
        // Opcode #$B7, #$F7 → set bit6 (mask #$40)
        lda     #MASK_BIT6
        bne     script_set_object_attr_bit
* = $622F
op_clear_obj_attr_bit6:
        // Opcode #$37, #$77 → clear bit6 (mask #$BF)
        lda     #MASK_CLEAR_BIT6
        bne     script_clear_object_attr_bit

* = $6233
op_set_obj_attr_bit5:
        // Opcode #$8F, #$CF → set bit5 (mask #$20)
        lda     #MASK_BIT5
        bne     script_set_object_attr_bit
* = $6237
op_clear_obj_attr_bit5:
        // Opcode #$0F, #$4F → clear bit5 (mask #$DF)
        lda     #MASK_CLEAR_BIT5
        bne     script_clear_object_attr_bit
/*
================================================================================
  op_if_active_io_eq_lo/16 - Opcodes #$A4, #$24
================================================================================
Summary
	Compare the script-referenced indirect object index against the active indirect
	object. Two entry points select comparison width: #$01 compares low byte only
	(#$A4), #$00 compares full 16-bit index (#$24). If equal, treat condition as
	true and skip the following offset; otherwise displace the script pointer.

Returns
	On equal: skips offset (condition true).
	On not equal: displaces pointer by signed offset (condition false).

Global Inputs
	active_io_id_lo   Low byte of active indirect object ID.
	active_io_id_hi   High byte of active indirect object ID.

Description
	- Load selector into A:
	  - #$01 for low-byte compare (opcode #$A4).
	  - #$00 for 16-bit compare (opcode #$24).
	- Call cmp_active_io_vs_script, which sets Z on equality.
	- BEQ → equal → jump to script_skip_offset (execute true block).
	- Else → jump to op_displace_pointer (execute false block).
================================================================================
*/
* = $623B
op_if_active_io_eq_lo:
        // ------------------------------------------------------------
        // Opcode #$A4  compare IO index, lo only
        // ------------------------------------------------------------
        lda     #IO_CMP_MODE_LO_ONLY
        bne     do_compare_active_io
		
* = $623F
op_if_active_io_eq_16:
        // ------------------------------------------------------------
        // Opcode #$24  compare IO index, lo+hi
        // ------------------------------------------------------------
        lda     #IO_CMP_MODE_HI0_AND_LO

do_compare_active_io:
        jsr     cmp_active_io_vs_script 	// sets Z if equal per selector
        beq     cond_true_skip_offset       // equal → condition true path

        // Condition false → add signed offset to script pointer
        jmp     op_displace_pointer

cond_true_skip_offset:
        // Condition true → skip offset and execute following block
        jmp     script_skip_offset
/*
================================================================================
  op_if_active_io_ne_lo/16
================================================================================
Summary
	If-equal check against the active indirect object. Two opcodes share this stub:
	#$E4 compares low byte only; #$64 requires hi==#$00 then compares low. Calls
	cmp_active_io_vs_script and branches: equal → skip offset; not equal → displace.

Returns
	- Equal (A = #$01) → BNE taken → jump to script_skip_offset
	- Not equal (A = #$00) → BNE not taken → jump to op_displace_pointer

Global Inputs
	active_io_id_lo  Low byte of active indirect object ID.
	active_io_id_hi  High byte of active indirect object ID.

Description
	- Load comparison mode per opcode.
	- Call cmp_active_io_vs_script which reads one script byte (candidate low ID)
	  and returns #$01 if equal per mode, else #$00.
	- Use BNE as the equality branch since equal returns #$01 (nonzero).
	- On equal, skip the following signed offset and execute the true block.
	- On not equal, add the signed offset to the script pointer.
================================================================================
*/
* = $624C
op_if_active_io_ne_lo:
        // ------------------------------------------------------------
        // Opcode #$E4  compare IO index, lo only
        // ------------------------------------------------------------
        lda     #IO_CMP_MODE_LO_ONLY
        bne     do_compare_active_io_2
* = $6250
op_if_active_io_ne_16:
        // ------------------------------------------------------------
        // Opcode #$64  compare IO index, lo+hi
        // ------------------------------------------------------------
        lda     #IO_CMP_MODE_HI0_AND_LO

do_compare_active_io_2:
        jsr     cmp_active_io_vs_script        // A := result (#$01 eq / #$00 neq)
        bne     cond_true_skip_offset_2        // ne → condition true path

        // Condition false → add signed offset to script pointer
        jmp     op_displace_pointer

cond_true_skip_offset_2:
        // Condition true → skip offset and execute following block
        jmp     script_skip_offset		
/*
================================================================================
  cmp_active_io_vs_script
================================================================================
Summary
	Compare the script-provided indirect-object low index against the active
	indirect object, using one of two modes selected in A. 
	Mode #IO_CMP_MODE_LO_ONLY compares only	the low byte. 
	Mode #IO_CMP_MODE_HI0_AND_LO first requires the high byte to be #$00, then compares
	the low byte. Returns True if equal per mode, otherwise False.

Arguments
	A  Comparison mode: 
		#IO_CMP_MODE_LO_ONLY = compare low byte only
		#IO_CMP_MODE_HI0_AND_LO = require hi==#$00 then compare low.

Returns
	A  #TRUE if equal per selected mode; #FALSE otherwise.
	Reads one script byte in both modes (the candidate low index).

Global Inputs
	active_io_id_lo   Low byte of the active indirect object ID.
	active_io_id_hi   High byte of the active indirect object ID.

Description
	- If A == #$00, require active_io_id_hi == #$00; if not, consume script low
	  index and return #$00.
	- In either case, read the candidate low index from the script stream.
	- Compare candidate low index with active_io_id_lo.
	- Return #$01 on equality, else #$00.

Notes
	- Consumes exactly one byte from the script stream in both modes.
	- Zero/Negative flags follow the final load into A (#$00 or #$01).
================================================================================
*/
* = $625D		
cmp_active_io_vs_script:
        beq     mode_hi0_then_lo      			// A==#$00 → hi must be 0 then compare lo

		// -----------------------------------------------------------------------------
		// Mode #IO_CMP_MODE_LO_ONLY: compare only low byte; hi is ignored for equality test.
		// If hi != 0 we still proceed to the common lo-compare path by forcing NZ.
		// -----------------------------------------------------------------------------
        lda     active_io_id_hi       			// test hi; ignored for equality but used to set flags
        beq     join_hi_gate                	// hi==0 → fall into lo-compare path
        lda     #$00                           	// force NZ clear path via carry add below
join_hi_gate:
        jmp     test_hi_zero_gate               // join common path

		// -----------------------------------------------------------------------------
		// Mode #IO_CMP_MODE_HI0_AND_LO: hi must be zero; otherwise return False and consume one script byte.
		// -----------------------------------------------------------------------------
mode_hi0_then_lo:
        clc
        adc     active_io_id_hi       			// A was #$00 → sets Z if hi==0
test_hi_zero_gate:
        beq     hi_zero_read_cmp_lo             // hi==0 → proceed to compare lo

        // hi != 0 → not equal; consume lo from stream and return #$00
        jsr     script_read_byte                // discard script lo index
        lda     #FALSE
        rts

		// -----------------------------------------------------------------------------
		// Common lo compare: read script lo and compare to active_io_id_lo.
		// -----------------------------------------------------------------------------
hi_zero_read_cmp_lo:
        jsr     script_read_byte                // A := script lo index
        cmp     active_io_id_lo          		// equal?
        beq     return_true                     // yes → True
        lda     #FALSE                          // no  → False
        rts

return_true:
        lda     #TRUE
        rts

/*
================================================================================
  op_pickup_object - Opcode #$90
================================================================================

Summary
	Pick up object. Resolve the target room object, allocate a heap
	block of its size, copy the object into the block, register it in the next free
	inventory slot, assign ownership to the current kid, update flags to mark it
	removed-from-room and stored-in-inventory, then recenter camera and refresh UI.

Arguments
	Operand: object index byte. 00 → use active direct object index.

Returns
	* On success: object copied to heap, inventory slot populated, attributes and
	  owner updated, camera/UI refreshed. A is not used as a success code.
	* On failure: if object not found the routine returns immediately with no
	  side effects beyond the lookup.

Global Inputs
	 active_do_id_lo         fallback object index when operand is 00
	 current_kid_idx         low-nibble owner id to stamp into attributes
	 object_attributes[]     attribute/owner packed byte per object
	 object_ptr_lo_tbl[]     inventory block pointer table, low byte (read for structure)
	 object_ptr_hi_tbl[]     inventory block pointer table, high byte (read for structure)
	 inventory_objects[]     inventory object indices (read for structure)

Global Outputs
	 object_ptr_lo_tbl[Y]    set to allocated heap block low byte for chosen slot
	 object_ptr_hi_tbl[Y]    set to allocated heap block high byte for chosen slot
	 inventory_objects[Y]    set to effective object index
	 object_attributes[idx]  bits7|5 set, owner low nibble set to current kid
	 cam_current_pos         set to default camera position

Vars/State
	 room_obj_index          effective object index passed to room search
	 room_obj_state          search state selector (set to 00)
	 obj_index_backup        saved index for re-lookup after allocation
	 obj_src_ptr             source pointer to room object data
	 obj_size_lo/obj_size_hi object size read from header; copy loop uses size_lo
	 heap_block_ptr          destination pointer to allocated heap block

Description
	* Decode operand; use active direct object if operand is 00.
	* Lookup room object; if not found, return.
	* Read size from object header and allocate heap block of that size.
	* Re-resolve the room object pointer to guard against pointer invalidation.
	* Copy payload downward for size_lo bytes into the allocated block.
	* Tag heap block header fields: type at +2, inventory slot index at +3.
	* Register block and object index in inventory tables at the chosen slot.
	* Update attributes: set removed-from-room and stored-in-inventory flags; set
	  owner nibble to current kid.
	* Recenter camera and refresh scripts/UI as required.

Notes
	* The copy loop ignores size_hi; resources must fit in 256 bytes or they will
	  be truncated. Consider extending the loop if larger objects are possible.
	* No explicit checks for mem_alloc failure or lack of a free inventory slot
================================================================================
*/
* = $6283
op_pickup_object:
		// ------------------------------------------------------------
		// Decode operand (object index)
		//
		// Read one-byte operand. If zero, fall back to active DO.
		// Z flag from CMP decides the branch to explicit index path.
		// ------------------------------------------------------------
		jsr     script_read_byte               // A := operand (candidate object index)
		cmp     #$00                           // set Z if operand == 0
		bne     set_object_index               // Z=0 → use operand as explicit index
		lda     active_do_id_lo                // Z=1 → load active direct object index

set_object_index:
		sta     room_obj_index                 // save effective index for lookup
		sta     obj_index_backup               // mirror index for re-lookup after alloc
		lda     #$00                           // clear state selector
		sta     room_obj_state                 // state := 0 (default search state)

		// ------------------------------------------------------------
		// Locate the object in the current room
		//
		// On success: room_obj_ptr_lo/hi set by callee, A != OBJ_NOT_FOUND
		// On failure: A == OBJ_NOT_FOUND → early return
		// ------------------------------------------------------------
		jsr     dispatch_to_room_search        // A := status; sets object pointer on success
		cmp     #OBJ_NOT_FOUND                 // Z=1 if not found
		bne     found_object_continue          // found → continue
		rts                                    // not found → exit immediately

found_object_continue:
		// ------------------------------------------------------------
		// Read object size and allocate heap block
		//
		// Copy resolved room object pointer into source pointer pair.
		// Keeps source stable across heap operations.
		// ------------------------------------------------------------
		
		// Setup read pointer of object data
		lda     room_obj_ptr_lo               
		sta     obj_src_ptr_lo                
		lda     room_obj_ptr_hi               
		sta     obj_src_ptr_hi                

		// Copy object size to local vars
		ldy     #$00                           
		lda     (obj_src_ptr),y                
		sta     obj_size_lo                    
		iny                                    
		lda     (obj_src_ptr),y                
		sta     obj_size_hi                    

		// Alloc a block
		ldx     obj_size_lo                    // X := alloc size low byte
		ldy     obj_size_hi                    // Y := alloc size high byte
		jsr     mem_alloc                      // allocate heap block of size (Y:X)
		
		// Save allocated block pointer
		stx     heap_block_ptr_lo              
		sty     heap_block_ptr_hi              

		// ------------------------------------------------------------
		// Re-resolve pointer in case heap moved
		//
		// Ensures source pointer is fresh after mem_alloc, which may
		// mutate tables or relocate memory used by room resources.
		// ------------------------------------------------------------
		lda     obj_index_backup               // A := saved object index for re-lookup
		sta     room_obj_index                 // set lookup arg: index = A		
		lda     #$00                           // A := default state selector
		sta     room_obj_state                 // set lookup arg: state = 0
		jsr     dispatch_to_room_search        // refresh room_obj_ptr_lo/hi for copy source

		// Refresh read pointer of object data
		lda     room_obj_ptr_lo               
		sta     obj_src_ptr_lo                
		lda     room_obj_ptr_hi               
		sta     obj_src_ptr_hi                

		// ------------------------------------------------------------
		// Copy object data into allocated block
		//
		// Copy byte-for-byte downward using Y as a countdown counter.
		// Loop runs for size.lo bytes; high byte is ignored by design.
		// CPY #$FF detects wrap from 0 → $FF to terminate.
		// ------------------------------------------------------------
		ldy     obj_size_lo                    // Y := size.lo (byte count)
		dey                                    // pre-decrement so last copied index is size.lo-1

copy_object_to_block_loop:
		lda     (obj_src_ptr),y                // A := src[Y]
		sta     (heap_block_ptr),y             // dst[Y] := A
		dey                                    // Y := Y-1
		cpy     #$ff                           // Z=1 when Y wrapped from 0 → $FF
		bne     copy_object_to_block_loop      // continue until wrap detected

		// ------------------------------------------------------------
		// Tag and register in inventory
		// ------------------------------------------------------------		
		// Write resource type = OBJECT
		ldy     #OFFSET_TYPE                   // Y := header offset for type
		lda     #RSRC_TYPE_OBJECT              // A := object resource type
		sta     (heap_block_ptr),y             // [blk+2] := RSRC_TYPE_OBJECT

		// Find free inventory slot, save slot index in .Y
		jsr     find_free_inventory_slot       

		// Copy block pointer into object ptr table[Y]
		lda     heap_block_ptr_lo              
		sta     object_ptr_lo_tbl,y            
		lda     heap_block_ptr_hi              
		sta     object_ptr_hi_tbl,y            

		// Copy object ID into table of inventory object IDs
		lda     obj_index_backup               // A := effective object index
		sta     inventory_objects,y            // slot[Y].object_index := A

		// Save inventory slot index into header
		tya                                    // A := inventory slot index
		ldy     #OFFSET_INV_INDEX              // Y := header offset for inventory index
		sta     (heap_block_ptr),y             // [blk+3] := slot index

		// ------------------------------------------------------------
		// Update ownership and attributes
		// ------------------------------------------------------------
		// Set removed-from-room and in-inventory flags.
		ldy     obj_index_backup               // Y := object index
		lda     object_attributes,y            // A := current attributes
		ora     #MASK_OBJ_FLAGS_SET            // set bits7|5: removed + stored
		
		// Replace owner low nibble with current kid index.
		and     #MASK_OBJ_CLEAR_OWNER         // keep high nibble, clear owner low nibble
		ora     current_kid_idx                // set owner = current kid
		sta     object_attributes,y            // commit updated attributes

		// ------------------------------------------------------------
		// Refresh camera and UI
		//
		// Ensure camera recenters; fix script pointers after reloc;
		// redraw inventory with safety guards.
		// ------------------------------------------------------------
		lda     #CAM_DEFAULT_POS              // A := default camera position
		sta     cam_current_pos               // camera_current_position := A
		jsr     refresh_script_addresses_if_moved // Recompute script PCs if heap moved
		jsr     refresh_inventory  // Redraw inventory UI safely
		rts                                   // done		
		

/*
================================================================================
  op_get_object_owner - Opcodes #$73, #$F3 
================================================================================
Summary
	Get an object's owner and write it to a game variable. The owner is encoded in
	the low nibble of the object's attributes byte.

Arguments
	- Variable index: one byte read after the opcode; destination in game_vars.
	- Object index operand (bit7-aware): if the decoded index is 00, use the active
	  direct object; otherwise use the explicit index.

Returns
	game_vars[variable_index] := owner id (0..15) from the object's attributes.

Global Inputs
	active_do_id_lo         active direct object index when operand resolves to 00
	object_attributes[]     per-object attributes; low nibble holds owner id

Global Outputs
	game_vars[]             target variable array updated at the given index

Description
	- Read destination variable index.
	- Decode object index using bit7 rules; fall back to active DO if 00.
	- Read attributes for that object and mask the owner low nibble.
	- Store the owner into the requested game variable.
================================================================================
*/
// Opcode #$73, #$F3
* = $6409
op_get_object_owner:
        // ------------------------------------------------------------
        // Read target variable index
        // ------------------------------------------------------------
        jsr     script_read_byte               // A := var index
        sta     variable_index                 // save index

        // ------------------------------------------------------------
        // Decode object index operand (bit7-aware)
        // 00 → use active object; else explicit index in A
        // ------------------------------------------------------------
        jsr     script_load_operand_bit7       // A := object index
        cmp     #$00
        bne     resolve_object_owner           // explicit index → use A
        lda     active_do_id_lo                // fallback to active object

resolve_object_owner:
        // ------------------------------------------------------------
        // Fetch owner from object attributes low nibble
        // ------------------------------------------------------------
        tay                                     // Y := object index
        lda     object_attributes,y             // A := attr byte
        and     #OWNER_NIBBLE_MASK              // keep owner nibble

        // ------------------------------------------------------------
        // Store owner into the requested game variable
        // ------------------------------------------------------------
        ldy     variable_index                  // Y := var index
        sta     game_vars,y                     // game_vars[Y] := owner
        rts
/*
================================================================================
  op_set_object_owner - OpcodeS #$29, #$69, #$A9, #$E9
================================================================================
Summary
	Set an object's owner. Decodes two operands (item, owner). If the item operand
	resolves to 00, uses the active direct object. If the owner operand is 00,
	signals deferred removal from inventory by setting a sentinel owner and a
	removal flag. Commits the owner id into the low nibble of the object's
	attributes, preserves the high-nibble flags, then refreshes inventory UI and
	repairs script pointers if relocation occurred.

Arguments
	* Item operand (bit7-aware): 00 → use active direct object index; else explicit.
	* Owner operand (bit6-aware): 00 → request removal path; else explicit owner id.

Returns
	No return value. 
	Side effects: updates object attributes and inventory/UI  state, and may 
		signal removal for the main loop to process.

Global Inputs
	active_do_id_lo  			fallback object index when item operand resolves to 00
	object_attributes[]  		per-object attribute bytes; high nibble = flags
	remove_obj_from_inv_flag  	checked by main loop to process removals

Global Outputs
	object_attributes[item]  	low nibble set to selected owner id
	remove_obj_from_inv_flag  	set nonzero when owner operand was 00
	inventory/UI state  		refreshed via inventory redraw routine
	script addresses  			refreshed via relocation maintenance routine

Vars/State
	owner  				temporary holding the selected owner id (possibly sentinel)
	item  				temporary holding the effective object index

Description
	* Read item operand; if 00, substitute the active direct object index.
	* Read owner operand; if 00, set removal flag and substitute sentinel owner.
	* Write (attributes[item] & $F0) | owner to commit owner low nibble.
	* Refresh inventory UI and run script relocation maintenance.
================================================================================

If the item operand is #$00, the active object is used instead.  
This allows generic scripts to manipulate whichever object is currently 
selected, avoiding the need to hardcode an object index (for example, 
a “give item” routine can handle any carried item).

If the owner operand is #$00, the object is scheduled for removal from 
the inventory. In that case, both the owner field and the removal flag 
are set to #$0D. The main loop interprets this sentinel as “no owner” 
rather than a valid actor index.

Some scripts explicitly assign owner #$0D instead of #$00 for the same 
effect. Both forms remove the item from the inventory and mark it as 
unowned.

The engine encodes ownership in the low nibble of the object’s attributes byte.  
Kid IDs occupy #$00–#$06. The value #$0D acts as a limbo sentinel, signaling 
that the object is not possessed by any active actor. Script logic checks 
ownership only against kid IDs; non-kid values such as #$0D are ignored.  
When ownership is set to #$0D, the item disappears from the player’s inventory 
and becomes inactive in the game world.
================================================================================
*/
* = $6424
op_set_object_owner:
        // ------------------------------------------------------------
        // Select target item
        // Operand bit7-aware. 00 → fallback to active object.
        // ------------------------------------------------------------
        jsr     script_load_operand_bit7       	// A := item index candidate
        cmp     #$00
        bne     item_determined                	// nonzero → explicit item index
        lda     active_do_id_lo               	// zero → use active object
item_determined:
        sta     item                           	// save item index

        // ------------------------------------------------------------
        // Select owner
        // Operand bit6-aware. OWNER_NONE → request removal path.
        // ------------------------------------------------------------
        jsr     script_load_operand_bit6       	// A := owner id candidate
        cmp     #OWNER_NONE
        bne     owner_determined               	// nonzero → explicit owner
        // Owner == OWNER_NONE → flag removal handled by main loop
		// Use OWNER_REMOVE_SENTINEL sentinel
        lda     #OWNER_REMOVE_SENTINEL
        sta     remove_obj_from_inv_flag       	// signal deferred removal
owner_determined:
        sta     owner                          	// owner := A (possibly $0D from above)

        // ------------------------------------------------------------
        // Commit owner low nibble into object attributes
        // Keep high nibble (flags) intact.
        // ------------------------------------------------------------
        ldy     item
        lda     object_attributes,y
        and     #MASK_OBJ_CLEAR_OWNER           // clear owner nibble
        ora     owner                          	// set owner nibble
        sta     object_attributes,y

        // ------------------------------------------------------------
        // Refresh inventory UI and relocate script pointers if needed
        // ------------------------------------------------------------
        jsr     refresh_inventory
        lda     owner
        jsr     refresh_script_addresses_if_moved
        rts

/*
================================================================================
  op_remove_from_inventory
================================================================================
Summary
	Remove an object from the inventory. Resolves the object’s heap-backed inventory
	resource, frees its memory block, and clears the slot’s object index and stored
	pointers.

Arguments
	X			Object index to remove.

Returns
	On success: Inventory slot cleared; associated heap block released.	  
	On failure (object not found in inventory): No changes.

Global Outputs
	inventory_objects[slot]: set to 0.
	object_ptr_lo_tbl[slot]: set to 0.
	object_ptr_hi_tbl[slot]: set to 0.
	Heap allocator: resource block returned to free list.

Description
	* Call the inventory resource resolver for the object index in X.
	* If not found, return.
	* Save Y (the inventory slot index).
	* Free the resource block using the pointer returned by the resolver.
	* Restore the slot index and clear its object index and both block-pointer
	  bytes to zero.
================================================================================
*/
* = $6453
op_remove_from_inventory:
        // ------------------------------------------------------------
        // Lookup object in inventory and get its resource pointer
        // ------------------------------------------------------------
        jsr     resolve_object_resource      // populates resource_ptr and sets A=0 if found
        cmp     #OBJ_FOUND_IN_INV
        bne     exit_remove_from_inv         // not found → exit

        // ------------------------------------------------------------
        // Save inventory slot index (Y) on stack
        // ------------------------------------------------------------
        tya
        pha

        // ------------------------------------------------------------
        // Release resource block
        // ------------------------------------------------------------
        ldx     resource_ptr                 // X := resource_ptr.lo
        ldy     resource_ptr+1               // Y := resource_ptr.hi
        jsr     mem_release                  // free allocated memory

        // ------------------------------------------------------------
        // Clear inventory slot and resource tables
        // ------------------------------------------------------------
        pla                                  
        tax                                  
        lda     #$00
        sta     inventory_objects,x
        sta     object_ptr_hi_tbl,x
        sta     object_ptr_lo_tbl,x

exit_remove_from_inv:
        rts
/*
================================================================================
  op_set_object_name - Opcodes #$54, #$D4
================================================================================
Summary
	Set an object's name from a zero-terminated string in the script stream.
	Opcode #$54 targets inventory; opcode #$D4 targets room objects. The routine
	selects the object, resolves a name buffer pointer for the chosen location,
	copies the new name byte-by-byte, then refreshes the inventory UI.

Arguments
	- Operand 1: object index byte. If 00, use the active direct object.
	- Implicit: opcode bit7 selects target location (0 = inventory, 1 = room).

Returns
	- No explicit return value. On completion the object name buffer holds the new
	  zero-terminated string and the inventory UI may refresh.

Global Inputs
	opcode                 selects location via bit7
	active_do_id_lo        fallback object index when operand is 00

Global Outputs
	Object name buffer     overwritten with zero-terminated string
	Inventory/UI state     refreshed after rename

Description
	- Read object index; if 00, substitute the active direct object.
	- Inspect opcode bit7 to choose inventory vs room name pointer.
	- Resolve the destination pointer for the object name.
	- Copy bytes from script until a 00 terminator is stored.
	- Request inventory refresh.

Notes
	- No bounds checks on the destination buffer; malformed or unterminated input
	  can overflow. Assumes the script provides a valid 00-terminated name.
================================================================================
*/
* = $69DA		
op_set_object_name:
        // ------------------------------------------------------------
        // Read object index operand
        // 00 → fallback to active object
        // ------------------------------------------------------------
        jsr     script_read_byte   				// A := object index
        cmp     #$00
        bne     load_effective_object_index		// nonzero → explicit index
		
        lda     active_do_id_lo               	// fallback to active object
load_effective_object_index:
        tax                                    	// X := object index

        // ------------------------------------------------------------
        // Determine object location
        // Bit7 of opcode → 1=room, 0=inventory
        // ------------------------------------------------------------
        lda     opcode
        bpl     select_inventory_location      	// bit7=0 → inventory
		
        lda     #OBJ_HI_IMMOVABLE               // bit7=1 → room
        jmp     resolve_name_pointer
		
select_inventory_location:
        lda     #OBJ_HI_MOVABLE
		
resolve_name_pointer:
        // ------------------------------------------------------------
        // Resolve object name pointer
        // ------------------------------------------------------------
        jsr     resolve_object_name        		// returns (object_name_pointer)

        // ------------------------------------------------------------
        // Copy new name string from script
        // ------------------------------------------------------------
        ldy     #$ff                           	// Y := $FF (will increment to 0)
write_name_from_script_loop:
        iny
        jsr     script_read_byte               	// A := next char from script
        sta     (object_name_pointer),y        	// write to name buffer
        cmp     #WORD_TERMINATOR                // reached terminator?
        bne     write_name_from_script_loop     // no → continue

        // ------------------------------------------------------------
        // Refresh inventory UI
        // ------------------------------------------------------------
        jsr     refresh_inventory
        rts

/*
procedure script_is_object_attr_bit_clear(bitmask)
    // bitmask comes in as A
    obj_bitmask = bitmask

    // Read attributes of the “current” object in this script context
    attrs = script_read_object_attributes()

    // Test selected bits
    test = attrs AND obj_bitmask

    if test != 0 then
        // Bit is set → condition is “true” → skip following offset
        script_skip_offset()
    else
        // Bit is clear → condition is “false” → apply signed offset
        op_displace_pointer()
    end if



procedure script_is_object_attr_bit_set(bitmask)
    // bitmask comes in as A
    obj_bitmask = bitmask

    // Read attributes of the “current” object in this script context
    attrs = script_read_object_attributes()

    // Test selected bits
    test = attrs AND obj_bitmask

    if test == 0 then
        // Bit is clear → condition “true” for the *set* test
        // (this helper is wired so “set” means: skip when bit is clear)
        script_skip_offset()
    else
        // Bit is set → condition “false” → apply signed offset
        op_displace_pointer()
    end if



procedure op_if_obj_attr_bit7_set()
    // Test “bit 7 is set” using shared helper
    script_is_object_attr_bit_set(MASK_BIT7)


procedure op_if_obj_attr_bit7_clear()
    // Test “bit 7 is clear” using shared helper
    script_is_object_attr_bit_clear(MASK_BIT7)


procedure op_if_obj_attr_bit6_set()
    script_is_object_attr_bit_set(MASK_BIT6)


procedure op_if_obj_attr_bit6_clear()
    script_is_object_attr_bit_clear(MASK_BIT6)


procedure op_if_obj_attr_bit5_set()
    script_is_object_attr_bit_set(MASK_BIT5)


procedure op_if_obj_attr_bit5_clear()
    script_is_object_attr_bit_clear(MASK_BIT5)


procedure script_clear_object_attr_bit(clear_mask)
    // clear_mask has 0 where bits should be cleared, 1 where preserved
    obj_bitmask = clear_mask

    attrs = script_read_object_attributes()
    attrs = attrs AND obj_bitmask
    script_set_object_attributes(attrs)


procedure script_set_object_attr_bit(set_mask)
    // set_mask has 1 where bits should be set
    obj_bitmask = set_mask

    attrs = script_read_object_attributes()
    attrs = attrs OR obj_bitmask
    script_set_object_attributes(attrs)


procedure op_set_obj_attr_bit7()
    script_set_object_attr_bit(MASK_BIT7)


procedure op_clear_obj_attr_bit7()
    script_clear_object_attr_bit(MASK_CLEAR_BIT7)


procedure op_set_obj_attr_bit6()
    script_set_object_attr_bit(MASK_BIT6)


procedure op_clear_obj_attr_bit6()
    script_clear_object_attr_bit(MASK_CLEAR_BIT6)


procedure op_set_obj_attr_bit5()
    script_set_object_attr_bit(MASK_BIT5)


procedure op_clear_obj_attr_bit5()
    script_clear_object_attr_bit(MASK_CLEAR_BIT5)



procedure op_if_active_io_eq_lo()
    // Compare only the low byte of active IO
    mode = IO_CMP_MODE_LO_ONLY
    result = cmp_active_io_vs_script(mode)   // TRUE if equal, FALSE otherwise

    if result == TRUE then
        script_skip_offset()     // condition “true”
    else
        op_displace_pointer()    // condition “false”
    end if



procedure op_if_active_io_eq_16()
    // Compare hi==0 and low byte
    mode = IO_CMP_MODE_HI0_AND_LO
    result = cmp_active_io_vs_script(mode)

    if result == TRUE then
        script_skip_offset()
    else
        op_displace_pointer()
    end if



procedure op_if_active_io_ne_lo()
    mode = IO_CMP_MODE_LO_ONLY
    result = cmp_active_io_vs_script(mode)

    if result == FALSE then
        script_skip_offset()
    else
        op_displace_pointer()
    end if



procedure op_if_active_io_ne_16()
    mode = IO_CMP_MODE_HI0_AND_LO
    result = cmp_active_io_vs_script(mode)

    if result == FALSE then
        script_skip_offset()
    else
        op_displace_pointer()
    end if



function cmp_active_io_vs_script(mode) -> byte  // returns TRUE (1) or FALSE (0)
    // mode == IO_CMP_MODE_HI0_AND_LO:
    //   require active_io_id_hi == 0, else consume one script byte and return FALSE
    // mode == IO_CMP_MODE_LO_ONLY:
    //   ignore hi for equality, always proceed to low-byte comparison

    if mode == IO_CMP_MODE_HI0_AND_LO then
        // Require hi == 0
        if active_io_id_hi != 0 then
            // Still must consume one script byte
            discard = script_read_byte()
            return FALSE
        end if
    else
        // LO_ONLY path: hi is not part of equality, but if hi != 0, we still
        // flow through into the lo compare; behavior is “compare only lo”.
        // Nothing to do here beyond establishing flags in assembly.
        pass
    end if

    // Common: read candidate low index from script
    candidate_lo = script_read_byte()

    if candidate_lo == active_io_id_lo then
        return TRUE
    else
        return FALSE
    end if
end function


procedure op_pickup_object()
    // 1) Decode object index operand: 0 means “use active direct object”
    operand = script_read_byte()
    if operand == 0 then
        effective_index = active_do_id_lo
    else
        effective_index = operand
    end if

    room_obj_index = effective_index
    obj_index_backup = effective_index
    room_obj_state = 0

    // 2) Find the object in the current room
    status = dispatch_to_room_search(room_obj_index, room_obj_state)
    if status == OBJECT_NOT_FOUND then
        return
    end if

    // At this point room_obj_ptr points to the room object data

    // 3) Read object size from header at room_obj_ptr[0..1]
    obj_src_ptr = room_obj_ptr
    size_lo = read_byte(obj_src_ptr + 0)
    size_hi = read_byte(obj_src_ptr + 1)
    obj_size_lo = size_lo
    obj_size_hi = size_hi

    // 4) Allocate a heap block of that size
    heap_block_ptr = mem_alloc(size_hi, size_lo)

    // 5) Re-resolve room object pointer in case allocation moved things
    room_obj_index = obj_index_backup
    room_obj_state = 0
    status = dispatch_to_room_search(room_obj_index, room_obj_state)
    if status == OBJECT_NOT_FOUND then
        // (Code doesn’t check this in detail, but logically this would be an error)
        return
    end if
    obj_src_ptr = room_obj_ptr

    // 6) Copy object data into heap block (size_lo bytes, ignoring size_hi)
    count = obj_size_lo
    // Copy bytes [0 .. count-1]
    for i from 0 to count - 1 do
        byte_val = read_byte(obj_src_ptr + i)
        write_byte(heap_block_ptr + i, byte_val)
    end for

    // 7) Tag block header and register in inventory
    // Header: [heap_block_ptr + OFFSET_TYPE] = RSRC_TYPE_OBJECT
    write_byte(heap_block_ptr + OFFSET_TYPE, RSRC_TYPE_OBJECT)

    // Find a free inventory slot, returned in slot_index
    slot_index = find_free_inventory_slot()

    // Register block pointer and object index in inventory tables
    object_ptr_lo_tbl[slot_index] = low_byte(heap_block_ptr)
    object_ptr_hi_tbl[slot_index] = high_byte(heap_block_ptr)
    inventory_objects[slot_index] = obj_index_backup

    // Also store the inventory slot index into the block header at OFFSET_INV_INDEX
    write_byte(heap_block_ptr + OFFSET_INV_INDEX, slot_index)

    // 8) Update ownership and object attributes
    idx = obj_index_backup
    attrs = object_attributes[idx]

    // Set “in inventory” and “removed from room” flags (bits 7 and 5)
    attrs = attrs OR MASK_OBJ_FLAGS_SET

    // Clear previous owner nibble and set new owner = current kid
    attrs = attrs AND MASK_OBJ_CLEAR_OWNER
    attrs = attrs OR current_kid_idx

    object_attributes[idx] = attrs

    // 9) Camera and UI refresh
    cam_current_pos = CAMERA_RESET_POSITION
    refresh_script_addresses_if_moved()
    refresh_inventory()



procedure op_get_object_owner()
    // Destination game variable index
    variable_index = script_read_byte()

    // Decode object index (bit7-aware)
    obj_index = script_load_operand_bit7()
    if obj_index == 0 then
        obj_index = active_do_id_lo
    end if

    // Owner is stored in low nibble of attributes
    attrs = object_attributes[obj_index]
    owner = attrs AND OWNER_NIBBLE_MASK

    // Write owner into game_vars[variable_index]
    game_vars[variable_index] = owner



procedure op_set_object_owner()
    // 1) Resolve item index (object)
    item_index = script_load_operand_bit7()
    if item_index == 0 then
        item_index = active_do_id_lo
    end if
    item = item_index

    // 2) Resolve owner
    owner_candidate = script_load_operand_bit6()

    if owner_candidate == OWNER_NONE then
        // Owner 0 means “remove this object from inventory”
        // Replace with sentinel and flag removal
        owner_candidate = OWNER_REMOVE_SENTINEL
        remove_obj_from_inv_flag = OWNER_REMOVE_SENTINEL
    end if

    owner = owner_candidate

    // 3) Commit owner into attributes low nibble
    attrs = object_attributes[item]
    attrs = attrs AND MASK_OBJ_CLEAR_OWNER   // clear low nibble
    attrs = attrs OR owner                   // set new owner nibble
    object_attributes[item] = attrs

    // 4) Refresh inventory UI and script relocations as needed
    refresh_inventory()
    refresh_script_addresses_if_moved()



procedure op_remove_from_inventory()
    // X contains the object index to remove

    // Look up the object’s inventory resource and its pointer
    status, resource_ptr, slot_index = resolve_object_resource(X)

    if status != OBJ_FOUND_IN_INV then
        return
    end if

    // Save slot_index, free the heap block
    heap_ptr = resource_ptr
    mem_release(heap_ptr)

    // Clear inventory slot and pointer tables for this slot
    inventory_objects[slot_index] = 0
    object_ptr_lo_tbl[slot_index] = 0
    object_ptr_hi_tbl[slot_index] = 0



procedure op_set_object_name()
    // 1) Resolve object index; 0 = active object
    raw_index = script_read_byte()
    if raw_index == 0 then
        object_index = active_do_id_lo
    else
        object_index = raw_index
    end if

    // 2) Determine location from opcode high bit:
    //    bit7 = 0 → inventory name
    //    bit7 = 1 → room object name
    if (opcode AND 0x80) == 0 then
        obj_hi_selector = OBJ_HI_MOVABLE      // inventory
    else
        obj_hi_selector = OBJ_HI_IMMOVABLE    // room object
    end if

    // 3) Resolve destination buffer pointer for this object’s name
    object_name_pointer = resolve_object_name(object_index, obj_hi_selector)

    // 4) Copy a zero-terminated string from the script into that buffer
    i = 0
    loop:
        ch = script_read_byte()
        write_byte(object_name_pointer + i, ch)
        i = i + 1
        if ch != WORD_TERMINATOR then
            goto loop
        end if

    // 5) Refresh inventory UI (name changes are visible there)
    refresh_inventory()

*/
