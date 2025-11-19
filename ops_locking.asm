/*
================================================================================
  ops_locking - resource lock/unlock script operations
================================================================================
Summary
	* Implements eight script opcodes that lock or unlock cached resources:
	  sounds, scripts, costumes, and rooms.
	* Each opcode reads an index from the running script, locates that
	  resource’s attribute byte, and toggles bit7 to protect or release it
	  from eviction.

	* Each resource type has an attribute table. Bit7 is the “lock” flag:
		  1 = locked (cannot be evicted)
		  0 = unlocked (normal eviction)
	* Execution steps:
		  1. Read index byte from script.
		  2. X := index → select table entry.
		  3. Lock → ORA #$80, Unlock → AND #$7F.
		  4. Store result back to the attribute table.
	* Locking only changes eviction eligibility, not loading state.

	* Prevents critical data from being evicted while in active use.
	* Often used before cutscenes, multi-room transitions, or audio sequences.

Limitations
	* No bounds checks on the index.
	* No reference counting; bit7 is a simple toggle.
	* No change to resource content, only metadata.

================================================================================

+-----------------------------+--------------------+--------------------------------------------+
| Routine                     | Opcodes (hex)      | Purpose                                    |
+-----------------------------+--------------------+--------------------------------------------+
| op_lock_sound               | 53                 | Lock sound resource (set bit7 in attrs)    |
| op_unlock_sound             | D3                 | Unlock sound resource (clear bit7)         |
| op_lock_script              | 33                 | Lock script resource (set bit7 in attrs)   |
| op_unlock_script            | B3                 | Unlock script resource (clear bit7)        |
| op_lock_costume             | 13                 | Lock costume resource (set bit7 in attrs)  |
| op_unlock_costume           | 93                 | Unlock costume resource (clear bit7)       |
| op_lock_room                | 4D                 | Lock room resource (set bit7 in attrs)     |
| op_unlock_room              | CD                 | Unlock room resource (clear bit7)          |
+-----------------------------+--------------------+--------------------------------------------+
*/

#importonce
#import "globals.inc"
#import "constants.inc"
#import "script_engine.asm"
#import "script_primitives.asm"
#import "ops_primitives.asm"

* = $6A5B
op_lock_sound:
		// ------------------------------------------------------------
		// Opcode #$53  Lock sound
		// Sets bit7 of sound_liveness_tbl[index] to lock the resource.
		// ------------------------------------------------------------
		jsr     script_read_byte  	// A := sound index
		tax                         // X := index
		lda     sound_liveness_tbl,x   // A := attrs[index]
		ora     #MASK_BIT7          // set bit7
		sta     sound_liveness_tbl,x   // commit lock
		rts                           

* = $6A68
op_unlock_sound:
		// ------------------------------------------------------------
		// Opcode #$D3  Unlock sound
		// Clears bit7 of sound_liveness_tbl[index] to unlock the resource.
		// ------------------------------------------------------------
		jsr     script_read_byte	// A := sound index
		tax                         // X := index
		lda     sound_liveness_tbl,x   // A := attrs[index]
		and     #MASK_CLEAR_BIT7    // clear bit7
		sta     sound_liveness_tbl,x   // commit unlock
		rts                         

* = $6A75
op_lock_script:
		// ------------------------------------------------------------
		// Opcode #$33  Lock script
		// Sets bit7 of script_liveness_tbl[index] to lock the resource.
		// ------------------------------------------------------------
		jsr     script_read_byte	// A := script index
		tax                         // X := index
		lda     script_liveness_tbl,x  // A := attrs[index]
		ora     #MASK_BIT7          // set bit7
		sta     script_liveness_tbl,x  // commit lock
		rts                         

* = $6A82
op_unlock_script:
		// ------------------------------------------------------------
		// Opcode #$B3  Unlock script
		// Clears bit7 of script_liveness_tbl[index] to unlock the resource.
		// ------------------------------------------------------------
		jsr     script_read_byte	// A := script index
		tax                         // X := index
		lda     script_liveness_tbl,x  // A := attrs[index]
		and     #MASK_CLEAR_BIT7    // clear bit7
		sta     script_liveness_tbl,x  // commit unlock
		rts                         

* = $6A8F
op_lock_costume:
		// ------------------------------------------------------------
		// Opcode #$13  Lock costume
		// Sets bit7 of costume_liveness_tbl[index] to lock the resource.
		// ------------------------------------------------------------
		jsr     script_read_byte    // A := costume index
		tax                         // X := index
		lda     costume_liveness_tbl,x // A := attrs[index]
		ora     #MASK_BIT7          // set bit7
		sta     costume_liveness_tbl,x // commit lock
		rts                         

* = $6A9C
op_unlock_costume:
		// ------------------------------------------------------------
		// Opcode #$93  Unlock costume
		// Clears bit7 of costume_liveness_tbl[index] to unlock the resource.
		// ------------------------------------------------------------
		jsr     script_read_byte		// A := costume index
		tax                             // X := index
		lda     costume_liveness_tbl,x     // A := attrs[index]
		and     #MASK_CLEAR_BIT7        // clear bit7
		sta     costume_liveness_tbl,x     // commit unlock
		rts                             

* = $6AA9
op_lock_room:
		// ------------------------------------------------------------
		// Opcode #$4D  Lock room
		// Sets bit7 of room_liveness_tbl[index] to lock the resource.
		// ------------------------------------------------------------
		jsr     script_read_byte	// A := room index
		tax                         // X := index
		lda     room_liveness_tbl,x    // A := attrs[index]
		ora     #MASK_BIT7          // set bit7
		sta     room_liveness_tbl,x    // commit lock
		rts                         

* = $6AB6
op_unlock_room:
		// ------------------------------------------------------------
		// Opcode #$CD  Unlock room
		// Clears bit7 of room_liveness_tbl[index] to unlock the resource.
		// ------------------------------------------------------------
		jsr     script_read_byte    // A := room index
		tax                         // X := index
		lda     room_liveness_tbl,x    // A := attrs[index]
		and     #MASK_CLEAR_BIT7    // clear bit7
		sta     room_liveness_tbl,x    // commit unlock
		rts                         
