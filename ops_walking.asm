/*
================================================================================
  ops_walking
================================================================================

Purpose:
	Implements all movement- and placement-related opcodes for the script engine.
	These routines handle walking or teleporting costumes (actors) to target
	coordinates, other actors, or object positions, as well as assigning costumes to
	rooms. They cover both interactive (pathfinding) and instantaneous moves.

	- op_walk_costume_to_location         (#$1E, #$3E, #$5E, #$7E, #$9E, #$BE, #$DE, #$FE)
	- op_put_costume_at                   (#$01, #$21, #$41, #$61, #$81, #$A1, #$C1, #$E1)

Core concept:
	Each opcode reads one or more operands (costume index, X/Y coordinates, room or
	object indices) and uses them to either compute a destination via helper
	routines (pathfinding or object resolution) or directly assign new coordinates.
	Movement may be continuous (walk/path) or instantaneous (teleport).

Global data used:
	- active_costume / costume_room_idx[...]  current and target costume state
	- target_x, target_y, target_room         destination parameters
	- actor_for_costume[...]                   actor mapping per costume
	- inlined_positive_walking_offset,
	  inlined_negative_walking_offset          local movement offsets
	- resolve_object_resource					support routines for resource lookup,
	  set_costume_target						pathfinding, and relocation management.
	  place_costume_at_target
	  refresh_script_addresses_if_moved        
	  

|-----------------|-------------------------------------|-------------------------------------------------|
| Opcode Range    | Routine                             | Action Description                              |
|-----------------|-------------------------------------|-------------------------------------------------|
| $36,$B6,$76,$F6 | op_walk_costume_to_[mov/imm]_object | Walk to object (inventory/room)                 |
| $0E,$4E,$8E,$CE | op_put_costume_at_[mov/imm]_object  | Teleport to object (inventory/room)             |
| $15,$55,$95,$D5 | op_walk_costume_to_costume          | Walk one actor to another                       |
| $1E,$3E,$5E,$7E | op_walk_costume_to_location         | Walk actor to explicit coordinates              |
| $9E,$BE,$DE,$FE |                                     |                                                 |
| $01,$21,$41,$61 | op_put_costume_at                   | Teleport actor to explicit coordinates          |
| $81,$A1,$C1,$E1 |                                     |                                                 |
| $2D,$6D,$AD,$ED | op_put_costume_in_room              | Assign costume to room mapping                  |
|-----------------|-------------------------------------|-------------------------------------------------|

Notes:
	- All operand readers (script_load_operand_bit7/6/5) handle bit-coded operands.
	- Object access distinguishes movable (inventory) from immovable (room) modes.
	- Fallback coordinates are used when an object reference is missing or invalid.
	- Script relocation helpers ensure stable pointer integrity after resource loads.
================================================================================
*/
#importonce
#import "globals.inc"
#import "constants.inc"
#import "script_primitives.asm"

.label costume = $d0                     // Temporary ZP storage for current costume index
.const COSTUME_FALLBACK_X_DEST = $1E     // Default X coordinate when object lookup fails
.const COSTUME_FALLBACK_Y_DEST = COSTUME_DFLT_Y_DEST // Default Y coordinate when object lookup fails

/*
================================================================================
  op_walk_costume_to_immovable_object - Opcodes #$76, #$F6
  op_walk_costume_to_movable_object - Opcodes #$36, #$B6
================================================================================
Summary:
	Walk a costume to an object. Reads costume and object indices from the script,
	resolves the object either in-room (immovable) or inventory (movable), derives a
	target location near the object, and queues movement toward that target. If the
	object is not found, the opcode exits without side effects.

Global Inputs:
	Script stream               Supplies costume index and object index.
	Object resolution helpers   resolve_object_resource selects room vs inventory
	space based on the pushed selector value.

Global Outputs:
	active_costume              Set from the script operand.
	x_destination, y_destination (via helper)  Computed near the target object.
	(actor movement state)      Updated by set_costume_target to begin walking.

Description:
	* Selects object space:
		• immovable path pushes the “room object” selector.
		• movable path pushes the “inventory object” selector.
	* Reads costume index and object index from the script.
	* Calls resolve_object_resource using (X=object_lo, A=space selector).
	* If the object exists, computes a nearby destination and calls
	set_costume_target to path the costume there; otherwise returns.
================================================================================
*/
* = $6653
op_walk_costume_to_immovable_object:		
		lda     #OBJ_HI_IMMOVABLE              // Select object-mode: immovable (A := hi selector ≠ 0)
		bne     walk_costume_to_object_common  // always taken

* = $6657
op_walk_costume_to_movable_object:
		lda     #OBJ_HI_MOVABLE                // Select object-mode: movable (A := hi selector)

walk_costume_to_object_common:
		// Push mode for resolve_object_resource
		pha                                     

		// Resolve costume
		jsr     script_load_operand_bit7        
		sta     active_costume                  

		// Read object index lo
		jsr     script_read_byte                
		tax                                     

		// Restore object index hi
		pla                                     

		// Resolve object (X/A)
		jsr     resolve_object_resource                
		
		// Did we find it?		
		cpy     #OBJ_NOT_FOUND                            
		beq     owctio_exit                            // no → nothing to do

		// Compute x/y near object
		jsr     set_approach_point_for_object      
		
		// Pathfind and walk to (x,y)
		jsr     set_costume_target       

owctio_exit:
		rts
/*
================================================================================
  op_put_costume_at_movable_object - Opcodes #$0E, #$8E
  op_put_costume_at_immovable_object - Opcodes #$4E, #$CE
================================================================================
Summary:
	Place a costume at the position associated with an object.
	Reads costume and object indices, resolves the object in inventory context,
	derives a destination near it, and teleports the costume there. Uses a fallback
	(X,Y) if the object is not found.

Operands from the script stream:
	* Byte 1: costume index (read via bit7 path)
	* Byte 2: object index (low byte)

Global Inputs:
	active_costume             Overwritten from script to select the target costume.
	costume_room_idx[...]      Read to derive the destination room for the costume.

Global Outputs:
	target_x              	   Set to object-derived or fallback X.
	target_y              	   Set to object-derived or fallback Y.
	target_room           Set to the costume’s current room.
	(actor position state)     Updated by place_costume_at_target.
	(script addresses)         May be refreshed by handle_script_relocation.

Description:
	* Pushes the “movable/immovable” selector and reads costume and object indices.
	* Resolves the object in inventory space. If found, computes destination from it;
	  otherwise uses a fixed fallback coordinate pair.
	* Teleports the costume to (target_x, target_y) in target_room.
	* Refreshes script pointers if any resource loads caused relocation.
================================================================================
*/
* = $6673
op_put_costume_at_immovable_object:
		lda     #OBJ_HI_IMMOVABLE              // Select object-mode: immovable (A := hi selector ≠ 0)
		bne     put_costume_at_object_common   // Unconditional branch to common (BNE always taken since A ≠ 0)

* = $6677
op_put_costume_at_movable_object:
		lda     #OBJ_HI_MOVABLE                // Select object-mode: movable (A := hi selector)

put_costume_at_object_common:
		// Save selected mode on stack for resolve_object_resource later
		pha                                     

		// Resolve costume
		jsr     script_load_operand_bit7        
		sta     active_costume                  

		// Read object index lo
		jsr     script_read_byte                
		tax                                     

		// Restore object index hi
		pla                                     

		// Resolve object (X/A)
		jsr     resolve_object_resource
		
		// Did we find it?		
		cpy     #OBJ_NOT_FOUND                           
		beq     default_destination             // no → use fallback destination

		// Compute x/y based on object
		jsr     set_approach_point_for_object      
		jmp     move_costume

default_destination:
		// Object not found - use fallback coordinates
		lda     #COSTUME_FALLBACK_X_DEST
		sta     target_x
		lda     #COSTUME_FALLBACK_Y_DEST
		sta     target_y

move_costume:
		// Fetch current room of costume
		ldx     active_costume                  
		lda     costume_room_idx,x              
		sta     target_room                
	
		// Move costume to (x,y,target_room)
		jsr     place_costume_at_target 
		
		// Fix relocated script pointers
		jsr     refresh_script_addresses_if_moved        
		rts
/*
================================================================================
  op_walk_costume_to_costume - Opcodes #$15, #$55, #$95, #$D5
================================================================================
Summary:
	Walk the active costume to the position of another costume’s actor, applying a
	signed lateral offset. If either costume has no assigned actor, consume the
	appropriate script bytes and return without movement.

Operands read from the script stream:
	* Byte 1 (bit7 path): active costume index.
	* Byte 2 (bit6 path): other costume index.
	* Byte 3: unsigned offset; routine derives both +offset and −offset.

Global Inputs:
	actor_for_costume[...]    Maps costume index → actor slot or $80..$FF if none.
	(script stream)           Source for the two costume indices and offset.

Global Outputs:
	active_costume            Set to the active costume index.
	inlined_positive_walking_offset  Saved +offset from script.
	inlined_negative_walking_offset  Saved two’s-complement −offset.
	(destination state)       Programmed by set_approach_point_for_actor.
	(actor movement state)    Updated by set_costume_target.

Description:
	* Read active costume. If its actor is unassigned, skip a 2-byte offset and exit.
	* Read other costume. If its actor is unassigned, consume one data byte and exit.
	* Read the unsigned offset, store it, then compute and store its negative form.
	* Call set_approach_point_for_actor to compute the target near the other actor.
	* Invoke set_costume_target to pathfind and start movement.
	* Notes on script consumption:
		• Active actor missing → script_skip_offset (2 bytes).
		• Other actor missing  → script_read_byte (1 byte).
================================================================================
*/
* = $66AC
op_walk_costume_to_costume:		
		// Resolve costume index
		jsr     script_load_operand_bit7       
		sta     active_costume

		// Resolve actor
		tay                                     // Y := active costume
		lda     actor_for_costume,y             // A := active actor slot
		
		// Is the active actor assigned?
		bpl     active_actor_assigned
		
		// Not assigned → skip 2-bytes and return
		jsr     script_skip_offset
		rts

active_actor_assigned:
		// Resolve the other actor’s costume index
		jsr     script_load_operand_bit6       
		
		// Resolve other actor
		tax                                     // X := other costume
		lda     actor_for_costume,x             // A := other actor slot
		
		// Is the other actor assigned?
		bpl     both_actors_assigned
		
		// Not assigned → consume 1 data byte and return
		jsr     script_read_byte
		rts

both_actors_assigned:
		// Read destination walking offset (unsigned)
		jsr     script_read_byte                // A := +offset
		sta     actor_approach_x_offset_pos_byte // save +offset

		// Compute two’s-complement negative offset
		eor     #$ff
		clc
		adc     #$01
		sta     actor_approach_x_offset_neg_byte // save −offset

		// Program the destination and walk
		jsr     set_approach_point_for_actor
		jsr     set_costume_target
		rts
/*
================================================================================
  op_walk_costume_to_location - Opcodes #$1E, #$3E, #$5E, #$7E, #$9E, #$BE, #$DE, #$FE
================================================================================
Summary:
	Queue a walk for the specified costume to an explicit (X,Y) target using the
	engine’s pathfinding.

Global Inputs:
	(script stream)            Supplies costume index, X, and Y via operand readers.

Global Outputs:
	active_costume             Set to target costume index.
	target_x              	   Set from operand.
	target_y              	   Set from operand.
	(actor movement state)     Updated by set_costume_target.

Description:
	* Read costume index, X, and Y from the script.
	* Store X and Y as the current walk destination for that costume.
	* Invoke the pathfinder to move the actor toward the target.
================================================================================
*/
* = $66DE
op_walk_costume_to_location:		
		// Resolve costume index
		jsr     script_load_operand_bit7       
		sta     active_costume                 

		// Resolve X
		jsr     script_load_operand_bit6       
		sta     target_x

		// Resolve Y
		jsr     script_load_operand_bit5       
		sta     target_y

		// Pathfind and walk to (x,y)
		jsr     set_costume_target      
		rts
/*
================================================================================
  op_put_costume_at - Opcodes #$01, #$21, #$41, #$61, #$81, #$A1, #$C1, #$E1
================================================================================
Summary:
	Teleport a costume to an explicit (X,Y) within its current room, then fix any
	script pointer relocations.

Global Inputs:
	active_costume             Overwritten here with target costume index.
	costume_room_idx[...]      Source of the costume’s current room.
	(script stream)            Supplies costume index, X, and Y via operand readers.

Global Outputs:
	target_x              	   Set from operand.
	target_y              	   Set from operand.
	target_room           Set to the costume’s current room.
	(active position state)    Updated by place_costume_at_target.
	(script pointers)          May be refreshed by handle_script_relocation.

Description:
	* Read costume index, X, and Y from the script stream.
	* Look up the costume’s current room and use it as target_room.
	* Teleport the costume to (target_x, target_y) in that room.
	* Refresh script addresses if any resources moved during the operation.
================================================================================
*/
* = $66F4
op_put_costume_at:		
		// Resolve costume index
		jsr     script_load_operand_bit7       
		sta     active_costume                 

		// Resolve X
		jsr     script_load_operand_bit6       
		sta     target_x

		// Resolve Y
		jsr     script_load_operand_bit5       
		sta     target_y

		// Fetch current costume's room
		ldx     active_costume                 
		lda     costume_room_idx,x             
		sta     target_room               

		// Move costume to (x,y,target_room)
		jsr     place_costume_at_target 
		
		// Fix pointers if resources moved
		jsr     refresh_script_addresses_if_moved       		
		rts
/*
================================================================================
  op_put_costume_in_room - Opcodes #$2D, #$6D, #$AD, #$ED
================================================================================
Summary:
	Assigns a costume to a room. Reads costume index and room index from the script
	stream and writes the mapping into the costume→room table.

Global Inputs:
	Script stream  Supplies costume index (bit7 path) and room index (bit6 path)

Global Outputs:
	costume_room_idx[...]  Entry for the specified costume is updated to the room

Description:
	* Read costume index via script_load_operand_bit7 and stash it.
	* Read room index via script_load_operand_bit6.
	* Store the room index into costume_room_idx[costume].
================================================================================
*/
* = $6716
op_put_costume_in_room:
		// Resolve costume index
		jsr     script_load_operand_bit7
		sta     costume

		// Resolve room index
		jsr     script_load_operand_bit6

		// Map costume to room and exit
		ldx     costume
		sta     costume_room_idx,x
		rts

