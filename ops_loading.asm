/*
================================================================================
ops_loading
================================================================================

This file defines all script opcodes that load resources: rooms, scripts,
costumes, and sounds. Every routine follows the same idea:
	1. Read the resource index (bit7 = immediate vs variable).
	2. Make sure that resource is loaded into memory.
	3. Refresh any relocated pointers.

Room-related loaders add video and camera management, while others only manage
data and relocation.

Room loaders
	* Load room 
		  Reads room index and compares it with current_room.
		  If it’s the same, it skips the heavy load.
		  Otherwise it prepares the display, calls switch_to_room, refreshes script
		  relocation tables, and resets cam_current_pos to its default.

	* Load room with ego 
		  Checks if the kid is frozen; if so, it consumes its operands and stops the
		  task.
		  If not frozen, it reads two operands (entry object and target room), sets the
		  kid’s room field, loads that room, finds the entry object, moves the kid’s
		  actor there, sets the new camera position (aimed at the ego’s X coordinate),
		  resets the camera state, reinitializes the sentence UI, clears any forced
		  sentence triggers, and finally halts the current script to hand control back
		  to the main loop.

	* Load room for subresource
		  Loads the room into cache only (no visual effects, no camera moves). Used when
		  a script wants to preload its objects or sound banks.

Other loaders
	* Load costume
		  Reads a costume index, ensures the costume resource is loaded, then refreshes
		  relocation.

	* Load script
		Reads a script index, loads it if missing, then fixes relocation.

	* Load sound
		Reads sound index, loads sound data, then refreshes relocation.

Shared behavior
	* All use script_load_operand_bit7, which interprets the next operand byte as
	  either an immediate number or a variable lookup.
	* Every loader finishes by calling a “refresh relocated addresses” routine so
	  any scripts moved in memory still have valid pointers.
===========================================================================

+-----------------------------+--------------------+----------------------------------------------+
| Routine                     | Opcodes (hex)      | Purpose                                      |
+-----------------------------+--------------------+----------------------------------------------+
| op_load_room                | $25, $A5           | Load a room; refresh relocation; reset cam   |
| op_load_room_with_ego       | $E5                | Load room and move ego there; reset UI/cam   |
| op_load_room_for_subresource| $4A, $CA           | Preload room data without changing display   |
| op_load_costume             | $30, $B0           | Ensure costume resource resident             |
| op_load_script              | $4C, $CC           | Ensure script resource resident              |
| op_load_sound               | $0C                | Ensure sound resource resident               |
+-----------------------------+--------------------+----------------------------------------------+

*/

#importonce
#import "globals.inc"
#import "constants.inc"
#import "script_engine.asm"
#import "script_primitives.asm"
#import "ops_primitives.asm"
#import "room_loader.asm"

.label room_idx   = $d0      // Zero-page temp storage for current or target room index

/*
================================================================================
 op_load_room - Opcodes #$25, #$A5
================================================================================
Summary:
	Loads a new room if it differs from the current one. Prepares the video system,
	loads the room resource, refreshes relocated script addresses, and resets the
	camera to its default position.

Vars/State:
	room_idx 		Temporary storage for target room index.

Global Inputs:
	current_room	ID of the currently active room.

Global Outputs:
	cam_current_pos 	Reset to CAM_DEFAULT_POS after load.

Description:
	- Reads the target room index from the script operand (high bit from opcode).
	- If the target matches the current room, skips the load.
	- Otherwise:
		• Prepares video hardware for transition.
		• Loads the target room resource into memory.
		• Refreshes relocated script address tables.
		• Resets the camera position to a default value.
================================================================================
*/
* = $6312
op_load_room:
		// ------------------------------------------------------------
		// Read room index operand and save to ZP
		// Opcode bit7 supplies the high bit; store result in room_idx.
		// ------------------------------------------------------------
		jsr     script_load_operand_bit7    // A := room index (hi from bit7)
		sta     room_idx                    // save index in ZP $D0

		// ------------------------------------------------------------
		// Compare loaded room index against current_room
		// If equal, skip load and jump to room_loaded (no transition).
		// ------------------------------------------------------------
		cmp     current_room                // same room as current?
		beq     room_loaded                 // yes → bypass load

		// ------------------------------------------------------------
		// Prepare and load new room resources
		// Initializes video state, then loads the target room by index.
		// ------------------------------------------------------------
		jsr     prepare_video_for_new_room  // prepare display for transition
		ldx     room_idx                    // X := room index
		jsr     switch_to_room                   // load room resources

		// ------------------------------------------------------------
		// Finalize after room load or skip
		// Refresh relocated script pointers and reset camera position.
		// ------------------------------------------------------------
room_loaded:
		jsr     refresh_script_addresses_if_moved       
		lda     #CAM_DEFAULT_POS
		sta     cam_current_pos        		// set default camera position
		rts                                 // return to dispatcher
/*
================================================================================
 op_load_room_with_ego - Opcode #$E5
================================================================================
Summary:
	Loads a new room and positions the current kid (ego) at a designated entry
	object within that room. If the kid is frozen (e.g., captured), the opcode
	skips its operands and terminates the task early.

Vars/State:
	obj_idx_lo — Temporary storage for entry object index.
	obj_idx_hi — OBJ_HI_MOVABLE for object lookup routines.

Global Inputs:
	current_kid_idx         — Active kid index.
	actor_vars              — Status flags for all actors.
	current_room            — Active room identifier.
	actor_for_costume       — Mapping from costume to actor index.
	actor_x_pos             — X-coordinate table for actors.

Global Outputs:
	costume_room_idx[]      — Updated with new room for current kid.
	active_costume          — Set to current kid’s costume index.
	room_destination         — Updated to the new room id.
	cam_target_pos           — Updated to the ego actor’s X position.
	cam_current_pos          — Reset to CAM_DEFAULT_POS.
	forced_sentence_trigger  — Cleared after relocation.
	sentence UI/stack state  — Reset via init_sentence_ui_and_stack.

Description:
	- If the current kid is frozen, consumes the next two script operands and
	exits immediately.
	- Otherwise:
		• Prepares the video subsystem for a room transition.
		• Reads entry object and room indices from the script.
		• Assigns the room to the current kid and loads it.
		• Locates the specified entry object in the room.
		• If found, computes a destination, teleports the kid there,
		 and resets camera and UI states.
		• If not found, exits cleanly.
================================================================================
*/
* = $632D
op_load_room_with_ego:
		// ------------------------------------------------------------
		// Frozen guard for ego room load
		//
		// If current kid has ACTOR_IS_FROZEN set, skip [object, room]
		// operands and terminate the task; otherwise continue load path.
		// ------------------------------------------------------------
        ldx     current_kid_idx               // X := index of active kid (ego)
        lda     actor_vars,x                  // A := status flags for that kid
        and     #ACTOR_IS_FROZEN              // isolate FROZEN bit; Z=1 → not frozen, Z=0 → frozen
        beq     start_load_path               // not frozen → continue normal load path

		// Kid frozen - terminate task immediately
        jsr     script_skip_offset            // kid frozen → consume 2 operand bytes [entry_object, room]
        jmp     op_stop_current_task          // terminate this task immediately

		// ------------------------------------------------------------
		// Prep video, read operands, set kid’s target room
		//
		// Reads entry object index (pushes to stack), then room index,
		// and records it into costume_room_idx[current_kid].
		// ------------------------------------------------------------
start_load_path:
        jsr     prepare_video_for_new_room     // Prep video/display state for transition
        jsr     script_read_byte               // Read entry object index from script
        pha                                    // Save entry object index on stack for later lookup
		
        jsr     script_read_byte               // Read target room index from script 
        ldx     current_kid_idx                // X := current kid index for per-kid tables
        sta     costume_room_idx,x             // Record kid’s target room; used by loader and state logic

		// ------------------------------------------------------------
		// Load destination room and prepare entry-object lookup
		//
		// X := room index for loader; switch_to_room updates current_room.
		// Restore entry object id before lookup.
		// ------------------------------------------------------------
        tax                                     // X := room index (for loader dispatch)
        jsr     switch_to_room                       // Load destination room resources; updates current_room

        pla                                     // Pop saved entry object index from stack → A
        sta     obj_idx_lo                    	// obj_idx_lo := entry object id		
        lda     #OBJ_HI_MOVABLE                           
        sta     obj_idx_hi                    

		// ------------------------------------------------------------
		// Locate entry object in destination room and branch on result
		//
		// Search returns Y := index or OBJ_NOT_FOUND. If found, continue to
		// placement path; otherwise terminate this task.
		// ------------------------------------------------------------
        jsr     dispatch_to_room_search         // Find object in newly loaded room; Y := index or OBJ_NOT_FOUND
        cpy     #OBJ_NOT_FOUND                  // Z=1 if not found, Z=0 if found
        bne     entry_object_found              // found → proceed to set destination
		
        jmp     stop_task_not_found             // not found → terminate task cleanly

		// ------------------------------------------------------------
		// Set destination from entry object and teleport ego
		//
		// Resolve target coords, select active costume, set dest room,
		// then warp costume to computed destination.
		// ------------------------------------------------------------
entry_object_found:
        jsr     set_actor_destination_to_object // Compute target coords from entry object; updates destination vars

		// Set current kid as the active costume for movement/teleport
        lda     current_kid_idx                 
        sta     active_costume                  

		// Record current room as destination room for placement routine
        lda     current_room                    
        sta     room_destination                

		// Instantly place active costume at computed destination
        jsr     teleport_costume_to_destination 

		// ------------------------------------------------------------
		// Align camera to ego and reset position
		//
		// Set cam_target_pos to the ego actor’s X, then reset current
		// camera position to the default starting value.
		// ------------------------------------------------------------
        ldx     current_kid_idx               // X := costume id of the active kid
        lda     actor_for_costume,x           // A := actor index mapped from that costume
        tax                                   // X := actor index
        lda     actor_x_pos,x                 // A := actor’s current X position
        sta     cam_target_pos                // camera target X := actor X

        lda     #CAM_DEFAULT_POS              // A := default camera position constant
        sta     cam_current_pos               // Reset current camera position to default

		// ------------------------------------------------------------
		// Reset sentence UI and clear forced-trigger
		//
		// Reinitialize the sentence UI/stack, then disable any pending
		// forced sentence trigger before terminating the task.
		// ------------------------------------------------------------
        jsr     init_sentence_ui_and_stack    // Reinitialize sentence UI and token stack
        lda     #FALSE                        
        sta     forced_sentence_trigger       // Disable any pending forced sentence trigger

stop_task_not_found:
        jmp     op_stop_current_task          // Terminate this task and return control to dispatcher
/*
================================================================================
  op_load_costume - Opcodes #$30, #$B0
================================================================================
Summary:
	Ensures the costume resource specified by the script operand is resident in
	memory, then refreshes relocated script addresses.

Arguments:
	Reads a one-byte costume index from the script stream via
	script_load_operand_bit7 (opcode bit7 provides the high bit).

Global Outputs:
	Costume resource cache — specified costume is ensured resident.
	Script address tables — refreshed if any pointers moved.

Description:
	- Read costume index from script operand.
	- Ensure the corresponding costume resource is loaded/resident.
	- Refresh script address tables in case resource movement changed pointers.
================================================================================
*/
* = $6A2D
op_load_costume:
		// ------------------------------------------------------------
		// Read costume index (hi from opcode bit7), load, then relocate.
		// ------------------------------------------------------------
		jsr     script_load_operand_bit7    		// A := costume index
		tax                                     	// X := index
		jsr     rsrc_cache_costume        // load costume resource
		jsr     refresh_script_addresses_if_moved   // fix relocated script pointers
		rts                                     
/*
================================================================================
  op_load_room_for_subresource - Opcodes #$4A, #$CA
================================================================================
Summary:
	Preloads a room resource for data access or subresource extraction. No screen
	transition or camera work is performed.

Arguments:
	Reads 1-byte room index via script_load_operand_bit7.

Global Outputs:
	Resource cache — target room ensured resident for subsequent queries.
	Script pointers — refreshed if any addresses moved.

Description:
	- Read room index from script operand.
	- Load the room resource so its subresources can be queried.
	- Refresh relocated script addresses after the load.
	- Does not prepare video or adjust camera/UI.
================================================================================
*/
* = $6A38
op_load_room_for_subresource:
		// ------------------------------------------------------------
		// Preloads a room resource into memory for data access or
		// subresource extraction. Does not trigger a room transition.
		// ------------------------------------------------------------
		jsr     script_load_operand_bit7       		// A := room index 
		tax                                     	// X := room index
		jsr     cache_room   				// load room into cache
		jsr     refresh_script_addresses_if_moved   // adjust relocated script pointers
		rts                                     
/*
================================================================================
  op_load_script - Opcodes #$4C, #$CC
================================================================================
Summary:
	Ensures the script resource referenced by the opcode operand is resident in
	memory, then refreshes relocated script addresses that may have moved.

Arguments:
	Reads a 1-byte script index from the script stream via
	script_load_operand_bit7 (opcode bit7 supplies the high bit).

Global Outputs:
	Script cache — specified script ensured resident.
	Script address tables — refreshed if any pointers moved.

Description:
	- Opcode #$4C (immediate) and #$CC (variable) supply the target script index.
	- Load/ensure the script resource is resident.
	- Refresh relocated script addresses after potential cache movement.
================================================================================
*/
* = $6A43
op_load_script:
		// ------------------------------------------------------------
		// Ensure the script resource is resident, then relocate pointers.
		// ------------------------------------------------------------
		jsr     script_load_operand_bit7       		// A := script index
		jsr     rsrc_cache_script    		// load/ensure resident
		jsr     refresh_script_addresses_if_moved   // fix relocated script pointers
		rts                                     
/*
================================================================================
  op_load_sound - Opcode #$0C
================================================================================
Summary:
	Ensures the sound resource specified by the opcode operand is resident in
	memory, then refreshes relocated script addresses that may have moved.

Arguments:
	Reads a 1-byte sound index from the script via script_load_operand_bit7
	(opcode bit7 supplies the high bit).

Global Outputs:
	Sound resource cache — target sound ensured resident.
	Script address tables — refreshed if any pointers moved.

Description:
	- Read sound index from the script operand.
	- Ensure the corresponding sound resource is loaded/resident.
	- Refresh relocated script addresses in case cache movement changed pointers.
================================================================================
*/
* = $6A4D
op_load_sound:
		// ------------------------------------------------------------
		// Read sound index (hi from opcode bit7), load, then relocate.
		// ------------------------------------------------------------
		jsr     script_load_operand_bit7       		// A := sound index
		jsr     rsrc_cache_sound          // load sound resource
		jsr     refresh_script_addresses_if_moved   // fix relocated script pointers
		rts                                     
