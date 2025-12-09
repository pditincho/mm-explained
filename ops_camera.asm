/*
================================================================================
  ops_camera
================================================================================

Purpose:
	Implements all camera-related opcodes for the script engine. These routines let
	scripts reposition or redirect the camera dynamically, either panning to a
	specific coordinate or locking onto an actor for continuous following.

Included opcodes and helpers:
	- op_cam_seek_to (#$32, #$B2): Enter pan mode and seek to a target X position.
	- op_set_camera_pan_goal (#$12, #$92): Enter pan mode and set a pan destination.
	- op_cam_follow_costume (#$52, #$D2): Begin following an actor by costume index.
	- script_cam_follow_costume: Helper that performs the actual follow, room load,
	  and script relocation checks.

Concept:
	The engine maintains camera mode, target position, and pan goals. These opcodes
	modify those parameters based on operands embedded in the script stream. 
	Pan	mode interpolates the camera toward a goal position. 
	Follow mode locks the camera to an actor’s movement across rooms.

Global data touched:
	- cam_mode, cam_target_pos, cam_pan_goal
	- camera_follow_target (via cam_follow_costume)
	- script operand reader (script_load_operand_bit7)
	- refresh_script_addresses_if_moved (post-room-load relocation fix)

| Opcode | Routine                  | Function                                 |
|--------|--------------------------|------------------------------------------|
| $32,$B2| op_cam_seek_to           | Set camera to pan and move to position   |
| $12,$92| op_set_camera_pan_goal   | Set camera to pan and define new goal    |
| $52,$D2| op_cam_follow_costume    | Follow actor by costume index            |
================================================================================
*/

#importonce
#import "globals.inc"
#import "constants.inc"
#import "script_engine.asm"
#import "script_primitives.asm"
#import "room_loader.asm"
#import "camera.asm"

/*
================================================================================
  op_cam_seek_to - Opcodes #$32, #$B2
================================================================================

Summary:
	Set camera seek target. Sets cam_mode to pan, reads the operand 
	position, and applies it via cam_seek_to.

Arguments:
	Operand in script stream supplies target position

Vars/State:
	A 						carries the target position into cam_seek_to.

Global Outputs:
	cam_mode                 Set to CAM_MODE_PAN.
	pan_camera_to            Updated by cam_seek_to.
	cam_target_pos           May be updated by cam_seek_to when snapping.

Description:
	* Force camera into pan-to-position mode.
	* Read target X position from the script operand.
	* Delegate to cam_seek_to to either snap or pan toward the target.
================================================================================
*/
* = $6801
op_cam_seek_to:
		// Set camera mode to PAN
		lda     #CAM_MODE_PAN                           
		sta     cam_mode

		// Resolve destination position
		jsr     script_load_operand_bit7       
		
		// Seek to position
		jsr     cam_seek_to                  
		rts                                    

/*
================================================================================
  op_set_camera_pan_goal - Opcodes #$12, #$92
================================================================================

Summary:
	Set camera to pan mode and write the target position read from the script operand.

Arguments:
	Operand in script stream supplies target position

Global Outputs:
	cam_mode       Set to CAM_MODE_PAN.
	cam_pan_goal   Updated with target pan position.

Description:
	* Force camera into pan-to-position mode.
	* Read target X position from the script operand.
	* Store it as the active pan destination so camera logic can approach it.
================================================================================
*/
* = $680D
op_set_camera_pan_goal:
		// Set camera mode to PAN
		lda     #CAM_MODE_PAN          
		sta     cam_mode

		// Resolve destination position
		jsr     script_load_operand_bit7      
		
		// Set pan goal
		sta     cam_pan_goal                  
		rts                                    
/*
================================================================================
  op_cam_follow_costume - Opcodes #$52, #$D2
================================================================================

Summary:
	Read costume index from the script and start following that
	actor with the camera.

Arguments:
	Operand in script stream supplies target costume

Global Outputs:
	Camera follow target      Updated indirectly by helper.
	Camera mode               Set to follow mode by helper as needed.

Description:
	* Reads the costume index operand from the script.
	* Falls through to the helper to center/follow that costume’s actor and
	  perform any required room transition and camera centering.
================================================================================
*/
* = $6819
op_cam_follow_costume:
		// Resolve costume index
		jsr     script_load_operand_bit7       
		// Fall through to script_cam_follow_costume

/*
================================================================================
  script_cam_follow_costume
================================================================================

Summary:
	Center and lock the camera on the actor associated with the given costume, then
	refresh script pointers if any resources moved.

Arguments:
	A  						  Costume index to follow.

Global Inputs:
	Costume→room map          To determine target room for the costume.
	Costume→actor map         To locate the actor for the costume.
	Actor X positions         For initial camera centering.

Global Outputs:
	Camera mode               Set to follow mode.
	Camera follow target      Updated to the target costume/actor.
	Current room              May be reloaded if the actor is in a different room.
	Script addresses          May be refreshed if relocation occurred.

Description:
	* Delegates to the follow helper to ensure correct room is active, center the
	  camera on the actor’s X position, and set follow mode.
	* Calls the relocation refresh to keep script addresses valid after any loads.
================================================================================
*/
* = $681C
script_cam_follow_costume:
		// Fix camera on costume
		jsr     cam_follow_costume
		// Refresh any relocated script pointers
		jsr     refresh_script_addresses_if_moved
		rts

/*
procedure op_cam_seek_to()
    // Force camera into pan mode
    cam_mode = CAM_MODE_PAN

    // Read target position operand from script
    target_pos = script_load_operand_bit7()

    // Delegate to core camera seek logic
    cam_seek_to(target_pos)

procedure op_set_camera_pan_goal()
    // Force camera into pan mode
    cam_mode = CAM_MODE_PAN

    // Read target position operand from script
    target_pos = script_load_operand_bit7()

    // Set target pan position for incremental camera updates
    cam_pan_goal = target_pos

procedure op_cam_follow_costume()
    // Read costume index operand from script
    costume_index = script_load_operand_bit7()

    // Conceptual fall-through
    script_cam_follow_costume(costume_index)

procedure script_cam_follow_costume(costume_index)
    // Lock camera onto actor associated with this costume
    cam_follow_costume(costume_index)

    // If resources or the room load repositioned scripts in memory,
    // refresh script instruction pointers
    refresh_script_addresses_if_moved()

*/