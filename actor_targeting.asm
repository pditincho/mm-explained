/*
================================================================================
Target and approach system
================================================================================

This module translates high–level “go there / face that” requests into concrete
per–actor targets that are safe for the walkbox/pathing system. It also
handles instant placements and basic facing selection so actors look and move
believably toward their targets.

High–level responsibilities
    • Facing toward targets:
        - Compute a standing facing clip toward a target actor or object.
        - Apply the facing clip to the moving actor.
        - For actor–actor interactions, optionally mirror the facing on the
          target actor so both characters look at each other.

    • Building approach points:
        - For actor targets:
            · Choose which side to stand on (left/right) based on relative X.
            · Apply a signed ±X offset around the target actor to build a
              “stand next to” target.
        - For object targets:
            · Use an explicit per–object approach point when defined.
            · Otherwise, aim at the geometric center of the object’s bounding
              box (width/height tables).
        - In all cases, snap the chosen point onto a valid walkbox so later
          pathing cannot drive the actor into walls or invalid space.

    • Actor vs object dispatch:
        - set_approach_point examines the target type and routes to the
          actor or object approach builder.
        - Saves the caller’s Y register, installs the default actor approach
          offsets, and arranges register usage so handlers can be reused from
          different call sites.

    • Walkbox snapping for targets:
        - snap_target_to_walkbox takes the high–level (target_x, target_y)
          pair and clamps/projects it into the current room’s walkbox set via
          snap_coords_to_walkbox.
        - snap_actor_target performs the same operation on an actor’s own
          actor_target_x/actor_target_y slots using the costume’s room id,
          leaving the target unchanged if no walkbox can be found.

    • Staging movement for on–screen actors:
        - set_costume_target resolves a costume to its actor, distinguishes
          off–screen vs on–screen actors, and:
            · for off–screen: records costume_target_x/y only (no pathing).
            · for on–screen: snaps to walkboxes, writes actor_target_x/y, and
              calls stage_actor_path_to_target so the path/waypoint system
              rebuilds a route from the current walkbox to the new target.

    • Instant placement of actors and costumes:
        - place_costume_at_target copies target_room and target_x/y into the
          costume’s persistent state.
        - If an actor is currently assigned:
            · Places the actor at that position (unless already there),
              sets a default “down” facing, then detaches the actor.
        - If the target room is the current room (and nonzero), attempts
          to assign a free actor to the costume so it becomes visible and can
          be controlled.

Data flow and integration points
    • Inputs:
        - High–level commands set target_obj_hi/lo, target_entity, target_room,
          target_x/target_y, and active_costume.
        - Actor/object tables (actor_for_costume, obj_* tables) provide
          geometry and mapping information.
    • Outputs:
        - actor_target_x/y and costume_target_x/y hold the resolved/safe
          targets.
        - actor and costume_room_idx are updated to reflect where movement or
          placement will occur.
        - Pathing is driven indirectly by stage_actor_path_to_target, which
          consumes the snapped actor_target_x/y and produces walkbox paths and
          waypoints in the pathing and walkbox traversal modules.

In effect, this file is the “target front–end”: it turns abstract targets
(“walk to that actor”, “approach this object”, “place in that room”) into
clamped world coordinates and staged path updates that the lower–level motion
and waypoint systems can execute safely.
*/
#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "actor_animation.asm"
#import "walkbox_waypoint_planner.asm"
#import "actor.asm"
#import "actor_walkbox_traversal.asm"

.label target_obj_hi               		= $FE50  // target entity type: OBJ_TYPE_COSTUME for costume/actor, other values = object types
.label target_obj_lo               		= $FE4F  // target entity index: costume id (for actors) or object index
.label target_entity               		= $FE51  // target object index (0..$FE), or $FF when no object target is active
.label saved_facing_clip_id            	= $0FC8   // mover’s last selected facing clip id
.label saved_param_y                   	= $0FC9   // dispatcher-saved Y param (restored on return)
.label snap_probe_x                    	= $FE72   // snap input X (pixels) for walkbox clamp
.label snap_probe_y                    	= $FE73   // snap input Y (pixels) for walkbox clamp
.label actor_approach_x_offset_pos_byte = $0F65   // writable LDA #imm operand for +X approach offset
.label actor_approach_x_offset_neg_byte = $0F6A   // writable LDA #imm operand for −X approach offset
.label target_room               		= $FDEA   // target room id for the placement or walk command; 0 means none
.label unused_var_1                   	= $26F7   // debug storage: last written target X coordinate (pixels)
.label unused_var_2                   	= $26F8   // debug storage: last written target Y coordinate (pixels)

.const CLIP_LR_BIT                      = $01     // clip id bit0 toggles left/right
.const LOOP_ONE_SHOT                    = $00     // play clip once (no loop)
.const ACTOR_APPROACH_X_OFFSET_POS     	= $04     // +4 px lateral offset when approaching actor
.const ACTOR_APPROACH_X_OFFSET_NEG     	= $FC     // −4 px lateral offset when approaching actor

/*
================================================================================
  face_toward_target
================================================================================
Summary
    Select and apply a facing clip so the current kid faces its target.
    For actor targets (non-plant costumes), also mirror the facing on the
    target so both actors face each other.

Global Inputs
    target_obj_hi             target entity type (OBJ_TYPE_COSTUME = actor).
    target_obj_lo             target costume id for actor targets.
    current_kid_idx           Costume index of the player-controlled kid.

Global Outputs
    saved_facing_clip_id      Updated with the current kid’s facing clip id.
    target_clip_id            Set to the clip applied to the selected costume.
    clip_loop_cnt             Set to LOOP_ONE_SHOT (play once).

Description
    - Call select_facing_clip_for_target to compute the clip that makes the
      current kid face its target.
    - Apply that clip as a one-shot to current_kid_idx.
    - If the target is not a costume, stop there.
    - If the target is a costume other than the plant:
        - Flip the left/right bit in the saved clip id.
        - Apply the mirrored clip as a one-shot to the target costume so
          the two actors face one another.
================================================================================
*/
* = $0D49
face_toward_target:
        // ------------------------------------------------------------
        // Get facing clip toward target and apply to current_kid_idx
        // ------------------------------------------------------------
        jsr     select_facing_clip_for_target   	
        sta     saved_facing_clip_id                
        sta     target_clip_id                      

		// Loop clip once
        lda     #LOOP_ONE_SHOT                             
        sta     clip_loop_cnt

		// Apply the facing clip
        lda     current_kid_idx 
        sta     active_costume
        jsr     assign_clip_to_costume                        	   

        // ------------------------------------------------------------
        // If target is not a costume, exit
        // ------------------------------------------------------------
        lda     target_obj_hi
        cmp     #OBJ_TYPE_COSTUME          
        bne     done_1

        // ------------------------------------------------------------
        // Target is a costume
        // ------------------------------------------------------------
		// Save the lo byte as the active costume index
        lda     target_obj_lo                         
        sta     active_costume
		
		// If it's the plant, exit
        cmp     #COSTUME_ID_PLANT                          
        beq     done_1

        // ------------------------------------------------------------
        // When walking to another costume, make the target costume face the kid
		// Horizontally mirror the current kid's clip
        // ------------------------------------------------------------
        lda     saved_facing_clip_id
		
		// Flip bit0 of the base clip id (flip horizontally)
        eor     #CLIP_LR_BIT                               
        sta     target_clip_id

		// Loop once
        lda     #LOOP_ONE_SHOT                             
        sta     clip_loop_cnt
		
		// Apply clip to target costume
        jsr     assign_clip_to_costume

done_1:
        rts
/*
================================================================================
  select_facing_clip_for_target
================================================================================
Summary
    Compute the standing facing clip id the current kid should use when
    preparing to walk toward the configured target (actor or object).

Global Inputs
    target_obj_hi             target entity type (OBJ_TYPE_COSTUME = actor).
    target_obj_lo             target costume id for actor targets.
    target_entity             target object index or $FF if none.
    current_kid_idx           Costume index of the player-controlled kid.
    actor_for_costume[]       Map from costume id → actor index.
    actor_pos_x[]             Actor X positions in pixels.
    obj_has_explicit_approach[]  Nonzero → object defines its own approach clip.

Returns
    A   CLIP_STAND_RIGHT      If target is an actor and kid_x ≤ target_x.
        CLIP_STAND_LEFT       If target is an actor and kid_x  > target_x.
        CLIP_STAND_DOWN       If target is an object without explicit clip or
                              target_entity == $FF.
        obj_has_explicit_approach[object]
                              If nonzero explicit approach clip is provided.

Description
    - If the target is a costume/actor:
        - Resolve the target actor’s X coordinate and store it in target_x.
        - Resolve the current kid’s actor index and X coordinate.
        - If kid_x ≤ target_x, choose CLIP_STAND_RIGHT; otherwise CLIP_STAND_LEFT.
		
    - If the target is an object:
        - If target_entity == $FF, return CLIP_STAND_DOWN.
        - Otherwise, if obj_has_explicit_approach[object] is nonzero, return
          that clip id; if it is zero, return CLIP_STAND_DOWN.
================================================================================
*/
* = $0D82
select_facing_clip_for_target:
        // ------------------------------------------------------------
        // Branch on target type: costume vs object
        // ------------------------------------------------------------
        lda     target_obj_hi
        cmp     #OBJ_TYPE_COSTUME
        bne     target_is_object

		// --------------------------------------------------------------------
		// Target is a costume, so face towards it
		// Face left/right based on their relative positions on the X axis
		// --------------------------------------------------------------------
		// Resolve target's X position
        ldx     target_obj_lo			// costume ID
        lda     actor_for_costume,x		// actor for that costume
        tax
        lda     actor_pos_x,x			// X coordinate for that actor
        sta     target_x

        // Resolve current kid’s X position
        ldx     current_kid_idx
        lda     actor_for_costume,x
        tax
        lda     actor_pos_x,x

        // If kid_x ≤ target_x → face right; else left
        cmp     target_x
        bcc     return_stand_right
        beq     return_stand_right

        lda     #CLIP_STAND_LEFT
        jmp     sfcfd_exit                                  

return_stand_right:
        lda     #CLIP_STAND_RIGHT
        jmp     sfcfd_exit                                  

target_is_object:
		// --------------------------------------------------------------------
		// Target is an object
		// --------------------------------------------------------------------
        ldy     target_entity
		// Object unknown? If so, face down
        cpy     #$FF
        beq     return_stand_down          

		// If the object has an explicit approach point, don't use the default and exit
        lda     obj_has_explicit_approach,y       
		bne		sfcfd_exit_2
        lda     #CLIP_STAND_DOWN
sfcfd_exit_2:		
        jmp     sfcfd_exit                            

return_stand_down:
		//Otherwise, use the default of "facing down"
        lda     #CLIP_STAND_DOWN
sfcfd_exit:
        rts
/*
================================================================================
  set_approach_point
================================================================================
Summary
    Dispatch target-setup to either the actor or object handler based on
    the entity type in A. For actor targets, install ±X approach offsets and
    rebind Y to the current kid before falling through to the actor handler.

Arguments
    A   target entity type:
        - OBJ_TYPE_COSTUME → costume target.
        - Other            → object target.
		
    X   If A = OBJ_TYPE_COSTUME:
            target costume id.
        Otherwise:
            Moving costume id for the object approach.
			
    Y   For actor targets:
            Preserved only in saved_param_y (not used for geometry).
        For object targets:
            target object index.

Global Inputs
    current_kid_idx            Costume index of the player-controlled kid.

Global Outputs
    Y                          Rebound to current_kid_idx on the actor path
                               just before entering set_approach_point_for_actor.

Description
    - Save the incoming Y into saved_param_y so the snap code can restore it.
    - Test A against OBJ_TYPE_COSTUME:
        - If not equal, jump to set_approach_point_for_object (object path).
        - If equal:
            - Write ACTOR_APPROACH_X_OFFSET_POS/NEG into the two SMC operand
              bytes used for horizontal offsets.
            - Load Y with current_kid_idx and fall through to
              set_approach_point_for_actor.
================================================================================
*/
* = $0F32
set_approach_point:
		sty     saved_param_y

		// Decide route: object vs actor based on the value of A
		cmp     #OBJ_TYPE_COSTUME
		bne     set_approach_point_for_object	// If object, go to the object's routine

		// Actor target: set approach offsets
		lda     #ACTOR_APPROACH_X_OFFSET_POS
		sta     actor_approach_x_offset_pos_byte
		lda     #ACTOR_APPROACH_X_OFFSET_NEG
		sta     actor_approach_x_offset_neg_byte

		// Use current_kid_idx as working Y, then fall through to actor handler
		ldy     current_kid_idx
		
		// fall through to set_approach_point_for_actor
/*
================================================================================
  set_approach_point_for_actor
================================================================================
Summary
    Compute an approach target near a target actor. Place the mover to
    the left or right of the target based on relative X, then snap that point
    to a valid walkbox.

Arguments
    X   target costume id (target actor’s costume).
    Y   Moving costume id
        - Normally current_kid_idx when entered via set_approach_point.
        - May be any valid costume id if called directly.

Global Inputs
    actor_for_costume[]       Map costume id → actor index.
    actor_pos_x[]             Actor X positions in pixels.
    actor_pos_y[]             Actor Y positions in pixels.

Global Outputs
    target_x                  Base target X for the mover (pre-snap).
    target_y                  Base target Y for the mover (pre-snap).

Description
    - Convert the moving and target costume ids into actor indices:
        - Y → mover actor index.
        - X → target actor index.
    - Copy target actor’s (pos_x, pos_y) into (target_x, target_y).
    - Compare mover_x against target_x:
        - If mover_x ≤ target_x, choose the “left-of-target” offset (NEG).
        - Otherwise choose the “right-of-target” offset (POS).
    - Add the chosen signed horizontal offset to target_x.
    - Jump to snap_target_to_walkbox so the target is clamped to a
      valid walkbox location in the current room.
================================================================================
*/
* = $0F46
set_approach_point_for_actor:
		// ------------------------------------------------------------
		// Map costumes to actor indices into Y (mover) and X (target)
		// ------------------------------------------------------------
		lda     actor_for_costume,y
		tay
		lda     actor_for_costume,x
		tax

		// ------------------------------------------------------------
		// Copy target's current position into target_y and target_x
		// ------------------------------------------------------------
		lda     actor_pos_y,x
		sta     target_y
		lda     actor_pos_x,x
		sta     target_x

		// ------------------------------------------------------------
		// Determine approach side using mover_x vs target_x
		//
		//   mover_x ≤ target_x → stand to the left  (negative offset)
		//   otherwise        	→ stand to the right (positive offset)
		// ------------------------------------------------------------
		lda     actor_pos_x,y
		cmp     target_x
		bcc     approach_from_left_or_equal
		beq     approach_from_left_or_equal

		// ------------------------------------------------------------
		// Approaching from the right: apply a positive offset
		// ------------------------------------------------------------
		lda     #ACTOR_APPROACH_X_OFFSET_POS
		jmp     add_offset_to_target

approach_from_left_or_equal:
		// ------------------------------------------------------------
		// Approaching from the left: apply a negative offset
		// ------------------------------------------------------------
		lda     #ACTOR_APPROACH_X_OFFSET_NEG

add_offset_to_target:
		// ------------------------------------------------------------
		// Apply the offset
		// ------------------------------------------------------------
		clc
		adc     target_x
		sta     target_x

		// Snap to a walkbox
		jmp     snap_target_to_walkbox
/*
================================================================================
  set_approach_point_for_object
================================================================================
Summary
    Compute a target point for approaching an object. Use explicit
    approach coordinates when provided; otherwise target the geometric center
    of the object’s bounding box, then snap to a walkbox.

Arguments
    Y   target object index.

Global Inputs
    obj_has_explicit_approach[]  Nonzero → object defines explicit approach.
    obj_left_col_tbl[]           Object left edge in pixels.
    obj_top_row_tbl[]            Object top edge in 4-pixel rows.
    obj_width_tbl[]              Object width in pixels.
    obj_height_tbl[]             Object height in 4-pixel rows.
    obj_approach_x[]             Explicit approach X in pixels.
    obj_approach_y[]             Explicit approach Y in 4-pixel rows.

Global Outputs
    target_x                     Base target X for the mover (pre-snap).
    target_y                     Base target Y for the mover (pre-snap).

Description
    - If obj_has_explicit_approach[Y] is nonzero:
        - Load obj_approach_x[Y] into target_x.
        - Load obj_approach_y[Y], scale by 4 (two ASLs) into target_y.
        - Fall through to snap_target_to_walkbox.
    - Otherwise, compute the center of the object’s box:
        - target_x := obj_left + (obj_width >> 1).
        - target_y := (obj_top + (obj_height >> 1)) << 2 (row-based Y → pixels).
        - Jump to snap_target_to_walkbox.
    - In both cases, the snapper clamps the target to a valid walkbox
      position in the current room.
================================================================================
*/
* = $0F75
set_approach_point_for_object:
		// ------------------------------------------------------------
		// Check if the object has an explicit approach point
		// ------------------------------------------------------------
		lda     obj_has_explicit_approach,y
		bne     explicit_approach_point

		// ------------------------------------------------------------
		// No explicit approach: compute center of the object box
		//
		//   target_x := obj_left + (obj_width >> 1)
		//   target_y := (obj_top + (obj_height >> 1)) << 2 (scaled)
		// ------------------------------------------------------------
		lda     obj_width_tbl,y
		lsr     
		clc
		adc     obj_left_col_tbl,y
		sta     target_x

		lda     obj_height_tbl,y
		lsr     
		clc
		adc     obj_top_row_tbl,y
		asl     
		asl     
		sta     target_y
		
		// Snap to a walkbox
		jmp     snap_target_to_walkbox


explicit_approach_point:
		// ------------------------------------------------------------
		// Explicit approach point: use it
		// ------------------------------------------------------------
		lda     obj_approach_x,y
		sta     target_x
		lda     obj_approach_y,y
		//   Scale Y axis accordingly
		asl     
		asl     
		sta     target_y
		// fall through to snap_target_to_walkbox
/*
================================================================================
  snap_target_to_walkbox
================================================================================
Summary
    Snap the global target (target_x, target_y) to the nearest valid
    walkbox point for the current room, restore the caller’s Y, and return.

Global Inputs
    target_x                   Tentative target X (pixels).
    target_y                   Tentative target Y (pixels).
    current_room               Active room id whose walkboxes will be used.

Global Outputs
    target_x                   Updated to snapped X if a walkbox is found.
    target_y                   Updated to snapped Y if a walkbox is found.

Description
    - Copy target_x/target_y into snap_probe_x/snap_probe_y.
    - Set walkbox_room equal to current_room.
    - Call snap_coords_to_walkbox to clamp/project the probe coords to a
      valid walkbox point.
    - Copy the snapped probe coords back into target_x/target_y.
    - Restore Y from saved_param_y and return.
================================================================================
*/
* = $0FA3
snap_target_to_walkbox:
        // Seed snap input coordinates from the actor’s target
		lda     target_x
		sta     snap_probe_x
		lda     target_y
		sta     snap_probe_y

        // Seed walkbox room as the current room
		lda     current_room
		sta     walkbox_room

        // Snap to nearest valid walkbox point
		jsr     snap_coords_to_walkbox

        // Write back snapped coordinates
		lda     snap_probe_x
		sta     target_x
		lda     snap_probe_y
		sta     target_y

		// Restore caller’s Y and return
		ldy     saved_param_y
		rts			
/*
================================================================================
  snap_actor_target
================================================================================
Summary
    Snap the active actor’s requested target to the nearest valid walkbox
    point in the actor’s room, updating the actor’s target only when a
    walkbox is found.

Global Inputs
    actor                      Index of the actor whose target is being snapped.
    active_costume             Costume currently owned by this logic.
    actor_target_x[]           Requested X target for each actor.
    actor_target_y[]           Requested Y target for each actor.
    costume_room_idx[]         Room id per costume.
    actor_for_costume[]        Map costume id → actor index.

Global Outputs
    actor_target_x[actor]      Updated to snapped X if a walkbox is found.
    actor_target_y[actor]      Updated to snapped Y if a walkbox is found.

Returns
    A   nearest_box_idx        Index of nearest walkbox, or NO_BOX_FOUND if
                               no valid walkbox exists.

Description
    - Use actor to load the actor’s current requested target into
      test_x/test_y.
    - Use active_costume to set walkbox_room from costume_room_idx.
    - Call snap_coords_to_walkbox, which returns nearest_box_idx in A.
    - If nearest_box_idx == NO_BOX_FOUND, return without changing the
      actor’s target.
    - Otherwise:
        - Re-resolve the actor index from active_costume via actor_for_costume.
        - Copy test_x/test_y back into actor_target_x/actor_target_y for that
          actor and return with A still holding nearest_box_idx.
================================================================================
*/
* = $1A68
snap_actor_target:
        // Seed snap input coordinates from the actor’s target
        ldx     actor
        lda     actor_target_x,x
        sta     test_x
        lda     actor_target_y,x
        sta     test_y

        // Resolve walkbox room as the current room for the active costume
        ldx     active_costume
        lda     costume_room_idx,x
        sta     walkbox_room

        // Snap to nearest valid walkbox point
        jsr     snap_coords_to_walkbox

        // No walkbox found? If so, exit
        cmp     #NO_BOX_FOUND
        bne     apply_snapped_target
        rts

apply_snapped_target:
		// Resolve actor for costume
        ldx     active_costume
        lda     actor_for_costume,x
        tax
		
        // Write back snapped coordinates
        lda     test_x
        sta     actor_target_x,x
        lda     test_y
        sta     actor_target_y,x
        rts		
/*
================================================================================
  set_costume_target
================================================================================
Summary
    Route a high-level target for the active costume either into
    off-screen costume target slots or on-screen actor pathing. On-screen
    actors have their target snapped to walkboxes and staged for path
    traversal.

Global Inputs
    active_costume             Index of the costume being targeted.
    actor_for_costume[]        Map costume id → actor index
    target_x/y				   Requested target coordinates in pixels.

Global Outputs
    costume_target_x/y[]         Updated for off-screen costumes.
    actor_target_x/y[]           Snapped target X for on-screen actor.

Description
    - Resolve the actor index for active_costume via actor_for_costume:
        - If the index is negative:
            - Treat the costume as off-screen.
            - Copy target_x/target_y into costume_target_x/y for that costume.
            - Return without invoking pathing.
        - If the index is non-negative:
            - Treat as an on-screen actor.
            - Store X (actor index) into actor.
            - Call snap_coords_to_walkbox to clamp target_x/target_y to a
              valid walkbox position.
            - Copy target_x/target_y into actor_target_x/y for that actor.
            - Tail-call stage_actor_path_to_target to build or refresh the
              actor’s path toward the snapped target.
================================================================================
*/
* = $1C1F
set_costume_target:
		// Resolve actor for active costume
        ldx     active_costume                 
        lda     actor_for_costume,x            
		
		// Actor assigned? 
        bpl     actor_onscreen                 	// If actor index ≥ 0 → on-screen actor

actor_offscreen:
		// ------------------------------------------------------------
		// Off-screen actor: store costume target for later use
		// ------------------------------------------------------------
        lda     target_x
        sta     costume_target_x,x         		
        lda     target_y
        sta     costume_target_y,x              
        rts                                     

actor_onscreen:
		// ------------------------------------------------------------
		// On-screen actor: snap to walkbox, set _actor_ target and stage path
		// ------------------------------------------------------------
        tax                             		
        stx     actor                           

        // Clamp target to a walkbox
		jsr     snap_coords_to_walkbox          

		// Store actor's target coordinates
        lda     target_x
        sta     actor_target_x,x                
        lda     target_y
        sta     actor_target_y,x                

        // Tail-call path staging routine		
		jmp     stage_actor_path_to_target      
/*
================================================================================
  place_costume_at_target
================================================================================
Summary
    Place the active costume at (target_x, target_y) in target_room. If an
    actor is currently assigned, move it there, set a default facing, then
    detach. Optionally reassign an actor if the target room is the current
    room and nonzero.

Global Inputs
    active_costume             Costume being placed.
    target_room                Room id of the target (0 = “no room”).
    target_x, target_y         target coordinates in pixels.
    actor_for_costume[]        Map costume id → actor index
    actor_pos_x/y[]			   Actor coordinates.
    current_room               Id of the currently active room.

Global Outputs
    costume_room_idx[]         Updated to target_room for the active costume.
    costume_target_x/y[]       Updated to target_x/y for the active costume.
    actor_pos_x[]              Updated if an actor was assigned and not already
                               at the target position.
    actor_cur_facing_direction[]	Set to DIRECTION_DOWN when the actor is moved.

Description
    - Publish the target room and coordinates into the costume’s own
      slots (costume_room_idx and costume_target_x/y).
    - Record the target coordinates into debug variables
    - Lookup the actor assigned to this costume:
        - If no actor is assigned (index < 0):
            - Skip directly to the conditional reassign logic.
        - If an actor is assigned:
            - If the actor is already at the target position, return.
            - Otherwise:
                - Copy costume_target_x/y into actor_pos_x/y.
                - Set actor_cur_facing_direction to DIRECTION_DOWN.
                - Call detach_actor_from_costume to release the actor.
    - If target_room == 0 or target_room != current_room, return.
    - Otherwise (target_room equals current_room and is nonzero):
        - Call assign_costume_to_free_actor to bind a new on-screen actor to
          the placed costume.
================================================================================
*/
* = $269C
place_costume_at_target:
		// Publish target room for the costume 
		ldx     active_costume
		lda     target_room
		sta     costume_room_idx,x

		// Publish target coordinates for the costume
		lda     target_x
		sta     costume_target_x,x
		lda     target_y
		sta     costume_target_y,x

		// Capture last target into debugging vars
		lda     target_x
		sta     unused_var_1
		lda     target_y
		sta     unused_var_2

		// Actor assigned to costume? Branch on outcome
		ldy     actor_for_costume,x
		bmi     reassign_if_in_current_room

		// ------------------------------------------------------------
		// Actor assigned		
		// ------------------------------------------------------------
		// Actor already at target? If so, exit
		lda     costume_target_x,x
		cmp     actor_pos_x,y
		bne     place_actor
		lda     costume_target_y,x
		cmp     actor_pos_y,y
		beq     exit_pcat

place_actor:
		// ------------------------------------------------------------
		// Actor not at target
		// ------------------------------------------------------------
		// Place actor at target
		lda     costume_target_x,x
		sta     actor_pos_x,y
		lda     costume_target_y,x
		sta     actor_pos_y,y
		
		// Set default facing direction to “down”
		lda     #DIRECTION_DOWN
		sta     actor_cur_facing_direction,y

		// Unassign the actor from this costume (will reassign if needed below)
		jsr     detach_actor_from_costume

reassign_if_in_current_room:
		// Target room is zero? If so, exit
		lda     target_room
		beq     exit_pcat
		
		// Target room is not current room? If so, exit
		cmp     current_room
		bne     exit_pcat

		// ------------------------------------------------------------
		// Target room is current room
		// ------------------------------------------------------------
		// Assign an actor
		ldx     active_costume
		jsr     assign_costume_to_free_actor

exit_pcat:
		rts

/*
Pseudo-code
function face_toward_target():
    // Select facing clip for current kid → target
    clip = select_facing_clip_for_target()
    saved_facing_clip_id = clip
    target_clip_id       = clip
    clip_loop_cnt        = LOOP_ONE_SHOT

    // Apply facing to current_kid_idx
    active_costume = current_kid_idx
    assign_clip_to_costume()

    // If target is not a costume/actor, we are done
    if target_obj_hi != OBJ_TYPE_COSTUME:
        return

    // target is a costume
    active_costume = target_obj_lo

    // Special case: plant does not mirror facing
    if active_costume == COSTUME_ID_PLANT:
        return

    // Mirror left/right bit and apply to target costume
    mirrored_clip   = saved_facing_clip_id XOR CLIP_LR_BIT
    target_clip_id  = mirrored_clip
    clip_loop_cnt   = LOOP_ONE_SHOT
    assign_clip_to_costume()


function select_facing_clip_for_target() -> clip_id:
    // Costume / actor target?
    if target_obj_hi == OBJ_TYPE_COSTUME:
        // Resolve target actor's X
        target_costume = target_obj_lo
        target_actor   = actor_for_costume[target_costume]
        target_x       = actor_pos_x[target_actor]

        // Resolve current kid's actor X
        kid_costume = current_kid_idx
        kid_actor   = actor_for_costume[kid_costume]
        kid_x       = actor_pos_x[kid_actor]

        // If kid_x ≤ target_x, face right; else left
        if kid_x <= target_x:
            return CLIP_STAND_RIGHT
        else:
            return CLIP_STAND_LEFT

    // Object target
    object_index = target_entity

    // Unknown / no object → default down
    if object_index == $FF:
        return CLIP_STAND_DOWN

    // If object has explicit approach clip, use it; otherwise down
    explicit_clip = obj_has_explicit_approach[object_index]
    if explicit_clip != 0:
        return explicit_clip
    else:
        return CLIP_STAND_DOWN


function set_approach_point(entity_type_A, x_reg, y_reg):
    // Actor/costume target?
    if entity_type_A == OBJ_TYPE_COSTUME:
        // Use current kid as mover for the actor path
        // (X is target costume id, Y becomes moving costume id)
        y_reg = current_kid_idx

        // Fall through to actor-specific handler
        set_approach_point_for_actor(x_reg, y_reg)
        return

    // Otherwise, object target
    // X = moving costume (actor), Y = object index
    set_approach_point_for_object(x_reg, y_reg)


function set_approach_point_for_actor(target_costume, mover_costume):
    // Convert costumes to actor indices
    mover_actor  = actor_for_costume[mover_costume]
    target_actor = actor_for_costume[target_costume]

    // Start with target actor’s current position
    target_y = actor_pos_y[target_actor]
    target_x = actor_pos_x[target_actor]

    mover_x = actor_pos_x[mover_actor]

    // Decide which side to stand on:
    // if mover_x ≤ target_x → stand on the left (negative offset)
    // else                  → stand on the right (positive offset)
    if mover_x <= target_x:
        offset = ACTOR_APPROACH_X_OFFSET_NEG
    else:
        offset = ACTOR_APPROACH_X_OFFSET_POS

    // Apply horizontal offset to target_x
    target_x = target_x + offset

    // Now snap (target_x, target_y) to a valid walkbox in current_room
    snap_target_to_walkbox()


function set_approach_point_for_object(mover_costume_x, object_index_y):
    object_index = object_index_y

    // If object has explicit approach point, use it
    if obj_has_explicit_approach[object_index] != 0:
        target_x = obj_approach_x[object_index]
        target_y = obj_approach_y[object_index] * 4   // rows → pixels
        // fall through to snap
        snap_target_to_walkbox()
        return

    // Otherwise, aim at the center of the object’s box:
    // X center: left + width/2
    // Y center: (top + height/2) in rows, then *4 → pixels

    width  = obj_width_tbl[object_index]
    height = obj_height_tbl[object_index]
    left   = obj_left_col_tbl[object_index]
    topRow = obj_top_row_tbl[object_index]

    target_x = left + (width  >> 1)
    target_row = topRow + (height >> 1)
    target_y = target_row * 4     // rows → pixels

    // Snap to walkbox system for the current room
    snap_target_to_walkbox()


function snap_target_to_walkbox():
    // Copy current target into snap probes
    snap_probe_x = target_x
    snap_probe_y = target_y

    // Select walkbox set from current_room
    walkbox_room = current_room

    // Clamp/project (snap_probe_x, snap_probe_y) to nearest walkbox point
    // and compute nearest_box_idx internally
    snap_coords_to_walkbox()

    // Write snapped coordinates back to target_x/target_y
    target_x = snap_probe_x
    target_y = snap_probe_y

    return


function snap_actor_target() -> nearest_box_idx:
    // Load this actor’s requested target into test coords
    a = actor
    test_x = actor_target_x[a]
    test_y = actor_target_y[a]

    // Use the active costume’s room as walkbox_room
    c = active_costume
    walkbox_room = costume_room_idx[c]

    // Ask walkbox system to snap to nearest valid point
    nearest_box_idx = snap_coords_to_walkbox()

    // If no walkbox intersects, leave target unchanged
    if nearest_box_idx == NO_BOX_FOUND:
        return nearest_box_idx

    // If valid, resolve actor index again from active costume
    actor_index = actor_for_costume[c]

    // Apply snapped coordinates back to that actor’s target
    actor_target_x[actor_index] = test_x
    actor_target_y[actor_index] = test_y

    return nearest_box_idx


function set_costume_target():
    // Resolve actor index for the active costume
    costume = active_costume
    actor_index = actor_for_costume[costume]

    // If negative → no on-screen actor; treat as off-screen
    if actor_index < 0:
        // Store target into costume’s own target slots
        costume_target_x[costume] = target_x
        costume_target_y[costume] = target_y
        return

    // On-screen actor:
    actor = actor_index        // publish actor globally

    // Snap (target_x, target_y) directly to walkboxes of current_room
    snap_coords_to_walkbox()

    // Save snapped target into actor’s per-actor target slots
    actor_target_x[actor_index] = target_x
    actor_target_y[actor_index] = target_y

    // Stage path update / path rebuild for this actor
    stage_actor_path_to_target()


function place_costume_at_target():
    costume = active_costume

    // Publish target room and coordinates into costume state
    costume_room_idx[costume]  = target_room
    costume_target_x[costume]  = target_x
    costume_target_y[costume]  = target_y

    // Check if an actor is currently assigned to this costume
    assigned_actor = actor_for_costume[costume]

    if assigned_actor >= 0:
        // There is an actor assigned

        // If actor is already at the costume target, no move needed
        if actor_pos_x[assigned_actor] == costume_target_x[costume] and
           actor_pos_y[assigned_actor] == costume_target_y[costume]:
            // Skip to reassign logic (room-based)
            goto room_reassign_check

        // Otherwise, place the actor at the costume’s target
        actor_pos_x[assigned_actor] = costume_target_x[costume]
        actor_pos_y[assigned_actor] = costume_target_y[costume]

        // Set default facing direction for the placed actor
        actor_cur_facing_direction[assigned_actor] = DIRECTION_DOWN

        // Detach this actor from the costume (free it for reuse)
        detach_actor_from_costume(costume)

    // Reassignment / visibility logic
room_reassign_check:
    // If target_room is “none”, stop
    if target_room == 0:
        return

    // If target_room is not the currently active room, stop
    if target_room != current_room:
        return

    // Otherwise, target_room == current_room and nonzero:
    // assign a free actor to the costume in this room
    assign_costume_to_free_actor(costume)
*/