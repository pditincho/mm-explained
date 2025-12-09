/*
================================================================================
  ops_costume.asm
================================================================================

Summary
        Implements costume- and actor-related opcode handlers. Includes
        bitmask, distance, and animation control operations used by the script
        engine to read or manipulate actor state. Handles querying positions,
        clip sets, mouth states, and actor attributes.

Global Inputs
        actor_for_costume        Maps costume index → actor index or unused flag
        actor_pos_x              Current X coordinate per actor
        actor_pos_y              Current Y coordinate per actor
        actor_motion_state       Motion state codes for each actor
        actor_mouth_state        Mouth animation state
        actor_vars               Actor variable array ($FEE3)
        costume_room_idx         Room index for each costume
        costume_target_x / _y      Destination coordinates
        costume_clip_set         Current active clip/animation per costume
        actor_cur_facing_direction Facing direction mask for each actor

Global Outputs
        game_vars                Script-accessible variable array
        actor_mouth_state        Updated for mouth control opcodes
        actor_motion_state       Updated for movement
        refreshed inventory display via refresh_inventory

Arguments
        Implicit via script stream:
        - destination game-var index
        - costume/actor indices (low byte from script, high from opcode bit)
        - optional bitmask or clip arguments

Returns
        None (effects stored in engine state or game_vars)

Description
        - Configures pointer and range for bitmask ops.
        - Performs set/get of costume variables.
        - Computes distances between costumes.
        - Determines nearest costume or kid costume.
        - Retrieves position and motion state for costumes.
        - Maps direction masks to standing clips.
        - Applies animation clips with mouth and reset control.
================================================================================
 Opcode Table — ops_costume.asm
================================================================================
 op_apply_bitmask_to_actor_var   → 0B, 2B, 4B, 6B, 8B, AB, CB, EB   Set/clear actor bit attribute via shared bitmask applier, then refresh inventory.
 op_test_costume_var_bitmask     → 1B, 5B, 9B, DB                   Test actor var with bitmask, store boolean to game var.
 op_get_costumes_distance        → 06, 34, 74, B4, F4               Compute distance between two costumes, write to game var.
 op_get_closest_costume          → 66                               Scan all costumes, store closest into destination game var.
 op_get_closest_kid              → 26                               Variant limited to kid costumes only.
 op_get_costume_motion_state     → 56, D6                           Return motion state for costume or STOPPED if none assigned.
 op_get_costume_room             → 07, 87                           Fetch current room index for costume.
 op_get_costume_x                → 47, C7                           Return current or destination X depending on movement state.
 op_get_costume_y                → 27, A7                           Return current or destination Y depending on movement state.
 op_get_costume_clip_set         → 67, E7                           Return active clip if moving else standing clip based on facing.
 op_apply_costume_clip           → 11, 51, 91, D1                   Apply clip set or handle FE/FD/FF mouth/reset control cases.
================================================================================
*/

#importonce
#import "globals.inc"
#import "constants.inc"
#import "script_primitives.asm"
#import "ops_primitives.asm"

.label reference_costume   = $17         // Reference actor/costume index for tests
.label current_costume     = $19         // Candidate actor/costume index (descending scan)
.label closest_distance    = $5f77       // Best distance found so far
.label closest_costume     = $5f78       // Best actor/costume index found

.const CLIP_ARG_KEEP_MOUTH_CLOSED = $FD  // Anim arg: force mouth closed
.const CLIP_ARG_KEEP_MOUTH_OPEN   = $FE  // Anim arg: force mouth open
.const CLIP_ARG_RESET_STATE       = $FF  // Anim arg: reset animation/render state

/*
================================================================================
  op_apply_bitmask_to_actor_var - Opcodes #$0B, #$2B, #$4B, #$6B, #$8B, #$AB, #$CB, #$EB
================================================================================

Summary
        Configure the actor-variables base pointer and bounds, then invoke the
        shared bitmask applicator to set/clear bits on an actor attribute.
        Finally refresh the items display to reflect state changes.

Arguments
        (script) op+bit7  → variable_index (actor index)
        (script) op+bit6  → bitmask
        (script) op+bit5  → mode (#$00=set, otherwise=clear)

Returns
        None

Description
        - pointer := $FEE3 (actor variables array).
        - max_var_index := #$19 (exclusive upper bound for actor indices).
        - JSR apply_bitmask_to_var to perform set/clear.
        - JMP refresh_items_displayed to update UI.
================================================================================
*/
* = $6391
op_apply_bitmask_to_actor_var:
        // ------------------------------------------------------------
        // Set up base pointer → $FEE3 (actor variables)
        // ------------------------------------------------------------
        lda     #<actor_vars
        sta     pointer
        lda     #>actor_vars
        sta     pointer+1

        // ------------------------------------------------------------
        // Set maximum valid actor index (exclusive) → #$19
        // ------------------------------------------------------------
        lda     #COSTUME_MAX_INDEX + 1
        sta     max_var_index

        // ------------------------------------------------------------
        // Apply bitmask set/clear, then refresh items UI
        // ------------------------------------------------------------
        jsr     apply_bitmask_to_var
        jmp     refresh_inventory
/*
================================================================================
  op_test_costume_var_bitmask - Opcodes #$1B, #$5B, #$9B, #$DB
================================================================================
Summary
        Point the variable base to actor variables, then tail-call the generic
        bitmask tester that ANDs a source variable with a bitmask and writes a
        boolean result (#$00/#$01) to a destination game variable.

Arguments
        (forwarded to callee test_var_bitmask)
        (script) byte0  → result_variable_index
        (script) bit7   → source_variable_index
        (script) bit6   → bitmask

Returns
        None (callee writes boolean to game_vars[result_variable_index])

Description
        - pointer := $FEE3 (actor variables base).
        - Jump to test_var_bitmask for AND and booleanization.
================================================================================
*/
* = $63A3
op_test_costume_var_bitmask:
        // ------------------------------------------------------------
        // Set up base pointer → $FEE3 (actor variables)
        // ------------------------------------------------------------
        lda     #<actor_vars
        sta     pointer
        lda     #>actor_vars
        sta     pointer+1

        // ------------------------------------------------------------
        // Tail-call shared tester (AND with mask → boolean result)
        // ------------------------------------------------------------
        jmp     test_var_bitmask
/*
================================================================================
  op_get_costumes_distance - Opcodes #$06, #$34, #$74, #$B4, #$F4 
================================================================================

Summary
        Read a destination variable, a reference actor index, and another actor
        index. Compute distance(reference, other) and store the result.

Arguments
        (script) byte0  → destination game-var index
        (script) byte1  → other actor index (hi from opcode bit6)
        (script) bit7   → reference actor index hi selector; low byte read next

Returns
        None (writes distance to game_vars[dest])

Description
        - destination_variable := byte0
        - reference_costume := operand via script_load_operand_bit7
        - current_costume   := operand via script_load_operand_bit6
        - A := compute_actors_distance(reference_costume, current_costume)
        - game_vars[destination_variable] := A
================================================================================
*/
* = $64CC
op_get_costumes_distance:
        // ----------------------------
        // Resolve destination variable index
        // ----------------------------
        jsr     script_read_byte              
        sta     dest_var

        // ----------------------------
        // Resolve reference costume index
        // ----------------------------
        jsr     script_load_operand_bit7      
        sta     reference_costume

        // ----------------------------
        // Resolve other costume index
        // ----------------------------
        jsr     script_load_operand_bit6      
        sta     current_costume

        // ----------------------------
        // Compute distance(reference, other)
        // ----------------------------
        jsr     compute_actors_distance       

        // ----------------------------
        // Store result into game_vars[dest]
        // ----------------------------
        ldx     dest_var
        sta     game_vars,x
        rts
/*
================================================================================
  op_get_closest_costume - Opcode #$66
================================================================================
Summary
        Scan all costumes from index #$19 down to #$01 relative to a reference
        costume. Track the minimum distance and return the closest costume index in
        a destination game variable.

Arguments
        (script) byte0  → destination game-var index for result
        (script) bit7   → reference costume index hi selector; low byte read next

Returns
        None (writes closest costume index to game_vars[dest])

Description
        - Reads destination variable index.
        - Reads the reference costume index.
        - Initializes closest_distance := #$FF and closest_costume := #$FF.
        - Iterates current_costume from #$19 downward to #$01:
            - Calls compute_actors_distance(reference_costume, current_costume).
            - If distance < closest_distance, updates both trackers.
        - Stores closest_costume into game_vars[dest].
================================================================================
*/
* = $64E6
op_get_closest_costume:
        // ------------------------------------------------------------
        // Resolve destination variable and reference costume indexes
        // ------------------------------------------------------------
        jsr     script_read_byte              // A := dest var index
        sta     dest_var

        jsr     script_load_operand_bit7      // A := reference costume index
        sta     reference_costume

        // ------------------------------------------------------------
        // Initialize iterator: start from #$19 and descend
        // ------------------------------------------------------------
        lda     #COSTUME_MAX_INDEX + 1
        sta     current_costume

init_distance_search:
        // ------------------------------------------------------------
        // Initialize search trackers
        // closest_distance := #$FF, closest_costume := #$FF
        // ------------------------------------------------------------
        lda     #$ff
        sta     closest_distance
        lda     #$ff
        sta     closest_costume

compare_costume_distance:
        // ------------------------------------------------------------
        // Compute distance(reference_costume, current_costume)
        // ------------------------------------------------------------
        jsr     compute_actors_distance       

        // ------------------------------------------------------------
        // If A < closest_distance, record new best
        // ------------------------------------------------------------
        cmp     closest_distance
        bcs     ogcc_next_costume             // A >= best → skip update
        sta     closest_distance              // new best distance
        lda     current_costume
        sta     closest_costume               // new best costume

ogcc_next_costume:
        // ------------------------------------------------------------
        // Decrement iterator; continue while non-zero
        // ------------------------------------------------------------
        dec     current_costume
        bne     compare_costume_distance

        // ------------------------------------------------------------
        // Store result into game_vars[dest]
        // ------------------------------------------------------------
        lda     closest_costume
        ldx     dest_var
        sta     game_vars,x
        rts
/*
================================================================================
  op_get_closest_kid - Opcode #$26

Summary
        Variant of “get closest actor” limited to kid costumes only. Reads a
        destination variable and a reference costume index, then initializes the
        iterator to the highest kid index (#$07) and tail-calls the shared
        distance-search routine.

Arguments
        (script) byte0  → destination game-var index
        (script) bit7   → reference costume index hi selector; low byte read next

Returns
        None (result written by shared search to game_vars[dest])

Description
        - Stores destination variable index into destination_variable.
        - Reads reference costume index into reference_costume.
        - Sets current_costume := #$07 and iterates downward in the shared
          routine (init_distance_search), which computes the closest kid and
          stores the result.
================================================================================
*/
* = $651D
op_get_closest_kid:
        // ------------------------------------------------------------
        // Resolve destination variable index
        // ------------------------------------------------------------
        jsr     script_read_byte              
        sta     dest_var

        // ------------------------------------------------------------
        // Resolve reference costume index
        // ------------------------------------------------------------
        jsr     script_load_operand_bit7      
        sta     reference_costume

        // ------------------------------------------------------------
        // Initialize iterator to highest kid index (#$07), then descend
        // ------------------------------------------------------------
        lda     #FIRST_NON_KID_INDEX - 1
        sta     current_costume

        // ------------------------------------------------------------
        // Tail-call shared distance search (uses current_* and reference_*)
        // ------------------------------------------------------------
        jmp     init_distance_search
/*
================================================================================
  op_get_costume_motion_state - Opcodes #$56, #$D6

Summary
        Read a destination variable index and a costume selector. If no actor is
        assigned to that costume, return STOPPED. Otherwise return the
        actor’s current motion state. Store the result into a game variable.

Arguments
        (script) byte0  → destination game-var index
        (script) bit7   → costume index hi selector; low byte read next

Returns
        None (writes result to game_vars[dest])

Description
        - Reads destination variable index from the script.
        - Loads costume index via script_load_operand_bit7.
        - Tests actor_for_costume[X]:
            - bit7=1 → no actor assigned → result := #$02 (stopped).
            - else   → result := actor_motion_state[actor_index].
        - Stores result into game_vars[dest].
================================================================================
*/
* = $6543
op_get_costume_motion_state:
        // ----------------------------
        // Resolve destination variable index
        // ----------------------------
        jsr     script_read_byte              
        sta     dest_var          

        // ----------------------------
        // Resolve costume index
        // ----------------------------
        jsr     script_load_operand_bit7      
		
		// Resolve actor for costume
        tax                                   
        lda     actor_for_costume,x           

        // ----------------------------
        // Actor assigned? bit7=0 → assigned; bit7=1 → none
        // ----------------------------
        bpl     actor_assigned                // assigned → use motion state

        // ----------------------------
        // No actor assigned → return STOPPED
        // ----------------------------
        lda     #MOTION_STOPPED_CODE
        jmp     ogcms_set_result

actor_assigned:
		// Fetch motion state
        tax                                    
        lda     actor_motion_state,x           

ogcms_set_result:
        // ----------------------------
        // Store result into game_vars[dest]
        // ----------------------------
        ldx     dest_var           
        sta     game_vars,x
        rts
/*
================================================================================
  op_get_costume_room - Opcodes #$07, #$87

Summary
        Read a destination variable index and a costume index. Load the room
        index for that costume and store it into the destination var.

Arguments
        (script) byte0  → destination game-var index
        (script) bit7   → costume index hi selector; low byte read next

Returns
        None (writes result to game_vars[dest])

Description
        - Reads destination variable index from the script.
        - Loads costume index via script_load_operand_bit7.
        - Reads costume_room_idx[X] to obtain the costume’s current room index.
        - Stores the room index into game_vars[dest].
================================================================================
*/
* = $68EB
op_get_costume_room:
        // ----------------------------
        // Resolve destination variable index
        // ----------------------------
        jsr     script_read_byte              
        sta     destination_variable          

        // ----------------------------
        // Resolve costume index
        // ----------------------------
        jsr     script_load_operand_bit7      
        tax                                   

        // ----------------------------
        // Fetch room index for costume
        // ----------------------------
        lda     costume_room_idx,x            

        // ----------------------------
        // Store result into game_vars[dest]
        // ----------------------------
        ldx     destination_variable          
        sta     game_vars,x                   
        rts
/*
================================================================================
  op_get_costume_x - Opcodes #$47, #$C7
================================================================================
Summary
        Read a destination variable index and a costume selector. 
		If the costume is moving, return its destination Y. 
		If not moving, return current Y. 
		Store the result into the selected game variable.

Arguments
        (script) byte0  → destination game-var index
        (script) bit7   → costume index hi selector; low byte read next

Returns
        None (writes result to game_vars[dest])

Description
        - Reads destination variable index from the script.
        - Loads costume index via script_load_operand_bit7.
        - If actor_for_costume[X] has bit7 set (moving), uses costume_target_x[X].
        - Else uses actor_pos_x[X].
        - Stores the X value into game_vars[dest].
================================================================================
*/
* = $68FD
op_get_costume_x:
        // ----------------------------
        // Resolve destination variable index
        // ----------------------------
        jsr     script_read_byte              
        sta     destination_variable          

        // ----------------------------
        // Resolve costume index
        // ----------------------------
        jsr     script_load_operand_bit7      
        tax                                     

        // ----------------------------
        // Resolve actor
        // ----------------------------
        lda     actor_for_costume,x           
		
		// Moving? actor_for_costume[X] bit7=1 means moving
        bmi     ogcx_actor_moving                  // moving → use destination X

        // ----------------------------
        // Not moving → return current X position
        // ----------------------------
        tax                                    
        lda     actor_pos_x,x                  
        jmp     ogcx_set_value_in_var          

ogcx_actor_moving:
        // ----------------------------
        // Moving → return path destination X
        // ----------------------------
        lda     costume_target_x,x               

ogcx_set_value_in_var:
        // ----------------------------
        // Store result into game_vars[dest]
        // ----------------------------
        ldx     destination_variable           
        sta     game_vars,x                    
        rts		
/*
================================================================================
  op_get_costume_y - Opcodes #$27, #$A7
================================================================================
Summary
        Read a destination variable index and a costume selector. 
		If the costume is moving, return its destination Y. 
		If not moving, return current Y. 
		Store the result into the selected game variable.

Arguments
        (script) byte0  → destination game-var index
        (script) bit7   → costume index hi selector; low byte read next

Returns
        None (writes result to game_vars[dest])

Description
        - Reads destination variable index from the script.
        - Loads costume index via script_load_operand_bit7.
        - If actor_for_costume[X] has bit7 set (moving), uses costume_target_y[X].
        - Else uses actor_pos_y[X].
        - Stores the Y value into game_vars[dest].
================================================================================
*/
* = $691B
op_get_costume_y:
        // ----------------------------
        // Resolve destination variable index
        // ----------------------------
        jsr     script_read_byte              
        sta     destination_variable          

        // ----------------------------
        // Resolve costume index
        // ----------------------------
        jsr     script_load_operand_bit7      
        tax                                     

        // ----------------------------
        // Resolve actor
        // ----------------------------
        lda     actor_for_costume,x           
		
		// Moving? actor_for_costume[X] bit7=1 means moving
        bmi     ogcy_actor_moving                  // moving → use destination Y

        // ----------------------------
        // Not moving → return current Y position
        // ----------------------------
        tax                                    
        lda     actor_pos_y,x                  
        jmp     ogcy_set_value_in_var          

ogcy_actor_moving:
        // ----------------------------
        // Moving → return path destination Y
        // ----------------------------
        lda     costume_target_y,x               

ogcy_set_value_in_var:
        // ----------------------------
        // Store result into game_vars[dest]
        // ----------------------------
        ldx     destination_variable           
        sta     game_vars,x                    
        rts		
/*
================================================================================
  op_get_costume_clip_set - Opcodes #$67, #$E7
================================================================================
Summary
        Read a destination variable index and a costume selector. 
		If the costume is moving, return the active clip/animation for that costume. 
		If not moving, map the facing direction to the standing clip. 
		Store result into the selected game variable.

Arguments
        (script) byte0  → destination game-var index
        (script) bit7   → costume index hi selector; low byte read next

Returns
        None (writes result to game_vars[dest])

Description
        - Reads destination variable index from the script.
        - Loads costume index via script_load_operand_bit7.
        - If actor_for_costume[X] has bit7 set (moving), uses costume_clip_set[X].
        - Else maps actor_cur_facing_direction[X] to a standing clip.
        - Stores the resolved clip id into game_vars[dest].
================================================================================
*/
* = $6939
op_get_costume_clip_set:
        // ----------------------------
        // Read destination variable index
        // ----------------------------
        jsr     script_read_byte               
        sta     destination_variable           

        // ----------------------------
        // Resolve costume index
        // ----------------------------
        jsr     script_load_operand_bit7       
        tax                                    

        // Resolve actor
        lda     actor_for_costume,x            
		
		// Moving? actor_for_costume[X] bit7=1 means moving
        bmi     ogccs_actor_moving

        // ----------------------------
        // Not moving → map facing direction to standing clip id
        // ----------------------------
        tax                                    
        lda     actor_cur_facing_direction,x     // A := path-facing mask
        jsr     map_facing_direction_to_standing_clip  // A := standing clip id
        jmp     ogccs_set_value_in_var               // store and exit

ogccs_actor_moving:
        // ----------------------------
        // Moving → use current clip/animation for this costume
        // ----------------------------
        lda     costume_clip_set,x              

ogccs_set_value_in_var:
        // ----------------------------
        // Store result into game_vars[dest]
        // ----------------------------
        ldx     destination_variable           
        sta     game_vars,x                    
        rts
/*
================================================================================
  op_apply_costume_clip - Opcodes #$11, #$51, #$91, #$D1
================================================================================
Summary
        Read costume index, clip set, and argument from the script.
        Apply special mouth-state cases (#$FE open, #$FD closed), reset state
        on #$FF, or apply the specified clip set. Persist loop count.

Arguments
        (script) byte0  → costume index (hi from opcode bit7)
        (script) byte1  → clip set (hi from opcode bit6)
        (script) byte2  → animation argument (loop count or flag)

Description
        - Stores costume index into active_costume.
        - Stores clip set into target_clip_id.
        - Stores animation argument into clip_loop_cnt.
        - If target_clip_id = #$FE → set mouth open and still.
        - If target_clip_id = #$FD → set mouth closed.
        - If target_clip_id = #$FF → reset actor render flags.
        - Else call assign_clip_to_costume.
================================================================================
*/
* = $695A
op_apply_costume_clip:
        // ----------------------------
        // Resolve costume index
        // ----------------------------
        jsr     script_load_operand_bit7      
        sta     active_costume                

        // ----------------------------
        // Resolve clip set
        // ----------------------------
        jsr     script_load_operand_bit6      
        sta     target_clip_id                

        // ----------------------------
        // Resolve clip argument (loop count / flag)
        // ----------------------------
        jsr     script_read_byte              
        sta     clip_loop_cnt                 

        // ----------------------------
        // Switch on target_clip_id
        // ----------------------------
        lda     target_clip_id
        cmp     #CLIP_ARG_KEEP_MOUTH_OPEN
        bne     check_arg_mouth_closed

        // ---------------------------------------
        // Case #$FE — keep actor's mouth open
        // ---------------------------------------
        ldx     active_costume
        ldy     actor_for_costume,x           
        bmi     done_mouth_open               // no actor assigned → exit
		
        lda     #MOUTH_STATE_HELD_OPEN        
        sta     actor_mouth_state,y
done_mouth_open:
        rts

check_arg_mouth_closed:
        cmp     #CLIP_ARG_KEEP_MOUTH_CLOSED
        bne     check_arg_reset

        // ---------------------------------------
        // Case #$FD — keep actor's mouth closed
        // ---------------------------------------
        ldx     active_costume
        ldy     actor_for_costume,x           
        bmi     done_mouth_closed             // no actor assigned → exit
		
        lda     #MOUTH_STATE_CLOSED           
        sta     actor_mouth_state,y
done_mouth_closed:
        rts

check_arg_reset:
        cmp     #CLIP_ARG_RESET_STATE
        bne     apply_clip

        // ---------------------------------------
        // Case #$FF — reset animation state
        // ---------------------------------------
        ldx     active_costume
        lda     actor_for_costume,x           
        bmi     done_reset                    // no actor assigned → exit
		
        sta     actor                         
        jsr     set_actor_stopped_and_render
done_reset:
        jmp     exit_oacc

apply_clip:
        // ---------------------------------------
        // Default — apply the clip set
        // ---------------------------------------
        jsr     assign_clip_to_costume

exit_oacc:
        rts

/*
procedure op_apply_bitmask_to_actor_var()
    // Point generic bitmask helper at actor variables
    pointer = &actor_vars[0]
    max_var_index = COSTUME_MAX_INDEX + 1   // valid indices: 0 .. COSTUME_MAX_INDEX

    // Apply script-driven set/clear operation, then refresh UI
    apply_bitmask_to_var(pointer, max_var_index)
    refresh_inventory()

procedure op_test_costume_var_bitmask()
    // Point generic tester at actor variables
    pointer = &actor_vars[0]

    // Delegate to shared helper:
    //   script provides:
    //     - destination game-var index
    //     - source actor-var index
    //     - bitmask
    //   helper writes 0/1 to game_vars[dest]
    test_var_bitmask(pointer)

procedure op_get_costumes_distance()
    // Read destination game-var index
    dest_var = script_read_byte()

    // Read the two costume indices to compare
    reference_costume = script_load_operand_bit7()
    other_costume     = script_load_operand_bit6()

    // Compute distance between the actors behind these costumes
    distance = compute_actors_distance(reference_costume, other_costume)

    // Store result
    game_vars[dest_var] = distance


procedure op_get_closest_costume()
    // Target game-var for the result
    dest_var = script_read_byte()

    // Costume we are measuring distances from
    reference_costume = script_load_operand_bit7()

    // Start from “one past last costume” and count down
    current_costume = COSTUME_MAX_INDEX + 1

    // Use shared search
    run_closest_costume_search(dest_var, reference_costume, current_costume)


procedure op_get_closest_kid()
    // Target game-var for the result
    dest_var = script_read_byte()

    // Costume we are measuring distances from
    reference_costume = script_load_operand_bit7()

    // Start from highest kid costume index and count down
    current_costume = FIRST_NON_KID_INDEX - 1

    // Use the same shared search logic as op_get_closest_costume
    run_closest_costume_search(dest_var, reference_costume, current_costume)


procedure run_closest_costume_search(dest_var, reference_costume, start_costume)
    current_costume  = start_costume
    closest_distance = 0xFF          // “no distance yet”
    closest_costume  = 0xFF          // “none yet”

    loop
        current_costume = current_costume - 1
        if current_costume == 0
            break

        distance = compute_actors_distance(reference_costume, current_costume)

        if distance < closest_distance
            closest_distance = distance
            closest_costume  = current_costume
        end if
    end loop

    game_vars[dest_var] = closest_costume


procedure op_get_costume_motion_state()
    // Where to store the motion state code
    dest_var = script_read_byte()

    // Which costume to inspect
    costume_index = script_load_operand_bit7()

    // Map costume → actor (or “none” sentinel)
    actor_entry = actor_for_costume[costume_index]

    if not costume_has_assigned_actor(actor_entry) then
        // No actor: treat as stopped
        result = MOTION_STOPPED_CODE
    else
        actor_index = extract_actor_index(actor_entry)
        result = actor_motion_state[actor_index]
    end if

    game_vars[dest_var] = result


procedure op_get_costume_room()
    // Destination game-var index
    dest_var = script_read_byte()

    // Costume whose room we want
    costume_index = script_load_operand_bit7()

    // Look up room directly from costume table
    room = costume_room_idx[costume_index]

    game_vars[dest_var] = room


procedure op_get_costume_x()
    // Destination game-var index
    dest_var = script_read_byte()

    // Costume whose X coordinate we want
    costume_index = script_load_operand_bit7()

    // Check whether we should use current position or path destination
    if costume_is_moving(costume_index) then
        // Use path’s destination X
        value = costume_target_x[costume_index]
    else
        // Use current actor X
        actor_entry = actor_for_costume[costume_index]
        actor_index = extract_actor_index(actor_entry)
        value = actor_pos_x[actor_index]
    end if

    game_vars[dest_var] = value


procedure op_get_costume_y()
    // Destination game-var index
    dest_var = script_read_byte()

    // Costume whose Y coordinate we want
    costume_index = script_load_operand_bit7()

    if costume_is_moving(costume_index) then
        // Use path’s destination Y
        value = costume_target_y[costume_index]
    else
        // Use current actor Y
        actor_entry = actor_for_costume[costume_index]
        actor_index = extract_actor_index(actor_entry)
        value = actor_pos_y[actor_index]
    end if

    game_vars[dest_var] = value


procedure op_get_costume_clip_set()
    // Destination game-var index
    dest_var = script_read_byte()

    // Costume whose clip we want
    costume_index = script_load_operand_bit7()

    actor_entry = actor_for_costume[costume_index]

    if costume_is_moving(costume_index) then
        // Use the currently active clip for this costume
        clip = costume_clip_set[costume_index]
    else
        // Not moving: derive a standing clip from facing direction
        actor_index = extract_actor_index(actor_entry)
        facing_mask = actor_cur_facing_direction[actor_index]
        clip = map_facing_direction_to_standing_clip(facing_mask)
    end if

    game_vars[dest_var] = clip


procedure op_apply_costume_clip()
    // Costume being affected
    active_costume = script_load_operand_bit7()

    // Clip “id” or control code
    target_clip_id = script_load_operand_bit6()

    // Loop count / extra argument
    clip_loop_cnt = script_read_byte()

    // Special control cases based on target_clip_id
    if target_clip_id == CLIP_ARG_KEEP_MOUTH_OPEN then
        // Keep actor’s mouth held open
        actor_index = extract_actor_index_if_any(actor_for_costume[active_costume])
        if actor_index then
            actor_mouth_state[actor_index] = MOUTH_STATE_HELD_OPEN
        end if
        return
    end if

    if target_clip_id == CLIP_ARG_KEEP_MOUTH_CLOSED then
        // Force actor’s mouth closed
        actor_index = extract_actor_index_if_any(actor_for_costume[active_costume])
        if actor_index then
            actor_mouth_state[actor_index] = MOUTH_STATE_CLOSED
        end if
        return
    end if

    if target_clip_id == CLIP_ARG_RESET_STATE then
        // Reset actor’s animation state and mark stopped
        actor_index = extract_actor_index_if_any(actor_for_costume[active_costume])
        if actor_index then
            actor = actor_index              // install as current actor
            set_actor_stopped_and_render()   // clear motion and refresh visuals
        end if
        return
    end if

    // Default: apply a normal animation clip to the costume
    assign_clip_to_costume(active_costume, target_clip_id, clip_loop_cnt)

*/