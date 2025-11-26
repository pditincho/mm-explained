/*
================================================================================
Script primitives and operand helpers
================================================================================

Summary
    Low-level building blocks for the script VM. Provides shared helpers for
    decoding opcode operands, accessing object attributes, computing simple
    spatial metrics, scanning inventory slots, advancing pause timers, and
    resolving sentence objects into concrete IDs.

Responsibilities
    • Operand decoding:
        - Use opcode bits 5, 6, and 7 as “operand kind” selectors.
        - Route each operand to a shared loader that chooses between immediate
          literals and game variable reads (game_vars[]).
        - Expose resolved operand values in A for higher-level opcode handlers.

    • Variable/operand comparisons:
        - Compare a script-selected variable against a second operand resolved
          via bit7, setting CMP-style flags (C/Z/N) for conditional branches.

    • Object attribute access:
        - Read an object’s attribute byte, choosing the object index either from
          the active direct object or from the next script byte (bit6-driven).
        - Write back updated attributes to the same object and reset camera
          position to a default.

    • Inventory helpers:
        - Scan inventory objects linearly to find the first free slot.
        - Return the slot index to callers that want to add new items.

    • Distance and proximity:
        - Compute an anisotropic distance between two costumes/actors using a
          room-consistency check, “live vs destination” coordinates, and a
          |Δx|/|Δy| metric with Y compressed (Δy >> 2) for interaction ranges.

    • Pause timing:
        - Maintain per-task 24-bit pause counters and automatically transition
          tasks from PAUSED to RUNNING when their high byte rolls to zero.

    • Sentence object resolution:
        - Translate script-level mode bytes into a concrete object ID pair
          (lo/hi), selecting between active IO, active DO, or an immediate ID
          with the high byte derived from opcode bit7.

Usage
    These primitives are intended to be called from script opcode handlers and
    the script engine core, keeping common operand decoding and small game
    mechanics logic centralized and consistent across the VM.
================================================================================
*/
#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "script_engine.asm"
#import "sentence_action.asm"

.label item_index_saved = $fe52           // Saved object index (Y) for get/set attrs

.label rhs_operand      = $d0             // ZP temp (shared): right-hand operand for CMP
.label ref_costume_idx  = $17             // Costume index: reference entity
.label cur_costume_idx  = $19             // Costume index: current entity
.label ref_actor_x      = $1b             // Temp: reference actor X position
.label ref_actor_y      = $1c             // Temp: reference actor Y position
.label cur_actor_x      = $1d             // Temp: current actor X position
.label cur_actor_y      = $1e             // Temp: current actor Y position
.label low_byte         = $d0             // ZP temp (shared): result low byte
.label hi_byte          = $d1             // ZP temp: result high byte
.label input_opcode     = $d2             // ZP: opcode snapshot for kind tests

.const SENTENCE_OBJ_ACTIVE_IO = $FF       // Mode: return active indirect object (IO)
.const SENTENCE_OBJ_ACTIVE_DO = $FE       // Mode: return active direct object (DO)

.const DISTANCE_INVALID       = $FF       // Return value when room check fails
.const ROOM_INDEX_INVALID     = $00       // Sentinel for “no valid room”

.const OPCODE_KIND_BIT5_MASK  = $20       // Opcode kind mask: bit 5
.const OPCODE_KIND_BIT6_MASK  = $40       // Opcode kind mask: bit 6
.const OPCODE_KIND_BIT7_MASK  = $80       // Opcode kind mask: bit 7

/*
================================================================================

The script operand system supports two argument kinds: variable and immediate.
To avoid duplicating code paths, the opcode embeds “operand-kind” bits that
indicate whether each operand should be read as a variable value or a literal
constant.

Bits 7, 6, and 5 of the opcode serve as independent operand-kind selectors:
- Bit set (1): operand is a variable; its index is read from the script, and
  the variable value is fetched from game_vars[].
- Bit clear (0): operand is immediate; the literal byte is read directly
  from the script.

Each bit has its own loader routine:
    script_load_operand_bit7
    script_load_operand_bit6
    script_load_operand_bit5

Each performs:
    A ← opcode
    AND #mask      ; mask = $80, $40, or $20
    JMP script_operand_type_check

The AND result defines the Z flag:
- Z=0 → bit set → variable path (BNE branch taken)
- Z=1 → bit clear → immediate path (branch not taken)

script_operand_type_check then executes:
    - Variable path: read index from script, fetch game_vars[index].
    - Immediate path: read byte literal from script.

The resolved operand value is returned in A.
================================================================================
*/

/*
================================================================================
script_compare_operands
================================================================================
Summary
	Compare a script-selected variable against a second operand resolved via opcode
	bit7, setting flags as per CMP (A − M).

Returns
	C  Set if var ≥ rhs_operand; clear if var < rhs_operand
	Z  Set if var == rhs_operand; clear otherwise
	N  Sign of (var − rhs_operand)

Global Inputs
	game_vars                table of game variables; indexed by byte read from script
	rhs_operand              zero-page temp holding right-hand operand loaded by bit7

Description
	- Read variable index from script into Y.
	- Resolve operand by calling script_load_operand_bit7, store into rhs_operand.
	- Load A := game_vars[Y] and CMP rhs_operand to set C/Z/N for caller.
================================================================================
*/
* = $5F79
script_compare_operands:
        // Read operand1 index from script → Y
        jsr     script_read_byte
        tay

        // Load operand2 (script-sourced) → rhs_operand
        jsr     script_load_operand_bit7
        sta     rhs_operand

        // Compare game_vars[Y] against rhs_operand
        // CMP sets: C=1 if A≥M, Z=1 if A==M, N=sign of (A−M)
        lda     game_vars,y
        cmp     rhs_operand
        rts
/*
================================================================================
script_load_operand_bit5/6/7
================================================================================

Summary
	Resolve an operand using opcode bit 5/6/7. Performs (opcode AND <mask>) to set Z, then
	jumps to script_operand_type_check to load either an immediate byte or a
	variable value.

Returns
	A  						Operand value (immediate or game_vars[index])

Global Inputs
	opcode                	source of operand-kind bits

Description
	- AND opcode with $40 to test bit 6.
	- If result != 0 (Z=0): variable path; else immediate path.
	- Tail-calls script_operand_type_check to perform the actual load.
================================================================================
*/
* = $5F88
script_load_operand_bit5:
        // ------------------------------------------------------------
        // Load operand selected by opcode bit5.
        // AND #$20 → Z=0 means bit5 set → use variable; Z=1 → immediate.
        // Falls into script_operand_type_check to dispatch.
        // ------------------------------------------------------------
        lda     opcode
        and     #OPCODE_KIND_BIT5_MASK                           
        jmp     script_operand_type_check

* = $5F90
script_load_operand_bit6:
        // ------------------------------------------------------------
        // Load operand selected by opcode bit6.
        // AND #$40 → Z=0 means bit6 set → use variable; Z=1 → immediate.
        // Falls into script_operand_type_check to dispatch.
        // ------------------------------------------------------------
        lda     opcode
        and     #OPCODE_KIND_BIT6_MASK                           
        jmp     script_operand_type_check

* = $5F98
script_load_operand_bit7:
        // ------------------------------------------------------------
        // Load operand selected by opcode bit7.
        // AND #$80 → Z=0 means bit7 set → use variable; Z=1 → immediate.
        // Falls into script_operand_type_check to dispatch.
        // ------------------------------------------------------------
        lda     opcode
        and     #OPCODE_KIND_BIT7_MASK                           
/*
================================================================================
script_operand_type_check
================================================================================
Summary
	Dispatch to either immediate- or variable-operand loader based on Z from the
	preceding AND of opcode with a kind mask. If Z=1 → immediate; if Z=0 → variable.
	Returns the resolved operand in A.

Returns
	A  			Operand value (immediate byte or game_vars[index])

Global Inputs
	game_vars[]  variable table read when loading by index

Description
	- If Z=1: read immediate byte from script and return it in A.
	- If Z=0: read variable index from script, load game_vars[index] into A.
================================================================================
*/
* = $5F9D
script_operand_type_check:
        // ------------------------------------------------------------
        // Branch on Z 
        // Z=0 → variable path; Z=1 → immediate path.
        // ------------------------------------------------------------
        bne     script_load_variable

script_load_immediate:
        // ------------------------------------------------------------
        // Immediate: read byte from script → A, return.
        // ------------------------------------------------------------
        jsr     script_read_byte
        rts

script_load_variable:
        // ------------------------------------------------------------
        // Variable: read var index from script, load game_vars[X] → A.
        // ------------------------------------------------------------
        jsr     script_read_byte
        tax
        lda     game_vars,x
        rts
/*
================================================================================
  script_read_object_attributes
================================================================================

Summary
	Return an object’s attribute byte in A. The operand source is selected by
	opcode bit 6: when set, use the active direct object index; when clear, read
	an object index from the script.

Returns
	A  							Attribute byte for the selected object

Global Inputs
	opcode                     bit6 selects operand source
	active_do_id_lo            index of current direct object when bit6=1
	object_attributes[]        attribute table indexed by object id

Global Outputs
	item_index_saved           stores the resolved object index for later use

Description
	* Test opcode bit6.
	* If set: Y := active DO index.
	* If clear: Y := next script byte.
	* Save Y into item_index_saved.
	* Load A := object_attributes[Y] and return.
================================================================================
*/
* = $5FAB
script_read_object_attributes:
        // ------------------------------------------------------------
        // Return object's attributes in A.
        // If opcode bit6=1 → use current direct object index.
        // If opcode bit6=0 → read object index from script.
        // ------------------------------------------------------------
        lda     opcode
        and     #OPCODE_KIND_BIT6_MASK                          
        beq     bit6_clear

bit6_set:
        // ------------------------------------------------------------
        // Bit6 set → use active direct object id as index.
        // ------------------------------------------------------------
        ldy     active_do_id_lo
        jmp     get_object_attributes

bit6_clear:
        // ------------------------------------------------------------
        // Bit6 clear → read object index from script → Y.
        // ------------------------------------------------------------
        jsr     script_read_byte
        tay

get_object_attributes:
        // ------------------------------------------------------------
        // Save index and fetch attributes → A, then return.
        // ------------------------------------------------------------
        sty     item_index_saved
        lda     object_attributes,y
        rts

/*
================================================================================
script_set_object_attributes
================================================================================

Summary
	Write the attribute byte in A to the object indexed by item_index_saved, then
	reset the camera position to the default.

Arguments
	item_index_saved  object index (Y destination for attribute write)
	A                 attribute byte to store

Global Outputs
	object_attributes[]  updated at [item_index_saved] with A
	cam_current_pos      set to CAM_DEFAULT_POS

Description
	- Load object index from item_index_saved into Y.
	- Store A into object_attributes[Y].
	- Set cam_current_pos to CAM_DEFAULT_POS.
================================================================================
*/
* = $5FC3
script_set_object_attributes:
        // ------------------------------------------------------------
        // Fetch object index saved earlier → Y
        // ------------------------------------------------------------
        ldy     item_index_saved

        // ------------------------------------------------------------
        // Write attributes in A to object_attributes[Y]
        // ------------------------------------------------------------
        sta     object_attributes,y

        // ------------------------------------------------------------
        // Reset camera position to default ($C8), then return
        // ------------------------------------------------------------
        lda     #CAM_DEFAULT_POS
        sta     cam_current_pos
        rts
/*
================================================================================
find_free_inventory_slot
================================================================================
Summary
	Scan inventory slots from index $01 upward and return the first free slot.

Returns
	Y  Free slot index on success

Global Inputs
	inventory_objects[]     slot contents (free iff byte == $00)

Description
	- Initialize Y := $01.
	- Loop: if inventory_objects[Y] == $00 then return with Y.
	- Else increment Y and continue while Y < INVENTORY_SLOTS.
	- Note: if no free slot exists, control falls through into the next routine.
================================================================================
*/
* = $5FCF
find_free_inventory_slot:
        // ------------------------------------------------------------
        // Use Y as the inventory slot cursor starting at #$01.
        // Scan upward until a slot holding #$00 is found.
        // On success: Y = free slot index; returns immediately.
        // ------------------------------------------------------------
        ldy     #$01

check_if_slot_is_free:
        // ------------------------------------------------------------
        // Slot free? inventory_objects[Y] == #$00 → found.
        // ------------------------------------------------------------
        lda     inventory_objects,y
        bne     next_slot
        rts

next_slot:
        // ------------------------------------------------------------
        // Advance to next slot; continue while Y < #$2D.
        // ------------------------------------------------------------
        iny
        cpy     #INVENTORY_SLOTS
        bne     check_if_slot_is_free

        // ------------------------------------------------------------
        // BUG: No free slot case falls through into the next routine.
        // This path should return an error or set a sentinel.
        // ------------------------------------------------------------
/*
================================================================================
  compute_actors_distance 
================================================================================
Summary
	Compute a distance between two actors with Y scaled by 1/4. 
	Return DISTANCE_INVALID when the actors are not in the same valid room.

Returns
	A  						Distance value
							$FF if rooms differ or room is invalid.

Global Inputs
	ref_costume_idx        reference costume index
	cur_costume_idx        current costume index
	current_room           active room index
	costume_room_idx[]     room index per costume
	actor_for_costume[]    actor index per costume
	actor_pos_x[]          live X for actor
	actor_pos_y[]          live Y for actor
	costume_target_x[]       destination X for costume
	costume_target_y[]       destination Y for costume

Global Outputs
	ref_actor_x            overwritten with |Δx|
	ref_actor_y            overwritten with (|Δy| >> 2)
	cur_actor_x            loaded with chosen current X (live or dest)
	cur_actor_y            loaded with chosen current Y (live or dest)

Description
	* Compare costume_room_idx for both costumes; if different, return $FF.
	* Reject room index $00 as invalid.
	* If room equals current_room use live actor positions; else use destinations.
	* Compute Δx := |cur_x − ref_x| and Δy := |cur_y − ref_y|, then Δy := Δy/4.
	* If Δy ≤ Δx → return (Δy * 2) + Δx; else return (Δx * 2) + Δy.
================================================================================
*/
* = $5FDC
compute_actors_distance:
		// ------------------------------------------------------------
		// Same-room check
		// Compare room index of reference vs current costume. If equal,
		// continue with room validity; otherwise caller returns #$FF.
		// ------------------------------------------------------------
        ldx     ref_costume_idx                // X := reference costume index
        lda     costume_room_idx,x             // A := reference costume's room index
        ldx     cur_costume_idx                // X := current costume index
        cmp     costume_room_idx,x             // compare both room indices
        beq     valid_room_check               // branch if same room
		
		// Not in the same room - return invalid distance
        lda     #DISTANCE_INVALID              
        rts                                   

valid_room_check:
        // ------------------------------------------------------------
        // Room valid? Zero means invalid.
        // ------------------------------------------------------------
        cmp     #ROOM_INDEX_INVALID
        bne     select_position_source

        // Invalid room → return invalid distance
        lda     #DISTANCE_INVALID
        rts

select_position_source:
        // ------------------------------------------------------------
        // If both in current_room use live XY. Otherwise use destinations.
        // ------------------------------------------------------------
        cmp     current_room
        bne     use_dest_coords

        ldx     cur_costume_idx                // X := current costume index
        ldy     actor_for_costume,x            // Y := actor index linked to this costume
		
        lda     actor_pos_x,y                  
        sta     cur_actor_x                    
        lda     actor_pos_y,y                  
        sta     cur_actor_y                    

        ldx     ref_costume_idx                // X := reference costume index
        ldy     actor_for_costume,x            // Y := actor index linked to this costume
		
        lda     actor_pos_x,y                  
        sta     ref_actor_x                    
        lda     actor_pos_y,y                  
        sta     ref_actor_y                    
        jmp     abs_dx                         

use_dest_coords:
        // ------------------------------------------------------------
        // Different from current room → use destination coordinates.
        // ------------------------------------------------------------
        ldx     cur_costume_idx                // X := current costume index
		
        lda     costume_target_x,x               
        sta     cur_actor_x                    
        lda     costume_target_y,x               
        sta     cur_actor_y                    

        ldx     ref_costume_idx                // X := reference costume index
		
        lda     costume_target_x,x               
        sta     ref_actor_x                    
        lda     costume_target_y,x               
        sta     ref_actor_y                    

abs_dx:
        // ------------------------------------------------------------
        // Δx := |cur_actor_x − ref_actor_x|
        // ------------------------------------------------------------
        lda     cur_actor_x                    // A := current actor X
        sec                                    // prepare for subtraction
        sbc     ref_actor_x                    // A := cur_x - ref_x
        bcs     commit_dx                      // if result ≥ 0, skip negation
		
        eor     #$ff                           // invert bits for two’s complement
        clc                                    // clear carry for +1
        adc     #$01                           // A := |Δx|
commit_dx:
        sta     ref_actor_x

        // ------------------------------------------------------------
        // Δy := |cur_actor_y − ref_actor_y|, then Δy := Δy / 4
        // ------------------------------------------------------------
        lda     cur_actor_y                    // A := current actor Y
        sec                                    // prepare for subtraction
        sbc     ref_actor_y                    // A := cur_y - ref_y
        bcs     commit_dy                      // if result ≥ 0, skip negation
		
        eor     #$ff                           // invert bits for two’s complement
        clc                                    // clear carry for +1
        adc     #$01                           // A := |Δy|
commit_dy:
        lsr                                    // divide Δy by 2 (first shift)
        lsr                                    // divide Δy by 4 (second shift)
        sta     ref_actor_y                    // store scaled Y distance

        // ------------------------------------------------------------
        // If Δy ≤ Δx → distance = (Δy * 2) + Δx
        // Else         distance = (Δx * 2) + Δy
        // ------------------------------------------------------------
        cmp     ref_actor_x                    // compare Δy against Δx
        bcc     dy_le_dx_path                  // C=0 when Δy < Δx → take ≤ path (handled with BEQ below)
        beq     dy_le_dx_path                  // Δy == Δx → take ≤ path

        // Δy > Δx → (Δx * 2) + Δy
        lda     ref_actor_x                    // A := Δx
        asl                                    // A := Δx * 2
        clc                                    
        adc     ref_actor_y                    // A := (Δx * 2) + Δy
        jmp     return_dist                    // done → return distance

dy_le_dx_path:
        // Δy ≤ Δx → (Δy * 2) + Δx
        lda     ref_actor_y                    // A := Δy
        asl                                    // A := Δy * 2
        clc                                    
        adc     ref_actor_x                    // A := (Δy * 2) + Δx

return_dist:
        rts
/*
================================================================================
  handle_paused_tasks
================================================================================

Summary
	Advance each paused task’s 24-bit pause counter (counter_3 → counter_2 → counter_1
	with carry propagation). When counter_1 rolls to $00 the task transitions from
	PAUSED to RUNNING.

Global Inputs
	task_state_tbl            read to test TASK_STATE_PAUSED per slot
	task_pause_counter_1      high byte of 24-bit pause counter
	task_pause_counter_2      mid  byte of 24-bit pause counter
	task_pause_counter_3      low  byte of 24-bit pause counter

Global Outputs
	task_pause_counter_3      incremented each tick; carries into counter_2
	task_pause_counter_2      incremented on carry from counter_3; may carry onward
	task_pause_counter_1      incremented on carry from counter_2; if rolls to $00,
								unpauses task
	task_state_tbl            set to TASK_STATE_RUNNING when counter_1 hits $00

Description
	* Iterate X from TASK_MAX_INDEX down to $01, skipping non-paused slots.
	* Increment counter_3; if it rolls to $00, increment counter_2; if that rolls to
	  $00, increment counter_1.
	* If counter_1 rolls to $00, mark the task as RUNNING.
================================================================================
*/
* = $6064
handle_paused_tasks:
        // ------------------------------------------------------------
        // Iterate tasks from X=#$0F down to #$01
        // ------------------------------------------------------------
        ldx     #TASK_MAX_INDEX

guard_if_paused:
        // ------------------------------------------------------------
        // Paused? task_state_tbl[X] == #$01 → advance 24-bit counter
        // ------------------------------------------------------------
        lda     task_state_tbl,x
        cmp     #TASK_STATE_PAUSED
        bne     next_task_p

        // ------------------------------------------------------------
        // counter3++ ; if rolled to #$00 → carry into counter2
        // ------------------------------------------------------------
        inc     task_pause_counter_3,x
        bne     next_task_p

        // ------------------------------------------------------------
        // counter2++ ; if rolled to #$00 → carry into counter1
        // ------------------------------------------------------------
        inc     task_pause_counter_2,x
        bne     next_task_p

        // ------------------------------------------------------------
        // counter1++ ; if rolled to #$00 → done → unpause
        // ------------------------------------------------------------
        inc     task_pause_counter_1,x
        bne     next_task_p

        // ------------------------------------------------------------
        // MSB reached #$00 → unpause: state := RUNNING
        // ------------------------------------------------------------
        lda     #TASK_STATE_RUNNING
        sta     task_state_tbl,x

next_task_p:
        // ------------------------------------------------------------
        // Next task; stop when X wraps to #$00
        // ------------------------------------------------------------
        dex
        bne     guard_if_paused
        rts
/*
================================================================================
  get_script_object_for_sentence
================================================================================
Summary
	Selects which object ID pair (lo/hi) to use for a queued sentence based on a
	mode byte read from the script, or an immediate low byte plus opcode bit7.

Returns
	Writes result to memory:
		low_byte  := selected object ID low byte
		hi_byte   := selected object ID high byte

Global Inputs
	active_io_id_lo/active_io_id_hi     current active indirect object ID
	active_do_id_lo/active_do_id_hi     current active direct object ID
	input_opcode                        opcode snapshot; bit7 selects hi for
										immediate-low mode
Global Outputs
	low_byte                             result low byte
	hi_byte                              result high byte

Description
	* Read mode from script:
		* If mode == $FF → return active indirect object (IO) lo/hi.
		* If mode == $FE → return active direct object (DO) lo/hi.
		* Otherwise      → use mode as low byte; set hi to 1 if input_opcode bit7=1,
		else hi=0.
================================================================================
*/
* = $6085
get_script_object_for_sentence:
        // Read mode selector byte from script → A
        jsr     script_read_byte               // A := mode
        cmp     #SENTENCE_OBJ_ACTIVE_IO        // A == active IO?
        bne     mode_fe_direct_object          // no → check DO case

        // ------------------------------------------------------------
		// Mode IO → return active indirect object (IO)
        // ------------------------------------------------------------
		lda     active_io_id_lo                // A := IO.lo
        sta     low_byte                       // save low result
        lda     active_io_id_hi                // A := IO.hi
        sta     hi_byte                        // save high result
        rts                                    // done

mode_fe_direct_object:
        cmp     #SENTENCE_OBJ_ACTIVE_DO        // A == #$FE (active DO)?
        bne     mode_other_immediate_low       // no → treat A as low index
		
        // ------------------------------------------------------------
        // Mode DO → return active direct object (DO)
        // ------------------------------------------------------------
        lda     active_do_id_lo                // A := DO.lo
        sta     low_byte                       // save low result
        lda     active_do_id_hi                // A := DO.hi
        sta     hi_byte                        // save high result
        rts                                    // done

mode_other_immediate_low:
        // ------------------------------------------------------------
        // Other mode → use byte0 as low index; hi from opcode bit7
        // ------------------------------------------------------------
        sta     low_byte                       // low := immediate byte0

        // If input_opcode bit7 set → hi := #$01 else #$00
        lda     input_opcode                   // A := opcode snapshot
        bpl     set_hi_when_bit7_clear         // bit7 clear → hi := 0

        lda     #$01                           // bit7 set → hi := 1
        jmp     commit_hi_result               // commit hi and return

set_hi_when_bit7_clear:
        lda     #$00                           // hi := 0 when bit7 clear

commit_hi_result:
        sta     hi_byte                        // store hi result
        rts                                    // return lo/hi pair

/*
Pseudo-code

function script_compare_operands():
    // Read first operand index from script
    var_index = script_read_byte()

    // Load second operand using opcode bit7 (variable or immediate)
    rhs = script_load_operand_bit7()
    rhs_operand = rhs

    // Compare game_vars[var_index] vs rhs_operand using CMP semantics
    left  = game_vars[var_index]
    cmp_result = left - rhs_operand

    // Set flags as CMP would:
    // C = (left >= rhs_operand)
    // Z = (left == rhs_operand)
    // N = sign bit of (left - rhs_operand)
    set_flags_from_subtraction(left, rhs_operand)

    return  // result is only in flags


function script_load_operand_bit5() -> byte:
    // Bit5 decides: 1 = variable, 0 = immediate
    masked = opcode AND OPCODE_KIND_BIT5_MASK   // $20

    // Z flag = 1 if masked == 0
    return script_operand_type_check(masked != 0)


function script_load_operand_bit6() -> byte:
    // Bit6 decides: 1 = variable, 0 = immediate
    masked = opcode AND OPCODE_KIND_BIT6_MASK   // $40

    // Z flag = 1 if masked == 0
    return script_operand_type_check(masked != 0)


function script_load_operand_bit7() -> byte:
    // Bit7 decides: 1 = variable, 0 = immediate
    masked = opcode AND OPCODE_KIND_BIT7_MASK   // $80

    // Z flag = 1 if masked == 0
    return script_operand_type_check(masked != 0)


// Called conceptually as: script_operand_type_check(is_variable: bool)
// In the real code, is_variable is inferred from the Z flag.
function script_operand_type_check(is_variable: bool) -> byte:
    if is_variable:
        // Variable path: index from script, then load from game_vars[]
        index = script_read_byte()
        return game_vars[index]
    else:
        // Immediate path: read literal directly from script
        value = script_read_byte()
        return value


function script_read_object_attributes() -> byte:
    // Decide where object index comes from based on opcode bit6
    if (opcode AND OPCODE_KIND_BIT6_MASK) != 0:
        // Bit6 set → use active DO index
        obj_index = active_do_id_lo
    else:
        // Bit6 clear → read object index from script
        obj_index = script_read_byte()

    // Save index for later writes
    item_index_saved = obj_index

    // Fetch attribute byte for that object
    attr = object_attributes[obj_index]
    return attr


function script_set_object_attributes(attr_byte: byte):
    // Use the previously saved object index
    obj_index = item_index_saved

    // Write attribute back to table
    object_attributes[obj_index] = attr_byte

    // Reset camera to default position
    cam_current_pos = CAM_DEFAULT_POS


// Returns: Y = index of first free slot (byte == 0)
// If no free slot exists, it falls through into the next routine (BUG).
function find_free_inventory_slot() -> (found: bool, slot_index: byte):
    slot = 1   // Start at slot 1, slot 0 reserved

    while slot < INVENTORY_SLOTS:
        if inventory_objects[slot] == 0:
            // Found free slot
            return (true, slot)

        slot += 1

    // BUG: no explicit failure handling here.
    // In the original code, control falls into the next routine.
    // Conceptually, caller should treat this as "no free slot".
    return (false, UNUSED)


// Uses ref_costume_idx and cur_costume_idx globals as inputs.
// Returns: distance (0..255) in A; DISTANCE_INVALID ($FF) if invalid.
function compute_actors_distance() -> byte:
    ref_idx = ref_costume_idx
    cur_idx = cur_costume_idx

    ref_room = costume_room_idx[ref_idx]
    cur_room = costume_room_idx[cur_idx]

    // 1) Same-room check
    if ref_room != cur_room:
        return DISTANCE_INVALID

    // 2) Room validity check (room index 0 is considered invalid)
    if ref_room == ROOM_INDEX_INVALID:
        return DISTANCE_INVALID

    // 3) Choose position source:
    //    - If in current_room: use live actor positions.
    //    - Else: use destination (target) positions.
    if ref_room == current_room:
        // Use live actor positions
        cur_actor = actor_for_costume[cur_idx]
        ref_actor = actor_for_costume[ref_idx]

        cur_x = actor_pos_x[cur_actor]
        cur_y = actor_pos_y[cur_actor]
        ref_x = actor_pos_x[ref_actor]
        ref_y = actor_pos_y[ref_actor]
    else:
        // Use destination coords for off-screen rooms
        cur_x = costume_target_x[cur_idx]
        cur_y = costume_target_y[cur_idx]
        ref_x = costume_target_x[ref_idx]
        ref_y = costume_target_y[ref_idx]

    // Store to scratch globals as in code
    cur_actor_x = cur_x
    cur_actor_y = cur_y
    ref_actor_x = ref_x
    ref_actor_y = ref_y

    // 4) Compute |Δx|
    dx = cur_x - ref_x
    if dx < 0:
        dx = -dx
    ref_actor_x = dx      // overwrite as scratch

    // 5) Compute |Δy| then scale by 1/4
    dy = cur_y - ref_y
    if dy < 0:
        dy = -dy
    dy = dy >> 2          // divide by 4
    ref_actor_y = dy      // overwrite as scratch

    // 6) Anisotropic distance:
    //    If dy ≤ dx → distance = (dy * 2) + dx
    //    Else        distance = (dx * 2) + dy
    if dy <= dx:
        distance = (dy * 2) + dx
    else:
        distance = (dx * 2) + dy

    return distance


// Advances 24-bit pause counters for PAUSED tasks.
// If the high byte rolls to 0, the task becomes RUNNING.
function handle_paused_tasks():
    x = TASK_MAX_INDEX  // highest task id

    while x != 0:
        if task_state_tbl[x] == TASK_STATE_PAUSED:
            // counter_3 (low byte)++
            task_pause_counter_3[x] += 1

            if task_pause_counter_3[x] == 0:
                // Carry into counter_2
                task_pause_counter_2[x] += 1

                if task_pause_counter_2[x] == 0:
                    // Carry into counter_1
                    task_pause_counter_1[x] += 1

                    // If MSB rolled back to 0, unpause
                    if task_pause_counter_1[x] == 0:
                        task_state_tbl[x] = TASK_STATE_RUNNING

        x -= 1  // next lower task


// Resolves an object id pair (lo, hi) for a sentence.
// The script supplies a "mode" byte:
//   $FF → active IO
//   $FE → active DO
//   else: mode is low byte; hi is 0 or 1 based on input_opcode bit7.
function get_script_object_for_sentence():
    mode = script_read_byte()

    if mode == SENTENCE_OBJ_ACTIVE_IO:
        // Use active indirect object
        low_byte  = active_io_id_lo
        hi_byte   = active_io_id_hi
        return

    if mode == SENTENCE_OBJ_ACTIVE_DO:
        // Use active direct object
        low_byte  = active_do_id_lo
        hi_byte   = active_do_id_hi
        return

    // Otherwise, treat mode as an immediate low byte
    low_byte = mode

    // Bit7 of input_opcode chooses high byte: 0 or 1
    if (input_opcode & 0x80) != 0:
        hi_byte = 0x01
    else:
        hi_byte = 0x00

    return
*/