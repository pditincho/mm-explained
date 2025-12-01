#importonce

#import "globals.inc"
#import "constants.inc"
#import "sentence_action.asm"
#import "shutter.asm"
#import "view.asm"

/*
================================================================================
  mem_fill_x_1
================================================================================
Summary
	Fill a contiguous memory region starting at fill_dest_ptr with the
	byte value in X, decrementing fill_byte_cnt until the entire range
	has been written.

Arguments
	.X 				Byte value to store into each location
	fill_dest_ptr 	Zero-page pointer to start of destination range
	fill_byte_cnt 	16-bit byte count for number of bytes to fill

Returns
	None (destination range is filled; registers are clobbered)

Description
	- Keeps Y fixed at zero and uses (fill_dest_ptr),Y as the store target.
	- Writes the fill value from X into each successive byte of the range.
	- Increments fill_dest_ptr after each store, handling low-byte wrap to
	the high byte to traverse the 16-bit address space.
	- Decrements the 16-bit fill_byte_cnt for each byte written and exits
	when both low and high bytes reach zero.
================================================================================
*/
* = $0435
mem_fill_x_1:
        // Keep Y at 0; advance fill_dest_ptr instead of Y
        ldy     #$00

        // Copy fill value from X into A
        txa

        // Store fill value at [fill_dest_ptr]
        sta     (fill_dest_ptr),y

        // Increment low byte of fill_dest_ptr; on wrap, bump high byte
        inc     fill_dest_ptr
        bne     counter_decrement
        inc     fill_dest_ptr + 1

counter_decrement:
        // Decrement 16-bit fill_byte_cnt (hi then lo)
        lda     fill_byte_cnt
        bne     counter_lo_decrement
        dec     fill_byte_cnt + 1

counter_lo_decrement:
        dec     fill_byte_cnt

        // OR both counter bytes; zero only when both reached 0
        lda     fill_byte_cnt
        ora     fill_byte_cnt + 1
        bne     mem_fill_x_1

        // Done filling range
        rts
/*
================================================================================
  alternate_frame_buffer
================================================================================
Summary
	Toggle the active frame buffer and video mode when environment lights
	are ON. If lights are not ON, exit without changes.

Global Inputs
	global_lights_state    current environment lights state (checked for ON)
	frame_buffer           current buffer selector (#$01 or other)

Global Outputs
	frame_buffer_base      updated to VIEW_FRAME_BUF_0 ($C828) or VIEW_FRAME_BUF_1 ($CC28)
	video_setup_mode       updated to VID_SETUP_CC00 or VID_SETUP_C800
	frame_buffer           updated to #$02 when switching to buffer 2, or #$01 when switching to buffer 1

Description
	- If global_lights_state != ON, return immediately.
	- If frame_buffer == #$01, switch to buffer 2:
		• frame_buffer_base := VIEW_FRAME_BUF_0 ($C828)
		• video_setup_mode  := VID_SETUP_CC00
		• frame_buffer      := #$02
	- Else switch to buffer 1:
		• frame_buffer_base := VIEW_FRAME_BUF_1 ($CC28)
		• video_setup_mode  := VID_SETUP_C800
		• frame_buffer      := #$01
================================================================================
*/
* = $074E
alternate_frame_buffer:
        // ------------------------------------------------------------
        // If environment lights are not ON (#$02), do nothing.
        // ------------------------------------------------------------
        lda     global_lights_state
        cmp     #LIGHTS_ENVIRONMENT_ON
        bne     exit

        // ------------------------------------------------------------
        // Toggle frame buffer and video mode based on current selector.
        // If frame_buffer == #$01 → switch to buffer 2 (CC00 base).
        // Else → switch to buffer 1 (C800 base).
        // ------------------------------------------------------------
        lda     frame_buffer
        cmp     #$01
        bne     switch_to_frame_buffer_1

* = $075B
switch_to_frame_buffer_2:
        // frame_buffer_base := $C828; video_setup_mode := #$02; frame_buffer := #$02
        lda     #<VIEW_FRAME_BUF_0
        sta     frame_buffer_base
        lda     #>VIEW_FRAME_BUF_0
        sta     frame_buffer_base + 1

        lda     #VID_SETUP_CC00
        sta     video_setup_mode
        sta     frame_buffer
        jmp     exit

switch_to_frame_buffer_1:
        // frame_buffer_base := $CC28; video_setup_mode := #$01; frame_buffer := #$01
        lda     #<VIEW_FRAME_BUF_1
        sta     frame_buffer_base
        lda     #>VIEW_FRAME_BUF_1
        sta     frame_buffer_base + 1

        lda     #VID_SETUP_C800
        sta     video_setup_mode
        sta     frame_buffer

exit:
        rts
		
* = $077B		
do_nothing_077B:
		rts
/*
================================================================================
  handle_entity_approach_and_trigger
================================================================================
Summary
	Move the active actor toward the target entity and, upon proximity, set a
	contact sentinel so the queued verb handler executes on the next tick.

Global Inputs
	target_entity         target entity id (#$00 none, #$FE contact)
	target_obj_lo         low byte of target object id for routing
	target_obj_hi         high byte of target object id for routing
	current_kid_idx            active kid index
	actor_for_costume[]        map: costume index → actor index
	actor_motion_state[]       actor motion state array
	target_x, target_y             routed destination coordinates
	actor_pos_x[], actor_pos_y[] 	actor position arrays

Global Outputs
	target_entity          set to ENTITY_CONTACT_SENTINEL (#$FE) on contact,
								cleared to ENTITY_NONE (#$00) when out of range

Description
	- Guard: exit if no destination entity is set.
	- Resolve actor for the current kid. Exit if the actor is not stopped
	(actor_motion_state ≠ ACTOR_STATE_STOPPED).
	- If target_entity == ENTITY_CONTACT_SENTINEL, clear it and jump to
	execute_verb_handler_for_object (consumes the queued action).
	- Otherwise:
		• Route destination by entity type → target_x/target_y.
		• Compute |ΔX| = |target_x − actor_x|:
			- If |ΔX| > CONTACT_X_THRESHOLD, clear flag and return.
			- If |ΔX| ≤ CONTACT_X_THRESHOLD, continue.
		• Compute |ΔY| = |target_y − actor_y|:
			- If |ΔY| > CONTACT_Y_THRESHOLD, clear flag and return.
			- If |ΔY| ≤ CONTACT_Y_THRESHOLD:
				· face_toward_target
				· target_entity := ENTITY_CONTACT_SENTINEL (contact latched)
================================================================================
*/

.const ENTITY_CONTACT_SENTINEL    = $FE    // Contact reached; trigger on next tick
.const CONTACT_X_THRESHOLD        = $04    // |ΔX| clamp for contact check
.const CONTACT_Y_THRESHOLD        = $08    // |ΔY| clamp for contact check

* = $0CD0
handle_entity_approach_and_trigger:
        // ----------------------------------------------------
        // Guard: is there a destination entity?
        // If none, return_from_entity_contact immediately.
        // ----------------------------------------------------
        lda     target_entity
        bne     resolve_actor_and_validate_state
        jmp     return_from_entity_contact

resolve_actor_and_validate_state:
        // ----------------------------------------------------
        // Get actor index for the current kid
        // ----------------------------------------------------
        ldx     current_kid_idx
        lda     actor_for_costume,x
        tax

        // ----------------------------------------------------
        // return_from_entity_contact if actor is still moving
        // ----------------------------------------------------
        lda     actor_motion_state,x
        cmp     #ACTOR_STATE_STOPPED
        bne     return_from_entity_contact

        // ----------------------------------------------------
        // If entity already marked as contacted (#$FE),
        // clear flag and run queued action.
        // ----------------------------------------------------
        lda     target_entity
        cmp     #ENTITY_CONTACT_SENTINEL
        bne     route_and_check_proximity

        lda     #ENTITY_NONE
        sta     target_entity
        jmp     execute_verb_handler_for_object

route_and_check_proximity:
        // ----------------------------------------------------
        // Route destination coordinates based on entity type
        // ----------------------------------------------------
        ldx     target_obj_lo
        lda     target_obj_hi
        ldy     target_entity
        jsr     set_approach_point

        // ----------------------------------------------------
        // Re-resolve actor for current kid
        // ----------------------------------------------------
        ldx     current_kid_idx
        lda     actor_for_costume,x
        tax

        // ----------------------------------------------------
        // Compute |ΔX| between actor and destination
        // ----------------------------------------------------
        lda     target_x
        sec
        sbc     actor_pos_x,x
        bcs     evaluate_x_threshold
        eor     #$FF
        clc
        adc     #$01

evaluate_x_threshold:
        // If |ΔX| == 4 → continue to Y
        // If |ΔX| > 4  → out of range → clear flag & return_from_entity_contact
        // If |ΔX| < 4  → close enough → check Y
        cmp     #CONTACT_X_THRESHOLD
        beq     compute_y_distance
        bcs     clear_contact_flag_far_x
        // else < threshold → check Y

compute_y_distance:
        // ----------------------------------------------------
        // Compute |ΔY| between actor and destination
        // ----------------------------------------------------
        lda     target_y
        sec
        sbc     actor_pos_y,x
        bcs     evaluate_y_threshold
        eor     #$FF
        clc
        adc     #$01

evaluate_y_threshold:
        // If |ΔY| == 8 → set facing & mark contact
        // If |ΔY| > 8  → out of range → clear flag & return_from_entity_contact
        // If |ΔY| < 8  → set facing & mark contact
        cmp     #CONTACT_Y_THRESHOLD
        beq     align_facing_and_mark_contact
        bcs     clear_contact_flag_far_y

align_facing_and_mark_contact:
        // ----------------------------------------------------
        // Set facing toward entity and mark contact (#$FE)
        // ----------------------------------------------------
        jsr     face_toward_target
        lda     #ENTITY_CONTACT_SENTINEL
        sta     target_entity
        jmp     return_from_entity_contact

clear_contact_flag_far_y:
        lda     #ENTITY_NONE
        sta     target_entity
        jmp     return_from_entity_contact

clear_contact_flag_far_x:
        lda     #ENTITY_NONE
        sta     target_entity

return_from_entity_contact:
        rts

/*
================================================================================
  copy_vic_color_ram_wrapper
================================================================================
Summary
	Temporarily map in I/O, invoke the color RAM copy routine, then restore
	the prior memory configuration.

Global Inputs
	(via callee) prepared color source buffer  bytes to be written to $D800

Global Outputs
	VIC color RAM ($D800)  updated by copy_vic_color_ram

Description
	- Write $25 to cpu_port to map in I/O, enabling access to VIC/Color RAM.
	- Call copy_vic_color_ram to transfer prepared color bytes to $D800.
	- Write $24 to cpu_port to remap RAM and exit.
================================================================================
*/
* = $1854
copy_vic_color_ram_wrapper:
        // Map in I/O (enable VIC/Color RAM access)
        ldy     #MAP_IO_IN
        sty     cpu_port

        // Copy color RAM from prepared buffer
        jsr     copy_vic_color_ram

        // Map out I/O (restore RAM config)
        ldy     #MAP_IO_OUT
        sty     cpu_port
        rts
/*
================================================================================
  copy_vic_color_ram
================================================================================

Summary
	Copy 680 bytes from the scene color buffer ($6D89–$7030) to Color RAM
	($D828–$DACF) as 17 rows × 40 columns using one outer Y loop and 17
	unrolled LDA/STA pairs per column.

Global Inputs
	Source row bases                 $6D89,$6DB1,$6DD9,$6E01,$6E29,$6E51,$6E79,
									 $6EA1,$6EC9,$6EF1,$6F19,$6F41,$6F69,$6F91,
									 $6FB9,$6FE1,$7009  (each +$28 from prior)
									 
	Destination row bases            $D828,$D850,$D878,$D8A0,$D8C8,$D8F0,$D918,
									 $D940,$D968,$D990,$D9B8,$D9E0,$DA08,$DA30,
									 $DA58,$DA80,$DAA8  (each +$28 from prior)
Global Outputs
	Color RAM range                	 $D828–$DACF written (680 bytes)
	color_ram_copy_done        	 set to TRUE on completion
	
Description
	- Initializes Y to 39 and decrements to 0, copying one column per pass.
	- Uses abs,Y addressing so each of the 17 row bases contributes a byte for
	the current column, minimizing loop overhead.
	- Layout matches VIC Color RAM’s 40-column stride ($28), avoiding 16-bit
	pointer math and page-cross penalties typical of linear copies.

Notes
	- Color RAM uses only the low nibble; high nibble in source bytes is ignored.
	- Intended for vblank/top-of-frame band where the bulk copy will not cause
	visible artifacts.
================================================================================
  
Color RAM sits at $D800–$DBE7 and is accessed as I/O space. Writes are slower
than normal RAM and per-byte overhead dominates. This routine copies the scene’s
17 visible rows by fixing each row’s absolute base and using a single outer Y
loop as the column index.

Advantages of copying in this way:

	• Absolute,Y beats pointer math
		Using abs,Y (LDA/STA $addr,Y) avoids 16-bit pointer adds inside the loop. The
		17 source and 17 destination bases are constants spaced by $28 (40 columns).
		Only Y changes. This removes ADC/INC and page-cross bookkeeping.

	• One branch per column
		The loop body is fully unrolled across rows, so each column costs 17 loads and
		17 stores plus one DEY/BPL. Fewer branches means more predictable timing.

	• Page-crossing is contained
		With abs,Y the CPU handles page crosses automatically. No manual carry fixups
		and no extra instructions per row. Timing variance stays bounded and small.

	• Best fit for I/O write latency
		Color RAM stores only the low nibble, and writes incur I/O wait states. The
		unrolled pattern keeps a steady stream of STA $D8xx,Y without extra ALU work,
		maximizing useful bus cycles during vblank/top-band time.

	• Cache-free determinism
		There is no self-modifying code or ZP pointer churn. Cycle cost per column is
		consistent, which is important when scheduled near raster boundaries.

Trade-offs vs a 16-bit indexed copy:
	* More code bytes, fewer cycles. On a 6502 the cycle savings are worth the
	  bytes for hot paths like frame setup.
	* Hard-coded bases. This is acceptable because the scene area layout is fixed:
	  each successive row is previous +$28 on both source and destination.
	  
================================================================================
*/
* = $1860
copy_vic_color_ram:
        ldy     #VIEW_COL_MAX_IDX          // Y := 39 (start at last column)

copy_color_column:
        lda     $6D89,y                         // Row 0 source → $D828
        sta     $D828,y
        lda     $6DB1,y                         // Row 1
        sta     $D850,y
        lda     $6DD9,y                         // Row 2
        sta     $D878,y
        lda     $6E01,y                         // Row 3
        sta     $D8A0,y
        lda     $6E29,y                         // Row 4
        sta     $D8C8,y
        lda     $6E51,y                         // Row 5
        sta     $D8F0,y
        lda     $6E79,y                         // Row 6
        sta     $D918,y
        lda     $6EA1,y                         // Row 7
        sta     $D940,y
        lda     $6EC9,y                         // Row 8
        sta     $D968,y
        lda     $6EF1,y                         // Row 9
        sta     $D990,y
        lda     $6F19,y                         // Row 10
        sta     $D9B8,y
        lda     $6F41,y                         // Row 11
        sta     $D9E0,y
        lda     $6F69,y                         // Row 12
        sta     $DA08,y
        lda     $6F91,y                         // Row 13
        sta     $DA30,y
        lda     $6FB9,y                         // Row 14
        sta     $DA58,y
        lda     $6FE1,y                         // Row 15
        sta     $DA80,y
        lda     $7009,y                         // Row 16
        sta     $DAA8,y

        dey                                     // next column (Y := Y-1)
        bpl     copy_color_column               // repeat until Y < 0

        // ------------------------------------------------------------
        // Mark color RAM copy completed
        // ------------------------------------------------------------
        lda     #TRUE
        sta     color_ram_copy_done
        rts
/*
================================================================================
  execute_room_entry_script
================================================================================
Summary
	If room_entry_script_ptr is nonzero, classify task 0 as a room script,
	seed its PC offset from the entry pointer, and invoke it via
	invoke_task_then_resume_parent. Otherwise return immediately.

Returns
	If no entry script: returns immediately.
	If present: returns after invoke_task_then_resume_parent completes.

Global Inputs
	room_entry_script_ptr   entry script pointer for the current room

Global Outputs
	task_type_tbl           slot 0 set to SCRIPT_TYPE_ROOM
	task_pc_ofs_lo_tbl      slot 0 low byte set from room_entry_script_ptr
	task_pc_ofs_hi_tbl      slot 0 high byte set from room_entry_script_ptr

Description
	- Test room_entry_script_ptr for nonzero.
	- On zero: no entry script; exit.
	- On nonzero:
		• Write SCRIPT_TYPE_ROOM into task_type_tbl[0].
		• Copy room_entry_script_ptr into task_pc_ofs_{lo,hi}_tbl[0].
		• Set X=0 and call invoke_task_then_resume_parent to run the script.
================================================================================
*/
* = $3A7D
execute_room_entry_script:
        // Entry script pointer nonzero?
        lda     room_entry_script_ptr
        ora     room_entry_script_ptr + 1
        bne     entry_script_present
        rts                                     // none → exit

entry_script_present:
        // Set script type = room
        lda     #SCRIPT_TYPE_ROOM
        sta     task_type_tbl

        // Load script PC offset into task 0
        lda     room_entry_script_ptr
        sta     task_pc_ofs_lo_tbl
        lda     room_entry_script_ptr + 1
        sta     task_pc_ofs_hi_tbl

        // Launch script in task 0, then resume parent
        ldx     #$00
        jsr     invoke_task_then_resume_parent
        rts
/*
================================================================================
  execute_room_exit_script
================================================================================
Summary
	If room_exit_script_ptr is nonzero, assign task 0 as a room script,
	set its PC offset from the exit pointer, and invoke it using
	invoke_task_then_resume_parent. Otherwise return immediately.

Returns
	If no exit script: returns immediately.
	If present: returns after the invoked script finishes.

Global Inputs
	room_exit_script_ptr    exit script pointer for the current room

Global Outputs
	task_type_tbl           slot 0 set to SCRIPT_TYPE_ROOM
	task_pc_ofs_lo_tbl      slot 0 low byte set from room_exit_script_ptr
	task_pc_ofs_hi_tbl      slot 0 high byte set from room_exit_script_ptr

Description
	- Check if room_exit_script_ptr is nonzero.
	- If zero, exit immediately.
	- If nonzero:
		• Store SCRIPT_TYPE_ROOM into task_type_tbl[0].
		• Copy room_exit_script_ptr into task_pc_ofs_{lo,hi}_tbl[0].
		• Set X=0 and call invoke_task_then_resume_parent to execute the
		script for the room’s exit transition.
================================================================================
*/		
* = $3A9D		
execute_room_exit_script:
        // Check if an exit script pointer is nonzero
        lda     room_exit_script_ptr
        ora     room_exit_script_ptr + 1
        bne     exit_script_present
        rts                                     // none → return

exit_script_present:
        // Set script type to room script
        lda     #SCRIPT_TYPE_ROOM
        sta     task_type_tbl

        // Load script PC offset into task 0
        lda     room_exit_script_ptr
        sta     task_pc_ofs_lo_tbl
        lda     room_exit_script_ptr + 1
        sta     task_pc_ofs_hi_tbl

        // Launch the script in task 0, then resume parent
        ldx     #$00
        jsr     invoke_task_then_resume_parent
        rts		
/*
==============================================================================
Summary
	Find the first non-kid actor under the cursor by scanning actor slots from
	highest to lowest and testing an axis-aligned bounding box in engine units.

Global Inputs
	costume_for_actor[x]   			Usage flag (bit7) and actor id for slot x
	actor_pos_x[x]         			Actor left X edge (quarter-char units)
	actor_pos_y[x]         			Actor bottom Y (half-row off-by-8 domain)
	cursor_x_pos_quarter_absolute  	Cursor X (quarter-char units)
	cursor_y_pos_half_off_by_8     	Cursor Y (half-row off-by-8 domain)
	current_kid_idx        			Index of the player-controlled kid to exclude

Returns
	A  			FIND_ACTOR_FOUND 		if a non-kid actor is hit
				FIND_ACTOR_NOT_FOUND 	otherwise
	X  			Index of the hit costume on success; #$00 if no actor matches

Description
	- Initialize X to ACTOR_MAX_INDEX and iterate down to 0.
	- Skip unused slots where costume_for_actor bit7 = 1.
	- Horizontal hit if cursor X ∈ [actor_pos_x .. actor_pos_x+ACTOR_X_RIGHT_PAD].
	- Vertical hit if cursor Y ∈ [actor_pos_y−VIEW_COLS .. actor_pos_y],
	  with the lower bound clamped to CURSOR_Y_MIN_ROW on underflow.
	- Exclude current_kid_idx from hits.
	- On first qualifying hit: TAX to return the costume index and set A=FIND_ACTOR_FOUND.
	- If none match: return X=#$00 and A≠#$02.
==============================================================================
*/
.const ACTOR_X_RIGHT_PAD      = $03    // Inclusive right-edge pad (quarter-char units) for X hit test
.const CURSOR_Y_MIN_ROW       = $01    // Clamp for top bound after (actor_pos_y - VIEW_COLS) underflow
.const FIND_ACTOR_FOUND       = $02    // Return code in A when a valid actor is hit
.const FIND_ACTOR_NOT_FOUND   = $00    // Return code in A when no actor is hit

* = $3BFD
find_actor_under_cursor_excl_kid:
        // ------------------------------------------------------------
        // Iterate actors in X from #$03 down to #$00.
        // Skip unused actors (costume bit7=1). Stop on first hit.
        // ------------------------------------------------------------
        ldx     #ACTOR_MAX_INDEX              // X := highest actor index to test

test_actor_bounds:
        // ------------------------------------------------------------
        // Active actor? costume_for_actor bit7=0 means in use.
        // ------------------------------------------------------------
        lda     costume_for_actor,x           // A := costume flags for actor X
        bmi     step_prev_actor_or_exit       // bit7=1 → slot unused → try previous

        // ------------------------------------------------------------
        // Horizontal test: cursor_x_quarter must be within
        // [actor_pos_x .. actor_pos_x+ACTOR_X_RIGHT_PAD]
        // ------------------------------------------------------------
        lda     actor_pos_x,x                 // A := left X edge (quarter chars)
        cmp     cursor_x_pos_quarter_absolute // cursor left-of-or-on left edge?
        beq     check_x_right_edge            // equal → still possible, check right edge
        bcs     step_prev_actor_or_exit       // carry set → actor_pos_x > cursor_x → left of box

check_x_right_edge:
        clc                                   
        adc     #ACTOR_X_RIGHT_PAD            // compute inclusive right edge
        cmp     cursor_x_pos_quarter_absolute // cursor to the right of box?
        bcc     step_prev_actor_or_exit       // cursor_x > right edge → no hit

        // ------------------------------------------------------------
        // Vertical test: cursor_y_half_off8 must be within
        // [actor_pos_y-#VIEW_COLS .. actor_pos_y]
        // Clamp lower bound to CURSOR_Y_MIN_ROW if subtraction underflows.
        // ------------------------------------------------------------
        lda     actor_pos_y,x                 // A := bottom Y
        cmp     cursor_y_pos_half_off_by_8    // cursor below bottom?
        bcc     step_prev_actor_or_exit       // cursor_y > bottom → no hit

        sec                                   // compute bottom - height
        sbc     #VIEW_COLS                    // tentative top bound
        bpl     check_y_upper_bound           // no underflow → use computed top
        lda     #CURSOR_Y_MIN_ROW             // underflow → clamp top to minimal row

check_y_upper_bound:
        cmp     cursor_y_pos_half_off_by_8    // cursor above top?
        beq     accept_hit_if_not_kid         // on the top edge → accept path
        bcs     step_prev_actor_or_exit       // A >= cursor_y implies top >= cursor_y → cursor above

accept_hit_if_not_kid:
        // ------------------------------------------------------------
        // Inside actor box. Exclude current kid before accepting.
        // costume_for_actor is used as actor identifier here.
        // ------------------------------------------------------------
        lda     costume_for_actor,x           // A := costume for actor X
        cmp     current_kid_idx               // is this the player-controlled kid?
        beq     step_prev_actor_or_exit       // ignore current kid; continue scanning

        tax                                   // X := costume id to return
        lda     #FIND_ACTOR_FOUND             // A := success code
        rts                                   // found a non-kid actor under cursor

step_prev_actor_or_exit:
        dex                                   // X := X-1
        bpl     test_actor_bounds             // more actors to test? → loop

        // ------------------------------------------------------------
        // No actor matched. Return X=#$00, A left non-#$02 by design.
        // ------------------------------------------------------------
        ldx     #$00                          // canonical “no actor” index
        rts
/*
================================================================================
  find_object_at_cursor
================================================================================
Summary
    Identify the room object under the current cursor and return its global
    object index. Iterates room objects, filters removed mutable objects,
    resolves parent containment, then dispatches to a bounds hit test.

Arguments
    None (uses globals; cursor position consumed by the hit-test routine)

Returns
    On hit:
        A = obj_idx_hi
        X = obj_idx_lo
    On miss:
        X = #$00  

Description
    • Early-exit if there are no room objects.
    • Scan room indices from 1..room_obj_count inclusive.
    • Immutable objects (hi≠0) are always candidates.
    • Mutable objects are skipped if "removed from room" (attributes bit5).
    • If a parent link exists, compare parent "inside" flag (bit7) against the
      required overlay state; on match, promote parent and re-check to support
      multi-level ancestry.
    • On a candidate, tail-jump to the bounds routine which sets A/X on hit.

Vars/State
    candidate_object         Temp room-object index latched for parent/hit checks.
    ancestor_overlay_req     Temp required overlay/inside state for parent compare.

Global Inputs
    room_obj_count           Total room objects; determines loop bounds.
    room_obj_idx_hi/lo       Map room index → global object index; hi≠0 marks immutable.
    object_attributes        Per-object flags; bit5=removed, bit7=inside-parent.
    parent_idx_tbl           Room-indexed parent link; 0 means no parent.
    ancestor_overlay_req_tbl Required inside/overlay state for containment check.
    (cursor position)        Consumed by check_cursor_object_bounds.

Global Outputs
    candidate_object         Updated while scanning to hold current candidate.
    ancestor_overlay_req     Written with requirement byte for parent comparison.
================================================================================
*/
.label candidate_object = $3EA0
.const OBJ_SCAN_START_IDX              = $01    // First room object index to test
.const NO_OBJECT_IDX                   = $00    // X return value when no hit

* = $3C3E
find_object_at_cursor:
        // ------------------------------------------------------------
        // Check for presence of room objects
        //
        // - Loads the total room object count.
        // - If count = 0, return immediately (no scan required).
        // - Otherwise, branch to initialization of object scan loop.
        // ------------------------------------------------------------
        ldx     room_obj_count                 // Load room object count; if zero, no scan needed
        bne     init_object_scan               // If X != 0, branch to init; else return
        rts                                    // No objects in room → exit

        // ------------------------------------------------------------
        // Initialize room object scan
        //
        // - Begin iteration at the first valid room object index.
        // - Each loop checks whether the object is immutable (always
        //   present) or mutable (presence depends on attributes).
        // ------------------------------------------------------------
init_object_scan:
        ldx     #OBJ_SCAN_START_IDX            // Initialize scan at first testable room object index

scan_room_objects:
        lda     room_obj_idx_hi,x              // Check immutability: hi≠0 means object is always present
        beq     path_mutable_check             // If hi==0, object is mutable
        jmp     latch_candidate_room_idx       // Immutable → treat as candidate

        // ------------------------------------------------------------
        // Validate mutable object presence
        // - Map room index to global index
        // - Skip if removed-from-room flag (bit5) is set
        // ------------------------------------------------------------
path_mutable_check:
        ldy     room_obj_idx_lo,x              // Map room index X → global object index in Y
        lda     object_attributes,y            // Load global object attributes
        and     #ATTR_REMOVED_FROM_ROOM_MASK   // Mask “removed from room” bit5
        bne     move_to_next_object            // Skip if removed

        // ------------------------------------------------------------
        // Latch candidate room index
        // - Preserve current room object index for parent checks and
        //   bounds testing
        // ------------------------------------------------------------
latch_candidate_room_idx:
        stx     candidate_object               // Cache current room object index as candidate

        // ------------------------------------------------------------
        // Resolve parent containment
        // - Load required overlay/inside state for this object
        // - If a parent exists, confirm its “inside” flag matches the
        //   required state; on match, promote parent and re-check
        // - If no parent, proceed to bounds test
        // ------------------------------------------------------------
resolve_parent_containment:
        lda     ancestor_overlay_req_tbl,x     // Load required parent/overlay state
        sta     ancestor_overlay_req           // Save requirement for comparison
        lda     parent_idx_tbl,x               // Read parent link; zero means no parent
        bne     ancestor_link_present          // If parent exists, validate containment
        jmp     hit_test_candidate_bounds      // No parent → check cursor bounds

        // ------------------------------------------------------------
        // Validate ancestor link and promote if required
        // - Y := parent room index; map to parent’s global index in X
        // - Compare parent “inside” flag (bit7) with required overlay
        // - On mismatch, skip candidate; on match, promote parent and
        //   re-check to support multi-level ancestry
        // ------------------------------------------------------------
ancestor_link_present:
        tay                                    // Y := parent room index
        lda     room_obj_idx_lo,y              // Load parent’s global object index (lo)
        tax                                    // X := parent’s global object index

        lda     object_attributes,x            // Load parent’s attributes
        and     #ATTR_INSIDE_PARENT_MASK       // Isolate parent “inside” flag bit7
        cmp     ancestor_overlay_req           // Compare flag to required overlay state
        bne     skip_due_to_ancestor_req_mismatch // Mismatch → skip this candidate

        tya                                    // Match → promote parent as new subject
        tax                                    // X := parent index
        jmp     resolve_parent_containment     // Recurse to check multi-level ancestry

        // ------------------------------------------------------------
        // Skip candidate on ancestor requirement mismatch
        // - Restore the original candidate room index
        // - Continue scanning with the next room object
        // ------------------------------------------------------------
skip_due_to_ancestor_req_mismatch:
        ldx     candidate_object               // Restore original candidate room index
        jmp     move_to_next_object            // Skip to next object in outer scan

        // ------------------------------------------------------------
        // Hit-test candidate bounds against cursor
        // - Restore candidate room index
        // - Tail-jump to cursor containment test routine
        // ------------------------------------------------------------
hit_test_candidate_bounds:
        ldx     candidate_object               // Restore candidate room index
        jmp     check_cursor_object_bounds // Jump to cursor-in-bounds test

        // ------------------------------------------------------------
        // Advance to next room object
        // - Increment X and compare with room_obj_count
        // - Inclusive loop: if X == room_obj_count, iterate once more
        // ------------------------------------------------------------
move_to_next_object:
        inx                                    // Increment X (next room object index)
        cpx     room_obj_count                 // Compare with total object count
        bcc     scan_room_objects              // Continue if below limit
        beq     scan_room_objects              // Include final index (equal case)
		
        ldx     #NO_OBJECT_IDX                 // Exhausted all objects → X := #$00
        rts                                    // Return (no hit)
/*
================================================================================
  check_cursor_object_bounds
================================================================================
Summary
    Hit-test the current cursor against the bounds of the room object indexed
    by X. On containment, returns the global object index in A/X; otherwise
    tail-jumps to continue the outer scan.

Arguments
    X  Room object index (room-scoped) for the candidate under test.

Returns
On hit:
        A = obj_idx_hi
        X = obj_idx_lo
    On miss:
        Tail-jumps to move_to_next_object (no register guarantees)


Global Inputs
    cursor_y_pos_half_off_by_8  Half-resolution Y; requires >>2 to grid units.
    cursor_x_pos_quarter_absolute  Quarter-absolute X; already in grid units.
    obj_left_col_tbl            Object left column per room index.
    obj_width_tbl               Object width per room index.
    obj_top_row_tbl             Object top row per room index.
    obj_height_tbl              Object height per room index.
    room_obj_idx_hi             Global object index high byte per room index.
    room_obj_idx_lo             Global object index low byte per room index.

Vars/State
    cursor_y  Zero-page scratch for normalized cursor Y.
    cursor_x  Zero-page scratch for normalized cursor X.
	
Description
    • Normalize cursor coordinates to the object grid:
      - Y: shift half-off-by-8 value right twice.
      - X: use quarter-absolute value directly.
    • Test horizontal containment:
      - If cursor_x < left → miss.
      - If cursor_x == left → verify using right edge.
      - right = left + width; treat right == cursor_x as outside.
    • Test vertical containment with analogous rules:
      - bottom = top + height; bottom == cursor_y is outside.
    • On success, fetch global object index from tables and return with A/X.
================================================================================
*/
.label cursor_y  = $15                        // ZP: cursor row (pixels→rows scaled)
.label cursor_x  = $17                        // ZP: cursor column (pixels→cols scaled)
* = $3CE8
check_cursor_object_bounds:
        // ------------------------------------------------------------
        // Compute cursor coordinates for hit testing
        //
        // - Y position: take half-resolution cursor value, divide by 4
        //   (right-shift twice) to normalize to object grid scale.
        // - X position: load quarter-absolute cursor value directly,
        //   already scaled to object coordinate units.
        // ------------------------------------------------------------
        lda     cursor_y_pos_half_off_by_8     // source Y (half, off by 8)
        lsr                                    // /2
        lsr                                    // /4
        sta     cursor_y                       // stash Y in ZP

        lda     cursor_x_pos_quarter_absolute  // source X (quarter-absolute)
        sta     cursor_x                       // stash X in ZP

        // ------------------------------------------------------------
        // X containment test
        // - If cursor_x < obj_left → skip.
        // - If cursor_x == obj_left → verify overlap using right edge.
        // - Compute right edge = left + width; treat right==cursor as outside.
        // ------------------------------------------------------------
        lda     obj_left_col_tbl,x              // A := object X start
        cmp     cursor_x                        // compare start vs cursor X
        beq     check_cursor_x_overlap          // equal → need width overlap check
        bcs     skip_to_next_object             // start > cursor X → left of object → skip

check_cursor_x_overlap:
        clc                                     // compute object X end = start + width
        adc     obj_width_tbl,x                 // add object width
        cmp     cursor_x                        // compare end vs cursor X
        bcc     skip_to_next_object             // end < cursor X → right of object → skip
        beq     skip_to_next_object             // end == cursor X → treat as outside → skip

        // ------------------------------------------------------------
        // Y containment test
        // - If cursor_y < obj_top → skip.
        // - If cursor_y == obj_top → verify overlap using bottom edge.
        // - Compute bottom = top + height; treat bottom==cursor as outside.
        // ------------------------------------------------------------
        lda     obj_top_row_tbl,x               // A := object Y start
        cmp     cursor_y                        // compare start vs cursor Y
        beq     check_cursor_y_overlap          // equal → need height overlap check
        bcs     skip_to_next_object             // start > cursor Y → above object → skip

check_cursor_y_overlap:
        clc                                     // compute object Y end = start + height
        adc     obj_height_tbl,x                // add object height
        cmp     cursor_y                        // compare end vs cursor Y
        bcc     skip_to_next_object             // end < cursor Y → below object → skip
        beq     skip_to_next_object             // end == cursor Y → treat as outside → skip

        // ------------------------------------------------------------
        // Hit detected: return global object index
        //
        // - Load object's global index hi/lo from tables.
        // - Transfer lo-byte to X and restore hi-byte to A.
        // - Return with A/X forming complete object index.
        // ------------------------------------------------------------
        lda     room_obj_idx_hi,x               // A := obj_idx_hi
        pha                                     // Save high byte on stack
        lda     room_obj_idx_lo,x               // A := obj_idx_lo
        tax                                     // X := obj_idx_lo
        pla                                     // A := obj_idx_hi
        rts                                     // Return with A/X set

		// -----------------------------------------------------------------
		// Miss: continue with next object (tail-jump back to caller's loop)
		// -----------------------------------------------------------------
skip_to_next_object:
        jmp     move_to_next_object             // advance outer scan
/*
================================================================================
  disk_id_check
================================================================================
Summary
	Read the first byte of track 1, sector 0 into disk_id_in_sector and
	compare it to active_side_id. On match, enter a debug infinite loop.
	On mismatch, ensure the correct side is active and return.

Returns
	If IDs mismatch: returns to caller after restoring start_track/sector.
	If IDs match: does not return; loops writing VIC border color.

Global Inputs
	start_track   caller’s intended track to restore
	start_sector  caller’s intended sector to restore
	active_side_id  expected disk side identifier

Global Outputs
	disk_id_in_sector  set to the Disk ID byte read from T1,S0, offset 0

Description
	- Save caller CHS (start_sector, start_track) on the stack.
	- Initialize chain for T1 with offset 0; seek to S0, offset 0; read.
	- Copy 1 byte into disk_id_in_sector.
	- Compare against active_side_id.
	- If equal: set debug_error_code, map in I/O, spin writing border.
	- If not equal: disk_ensure_correct_side(active_side_id), restore CHS, return.
================================================================================
*/
.label  disk_id_in_sector = $567A          // Buffer for disk ID byte (T1 S0 offset 0)
* = $5627
disk_id_check:
        // ------------------------------------------------------------
        // Preserve caller’s starting track and sector
		//
        // Save start_sector then start_track on the stack so this routine
        // can probe T1,S0 for the Disk ID and later restore the caller’s
        // requested position. Pop order will be track, then sector.
        // ------------------------------------------------------------
        lda     start_sector
        pha
        lda     start_track
        pha

		// ------------------------------------------------------------
		// Read T1,S0 at offset 0
		//
		// Initialize the disk read chain for track #$01 with byte offset #$00,
		// then position to sector #$00 at offset #$00 and start the read.
		// Assumes: X = offset, Y = track for disk_init_chain; X = offset,
		// Y = sector for disk_seek_and_read_sector. Clobbers X,Y,A as per callee specs.
		// ------------------------------------------------------------
        ldx     #$00
        ldy     #$01
        jsr     disk_init_chain
        ldx     #$00
        ldy     #$00
        jsr     disk_seek_and_read_sector

        // Copy 1 byte (disk ID) to disk_id_in_sector
        ldx     #<disk_id_in_sector
        ldy     #>disk_id_in_sector
        lda     #$01
        sta     disk_copy_count_lo
        lda     #$00
        sta     disk_copy_count_hi
        jsr     disk_stream_copy

		// ------------------------------------------------------------
		// Validate Disk ID against expected side
		//
		// Load expected ID (active_side_id) and compare to the byte read
		// from T1,S0 at offset 0 (disk_id_in_sector). CMP sets Z=1 on
		// match; BNE branches to disk_id_mismatch when Z=0 (IDs differ).
		// ------------------------------------------------------------
        lda     active_side_id               // A := expected Disk ID for the current side
        cmp     disk_id_in_sector            // Set Z if A == disk_id_in_sector; Z=0 on mismatch
        bne     disk_id_mismatch             // Z=0 → IDs differ → branch to mismatch handler

        // Debug path if IDs match
        lda     #$2F
        sta     debug_error_code

        // Map in I/O
        ldy     #MAP_IO_IN
        sty     cpu_port

infinite_loop:
        sta     vic_border_color_reg
        jmp     infinite_loop
		
		//Unreachable code - replicated from original
		jmp		$566D

        // Disk ID doesn't match
disk_id_mismatch:
        lda     active_side_id               // A := target side ID we expect to be active
        jsr     disk_ensure_correct_side             // Switch/verify drive is on that side; updates media state

		// ------------------------------------------------------------
		// Restore caller’s track and sector
		// ------------------------------------------------------------
		pla
		sta     start_track
		pla
		sta     start_sector
		rts
/*
================================================================================
mem_fill_x  write X to memory fill_byte_cnt times starting at dest

  Arguments:
    X      Fill byte to write to each destination address.

  Global Inputs:
    fill_dest_ptr      16-bit base address to start filling (advanced across pages).
    fill_byte_cnt      16-bit number of bytes to write; must be nonzero on entry.

  Global Outputs:
    fill_dest_ptr      Advanced to the first address past the last written byte.
    fill_byte_cnt      Decremented to zero when the routine returns.

  Returns:
    A,Y,flags
      Clobbered; X preserved. Z=1 on return_from_entity_contact (counter reached zero).

  Description:
    - Writes the value from X to *(fill_dest_ptr) and increments fill_dest_ptr
      after each store (handles page crossings via INC low/INC high).
    - Decrements a 16-bit counter and loops until both counter bytes are zero.
    - Y remains 0 the entire time; the base pointer moves instead of Y.

  Notes:
    - Caller must ensure fill_byte_cnt > 0; a zero-length request is invalid.
================================================================================
*/
* = $5D32
mem_fill_x:
        // ------------------------------------------------------------
        // Initialize Y to 0 (use base-pointer incrementing, not Y stepping)
        // ------------------------------------------------------------
        ldy     #$00               // Y stays 0 for (fill_dest_ptr),Y addressing

        // ------------------------------------------------------------
        // Store fill byte and advance destination pointer (handles page cross)
        // ------------------------------------------------------------
        txa                        // A := fill byte (preserve X)
        sta     (fill_dest_ptr),y  // write A to *fill_dest_ptr
        inc     fill_dest_ptr     // dest.lo++
        bne     dec_count          // no wrap → skip high-byte increment
        inc     fill_dest_ptr + 1     // wrapped → dest.hi++

dec_count:
        // ------------------------------------------------------------
        // Decrement 16-bit remaining count (borrow when low byte is 0)
        // ------------------------------------------------------------
        lda     fill_byte_cnt     // low byte
        bne     dec_count_lo       // if low != 0, no borrow
        dec     fill_byte_cnt + 1     // borrow: dec high byte
dec_count_lo:
        dec     fill_byte_cnt     // dec low byte

        // ------------------------------------------------------------
        // Loop until both counter bytes are zero
        // ------------------------------------------------------------
        lda     fill_byte_cnt     // low
        ora     fill_byte_cnt + 1     // combine with high; Z=1 iff both zero
        bne     mem_fill_x          // not done → continue
        rts                        // done (Z=1, Y=0, X preserved)
/*
================================================================================
  prepare_video_for_new_room
================================================================================
Summary
	Prepare the video system for entering a new room. Hides UI cursor and
	actors, clears top-bar text, performs a closing shutter animation,
	clears both view buffers, switches to frame buffer 2, then synchronizes
	with the raster IRQ by signaling and waiting for acknowledgment.

Global Outputs
	hide_cursor_flag         set TRUE to hide cursor
	src_msg_ptr              set to empty_msg for shutdown_topbar_talking
	video_update_signal      set to SIGNAL_SET, then waited until IRQ clears
	(via subroutines) view buffers cleared and buffer 2 made active

Description
	- Hide cursor and all actors; release any sprite resources.
	- Point src_msg_ptr at empty_msg and clear the top-bar/talking UI.
	- Run the shutter effect in closing mode using delta table entry 0.
	- Clear both view frame buffers and switch to frame buffer 2.
	- Set video_update_signal and busy-wait until the IRQ handler clears it.
================================================================================
*/
* = $D69D
prepare_video_for_new_room:
        // Hide cursor
        lda     #TRUE
        sta     hide_cursor_flag

        // Hide all actors and free their sprites
        jsr     hide_all_actors

        // Set src_msg_ptr := empty_msg for shutdown_topbar_talking
        lda     #<empty_msg
        sta     src_msg_ptr
        lda     #>empty_msg
        sta     src_msg_ptr + 1

        // Clear top bar text / talking UI
        jsr     shutdown_topbar_talking

        // Run shutter effect in closing mode using first delta-table entry
        lda     #SHUTTER_CLOSE                          
        ldx     #$00                          	// X := delta table offset 0
        jsr     do_shutter_effect

        // Clear both view frame buffers
        jsr     clear_view_buffers

        // Switch to frame buffer 2
        jsr     switch_to_frame_buffer_2

        // Signal video update and wait for IRQ to clear it
        lda     #SIGNAL_SET
        sta     video_update_signal
wait_for_video_update_ack:
        lda     video_update_signal
        bne     wait_for_video_update_ack
        rts
/*
================================================================================
 ensure_raster_irq_ready
================================================================================
Summary
	Ensure the raster IRQ environment is initialized. If initialization is
	pending, run the init routine until cleared. Then issue a one-shot
	signal to the IRQ handler and wait for its acknowledgment.

Vars/State
	Clobbers A, X, and processor flags. Y unchanged.

Global Inputs
	raster_irq_init_pending_flag   nonzero indicates init work remains

Global Outputs
	video_processed_signal    set to #$01 to request IRQ work, waits for #$00
	(via callee) raster IRQ configuration  established by init_raster_irq_env

Description
	- Poll raster_irq_init_pending_flag; while nonzero, call init_raster_irq_env
	and recheck.
	- After init completes, set video_processed_signal := #$01 to hand a unit
	of work to the IRQ handler, then spin until the handler clears it.
	- Execute a final DEX for a minimal delay and exit.
================================================================================
*/
* = $D7BA
ensure_raster_irq_ready:
        // work to the IRQ handler via video_processed_signal.
        ldx     #$00                           // single-pass spacer/counter

check_raster_pending_init:
        lda     raster_irq_init_pending_flag        // pending init work?
        beq     signal_irq_handler_once        // no → proceed to signal-and-wait

        jsr     init_raster_irq_env            // perform pending raster init work
        jmp     check_raster_pending_init      // recheck until cleared

signal_irq_handler_once:
        lda     #SIGNAL_SET
        sta     video_processed_signal         // request IRQ handler to run a unit of work

wait_irq_handler_ack_clear:
        lda     video_processed_signal         // wait until IRQ handler clears it to #SIGNAL_CLEAR
        bne     wait_irq_handler_ack_clear

        dex                                    // one extra breath; X: 00→FF, sets N=1
        bpl     signal_irq_handler_once        // not taken (negative), so exit

        rts
