/*
================================================================================
  Module: UI interaction and hotspot handling
================================================================================

Summary
        Implements the high-level cursor interaction model for the UI:
			-hotspot hit-testing
			-highlight/dehighlight behavior
			-click dispatch
			-inventory-region rendering (including scroll arrows and item labels)

Description
	- Hotspots are stored as compact records describing rectangular regions,
	  types (inventory item, scroll arrow, general UI element), and handler
	  indices. The file walks these tables to find which hotspot, if any,
	  lies under the cursor each frame.
	  
	- Highlight and dehighlight are implemented via color updates and		
	  per-type rules (e.g., inventory items vs. scroll arrows), with the
	  actual behavior delegated to small handler routines called through
	  self-modified JSR vectors. The handler vector words are patched at
	  runtime so the central hit-test logic can jump through a single
	  inline JSR for each kind of action (render, highlight, click).
	  
	- Inventory display logic uses a sliding window over the active kid’s
	  inventory slots, along with simple thresholds and offsets to decide
	  when the up/down arrows should appear and whether they are clickable.
	  
	- When an inventory item hotspot is clicked, the module resolves which
	  inventory slot actually belongs to the active kid (via ownership
	  tables), then assigns that object as either the DO or IO of the
	  current verb, updating the on-screen sentence bar accordingly.
	  
	- The file functions as the glue between low-level cursor coordinates
	  and high-level game semantics: it turns “cursor at (x,y) with a click”
	  into concrete actions such as highlighting, scroll movement, or verb
	  argument selection.
	  
Public routines
        step_cursor_and_dispatch_hotspot
            Per-frame entry point:
                - maps cursor position to a hotspot (if any),
                - updates highlight state,
                - calls the appropriate per-hotspot render / click handlers via
                  self-modified JSR vectors.

        render_all_hotspots
            Walk all hotspot records, rendering each visible hotspot’s text.
			
        refresh_inventory
            Rebuild the inventory display region based on the active kid’s
            inventory window and scroll state (including arrows, empty slots,
            and text colors).
			
Private routines
	Helpers
        find_hotspot_at_cursor
            Given cursor grid coordinates, locate the first hotspot whose
            rectangular bounds contain the cursor. 

		refresh_inventory_regions
			Clamps the inventory window offset, then re-renders all inventory item
			and scroll-arrow hotspots.

		find_owned_slot_for_display
			Maps a visible inventory cell index to the corresponding owned slot for
			the current kid, or reports “not owned.”

		count_active_kid_inventory_items
			Counts how many inventory slots are occupied by objects owned by the
			active kid.
			
	Click handlers
		mark_room_scene_clicked
			Marks that the room scene has been clicked, signaling higher-level room
			interaction logic.

		set_current_verb_and_refresh
			Sets the current verb from the hotspot operand and flags the sentence
			bar for initialization or rebuild.

		rebuild_sentence
			Requests a full rebuild of the action sentence without changing verb or
			objects.

        set_inv_item_as_do_or_io
            For a clicked inventory hotspot, resolve the matching inventory
            slot for the active kid and set the verb’s DO or IO object fields
            accordingly.

        scroll_inventory_page
            Handle clicks on inventory up/down scroll arrows, adjusting the
            inventory window offset and triggering UI refresh when movement is
            possible.

	Render handlers
		render_verb_label
			Looks up verb text from the verb pointer tables and blits it into the
			verb hotspot row.

        render_scroll_arrow_if_eligible
            Conditionally render inventory scroll arrows (up/down) based on the
            active kid’s inventory count and current offset window.

        render_item_name_if_owned
            Given a displayed inventory object id, render its name only if the
            active kid actually owns a copy in one of the visible slots.
			
	Normal color renderers
		render_verb_hotspot
			Renders the verb label for a verb hotspot, then applies its normal
			color fill.

		render_item_hotspot
			Renders the inventory item name for an item hotspot if owned, then
			applies its normal color fill.

		render_scroll_arrow_hotspot
			Conditionally renders the inventory scroll arrow (up/down) for a scroll
			hotspot, then applies its normal color fill.

		reset_sentence_bar_hotspot
			Clears the sentence-bar hotspot by rendering an empty string, then
			applies its normal color fill.

	Render helpers
		dummy_handler
			No-op handler used as a safe default for uninitialized hotspot vectors.

		apply_highlight_color_and_fill
			Chooses the highlight color for the current hotspot type and floods its
			color RAM rectangle.

		apply_normal_color_and_fill
			Chooses the baseline (non-highlight) color for the current hotspot type
			and floods its color RAM rectangle.

		fill_hotspot_color_span
			Fills the rectangular color RAM span for the active hotspot using the
			current hotspot_text_color.
			
		blit_text_to_hotspot_row
			Copies a zero-terminated text row into the active hotspot’s screen
			region, padding with spaces after the terminator.
			
================================================================================
Techniques used
================================================================================

• Hotspot state-machine logic  
  Changes in hotspot selection drive a lifecycle of dehighlight → commit →
  physics update → highlight, with click edge-detection layered on top.
  
• Coarse-grid rectangle hit-testing  
  Cursor pixel coordinates are quantized to 4×8 cell units, allowing hotspot
  detection using simple integer compares over compact fixed-stride records.

================================================================================
Optimizations
================================================================================

• Self-modifying dispatch vectors  
  Render, highlight, dehighlight, and click actions are routed through patched
  JSR $0000 vectors, avoiding indirect-call overhead while supporting per-type
  behavior tables.

• Self-modified loop bounds  
  Column and row limits are patched directly into instruction immediates to
  reduce per-iteration comparisons in both text and color operations.

================================================================================
*/

#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "text_data.inc"
#import "hotspots_metadata.inc"
#import "cursor.asm"
#import "input_scan.asm"

// ------------------------------------------------------------
// Hotspot lookup and metadata
// ------------------------------------------------------------
.const  HOTSPOT_END          	  	= $8A    // Sentinel “end hotspot” index (out of table range)
.const  HOTSPOT_REC_STRIDE        	= $06    // Bytes per hotspot record (row/col bounds, type, aux)

// ------------------------------------------------------------
// Hotspot types (matches hotspot_type field at +4)
// ------------------------------------------------------------
.const  HOTSPOT_TYPE_ITEM         	= $02    // Inventory item cell (2×2 grid entry)
.const  HOTSPOT_TYPE_SCROLL       	= $03    // Inventory scroll arrow (up/down)

// ------------------------------------------------------------
// Screen and color memory
// ------------------------------------------------------------
.const  SCREEN_BASE               	= $CC00  // Text screen RAM base
.const  COLOR_BASE                	= $D800  // VIC-II color RAM base

// ------------------------------------------------------------
// Scroll eligibility thresholds
// ------------------------------------------------------------
.const  MIN_OFFSET_FOR_UP         	= $01    // Can scroll up if display offset ≥ 1
.const  ITEMS_THRESHOLD_FOR_DOWN  	= $05    // Can scroll down if ≥5 items remain below view

// ------------------------------------------------------------
// Viewport paging
// ------------------------------------------------------------
.const  VISIBLE_SLOTS             	= $04    // Number of visible item cells per page (2×2)
.const  INVENTORY_PAGE_STEP       	= $02    // Items advanced per scroll click


.label hotspot_entry_ofs = $CB88   // Current hotspot offset (in bytes) from start of hotspot table
                                    // Each hotspot entry = 6 bytes (see HOTSPOT_ENTRY_SIZE)
                                    // Used as an index into hotspot metadata for current cursor region
                                    // Valid range: 0 .. (num_hotspots * HOTSPOT_ENTRY_SIZE - 1)
                                    // Serves as cursor’s current interaction context


// Source text pointer (lo at $C5, hi at $C6). Used by text copy/render.
.label src_ptr               		= $C5       // ZP pointer to text source
// Destination pointers (lo at $C7, hi at $C8). Dual use:
// - Screen character destination
// - Color RAM row pointer (paired with COLOR_BASE paging logic)
.label scr_dest_ptr                 = $C7       // ZP pointer to screen destination

// ------------------------------------------------------------
// Hotspot metadata view (decoded fields for current hotspot)
// Offsets correspond to the per-entry layout:
// +0 row_start, +1 row_end_ex, +2 col_start, +3 col_end_ex, +4 type
// ------------------------------------------------------------
.label hotspot_row_start     		= $F243     // Current hotspot row start (inclusive)
.label hotspot_row_end_ex    		= $F244     // Current hotspot row end (exclusive)
.label hotspot_col_start     		= $F245     // Current hotspot column start (inclusive)
.label hotspot_col_end_ex    		= $F246     // Current hotspot column end (exclusive)
.label hotspot_type          		= $F247     // Current hotspot type enum

// ------------------------------------------------------------
// Self-modifying call targets (patched JSR vectors)
// Each label marks the two-byte destination for the inline JSR
// ------------------------------------------------------------
.label inlined_render_handler    		= $F3AA  // Patched target for render handler
.label inlined_dehighlighted_handler 	= $F344 // Patched target for dehighlight handler
.label inlined_highlighted_handler 		= $F366 // Patched target for highlight handler
.label inlined_click_handler     		= $F386  // Patched target for click handler

// Self-modified loop bounds (CPY immediates)
.label inlined_max_column        	= $F63D  // Patched target for column limit routine
.label inlined_max_row           	= $F645  // Patched target for row limit routine
.label inlined_column_length     	= $F605  // Patched target for column length calc

// ------------------------------------------------------------
// Inventory view state and shared temporaries
// Note: $CB8C is shared scratch. Only one alias is valid at a time.
// ------------------------------------------------------------
.label inv_display_item_offset 		= $CB8B    // First visible inventory index (display offset)
.label inv_count_accum         		= $CB8C    // TEMP: running count of inventory items
.label owned_ordinal_remaining 		= $CB8C    // TEMP: remaining owned-item ordinal
.label tmp_row_index           		= $CB8C    // TEMP: screen row index (0–25)
.label hotspot_text_color      		= $CB8F    // Active text color attribute for hotspot rendering
											   // Used when highlighting or dehighlighting labels
											   // Written before drawing text into COLOR_BASE rows

// ------------------------------------------------------------
// Hotspot operand table
// Maps hotspot → operand used by render/handlers (0–3 display slot)
// ------------------------------------------------------------
.label hotspot_operand_slot    		= $F248    // Table byte: 0..3 for item grid slot

// ------------------------------------------------------------
// Cursor/grid position math scratch
// ------------------------------------------------------------
.label raw_row_base_lo         		= $CB7E    // Row base low byte before bias/corrections
.label pos16_lo                		= $CB8C    // 16-bit position low (shared scratch)
.label pos16_hi                		= $CB8D    // 16-bit position high

/*
================================================================================
  refresh_inventory
================================================================================
Summary
    Safely refresh inventory hotspots by temporarily switching to cutscene
    control mode.

Global Inputs
    control_mode      current UI control state; read to decide early exit

Global Outputs
    control_mode      restored to its prior value after the redraw

Description
    - If already in cutscene mode, return immediately.
    - Save current control_mode on the stack and force cutscene mode.
    - Map in I/O and color RAM via cpu_port.
    - Call refresh_inventory_regions to redraw items and arrows.
    - Map I/O back out and restore the original control_mode.
================================================================================
*/
* = $5F2B
refresh_inventory:
        lda     control_mode                 // load current control mode
        cmp     #CONTROL_MODE_CUTSCENE       // are we already in cutscene mode?
        beq     ri_exit						 // yes → nothing to redraw safely here

        lda     control_mode                 // save previous control mode
        pha
        lda     #CONTROL_MODE_CUTSCENE       // force cutscene mode to suppress input/side effects
        sta     control_mode

        ldy     #MAP_IO_IN                   // map in I/O and color RAM ($D000–$DFFF)
        sty     cpu_port

        jsr     refresh_inventory_regions    // redraw item names and scroll arrows

        ldy     #MAP_IO_OUT                  // restore normal memory mapping
        sty     cpu_port

        pla                                  // restore prior control mode
        sta     control_mode

ri_exit:
        rts                                   
/*
================================================================================
  step_cursor_and_dispatch_hotspot
================================================================================
Summary
    Update cursor physics, resolve the current hotspot, and handle lifecycle
    transitions and clicks. On hotspot change, clears button edge state,
    runs dehighlight for the old hotspot, updates physics, then runs highlight
    for the new hotspot. Detects a fresh click edge and dispatches the click
    handler.

Vars/State
    hotspot_entry_ofs           	current hotspot index; updated to new value
    hotspot_type                	hotspot type table indexed by hotspot_entry_ofs
    button_presses              	latched count/flag for processed clicks
    inlined_dehighlighted_handler 	two-byte JSR target patched per hotspot type
    inlined_highlighted_handler   	two-byte JSR target patched per hotspot type
    inlined_click_handler       	two-byte JSR target patched per hotspot type

Global Outputs
    hotspot_entry_ofs           	set to the newly detected hotspot index
    button_presses              	cleared on hotspot change; incremented on click

Description
    - Step cursor physics and update grid coordinates.
    - Find hotspot at cursor (X := new index).
    - If index differs from hotspot_entry_ofs:
        • Clear button_presses.
        • Run dehighlight for the previous hotspot via patched JSR.
        • Commit hotspot_entry_ofs := X.
        • Update cursor physics using the new hotspot.
        • Run highlight for the new hotspot via patched JSR.
    - Detect a rising-edge click; if present, dispatch the type-specific click
      handler via patched JSR and increment button_presses.

Notes
    - Uses self-modifying JSR targets to avoid indirect call overhead.
================================================================================
*/
* = $F316
step_cursor_and_dispatch_hotspot:
        // ------------------------------------------------------------
        // Update cursor physics and determine current hotspot
        //
        // - Steps cursor motion and drag.
        // - Updates grid coordinates.
        // - Determines cursor hotspot index (X = new hotspot).
        // ------------------------------------------------------------
        jsr     cursor_physics_step
        jsr     update_cursor_grid_coords
        jsr     find_hotspot_at_cursor

        // ------------------------------------------------------------
        // If hotspot unchanged, skip to click handling
        // ------------------------------------------------------------
        cpx     hotspot_entry_ofs
        beq     handle_click

        // ------------------------------------------------------------
        // Handle hotspot change → reset button state
        // ------------------------------------------------------------
        txa
        pha
        lda     #$00
        sta     button_presses

        // ------------------------------------------------------------
        // Run dehighlighted handler for previous hotspot
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        cpx     #HOTSPOT_END
        beq     no_dehighlight_needed

		// Resolve hotspot type
        lda     hotspot_type,x
		// Resolve handler index
        asl
        tay		
		// Patch inlined handler
        lda     hotspot_dehighlighted_handlers,y
        sta     inlined_dehighlighted_handler
        lda     hotspot_dehighlighted_handlers+1,y
        sta     inlined_dehighlighted_handler+1

        jsr     $0000                  // invoke dehighlighted handler

no_dehighlight_needed:
        pla
        sta     hotspot_entry_ofs

        // ------------------------------------------------------------
        // Update hotspot-specific cursor physics (accel/drag)
        // ------------------------------------------------------------
        jsr     update_cursor_physics_from_hotspot

        // ------------------------------------------------------------
        // Run handler for new hotspot to highlight text
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        cpx     #HOTSPOT_END
        beq     handle_click

		// Resolve hotspot type
        lda     hotspot_type,x
		// Resolve handler index
        asl
        tay
		// Patch inlined handler
        lda     hotspot_highlighted_handlers,y
        sta     inlined_highlighted_handler
        lda     hotspot_highlighted_handlers+1,y
        sta     inlined_highlighted_handler+1

        jsr     $0000                  // invoke highlighted handler

handle_click:
        // ------------------------------------------------------------
        // Handle new button press edge and dispatch click handlers
        // ------------------------------------------------------------
        jsr     detect_fire_press_edge
        beq     exit_hotspot_handler

        ldx     hotspot_entry_ofs
        cpx     #HOTSPOT_END
        beq     register_press_and_exit

		// Resolve hotspot type
        lda     hotspot_type,x
		// Resolve handler index
        asl
        tay
		// Patch inlined handler
        lda     hotspot_click_handlers,y
        sta     inlined_click_handler
        lda     hotspot_click_handlers+1,y
        sta     inlined_click_handler+1

        jsr     $0000                  // invoke click handler
register_press_and_exit:		
        inc     button_presses
exit_hotspot_handler:
        rts
/*
================================================================================
  render_all_hotspots
================================================================================
Summary
    Iterate all UI hotspots and render each by dispatching to its type-specific
    handler via a self-modified JSR target. Stops when HOTSPOT_END is reached,
    then updates cursor physics from the final hotspot context.

Vars/State
    hotspot_entry_ofs       current hotspot index; incremented in +6 strides
    hotspot_type            hotspot type table; selects render handler per index
    hotspot_render_handlers table of handler addresses; indexed by type*2
    inlined_render_handler  two-byte JSR target patched with the handler address

Description
    - Initialize hotspot_entry_ofs to 0.
    - Loop:
        • If hotspot_entry_ofs == HOTSPOT_END, exit the loop.
        • Read type = hotspot_type[hotspot_entry_ofs].
        • Compute table index = type*2.
        • Patch inlined_render_handler with the two-byte address from
          hotspot_render_handlers[index .. index+1].
        • Invoke the patched handler.
        • Advance hotspot_entry_ofs by the record stride.
      When finished, jump to update_cursor_physics_from_hotspot.

Notes
    - Uses self-modifying code to avoid indirect JSR overhead per hotspot.
================================================================================
*/
* = $F38C
render_all_hotspots:
        // ------------------------------------------------------------
        // Initialize hotspot iteration at #$00
        // ------------------------------------------------------------
        lda     #$00
        sta     hotspot_entry_ofs

render_hotspot:
        // ------------------------------------------------------------
        // Skip drawing if hotspot_entry_ofs == HOTSPOT_END
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        cpx     #HOTSPOT_END
        beq     next_region

        // ------------------------------------------------------------
        // Resolve and call hotspot entry handler
        //
        // - hotspot_type[X] → handler index
        // - index*2 → word offset into hotspot_render_handlers table
        // - Patch JSR $0000 target at inlined_render_handler
        // - Invoke handler via the inlined JSR
        // ------------------------------------------------------------
        lda     hotspot_type,x
        asl
        tay

        lda     hotspot_render_handlers,y
        sta     inlined_render_handler
        lda     hotspot_render_handlers+1,y
        sta     inlined_render_handler+1

        // Call patched handler
        jsr     $0000

next_region:
        // ------------------------------------------------------------
        // Advance to next hotspot. 
        // Exit once HOTSPOT_END is reached.
        // ------------------------------------------------------------
        clc
        lda     hotspot_entry_ofs
        adc     #HOTSPOT_REC_STRIDE
        sta     hotspot_entry_ofs

        cmp     #HOTSPOT_END
        bne     render_hotspot

        // ------------------------------------------------------------
        // Done drawing all regions → update cursor physics from hotspot
        // ------------------------------------------------------------
        jmp     update_cursor_physics_from_hotspot

* = $F3E8
dummy_handler:
		rts
/*
================================================================================
  apply_highlight_color_and_fill
================================================================================
Summary
    Select the highlight color for the current hotspot type and fill its
    rectangular area in color RAM.

Global Inputs
    hotspot_entry_ofs               current hotspot index
	
Vars
    hotspot_type                    hotspot type for the current index
    hotspot_highlight_colors  		lookup table of highlight colors, indexed by type

Description
    - Load hotspot type from hotspot_type[hotspot_entry_ofs].
    - Read highlight color from the color table and store into hotspot_text_color.
    - Jump to fill_hotspot_color_span to paint the hotspot rectangle.
================================================================================
*/
* = $F3E9
apply_highlight_color_and_fill:
		// Resolve hotspot type for hotspot index
        ldx     hotspot_entry_ofs
        ldy     hotspot_type,x
		// Resolve highlight color for type
        lda     hotspot_highlight_colors,y
        sta     hotspot_text_color
		// Apply the color
        jmp     fill_hotspot_color_span
/*
================================================================================
  apply_normal_color_and_fill
================================================================================
Summary
    Restore the normal color for the current hotspot type and fill its region
    in color RAM.

Global Inputs
    hotspot_entry_ofs      current hotspot index
	
Vars
    hotspot_type           per-hotspot type table; indexed by hotspot_entry_ofs
    hotspot_normal_colors  lookup table of normal colors; indexed by hotspot type

Description
    - Load hotspot type from hotspot_type[hotspot_entry_ofs].
    - Fetch its normal color from hotspot_normal_colors[type] into A.
    - Store A into hotspot_text_color.
    - Jump to fill_hotspot_color_span to paint the hotspot rectangle.
================================================================================
*/
* = $F3F8
apply_normal_color_and_fill:
		// Resolve hotspot type for hotspot index
        ldx     hotspot_entry_ofs
        ldy     hotspot_type,x
		// Resolve normal color for type
        lda     hotspot_normal_colors,y
        sta     hotspot_text_color
		// Apply the color
        jmp     fill_hotspot_color_span
/*
================================================================================
  render_verb_hotspot
================================================================================
Summary
    Render the verb label for this hotspot, then apply the normal dehighlight
    color fill.

Description
    - Call render_verb_label to draw the verb text.
    - Jump to apply_normal_color_and_fill to paint baseline colors.
================================================================================
*/
* = $F407
render_verb_hotspot:
        // ------------------------------------------------------------
        // Render verb with normal color
        // ------------------------------------------------------------
        jsr     render_verb_label
        jmp     apply_normal_color_and_fill
/*
================================================================================
  render_item_hotspot
================================================================================
Summary
    Render the inventory item text for this hotspot, then apply the normal
    color fill.

Description
    - Call render_item_name_if_owned to draw the item’s name or nothing.
    - Jump to apply_normal_color_and_fill to paint baseline colors.
================================================================================
*/
* = $F40D
render_item_hotspot:
        // ------------------------------------------------------------
        // Render item text with normal color
        // ------------------------------------------------------------
        jsr     render_item_name_if_owned
        jmp     apply_normal_color_and_fill
/*
================================================================================
  render_scroll_arrow_hotspot
================================================================================
Summary
    Render the inventory scroll arrow for this hotspot (if eligible), then
    apply the normal color fill.

Description
    - Call render_scroll_arrow_if_eligible to draw the up/down arrow or nothing.
    - Jump to apply_normal_color_and_fill to paint baseline colors.
================================================================================
*/
* = $F413
render_scroll_arrow_hotspot:
        // ------------------------------------------------------------
        // Render scroll arrow (if any) with normal color
        // ------------------------------------------------------------
        jsr     render_scroll_arrow_if_eligible
        jmp     apply_normal_color_and_fill
/*
================================================================================
  reset_sentence_bar_hotspot
================================================================================

Summary
    Render an empty string into the sentence-bar hotspot, then tail-call the
    normal color fill routine.

Vars/State
    src_ptr    source pointer set to the empty string before rendering

Description
    - Point src_ptr at the empty string.
    - Call blit_text_to_hotspot_row to clear the hotspot text.
    - Jump to apply_normal_color_and_fill to paint baseline colors.
================================================================================
*/
* = $F419
reset_sentence_bar_hotspot:
        // ------------------------------------------------------------
        // Render empty string with normal color
        // ------------------------------------------------------------
        lda     #<empty_string
        sta     src_ptr                           
        lda     #>empty_string
        sta     src_ptr + 1                       
        jsr     blit_text_to_hotspot_row
        jmp     apply_normal_color_and_fill
/*
================================================================================
  mark_room_scene_clicked
================================================================================
Summary
    Mark that the room scene was clicked.

Global Outputs
    room_scene_clicked_flag    set to TRUE to signal a room-scene click
================================================================================
*/
* = $F427
mark_room_scene_clicked:
        // ------------------------------------------------------------
        // Mark that the room scene was clicked
        // ------------------------------------------------------------
        lda     #TRUE
        sta     room_scene_clicked_flag
        rts
/*
================================================================================
  set_current_verb_and_refresh
================================================================================
Summary
    Set the current verb from the hotspot operand. If any button press is
    latched, request sentence rebuild; otherwise request sentence UI init.
    Always mark the sentence bar for refresh.

Global Inputs
    hotspot_entry_ofs       current hotspot index to read operand
    hotspot_operand_slot    per-hotspot operand table; verb index at [X]
    button_presses          nonzero indicates a button press is latched

Global Outputs
    current_verb_id         	updated to selected verb id
    needs_sentence_rebuild  	set when a button press is latched
    init_sentence_ui_flag   	set when no button press is latched
    sentence_bar_needs_refresh  set to request bar redraw

Description
    - Load verb index from hotspot_operand_slot[hotspot_entry_ofs] and store it
      in current_verb_id.
    - If button_presses ≠ 0: set needs_sentence_rebuild; else set
      init_sentence_ui_flag.
    - Set sentence_bar_needs_refresh and return.
================================================================================
*/
* = $F42D
set_current_verb_and_refresh:
        // Fetch verb from the interaction hotspot
        ldx     hotspot_entry_ofs
        lda     hotspot_operand_slot,x

        // Set it as the current verb
        sta     current_verb_id

        // If any button presses are latched, rebuild the sentence
        lda     button_presses
        beq     reset_sentence_ui_and_stack

        // Button latched, rebuild the sentence
        lda     #TRUE
        sta     needs_sentence_rebuild
        jmp     mark_sentence_bar_refresh

reset_sentence_ui_and_stack:
        // ------------------------------------------------------------
        // Reset the sentence queue/UI
        // ------------------------------------------------------------
        lda     #TRUE
        sta     init_sentence_ui_flag

mark_sentence_bar_refresh:
        // ------------------------------------------------------------
        // Mark sentence bar for refresh and exit
        // ------------------------------------------------------------
        lda     #TRUE
        sta     sentence_bar_needs_refresh
        rts
/*
================================================================================
  rebuild_sentence
================================================================================
Summary
    Request a rebuild of the action sentence.

Global Outputs
    needs_sentence_rebuild    set to TRUE to signal sentence reconstruction
================================================================================
*/
* = $F44D
rebuild_sentence:
        // ------------------------------------------------------------
        // Rebuild the action sentence
        // ------------------------------------------------------------
        lda     #TRUE
        sta     needs_sentence_rebuild
        rts
/*
================================================================================
  set_inv_item_as_do_or_io
================================================================================
Summary
    Handle click on an inventory item hotspot. Uses the hotspot operand as the
    visible cell index, checks ownership via find_owned_slot_for_display, and
    sets either the Direct Object (no preposition set) or the Indirect
    Object (preposition set and item differs from current DO). Always marks
    the sentence bar for refresh; triggers sentence rebuild if a button press
    is latched.

Global Inputs
    hotspot_entry_ofs       	current hotspot index; selects operand
    hotspot_operand_slot    	per-hotspot operand; visible cell index 0..3
    current_preposition     	nonzero means a preposition is set
    button_presses          	nonzero means a button was pressed since last clear
	
Vars/State
    direct_object_idx_lo/hi    	low/hi byte of current Direct Object index; written here
    indirect_object_idx_lo/hi  	low/hi byte of current Indirect Object index; written here
    needs_sentence_rebuild  	flag to request sentence rebuild; may be set here
    sentence_bar_needs_refresh  flag to refresh sentence bar; set here on exit

Description
    - Read display cell index from hotspot_operand_slot[hotspot_entry_ofs].
    - Call find_owned_slot_for_display:
        • If carry is set, item is not owned → skip object updates.
        • If carry is clear, Y holds the inventory slot:
            · If no preposition is set, commit Direct Object = (X=cell, Y=slot).
            · If a preposition is set, keep DO if it matches (X,Y);
              otherwise commit Indirect Object = (X,Y).
    - If any button press is latched, set needs_sentence_rebuild.
    - Always set sentence_bar_needs_refresh before returning.

Notes
    - This routine does not clear button_presses; upstream code owns that.
    - Object index pairs are stored as (lo, hi) bytes for later consumers.
================================================================================
*/
* = $F452
set_inv_item_as_do_or_io:
        // ------------------------------------------------------------
        // Resolve inventory item slot
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        lda     hotspot_operand_slot,x
		
        // ------------------------------------------------------------
        // Find the owned item for this slot
        //   - On CLC (owned): .Y = inventory slot index; proceed to set DO/IO.
        //   - On SEC (not owned): skip to UI refresh.
        // ------------------------------------------------------------
        jsr     find_owned_slot_for_display
        bcs     post_click_ui_update          	// C=1 → not owned

        // ------------------------------------------------------------
        // Item is owned → decide whether it becomes DO or IO
        // ------------------------------------------------------------
        ldy     #$00
        lda     current_preposition
        bne     check_is_current_do             // preposition set → possible IO path
		
        // ------------------------------------------------------------
        // Preposition clear: commit as DO
        // ------------------------------------------------------------
        stx     direct_object_idx_lo            
        sty     direct_object_idx_hi
        jmp     post_click_ui_update

        // ------------------------------------------------------------
        // Preposition set: if item is already DO, skip; else commit as IO
        // ------------------------------------------------------------
check_is_current_do:		
        cpx     direct_object_idx_lo            // same DO lo?
        bne     commit_indirect_object
        cpy     direct_object_idx_hi            // same DO hi?
        bne     commit_indirect_object
        jmp     post_click_ui_update

        // ------------------------------------------------------------
        // Commit Indirect Object := (X,Y)
        // ------------------------------------------------------------
commit_indirect_object:		
        stx     indirect_object_idx_lo
        sty     indirect_object_idx_hi

post_click_ui_update:
        // ------------------------------------------------------------
        // If a button is latched, rebuild the sentence
        // ------------------------------------------------------------
        lda     button_presses                  
        beq     return_refresh_bar
		
		// Button latched, rebuild sentence
        lda     #TRUE
        sta     needs_sentence_rebuild

        // ------------------------------------------------------------
        // Mark sentence bar for refresh and exit
        // ------------------------------------------------------------
return_refresh_bar:		
        lda     #TRUE
        sta     sentence_bar_needs_refresh
        rts
/*
================================================================================
  scroll_inventory_page
================================================================================
Summary
    Handle click on an inventory scroll arrow. Operand 0=up, 1=down. Adjust
    inv_display_item_offset by ±INVENTORY_PAGE_STEP and then refresh inventory
    regions.

Global Inputs
    hotspot_entry_ofs       	current hotspot index to read the operand
    hotspot_operand_slot    	per-hotspot operand; 0=up, 1=down
    inv_display_item_offset 	current inventory window start index before adjustment

Global Outputs
    inv_display_item_offset 	updated inventory window start index after adjustment

Description
    - Read operand from hotspot_operand_slot[hotspot_entry_ofs].
	Operand is scroll direction.
	
    - If operand=1 (down): add INVENTORY_PAGE_STEP to inv_display_item_offset,
      commit, and jump to refresh_inventory_regions.
	  
    - If operand=0 (up): subtract INVENTORY_PAGE_STEP. If result is negative,
      clamp to 0. Commit and fall through to refresh_inventory_regions.
================================================================================
*/
* = $F48F
scroll_inventory_page:
        // ------------------------------------------------------------
        // Handle click on inventory scroll arrow
        //
        // Operand (0 = up, 1 = down) determines scroll direction.
        // Adjusts inv_display_item_offset by ±2 and triggers
        // refresh of all visible items and arrows.
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        lda     hotspot_operand_slot,x
        bne     scroll_page_down
		jmp		scroll_page_up

scroll_page_down:
        // ------------------------------------------------------------
        // Scroll down: add 2 to item display offset
        // ------------------------------------------------------------
        clc
        lda     inv_display_item_offset
        adc     #INVENTORY_PAGE_STEP
        sta     inv_display_item_offset
        jmp     refresh_inventory_regions

scroll_page_up:
        // ------------------------------------------------------------
        // Scroll up: subtract 2 from item display offset
        // Clamp to 0 if subtraction underflows
        // ------------------------------------------------------------
        sec
        lda     inv_display_item_offset
        sbc     #INVENTORY_PAGE_STEP
        bpl     commit_display_offset
        lda     #$00
commit_display_offset:
        sta     inv_display_item_offset
        // Fall through to refresh_inventory_regions	
/*
================================================================================
  refresh_inventory_regions
================================================================================
Summary
    Refresh all inventory-related hotspots (items and scroll arrows). 
	Clamps the inventory window offset to the highest valid page, then scans all hotspots 
	and re-renders those whose type is inventory item or scroll arrow.

Global Inputs
    inv_display_item_offset    index of the top-left visible item; may be clamped
    hotspot_entry_ofs          current hotspot index; preserved across this pass
    hotspot_type               per-hotspot type table read during the scan

Description
    - Compute max valid window start as max(count_owned - 4, 0).
    - Clamp inv_display_item_offset if it exceeds that value.
    - Save current hotspot_entry_ofs, then iterate all hotspots from 0
      in +6 strides until HOTSPOT_END:
        • If type == ITEM, call render_item_name_if_owned.
        • If type == SCROLL, call render_scroll_arrow_if_eligible.
    - Restore hotspot_entry_ofs and return.

Notes
    - The visible grid holds 4 item cells; paging step depends on layout.
================================================================================
*/
* = $F4B3
refresh_inventory_regions:
        // ------------------------------------------------------------
        // Clamp display offset to (owned_items - 4) minimum 0
        // ------------------------------------------------------------
        jsr     count_active_kid_inventory_items
        sec
        sbc     #VISIBLE_SLOTS
        bpl     clamp_window_start
        lda     #$00
clamp_window_start:
        cmp     inv_display_item_offset
        bpl     push_current_region
        sta     inv_display_item_offset

push_current_region:
        // ------------------------------------------------------------
        // Preserve current hotspot index
        // ------------------------------------------------------------
        lda     hotspot_entry_ofs
        pha

        // ------------------------------------------------------------
        // Begin scanning all hotspots from hotspot #$00
        // ------------------------------------------------------------
        lda     #$00
        sta     hotspot_entry_ofs

scan_all_hotspots:
        ldx     hotspot_entry_ofs
        lda     hotspot_type,x

        // ------------------------------------------------------------
        // hotspot type item?
        // ------------------------------------------------------------
        cmp     #HOTSPOT_TYPE_ITEM
        bne     dispatch_scroll_arrow
		
		// Item - render item name if owned
        jsr     render_item_name_if_owned
        jmp     advance_region_or_finish

dispatch_scroll_arrow:
        // ------------------------------------------------------------
        // hotspot type scroll arrow?
        // ------------------------------------------------------------
        cmp     #HOTSPOT_TYPE_SCROLL
        bne     advance_region_or_finish
		
		// Scroll arrow - render if eligible
        jsr     render_scroll_arrow_if_eligible

advance_region_or_finish:
        // ------------------------------------------------------------
        // Advance to next hotspot and continue
        // ------------------------------------------------------------
        clc
        lda     hotspot_entry_ofs
        adc     #HOTSPOT_REC_STRIDE
        sta     hotspot_entry_ofs
		
        cmp     #HOTSPOT_END
        bne     scan_all_hotspots

        // ------------------------------------------------------------
        // Restore hotspot and exit
        // ------------------------------------------------------------
        pla
        sta     hotspot_entry_ofs
        rts
/*
================================================================================
  render_scroll_arrow_if_eligible
================================================================================
Summary
    Render the inventory scroll arrow (up or down) into the hotspot row if the
    paging condition is met; otherwise render an empty string.

Global Inputs
    hotspot_entry_ofs       	current hotspot index used to read operand
	
Vars/State
    hotspot_operand_slot    	per-hotspot operand; 0=up arrow, 1=down arrow
    inv_display_item_offset 	index of the top-left visible item
    inv_arrow_ptr_lo        	low bytes of arrow text pointers (indexed by operand)
    inv_arrow_ptr_hi       		high bytes of arrow text pointers (indexed by operand)
    src_ptr                 	source pointer for arrow text (written here)

Description
    - If operand=0 (up): show arrow only when inv_display_item_offset ≥ 1.
    - If operand=1 (down): show arrow only when
        count_active_kid_inventory_items() - inv_display_item_offset ≥ 5.
    - On success: load arrow text pointer and blit into the hotspot row.
    - On failure: point src_ptr to the empty string and blit.

Notes
    - Uses blit_text_to_hotspot_row for rendering.
================================================================================
*/
* = $F4F7
render_scroll_arrow_if_eligible:
        // ------------------------------------------------------------
        // Resolve arrow index into Y
		//
		// Operand (0 = up, 1 = down) defines which arrow to display.
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        ldy     hotspot_operand_slot,x
        bne     check_down_arrow

        // ------------------------------------------------------------
        // Up arrow: show only if inv_display_item_offset ≥ 1
        // ------------------------------------------------------------
        lda     inv_display_item_offset
        cmp     #MIN_OFFSET_FOR_UP
        jmp     decide_arrow_display

check_down_arrow:
        // ------------------------------------------------------------
        // Down arrow: show only if remaining items ≥ 5
        // ------------------------------------------------------------
        jsr     count_active_kid_inventory_items
        sec
        sbc     inv_display_item_offset
        cmp     #ITEMS_THRESHOLD_FOR_DOWN

decide_arrow_display:
        // ------------------------------------------------------------
        // If comparison fails → render empty string (no arrow)
        // ------------------------------------------------------------
        bcc     no_arrow

        // ------------------------------------------------------------
        // Valid arrow → resolve arrow text pointer and render
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        ldy     hotspot_operand_slot,x
        lda     inv_arrow_ptr_lo,y
        sta     src_ptr
        lda     inv_arrow_ptr_hi,y
        sta     src_ptr+1
        jmp     rsaie_trampoline

no_arrow:
        // ------------------------------------------------------------
        // No arrow -> point to empty string
        // ------------------------------------------------------------
        lda     #<empty_string
        sta     src_ptr
        lda     #>empty_string
        sta     src_ptr+1
rsaie_trampoline:		
        jmp     blit_text_to_hotspot_row
/*
================================================================================
  render_verb_label
================================================================================
Summary
    Copy the verb text referenced by the hotspot into the hotspot’s screen row.

Global Inputs
    hotspot_entry_ofs           current hotspot index used to read operand
	
Vars/State
    hotspot_operand_slot        per-hotspot operand; verb index for this entry
    verb_pointers_lo            table of verb text pointers, low bytes
    verb_pointers_hi            table of verb text pointers, high bytes
    src_ptr                     source pointer for text bytes (written here)

Description
    - Load verb index from hotspot_operand_slot[hotspot_entry_ofs].
    - Build src_ptr from verb_pointers_{lo,hi}[index].
    - Jump to blit_text_to_hotspot_row to render into the hotspot row.

Notes
    - Assumes verb text is zero-terminated and compatible with the blitter’s
      space/terminator handling.
================================================================================
*/
* = $F530
render_verb_label:
        // ------------------------------------------------------------
		// Resolve verb index into Y
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        ldy     hotspot_operand_slot,x

        // ------------------------------------------------------------
        // Resolve pointer to verb text
        // ------------------------------------------------------------
        lda     verb_pointers_lo,y
        sta     src_ptr
        lda     verb_pointers_hi,y
        sta     src_ptr+1

        // ------------------------------------------------------------
        // Render the verb text into the hotspot row
        // ------------------------------------------------------------
        jmp     blit_text_to_hotspot_row
/*
================================================================================
  render_item_name_if_owned
================================================================================
Summary
    Copy the current inventory item’s name into its hotspot row. Uses the
    hotspot’s operand to select a visible display cell, checks ownership
    against the current kid, and either blits the resolved name or an empty
    string into the screen region.

Global Inputs
    hotspot_entry_ofs           current hotspot index for this render
	
Vars/State
    hotspot_operand_slot        table: maps hotspot to display cell index (0..3)
    object_ptr_lo_tbl           table: per-inventory-slot object base pointer lo
    object_ptr_hi_tbl           table: per-inventory-slot object base pointer hi
    src_ptr                     source pointer for text bytes (written here)

Description
    - Read the hotspot’s display cell index from hotspot_operand_slot[X].
    - Call find_owned_slot_for_display:
        • If C=1, set src_ptr to the empty string and blit.
        • If C=0, Y holds the matching inventory slot index.
    - On C=0, build src_ptr from object_ptr_{lo,hi}_tbl[Y], then add the
      object’s name offset at byte +$0D and blit via blit_text_to_hotspot_row.

Notes
    - Name offset (+$0D) is part of the object record layout.
================================================================================
*/
* = $F543
render_item_name_if_owned:
        // ------------------------------------------------------------
		// Resolve operand slot index for hotspot entry -> A
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        lda     hotspot_operand_slot,x
		
        // ------------------------------------------------------------
        // Determine if the item corresponding to the hotspot belongs
        // to the current kid. 
        // ------------------------------------------------------------
        jsr     find_owned_slot_for_display
        bcc     resolve_name_ptr_and_blit

        // ------------------------------------------------------------
        // Item not owned → set src_ptr to empty string
        // ------------------------------------------------------------
        lda     #<empty_string
        sta     src_ptr
        lda     #>empty_string
        sta     src_ptr+1
        jmp     blit_text_trampoline

resolve_name_ptr_and_blit:
        // ------------------------------------------------------------
        // Resolve object pointer for inventory slot in .Y
        // ------------------------------------------------------------
        lda     object_ptr_lo_tbl,y
        sta     src_ptr
        lda     object_ptr_hi_tbl,y
        sta     src_ptr+1

        // ------------------------------------------------------------
        // Add name offset
        // ------------------------------------------------------------
        ldy     #OBJ_NAME_OFS
        clc
        lda     src_ptr
        adc     (src_ptr),y
        sta     src_ptr
        bcc     blit_text_trampoline
        inc     src_ptr+1

blit_text_trampoline:
        jmp     blit_text_to_hotspot_row
/*
================================================================================
  find_owned_slot_for_display
================================================================================
Summary
    Resolve whether the Nth visible inventory item (by display index) is owned
    by the current kid. Converts display index to an ordinal within the stream
    of owned items, scans inventory slots, and returns via the carry flag.

Arguments
    A  input display index in 0..3 (0=top-left)

Vars/State
    owned_ordinal_remaining    remaining count to reach the target owned item

Global Inputs
    inv_display_item_offset    index of top-left item in the window
    inventory_objects          inventory slot table; $00 means empty slot
    object_attributes          per-object attributes; low nibble encodes owner id
    current_kid_idx            active kid index used to match ownership

Returns
    C  				clear 	if the targeted visible item is owned by current kid
					set   	if not owned or no such item
	Y 				inventory slot index for the owned object

Description
    - Compute target ordinal: display_index + inv_display_item_offset.
    - Iterate inventory slots:
        • Skip empty entries.
        • Compare each object’s owner nibble with current_kid_idx.
        • On each match, decrement owned_ordinal_remaining.
        • When it underflows (<0), the Nth owned item has been reached → C=0.
    - If the scan ends without underflow, no owned item occupies that cell → C=1.

Notes
    - Slot count and owner-nibble mask are constants defined elsewhere.
================================================================================
*/
* = $F573
find_owned_slot_for_display:
        // ------------------------------------------------------------
        // Initialize ordinal := display index + inv_display_item_offset
        //
        // display index = 0–3, index of each of the 4 visible inventory 
		// 		item slots in the UI
        // inv_display_item_offset = index of top-left item
        // ------------------------------------------------------------
        clc
        adc     inv_display_item_offset
        sta     owned_ordinal_remaining

        // ------------------------------------------------------------
        // Scan through all inventory slots
        // ------------------------------------------------------------
        ldy     #$00

scan_slots_for_owned_match:
        // Load object ID from slot; skip if empty (#$00)
        ldx     inventory_objects,y
        beq     advance_slot_or_finish

        // Check if item belongs to the active kid
        lda     object_attributes,x
        and     #OWNER_NIBBLE_MASK
        cmp     current_kid_idx
        bne     advance_slot_or_finish

        // Match → decrement remaining ordinal
        dec     owned_ordinal_remaining
        bpl     advance_slot_or_finish

        // Found the Nth owned item → carry clear
        clc
        rts

advance_slot_or_finish:
        // Move to next slot and continue until all checked
        iny
        cpy     #INVENTORY_SLOTS
        bne     scan_slots_for_owned_match

        // None matched → carry set
        sec
        rts
/*
================================================================================
  count_active_kid_inventory_items
================================================================================
Summary
    Count how many inventory objects are owned by the current kid. Scans the
    inventory slot table, filters empty slots, checks the owner nibble against
    current_kid_idx, and accumulates a total.

Global Inputs
    inventory_objects     	inventory slot table; each entry is an object id or $00
    current_kid_idx       	active kid index to match against object owner nibble

Vars/State
    inv_count_accum    		accumulator for the running item count (written here)
    object_attributes     	per-object attribute bytes; low nibble encodes owner id

Returns
    A 						total number of owned items

Description
    - Initialize inv_count_accum to zero and Y to 0.
    - For each inventory slot:
        • Read object id; skip if $00.
        • Read object_attributes[id], mask owner nibble, compare to current_kid_idx.
        • If equal, increment inv_count_accum.
    - When Y reaches INVENTORY_SLOTS, move inv_count_accum to A and return.

Notes
    - OWNER_NIBBLE_MASK and INVENTORY_SLOTS are constants defined elsewhere.
    - Empty slots are encoded as $00 in inventory_objects.
================================================================================
*/
* = $F599
count_active_kid_inventory_items:
        // ------------------------------------------------------------
        // Initialize count
        // ------------------------------------------------------------
        lda     #$00
        sta     inv_count_accum
        ldy     #$00

scan_slot_check_owner:
        // ------------------------------------------------------------
        // Load object ID from inventory slot; skip if empty (#$00)
        // ------------------------------------------------------------
        ldx     inventory_objects,y
        beq     advance_slot_or_exit

        // ------------------------------------------------------------
        // Check if object belongs to the active kid
        // ------------------------------------------------------------
        lda     object_attributes,x
        and     #OWNER_NIBBLE_MASK
        cmp     current_kid_idx
        bne     advance_slot_or_exit

        // ------------------------------------------------------------
        // Match found → increment counter
        // ------------------------------------------------------------
        inc     inv_count_accum

advance_slot_or_exit:
        // ------------------------------------------------------------
        // Advance to next slot; stop after all entries exhausted
        // ------------------------------------------------------------
        iny
        cpy     #INVENTORY_SLOTS
        bne     scan_slot_check_owner

        // ------------------------------------------------------------
        // Load final count into .A and return
        // ------------------------------------------------------------
        lda     inv_count_accum
        rts
/*
================================================================================
  blit_text_to_hotspot_row
================================================================================
Summary
    Copy one text row from src_ptr into the active hotspot’s screen region.
    Converts $40 and $00 to space; a $00 also switches to fill mode to pad
    remaining columns with spaces. Uses start-inclusive, end-exclusive bounds.

Global Inputs
    hotspot_entry_ofs      current hotspot index; HOTSPOT_END means none active
    src_ptr                source pointer for text bytes (lo/hi)
	
Vars/State
    scr_dest_ptr           destination pointer into screen RAM (lo/hi)
    hotspot_row_start      row start (inclusive) per hotspot
    hotspot_col_start      column start (inclusive) per hotspot, indexed by X
    hotspot_col_end_ex     column end (exclusive) per hotspot, indexed by X
    screen_row_offsets_lo  low bytes of per-row screen offsets
    screen_row_offsets_hi  high bytes of per-row screen offsets
    inlined_column_length  self-modified byte for CPY #column_count

Description
    - Guard: if hotspot_entry_ofs == HOTSPOT_END, return.
    - Compute scr_dest_ptr = SCREEN_BASE + screen_row_offset[row_start] + col_start.
    - Compute column_count = col_end_ex - col_start and patch CPY immediate.
    - Copy bytes from src_ptr:
        • Treat $40 and $00 as space.
        • When $00 is seen, enter fill mode and write spaces for the rest.
    - Stop when Y reaches column_count.

Notes
    - Fill mode uses X ≠ 0 as the fast-path signal to skip source reads.
    - SCREEN_BASE is fixed elsewhere; this routine only builds the offset.
================================================================================
*/
* = $F5BB
blit_text_to_hotspot_row:
        // ------------------------------------------------------------
        // Guard: exit when no hotspot is active
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        cpx     #HOTSPOT_END
        bne     compute_screen_ptr_for_hotspot
        rts

compute_screen_ptr_for_hotspot:
        // ------------------------------------------------------------
        // Compute destination pointer:
		//
        // scr_dest_ptr = SCREEN_BASE + screen_row_offsets[row_start]
        // scr_dest_ptr += hotspot_col_start
        // ------------------------------------------------------------
        ldy     hotspot_row_start,x
        clc
        lda     screen_row_offsets_lo,y
        adc     #<SCREEN_BASE
        sta     scr_dest_ptr
        lda     screen_row_offsets_hi,y
        adc     #>SCREEN_BASE
        sta     scr_dest_ptr+1

        // Add column offset
        clc
        lda     scr_dest_ptr
        adc     hotspot_col_start,x
        sta     scr_dest_ptr
        bcc     calc_hotspot_col_width
        inc     scr_dest_ptr+1

calc_hotspot_col_width:
        // ------------------------------------------------------------
        // Compute column count = col_end_ex - col_start
        // ------------------------------------------------------------
        sec
        lda     hotspot_col_end_ex,x
        sbc     hotspot_col_start,x
        sta     inlined_column_length          // patch CPY immediate

        // Initialize src_ptr and index
        ldy     #$00
        ldx     #$00

copy_or_fill_loop:
        // ------------------------------------------------------------
        // Copy loop with terminator handling
        // ------------------------------------------------------------
        txa
        bne     emit_char_to_screen         // X != 0 → fill spaces

        lda     (src_ptr),y                 // read next source byte
        cmp     #WORD_HARD_STOP
        bne     check_space_conversion
        lda     #$00                        // convert hard stop → 0

check_space_conversion:
        cmp     #$00
        bne     emit_char_to_screen
        lda     #SPACE_CHAR					// convert 0 → space
        tax                                 // enter fill mode

emit_char_to_screen:
        sta     (scr_dest_ptr),y
        iny
        cpy     #$12                        // patched with col_end_ex
        bne     copy_or_fill_loop
        rts
/*
================================================================================
  fill_hotspot_color_span
================================================================================
Summary
    Fill the active hotspot’s rectangular area in color RAM using the current
    hotspot color. Uses start-inclusive, end-exclusive bounds. Patches loop
    limits via self-modified bytes for speed.

Global Inputs
    hotspot_entry_ofs       current hotspot index; HOTSPOT_END means none active
    hotspot_text_color      color byte to write into the region
	
Vars/State
    hotspot_row_start       row start (inclusive) per hotspot
    hotspot_row_end_ex      row end (exclusive) per hotspot
    hotspot_col_start       column start (inclusive) per hotspot
    hotspot_col_end_ex      column end (exclusive) per hotspot
    screen_row_offsets_lo   low bytes of per-row screen offsets
    screen_row_offsets_hi   high bytes of per-row screen offsets
    scr_dest_ptr            destination pointer into color RAM (lo/hi)
    tmp_row_index         	working copy of current row index
    inlined_max_column    	self-modified byte for CPY #col_end_ex
    inlined_max_row       	self-modified byte for CPY #row_end_ex

Description
    - If no hotspot is active, return immediately.
    - Patch the column and row end limits into inlined_max_column and
      inlined_max_row to drive CPY immediate bounds in the inner loops.
    - For each row from row_start to row_end_ex:
        • Build scr_dest_ptr = COLOR_BASE + screen_row_offset[row]
        • Fill columns [col_start, col_end_ex) with hotspot_text_color.

Notes
    - Self-modifies two CPY #imm operands to avoid extra compares.
    - COLOR_BASE is assumed fixed by the calling context.
================================================================================
*/
* = $F60A
fill_hotspot_color_span:
        // ------------------------------------------------------------
        // Guard: exit when no hotspot is active
        //
        // Loads the current hotspot index and compares it against the
        // sentinel HOTSPOT_END. If equal, no region needs coloring,
        // so return immediately; otherwise proceed to load bounds.
        // ------------------------------------------------------------
		ldx     hotspot_entry_ofs
        cpx     #HOTSPOT_END
        bne     patch_rowcol_limits
        rts

patch_rowcol_limits:
        // ------------------------------------------------------------
        // Inline hotspot’s max column and max row as self-modified bytes
        // ------------------------------------------------------------
        lda     hotspot_col_end_ex,x
        sta     inlined_max_column           // replaces CPY immediate
        lda     hotspot_row_end_ex,x
        sta     inlined_max_row              // replaces CPY immediate

        // ------------------------------------------------------------
        // Initialize current row index = hotspot_row_start[X]
        // ------------------------------------------------------------
        ldy     hotspot_row_start,x

row_begin_compute_color_ptr:
        sty     tmp_row_index                

        // ------------------------------------------------------------
        // Compute color buffer address for this row:
        // scr_dest_ptr = COLOR_BASE + screen_row_offset[Y]
        // ------------------------------------------------------------
        clc
        lda     screen_row_offsets_lo,y
        adc     #<COLOR_BASE
        sta     scr_dest_ptr
        lda     screen_row_offsets_hi,y
        adc     #>COLOR_BASE
        sta     scr_dest_ptr+1

        // ------------------------------------------------------------
        // Prepare column loop
        // ------------------------------------------------------------
        ldy     hotspot_col_start,x
        lda     hotspot_text_color

fill_row_colors_loop:
        sta     (scr_dest_ptr),y
        iny
        cpy     #$0F                         // patched with col_end_ex
        bne     fill_row_colors_loop

        // ------------------------------------------------------------
        // Advance to next row and compare with max
        // ------------------------------------------------------------
        ldy     tmp_row_index
        iny
        cpy     #$15                         // patched with row_end_ex
        bne     row_begin_compute_color_ptr
        rts
/*
================================================================================
  find_hotspot_at_cursor
================================================================================
Summary
    Hit-test the UI hotspot table using the current cursor pixel position.
    Converts cursor_x_pos and cursor_y_pos into cell coordinates (x/4, y/8),
    scans hotspot records in +6-byte strides, and returns the matching hotspot
    index. Uses inclusive start bounds and exclusive end bounds.

Global Inputs
    cursor_x_pos        current cursor X position in pixels
    cursor_y_pos        current cursor Y position in pixels
	
Vars/State
    cursor_x_cell    	written: x cell coordinate = cursor_x_pos >> 2
    cursor_y_cell    	written: y cell coordinate = cursor_y_pos >> 3
    hotspot_row_start   row start (inclusive) per hotspot, indexed by X
    hotspot_row_end_ex  row end (exclusive) per hotspot, indexed by X
    hotspot_col_start   column start (inclusive) per hotspot, indexed by X
    hotspot_col_end_ex  column end (exclusive) per hotspot, indexed by X

Returns
    X  					hotspot index on hit
						or HOTSPOT_END if none

Description
    - Derive cell coordinates from pixel positions for coarse grid hit-testing.
    - Iterate hotspot records with stride HOTSPOT_REC_STRIDE.
    - For each record, require:
        • row_start ≤ y_cell < row_end_ex
        • col_start ≤ x_cell < col_end_ex
    - On first match, return immediately with X = index.
    - If no match, return with X = HOTSPOT_END.

Notes
    - Bounds use start-inclusive, end-exclusive semantics (“_end_ex”).
    - Table layout is assumed to be tightly packed with fixed stride.
    - Caller should treat HOTSPOT_END as “no selection”.
================================================================================
*/
* = $F65C
find_hotspot_at_cursor:
        // ------------------------------------------------------------
        // Compute cursor_x_cell = cursor_x_pos / 4
        // ------------------------------------------------------------
        lda     cursor_x_pos
        lsr     
        lsr     
        sta     cursor_x_cell

        // ------------------------------------------------------------
        // Compute cursor_y_cell = cursor_y_pos / 8
        // ------------------------------------------------------------
        lda     cursor_y_pos
        lsr     
        lsr     
        lsr     
        sta     cursor_y_cell

        // ------------------------------------------------------------
        // Initialize hotspot index to #$00
        // ------------------------------------------------------------
        ldx     #$00

test_hotspot_bounds:
        // ------------------------------------------------------------
        // Check if Y cell coordinate within hotspot bounds
        // ------------------------------------------------------------
        lda     cursor_y_cell
        cmp     hotspot_row_start,x
        bcc     advance_to_next_hotspot
		
        cmp     hotspot_row_end_ex,x
        bcs     advance_to_next_hotspot

        // ------------------------------------------------------------
        // Check if X cell coordinate within hotspot bounds
        // ------------------------------------------------------------
        lda     cursor_x_cell
        cmp     hotspot_col_start,x
        bcc     advance_to_next_hotspot
		
        cmp     hotspot_col_end_ex,x
        bcs     advance_to_next_hotspot

        // ------------------------------------------------------------
        // Hotspot found → return with .X = hotspot index
        // ------------------------------------------------------------
        rts

advance_to_next_hotspot:
        // ------------------------------------------------------------
        // Move to next hotspot
        // ------------------------------------------------------------
        clc
        txa
        adc     #HOTSPOT_REC_STRIDE
        tax

        // ------------------------------------------------------------
        // Check if all hotspots tested
        // ------------------------------------------------------------
        cpx     #HOTSPOT_END
        bne     test_hotspot_bounds

        // ------------------------------------------------------------
        // No matching hotspot → return with .X = HOTSPOT_END
        // ------------------------------------------------------------
        rts

/*
Pseudo-code
		
// -----------------------------------------------------------------------------
// refresh_inventory
// -----------------------------------------------------------------------------
function refresh_inventory():
    if control_mode == CONTROL_MODE_CUTSCENE:
        return

    oldMode = control_mode
    control_mode = CONTROL_MODE_CUTSCENE

    // Map I/O + color RAM
    cpu_port = MAP_IO_IN

    refresh_inventory_regions()

    // Restore normal memory map
    cpu_port = MAP_IO_OUT

    // Restore previous UI mode
    control_mode = oldMode

// -----------------------------------------------------------------------------
// step_cursor_and_dispatch_hotspot
// -----------------------------------------------------------------------------
function step_cursor_and_dispatch_hotspot():
    // 1) Update cursor physics and grid coordinates, then find hotspot
    cursor_physics_step()
    update_cursor_grid_coords()
    newHotspot = find_hotspot_at_cursor()   // returns HOTSPOT_END if none

    // 2) If hotspot changed, handle dehighlight + physics + highlight
    if newHotspot != hotspot_entry_ofs:
        // Clear click latch when hotspot changes
        button_presses = 0

        oldHotspot = hotspot_entry_ofs

        // Dehighlight old hotspot (if any)
        if oldHotspot != HOTSPOT_END:
            oldType = hotspot_type[oldHotspot]
            // Lookup and call dehighlight handler for type
            call_dehighlight_handler_for_type(oldType)

        // Commit new hotspot
        hotspot_entry_ofs = newHotspot

        // Allow hotspot-specific cursor-physics tweaks
        update_cursor_physics_from_hotspot()

        // Highlight new hotspot (if any)
        if hotspot_entry_ofs != HOTSPOT_END:
            newType = hotspot_type[hotspot_entry_ofs]
            // Lookup and call highlight handler for type
            call_highlight_handler_for_type(newType)
    

    // 3) Handle click edge and dispatch type-specific click handler
    if detect_fire_press_edge() == PRESSED_EDGE:
        if hotspot_entry_ofs != HOTSPOT_END:
            type = hotspot_type[hotspot_entry_ofs]
            // Lookup and call click handler for type
            call_click_handler_for_type(type)
        
        // Track total presses seen while this hotspot is active
        button_presses += 1
    
// -----------------------------------------------------------------------------
// render_all_hotspots
// -----------------------------------------------------------------------------
function render_all_hotspots():
    hotspot_entry_ofs = 0

    while hotspot_entry_ofs != HOTSPOT_END:
        type = hotspot_type[hotspot_entry_ofs]

        // Call the render handler for this hotspot type
        call_render_handler_for_type(type)

        // Move to next hotspot record
        hotspot_entry_ofs += HOTSPOT_REC_STRIDE
    

    // Let cursor physics know about final hotspot context
    update_cursor_physics_from_hotspot()

// -----------------------------------------------------------------------------
// dummy_handler
// -----------------------------------------------------------------------------
function dummy_handler():
    // Intentionally does nothing
    return

// -----------------------------------------------------------------------------
// apply_highlight_color_and_fill
// -----------------------------------------------------------------------------
function apply_highlight_color_and_fill():
    if hotspot_entry_ofs == HOTSPOT_END:
        return

    type = hotspot_type[hotspot_entry_ofs]
    hotspot_text_color = hotspot_highlight_colors[type]

    fill_hotspot_color_span()

// -----------------------------------------------------------------------------
// apply_normal_color_and_fill
// -----------------------------------------------------------------------------
function apply_normal_color_and_fill():
    if hotspot_entry_ofs == HOTSPOT_END:
        return

    type = hotspot_type[hotspot_entry_ofs]
    hotspot_text_color = hotspot_normal_colors[type]

    fill_hotspot_color_span()

// -----------------------------------------------------------------------------
// render_verb_hotspot
// -----------------------------------------------------------------------------
function render_verb_hotspot():
    render_verb_label()
    apply_normal_color_and_fill()

// -----------------------------------------------------------------------------
// render_item_hotspot
// -----------------------------------------------------------------------------
function render_item_hotspot():
    render_item_name_if_owned()
    apply_normal_color_and_fill()

// -----------------------------------------------------------------------------
// render_scroll_arrow_hotspot
// -----------------------------------------------------------------------------
function render_scroll_arrow_hotspot():
    render_scroll_arrow_if_eligible()
    apply_normal_color_and_fill()

// -----------------------------------------------------------------------------
// reset_sentence_bar_hotspot
// -----------------------------------------------------------------------------
function reset_sentence_bar_hotspot():
    src_ptr = &empty_string
    blit_text_to_hotspot_row()
    apply_normal_color_and_fill()

// -----------------------------------------------------------------------------
// mark_room_scene_clicked
// -----------------------------------------------------------------------------
function mark_room_scene_clicked():
    room_scene_clicked_flag = TRUE

// -----------------------------------------------------------------------------
// set_current_verb_and_refresh
// -----------------------------------------------------------------------------
function set_current_verb_and_refresh():
    index = hotspot_entry_ofs
    verbIndex = hotspot_operand_slot[index]
    current_verb_id = verbIndex

    if button_presses != 0:
        needs_sentence_rebuild = TRUE
    else:
        init_sentence_ui_flag = TRUE
    

    sentence_bar_needs_refresh = TRUE

// -----------------------------------------------------------------------------
// rebuild_sentence
// -----------------------------------------------------------------------------
function rebuild_sentence():
    needs_sentence_rebuild = TRUE

// -----------------------------------------------------------------------------
// set_inv_item_as_do_or_io
// -----------------------------------------------------------------------------
function set_inv_item_as_do_or_io():
    // Resolve display cell index (0..3) for this hotspot
    index = hotspot_entry_ofs
    displayIndex = hotspot_operand_slot[index]

    // Try to find the inventory slot that corresponds to this visible cell,
    // for the current kid.
    (owned, slotIndex) = find_owned_slot_for_display(displayIndex)

    if owned:
        // If no preposition is set, treat this as Direct Object
        if current_preposition == 0:
            direct_object_idx_lo = displayIndex
            direct_object_idx_hi = slotIndex
        else
            // Preposition set → DO may already be set.
            // If same pair as current DO, keep DO as-is.
            if direct_object_idx_lo == displayIndex and
               direct_object_idx_hi == slotIndex:
                // Nothing changes
            else
                // Otherwise treat this as Indirect Object
                indirect_object_idx_lo = displayIndex
                indirect_object_idx_hi = slotIndex
            
    // Update sentence flags
    if button_presses != 0:
        needs_sentence_rebuild = TRUE
    
    sentence_bar_needs_refresh = TRUE

// -----------------------------------------------------------------------------
// scroll_inventory_page
// -----------------------------------------------------------------------------
function scroll_inventory_page():
    index = hotspot_entry_ofs
    operand = hotspot_operand_slot[index]   // 0 = up, 1 = down

    if operand == 1:
        // Scroll down
        inv_display_item_offset += INVENTORY_PAGE_STEP
        refresh_inventory_regions()
    else
        // Scroll up
        inv_display_item_offset -= INVENTORY_PAGE_STEP
        if inv_display_item_offset < 0:
            inv_display_item_offset = 0
        
        refresh_inventory_regions()
    
// -----------------------------------------------------------------------------
// refresh_inventory_regions
// -----------------------------------------------------------------------------
function refresh_inventory_regions():
    // Clamp inv_display_item_offset to maximum valid start
    totalOwned = count_active_kid_inventory_items()
    maxStart = totalOwned - VISIBLE_SLOTS
    if maxStart < 0:
        maxStart = 0
    
    if inv_display_item_offset > maxStart:
        inv_display_item_offset = maxStart
    
    // Preserve current hotspot index
    savedHotspot = hotspot_entry_ofs

    // Scan all hotspots
    hotspot_entry_ofs = 0
    while hotspot_entry_ofs != HOTSPOT_END:
        type = hotspot_type[hotspot_entry_ofs]

        if type == HOTSPOT_TYPE_ITEM:
            // Render inventory item name for this cell
            render_item_name_if_owned()
        else if type == HOTSPOT_TYPE_SCROLL:
            // Render up/down arrow if eligible
            render_scroll_arrow_if_eligible()
        
        hotspot_entry_ofs += HOTSPOT_REC_STRIDE
    
    // Restore original hotspot index
    hotspot_entry_ofs = savedHotspot

// -----------------------------------------------------------------------------
// render_scroll_arrow_if_eligible
// -----------------------------------------------------------------------------
function render_scroll_arrow_if_eligible():
    index = hotspot_entry_ofs
    arrowIndex = hotspot_operand_slot[index]   // 0 = up, 1 = down

    showArrow = false

    if arrowIndex == 0:
        // Up arrow: show only if there is a previous page
        if inv_display_item_offset >= MIN_OFFSET_FOR_UP:
            showArrow = true
        
    else
        // Down arrow: show only if there are >= ITEMS_THRESHOLD_FOR_DOWN items
        // beyond the current window start
        totalOwned = count_active_kid_inventory_items()
        remaining = totalOwned - inv_display_item_offset
        if remaining >= ITEMS_THRESHOLD_FOR_DOWN:
            showArrow = true
        
    if showArrow:
        src_ptr = pointer_to_inv_arrow_text(arrowIndex)
    else
        src_ptr = &empty_string
    
    blit_text_to_hotspot_row()

// -----------------------------------------------------------------------------
// render_verb_label
// -----------------------------------------------------------------------------
function render_verb_label():
    index = hotspot_entry_ofs
    verbIndex = hotspot_operand_slot[index]

    src_ptr = verb_pointers[verbIndex]   // combined lo/hi table
    blit_text_to_hotspot_row()

// -----------------------------------------------------------------------------
// render_item_name_if_owned
// -----------------------------------------------------------------------------
function render_item_name_if_owned():
    index = hotspot_entry_ofs
    displayIndex = hotspot_operand_slot[index]

    (owned, slotIndex) = find_owned_slot_for_display(displayIndex)

    if not owned:
        src_ptr = &empty_string
        blit_text_to_hotspot_row()
        return
    
    // SlotIndex is an inventory slot; resolve its object pointer
    objectBase = object_ptr_tbl[slotIndex]

    // Object layout: at offset OBJ_NAME_OFS lies the name offset
    nameOffset = objectBase[OBJ_NAME_OFS]
    src_ptr = objectBase + nameOffset

    blit_text_to_hotspot_row()

// -----------------------------------------------------------------------------
// find_owned_slot_for_display
// -----------------------------------------------------------------------------
function find_owned_slot_for_display(displayIndex) -> (bool owned, byte slotIndex):
    // Target ordinal in the owned-items sequence
    targetOrdinal = displayIndex + inv_display_item_offset
    remaining = targetOrdinal

    // Scan all inventory slots
    for slot = 0 to INVENTORY_SLOTS-1:
        objId = inventory_objects[slot]
        if objId == 0:
            continue   // empty slot

        ownerNibble = object_attributes[objId] & OWNER_NIBBLE_MASK
        if ownerNibble != current_kid_idx:
            continue   // belongs to someone else
        
        remaining -= 1
        if remaining < 0:
            // Found the Nth owned item for this display cell
            return (true, slot)
        
    // No matching owned item for this cell
    return (false, 0)

// -----------------------------------------------------------------------------
// count_active_kid_inventory_items
// -----------------------------------------------------------------------------
function count_active_kid_inventory_items() -> byte:
    count = 0

    for slot = 0 to INVENTORY_SLOTS-1:
        objId = inventory_objects[slot]
        if objId == 0:
            continue   // empty
        
        ownerNibble = object_attributes[objId] & OWNER_NIBBLE_MASK
        if ownerNibble == current_kid_idx:
            count += 1
        
    return count

// -----------------------------------------------------------------------------
// blit_text_to_hotspot_row
// -----------------------------------------------------------------------------
function blit_text_to_hotspot_row():
    index = hotspot_entry_ofs
    if index == HOTSPOT_END:
        return

    row = hotspot_row_start[index]
    colStart = hotspot_col_start[index]
    colEnd = hotspot_col_end_ex[index]
    colCount = colEnd - colStart

    // Compute destination pointer into SCREEN_BASE:
    //   rowOffset = screen_row_offset[row]
    //   dest = SCREEN_BASE + rowOffset + colStart
    dest = SCREEN_BASE + screen_row_offset[row] + colStart

    // Copy characters from src_ptr with terminator/space rules.
    // The first 0-byte switches to "fill mode", which writes spaces
    // for all remaining columns.
    fillMode = false

    for col = 0 to colCount-1:
        if not fillMode:
            ch = src_ptr[col]

            if ch == WORD_HARD_STOP:
                ch = 0   // treat as terminator
            
            if ch == 0:
                ch = SPACE_CHAR
                fillMode = true            
        else:
            ch = SPACE_CHAR
        
        dest[col] = ch
    
// -----------------------------------------------------------------------------
// fill_hotspot_color_span
// -----------------------------------------------------------------------------
function fill_hotspot_color_span():
    index = hotspot_entry_ofs
    if index == HOTSPOT_END:
        return

    color = hotspot_text_color

    rowStart = hotspot_row_start[index]
    rowEnd = hotspot_row_end_ex[index]       // exclusive
    colStart = hotspot_col_start[index]
    colEnd = hotspot_col_end_ex[index]       // exclusive

    for row = rowStart to rowEnd-1:
        // Compute base pointer into COLOR_BASE for this row
        colorRowPtr = COLOR_BASE + screen_row_offset[row]

        for col = colStart to colEnd-1:
            colorRowPtr[col] = color
        
// -----------------------------------------------------------------------------
// find_hotspot_at_cursor
// -----------------------------------------------------------------------------
function find_hotspot_at_cursor() -> byte:
    // Convert pixel coordinates to cell grid
    xCell = cursor_x_pos / 4
    yCell = cursor_y_pos / 8

    index = 0
    while index < HOTSPOT_END:
        rowStart = hotspot_row_start[index]
        rowEnd = hotspot_row_end_ex[index]
        colStart = hotspot_col_start[index]
        colEnd = hotspot_col_end_ex[index]

        if (yCell >= rowStart and yCell < rowEnd) and
           (xCell >= colStart and xCell < colEnd):
            // Hit
            return index
        
        index += HOTSPOT_REC_STRIDE
    
    // No hotspot under cursor
    return HOTSPOT_END
*/		