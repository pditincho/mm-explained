/*
================================================================================
 View rendering and scrolling system
================================================================================
Summary
	Drive the room background view: manage double-buffered tile frame buffers,
	decide between incremental scrolling and full redraw, decode compressed
	TILE/COLOR/MASK layers, and composite room objects over the result.

Description
	• Entry point
		- render_room_view is the top-level entry for drawing the current room.
		- It validates that the active room resource and its gfx-layer streams
		match the current_room, rebuilding per-column decode snapshots when
		needed.
		- It compares the camera’s target column to the current column to
		decide whether to scroll by exactly one column (left/right) or
		perform an initial full redraw of the visible window.

	• Scrolling and double buffering
		- scroll_view_left and scroll_view_right implement horizontal scrolling
		  of the TILE, COLOR, and MASK layers by one column.
		- The TILE layer uses a pair of frame buffers; depending on which is
		  active, one buffer can serve as the scroll source while the other is
		  the destination, or scrolling can occur in place in flashlight mode.
		- COLOR and MASK layers are stored in fixed layer buffers and are
		  shifted in place by one column per scroll.
		- The scroll routines scroll_left_copy and scroll_right_copy are overlap-
		  safe copy routines that move one column’s worth of data per row
		  while leaving exactly one “newly exposed” column empty for decoding.

	• Compressed background decoding
		- Each visible column in TILE, COLOR, and MASK has a snapshot of the
		  decompressor state: stream pointer, run/literal mode, remaining emit
		  count, and run symbol.
		- decode_visible_window takes a world-space column index and uses the
		  snapshot tables to restore the decompressor state for each layer,
		  then calls decode_room_gfx three times (one per layer) to fill the
		  requested column or the entire view width.
		- decode_room_gfx unpacks the mode/count header, loads the appropriate
		  4-symbol dictionary for the current layer, and dispatches either
		  decode_room_column (single column) or decode_room_all_columns (full
		  width) based on decode_scope.
		- decode_room_column uses decomp_stream_next to synthesize one vertical
		  column of VIEW_ROWS cells, walking down the column by stepping
		  through a row-major buffer with a stride of VIEW_COLS.
		- decode_room_all_columns calls decode_room_column once per column
		  across the view, updating dest_ptr as it walks horizontally.

	• Decode scope and full redraws
		- decode_scope selects between incremental single-column updates and a
		  full-window decode.
		- render_room_view sets decode_scope to DECOMPRESS_WHOLE_ROOM only for
		  the initial full redraw path; decode_visible_window resets it back to
		  single-column mode after decoding so subsequent frames default to
		  incremental updates.
		- For scroll frames, the caller positions the destination pointers at
		  the newly exposed edge column and passes the corresponding world-
		  space column index into decode_visible_window.

	• Object compositing and initialization
		- After the background (TILE/COLOR/MASK) is decoded for the requested
		  region, decode_visible_window calls render_room_objects to draw all
		  dynamic objects over the static room graphics.
		- clear_view_buffers provides a utility to initialize both tile frame
		  buffers with a uniform fill pattern before the first room decode or
		  when resetting the renderer.
================================================================================
*/
#importonce
#import "globals.inc"
#import "constants.inc"
#import "misc.asm"
#import "decompressor.asm"
#import "room_gfx_rsrc.asm"
#import "render_object.asm"

// scroll_left_copy / scroll_right_copy
.label scroll_src               = $17    // source ptr for row copy (lo/hi); set per pass (tile/color/mask)
.label scroll_dest              = $19    // destination ptr for row copy (lo/hi); set per pass (active fb/fixed)

// decode_room_column
.label dest_ptr                 = $15    // 16-bit base of top cell for current decode/write column
.label rows_remaining           = $61    // loop counter: rows left to decode/store in this column (17→0)
.label view_x_saved            = $41f9  // saves caller’s X across column decode
.label view_y_saved            = $41fb  // saves caller’s Y across column decode

// decode scope (single column vs full window)
.label decode_scope             = $41fd  // dispatch selector: 0=one column, nonzero=full 40×17

// decode_room_all_columns
.label column_ptr               = $17    // walking base (lo/hi) for next column’s dest_ptr
.label view_y_saved_2          = $41fc  // saves caller’s Y across full-surface decode

// Per-column snapshot tables (structure-of-arrays; Y indexes column)
.label tile_src_tbl_lo          = $64    // TILE stream src pointer (low byte) per column
.label tile_src_tbl_hi          = $66    // TILE stream src pointer (high byte) per column
.label tile_emit_rem_tbl        = $68    // TILE packed mode+remaining (bit7=mode; bits6..0=count)
.label tile_run_symbol_tbl      = $6a    // TILE run symbol (byte to repeat) per column

.label color_src_tbl_lo         = $6c    // COLOR stream src pointer (low byte) per column
.label color_src_tbl_hi         = $6e    // COLOR stream src pointer (high byte) per column
.label color_emit_rem_tbl       = $70    // COLOR packed mode+remaining (bit7=mode; bits6..0=count)
.label color_run_symbol_tbl     = $72    // COLOR run symbol (byte to repeat) per column

.label mask_src_tbl_lo          = $74    // MASK stream src pointer (low byte) per column
.label mask_src_tbl_hi          = $76    // MASK stream src pointer (high byte) per column
.label mask_emit_rem_tbl        = $78    // MASK packed mode+remaining (bit7=mode; bits6..0=count)
.label mask_run_symbol_tbl      = $7a    // MASK run symbol (byte to repeat) per column

// Layer destinations (row-major surfaces)
.label tile_matrix_ptr          = $17    // TILE layer output base (lo/hi) for current column window
.label color_layer_ptr          = $19    // COLOR layer output base (lo/hi) for current column window
.label mask_layer_ptr           = $1b    // MASK  layer output base (lo/hi) for current column window

// 4-symbol dictionaries (per-layer) and inline-dict anchor
.label tile_dict4               = $714a  // TILE 4-byte dictionary used by decoder
.label color_dict4              = $714e  // COLOR 4-byte dictionary used by decoder
.label mask_dict4               = $7152  // MASK  4-byte dictionary used by decoder
.label dict4_base_ptr           = $419f  // code-local anchor that gets patched to selected dict4

.const FILL_VALUE               = $00    // mem_fill_x pattern used to clear framebuffers

.const COLS_PER_ROW             = VIEW_COLS  // copy width used by scroll cores (39 when VIEW_COLS=40)

.const COLOR_LAYER_COL_1        = COLOR_LAYER_COL_0 + 1  // COLOR layer column 1 base
.const MASK_LAYER_COL_1         = MASK_LAYER_COL_0 + 1  // MASK  layer column 1 base

.const DECOMPRESS_ONE_COL       = $00    // decode_scope: single-column decode (scroll updates)
.const DECOMPRESS_WHOLE_ROOM    = $01    // decode_scope: full 40×17 decode (initial draw)

.const VIEW_LAST_COL           = $27    // last visible column index (39); used for right-edge positioning

/*
================================================================================
  render_room_view
================================================================================
Summary
	Top-level room view renderer: validates the active room’s decode snapshots,
	decides between horizontal scrolling and full redraw, scrolls the TILE /
	COLOR / MASK layers accordingly, decodes the newly exposed region, and
	renders all room objects.

Global Inputs
	current_room              Index of the room to display
	room_ptr_lo/hi_tbl        Table: room index → room resource pointer
	current_room_rsrc         Cached pointer to the currently active room resource
	room_gfx_layers_lo/hi     Pointer to current room’s gfx-layer streams
	room_gfx_layers_active_ptr	Cached active gfx-layer pointer
	cam_target_pos            Target camera column (world-space)
	cam_current_pos           Current camera column (world-space)
	viewport_left_col         World-space column index at the left edge of the viewport
	frame_buffer_base         Base address of the currently active TILE frame buffer

Global Outputs
	current_room_rsrc         Updated when the room resource pointer changes
	room_gfx_layers_active_ptr	Updated when the active gfx-layer pointer changes
	screen_scroll_flag        Set to indicate scroll direction or initial redraw (left/right/init)
	tile_matrix_ptr           Seeded to the TILE destination column or slice to	decode
	color_layer_ptr           Seeded to the COLOR destination column or slice to decode
	mask_layer_ptr            Seeded to the MASK destination column or slice to	decode
	decode_scope              Set to DECOMPRESS_WHOLE_ROOM on initial redraw
	background layers         TILE / COLOR / MASK contents updated by the decode path

Returns
	No explicit return value. On exit, the room background has been scrolled or
	redrawn as needed, and room objects have been rendered over the updated
	view.

Description
	• Validates that the cached room resource pointer and active gfx-layer
	pointer match the current_room; if either differs, calls
	pointer match the current_room; if either differs, calls
	setup_room_columns_with_decode_snapshots to rebuild per-column decode
	snapshots and layer pointers.
	• Compares cam_target_pos against cam_current_pos to classify the frame as:
		– Scroll right by one column (camera moves right, view appears to move
		left).
		– Scroll left by one column (camera moves left, view appears to move
		right).
		– No one-column delta → perform an initial full redraw.
	• For a scroll-right frame:
		– Marks screen_scroll_flag for right scroll.
		– Calls scroll_view_right to shift all layers left by one column.
		– Seeds TILE / COLOR / MASK destination pointers to the rightmost
		visible column.
		– Computes the world-space column index of the newly exposed right edge
		and calls decode_visible_window to decode that column for all layers.
	• For a scroll-left frame:
		– Marks screen_scroll_flag for left scroll.
		– Calls scroll_view_left to shift all layers right by one column.
		– Seeds TILE / COLOR / MASK destination pointers to the leftmost
		visible column.
		– Uses viewport_left_col as the world-space index of the newly exposed
		left edge and calls decode_visible_window to decode that column.
	• For an initial full redraw:
		– Marks screen_scroll_flag as “init” and sets decode_scope to
		DECOMPRESS_WHOLE_ROOM so the decoder fills the entire view width.
		– Seeds TILE / COLOR / MASK destination pointers to column 0.
		– Uses viewport_left_col as the leftmost world-space column and calls
		decode_visible_window, which decodes the full 2D window and renders
		room objects.
================================================================================
*/
* = $404D
render_room_view:
        // ------------------------------------------------------------
        // Validate room data resources for the active room; rebuild if stale
        // ------------------------------------------------------------
        ldx     current_room                  // select table slot for the active room

        // verify cached room resource base
		lda     room_ptr_lo_tbl,x             			
        cmp     current_room_rsrc
        bne     refresh_room_resources_and_snapshots 
        lda     room_ptr_hi_tbl,x             
        cmp     current_room_rsrc + 1
        bne     refresh_room_resources_and_snapshots 

		// verify published gfx-layers pointer matches active_ptr
        lda     room_gfx_layers_lo            
        cmp     room_gfx_layers_active_ptr_lo
        bne     refresh_room_resources_and_snapshots 
        lda     room_gfx_layers_hi            
        cmp     room_gfx_layers_active_ptr_hi
        bne     refresh_room_resources_and_snapshots 

		// Resources are resident, proceed to check scroll flag
        jmp     test_scroll_right

refresh_room_resources_and_snapshots:
		// ------------------------------------------------------------
        // Update cached room resource base for current_room and rebuild
        // per-column decode snapshots (src pointer, mode+count, run symbol)
        // ------------------------------------------------------------
        lda     room_ptr_lo_tbl,x
        sta     current_room_rsrc
        lda     room_ptr_hi_tbl,x
        sta     current_room_rsrc + 1
        jsr     setup_room_columns_with_decode_snapshots

test_scroll_right:
        // ------------------------------------------------------------
        // Scroll-right test: consider it a right scroll when
        // (cam_target_pos - 1) == cam_current_pos
        // ------------------------------------------------------------
        ldx     cam_target_pos                
        dex                                   
        cpx     cam_current_pos               // equal → need right scroll this frame
        bne     test_scroll_left              // not equal → test left scroll path

        // ------------------------------------------------------------
        // Perform right scroll and prep decode of new rightmost column
        // ------------------------------------------------------------
        lda     #SCROLL_RIGHT_FLAG            // set scroll flag: right scroll this frame
        sta     screen_scroll_flag
        jsr     scroll_view_right             // shift layers left; reveal new rightmost column

        //  tile_matrix_ptr := frame_buffer_base + 39 (target last visible column) 
        clc
        lda     frame_buffer_base             // base of tile layer column 0 in active FB
        adc     #<VIEW_LAST_COL               // +39 columns → column 39 base (low)
        sta     tile_matrix_ptr
        lda     frame_buffer_base + 1
        adc     #>VIEW_LAST_COL
        sta     tile_matrix_ptr + 1

        //  color_layer_ptr := COLOR_LAYER_COL_0 + 39 (last visible column) 
        clc
        lda     #<COLOR_LAYER_COL_0           // color layer column 0 base
        adc     #<VIEW_LAST_COL               // +39 → column 39 base (low)
        sta     color_layer_ptr
        lda     #>COLOR_LAYER_COL_0
        adc     #>VIEW_LAST_COL
        sta     color_layer_ptr + 1

        //  mask_layer_ptr := MASK_LAYER_COL_0 + 39 (last visible column) 
        clc
        lda     #<MASK_LAYER_COL_0            // mask layer column 0 base
        adc     #<VIEW_LAST_COL               // +39 → column 39 base (low)
        sta     mask_layer_ptr
        lda     #>MASK_LAYER_COL_0
        adc     #>VIEW_LAST_COL
        sta     mask_layer_ptr + 1

		// ------------------------------------------------------------
        // Decode visible columns starting at new right edge
        // ------------------------------------------------------------		
        lda     viewport_left_col        // compute index of new rightmost column: left_edge + 39
        clc
        adc     #VIEW_LAST_COL
        jmp     decode_visible_window    // decode TILE/COLOR/MASK starting at that column

test_scroll_left:
		// ------------------------------------------------------------
        // Scroll-left test: consider it a left scroll when
        // (cam_target_pos + 1) == cam_current_pos
        // ------------------------------------------------------------
        ldx     cam_target_pos                
        inx                                   
        cpx     cam_current_pos               // equal → need left scroll this frame
        bne     initial_full_redraw           // not equal → no scroll path

		// ------------------------------------------------------------
        // Perform left scroll and prep decode of new leftmost column
        // ------------------------------------------------------------
        lda     #SCROLL_LEFT_FLAG             // set scroll flag: left scroll this frame
        sta     screen_scroll_flag
        lda     #$00                          // legacy/side flag (unchanged behavior)
        sta     $4f
        jsr     scroll_view_left              // shift layers right; reveal new leftmost column

        //  tile_matrix_ptr := frame_buffer_base (leftmost column base) 
        lda     frame_buffer_base             // tile layer column 0 base (active FB)
        sta     tile_matrix_ptr
        lda     frame_buffer_base + 1
        sta     tile_matrix_ptr + 1

        //  color_layer_ptr := COLOR_LAYER_COL_0 (leftmost color column base) 
        lda     #<COLOR_LAYER_COL_0           // color layer column 0 base
        sta     color_layer_ptr
        lda     #>COLOR_LAYER_COL_0
        sta     color_layer_ptr + 1

        //  mask_layer_ptr := MASK_LAYER_COL_0 (leftmost mask column base) 
        lda     #<MASK_LAYER_COL_0            // mask layer column 0 base
        sta     mask_layer_ptr
        lda     #>MASK_LAYER_COL_0
        sta     mask_layer_ptr + 1

		// ------------------------------------------------------------
        // Decode visible columns starting at new left edge
        // ------------------------------------------------------------
        lda     viewport_left_col          	 // leftmost visible column index for decode
        jmp     decode_visible_window    	 // decode TILE/COLOR/MASK starting at that column

initial_full_redraw:
		// ------------------------------------------------------------
        // Initial draw (no scroll): full-window decode
        // ------------------------------------------------------------
        lda     #SCROLL_INIT_FLAG             // mark frame action: initial draw (no scroll this frame)
        sta     screen_scroll_flag

        lda     #DECOMPRESS_WHOLE_ROOM        // decode scope := full 40×17 window (not single column)
        sta     decode_scope

        //  tile_matrix_ptr := frame_buffer_base (start at leftmost tile column) 
        lda     frame_buffer_base             // tile layer column 0 base (active FB)
        sta     tile_matrix_ptr
        lda     frame_buffer_base + 1
        sta     tile_matrix_ptr + 1

        //  color_layer_ptr := COLOR_LAYER_COL_0 (leftmost color column base) 
        lda     #<COLOR_LAYER_COL_0           // color layer column 0 base
        sta     color_layer_ptr
        lda     #>COLOR_LAYER_COL_0
        sta     color_layer_ptr + 1

        //  mask_layer_ptr := MASK_LAYER_COL_0 (leftmost mask column base) 
        lda     #<MASK_LAYER_COL_0            // mask layer column 0 base
        sta     mask_layer_ptr
        lda     #>MASK_LAYER_COL_0
        sta     mask_layer_ptr + 1

        lda     viewport_left_col           // load leftmost visible column index for decode
        // fall through to decode_visible_window
/*
================================================================================
 decode_visible_window
================================================================================
Summary
	Decode the TILE, COLOR, and MASK layers for the currently visible column or
	full view slice, using per-column decode snapshots, and then render all
	room objects on top. Always resets the decompressor scope back to
	single-column mode afterward.

Arguments
	A                        Logical column index to decode:
								- For scroll: the newly exposed column index.
								- For full redraw: the leftmost visible column.
	tile_matrix_ptr          Base address of the current TILE column in the tile frame buffer
	color_layer_ptr          Base address of the current COLOR column in the color layer buffer
	mask_layer_ptr           Base address of the current MASK column in the mask layer buffer
	tile_src_tbl_lo/hi       Per-column compressed-stream pointers for TILE
	tile_emit_rem_tbl        Per-column packed emit mode/count for TILE
	tile_run_symbol_tbl      Per-column run symbol for TILE
	color_src_tbl_lo/hi      Per-column compressed-stream pointers for COLOR
	color_emit_rem_tbl       Per-column packed emit mode/count for COLOR
	color_run_symbol_tbl     Per-column run symbol for COLOR
	mask_src_tbl_lo/hi       Per-column compressed-stream pointers for MASK
	mask_emit_rem_tbl        Per-column packed emit mode/count for MASK
	mask_run_symbol_tbl      Per-column run symbol for MASK
	tile_dict4               4-byte dictionary for TILE decoding
	color_dict4              4-byte dictionary for COLOR decoding
	mask_dict4               4-byte dictionary for MASK decoding
	decode_scope             Decompressor scope selector:
								- DECOMPRESS_ONE_COL: decode a single column
								- DECOMPRESS_WHOLE_ROOM: decode full view width

Global Outputs
	decode_scope             Forced back to DECOMPRESS_ONE_COL before return
	tile_matrix_ptr          Updated for the decoded TILE region
	color_layer_ptr          Updated for the decoded COLOR region
	mask_layer_ptr           Updated for the decoded MASK region

Returns
	On return, the TILE, COLOR, and MASK layers for the requested column (or
	full view slice, if decode_scope was set accordingly) are decoded and all
	room objects have been rendered on top. decode_scope is reset to
	DECOMPRESS_ONE_COL for subsequent incremental decodes.

Description
	• Interprets A as the logical column index and copies it into Y so the
	per-column snapshot tables can be indexed consistently across TILE,
	COLOR, and MASK.
	• For the TILE layer:
		– Seeds dest_ptr from tile_matrix_ptr.
		– Points dict4_base_ptr at tile_dict4.
		– Restores the per-column decompressor state from the TILE snapshot
		tables (stream pointer, emit mode/count, run symbol).
		– Invokes decode_room_gfx, which decodes either one column or the full
		view width based on decode_scope.
	• Repeats the same sequence for the COLOR layer, using color_layer_ptr,
	color_dict4, and the COLOR snapshot tables, then for the MASK layer with
	mask_layer_ptr, mask_dict4, and MASK snapshot tables.
	• After all three layers have been decoded, forces decode_scope back to
	DECOMPRESS_ONE_COL so that subsequent scrolling frames default to
	single-column updates unless a caller explicitly requests a full redraw.
	• Finally, calls render_room_objects so dynamic room objects are drawn over
	the freshly decoded background layers before returning to the caller.
================================================================================
*/
* = $4118
decode_visible_window:
		// Y = column index := leftmost visible (used to index per-column snapshots)
        tay                                   

		// TILE layer: bind destination buffer for decode output
        lda     tile_matrix_ptr      		
        sta     dest_ptr
        lda     tile_matrix_ptr + 1         
        sta     dest_ptr + 1

        lda     #<tile_dict4                
        sta     dict4_base_ptr
        lda     #>tile_dict4                
        sta     dict4_base_ptr + 1

        lda     (tile_src_tbl_lo),y 
        sta     decomp_src_ptr_lo
        lda     (tile_src_tbl_hi),y 
        sta     decomp_src_ptr_hi

        lda     (tile_run_symbol_tbl),y   
        sta     decomp_run_symbol         

        lda     (tile_emit_rem_tbl),y     
        jsr     decode_room_gfx           

		// COLOR layer: bind destination buffer and select color dict
        lda     color_layer_ptr              
        sta     dest_ptr
        lda     color_layer_ptr + 1          
        sta     dest_ptr + 1

        lda     #<color_dict4                
        sta     dict4_base_ptr
        lda     #>color_dict4                
        sta     dict4_base_ptr + 1

        lda     (color_src_tbl_lo),y   
        sta     decomp_src_ptr_lo
        lda     (color_src_tbl_hi),y   
        sta     decomp_src_ptr_hi

        lda     (color_run_symbol_tbl),y   
        sta     decomp_run_symbol          

        lda     (color_emit_rem_tbl),y     
        jsr     decode_room_gfx            

        // MASK layer: bind destination buffer and select mask dict
        lda     mask_layer_ptr             
        sta     dest_ptr
        lda     mask_layer_ptr + 1         
        sta     dest_ptr + 1

        lda     #<mask_dict4                  
        sta     dict4_base_ptr
        lda     #>mask_dict4                  
        sta     dict4_base_ptr + 1

        lda     (mask_src_tbl_lo),y   
        sta     decomp_src_ptr_lo
        lda     (mask_src_tbl_hi),y   
        sta     decomp_src_ptr_hi

        lda     (mask_run_symbol_tbl),y 
        sta     decomp_run_symbol					

        lda     (mask_emit_rem_tbl),y      
        jsr     decode_room_gfx                  

        // From now on, decode only one column per update (for scrolling)
        lda     #DECOMPRESS_ONE_COL           
        sta     decode_scope     

		// Draw the dynamic objects onto the freshly decoded layers
        jsr     render_room_objects
        rts
/*
================================================================================
  decode_room_gfx
================================================================================
Summary
	Prepare the decompressor state for a room-gfx column/window: unpack the
	mode/count header byte, load the 4-symbol dictionary for the current layer,
	and dispatch either a single-column decode or a full-window decode based on
	decode_scope.

Arguments
	A                        Packed emit header: 
								bit7 = emit mode (run/direct)
								bits6..0 = remaining count
	dest_ptr                 Base address in the current layer buffer 
								for the column/window to be decoded
	decomp_src_ptr_lo/hi     Current position in the compressed stream for this	column/window
	decomp_run_symbol        Symbol to emit when operating in run mode
	decode_scope             Selector for single-column vs full-surface decode
								(DECOMPRESS_ONE_COL vs DECOMPRESS_WHOLE_ROOM)

Global Inputs
	dict4_base_ptr           Pointer to the 4-byte dictionary for the active
								layer (tile_dict4/color_dict4/mask_dict4)
	decomp_dict4             Target buffer that receives the 4-byte dictionary
	decomp_emit_mode         Previous emit mode (overwritten by this routine)
	decomp_emit_rem          Previous remaining count (overwritten)

Global Outputs
	decomp_emit_mode         Set to DIRECT_MODE or RUN_MODE from the header
	byte in A
	decomp_emit_rem          Set to the low 7 bits of the header byte in A
	decomp_src_ptr_lo/hi     Advanced as the underlying stream is consumed by
								decode_room_column / decode_room_all_columns
	dest_ptr                 Used by the called decoder to select the column
								base in the layer buffer
	layer buffers            Tile/color/mask layer memory updated via the
								called decode routine(s)

Returns
	On return, the selected decode routine has filled either a single column or
	an entire VIEW_COLS-wide window in the current layer buffer.

Description
	• Interprets the packed header in A: if bit7 is set, enters run mode by
	stripping bit7 into decomp_emit_rem and setting decomp_emit_mode to
	RUN_MODE; otherwise, treats A as a literal count in DIRECT_MODE.
	• Copies the 4-byte dictionary for the current layer from dict4_base_ptr
	into decomp_dict4 using COMP_DICT_MAX_OFS as the loop bound, providing
	the symbol palette used by the low-level stream decoder.
	• Reads decode_scope to decide whether to decode a single column or an
	entire VIEW_COLS-wide window:
		– DECOMPRESS_ONE_COL → calls decode_room_column.
		– DECOMPRESS_WHOLE_ROOM → calls decode_room_all_columns.
	• Leaves the compressed stream pointers, emit mode/count, dictionary, and
	layer buffer contents in a consistent state for subsequent incremental
	decodes or follow-up render passes.
================================================================================
*/
* = $418B
decode_room_gfx:
        // ------------------------------------------------------------
		// Direct or run-length mode?
        // - bit7=1 → run mode; count = A&$7F
        // - bit7=0 → direct mode; count = A
        // ------------------------------------------------------------
        bpl     set_direct_mode               // N=0 → direct mode

        // run-length mode
        and     #$7f                          // keep 7-bit repeat count
        sta     decomp_emit_rem
        lda     #RUN_MODE                     // RUN_MODE
        jmp     commit_initial_mode

set_direct_mode:
        // direct (literal) mode
        sta     decomp_emit_rem               // full 8-bit literal count
        lda     #DIRECT_MODE                  // DIRECT_MODE

commit_initial_mode:
        sta     decomp_emit_mode

        // Load 4-byte symbol dictionary into decomp_dict4[0..3]
        ldx     #COMP_DICT_MAX_OFS
load_dict4_loop:
        lda     $ffff,x                       // patched to point at inlined dict
        sta     decomp_dict4,x
        dex
        bpl     load_dict4_loop

        // Dispatch: 0 = column decode, else full-surface decode
        lda     decode_scope
        beq     decode_room_column
        jmp     decode_room_all_columns
/*
================================================================================
  decode_room_column
================================================================================
Summary
	Decode a single room-gfx column (TILE/COLOR/MASK) from the compressed
	stream into the current layer buffer, writing one byte per row down the
	screen.

Arguments
	dest_ptr                 Base address of the top cell for this column in
								the current layer buffer
	decomp_src_ptr_lo/hi     Current position in the compressed stream for this
								column (advanced by decomp_stream_next)
	decomp_emit_mode         Current emit mode for the stream (run vs direct)
	decomp_emit_rem          Remaining emit count for the current run/direct
								segment
	decomp_run_symbol        Symbol used when the stream is in run mode

Global Outputs
	dest_ptr                 May be incremented by page-crossing during the
								vertical walk down the column
	decomp_src_ptr_lo/hi     Advanced as decomp_stream_next consumes the
								compressed stream
	decomp_emit_mode         Updated whenever decomp_stream_next rolls over to
									a new segment
	decomp_emit_rem          Updated as bytes are emitted for the column

Description
	• Saves the caller’s X and Y into view_x_saved/view_y_saved so the decode
	loop can freely use registers.
	• Initializes rows_remaining to VIEW_ROWS-1 and Y to 0, then iterates once
	per visible row.
	• On each row, calls decomp_stream_next to obtain the next decoded byte and
	stores it at (dest_ptr),Y in the current layer buffer.
	• Walks vertically down the column by adding VIEW_COLS to Y for each row,
	incrementing dest_ptr+1 on page crossings so the (dest_ptr),Y addressing
	stays aligned.
	• Decrements rows_remaining until all VIEW_ROWS entries have been written,
	then restores X and Y from the saved copies and returns to the caller.
================================================================================
*/
* = $41AF
decode_room_column:
        // Save registers
        stx     view_x_saved
        sty     view_y_saved

        // Initialize loop counter and index
        lda     #VIEW_ROWS - 1
        sta     rows_remaining
        ldy     #$00

decode_store_next:
        // Decode next byte and store it at current column position
        jsr     decomp_stream_next            
        sta     (dest_ptr),y

        // Advance vertical index by row length; fix page cross if any
        tya
        clc
        adc     #VIEW_COLS
        tay
        bcc     dec_rows
        inc     dest_ptr+1                    

dec_rows:
        // Decrement remaining count and loop
        dec     rows_remaining
        bpl     decode_store_next

        // Restore registers
        ldx     view_x_saved
        ldy     view_y_saved
        rts
/*
================================================================================
  decode_room_all_columns
================================================================================
Summary
	Decode an entire VIEW_COLS-wide slice of the current layer (TILE/COLOR/MASK)
	starting at dest_ptr, calling decode_room_column once per column and
	walking horizontally across the layer buffer while preserving the caller’s
	Y register.

Arguments
	dest_ptr                 Base address of the top cell for the first (left)
								column in the current layer buffer
	decomp_src_ptr_lo/hi     Current position in the compressed stream for this
								layer (advanced by decode_room_column via
								decomp_stream_next)
	decomp_emit_mode         Current emit mode for the stream (run vs direct)
	decomp_emit_rem          Remaining emit count for the current run/direct
								segment
	decomp_run_symbol        Symbol used when the stream is in run mode

Global Outputs
	column_ptr               Seeded from dest_ptr and advanced one byte per
								column as the loop walks left→right
	dest_ptr                 Updated each iteration from column_ptr so each
								call to decode_room_column targets the correct
								column base
	decomp_src_ptr_lo/hi     Advanced as the underlying stream is consumed by
								decode_room_column
	decomp_emit_mode         Updated when the stream rolls to a new segment
	decomp_emit_rem          Updated as bytes are emitted across all columns

Description
	• Saves the caller’s Y into view_y_saved_2 so the column loop can reuse Y
	as a column index without disturbing the caller’s state.
	• Copies dest_ptr into column_ptr to seed the horizontal cursor for the
	first (leftmost) column.
	• Initializes Y to VIEW_COLS-1 and iterates once per column, counting down
	from the rightmost column index to 0.
	• On each iteration, copies column_ptr into dest_ptr and calls
	decode_room_column, which decodes VIEW_ROWS rows for that column from the
	current compressed stream state.
	• After each column is decoded, increments column_ptr by one byte so the
	next iteration starts at the next column to the right in the layer
	buffer.
	• When all VIEW_COLS columns have been decoded, restores Y from
	view_y_saved_2 and returns with the compressed stream state and layer
	buffer fully updated for this layer.
================================================================================
*/
* = $41D4
decode_room_all_columns:
        // Save Y
        sty     view_y_saved_2
		
		// Seed column_ptr := dest_ptr
        lda     dest_ptr
        sta     column_ptr
        lda     dest_ptr + 1
        sta     column_ptr + 1

        // Decode columns: Y := $27 .. $00  (40 columns total)
        ldy     #VIEW_COLS - 1
decode_col_loop:
		// Decode one column
        jsr     decode_room_column    

        // Advance to next column base)
        inc     column_ptr
        bne     set_col_base_next
        inc     column_ptr + 1
set_col_base_next:
        // dest_ptr := column_ptr  (set base for next column decode)
        lda     column_ptr
        sta     dest_ptr
        lda     column_ptr + 1
        sta     dest_ptr + 1

        // Next column until Y becomes -1
        dey
        bpl     decode_col_loop

        // Restore Y
        ldy     view_y_saved_2
        rts
/*
================================================================================
  scroll_view_left
================================================================================
Summary
    Shift the visible room view one column to the right (visual scroll left) by
    horizontally copying existing columns in the tile, color, and mask layers,
    leaving the leftmost column free for a freshly decoded column.

Global Inputs
    frame_buffer            Active view frame buffer index (0 or 1).
    frame_buffer_base       Base address of the currently active view buffer.
    global_lights_state     Global lighting mode; when set to “flashlight
                            only”, tile scrolling is done in place rather than
                            from the alternate buffer.

Description
    - Tile layer (frame buffers):
        • If double-buffering is active, choose the non-active frame buffer as
          scroll_src and the active frame buffer (frame_buffer_base) as
          scroll_dest so tile data is scrolled from the previous frame into the
          current one.
        • If global_lights_state indicates “flashlight only”, override the
          choice and use the active frame buffer as both scroll_src and
          scroll_dest so the scroll is performed in place.
        • Point scroll_dest at column 1 in the destination so the copy moves
          existing columns one step to the right, leaving column 0 free for a
          fresh decode of the new leftmost column.
        • Call scroll_left_copy to copy 39 bytes per row from right to left,
          ensuring correct behavior even when source and destination rows
          overlap in memory.

    - Color layer:
        • Set scroll_src to COLOR_LAYER_COL_0 and scroll_dest to
          COLOR_LAYER_COL_1 so the existing color data is shifted one column to
          the right.
        • Call scroll_left_copy so the color layer stays aligned with the
          scrolled tiles and column 0 in the color layer is available for the
          newly decoded column.

    - Mask layer:
        • Set scroll_src to MASK_LAYER_COL_0 and scroll_dest to
          MASK_LAYER_COL_1.
        • Call scroll_left_copy again so the foreground mask index layer
          scrolls in lockstep with tiles and colors, preserving occlusion while
          freeing the leftmost mask column for new data.

Notes
    - The asymmetric copy pattern in scroll_left_copy (39 columns, copied
      right-to-left) is deliberate: it avoids overwriting source data when
      scroll_src and scroll_dest overlap and leaves one free column for the
      decode path that draws the newly revealed column at the view’s left edge.
	  
================================================================================

How columns are copied 

Goal:
- Shift the view right by 1 column. Copy 39 bytes per row (39 is the view width in columns - 1):
    source columns 0..38 → destination columns 1..39
- Column 0 of the destination is left for the newly revealed content.

Per-row mapping (indexes are column numbers within a 40-byte row):
- Copy order: y = 38, 37, ..., 0  (right→left to avoid overlap)
- Assignment: dest[1 + y] := source[0 + y]

Examples at row r:
  y=38 → dest[r, 39] := source[r, 38]
  y=37 → dest[r, 38] := source[r, 37]
   ...
  y= 1 → dest[r,  2] := source[r,  1]
  y= 0 → dest[r,  1] := source[r,  0]

Resulting row picture:
  BEFORE (source row r):  [ c0  c1  c2 ... c38  c39 ]
  AFTER  (dest row r):    [  ?  c0  c1 ... c37  c38 ]
                             ^                   
               (c0 left for later draw) 

Implementation detail (tile framebuffer pass):
- Destination pointer is advanced by +1 before the copy:
    scroll_dest := scroll_dest + 1
- The loop then copies 39 bytes right→left:
    for y = 38..0: *(scroll_dest + y) := *(scroll_src + y)
- Because we read from the same index as we write (and dest is one byte ahead),
  right→left order guarantees we do not overwrite bytes we still need to read.

Color/Mask passes:
- For fixed layers, set:
    scroll_src := <LAYER>_COL_0   ; column 0 base
    scroll_dest := <LAYER>_COL_1  ; column 1 base
  Then perform the same 39-byte, right→left copy.

Why 39 bytes (not 40)?
- Copying 40 with src=dest-1 would read past the end of the row on the last byte.
- 39 bytes ensures no out-of-bounds reads; column 0 (newly revealed) is drawn later.
================================================================================
*/
* = $41FE
scroll_view_left:
		// Choose tile layer source based on active frame buffer
        lda     frame_buffer
        cmp     #$01
        bne     frame_buffer_2

        // ------------------------------------------------------------
		// Frame buffer 1 is active
		// Select frame buffer 0 as source
		// Load VIEW_FRAME_BUF_0 into scroll_src
        // ------------------------------------------------------------
        lda     #<VIEW_FRAME_BUF_0
        sta     scroll_src
        lda     #>VIEW_FRAME_BUF_0
        sta     scroll_src + 1
        jmp     lscroll_set_dest_to_active_fb

frame_buffer_2:
        // ------------------------------------------------------------
		// Frame buffer 0 is active
		// Select frame buffer 1 as source
		// Load VIEW_FRAME_BUF_1 into scroll_src
        // ------------------------------------------------------------
        lda     #<VIEW_FRAME_BUF_1
        sta     scroll_src
        lda     #>VIEW_FRAME_BUF_1
        sta     scroll_src + 1

lscroll_set_dest_to_active_fb:
        // ------------------------------------------------------------
		// Set destination to the active frame buffer base
        // ------------------------------------------------------------
        lda     frame_buffer_base
        sta     scroll_dest
        lda     frame_buffer_base + 1
        sta     scroll_dest + 1

		// Lights mode == Flashlight?
        lda     global_lights_state
        cmp     #$01
        bne     move_one_column_left
		
        // ------------------------------------------------------------
		// Lights == Flashlight - set source = frame buffer base (no column offset)
        // ------------------------------------------------------------
        lda     frame_buffer_base
        sta     scroll_src
        lda     frame_buffer_base + 1
        sta     scroll_src + 1

move_one_column_left:
        // ------------------------------------------------------------
		// Lights != Flashlight
		//
		// Start writing at column #1: scroll_dest := scroll_dest + 1
		// - Copy will run right→left (39 bytes) into columns 0..38
        // ------------------------------------------------------------
        inc     scroll_dest
        bne     lscroll_copy_tiles_block
        inc     scroll_dest + 1

lscroll_copy_tiles_block:
        //Copy TILE layer
        jsr     scroll_left_copy

        // ------------------------------------------------------------
		// Copy COLOR layer
		//
		// - Set scroll_src to COLOR_LAYER_COL_0 (column #0 start)
		// - Set scroll_dest to COLOR_LAYER_COL_1 (column 1 start)
		// - Copy 40×17 block right→left
        // ------------------------------------------------------------
        lda     #<COLOR_LAYER_COL_0           
        sta     scroll_src
        lda     #>COLOR_LAYER_COL_0
        sta     scroll_src + 1
        lda     #<COLOR_LAYER_COL_1           
        sta     scroll_dest
        lda     #>COLOR_LAYER_COL_1
        sta     scroll_dest + 1
        jsr     scroll_left_copy

        // ------------------------------------------------------------
		// Copy MASK layer
		//
		// - Set scroll_src to MASK_LAYER_COL_0 (column #0 start)
		// - Set scroll_dest to MASK_LAYER_COL_1 (column 1 start)
		// - Copy 40×17 block right→left
        // ------------------------------------------------------------
        lda     #<MASK_LAYER_COL_0            
        sta     scroll_src
        lda     #>MASK_LAYER_COL_0
        sta     scroll_src + 1
        lda     #<MASK_LAYER_COL_1            
        sta     scroll_dest
        lda     #>MASK_LAYER_COL_1
        sta     scroll_dest + 1
        jsr     scroll_left_copy

        rts
/*
================================================================================
  scroll_view_right
================================================================================
Summary
    Shift the visible room view one column to the left (visual scroll right) by
    copying existing columns horizontally and preparing the rightmost column
    for a fresh decode across the tile, color, and mask layers.

Global Inputs
    frame_buffer            Index of the active view frame buffer.
    frame_buffer_base       Base address of the active view frame buffer.
    global_lights_state     Global lighting mode; when set to “flashlight
                            only”, tile scrolling is done in place.

Description
    - Tile layer (view frame buffers):
        • Choose the non-active frame buffer as scroll_src and the active
          frame buffer (frame_buffer_base) as scroll_dest, implementing
          double-buffered scrolling.
        • If global_lights_state indicates “flashlight only”, override and
          use the active frame buffer as both scroll_src and scroll_dest so
          scrolling happens in place.
        • Advance scroll_src by one column to align it with the column
          immediately to the right of scroll_dest’s column 0.
        • Call scroll_right_copy to copy each row left-to-right, effectively
          shifting visible tile columns left and leaving the rightmost column
          available for a fresh decode.

    - Color layer:
        • Set scroll_src to COLOR_LAYER_COL_1 and scroll_dest to
          COLOR_LAYER_COL_0.
        • Call scroll_right_copy to shift the color layer one column left,
          preserving per-cell colors for the scrolled content and freeing the
          rightmost color column for new data.

    - Mask layer:
        • Set scroll_src to MASK_LAYER_COL_1 and scroll_dest to
          MASK_LAYER_COL_0.
        • Call scroll_right_copy again to shift the foreground mask index
          layer left by one column, keeping object occlusion in sync with the
          scrolled tiles.

Notes
    - The scroll_right_copy kernel performs a full-row copy left-to-right
      using the caller’s scroll_src and scroll_dest. For non-overlapping
      source/destination pairs (e.g., different frame buffers), this is a
      straightforward 40-column copy; when buffers overlap, the caller’s
      choice of scroll_src and scroll_dest determines the effective horizontal
      shift and must account for the full-span copy pattern.
	  
================================================================================

How columns are copied

Goal:
- Achieve a rightward visual scroll by shifting framebuffer bytes left by 1.
- Copy 39 bytes per row, left→right:
    source columns 1..39 → destination columns 0..38
- Column 39 (rightmost) is left for the newly revealed content.

Per-row mapping (indexes are column numbers within a 40-byte row):
- Copy order: y = 0, 1, ..., 38  (left→right to avoid overlap when src = dest+1)
- Assignment: dest[0 + y] := source[1 + y]

Examples at row r:
  y= 0 → dest[r,  0] := source[r,  1]
  y= 1 → dest[r,  1] := source[r,  2]
   ...
  y=37 → dest[r, 37] := source[r, 38]
  y=38 → dest[r, 38] := source[r, 39]

Resulting row picture:
  BEFORE (source row r):  [ c0  c1  c2 ... c38  c39 ]
  AFTER  (dest row r):    [ c1  c2  c3 ... c39   ?  ]
                                                ^
                                    column 39 drawn later (new content)

Why left→right order is safe:
- Caller sets scroll_src = scroll_dest + 1 (read one byte ahead of where we write).
- Copying from low to high addresses ensures we always read a byte before the byte
  that will be overwritten by the shift, avoiding self-overwrite.

Color/Mask passes:
- Same mapping applies. Their bases are set so that “source column 1” is copied into
  “destination column 0” across 39 bytes per row.

Note on byte count:
- Copy exactly 39 bytes per row to avoid reading past the end of the row when
  scroll_src = scroll_dest + 1. Column 39 is filled by the code that draws the
  newly revealed column.
  
================================================================================
*/
* = $425E
scroll_view_right:
		// Choose tile layer source based on active frame buffer
        lda     frame_buffer
        cmp     #$01
        bne     pick_other_fb_when_fb0

        // ------------------------------------------------------------
		// Frame buffer 1 is active
		// Select frame buffer 0 as source
		// Load VIEW_FRAME_BUF_0 into scroll_src
        // ------------------------------------------------------------
        lda     #<VIEW_FRAME_BUF_0
        sta     scroll_src
        lda     #>VIEW_FRAME_BUF_0
        sta     scroll_src + 1
        jmp     set_dest_to_active_fb

pick_other_fb_when_fb0:
        // ------------------------------------------------------------
		// Frame buffer 0 is active
		// Select frame buffer 1 as source
		// Load VIEW_FRAME_BUF_1 into scroll_src
        // ------------------------------------------------------------
        lda     #<VIEW_FRAME_BUF_1
        sta     scroll_src
        lda     #>VIEW_FRAME_BUF_1
        sta     scroll_src + 1

set_dest_to_active_fb:
        // ------------------------------------------------------------
		// Set destination to the active frame buffer base
        // ------------------------------------------------------------
        lda     frame_buffer_base            
        sta     scroll_dest
        lda     frame_buffer_base + 1            
        sta     scroll_dest + 1

		// Lights mode == Flashlight?
        lda     global_lights_state  
        cmp     #$01                 
        bne     offset_src_to_col1            

        // ------------------------------------------------------------
		// Lights == Flashlight - set source = frame buffer base (no column offset)
        // ------------------------------------------------------------
        lda     frame_buffer_base            
        sta     scroll_src
        lda     frame_buffer_base + 1            
        sta     scroll_src + 1

offset_src_to_col1:
        // ------------------------------------------------------------
		// Lights != Flashlight
		// Shift by one column: start reading at column #1 (scroll_src += 1)
        // ------------------------------------------------------------
        inc     scroll_src
        bne     copy_tiles_block
        inc     scroll_src + 1

copy_tiles_block:
		// Copy tiles layer: columns 1..39 → 0..38 (40×17 block)
        jsr     scroll_right_copy

        // ------------------------------------------------------------
		// Copy COLOR layer
		//
		// - Set scroll_src to COLOR_LAYER_COL_1 (column #1 start)
		// - Set scroll_dest to COLOR_LAYER_COL_0 (column 0 start)
		// - Copy 40×17 block left→right
        // ------------------------------------------------------------
        lda     #<COLOR_LAYER_COL_1           
        sta     scroll_src
        lda     #>COLOR_LAYER_COL_1           
        sta     scroll_src + 1
        lda     #<COLOR_LAYER_COL_0           
        sta     scroll_dest
        lda     #>COLOR_LAYER_COL_0           
        sta     scroll_dest + 1
        jsr     scroll_right_copy             

        // ------------------------------------------------------------
		// Copy MASK layer
		//
		// - Set scroll_src to MASK_LAYER_COL_1 (column #1 start)
		// - Set scroll_dest to MASK_LAYER_COL_0 (column 0 start)
		// - Copy 40×17 block left→right
        // ------------------------------------------------------------
        lda     #<MASK_LAYER_COL_1            
        sta     scroll_src
        lda     #>MASK_LAYER_COL_1            
        sta     scroll_src + 1
        lda     #<MASK_LAYER_COL_0            
        sta     scroll_dest
        lda     #>MASK_LAYER_COL_0            
        sta     scroll_dest + 1
        jsr     scroll_right_copy             

        rts
/*
================================================================================
  scroll_left_copy
================================================================================
Summary
    Copy a rectangular view region row by row from scroll_src to scroll_dest,
    moving right-to-left within each row and copying only the leftmost
    COLS_PER_ROW-1 bytes. Used as the core copy kernel for leftward view
    scrolling (visual view moves left, contents shift right by one column).

Global Inputs
    scroll_src          Pointer to the start of the source view region for the
                        current row.
    scroll_dest         Pointer to the start of the destination view region for
                        the current row.

Global Outputs
    scroll_dest         For each of VIEW_ROWS rows, COLS_PER_ROW-1 bytes at the
                        destination are overwritten with data from scroll_src,
                        effectively shifting the visible content one column
                        to the right.

Description
    - For each of VIEW_ROWS rows:
        - Initialize Y to COLS_PER_ROW-2 so the first copy in the row targets
          the second-to-last column.
        - Copy COLS_PER_ROW-1 bytes from scroll_src to scroll_dest, stepping
          right-to-left within the row:
              dest[Y] := src[Y] for Y in [COLS_PER_ROW-2 .. 0].
        - After finishing a row, advance both scroll_src and scroll_dest by
          COLS_PER_ROW bytes so they point at the next row.
    - Because the copy proceeds from right to left and omits the final column
      (COLS_PER_ROW-1), overlapping source and destination layouts where
      scroll_dest is one column to the right of scroll_src are safe:
        - Each destination cell reads its source before that source cell is
          overwritten.
        - The rightmost destination column is left untouched, ready to receive
          freshly decoded data for the newly revealed column on the leftward
          scroll.

Notes
    - Typical call patterns:
        - Tile layer: scroll_src and scroll_dest may point into the same frame
          buffer, with scroll_dest offset by one column to implement an
          in-place right shift.
        - Color and mask layers: scroll_src points at column 0 and scroll_dest
          at column 1, achieving the same 1-column right shift for those
          parallel layers.
================================================================================
*/
* = $42BE
scroll_left_copy:
		// Initialize row counter (X := 16 .. 0 → 17 rows total)
        ldx     #VIEW_ROWS - 1

left_copy_next_row:
		// Copy one row right→left: Y := 38 .. 0 (39 bytes)
        ldy     #VIEW_COLS - 2
left_copy_next_col:
        lda     (scroll_src),y
        sta     (scroll_dest),y
        dey
        bpl     left_copy_next_col

		// Advance source pointer to next row (scroll_src += 40)
        clc
        lda     scroll_src
        adc     #<COLS_PER_ROW
        sta     scroll_src
        lda     scroll_src + 1
        adc     #>COLS_PER_ROW
        sta     scroll_src + 1

		// Advance destination pointer to next row (scroll_dest += 40)
        clc
        lda     scroll_dest
        adc     #<COLS_PER_ROW
        sta     scroll_dest
        lda     scroll_dest + 1
        adc     #>COLS_PER_ROW
        sta     scroll_dest + 1

		// Next row until X becomes -1 
        dex
        bpl     left_copy_next_row
        rts
/*
================================================================================
  scroll_right_copy
================================================================================
Summary
    Copy an entire rectangular view region row by row from scroll_src to
    scroll_dest, moving left-to-right within each row. Used as the core copy
    kernel for rightward view scrolling.

Global Inputs
    scroll_src          Pointer to the start of the source view region for the
                        current row.
    scroll_dest         Pointer to the start of the destination view region for
                        the current row.

Global Outputs
    scroll_dest         For each of VIEW_ROWS rows, COLS_PER_ROW bytes at the
                        destination are overwritten with data from scroll_src.

Description
    - For each of VIEW_ROWS rows:
        • Initialize Y to 0.
        • Copy COLS_PER_ROW bytes from the current scroll_src row to the
          current scroll_dest row, stepping left-to-right:
              dest[Y] := src[Y] for Y in [0 .. COLS_PER_ROW-1].
        • After finishing a row, advance both scroll_src and scroll_dest by
          COLS_PER_ROW bytes so they point at the next row.
    - The caller is responsible for choosing scroll_src and scroll_dest such
      that the copy pattern corresponds to a “scroll view right” operation
      (typically by setting scroll_src one column ahead of scroll_dest and
      then decoding the new rightmost column separately).

Notes
    - With non-overlapping buffers (e.g., source and destination in different
      frame buffers), copying COLS_PER_ROW bytes per row is safe and performs
      a straightforward full-row copy.
    - When using overlapping buffers to implement an in-place horizontal shift,
      callers must ensure that any dependence on a “one-column-ahead” source
      layout accounts for the fact that this routine copies the full
      COLS_PER_ROW span per row.
================================================================================
*/
* = $42E7
scroll_right_copy:
        // Initialize row counter (X := 16 .. 0 → 17 rows total)
        ldx     #VIEW_ROWS - 1

right_copy_next_row:
        // Copy one row: Y := 0 .. 39, byte-by-byte left → right
        ldy     #$00
right_copy_next_col:
        lda     (scroll_src),y
        sta     (scroll_dest),y
        iny
        cpy     #COLS_PER_ROW
        bne     right_copy_next_col

        // Advance scroll_src pointer to next row (scroll_src += 40)
        clc
        lda     scroll_src
        adc     #<COLS_PER_ROW
        sta     scroll_src
        lda     scroll_src + 1
        adc     #>COLS_PER_ROW
        sta     scroll_src + 1

        // Advance destination pointer to next row (scroll_dest += 40)
        clc
        lda     scroll_dest
        adc     #<COLS_PER_ROW
        sta     scroll_dest
        lda     scroll_dest + 1
        adc     #>COLS_PER_ROW
        sta     scroll_dest + 1

        // Next row until X becomes -1
        dex
        bpl     right_copy_next_row
        rts
/*
================================================================================
  clear_view_buffers
================================================================================
Summary
    Clear both room view frame buffers to a known fill value, so subsequent
    room decoding and scrolling start from a clean background.

Global Outputs
    VIEW_FRAME_BUF_0    Contents replaced with FILL_VALUE across the full view
                        area.
    VIEW_FRAME_BUF_1    Contents replaced with FILL_VALUE across the full view
                        area.

Description
    - Program fill_dest_ptr and fill_byte_cnt to cover VIEW_FRAME_BUF_0, load
      X with FILL_VALUE, and invoke mem_fill_x to clear the first view buffer.
    - Reprogram fill_dest_ptr and fill_byte_cnt to cover VIEW_FRAME_BUF_1,
      keep X as FILL_VALUE, and invoke mem_fill_x again to clear the second
      view buffer.
    - Leaves both view frame buffers in a uniform, predictable state ready for
      room background decoding and subsequent scroll updates.
================================================================================
*/
* = $5F4B
clear_view_buffers:
        // Clear VIEW_FRAME_BUF_0 with FILL_VALUE for VIEW_FRAME_BUF_SIZE bytes
        lda     #<VIEW_FRAME_BUF_0           
        sta     fill_dest_ptr
        lda     #>VIEW_FRAME_BUF_0           
        sta     fill_dest_ptr + 1
        lda     #<VIEW_FRAME_BUF_SIZE        
        sta     fill_byte_cnt
        lda     #>VIEW_FRAME_BUF_SIZE        
        sta     fill_byte_cnt + 1
        ldx     #FILL_VALUE                  
        jsr     mem_fill_x                   

        // Clear VIEW_FRAME_BUF_1 with FILL_VALUE for VIEW_FRAME_BUF_SIZE bytes
        lda     #<VIEW_FRAME_BUF_1           
        sta     fill_dest_ptr
        lda     #>VIEW_FRAME_BUF_1           
        sta     fill_dest_ptr + 1
        lda     #<VIEW_FRAME_BUF_SIZE        
        sta     fill_byte_cnt
        lda     #>VIEW_FRAME_BUF_SIZE        
        sta     fill_byte_cnt + 1 
        ldx     #FILL_VALUE                  
        jsr     mem_fill_x                   
        rts

/*
function render_room_view():
    room_index = current_room

    # Ensure room resource base is up to date
    if room_ptr_tbl[room_index] != current_room_rsrc:
        current_room_rsrc = room_ptr_tbl[room_index]
        setup_room_columns_with_decode_snapshots()

    # Ensure active gfx-layer base matches the current room
    if room_gfx_layers != room_gfx_layers_active_ptr:
        room_gfx_layers_active_ptr = room_gfx_layers
        setup_room_columns_with_decode_snapshots()

    # Decide whether we are doing a one-column scroll or a full redraw
    if cam_target_pos - 1 == cam_current_pos:
        # Scroll right (camera moves right, view shifts left)
        screen_scroll_flag = SCROLL_RIGHT_FLAG

        scroll_view_right()

        # Prepare to decode the newly exposed rightmost column
        tile_matrix_ptr  = frame_buffer_base + VIEW_LAST_COL
        color_layer_ptr  = COLOR_LAYER_COL_0 + VIEW_LAST_COL
        mask_layer_ptr   = MASK_LAYER_COL_0  + VIEW_LAST_COL

        new_world_col = viewport_left_col + VIEW_LAST_COL
        decode_visible_window(new_world_col)
        return

    if cam_target_pos + 1 == cam_current_pos:
        # Scroll left (camera moves left, view shifts right)
        screen_scroll_flag = SCROLL_LEFT_FLAG
        legacy_scroll_flag_4f = LEGACY_SCROLL_LEFT

        scroll_view_left()

        # Prepare to decode the newly exposed leftmost column
        tile_matrix_ptr  = frame_buffer_base + 0
        color_layer_ptr  = COLOR_LAYER_COL_0 + 0
        mask_layer_ptr   = MASK_LAYER_COL_0  + 0

        new_world_col = viewport_left_col
        decode_visible_window(new_world_col)
        return

    # No 1-column delta → full redraw of the visible window
    screen_scroll_flag = SCROLL_INIT_FLAG
    decode_scope = DECOMPRESS_WHOLE_ROOM

    tile_matrix_ptr  = frame_buffer_base + 0
    color_layer_ptr  = COLOR_LAYER_COL_0 + 0
    mask_layer_ptr   = MASK_LAYER_COL_0  + 0

    left_world_col = viewport_left_col
    decode_visible_window(left_world_col)


function decode_visible_window(column_index):
    # Use Y as column index into snapshot tables
    y = column_index

    # --- TILE layer ---
    dest_ptr       = tile_matrix_ptr
    dict4_base_ptr = tile_dict4

    decomp_src_ptr = tile_src_tbl[y]
    packed_header  = tile_emit_rem_tbl[y]
    decomp_run_symbol = tile_run_symbol_tbl[y]

    decode_room_gfx(packed_header)

    # --- COLOR layer ---
    dest_ptr       = color_layer_ptr
    dict4_base_ptr = color_dict4

    decomp_src_ptr = color_src_tbl[y]
    packed_header  = color_emit_rem_tbl[y]
    decomp_run_symbol = color_run_symbol_tbl[y]

    decode_room_gfx(packed_header)

    # --- MASK layer ---
    dest_ptr       = mask_layer_ptr
    dict4_base_ptr = mask_dict4

    decomp_src_ptr = mask_src_tbl[y]
    packed_header  = mask_emit_rem_tbl[y]
    decomp_run_symbol = mask_run_symbol_tbl[y]

    decode_room_gfx(packed_header)

    # After this call, further decodes default to single-column mode
    decode_scope = DECOMPRESS_ONE_COL

    # Draw dynamic room objects over freshly decoded background
    render_room_objects()


function decode_room_gfx(packed_header):
    # Interpret bit7 as mode, bits0..6 as remaining count
    if packed_header has bit7 set:
        decomp_emit_mode = RUN_MODE
        decomp_emit_rem  = packed_header & 0x7F
    else:
        decomp_emit_mode = DIRECT_MODE
        decomp_emit_rem  = packed_header

    # Load 4-symbol dictionary for this layer
    for i from 0 to COMP_DICT_MAX_OFS:
        decomp_dict4[i] = dict4_base_ptr[i]

    # Dispatch based on scope: per-column or full 2D view
    if decode_scope == DECOMPRESS_ONE_COL:
        decode_room_column()
    else:
        decode_room_all_columns()


function decode_room_column():
    # Decode one entire column (VIEW_ROWS rows)
    rows_remaining = VIEW_ROWS - 1
    y_offset = 0

    while rows_remaining >= 0:
        value = decomp_stream_next()     # Uses decomp_src_ptr, decomp_emit_mode, etc.
        dest_ptr[y_offset] = value

        # Move down one row in this column: stride by VIEW_COLS
        y_offset += VIEW_COLS

        # If y_offset crosses a page boundary, adjust dest_ptr high byte accordingly
        adjust_dest_ptr_for_page_cross_if_needed()

        rows_remaining -= 1


function decode_room_all_columns():
    column_ptr = dest_ptr          # Base of leftmost column
    col_index  = VIEW_COLS - 1     # We iterate columns from right → left

    while col_index >= 0:
        dest_ptr = column_ptr      # Decode one column at column_ptr
        decode_room_column()

        column_ptr += 1            # Move to next column to the right
        col_index  -= 1

function scroll_view_left():
    # --- Tile layer: choose source frame buffer based on double-buffering ---
    if frame_buffer == 1:
        tile_src_base = VIEW_FRAME_BUF_0   # active is 1 → source is 0
    else:
        tile_src_base = VIEW_FRAME_BUF_1   # active is 0 → source is 1

    scroll_src  = tile_src_base
    scroll_dest = frame_buffer_base        # active frame buffer is destination

    # Flashlight mode changes tile copy to in-place
    if global_lights_state == FLASHLIGHT_ONLY:
        scroll_src = frame_buffer_base     # src == dest, in-place
        # Copy full row range; caller’s layout ensures correct “shift right”
    else:
        # Normal mode: write starting at column 1 so columns 0..(N-2) get data
        scroll_dest = frame_buffer_base + 1

    # Copy tile layer block using 39-byte right→left kernel
    scroll_left_copy()

    # --- COLOR layer: fixed base addresses, no double-buffering ---
    scroll_src  = COLOR_LAYER_COL_0        # start at column 0
    scroll_dest = COLOR_LAYER_COL_1        # write starting at column 1
    scroll_left_copy()

    # --- MASK layer: same pattern as COLOR ---
    scroll_src  = MASK_LAYER_COL_0
    scroll_dest = MASK_LAYER_COL_1
    scroll_left_copy()

    # After this, each layer’s columns have been shifted one to the right;
    # column 0 in each layer is free for a newly decoded column.


function scroll_view_right():
    # --- Tile layer: double-buffering selection ---
    if frame_buffer == 1:
        tile_src_base = VIEW_FRAME_BUF_0   # non-active as source
    else:
        tile_src_base = VIEW_FRAME_BUF_1

    scroll_src  = tile_src_base
    scroll_dest = frame_buffer_base        # active frame buffer

    # Flashlight mode → in-place scrolling
    if global_lights_state == FLASHLIGHT_ONLY:
        scroll_src = frame_buffer_base     # src == dest, in-place

    # For a left shift, source begins at column 1, destination at column 0
    scroll_src = scroll_src + 1
    # scroll_dest stays at column 0

    # Copy tile layer block using full-row left→right kernel
    scroll_right_copy()

    # --- COLOR layer: fixed bases, shift left by 1 ---
    scroll_src  = COLOR_LAYER_COL_1        # start at column 1
    scroll_dest = COLOR_LAYER_COL_0        # write to column 0
    scroll_right_copy()

    # --- MASK layer: same as COLOR ---
    scroll_src  = MASK_LAYER_COL_1
    scroll_dest = MASK_LAYER_COL_0
    scroll_right_copy()

    # After this, columns 0..(N-2) are filled from 1..(N-1),
    # and the rightmost column (N-1) is free for a new decode.


function scroll_left_copy():
    # Copy block row by row, shifting data to the right by 1 column.
    # Precondition (normal case): scroll_dest = scroll_src + 1

    for row in 0 .. VIEW_ROWS-1:
        # Start at second-to-last column index (VIEW_COLS-2)
        # and walk right→left so overlapping src/dest are safe.
        for col in (VIEW_COLS - 2) down to 0:
            dest_row_base = scroll_dest + row * COLS_PER_ROW
            src_row_base  = scroll_src  + row * COLS_PER_ROW

            dest_row_base[col] = src_row_base[col]

        # (Pointer arithmetic is abstracted into the row*COLS_PER_ROW terms
        # here; in assembly it bumps scroll_src/scroll_dest by COLS_PER_ROW.)

    # Column (VIEW_COLS-1) of each row is left untouched; caller treats it
    # as the “newly exposed” column to be filled by the decoder.


function scroll_right_copy():
    # Copy block row by row, shifting data to the left by 1 column.
    # Precondition (normal case): scroll_src = scroll_dest + 1

    for row in 0 .. VIEW_ROWS-1:
        # Walk left→right across the entire row width
        for col in 0 .. COLS_PER_ROW-1:
            dest_row_base = scroll_dest + row * COLS_PER_ROW
            src_row_base  = scroll_src  + row * COLS_PER_ROW

            dest_row_base[col] = src_row_base[col]

        # (As above, row stepping is abstracted; the assembly increments
        # scroll_src/scroll_dest by COLS_PER_ROW each iteration.)

    # With the usual layout, columns 0..(N-2) get data from 1..N-1.
    # Column N-1 is considered “newly exposed” and will be decoded next.


function clear_view_buffers():
    # Clear tile frame buffer 0
    fill_dest_ptr = VIEW_FRAME_BUF_0
    fill_byte_cnt = VIEW_FRAME_BUF_SIZE
    mem_fill_x(FILL_VALUE)      # Fills [fill_dest_ptr, fill_dest_ptr + fill_byte_cnt) with FILL_VALUE

    # Clear tile frame buffer 1
    fill_dest_ptr = VIEW_FRAME_BUF_1
    fill_byte_cnt = VIEW_FRAME_BUF_SIZE
    mem_fill_x(FILL_VALUE)

    # Both frame buffers now contain a uniform background pattern ready for
    # initial room decode and subsequent scroll operations.

*/