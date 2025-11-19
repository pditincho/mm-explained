/*================================================================================
view.asm  view rendering, scrolling, and per-column decode for TILE/COLOR/MASK

Exports:
  - render_room_view                 validate resources; scroll or redraw
  - clear_view_buffers               clear both view frame buffers

Internals:
  - scroll_view_left / scroll_view_right       layer-wise 1-column shifts
  - scroll_left_copy / scroll_right_copy       row copy cores (39/40 bytes)
  - decode_visible_window                      decode visible slice from snapshots
  - decode_room_gfx                            seed mode/dict; dispatch decode
  - decode_room_column / decode_room_all_columns	column/full-surface decoders

Global Inputs:
  - current_room, cam_target_pos, cam_current_pos
  - room_ptr_lo_tbl/room_ptr_hi_tbl, current_room_rsrc
  - room_gfx_layers_lo/hi, room_gfx_layers_active_ptr
  - frame_buffer_base, frame_buffer, global_lights_state
  - snapshot tables per layer:
      *_src_tbl_lo/hi, *_emit_rem_tbl, *_run_symbol_tbl
  - dicts per layer: tile_dict4, color_dict4, mask_dict4

Global Outputs:
  - screen_scroll_flag                $00=right, $FF=left, $01=initial redraw
  - decode_scope                      $00=one column, $01=full 40×17
  - tile_matrix_ptr / color_layer_ptr / mask_layer_ptr
  - view frame buffers (C828/CC28) and fixed COLOR/MASK surfaces

Description:
  - Orchestrates the room “view”:
    • Validates that cached room resources match the active room; rebuilds per-column
      decode snapshots if stale (addresses + packed mode/count + run symbol).
    • Chooses between three paths per frame:
      – Right scroll: shift tiles left; decode the newly revealed rightmost column.
      – Left scroll : shift tiles right; decode the newly revealed leftmost column.
      – Initial draw: decode the full 40×17 window.
    • Uses structure-of-arrays snapshots to resume the 4-symbol RLE-like decoder at
      an arbitrary column without reprocessing earlier data.
    • Performs layer order TILE → COLOR → MASK with per-layer dictionaries.

  - Scrolling details:
    • Left visual scroll (view moves right): copy 39 bytes/row right→left to avoid
      overlap (column 0 filled later by decode).
    • Right visual scroll (view moves left): copy 39 bytes/row left→right (column 39
      filled later).
    • Double-buffering for TILE layer when not in flashlight-only mode; COLOR/MASK use
      fixed layer bases.

Notes:
  - Snapshot layout is interleaved per list: all column bases for list#0, then list#1,
    etc.; each list holds a 16-bit base used to store column-indexed values.
  - The decoder’s “packed mode+count” uses bit7 for mode (run vs direct) and bits6..0
    for the remaining count, enabling exact resume semantics per column.
================================================================================*/

#importonce
#import "globals.inc"
#import "constants.inc"
#import "misc.asm"
#import "decompressor.asm"
#import "room_gfx_rsrc.asm"
#import "render_object.asm"

/*================================= Variables =================================*/

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

/*================================ Constants ==================================*/

.const FILL_VALUE               = $00    // mem_fill_x pattern used to clear framebuffers

.const COLS_PER_ROW             = VIEW_COLS  // copy width used by scroll cores (39 when VIEW_COLS=40)

.const COLOR_LAYER_COL_1        = COLOR_LAYER_COL_0 + 1  // COLOR layer column 1 base
.const MASK_LAYER_COL_1         = MASK_LAYER_COL_0 + 1  // MASK  layer column 1 base

.const DECOMPRESS_ONE_COL       = $00    // decode_scope: single-column decode (scroll updates)
.const DECOMPRESS_WHOLE_ROOM    = $01    // decode_scope: full 40×17 decode (initial draw)

.const VIEW_LAST_COL           = $27    // last visible column index (39); used for right-edge positioning

/*================================================================================
  render_room_view  validate resources; scroll right/left or fully redraw

  Arguments: (none)

  Returns:
    A, X, Y, flags      Clobbered by lookups, pointer math, and called routines.

  Global Inputs:
    current_room                       room index used to select resource bases
    room_ptr_lo_tbl/room_ptr_hi_tbl    expected room resource base for current_room
    current_room_rsrc                  cached room resource base to validate
    room_gfx_layers_lo/hi              published gfx-layers block pointer
    room_gfx_layers_active_ptr         “current” gfx-layers pointer to validate
    cam_target_pos, cam_current_pos    camera desired/current column for scroll test
    frame_buffer_base                  active framebuffer base for TILE layer
    viewport_left_col               world-space column of leftmost visible tile

  Global Outputs:
    screen_scroll_flag                 $00=right, $FF=left, $01=initial redraw
    tile_matrix_ptr                    TILE destination base (column 0 or 39)
    color_layer_ptr                    COLOR destination base (column 0 or 39)
    mask_layer_ptr                     MASK  destination base (column 0 or 39)
    decode_scope                       set to DECOMPRESS_WHOLE_ROOM on initial draw
    (memory)                           updated by scroll_view_* and decode_visible_window

  Description:
    - Verifies room data/layers pointers match the active room; if stale, refreshes
      cached bases and rebuilds per-column decode snapshots.
    - Determines action based on camera positions:
      • Right scroll when (target − 1) == current
      • Left  scroll when (target + 1) == current
      • Otherwise perform initial full redraw
    - For scroll:
      • Calls scroll_view_right/left to shift existing tiles
      • Sets per-layer destination pointers to the newly revealed edge (col 39 or 0)
      • Calls decode_visible_window starting at the appropriate column index
    - For initial draw:
      • Marks full-window decode, seeds per-layer destinations at column 0
      • Calls decode_visible_window for the entire visible window
================================================================================*/
* = $404D
render_room_view:
        /*------------------------------------------------------------
           Validate room data resources for the active room; rebuild if stale
        ------------------------------------------------------------*/
        ldx     current_room                  // select table slot for the active room

        lda     room_ptr_lo_tbl,x             // verify cached room resource base: low byte must match expected
        cmp     current_room_rsrc
        bne     refresh_room_resources_and_snapshots // mismatch → resource stale/missing → rebuild
        lda     room_ptr_hi_tbl,x             // verify cached room resource base: high byte must match expected
        cmp     current_room_rsrc + 1
        bne     refresh_room_resources_and_snapshots // mismatch → resource stale/missing → rebuild

        lda     room_gfx_layers_lo            // verify published gfx-layers pointer (low) matches active_ptr.low
        cmp     room_gfx_layers_active_ptr_lo
        bne     refresh_room_resources_and_snapshots // mismatch → layers resource not the current one → rebuild
        lda     room_gfx_layers_hi            // verify published gfx-layers pointer (high) matches active_ptr.high
        cmp     room_gfx_layers_active_ptr_hi
        bne     refresh_room_resources_and_snapshots // mismatch → layers resource not the current one → rebuild

		// Resources are resident, proceed to check scroll flag
        jmp     test_scroll_right

refresh_room_resources_and_snapshots:
        /*------------------------------------------------------------
           Update cached room resource base for current_room and rebuild
           per-column decode snapshots (src pointer, mode+count, run symbol)
        ------------------------------------------------------------*/
        lda     room_ptr_lo_tbl,x
        sta     current_room_rsrc
        lda     room_ptr_hi_tbl,x
        sta     current_room_rsrc + 1
        jsr     setup_room_columns_with_decode_snapshots

test_scroll_right:
        /*------------------------------------------------------------
           Scroll-right test: consider it a right scroll when
           (cam_target_pos - 1) == cam_current_pos
        ------------------------------------------------------------*/
        ldx     cam_target_pos                // X := desired column
        dex                                   // X := desired - 1
        cpx     cam_current_pos               // equal → need right scroll this frame
        bne     test_scroll_left             // not equal → test left scroll path

        /*------------------------------------------------------------
           Perform right scroll and prep decode of new rightmost column
        ------------------------------------------------------------*/
        lda     #SCROLL_RIGHT_FLAG                          // set scroll flag: right scroll this frame
        sta     screen_scroll_flag
        jsr     scroll_view_right            // shift layers left; reveal new rightmost column

        /*  tile_matrix_ptr := frame_buffer_base + 39 (target last visible column) */
        clc
        lda     frame_buffer_base            // base of tile layer column 0 in active FB
        adc     #<VIEW_LAST_COL                          // +39 columns → column 39 base (low)
        sta     tile_matrix_ptr
        lda     frame_buffer_base + 1
        adc     #>VIEW_LAST_COL
        sta     tile_matrix_ptr + 1

        /*  color_layer_ptr := COLOR_LAYER_COL_0 + 39 (last visible column) */
        clc
        lda     #<COLOR_LAYER_COL_0           // color layer column 0 base
        adc     #<VIEW_LAST_COL                 // +39 → column 39 base (low)
        sta     color_layer_ptr
        lda     #>COLOR_LAYER_COL_0
        adc     #>VIEW_LAST_COL
        sta     color_layer_ptr + 1

        /*  mask_layer_ptr := MASK_LAYER_COL_0 + 39 (last visible column) */
        clc
        lda     #<MASK_LAYER_COL_0            // mask layer column 0 base
        adc     #<VIEW_LAST_COL                          // +39 → column 39 base (low)
        sta     mask_layer_ptr
        lda     #>MASK_LAYER_COL_0
        adc     #>VIEW_LAST_COL
        sta     mask_layer_ptr + 1

        /*------------------------------------------------------------
           Decode visible columns starting at new right edge
        ------------------------------------------------------------*/
        lda     viewport_left_col          // compute index of new rightmost column: left_edge + 39
        clc
        adc     #VIEW_LAST_COL
        jmp     decode_visible_window    // decode TILE/COLOR/MASK starting at that column

test_scroll_left:
        /*------------------------------------------------------------
           Scroll-left test: consider it a left scroll when
           (cam_target_pos + 1) == cam_current_pos
        ------------------------------------------------------------*/
        ldx     cam_target_pos                // X := desired column
        inx                                   // X := desired + 1
        cpx     cam_current_pos               // equal → need left scroll this frame
        bne     initial_full_redraw                     // not equal → no scroll path

        /*------------------------------------------------------------
           Perform left scroll and prep decode of new leftmost column
        ------------------------------------------------------------*/
        lda     #SCROLL_LEFT_FLAG                          // set scroll flag: left scroll this frame
        sta     screen_scroll_flag
        lda     #$00                          // legacy/side flag (unchanged behavior)
        sta     $4f
        jsr     scroll_view_left             // shift layers right; reveal new leftmost column

        /*  tile_matrix_ptr := frame_buffer_base (leftmost column base) */
        lda     frame_buffer_base            // tile layer column 0 base (active FB)
        sta     tile_matrix_ptr
        lda     frame_buffer_base + 1
        sta     tile_matrix_ptr + 1

        /*  color_layer_ptr := COLOR_LAYER_COL_0 (leftmost color column base) */
        lda     #<COLOR_LAYER_COL_0           // color layer column 0 base
        sta     color_layer_ptr
        lda     #>COLOR_LAYER_COL_0
        sta     color_layer_ptr + 1

        /*  mask_layer_ptr := MASK_LAYER_COL_0 (leftmost mask column base) */
        lda     #<MASK_LAYER_COL_0            // mask layer column 0 base
        sta     mask_layer_ptr
        lda     #>MASK_LAYER_COL_0
        sta     mask_layer_ptr + 1

        /*------------------------------------------------------------
           Decode visible columns starting at new left edge
        ------------------------------------------------------------*/
        lda     viewport_left_col          // leftmost visible column index for decode
        jmp     decode_visible_window    // decode TILE/COLOR/MASK starting at that column

initial_full_redraw:
        /*------------------------------------------------------------
           Initial draw (no scroll): full-window decode
        ------------------------------------------------------------*/
        lda     #SCROLL_INIT_FLAG                          // mark frame action: initial draw (no scroll this frame)
        sta     screen_scroll_flag

        lda     #DECOMPRESS_WHOLE_ROOM        // decode scope := full 40×17 window (not single column)
        sta     decode_scope

        /*  tile_matrix_ptr := frame_buffer_base (start at leftmost tile column) */
        lda     frame_buffer_base            // tile layer column 0 base (active FB)
        sta     tile_matrix_ptr
        lda     frame_buffer_base + 1
        sta     tile_matrix_ptr + 1

        /*  color_layer_ptr := COLOR_LAYER_COL_0 (leftmost color column base) */
        lda     #<COLOR_LAYER_COL_0           // color layer column 0 base
        sta     color_layer_ptr
        lda     #>COLOR_LAYER_COL_0
        sta     color_layer_ptr + 1

        /*  mask_layer_ptr := MASK_LAYER_COL_0 (leftmost mask column base) */
        lda     #<MASK_LAYER_COL_0            // mask layer column 0 base
        sta     mask_layer_ptr
        lda     #>MASK_LAYER_COL_0
        sta     mask_layer_ptr + 1

        lda     viewport_left_col          // load leftmost visible column index for decode
                                              // fall through to decode_visible_window
/*================================================================================
decode_visible_window  decode TILE/COLOR/MASK layers for the visible slice

Arguments:
  A    						Leftmost visible column index (0..room_width-1); copied into Y.

Returns:
  A,X,Y,flags    			Clobbered by called routines. Y ends unspecified.

Global Inputs:
  tile_matrix_ptr, 
  color_layer_ptr, 
  mask_layer_ptr    		16-bit destinations (row-major) for each layer.
  
  tile_dict4, 
  color_dict4, 
  mask_dict4    			4-byte dictionaries (per-layer) for the 4-symbol decoder.
  
  tile_src_tbl_lo/hi, 
  color_src_tbl_lo/hi, 
  mask_src_tbl_lo/hi    	Per-column source pointers into each compressed stream (structure-of-arrays).
  
  tile_run_symbol_tbl, 
  color_run_symbol_tbl, 
  mask_run_symbol_tbl    	Per-column run symbols (one byte per column).
  
  tile_emit_rem_tbl, 
  color_emit_rem_tbl, 
  mask_emit_rem_tbl    		Per-column packed mode+count (bit7=mode; bits6..0=remaining).
  

Global Outputs:
  (memory)              	decoded 40×17 slice for each layer (initial draw).
  decode_scope    			Dispatch selector used by decompress_room_gfx (set here at the end).
  
State:
  dest_ptr  				set per layer to point at that layer’s buffer.
  decomp_src_ptr       		set per layer from the per-column snapshot.
  decomp_run_symbol     	set per layer from the per-column snapshot.
  dict4_base_ptr 			retargeted per layer to its 4-byte dict.
  
  Description:
    - Binds each layer’s destination buffer for output.
    - Selects the appropriate 4-symbol dictionary per layer (self-modified anchor).
    - Restores the per-column decoder state for the requested column:
      • Source pointer from *_src_tbl_{lo,hi}[Y]
      • Packed mode+count from *_emit_rem__tbl[Y]
      • Run symbol from *_run_symbol_tbl[Y]
    - Invokes the decoder, which seeds emit mode/counter and performs the decode
      according to the current decode_scope (full window vs single column).
    - Sets decode_scope to DECOMPRESS_ONE_COL so future scrolls only decode the
      newly revealed column.  
================================================================================*/
* = $4118
decode_visible_window:
        tay                                   // column index := leftmost visible (used to index per-column snapshots)

        /*------------------------------------------------------------
           TILE layer: bind destination buffer for decode output
        ------------------------------------------------------------*/
        lda     tile_matrix_ptr              // set dest_ptr := tile_matrix_ptr (lo)
        sta     dest_ptr
        lda     tile_matrix_ptr + 1              // ...and high byte
        sta     dest_ptr + 1

        lda     #<tile_dict4                   // point inline 4-byte dict to tile_dict4 (lo)
        sta     dict4_base_ptr
        lda     #>tile_dict4                   // ...and its high byte
        sta     dict4_base_ptr + 1

        lda     (tile_src_tbl_lo),y // load per-column src pointer (lo) from snapshot[Y]
        sta     decomp_src_ptr_lo
        lda     (tile_src_tbl_hi),y // ...src pointer (hi)
        sta     decomp_src_ptr_hi

        lda     (tile_run_symbol_tbl),y   // per-column run symbol (byte to emit in run mode)
        sta     decomp_run_symbol                   // seed decoder’s repeat value

        lda     (tile_emit_rem_tbl),y      // packed mode+count for column Y (bit7=mode, bits6..0=rem)
        jsr     decode_room_gfx                   // initialize emit_mode/emit_rem + dict and perform decode

        /*------------------------------------------------------------
           COLOR layer: bind destination buffer and select color dict
        ------------------------------------------------------------*/
        lda     color_layer_ptr              // dest_ptr := color_layer_ptr (lo)
        sta     dest_ptr
        lda     color_layer_ptr + 1              // ...high byte
        sta     dest_ptr + 1

        lda     #<color_dict4                  // retarget inline 4-byte dict → color_dict4 (lo)
        sta     dict4_base_ptr
        lda     #>color_dict4                  // ...high byte
        sta     dict4_base_ptr + 1

        lda     (color_src_tbl_lo),y   // load per-column src pointer (low) from snapshot[Y]
        sta     decomp_src_ptr_lo
        lda     (color_src_tbl_hi),y   // ...and high byte of src pointer
        sta     decomp_src_ptr_hi

        lda     (color_run_symbol_tbl),y   // per-column run symbol for COLOR (byte repeated in run mode)
        sta     decomp_run_symbol                   // seed decoder’s repeat value

        lda     (color_emit_rem_tbl),y      // packed mode+count for column Y (bit7=mode, bits6..0=rem)
        jsr     decode_room_gfx                   // init emit state + dict, then decode COLOR for this column

        /*------------------------------------------------------------
           MASK layer: bind destination buffer and select mask dict
        ------------------------------------------------------------*/
        lda     mask_layer_ptr              // dest_ptr := mask_layer_ptr (lo)
        sta     dest_ptr
        lda     mask_layer_ptr + 1              // ...high byte
        sta     dest_ptr + 1

        lda     #<mask_dict4                  // retarget inline 4-byte dict → mask_dict4 (lo)
        sta     dict4_base_ptr
        lda     #>mask_dict4                  // ...high byte
        sta     dict4_base_ptr + 1

        lda     (mask_src_tbl_lo),y   // per-column src pointer (low) from snapshot[Y]
        sta     decomp_src_ptr_lo
        lda     (mask_src_tbl_hi),y   // ...src pointer (high)
        sta     decomp_src_ptr_hi

        lda     (mask_run_symbol_tbl),y // per-column run symbol for MASK
        sta     decomp_run_symbol					// seed decoder’s repeat value

        lda     (mask_emit_rem_tbl),y      // packed mode+count for column Y (bit7=mode, bits6..0=rem)
        jsr     decode_room_gfx                  // seed emit_mode/emit_rem + dict, then decode MASK for this column

        /*------------------------------------------------------------
           From now on, decode only one column per update (for scrolling)
        ------------------------------------------------------------*/
        lda     #DECOMPRESS_ONE_COL           // switch decoder scope to “single column” for subsequent scroll updates
        sta     decode_scope     // from now on, decompress_room_gfx will take the per-column path

        /*------------------------------------------------------------
           Draw the dynamic objects onto the freshly decoded layers
        ------------------------------------------------------------*/
        jsr     render_room_objects
        rts
/*================================================================================
  decompress_room_gfx  init mode/counter, load 4-byte dict, dispatch decode

  Arguments:
    A      Packed header: bit7 = mode (1=run, 0=direct), bits6..0 = initial count.

  Global Inputs:
    decode_scope   	0 → decode one column; nonzero → decode full
    dict4_base_ptr	4-byte literal table adjacent to code

  Global Outputs:
    decomp_emit_mode            	$FF (run) or $00 (direct)
    decomp_emit_rem             	initial remaining count (7 or 8 bits as packed)
    decomp_dict4[0..3]          	symbol dictionary loaded from inlined table
    (control)                   	tail-calls column/full decode routine

  Returns:
    (no local return; control jumps to the selected decode routine)

  Description:
    - Interprets the packed header in A to seed emission mode and remaining count.
    - Copies the 4-byte dictionary into decomp_dict4 via an X=3..0 loop.
    - Branches to either single-column or full-view decode based on the region mode.
================================================================================*/
* = $418B
decode_room_gfx:
        /*------------------------------------------------------------
           Decode packed header in A → (decomp_emit_mode, decomp_emit_rem)
           - bit7=1 → run mode; count = A&$7F
           - bit7=0 → direct mode; count = A
        ------------------------------------------------------------*/
        bpl     set_direct_mode                   // N=0 → direct (bit7 clear)

        /*------------------------------ run-length mode -----------------------*/
        and     #$7f                          // keep 7-bit repeat count
        sta     decomp_emit_rem
        lda     #RUN_MODE                          // RUN_MODE
        jmp     commit_initial_mode

set_direct_mode:
        /*------------------------------ direct (literal) mode -----------------*/
        sta     decomp_emit_rem               // full 8-bit literal count
        lda     #DIRECT_MODE                          // DIRECT_MODE

commit_initial_mode:
        sta     decomp_emit_mode

        /*------------------------------------------------------------
           Load 4-byte symbol dictionary into decomp_dict4[0..3]
        ------------------------------------------------------------*/
        ldx     #COMP_DICT_MAX_OFS
load_dict4_loop:
        lda     $ffff,x                       // patched to point at inlined dict
        sta     decomp_dict4,x
        dex
        bpl     load_dict4_loop

        /*------------------------------------------------------------
           Dispatch: 0 = column decode, else full-surface decode
        ------------------------------------------------------------*/
        lda     decode_scope
        beq     decode_room_column
        jmp     decode_room_all_columns

/*================================================================================
  decode_room_column  emit one column (17 bytes) into row-major buffer

  Arguments:
    (none)

  Returns:
    A,X,Y,flags
      Clobbered; X/Y restored to entry values on exit.

  Global Inputs:
    dest_ptr                   16-bit base address of the column’s top cell

  Global Outputs:
    (memory)                   17 decoded bytes written at offsets
                                0, ROW_LENGTH, 2*ROW_LENGTH, ... down the column

  Description:
    - Saves X and Y, then decodes exactly 17 bytes (counter = $10 .. $00).
    - After each store, advances the vertical index by ROW_LENGTH; if this
      crosses a page, increments the high byte of dest_ptr to keep addressing
      aligned down the column.
    - Restores X/Y before returning.
================================================================================*/
* = $41AF
decode_room_column:
        /*------------------------------------------------------------
           Preserve caller’s X and Y
        ------------------------------------------------------------*/
        stx     view_x_saved
        sty     view_y_saved

        /*------------------------------------------------------------
           Initialize loop counter (17 iterations: $10..$00) and index
        ------------------------------------------------------------*/
        lda     #VIEW_ROWS - 1
        sta     rows_remaining
        ldy     #$00

decode_store_next:
        /*------------------------------------------------------------
           Decode next byte and store it at current column position
        ------------------------------------------------------------*/
        jsr     decomp_stream_next            // A := next decoded byte
        sta     (dest_ptr),y

        /*------------------------------------------------------------
           Advance vertical index by row length; fix page cross if any
        ------------------------------------------------------------*/
        tya
        clc
        adc     #VIEW_COLS
        tay
        bcc     dec_rows
        inc     dest_ptr+1                    // carry crossed page → bump high byte

dec_rows:
        /*------------------------------------------------------------
           Decrement remaining count; loop for 17 total stores
        ------------------------------------------------------------*/
        dec     rows_remaining
        bpl     decode_store_next

        /*------------------------------------------------------------
           Restore registers and return
        ------------------------------------------------------------*/
        ldx     view_x_saved
        ldy     view_y_saved
        rts
/*================================================================================
  decode_room_all_columns  decode 40 columns × 17 rows into row-major surface

  Arguments:
    (none)

  Global Inputs:
    dest_ptr                  16-bit base of column 0 (top-left cell)

  Global Outputs:
    (memory)                  full 40×17 image written, column by column
    dest_ptr                  advanced to the base of column 1 (start + 1)

  Returns:
    A,X,Y,flags                clobbered; Y restored to entry value

  Description:
    - Seeds column_ptr := dest_ptr.
    - Repeats 40 times (Y=$27..$00):
      • Calls decompress_room_gfx_column to write 17 bytes down the current column.
      • Increments column_ptr by 1 (next column to the right).
      • Updates dest_ptr := column_ptr for the next column decode.
================================================================================*/
* = $41D4
decode_room_all_columns:
        /*------------------------------------------------------------
           Preserve caller’s Y and seed column_ptr := dest_ptr
        ------------------------------------------------------------*/
        sty     view_y_saved_2
        lda     dest_ptr
        sta     column_ptr
        lda     dest_ptr + 1
        sta     column_ptr + 1

        /*------------------------------------------------------------
           Decode columns: Y := $27 .. $00  (40 columns total)
        ------------------------------------------------------------*/
        ldy     #VIEW_COLS - 1
decode_col_loop:
        jsr     decode_room_column    // writes 17 rows down from dest_ptr

        /*------------------------------------------------------------
           column_ptr := column_ptr + 1  (advance to next column base)
        ------------------------------------------------------------*/
        inc     column_ptr
        bne     set_col_base_next
        inc     column_ptr + 1
set_col_base_next:
        /*------------------------------------------------------------
           dest_ptr := column_ptr  (set base for next column decode)
        ------------------------------------------------------------*/
        lda     column_ptr
        sta     dest_ptr
        lda     column_ptr + 1
        sta     dest_ptr + 1

        /*------------------------------------------------------------
           Next column until Y becomes -1 (BPL falls through when done)
        ------------------------------------------------------------*/
        dey
        bpl     decode_col_loop

        /*------------------------------------------------------------
           Restore Y and return
        ------------------------------------------------------------*/
        ldy     view_y_saved_2
        rts
/*================================================================================
  scroll_view_left  shift view RIGHT (that is, scroll LEFT) by one column across tile/color/mask

  Arguments:
    (none)

  Global Inputs:
    frame_buffer                 selects active frame buffer; determines “other” source
    frame_buffer_base            16-bit base address of the active frame buffer (dest)
    global_lights_state                1 → flashlight-only (forces in-place copy)

  Global Outputs:
    scroll_src                   16-bit source pointer (set per pass: tile/color/mask)
    scroll_dest                  16-bit destination pointer (active fb or fixed layer base)
    (memory)                     copies 39 bytes × 17 rows for tile/color/mask: columns
                                  1.. → 0.. (rightward visual scroll)

  Returns:
    A,X,Y,flags                  clobbered by subroutines and pointer math

  Description:
    - Tile layer:
      • If double-buffering: choose the “other” framebuffer as scroll_src; set scroll_dest
        to frame_buffer_base. If flashlight-only: scroll_src := scroll_dest (in-place).
      • Start writing at column 1 by incrementing scroll_dest; call scroll_left_copy to
        copy columns 1.. → 0.. (39 bytes/row, right→left) safely with overlap.
    - Color layer:
      • scroll_src := COLOR_LAYER_COL_0, scroll_dest := COLOR_LAYER_COL_1; call
        scroll_left_copy to shift columns 1.. → 0...
    - Mask layer:
      • scroll_src := MASK_LAYER_COL_0, scroll_dest := MASK_LAYER_COL_1; call
        scroll_left_copy similarly.
    - Copy order (right→left) prevents self-overwrite when shifting the image left in
      memory; the visual effect is a rightward scroll (new column appears on the right).

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
================================================================================*/
* = $41FE
scroll_view_left:
        /*------------------------------------------------------------
           Choose tile layer source based on active frame buffer
           - If frame_buffer == 1: scroll_src := VIEW_FRAME_BUF_0 (other)
           - Else:                  scroll_src := VIEW_FRAME_BUF_1 (other)
           - Destination will be set to active frame buffer base
        ------------------------------------------------------------*/
        lda     frame_buffer
        cmp     #$01
        bne     frame_buffer_2

        /*------------------------------------------------------------
           Select the other frame buffer as source (active is FB1)
           - Load VIEW_FRAME_BUF_0 into scroll_src (lo/hi)
           - Destination will be set to the active frame buffer next
        ------------------------------------------------------------*/
        lda     #<VIEW_FRAME_BUF_0
        sta     scroll_src
        lda     #>VIEW_FRAME_BUF_0
        sta     scroll_src + 1
        jmp     lscroll_set_dest_to_active_fb

        /*------------------------------------------------------------
           Select the other frame buffer as source (active is FB0)
           - Load VIEW_FRAME_BUF_1 into scroll_src (lo/hi)
        ------------------------------------------------------------*/
frame_buffer_2:
        lda     #<VIEW_FRAME_BUF_1
        sta     scroll_src
        lda     #>VIEW_FRAME_BUF_1
        sta     scroll_src + 1

lscroll_set_dest_to_active_fb:
        /*------------------------------------------------------------
           Set destination to the active frame buffer base
        ------------------------------------------------------------*/
        lda     frame_buffer_base
        sta     scroll_dest
        lda     frame_buffer_base + 1
        sta     scroll_dest + 1

        /*------------------------------------------------------------
           If flashlight-only, switch to in-place copy (scroll_src := scroll_dest)
        ------------------------------------------------------------*/
        lda     global_lights_state
        cmp     #$01
        bne     move_one_column_left
        lda     frame_buffer_base
        sta     scroll_src
        lda     frame_buffer_base + 1
        sta     scroll_src + 1

move_one_column_left:
        /*------------------------------------------------------------
           Start writing at column #1: scroll_dest := scroll_dest + 1
           - Copy will run right→left (39 bytes) into columns 0..38
        ------------------------------------------------------------*/
        inc     scroll_dest
        bne     lscroll_copy_tiles_block
        inc     scroll_dest + 1

lscroll_copy_tiles_block:
        /*------------------------------------------------------------
           Copy TILE layer: columns 1.. → 0.. (39 bytes × 17 rows)
           - scroll_left_copy reads/writes right→left to avoid overlap issues
        ------------------------------------------------------------*/
        jsr     scroll_left_copy

        /*------------------------------------------------------------
           Copy COLOR layer: columns 1.. → 0..
           - scroll_src := COLOR_LAYER_COL_0 (base), scroll_dest := COLOR_LAYER_COL_1 (base+1)
           - We incremented scroll_dest in TILE path; for fixed layers we set both explicitly
        ------------------------------------------------------------*/
        lda     #<COLOR_LAYER_COL_0           // scroll_src at column 0 base
        sta     scroll_src
        lda     #>COLOR_LAYER_COL_0
        sta     scroll_src + 1
        lda     #<COLOR_LAYER_COL_1           // scroll_dest at column 1 base
        sta     scroll_dest
        lda     #>COLOR_LAYER_COL_1
        sta     scroll_dest + 1
        jsr     scroll_left_copy

        /*------------------------------------------------------------
           Copy MASK layer: columns 1.. → 0..
        ------------------------------------------------------------*/
        lda     #<MASK_LAYER_COL_0            // scroll_src at column 0 base
        sta     scroll_src
        lda     #>MASK_LAYER_COL_0
        sta     scroll_src + 1
        lda     #<MASK_LAYER_COL_1            // scroll_dest at column 1 base
        sta     scroll_dest
        lda     #>MASK_LAYER_COL_1
        sta     scroll_dest + 1
        jsr     scroll_left_copy

        rts
/*================================================================================
  scroll_view_right  shift view LEFT (that is, scroll RIGHT) by one column across tile/color/mask layers

  Arguments:
    (none)

  Global Inputs:
    frame_buffer             selects active frame buffer (1 → CC28, else C828)
    frame_buffer_base        16-bit base of the active frame buffer (scroll_dest)
    global_lights_state            1 → flashlight-only mode (forces in-place copy)

  Global Outputs:
    scroll_src                   16-bit source pointer set per pass (tile/color/mask)
    scroll_dest                     16-bit destination pointer set per pass (active fb)

  Returns:
    A,X,Y,flags              clobbered

  Description:
    - Tile layer:
        • If double-buffering: scroll_src := other frame buffer; scroll_dest := active.
        • If flashlight-only: scroll_src := scroll_dest (in-place).
        • scroll_src += 1 so we copy columns 1..39 → 0..38 via scroll_right_copy.
    - Color layer: copy 6D8A.. → 6D89.. (columns 1.. → 0..).
    - Mask  layer: copy 6AE2.. → 6AE1.. (columns 1.. → 0..).
    - scroll_right_copy copies 40 bytes × 17 rows, left→right; safe for
      overlapping case where scroll_src = scroll_dest + 1.
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
 
================================================================================*/
* = $425E
scroll_view_right:
        /*------------------------------------------------------------
           Choose tile layer source based on active frame buffer
           - If frame_buffer == 1: scroll_src := C828 (other); scroll_dest := active
           - Else:                  scroll_src := CC28 (other); scroll_dest := active
        ------------------------------------------------------------*/
        lda     frame_buffer
        cmp     #$01
        bne     pick_other_fb_when_fb0

        /*------------------------------------------------------------
           Select the other frame buffer as source (active is FB1)
           - Load VIEW_FRAME_BUF_0 into scroll_src (lo/hi)
           - Destination will be set to the active frame buffer next
        ------------------------------------------------------------*/
        lda     #<VIEW_FRAME_BUF_0
        sta     scroll_src
        lda     #>VIEW_FRAME_BUF_0
        sta     scroll_src + 1
        jmp     set_dest_to_active_fb

        /*------------------------------------------------------------
           Select the other frame buffer as source (active is FB0)
           - Load VIEW_FRAME_BUF_1 into scroll_src (lo/hi)
        ------------------------------------------------------------*/
pick_other_fb_when_fb0:
        lda     #<VIEW_FRAME_BUF_1
        sta     scroll_src
        lda     #>VIEW_FRAME_BUF_1
        sta     scroll_src + 1

set_dest_to_active_fb:
        /*------------------------------------------------------------
           Set destination to the active frame buffer base
           - Copy frame_buffer_base (lo/hi) into scroll_dest
        ------------------------------------------------------------*/
        lda     frame_buffer_base            // dest.lo := active FB low
        sta     scroll_dest
        lda     frame_buffer_base + 1            // dest.hi := active FB high
        sta     scroll_dest + 1

        /*------------------------------------------------------------
           If flashlight-only, switch to in-place copy (src := dest)
           - When global_lights_state == 1, read and write the same frame buffer
           - Otherwise, proceed to offset source to column #1
        ------------------------------------------------------------*/
        lda     global_lights_state                 // read lighting mode
        cmp     #$01                          // 1 → flashlight-only
        bne     offset_src_to_col1            // not flashlight-only → keep double-buffer source

        lda     frame_buffer_base            // src.lo := active FB low
        sta     scroll_src
        lda     frame_buffer_base + 1            // src.hi := active FB high
        sta     scroll_src + 1

offset_src_to_col1:
        /*------------------------------------------------------------
           Shift by one column: start reading at column #1 (scroll_src += 1)
        ------------------------------------------------------------*/
        inc     scroll_src
        bne     copy_tiles_block
        inc     scroll_src + 1

copy_tiles_block:
        /*------------------------------------------------------------
           Copy tiles layer: columns 1..39 → 0..38 (40×17 block)
        ------------------------------------------------------------*/
        jsr     scroll_right_copy

        /*------------------------------------------------------------
           Copy COLOR layer: columns 1.. → 0.. using scroll_right_copy
           - Set scroll_src to COLOR_LAYER_COL_1 (column #1 start)
           - Set scroll_dest to COLOR_LAYER_COL_0 (column 0 start)
           - Copy 40×17 block left→right (safe with src = dest+1)
        ------------------------------------------------------------*/
        lda     #<COLOR_LAYER_COL_1           // src.lo := color layer col #1 low
        sta     scroll_src
        lda     #>COLOR_LAYER_COL_1           // src.hi := color layer col #1 high
        sta     scroll_src + 1
        lda     #<COLOR_LAYER_COL_0           // dest.lo := color layer col #0 low
        sta     scroll_dest
        lda     #>COLOR_LAYER_COL_0           // dest.hi := color layer col #0 high
        sta     scroll_dest + 1
        jsr     scroll_right_copy             // perform 40×17 left-shift copy

        /*------------------------------------------------------------
           Copy MASK layer: columns 1.. → 0.. using scroll_right_copy
           - Set scroll_src to MASK_LAYER_COL_1 (column #1 start)
           - Set scroll_dest to MASK_LAYER_COL_0 (column 0 start)
           - Copy 40×17 block left→right (safe with src = dest+1)
        ------------------------------------------------------------*/
        lda     #<MASK_LAYER_COL_1            // src.lo := mask layer col #1 low
        sta     scroll_src
        lda     #>MASK_LAYER_COL_1            // src.hi := mask layer col #1 high
        sta     scroll_src + 1
        lda     #<MASK_LAYER_COL_0            // dest.lo := mask layer col #0 low
        sta     scroll_dest
        lda     #>MASK_LAYER_COL_0            // dest.hi := mask layer col #0 high
        sta     scroll_dest + 1
        jsr     scroll_right_copy             // perform 40×17 left-shift copy

        rts
/*================================================================================
scroll_left_copy  copy 39 bytes per row right→left for 17 rows (scroll-left core)

  Arguments:
    (none)

  Global Inputs:
    scroll_src   base address of the top-left source row (lo/hi)
    scroll_dest     base address of the top-left destination row (lo/hi)

  Returns:
    A,X,Y,flags   clobbered

  Description:
    - For each of 17 rows (X = 16..0), copy bytes from column 38 down to 0
      (39 bytes total), using (ptr),Y addressing.
    - After each row, advance both scroll_src and scroll_dest by 40 bytes (row stride).
    - Right→left traversal avoids self-overwrite when shifting the image left
      in-place (scroll_dest = scroll_src - 1).
================================================================================*/
* = $42BE
scroll_left_copy:
        /*------------------------------------------------------------
           Initialize row counter (X := 16 .. 0 → 17 rows total)
        ------------------------------------------------------------*/
        ldx     #VIEW_ROWS - 1

left_copy_next_row:
        /*------------------------------------------------------------
           Copy one row right→left: Y := 38 .. 0 (39 bytes)
        ------------------------------------------------------------*/
        ldy     #VIEW_COLS - 2
left_copy_next_col:
        lda     (scroll_src),y
        sta     (scroll_dest),y
        dey
        bpl     left_copy_next_col

        /*------------------------------------------------------------
           Advance source pointer to next row (scroll_src += 40)
        ------------------------------------------------------------*/
        clc
        lda     scroll_src
        adc     #<COLS_PER_ROW
        sta     scroll_src
        lda     scroll_src + 1
        adc     #>COLS_PER_ROW
        sta     scroll_src + 1

        /*------------------------------------------------------------
           Advance destination pointer to next row (scroll_dest += 40)
        ------------------------------------------------------------*/
        clc
        lda     scroll_dest
        adc     #<COLS_PER_ROW
        sta     scroll_dest
        lda     scroll_dest + 1
        adc     #>COLS_PER_ROW
        sta     scroll_dest + 1

        /*------------------------------------------------------------
           Next row until X becomes -1 (BPL falls through when done)
        ------------------------------------------------------------*/
        dex
        bpl     left_copy_next_row
        rts
/*================================================================================
scroll_right_copy  copy a 40×17 rectangle from scroll_src to scroll_dest, row by row

  Arguments:
    (none)

  Global Inputs:
    scroll_src      	16-bit base address of the top-left source row.
    scroll_dest        16-bit base address of the top-left destination row.

  Returns:
    A,X,Y,flags
      Clobbered.

  Description:
    - For each of 17 rows (X = 16..0), copy 40 bytes left→right using (ptr),Y.
    - After each row, add 40 (COLS_PER_ROW) to both scroll_src and scroll_dest (16-bit add).
    - Assumes non-overlapping regions or separate buffers; forward copy will
      corrupt data for in-place rightward scrolls if src/dest overlap.
================================================================================*/
/*================================================================================
Why copying 40 bytes with src = dest + 1 causes an out-of-bounds read
- For a rightward visual scroll, callers set scroll_src = scroll_dest + 1 so that
  copying columns (1..39) into (0..38) shifts the row left by one column.
- If the inner loop copies 40 bytes (Y = 0..39), the last read is:
      read = *(scroll_src + 39) = *(scroll_dest + 1 + 39) = *(scroll_dest + 40)
  which is one byte past the current 40-byte row (out of bounds), even though the
  destination’s final column will be overwritten later by new content.
- Consequence: OOB read can fetch unrelated memory (even though it's later overwritten)
================================================================================*/
* = $42E7
scroll_right_copy:
        /*------------------------------------------------------------
           Initialize row counter (X := 16 .. 0 → 17 rows total)
        ------------------------------------------------------------*/
        ldx     #VIEW_ROWS - 1

right_copy_next_row:
        /*------------------------------------------------------------
           Copy one row: Y := 0 .. 39, byte-by-byte left → right
        ------------------------------------------------------------*/
        ldy     #$00
right_copy_next_col:
        lda     (scroll_src),y
        sta     (scroll_dest),y
        iny
        cpy     #COLS_PER_ROW
        bne     right_copy_next_col

        /*------------------------------------------------------------
           Advance scroll_src pointer to next row (scroll_src += 40)
        ------------------------------------------------------------*/
        clc
        lda     scroll_src
        adc     #<COLS_PER_ROW
        sta     scroll_src
        lda     scroll_src + 1
        adc     #>COLS_PER_ROW
        sta     scroll_src + 1

        /*------------------------------------------------------------
           Advance destination pointer to next row (scroll_dest += 40)
        ------------------------------------------------------------*/
        clc
        lda     scroll_dest
        adc     #<COLS_PER_ROW
        sta     scroll_dest
        lda     scroll_dest + 1
        adc     #>COLS_PER_ROW
        sta     scroll_dest + 1

        /*------------------------------------------------------------
           Next row until X becomes -1 (BPL falls through when done)
        ------------------------------------------------------------*/
        dex
        bpl     right_copy_next_row
        rts
/*================================================================================
clear_view_buffers  clear both view frame buffers using mem_fill_x

  Arguments:
    (none)

  Returns:
    A,X,Y,flags               clobbered according to mem_fill_x; buffers filled

  Description:
    - Clears VIEW_FRAME_BUF_0 and VIEW_FRAME_BUF_1 by calling mem_fill_x with
      dest := buffer base, count := VIEW_FRAME_BUF_SIZE, X := FILL_VALUE.
    - mem_fill_x writes X for 'count' bytes starting at 'dest', advancing the
      pointer and handling page crossings internally.
================================================================================*/
* = $5F4B
clear_view_buffers:
        /*------------------------------------------------------------
           Clear VIEW_FRAME_BUF_0 with FILL_VALUE for VIEW_FRAME_BUF_SIZE bytes
        ------------------------------------------------------------*/
        lda     #<VIEW_FRAME_BUF_0           // dest.lo := base low
        sta     fill_dest_ptr
        lda     #>VIEW_FRAME_BUF_0           // dest.hi := base high
        sta     fill_dest_ptr + 1
        lda     #<VIEW_FRAME_BUF_SIZE        // count.lo := size low
        sta     fill_byte_cnt
        lda     #>VIEW_FRAME_BUF_SIZE        // count.hi := size high
        sta     fill_byte_cnt + 1
        ldx     #FILL_VALUE                   // X := fill byte
        jsr     mem_fill_x                    // fill [dest, dest+size)

        /*------------------------------------------------------------
           Clear VIEW_FRAME_BUF_1 with FILL_VALUE for VIEW_FRAME_BUF_SIZE bytes
        ------------------------------------------------------------*/
        lda     #<VIEW_FRAME_BUF_1           // dest.lo := base low
        sta     fill_dest_ptr
        lda     #>VIEW_FRAME_BUF_1           // dest.hi := base high
        sta     fill_dest_ptr + 1
        lda     #<VIEW_FRAME_BUF_SIZE        // count.lo := size low
        sta     fill_byte_cnt
        lda     #>VIEW_FRAME_BUF_SIZE        // count.hi := size high
        sta     fill_byte_cnt + 1 
        ldx     #FILL_VALUE                   // X := fill byte
        jsr     mem_fill_x                    // fill [dest, dest+size)
        rts



