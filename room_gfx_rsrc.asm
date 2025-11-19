/*
================================================================================
  Room gfx-layer
================================================================================

This module builds and maintains a helper resource that lets the renderer jump to
the start of any column in the room’s compressed TILE/COLOR/MASK streams in O(1)
time, without re-decoding all prior columns.

Core idea
  - Each vertical column of the room is produced by decoding a fixed number of
    bytes from three independent compressed streams:
      * TILE layer    → which tile to draw
      * COLOR layer   → how that tile is colored
      * MASK layer    → masking and priority bits
  - Instead of decoding from the beginning each time, we precompute the exact
    decoder state at the start of every column and store it in a compact,
    structure-of-arrays resource.

What the resource contains
  - A 24-byte directory (column_decode_snapshot_bases): 12 entries, each a
    16-bit base address for one “pointer list” (lo/hi stored interleaved).
  - 12 pointer lists, each of length room_width bytes, laid out back-to-back:
      * For each layer (TILE, COLOR, MASK), four lists that represent the decompressor state:
          1) src_lo     (low byte of source pointer)
          2) src_hi     (high byte of source pointer)
          3) emit_rem   (packed mode+remaining-count)
          4) run_symbol (last run symbol, if in run mode)
      * Lists are stored in structure-of-arrays form:
          list[i][col] = byte i for column “col” (0..room_width-1).
  - A mirrored copy of the directory (mirrored_bases_lo/hi) used by
    assert_pointer_lists_match as a fail-fast guard against corruption.

Key routines
  - setup_room_columns_with_decode_snapshots
      * Optionally frees a previous gfx-layers resource.
      * Computes the allocation size (room_width×12 + MEM_HDR_LEN) using
        shift-and-add (no hardware multiply).
      * Allocates a block, writes a standard resource header, and publishes the
        handle as “room gfx layers”.
      * Lays out the 12 per-column pointer lists and records their 16-bit bases
        into column_decode_snapshot_bases and mirrored_bases_*.
      * For each of TILE, COLOR, MASK:
          - Sets decomp_src_ptr to the start of that layer in room_data_ptr.
          - Binds the appropriate src_lo/src_hi/emit_rem/run_symbol lists into
            col_* list pointers.
          - Calls snapshot_column_decoder_state to fill all per-column entries.

  - snapshot_column_decoder_state
      * Runs a dictionary init for the 4-symbol decompressor so the source
        pointer is positioned at the payload bytes.
      * For each column:
          - Saves decomp_src_ptr into src_lo/src_hi lists at index “col”.
          - Packs the current emit mode and remaining count into a single byte
            (bit7=mode, bits6..0=count) and stores it into emit_rem list.
          - Stores decomp_run_symbol into the run_symbol list.
          - Calls decomp_stream_next exactly 17 times to advance the decoder to
            the next column boundary, without writing any pixels here.

  - assert_pointer_lists_match
      * Compares the canonical directory and mirror byte-by-byte (CMP_LEN
        entries).
      * On mismatch, sets a debug flag, maps I/O in, and enters an infinite loop
        writing the VIC border color as a visible “red screen” style halt.

How it is used by the renderer
  - To render or scroll, the engine can:
      * Choose a target column “c”.
      * For each layer, read 24 bytes of state by sampling one byte from each
        list at offset “c” (using the directory to find list bases).
      * Restore the decompressor state from those snapshots, then decode only
        the visible column instead of replaying the entire stream from the
        beginning.
  - This enables smooth scrolling and wide rooms without pre-rendering the
    entire background into RAM, trading a small amount of metadata for a large
    reduction in decode work per frame.
	
================================================================================
 
Memory layout for precomputed per-column pointer bytes
------------------------------------------------------

For a single column, the engine conceptually needs 24 bytes (12 pointers × 2 bytes). 
Listed in logical order for one column:
  0: tile.src_lo.ptr_lo         1: tile.src_lo.ptr_hi
  2: tile.src_hi.ptr_lo         3: tile.src_hi.ptr_hi
  4: tile.mode_cnt.ptr_lo       5: tile.mode_cnt.ptr_hi
  6: tile.run_sym.ptr_lo        7: tile.run_sym.ptr_hi
  8: color.src_lo.ptr_lo        9: color.src_lo.ptr_hi
 10: color.src_hi.ptr_lo       11: color.src_hi.ptr_hi
 12: color.mode_cnt.ptr_lo     13: color.mode_cnt.ptr_hi
 14: color.run_sym.ptr_lo      15: color.run_sym.ptr_hi
 16: mask.src_lo.ptr_lo        17: mask.src_lo.ptr_hi
 18: mask.src_hi.ptr_lo        19: mask.src_hi.ptr_hi
 20: mask.mode_cnt.ptr_lo      21: mask.mode_cnt.ptr_hi
 22: mask.run_sym.ptr_lo       23: mask.run_sym.ptr_hi

However, these 24 bytes are NOT stored contiguously per column. Instead, memory
is organized as 24 separate lists (streams), one list per logical byte above.
Each list holds one byte per column, laid out consecutively in column order.

High-level layout (structure-of-arrays across columns)
------------------------------------------------------

  list[ 0]  = tile.src_lo.ptr_lo:  B[0], B[1], ..., B[room_width-1]
  list[ 1]  = tile.src_lo.ptr_hi:  B[0], B[1], ..., B[room_width-1]
  list[ 2]  = tile.src_hi.ptr_lo:  B[0], B[1], ..., B[room_width-1]
  list[ 3]  = tile.src_hi.ptr_hi:  B[0], B[1], ..., B[room_width-1]
  list[ 4]  = tile.mode_cnt.lo:    B[0], B[1], ..., B[room_width-1]
  list[ 5]  = tile.mode_cnt.hi:    B[0], B[1], ..., B[room_width-1]
  list[ 6]  = tile.run_sym.lo:     B[0], B[1], ..., B[room_width-1]
  list[ 7]  = tile.run_sym.hi:     B[0], B[1], ..., B[room_width-1]

  list[ 8]  = color.src_lo.ptr_lo: B[0], B[1], ..., B[room_width-1]
  list[ 9]  = color.src_lo.ptr_hi: B[0], B[1], ..., B[room_width-1]
  list[10]  = color.src_hi.ptr_lo: B[0], B[1], ..., B[room_width-1]
  list[11]  = color.src_hi.ptr_hi: B[0], B[1], ..., B[room_width-1]
  list[12]  = color.mode_cnt.lo:   B[0], B[1], ..., B[room_width-1]
  list[13]  = color.mode_cnt.hi:   B[0], B[1], ..., B[room_width-1]
  list[14]  = color.run_sym.lo:    B[0], B[1], ..., B[room_width-1]
  list[15]  = color.run_sym.hi:    B[0], B[1], ..., B[room_width-1]

  list[16]  = mask.src_lo.ptr_lo:  B[0], B[1], ..., B[room_width-1]
  list[17]  = mask.src_lo.ptr_hi:  B[0], B[1], ..., B[room_width-1]
  list[18]  = mask.src_hi.ptr_lo:  B[0], B[1], ..., B[room_width-1]
  list[19]  = mask.src_hi.ptr_hi:  B[0], B[1], ..., B[room_width-1]
  list[20]  = mask.mode_cnt.lo:    B[0], B[1], ..., B[room_width-1]
  list[21]  = mask.mode_cnt.hi:    B[0], B[1], ..., B[room_width-1]
  list[22]  = mask.run_sym.lo:     B[0], B[1], ..., B[room_width-1]
  list[23]  = mask.run_sym.hi:     B[0], B[1], ..., B[room_width-1]

Where B[i] is the byte for column i for that list.

Directory (24 bytes of 12×16-bit bases)
---------------------------------------

  list_base_dir[0..23] holds the base address for each list above (two bytes per
  entry, lo then hi). The i-th list’s byte for column c is at:
      addr = list_base_dir[i] + c

Visualization (columns across, lists down)
------------------------------------------

         col0  col1  col2  ...  colN-1
  L0:    B0    B1    B2         B(N-1)
  L1:    B0    B1    B2         B(N-1)
  ...
  L23:   B0    B1    B2         B(N-1)

To reconstruct the 24 bytes for a given column c, read one byte from each list:
  for i in 0..23: out[i] = *(list_base_dir[i] + c)
================================================================================
*/
#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "decompressor.asm"
#import "memory_mgmt.asm"

.label rsrc_total_bytes_local_lo      = $15     // low byte of total size = room_width×12 + MEM_HDR_LEN (aliases word_lo)
.label rsrc_total_bytes_local_hi      = $16     // high byte of total size = room_width×12 + MEM_HDR_LEN (aliases word_hi)
.label word_lo                        = $15     // generic 16-bit scratch low byte (shared with width_x* and total-size temps)
.label word_hi                        = $16     // generic 16-bit scratch high byte (shared with width_x* and total-size temps)
.label width_x12_lo                   = $15     // low byte of width×12 payload size (aliases word_lo)
.label width_x12_hi                   = $16     // high byte of width×12 payload size (aliases word_hi)
.label width_x8_lo                    = $15     // low byte of width×8 product (aliases word_lo)
.label width_x8_hi                    = $16     // high byte of width×8 product (aliases word_hi)
.label room_width_16bit_lo            = $15     // room_width as 16-bit value: low byte (aliases word_lo)
.label room_width_16bit_hi            = $16     // room_width as 16-bit value: high byte (aliases word_hi)
.label width_x4_lo                    = $17     // low byte of width×4 product (aliases col_src_lo_list low)
.label width_x4_hi                    = $18     // high byte of width×4 product (aliases rsrc_write_ptr_hi)
.label rsrc_write_ptr_lo              = $17     // low byte of write cursor inside allocated block (steps by room_width)
.label rsrc_write_ptr_hi              = $18     // high byte of write cursor inside allocated block (steps by room_width)

.label col_src_lo_list                = $17     // ZP 16-bit base for per-column src_lo lists (#0/#8/#16)
.label col_src_hi_list                = $19     // ZP 16-bit base for per-column src_hi lists (#1/#9/#17)
.label col_emit_rem_list              = $1B     // ZP 16-bit base for per-column packed mode+count lists (#2/#10/#18)
.label col_run_symbol_list            = $1D     // ZP 16-bit base for per-column run-symbol lists (#3/#11/#19)

.label alloc_base_lo                  = $4F     // low byte of heap block base returned by mem_alloc for this resource
.label alloc_base_hi                  = $50     // high byte of heap block base returned by mem_alloc for this resource
.label column_decode_snapshot_bases   = $64     // canonical 24-byte directory of 12×16-bit list bases (lo/hi interleaved)

.label room_gfx_x_saved               = $41F9   // scratch byte used to preserve X across decomp_stream_next loop
.label room_data_ptr_lo               = $44B9   // low byte of base pointer to active room’s compressed data
.label room_data_ptr_hi               = $44BA   // high byte of base pointer to active room’s compressed data
.label mirrored_bases_lo              = $44BB   // low-byte mirror of 12×16-bit list-base directory for sanity checks
.label mirrored_bases_hi              = $44BC   // high-byte mirror of 12×16-bit list-base directory for sanity checks

.const CMP_LEN                        = $18     // number of bytes compared between canonical and mirror (24 decimal)
.const BORDER_WHITE                   = $01     // VIC border color value written on fatal pointer-table mismatch
.const ROW_MAX_INDEX                  = VIEW_ROWS - 1   // highest valid row index in the visible viewport

/*
================================================================================
  setup_room_columns_with_decode_snapshots
================================================================================
Summary
  Allocate and publish a “room gfx layers” resource, lay out per-column pointer
  lists, and precompute decoder entry snapshots for TILE, COLOR, and MASK
  layers to enable O(1) random access to any column.

Global Inputs
  room_width                           Number of columns in the room (bytes per
                                       list).
  current_room                         Index into room_ptr_lo_tbl/room_ptr_hi_tbl.
  room_ptr_lo/hi_tbl				   Per-room base pointers.
  tile_matrix_ofs_lo/hi                Offset of TILE layer payload from room base.
  color_layer_ofs_lo/hi                Offset of COLOR layer payload from room base.
  mask_layer_ofs_lo/hi                 Offset of MASK layer payload from room base.
  room_gfx_layers_lo/hi                Previously published gfx-layers resource
                                       (if any).
Global Outputs
  room_gfx_layers_lo/hi                Published handle of the gfx-layers resource.
  room_gfx_layers_active_ptr_lo/hi     Cached handle for “still current?” checks.
  column_decode_snapshot_bases         Filled with 12×16-bit list bases for
                                       TILE/COLOR/MASK metadata.
  mirrored_bases_lo/hi                 Filled with the same bases for
                                       assert_pointer_lists_match.

Description
  - If a previous room gfx-layers resource is active (room_gfx_layers_hi != 0),
    free it via mem_release.
  - Compute width×12 from room_width using shift-and-add (width×4, then ×8,
    then sum), add MEM_HDR_LEN to obtain the allocation size, and call
    mem_alloc to reserve a contiguous block.
  - Initialize and write the standard 4-byte resource header (type/index for
    room gfx layers) at alloc_base via rsrc_hdr_init, then publish the new
    handle in room_gfx_layers_lo/hi and mirror it in
    room_gfx_layers_active_ptr_lo/hi.
  - Initialize rsrc_write_ptr to the start of the block, advance past the
    header, and then lay out 12 lists back-to-back, each of length room_width,
    recording their 16-bit bases into column_decode_snapshot_bases and
    mirrored_bases.
  - Resolve room_data_ptr from the current_room tables and, for each of
    TILE, COLOR, and MASK:
      • Set decomp_src_ptr := room_data_ptr + layer_offset.
      • Bind src_lo/src_hi/mode+count/run_symbol list bases from the directory
        into col_src_lo_list, col_src_hi_list, col_emit_rem_list, and
        col_run_symbol_list.
      • Call snapshot_column_decoder_state to record per-column entry state and
        advance the decompressor by one column’s worth of decoded data.
		
================================================================================

Why rsrc_write_ptr is incremented by room_width each iteration
--------------------------------------------------------------

- Each pointer list holds 1 byte per column, so the list size is room_width * 1 bytes.
- After recording the current list’s base, the next list must begin exactly
  room_width bytes later to pack lists contiguously without gaps or overlap.
- The 16-bit add (low then high with carry) correctly handles page crossings.
- Repeating this yields 12 back-to-back regions: per layer, four lists
  (src_lo, src_hi, mode+count, run_symbol), each length room_width.
  
  
How to compute x12 product
--------------------------

- Express 12 as (8 + 4). Compute width×8 and width×4 separately, then add them.

- Shifts make powers of two: ASL multiplies the low byte by 2; any overflow goes
  into the carry flag. ROL on a high byte rotates that carry in, forming a 16-bit
  product. Two ASL/ROL pairs yield ×4; doubling that 16-bit value yields ×8.
  
- Algorithm:
  1) Clear the high accumulator, load A := width.
  2) ASL; ROL high   → width×2 (16-bit)
  3) ASL; ROL high   → width×4 (16-bit). Save as the ×4 partial.
  4) ASL low; ROL high on that 16-bit value → width×8 (16-bit).
  5) CLC; ADC low(×8)+low(×4) → low(×12); then ADC high(×8)+high(×4)+carry → high.
  
- Result: exact 16-bit width×12 using only shifts (ASL/ROL) and additions (ADC),
  with carry propagation handling page crossings automatically.
  
================================================================================
*/
* = $4312
setup_room_columns_with_decode_snapshots:
        // ------------------------------------------------------------
        // Release previously published gfx-layers block if present
        // ------------------------------------------------------------
        ldx     room_gfx_layers_lo            // prepare address (lo) of current layers block
        ldy     room_gfx_layers_hi            // prepare address (hi); Y==0 means “no block”
        beq     calculate_rsrc_size           // no active block → skip free and size it
        jsr     mem_release                   // free previous layers block (X=lo, Y=hi)

calculate_rsrc_size:
        // ----------------------------------------
        // Compute width × 4 (but store it in width_x8 temporarily)
        // ----------------------------------------
        lda     #$00                          
        sta     width_x8_hi

        lda     room_width                    
        asl                 		// form width × 2 (carry feeds next ROL)
        rol     width_x8_hi         // propagate carry into high byte
        asl                         // width × 4
        rol     width_x8_hi         // accumulate high byte of (width × 4)
        sta     width_x8_lo         // low byte of width × 4

        // ----------------------------------------
        // Copy width_x8 into width_x4
        // ----------------------------------------
		sta     width_x4_lo                     
        lda     width_x8_hi
        sta     width_x4_hi                     

        // ----------------------------------------
        // Double width_x4 to get width_x8
        // ----------------------------------------
		asl     width_x8_lo			// double ×4 → ×8 (low)
        rol     width_x8_hi         // and its high via carry

        // ----------------------------------------
        // Add ×8 and ×4 to produce ×12
        // ----------------------------------------
        clc                                   
        lda     width_x8_lo                     
        adc     width_x4_lo                     
        sta     width_x12_lo                    
		
        lda     width_x8_hi                     
        adc     width_x4_hi                     
        sta     width_x12_hi                    
		
        // ----------------------------------------
        // Add header length to payload size -> rsrc_total_bytes_local
        // ----------------------------------------
        clc                                   
        lda     width_x12_lo                  
        adc     #MEM_HDR_LEN                  
        sta     rsrc_total_bytes_local_lo       
		
        lda     width_x12_hi                  
        adc     #$00                          
        sta     rsrc_total_bytes_local_hi    

        // ----------------------------------------
        // Allocate heap block for resource
		//
        // - mem_alloc expects block size in X:lo, Y:hi
        // - mem_alloc returns base address in X:lo, Y:hi
        // ----------------------------------------
        ldx     rsrc_total_bytes_local_lo       // allocation size (low)
        ldy     rsrc_total_bytes_local_hi		// allocation size (high)
        jsr     mem_alloc                     	// allocate; returns base in X:lo, Y:hi

        // ------------------------------------------------------------
        // Stash allocated block base for later
        // ------------------------------------------------------------
        stx     alloc_base_lo
        sty     alloc_base_hi

        // ------------------------------------------------------------
        // Initialize resource header
        // ------------------------------------------------------------
        lda     #RSRC_TYPE_ROOM_LAYERS        	// resource type := room layers
        sta     rsrc_resource_type
        lda     #RSRC_INDEX_GFX_LAYERS        	// resource subtype (index) := gfx layer slot
        sta     rsrc_resource_index
        jsr     rsrc_hdr_init                  	// write header at alloc_base

        // ------------------------------------------------------------
        // Publish newly allocated block as the active gfx layers resource
        // ------------------------------------------------------------
        ldx     alloc_base_lo                    
        ldy     alloc_base_hi                    
        stx     room_gfx_layers_lo            
        sty     room_gfx_layers_hi            
        stx     room_gfx_layers_active_ptr_lo    
        sty     room_gfx_layers_active_ptr_hi    

        // ------------------------------------------------------------
        // Initialize rsrc_write_ptr to the start of the allocated block
        // ------------------------------------------------------------
        stx     rsrc_write_ptr_lo                
        sty     rsrc_write_ptr_hi                

        // ------------------------------------------------------------
        // Advance write cursor past the 4-byte resource header
        // ------------------------------------------------------------
        clc                                   
        lda     rsrc_write_ptr_lo             
        adc     #<MEM_HDR_LEN                 
        sta     rsrc_write_ptr_lo             
        lda     rsrc_write_ptr_hi             
        adc     #>MEM_HDR_LEN                 
        sta     rsrc_write_ptr_hi             

        // ------------------------------------------------------------
        // Convert room_width (8-bit value) to a 16-bit value for address arithmetic
        // ------------------------------------------------------------
        lda     room_width                    
        sta     room_width_16bit_lo             
        lda     #$00                         
        sta     room_width_16bit_hi         

        // ------------------------------------------------------------
        // Record 16-bit base address for each pointer list
		//
        // Write the address to both column_decode_snapshot_bases and
        //   to mirrored_bases for a sanity check mirror
        // ------------------------------------------------------------
        // Y iterates over byte pairs (0,2,...): list index = Y >> 1
        ldy     #$00                          
compute_pointer_list_start:
        lda     rsrc_write_ptr_lo               	// current list base: low byte
        sta.a   column_decode_snapshot_bases,y  	// canonical table low byte at offset Y
        sta     mirrored_bases_lo,y           		// mirror low byte
		
        lda     rsrc_write_ptr_hi               	// current list base: high byte
        sta.a   column_decode_snapshot_bases + 1,y 	// canonical table high byte at offset Y+1
        sta     mirrored_bases_hi,y         		// mirror high byte

        // ------------------------------------------------------------
        // Advance list write pointer by room_width (16-bit add)
        // ------------------------------------------------------------
        clc                                   
        lda     rsrc_write_ptr_lo               
        adc     room_width_16bit_lo             
        sta     rsrc_write_ptr_lo               
		
        lda     rsrc_write_ptr_hi               
        adc     room_width_16bit_hi             
        sta     rsrc_write_ptr_hi               

        // ------------------------------------------------------------
        // Step to next pointer-list pair
        // ------------------------------------------------------------
        iny                                   
        iny                                   
		
        // ------------------------------------------------------------
		// Loop over all 12 lists (24 elements, offset $18)
        // ------------------------------------------------------------
        cpy     #$18                          
        bne     compute_pointer_list_start    // more lists pending → continue

        // ------------------------------------------------------------
        // Resolve base pointer for the current room resource
		//
        // X := current_room; index into room_ptr_{lo,hi}_tbl
        // Store the selected room base into room_data_ptr (lo/hi)
        // ------------------------------------------------------------
        ldx     current_room                  
        lda     room_ptr_lo_tbl,x             
        sta     room_data_ptr_lo                
        lda     room_ptr_hi_tbl,x             
        sta     room_data_ptr_hi                

        // ------------------------------------------------------------
        // Set up TILE layer source decompression pointer = room_data_ptr + tile_matrix_ofs
		//
        // Result placed in decomp_src_ptr (decoder will read from there)
        // ------------------------------------------------------------
        clc                                   
        lda     room_data_ptr_lo                
        adc     tile_matrix_ofs_lo              
        sta     decomp_src_ptr_lo               
		
        lda     room_data_ptr_hi                
        adc     tile_matrix_ofs_hi              
        sta     decomp_src_ptr_hi               

        // ------------------------------------------------------------
        // Bind TILE layer pointer lists from the directory
		//
        // Map precomputed entries #0..#7 to list bases, each of them lo and hi bytes.
		//
        //   	src_lo
		//		src_hi
		//		mode+count
		//		run_symbol
        // ------------------------------------------------------------
        lda     column_decode_snapshot_bases+0    // src_lo list base (low)
        sta     col_src_lo_list
        lda     column_decode_snapshot_bases+1    // src_lo list base (high)
        sta     col_src_lo_list + 1

        lda     column_decode_snapshot_bases+2    // src_hi list base (low)
        sta     col_src_hi_list
        lda     column_decode_snapshot_bases+3    // src_hi list base (high)
        sta     col_src_hi_list + 1

        lda     column_decode_snapshot_bases+4    // mode+count list base (low)
        sta     col_emit_rem_list
        lda     column_decode_snapshot_bases+5    // mode+count list base (high)
        sta     col_emit_rem_list + 1

        lda     column_decode_snapshot_bases+6    // run_symbol list base (low)
        sta     col_run_symbol_list
        lda     column_decode_snapshot_bases+7    // run_symbol list base (high)
        sta     col_run_symbol_list + 1

        // ------------------------------------------------------------
        // Snapshot per-column decoder state
		//
        // - Records src_lo/src_hi, packed mode+count, and run_symbol per column
        // - Advances the decoder by 17 bytes per column to the next boundary
        // ------------------------------------------------------------
        jsr     snapshot_column_decoder_state  

        // ---------------------------------------
        // Repeat for the COLOR Layer
        // --------------------------------------
        clc
        lda     room_data_ptr_lo
        adc     color_layer_ofs_lo
        sta     decomp_src_ptr_lo              
		
        lda     room_data_ptr_hi
        adc     color_layer_ofs_hi
        sta     decomp_src_ptr_hi              

        lda     column_decode_snapshot_bases+8
        sta     col_src_lo_list       
        lda     column_decode_snapshot_bases+9
        sta     col_src_lo_list + 1
        lda     column_decode_snapshot_bases+10
        sta     col_src_hi_list       
        lda     column_decode_snapshot_bases+11
        sta     col_src_hi_list + 1
        lda     column_decode_snapshot_bases+12
        sta     col_emit_rem_list             
        lda     column_decode_snapshot_bases+13
        sta     col_emit_rem_list + 1
        lda     column_decode_snapshot_bases+14
        sta     col_run_symbol_list         
        lda     column_decode_snapshot_bases+15
        sta     col_run_symbol_list + 1

        jsr     snapshot_column_decoder_state

        // ---------------------------------------
        // Repeat for the MASK Layer
        // --------------------------------------
        clc
        lda     room_data_ptr_lo
        adc     mask_layer_ofs_lo
        sta     decomp_src_ptr_lo              
        lda     room_data_ptr_hi
        adc     mask_layer_ofs_hi
        sta     decomp_src_ptr_hi              

        lda     column_decode_snapshot_bases+16
        sta     col_src_lo_list       
        lda     column_decode_snapshot_bases+17
        sta     col_src_lo_list + 1
        lda     column_decode_snapshot_bases+18
        sta     col_src_hi_list       
        lda     column_decode_snapshot_bases+19
        sta     col_src_hi_list + 1
        lda     column_decode_snapshot_bases+20
        sta     col_emit_rem_list             
        lda     column_decode_snapshot_bases+21
        sta     col_emit_rem_list + 1
        lda     column_decode_snapshot_bases+22
        sta     col_run_symbol_list         
        lda     column_decode_snapshot_bases+23
        sta     col_run_symbol_list + 1

        jsr     snapshot_column_decoder_state

        rts
/*
================================================================================
  snapshot_column_decoder_state
================================================================================

Summary
  Precompute and store per-column decoder entry state for the current layer,
  then advance the decompressor to the next column boundary.

Global Inputs
  decomp_src_ptr              Current decoder source pointer (lo/hi).
  decomp_emit_mode            Current emission mode flag byte; sign bit mirrors
                              literal vs. run mode.
  decomp_emit_rem             Remaining count for the current literal or run.
  decomp_run_symbol           Byte to repeat when in run mode.
  room_width                  Number of columns in the current room.

Global Outputs
  col_src_lo_list             Per-column table of source pointer low bytes.
  col_src_hi_list             Per-column table of source pointer high bytes.
  col_emit_rem_list           Per-column packed mode+count bytes.
  col_run_symbol_list         Per-column run symbol bytes.
  decomp_src_ptr              Advanced by exactly 17 decoded bytes per column.

Description
  - Run a fail-fast sanity check (assert_pointer_lists_match) to verify that the
    canonical and mirrored pointer-list directories are identical.
  - Initialize the 4-symbol dictionary (decomp_dict4_init) so the decoder is
    positioned at the compressed payload for the current layer.
  - For each column y in [0, room_width):
      • Snapshot the current decomp_src_ptr into the src_lo and src_hi lists.
      • Encode the emission mode and remaining count into col_emit_rem_list[y]
        by packing the count in bits 6..0 and the mode in bit 7
        (1 = literal, 0 = run).
      • Store decomp_run_symbol into col_run_symbol_list[y] for potential run
        mode resumption.
      • Consume exactly 17 decoded bytes via decomp_stream_next calls to advance
        the decoder state to the next column boundary without writing pixels.
  - The resulting tables provide O(1) resume points for any column by restoring
    the pointer, packed mode+count, and run symbol, avoiding re-decoding
    preceding columns.

================================================================================

 Snapshot decompressor state for a column

 When we jump into the middle of a compressed stream (start of an arbitrary
 column), we must fully reconstruct the decoder state without consuming any
 bytes from prior columns. There are three pieces of state we need per column
 snapshot:

   1) Stream position:      decomp_src_ptr  (we save the pointer for column y)
   2) Emit mode + remaining count: literal-vs-run and its 7-bit counter
   3) Run symbol:           the byte to repeat if in run mode

 The live decoder indicates the mode via a flag (N=1 → literal, N=0 → run). We
 do not persist CPU flags, so we encode the mode into the top bit of the saved
 counter byte:

   bit7 = 1  → literal (direct) mode
   bit7 = 0  → run mode
   bits6..0  → remaining count for that mode

 This "packed" byte lets the resume code reestablish the exact mode without
 having to re-read the stream. If we didn’t set bit7 when starting in literal
 mode, a remaining count of N would be ambiguous with "run for N" - the next
 decomp_stream_next would then produce the wrong output and drift the stream
 state. By forcing bit7 appropriately, the first resumed decode step behaves
 identically to continuing from the previous column: it emits the correct type
 (literal vs run), uses the correct remaining count, and decrements
 consistently.

 Note: The 4-byte dictionary has already been consumed globally via
 decomp_dict4_init before taking snapshots, so the per-column resume only needs
 (src_ptr, packed_mode+count, run_symbol) to be exact.

 Source of truth (from the active decompressor):
   decomp_emit_mode  : N flag mirrors mode (N=1 literal, N=0 run)
   decomp_emit_rem   : 7-bit remaining count
   decomp_run_symbol : byte to repeat if in run mode
	  
================================================================================
*/
* = $4463
snapshot_column_decoder_state:
		// Sanity check: confirm scratch tables/mirrors are consistent
        jsr     assert_pointer_lists_match    
		
        // ------------------------------------------------------------
        // Init 4-symbol decompression decoder
		// Source pointer now points at payload
        // ------------------------------------------------------------
		jsr     decomp_dict4_init         

        // Y = column index (0 .. room_width-1)
		ldy     #$00                      
snapshot_this_column:
        // ------------------------------------------------------------
        // Capture decoder source at the start of this column’s payload
        // (little-endian pointer split across two parallel lists)
        // ------------------------------------------------------------
        lda     decomp_src_ptr_lo
        sta     (col_src_lo_list),y       	// lo byte
        lda     decomp_src_ptr_hi
        sta     (col_src_hi_list),y       	// hi byte
		 
        // ------------------------------------------------------------
		//	Check emission mode: literal or run?
        // ------------------------------------------------------------
        lda     decomp_emit_mode           	// N=1 → literal mode, N=0 → run mode
        bpl     handle_run_mode            

        // ------------------------------------------------------------
        // Literal (direct) mode: set the mode flag (bit7=1) and keep the low 7 bits as the count
        // ------------------------------------------------------------
        lda     #$80
        ora     decomp_emit_rem            	// bit7:=1 | count(6..0)
        jmp     commit_mode_counter        	// unconditional (result is never zero)

handle_run_mode:
        // ------------------------------------------------------------
        // Run mode: store the count as-is (bit7 remains 0 → run)
        // ------------------------------------------------------------
        lda     decomp_emit_rem

commit_mode_counter:
        // ------------------------------------------------------------
		// Save packed mode/count (emit_rem) and run symbol for this column
        // ------------------------------------------------------------
        sta     (col_emit_rem_list),y      	
        lda     decomp_run_symbol
        sta     (col_run_symbol_list),y    	

        // ------------------------------------------------------------
        // Consume all column bytes
        // ------------------------------------------------------------
        ldx     #ROW_MAX_INDEX            	// X = 16 .. 0 → total 17 iterations					
consume_column_bytes:
        stx     room_gfx_x_saved          	// save X
        jsr     decomp_stream_next        	// emit 1 byte; advances decomp_src_ptr & internal state
        ldx     room_gfx_x_saved          	// restore X
        dex                               	// next row in this column
        bpl     consume_column_bytes      	// keep going until X becomes -1

        // Move on to the next column; stop after the last column (Y == room_width).
        iny                               	
        cpy     room_width                	
        bne     snapshot_this_column      	

        rts
/*
================================================================================
  assert_pointer_lists_match
================================================================================
Summary
  Compare canonical and mirrored 24-byte pointer-base directories; halt on
  mismatch with a visible border-color hang.

Global Inputs
  column_decode_snapshot_bases  Canonical 24-byte directory (2×12 entries).
  mirrored_bases                Mirror copy of the same 24 bytes for sanity
                                checking.
Returns
  On success: returns
  On failure: No return; infinite loop writing the border color register.

Description
  - Walk the canonical and mirrored pointer-base tables byte-by-byte for
    CMP_LEN entries, using Y as the index.
  - On the first unequal pair:
    • Store a non-zero error code into debug_error_code.
    • Map I/O into the CPU address space so VIC registers are visible.
    • Enter a tight loop that continuously writes the border color register,
      producing a hard visual halt for debugging.
  - If all CMP_LEN bytes match, return normally; subsequent code can safely
    perform bulk writes to the pointer lists knowing their bases are aligned.
	
================================================================================

 Why this sanity check runs before decompression

 - It verifies the canonical and mirrored 24-byte pointer-base tables still match;
   any drift implies wrong destinations for subsequent writes.
 - The precompute/decompression step emits many bytes per column across multiple
   lists; bad bases would spray data into unrelated memory and corrupt state.
   
================================================================================
*/
* = $449B
assert_pointer_lists_match:
        // ----------------------------------------
        // Initialize scan index (Y := 0) to begin byte-by-byte compare
        // ----------------------------------------
        ldy     #$00                          

compare_next:
        // ----------------------------------------
        // Compare corresponding bytes of the two tables at index Y
        // ----------------------------------------
        lda     column_decode_snapshot_bases,y  // A := column_decode_snapshot_bases[Y]
        cmp     mirrored_bases_lo,y           	// compare against mirrored_bases[Y]
        beq     advance_or_return               // equal → continue with next index; else mismatch path

        // ----------------------------------------
        // Mismatch encountered:
        // - record an error code (1) for diagnostics
        // - set border color and halt
        // ----------------------------------------
        lda     #$01                          	// prepare error code and border color value 
        sta     debug_error_code              	
        ldy     #MAP_IO_IN                      
        sty     cpu_port       				  	
mismatch_halt_loop:
        sta     vic_border_color_reg              
        jmp     mismatch_halt_loop          	// infinite loop 

advance_or_return:
        // ----------------------------------------
        // Advance to next byte; terminate when Y == CMP_LEN
        // ----------------------------------------
        iny                                   
        cpy     #CMP_LEN                      
        bne     compare_next                
        rts                                 

/*
Pseudo-code

function setup_room_columns_with_decode_snapshots():
    //----------------------------------------------------------------------
    // 1) Free any previously published gfx-layers resource
    //----------------------------------------------------------------------
    if room_gfx_layers_handle is not null:
        mem_release(room_gfx_layers_handle)

    //----------------------------------------------------------------------
    // 2) Compute total size for the new resource
    //    - payload: 12 lists × room_width bytes each
    //    - plus resource header
    //----------------------------------------------------------------------
    width = room_width                     // integer
    payload_size = width * 12              // 12 lists × width bytes
    total_size = payload_size + MEM_HDR_LEN

    //----------------------------------------------------------------------
    // 3) Allocate new resource block and initialize header
    //----------------------------------------------------------------------
    alloc_base = mem_alloc(total_size)     // returns a pointer/handle

    rsrc_resource_type  = RSRC_TYPE_ROOM_LAYERS
    rsrc_resource_index = RSRC_INDEX_GFX_LAYERS
    rsrc_hdr_init(alloc_base)              // writes 4-byte header at alloc_base

    //----------------------------------------------------------------------
    // 4) Publish handles and initialize write pointer
    //----------------------------------------------------------------------
    room_gfx_layers_handle       = alloc_base
    room_gfx_layers_active_ptr   = alloc_base

    write_ptr = alloc_base + MEM_HDR_LEN   // start of pointer-list payload

    //----------------------------------------------------------------------
    // 5) Lay out 12 pointer lists back-to-back, build directory + mirror
    //    - Each list_i: width bytes, contiguous
    //    - Directory stores list_i base addresses
    //----------------------------------------------------------------------
    for i from 0 to 11:
        list_base = write_ptr

        column_decode_snapshot_bases[i] = list_base
        mirrored_bases[i]               = list_base

        write_ptr = write_ptr + width   // advance to next list region

    //----------------------------------------------------------------------
    // 6) Resolve room resource base pointer for the active room
    //----------------------------------------------------------------------
    room_data_ptr = room_ptr_table[current_room]  // (lo/hi combined)

    //----------------------------------------------------------------------
    // 7) For each layer, bind source pointer and list bases,
    //    then snapshot per-column decoder state
    //----------------------------------------------------------------------
    // --- TILE layer ---
    decomp_src_ptr = room_data_ptr + tile_matrix_ofs

    col_src_lo_list   = column_decode_snapshot_bases[ 0]
    col_src_hi_list   = column_decode_snapshot_bases[ 1]
    col_emit_rem_list = column_decode_snapshot_bases[ 2]   // conceptually “mode+count”
    col_run_symbol_list = column_decode_snapshot_bases[ 3]

    snapshot_column_decoder_state()

    // --- COLOR layer ---
    decomp_src_ptr = room_data_ptr + color_layer_ofs

    col_src_lo_list   = column_decode_snapshot_bases[ 4]
    col_src_hi_list   = column_decode_snapshot_bases[ 5]
    col_emit_rem_list = column_decode_snapshot_bases[ 6]
    col_run_symbol_list = column_decode_snapshot_bases[ 7]

    snapshot_column_decoder_state()

    // --- MASK layer ---
    decomp_src_ptr = room_data_ptr + mask_layer_ofs

    col_src_lo_list   = column_decode_snapshot_bases[ 8]
    col_src_hi_list   = column_decode_snapshot_bases[ 9]
    col_emit_rem_list = column_decode_snapshot_bases[10]
    col_run_symbol_list = column_decode_snapshot_bases[11]

    snapshot_column_decoder_state()

    //----------------------------------------------------------------------
    // 8) Return to caller
    //----------------------------------------------------------------------
    return


function snapshot_column_decoder_state():
    //----------------------------------------------------------------------
    // Preconditions:
    //  - decomp_src_ptr is set to the start of the layer’s compressed stream
    //  - col_src_lo_list, col_src_hi_list, col_emit_rem_list, col_run_symbol_list
    //    are bound to the correct list bases for this layer
    //----------------------------------------------------------------------

    //----------------------------------------------------------------------
    // 1) Sanity check pointer-list directories
    //----------------------------------------------------------------------
    assert_pointer_lists_match()   // aborts on failure, returns on success

    //----------------------------------------------------------------------
    // 2) Initialize dictionary-based decompressor for this stream
    //----------------------------------------------------------------------
    decomp_dict4_init()            // consumes dictionary header; src now at payload

    //----------------------------------------------------------------------
    // 3) For each column, snapshot state and advance decoder one column
    //----------------------------------------------------------------------
    for col from 0 to room_width - 1:
        // 3a) Snapshot source pointer
        col_src_lo_list[col] = low_byte_of(decomp_src_ptr)
        col_src_hi_list[col] = high_byte_of(decomp_src_ptr)

        // 3b) Snapshot emission mode + remaining count
        //     - literal vs. run mode
        //     - remaining run/literal length
        if decompressor_is_in_literal_mode():
            mode_bit = 1
        else:
            mode_bit = 0

        count = decomp_emit_rem   // remaining length from the decompressor
        packed_mode_and_count = pack_mode_and_count(mode_bit, count)
        col_emit_rem_list[col] = packed_mode_and_count

        // 3c) Snapshot run symbol (meaningful if in run mode)
        col_run_symbol_list[col] = decomp_run_symbol

        // 3d) Consume exactly one column’s worth of decoded values (17)
        for k from 1 to COLUMN_HEIGHT:    // COLUMN_HEIGHT = 17
            decomp_stream_next()          // decode and discard one output byte

    return


function assert_pointer_lists_match():
    //----------------------------------------------------------------------
    // Compare the canonical and mirrored base-address directories.
    // On mismatch: set a debug flag, map I/O, and spin forever changing
    // the border color as a visual halt indicator.
    //----------------------------------------------------------------------

    length = CMP_LEN    // number of bytes (or entries) to compare

    for i from 0 to length - 1:
        a = column_decode_snapshot_bases_raw[i]   // raw byte/entry from canonical
        b = mirrored_bases_raw[i]                // raw byte/entry from mirror

        if a != b:
            debug_error_code = 1
            map_io_in()                          // ensure border register is visible

            while true:
                vic_border_color_reg = BORDER_WHITE
            // never returns

    // If we get here, all entries match
    return
*/