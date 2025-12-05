/*
==============================================================================
 Blit and mask cel

 This routine draws one cel (a small rectangular image fragment)
 into a target sprite buffer. It supports optional horizontal flipping.

 Each frame is assembled by compositing several cels over a base sprite.
 This additive system avoids redundancy: only the parts that change between
 frames are stored as separate cels, and static parts are reused.

 Example:
   A speaking character reuses the same head cel and overlays only the mouth:
       1. head cel (without mouth)
       2. mouth-open cel
       3. mouth-closed cel
   This saves space because small mouth cels replace entire head duplicates.

 Rendering model:
   Every visible element is produced by blitting its pixels with transparency
   into the composite sprite buffer. Multiple cels can overlap and merge to
   form a complete pose or animation frame.

 Multicolor sprite format (C64):
   - Size: 24×21 pixels → 12×21 in multicolor mode (2 bits per pixel)
   - Memory: 63 bytes per sprite
   - Each row = 12 pixels = 3 bytes:
       byte 0 → pixels 0–3
       byte 1 → pixels 4–7
       byte 2 → pixels 8–11
   - Rows are stored sequentially (top to bottom).

 Partial-width optimization:
   Some cels occupy only 1/3 or 2/3 of a sprite’s width:
       1 byte → 4 pixels (1/3 width)
       2 bytes → 8 pixels (2/3 width)
       3 bytes → 12 pixels (full width)
   This allows efficient storage of narrow limbs or small details.

 During blitting:
   - The cel’s header defines width (bytes_per_row), height, and offsets.
   - Width determines which specialized copy/mask routines will be used.
   - These routines are selected dynamically through dispatch tables.
   - Three families of entry points exist:
       • Copy family (copy_row_3/2/1) - reads bytes from cel stream
       • Flip family (flip_row_3/2/1) - horizontally mirrors source bytes
       • Mask family (apply_mask_3/2/1) - merges source and destination pixels
   - Each family uses multiple entry points to skip unneeded code paths,
     optimizing for small widths and minimizing per-byte branching.

 ------------------------------------------------------------------------------
 Cel data layout (header and pixel data)

  Offset   Meaning
  ======   ================================================
   #00     Bytes per row
   #01     Vertical size (rows)
   #02     Horizontal offset (byte groups)
   #03     Vertical offset (from bottom of sprite)
   #04     Unused (reserved)
   #05     Inter-cel vertical offset (applies to next cel)
   <pixel bytes for row 0>
   ...
   <pixel bytes for row n>

 Field descriptions:
   Bytes per row:
       Number of data bytes per scanline. Range #00–#03.
       Each byte represents 4 pixels in multicolor mode.
       #00 means the cel contains no pixel data (empty region).

   Vertical size:
       Height of the cel in rows (number of scanlines to draw).

   Horizontal offset:
       Byte-based horizontal shift (0–2) to align partial-width cels.
       This tells the renderer how many 4-pixel groups to skip before
       plotting the cel’s pixels, ensuring proper horizontal placement
       within a 12-pixel-wide sprite row.

   Vertical offset:
       Number of rows to raise the cel relative to the sprite’s bottom
       reference row. This positions each body part correctly. For example,
       a head cel has a large positive offset, while feet have none.

   Inter-cel vertical offset:
       Optional Y adjustment applied to the *next* cel drawn.
       Used in a few animation sequences to fine-tune stacking.

 After the 6-byte header, the pixel data follows immediately, with each row
 storing “bytes per row” bytes of packed 2-bit pixels.

 ------------------------------------------------------------------------------

 Pixel data layout and row interpretation

  Depending on the “bytes per row” field, each row stores a variable number
  of bytes representing 4, 8, or 12 pixels in multicolor format:

    3 bytes per row:
        Row 0 → Byte0, Byte1, Byte2
        Row 1 → Byte0, Byte1, Byte2
        ...

    2 bytes per row:
        Row 0 → Byte0, Byte1
        Row 1 → Byte0, Byte1
        ...

    1 byte per row:
        Row 0 → Byte0
        Row 1 → Byte0
        ...

  The total row width in bytes (1–3) determines how many columns of pixels
  this cel covers. The horizontal_offset field defines *where* those bytes
  are positioned within the 3-byte (12-pixel) sprite row.

  When bytes_per_row = 3:
      The renderer simply copies all 3 bytes directly.

  When bytes_per_row = 2:
      Two possible placements exist:

		 Destination bytes before copy: D0 D1 D2
		 
        Case A - shifted right (horizontal_offset = 1)
            Source bytes:       -- S0 S1		(-- is the offset)
            Destination bytes:  D0 S0 S1

        Case B - aligned left (horizontal_offset = 0)
            Source bytes:       S0 S1			(no offset)	
            Destination bytes:  S0 S1 D2

  When bytes_per_row = 1:
      A similar logic applies: horizontal_offset chooses which of the
      three destination bytes receives the single source byte.

  In summary:
      horizontal_offset (0–2) specifies the target byte position within
      the 3-byte sprite row where the cel’s pixel data begins.

 ------------------------------------------------------------------------------

 Horizontal flipping

  To conserve memory, mirrored (left-facing) cels are not stored separately.
  Instead, they are generated dynamically by *flipping* the right-facing
  version at render time.

  Mechanism:
    - Each pixel byte encodes 4 multicolor pixels (2 bits per pixel).
    - There are 256 possible byte values (00–FF).
    - A precomputed lookup table (flipped_patterns[256]) stores the horizontally
      reversed equivalent of every possible byte.
    - During flipped rendering, each input byte is replaced by its flipped
      counterpart using this table.

  The renderer thus maintains two blit paths:
    • Normal copy - uses source bytes directly.
    • Flipped copy - replaces each source byte with flipped_patterns[value].

 ------------------------------------------------------------------------------

 Masking logic
 
  Once the row’s pixel bytes are prepared (either copied or flipped),
  the result is *masked* onto the existing sprite buffer. This process is
  analogous to traditional cel animation compositing:

      final_byte = (sprite_byte AND mask_pattern) OR blit_byte

  The AND preserves portions of the background sprite, while OR overlays
  the new pixels from the cel, creating a layered effect.

  Code structure:

    - The renderer contains *families* of routines for each width variant:
         • copy_row_xxx     - straight copy (1, 2, or 3 bytes)
         • flip_row_xxx     - mirrored copy via lookup
         • apply_mask_xxx   - masking/merge into destination

    - Each family has multiple *entry points* corresponding to different
      “bytes per row” sizes:
         • 3-byte variant → full width
         • 2-byte variant → mid width (reuses tail of 3-byte code)
         • 1-byte variant → narrow width (reuses tail of 2-byte code)
         • 0-byte variant → handled by special dispatch skip

    - Each larger variant (3-byte) contains the logic of the smaller ones.
    - This structure eliminates inner loops, reducing per-byte overhead.
    - The entry point is chosen via a dispatch table indexed by bytes_per_row.

  Result:
    Compact code with four minimal entry vectors handles all possible
    cel widths and orientations efficiently while maintaining pixel fidelity.
==============================================================================
*/
#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "render_actor.asm"

.label  sprite_ptr                 = $80        // ZP: pointer to destination sprite row buffer (lo/hi)
.label  cel_ptr                    = $7e        // ZP: pointer to current cel data stream (lo/hi)
.label  cel_ofs                    = $fc00      // Current read offset within cel (header or pixel index)
.label  saved_read_ofs             = $17        // Temporary backup of Y read offset during header parsing

.label  bytes_per_row              = $fc01      // Width in bytes for each pixel row (0–3)
.label  vertical_size              = $fc02      // Height of cel in scanlines
.label  current_row_idx            = $fc28      // Sprite-space index of first row to render
.label  last_row_idx               = $fc29      // Sprite-space index of last row (exclusive upper bound)
.label  horizontal_byte_offset     = $fc2a      // Destination byte slot index for first cel byte (0–2)
.label  rows_to_skip               = $fc3b      // Number of leading input rows to skip (when clamped to top)
.label  horizontal_offset          = $34        // Header field: horizontal byte offset (0–2) for placement
.label  blit_bytes                 = $49        // Temporary buffer for up to 3 source bytes (pre-mask stage)
.label  row_skip_needed            = $1e90      // Boolean flag (1 = skip rows before rendering, 0 = none)

/*
 Indirect dispatch vectors (patched at runtime based on bytes_per_row and flip flag)
*/
.label  mask_dispatch              = $1fe9      // Jump vector to active mask routine (apply_mask_{1..3})
.label  blit_dispatch              = $203c      // Jump vector to active blit routine (copy_row_* or flip_row_*)

/*
 Dispatch tables (address sets, indexed by bytes_per_row)
   Each pair of *_entry_lo/hi defines the low/high bytes for 4 code entry points:
   [0] = empty (skip), [1] = 1-byte path, [2] = 2-byte path, [3] = 3-byte path
*/
.label  mask_entry_lo              = $203f      // Table of low bytes for mask routine entry points
.label  mask_entry_hi              = $2043      // Table of high bytes for mask routine entry points
.label  copy_entry_lo              = $2047      // Table of low bytes for straight (non-flipped) copy entry points
.label  copy_entry_hi              = $204b      // Table of high bytes for straight (non-flipped) copy entry points
.label  flip_entry_lo              = $204f      // Table of low bytes for horizontally flipped copy entry points
.label  flip_entry_hi              = $2053      // Table of high bytes for horizontally flipped copy entry points

* = $203F
/*
 ------------------------------------------------------------
 Mask routine dispatch table
 ------------------------------------------------------------
 Indexed by bytes_per_row (0..3):
   0 → advance_output_row  (skip mask, no visible data)
   1 → apply_mask_1byte    (merge 4-pixel-wide cel)
   2 → apply_mask_2bytes   (merge 8-pixel-wide cel)
   3 → apply_mask_3bytes   (merge 12-pixel-wide cel)
 Each entry stores low/high address bytes for indirect dispatch.
*/
mask_entry:
.byte   <advance_output_row, <apply_mask_1byte, <apply_mask_2bytes, <apply_mask_3bytes
.byte   >advance_output_row, >apply_mask_1byte, >apply_mask_2bytes, >apply_mask_3bytes


* = $2047
/*
 ------------------------------------------------------------
 Straight (non-flipped) blit dispatch table
 ------------------------------------------------------------
 Indexed by bytes_per_row (0..3):
   0 → dispatch_mask_after_copy (no pixels, skip straight to mask stage)
   1 → copy_row_1byte           (copy 1 source byte to blit buffer)
   2 → copy_row_2bytes          (copy 2 source bytes to blit buffer)
   3 → copy_row_3bytes          (copy 3 source bytes to blit buffer)
 The routine entry points perform inline copies before invoking mask.
*/
copy_entry:
.byte   <dispatch_mask_after_copy, <copy_row_1byte, <copy_row_2bytes, <copy_row_3bytes
.byte   >dispatch_mask_after_copy, >copy_row_1byte, >copy_row_2bytes, >copy_row_3bytes


* = $204F
/*
 ------------------------------------------------------------
 Flipped blit dispatch table
 ------------------------------------------------------------
 Indexed by bytes_per_row (0..3):
   0 → dispatch_mask_after_flip (no pixels, skip straight to mask stage)
   1 → flip_row_1byte           (flip and copy 1 byte using lookup table)
   2 → flip_row_2bytes          (flip and copy 2 bytes)
   3 → flip_row_3bytes          (flip and copy 3 bytes)
 The flipped path reverses bit order via flipped_patterns lookup.
*/
flip_entry:
.byte   <dispatch_mask_after_flip, <flip_row_1byte, <flip_row_2bytes, <flip_row_3bytes
.byte   >dispatch_mask_after_flip, >flip_row_1byte, >flip_row_2bytes, >flip_row_3bytes


* = $D200
/*
 ------------------------------------------------------------
 Sprite mask patterns
 ------------------------------------------------------------
 256-byte lookup table providing bit masks for pixel compositing.
 For each 2-bit pixel pattern (00–FF), the entry defines which
 bits of the existing sprite pixel should be *kept* during masking.

 Used in: apply_mask_* routines.
 Operation:
    sprite_byte = (sprite_byte AND sprite_mask_patterns[src_byte]) OR src_byte
*/
sprite_mask_patterns:
.byte $ff,$fc,$fc,$fc,$f3,$f0,$f0,$f0,$f3,$f0,$f0,$f0,$f3,$f0,$f0,$f0
.byte $cf,$cc,$cc,$cc,$c3,$c0,$c0,$c0,$c3,$c0,$c0,$c0,$c3,$c0,$c0,$c0
.byte $cf,$cc,$cc,$cc,$c3,$c0,$c0,$c0,$c3,$c0,$c0,$c0,$c3,$c0,$c0,$c0
.byte $cf,$cc,$cc,$cc,$c3,$c0,$c0,$c0,$c3,$c0,$c0,$c0,$c3,$c0,$c0,$c0
.byte $3f,$3c,$3c,$3c,$33,$30,$30,$30,$33,$30,$30,$30,$33,$30,$30,$30
.byte $0f,$0c,$0c,$0c,$03,$00,$00,$00,$03,$00,$00,$00,$03,$00,$00,$00
.byte $0f,$0c,$0c,$0c,$03,$00,$00,$00,$03,$00,$00,$00,$03,$00,$00,$00
.byte $0f,$0c,$0c,$0c,$03,$00,$00,$00,$03,$00,$00,$00,$03,$00,$00,$00
.byte $3f,$3c,$3c,$3c,$33,$30,$30,$30,$33,$30,$30,$30,$33,$30,$30,$30
.byte $0f,$0c,$0c,$0c,$03,$00,$00,$00,$03,$00,$00,$00,$03,$00,$00,$00
.byte $0f,$0c,$0c,$0c,$03,$00,$00,$00,$03,$00,$00,$00,$03,$00,$00,$00
.byte $0f,$0c,$0c,$0c,$03,$00,$00,$00,$03,$00,$00,$00,$03,$00,$00,$00
.byte $3f,$3c,$3c,$3c,$33,$30,$30,$30,$33,$30,$30,$30,$33,$30,$30,$30
.byte $0f,$0c,$0c,$0c,$03,$00,$00,$00,$03,$00,$00,$00,$03,$00,$00,$00
.byte $0f,$0c,$0c,$0c,$03,$00,$00,$00,$03,$00,$00,$00,$03,$00,$00,$00
.byte $0f,$0c,$0c,$0c,$03,$00,$00,$00,$03,$00,$00,$00,$03,$00,$00,$00


* = $D300
/*
 ------------------------------------------------------------
 Flipped pixel pattern lookup table
 ------------------------------------------------------------
 256-byte precomputed table mapping each 8-bit pixel pattern
 (representing 4 multicolor pixels) to its horizontally mirrored form.

 Example:
   Original byte → Flipped horizontally by reversing 2-bit groups.
   Used in: flip_row_* routines to build mirrored cels in real time.

 Indexing:  input_byte = cel_data_byte
             flipped_output = flipped_patterns[input_byte]
*/
flipped_patterns:
.byte $00,$40,$80,$c0,$10,$50,$90,$d0,$20,$60,$a0,$e0,$30,$70,$b0,$f0
.byte $04,$44,$84,$c4,$14,$54,$94,$d4,$24,$64,$a4,$e4,$34,$74,$b4,$f4
.byte $08,$48,$88,$c8,$18,$58,$98,$d8,$28,$68,$a8,$e8,$38,$78,$b8,$f8
.byte $0c,$4c,$8c,$cc,$1c,$5c,$9c,$dc,$2c,$6c,$ac,$ec,$3c,$7c,$bc,$fc
.byte $01,$41,$81,$c1,$11,$51,$91,$d1,$21,$61,$a1,$e1,$31,$71,$b1,$f1
.byte $05,$45,$85,$c5,$15,$55,$95,$d5,$25,$65,$a5,$e5,$35,$75,$b5,$f5
.byte $09,$49,$89,$c9,$19,$59,$99,$d9,$29,$69,$a9,$e9,$39,$79,$b9,$f9
.byte $0d,$4d,$8d,$cd,$1d,$5d,$9d,$dd,$2d,$6d,$ad,$ed,$3d,$7d,$bd,$fd
.byte $02,$42,$82,$c2,$12,$52,$92,$d2,$22,$62,$a2,$e2,$32,$72,$b2,$f2
.byte $06,$46,$86,$c6,$16,$56,$96,$d6,$26,$66,$a6,$e6,$36,$76,$b6,$f6
.byte $0a,$4a,$8a,$ca,$1a,$5a,$9a,$da,$2a,$6a,$aa,$ea,$3a,$7a,$ba,$fa
.byte $0e,$4e,$8e,$ce,$1e,$5e,$9e,$de,$2e,$6e,$ae,$ee,$3e,$7e,$be,$fe
.byte $03,$43,$83,$c3,$13,$53,$93,$d3,$23,$63,$a3,$e3,$33,$73,$b3,$f3
.byte $07,$47,$87,$c7,$17,$57,$97,$d7,$27,$67,$a7,$e7,$37,$77,$b7,$f7
.byte $0b,$4b,$8b,$cb,$1b,$5b,$9b,$db,$2b,$6b,$ab,$eb,$3b,$7b,$bb,$fb
.byte $0f,$4f,$8f,$cf,$1f,$5f,$9f,$df,$2f,$6f,$af,$ef,$3f,$7f,$bf,$ff

/*
===============================================================================
 blit_and_mask_cel — render one cel into a multicolor sprite (with flip+mask)
===============================================================================
 Summary
   Parses a 6-byte cel header, selects copy/flip and mask handlers by width,
   clamps the target scanline range, then composites the cel’s rows into the
   destination sprite buffer using transparency masks.

 Inputs (globals / pointers)
   cel_ptr                ptr to cel stream (lo/hi)
   actor_sprite_base      sprite base address (lo/hi)
   sprite_row_offsets_*   per-row offsets table
   mask_row_ofs_*         per-tile-row mask offsets (lo/hi)
   current_sprite_y_pos   bottom Y of composite sprite (pixels)
   current_limb_flip      0=normal, !0=flipped
   flipped_patterns       byte-wise horizontal mirror LUT
   sprite_mask_patterns   per-byte keep-mask LUT for compositing

 Outputs / state
   sprite_ptr             ptr to destination sprite row (lo/hi)
   mask_row_ptr           ptr to mask-layer row (lo/hi)
   current_row_idx        start row in sprite space (clamped)
   last_row_idx           end row (exclusive, clamped)
   cel_ofs                saved Y read offset within cel
   horizontal_byte_offset destination byte slot for first cel byte (0..2)

 Invariants / validation
   (bytes_per_row + horizontal_offset) ≤ 3
   On violation: records error code and halts with visible border write.

 Dispatch (jump tables indexed by BPR, Bytes Per Row)
   blit_dispatch := copy_entry[BPR]       normal
                    or flip_entry[BPR]    flipped (uses flipped_patterns)
   mask_dispatch := mask_entry[BPR]
   Entry 0 in each table skips work (no pixel data).

 Placement
   Normal:  dst_xbyte = (BPR - 1) + horizontal_offset    left-anchored
   Flipped: dst_xbyte = 2 - horizontal_offset            right-anchored

 Clamping
   start = current_sprite_y_pos - vertical_offset
   end   = start + vertical_size  (exclusive)
   If end ≤ SCREEN_Y_MAX → proceed.
   If start ≤ SCREEN_Y_MAX < end → clamp end to SCREEN_Y_MAX.
   Else (start > SCREEN_Y_MAX) → not visible → RTS.
   If start underflows (<1) → clamp start to 1 and skip input rows accordingly.

 Row loop (per visible scanline)
 
   1) sprite_ptr := actor_sprite_base + sprite_row_offsets[start]
      mask_row_ptr from mask_row_ofs[(start>>3)] (+ optional debug offsets).
	  
   2) Execute blit_dispatch:
        copy_row_3/2/1 or flip_row_3/2/1
        Multiple entry points avoid per-byte inner loops.
        Stages up to 3 bytes in blit_bytes[].
		
   3) Execute mask_dispatch:
        For each byte k: dst = (dst AND mask[src_k]) OR src_k
        Y = horizontal_byte_offset, DEY across bytes (2→0).
		
   4) Advance to next row, recompute sprite_ptr, repeat until start == end.

 Performance notes
   - Table-driven dispatch + multi-entry routines remove inner branching.
   - Flip is O(1) per byte via LUT.
   - Zero-width rows short-circuit through entry 0.
===============================================================================
*/
* = $1E91
blit_and_mask_cel:
		// ----------------------------------------------------------------------
        // Initialize state before header parsing
		//
        //  clear row-skip flag (no skipped input rows yet)
        //  set read offset as header read index (points to first header byte)
		// ----------------------------------------------------------------------
        lda     #$00
        sta     row_skip_needed
        ldy     #$00
        sty     cel_ofs

		// ----------------------------------------------------------------------
        // Parse cel header (Y = 0..2)
		//
        //   bytes_per_row (0..3) → selects copy/flip/mask variants
        //   vertical_size        → number of scanlines to render
        //   horizontal_offset    → byte shift within 12-px row (0..2)
		// ----------------------------------------------------------------------
        lda     (cel_ptr),y
        sta     bytes_per_row
        tax                                     // X := bytes_per_row for jump tables
        iny                                     // Y -> header[1]
        lda     (cel_ptr),y
        sta     vertical_size
        iny                                     // Y -> header[2]
        lda     (cel_ptr),y
        sta     horizontal_offset

		// ----------------------------------------------------------------------
        // Validate header constraint
		//
        //   Test: (horizontal_offset + bytes_per_row) ≤ 3
        //   If true (C=0 or result==3), proceed; else enter debug halt below.
		// ----------------------------------------------------------------------
        clc                                     // prepare add: A := horiz_ofs + bpr
        adc     bytes_per_row
        cmp     #$03                            // result < 3  → C=0,Z=0 → BCC
        bcc     select_blit_variant             // result == 3 → Z=1     → BEQ
        beq     select_blit_variant

		// ----------------------------------------------------------------------
        // Assertion trap
		//
        //   record error code 04
        //   enable I/O mapping so VIC registers are visible
		// ----------------------------------------------------------------------
        lda     #$04
        sta     debug_error_code
        ldy     #MAP_IO_IN
        sty     cpu_port
hang_loop_2:
        sta     vic_border_color_reg          
        jmp     hang_loop_2

select_blit_variant:
        // Preserve header read index (Y) for later restore
        sty     saved_read_ofs
        // Load actor index (unused in this block; safe to remove)
        ldy     actor
		
		// ----------------------------------------------------------------------
        // Flip test: #$00 → normal path, nonzero → flipped path
		// ----------------------------------------------------------------------
        lda     current_limb_flip
        beq     configure_normal_variant      
                                            
		// ----------------------------------------------------------------------
        // Set blit dispatch for flipped copy:
		//
        //   X = bytes_per_row (0..3) indexes jump table
        //   blit_dispatch := flip_entry[x]
		// ----------------------------------------------------------------------
        lda     flip_entry_lo,x
        sta     blit_dispatch
        lda     flip_entry_hi,x
        sta     blit_dispatch + 1
		
		// ----------------------------------------------------------------------
        // Compute destination byte index for flipped blit
		//
        //   horizontal_byte_offset = 2 - horizontal_offset  (right-anchored within 3-byte row)
		// ----------------------------------------------------------------------
        lda     #$02
        sec
        sbc     horizontal_offset
        jmp     set_column_offset

configure_normal_variant:
		// ----------------------------------------------------------------------
        // Set blit dispatch for non-flipped copy
		//
        //   X = bytes_per_row (0..3) → blit_dispatch := copy_entry[x]
		// ----------------------------------------------------------------------
        lda     copy_entry_lo,x
        sta     blit_dispatch
        lda     copy_entry_hi,x
        sta     blit_dispatch + 1
		
		// ----------------------------------------------------------------------
        // Compute destination byte index for normal blit
		//
        //   horizontal_byte_offset = (bytes_per_row - 1) + horizontal_offset  (left-anchored)
        //   Ranges: bytes_per_row∈{0,1,2,3}, horizontal_offset∈{0,1,2} → result 0..2
		//
        //   Note: if bytes_per_row==0, A underflows to $FF but zero-byte path is dispatched
        //         before use, so horizontal_byte_offset is ignored.
		// ----------------------------------------------------------------------
        lda     bytes_per_row
        sec
        sbc     #$01
        clc
        adc     horizontal_offset

set_column_offset:
        //   Restore Y := saved header read index
        ldy     saved_read_ofs
		// ----------------------------------------------------------------------
        // Finalize horizontal placement
		//
        //   horizontal_byte_offset := A (computed dst byte 0..2)
		// ----------------------------------------------------------------------
        sta     horizontal_byte_offset
		
		// ----------------------------------------------------------------------
        // Select mask handler by width
		//
        //   X = bytes_per_row (0..3) → mask_dispatch := mask_entry[x]
		// ----------------------------------------------------------------------
        lda     mask_entry_lo,x
        sta     mask_dispatch
        lda     mask_entry_hi,x
        sta     mask_dispatch + 1

		// ----------------------------------------------------------------------
        // Read header[3]:
		// 
        //  vertical_offset := rows above sprite's Y position to place this cel
		// ----------------------------------------------------------------------
        iny
        lda     (cel_ptr),y
        sta     vertical_offset

		// ----------------------------------------------------------------------
        // Start row = sprite's Y position - cel’s vertical offset
		// ----------------------------------------------------------------------
        sec
        lda     current_sprite_y_pos
        sbc     vertical_offset
        sta     current_row_idx

		// ----------------------------------------------------------------------
        // Compute last visible row index
		//
        //   last_row_idx = current_row_idx + vertical_size
        //   Defines bottom limit (exclusive) of the cel’s visible scanlines.
		// ----------------------------------------------------------------------
        clc
        adc     vertical_size
        sta     last_row_idx

		// ----------------------------------------------------------------------
        // Bottom bound check
		//
        //   If last_row_index ≤ SCREEN_Y_MAX → sprite visible, continue
		// ----------------------------------------------------------------------
        cmp     #SCREEN_Y_MAX
        bcc     clamp_rows_to_screen
        beq     clamp_rows_to_screen

		// ----------------------------------------------------------------------
        // Fully-below-screen test
		//
        //   If start ≤ SCREEN_Y_MAX → only end exceeds → clamp end
        //   Else start > SCREEN_Y_MAX → entirely below → not visible, return
		// ----------------------------------------------------------------------
        lda     current_row_idx    
        cmp     #SCREEN_Y_MAX
        bcc     clamp_end_to_screenmax
        beq     clamp_end_to_screenmax
        rts

clamp_end_to_screenmax:
		// ----------------------------------------------------------------------
        // Clamp bottom to screen limit (start is within view)
		//
        //   last_row_index := SCREEN_Y_MAX
		// ----------------------------------------------------------------------
        lda     #SCREEN_Y_MAX
        sta     last_row_idx      
        jmp     init_output_and_mask_ptrs

clamp_rows_to_screen:
		// ----------------------------------------------------------------------
        // If start ≤ SCREEN_Y_MAX, continue without further clamping.
        // Otherwise fall through to code that sets rows_to_skip and clamps start.
		// ----------------------------------------------------------------------
        lda     current_row_idx
        cmp     #SCREEN_Y_MAX
        bcc     init_output_and_mask_ptrs
        beq     init_output_and_mask_ptrs		

		// ----------------------------------------------------------------------
		// Clamp
		// ----------------------------------------------------------------------
        // rows_to_skip := current_row_idx - 1
        sta     rows_to_skip
        dec     rows_to_skip
		
        // Clamp start to first visible row (row index = 1)
        lda     #$01
        sta     current_row_idx

        // Indicate clamped start → skip leading input rows before rendering
        lda     #$01
        sta     row_skip_needed

		// ----------------------------------------------------------------------
        // Range check after clamping
		// ----------------------------------------------------------------------
        //   if last_row_idx ≥ current_row_idx → continue
        //   else (end < start) → nothing to render → RTS
        lda     last_row_idx          
        cmp     current_row_idx       
        bcs     init_output_and_mask_ptrs
        rts

init_output_and_mask_ptrs:
		// ----------------------------------------------------------------------
        // Compute destination sprite row pointer
		//
        //   sprite_ptr := actor_sprite_base + sprite_row_offsets[current_row_idx]
		// ----------------------------------------------------------------------
        ldx     current_row_idx           	
        lda     sprite_row_offsets_lo,x   	
        clc                               	
        adc     actor_sprite_base        	
        sta     sprite_ptr        			 
        lda     sprite_row_offsets_hi,x   	
        adc     actor_sprite_base + 1
        sta     sprite_ptr + 1

		// ----------------------------------------------------------------------
        // Derive mask-layer row pointer from tile row = current_row_idx >> 3
		//
        //   - Bring X (row index) into A, divide by 8 via three LSRs.
        //   - Put tile-row index back in X.
        //   - Look up mask row base offset (lo/hi), add optional debug tweaks,
        //     and form mask_row_ptr.
		// ----------------------------------------------------------------------
        txa
        lsr
        lsr
        lsr
        tax
        lda     mask_row_ofs_lo,x        
        clc
        adc     $FD21                    
        sta     mask_row_ptr            
        lda     mask_row_ofs_hi,x        
        adc     $FD22                    
        sta     mask_row_ptr + 1

		// ----------------------------------------------------------------------
        // Read header byte #4 (unused) and inter-cel vertical displacement
		// ----------------------------------------------------------------------
        iny
        lda     (cel_ptr),y
        sta     fifth_byte
        iny
        lda     (cel_ptr),y
        sta     intercel_vertical_displacement

		// ----------------------------------------------------------------------
        // Advance cel_ptr to first pixel byte by skipping the header (Y + 1 = 6 bytes)
		// ----------------------------------------------------------------------
        tya
        sec
        adc     cel_ptr
        sta     cel_ptr
        bcc     handle_row_skip
        inc     cel_ptr + 1

handle_row_skip:
		// ----------------------------------------------------------------------
		// Skip rows if needed
		//
        // If clamped, skip 'rows_to_skip' rows worth of input bytes
		// Otherwise, go straight to the row blit
		// ----------------------------------------------------------------------
        lda     row_skip_needed
        beq     next_row

        ldy     rows_to_skip
next_row_to_skip:
        ldx     bytes_per_row
skip_row_bytes:
        inc     cel_ptr
        bne     next_byte_3
        inc     cel_ptr + 1
next_byte_3:
        dex
        bne     skip_row_bytes
        iny
        bne     next_row_to_skip

next_row:
        jmp     blit_next_row

/*
==============================================================================
 Straight copy variants (bytes_per_row = 3 → 2 → 1)
 
 Entry points:
   copy_row_3bytes → copies 3 bytes, then falls through to 2→1.
   copy_row_2bytes → copies 2 bytes, then falls through to 1.
   copy_row_1byte  → copies 1 byte, then dispatches to masking.
   
 Conventions:
   Y = cel read index
   X = in-row byte cursor (0..2)
   blit_bytes[] = staging
   
 Flow:
   Each label loads (cel_ptr),Y → blit_bytes[X], then INX, INY.
   After the final copy, Y is saved to cel_ofs and control jumps to mask. 
==============================================================================
*/
copy_row_3bytes:
        lda     (cel_ptr),y
        sta     blit_bytes,x
        inx
        iny
copy_row_2bytes:
        lda     (cel_ptr),y
        sta     blit_bytes,x
        inx
        iny
copy_row_1byte:
        lda     (cel_ptr),y
        sta     blit_bytes,x
        inx
        iny
        sty     cel_ofs
		
dispatch_mask_after_copy:
        jmp     dispatch_mask_after_flip

/*
==============================================================================
 Flipped copy variants (bytes_per_row = 3 → 2 → 1)
 
 Entry points:
   flip_row_3bytes → flips 3 bytes, then falls through to 2→1.
   flip_row_2bytes → flips 2 bytes, then falls through to 1.
   flip_row_1byte  → flips 1 byte, then dispatches to masking.
   
 Conventions:
   Y = cel read index
   X = temp for flip lookup
   blit_bytes[] = staging
   
 Flow:
   Load (cel_ptr),Y → X → flipped_patterns[X] → write to blit_bytes[k].
   INY after each byte. Save Y to cel_ofs at the end, then dispatch.
==============================================================================
*/
flip_row_3bytes:
        lda     (cel_ptr),y
        tax
        lda     flipped_patterns,x
        sta     blit_bytes + 2
        iny
flip_row_2bytes:
        lda     (cel_ptr),y
        tax
        lda     flipped_patterns,x
        sta     blit_bytes + 1
        iny
flip_row_1byte:
        lda     (cel_ptr),y
        tax
        lda     flipped_patterns,x
        sta     blit_bytes
        iny
        sty     cel_ofs
		
dispatch_mask_after_flip:
        ldy     horizontal_byte_offset
        jmp     $0000 //mask_dispatch
        jmp 	advance_output_row   //unreachable code

/*
==============================================================================
 Masking variants (bytes_per_row = 3 → 2 → 1)
 
 Entry points:
   apply_mask_3bytes → processes byte2 then falls through to 2→1.
   apply_mask_2bytes → processes byte1 then falls through to 1.
   apply_mask_1byte  → processes byte0 and returns.

 Conventions:
   blit_bytes[k]     = source pixel byte for sprite byte k (k=0..2)
   sprite_mask_patterns[blit_bytes[k]] → transparency mask for that byte
   Y = destination byte index (2..0). Each step DEY moves to next byte left.
   (sprite_ptr),Y    = destination sprite row byte

 Operation per byte:
   A := sprite_mask_patterns[ blit_bytes[k] ]
   A := A AND (sprite_ptr)[Y]          keep visible bg pixels
   A := A OR  blit_bytes[k]            overlay cel pixels
   (sprite_ptr)[Y] := A                write merged result 
==============================================================================
*/
apply_mask_3bytes:
        ldx     blit_bytes + 2
        lda     sprite_mask_patterns,x
        and     (sprite_ptr),y
        ora     blit_bytes + 2
        sta     (sprite_ptr),y
        dey
apply_mask_2bytes:
        ldx     blit_bytes + 1
        lda     sprite_mask_patterns,x
        and     (sprite_ptr),y
        ora     blit_bytes + 1
        sta     (sprite_ptr),y
        dey
apply_mask_1byte:
        ldx     blit_bytes
        lda     sprite_mask_patterns,x
        and     (sprite_ptr),y
        ora     blit_bytes
        sta     (sprite_ptr),y

/*
==============================================================================
 Row advance and loop control
==============================================================================
*/
advance_output_row:
        // Advance to next destination row unless at last one:
        //    
        //   if current_row_idx == last_row_idx → all rows rendered → exit
        //   else continue to compute new sprite_ptr
        ldx     current_row_idx
        cpx     last_row_idx
        beq     exit_blit

        // sprite_ptr := actor_sprite_base + sprite_row_offsets[X]  (16-bit add)
        lda     sprite_row_offsets_lo,x
        clc
        adc     actor_sprite_base
        sta     sprite_ptr
        lda     sprite_row_offsets_hi,x
        adc     actor_sprite_base + 1
        sta     sprite_ptr + 1

blit_next_row:
        // Loop guard for row rendering:
        //   if current_row_idx == last_row_idx → done → exit_blit
        ldx     current_row_idx
        cpx     last_row_idx
        beq     exit_blit

        // Advance to next row and restart blit
        inx
        stx     current_row_idx     // row_y := row_y + 1
        ldx     #$00                // X := 0 (in-row byte selector)
        ldy     cel_ofs   			// restore Y (saved read index)
        jmp     $0000 				//blit_dispatch - dispatch to chosen copy/flip path

exit_blit:
        rts

/*
procedure blit_and_mask_cel():
    # Reset row-skip state
    row_skip_needed = False
    header_read_index = 0
    cel_ofs = 0

    # --- Parse 3-byte header prefix ---
    bytes_per_row     = read_byte(cel_ptr + header_read_index); header_read_index += 1
    vertical_size     = read_byte(cel_ptr + header_read_index); header_read_index += 1
    horizontal_offset = read_byte(cel_ptr + header_read_index); header_read_index += 1

    # Validate: horizontal_offset + bytes_per_row ≤ 3 (3 bytes per 12-pixel row)
    if horizontal_offset + bytes_per_row > 3:
		halt

    # Choose copy/flip variant and horizontal destination byte index
    select_blit_variant(bytes_per_row,
                        horizontal_offset,
                        current_limb_flip)

    # --- Compute vertical range in sprite space ---
    # current_row_idx: where the cel’s top row lands in sprite Y
    vertical_offset = read_byte(cel_ptr + header_read_index)
    header_read_index += 1

    current_row_idx = current_sprite_y_pos - vertical_offset

    # last_row_idx: exclusive bottom bound (current + height)
    last_row_idx = current_row_idx + vertical_size

    # If bottom ≤ SCREEN_Y_MAX, do simple clamp of start.
    # If bottom > SCREEN_Y_MAX, perform visibility check (fully below vs partially visible).
    if last_row_idx <= SCREEN_Y_MAX:
        clamp_rows_to_screen()
    else:
        if current_row_idx <= SCREEN_Y_MAX:
            clamp_end_to_screenmax()
        else:
            return    # fully below screen → nothing to draw


procedure clamp_end_to_screenmax():
    # Bottom exceeds screen, but top is visible: clamp the end.
    last_row_idx = SCREEN_Y_MAX
    init_output_and_mask_ptrs()


procedure clamp_rows_to_screen():
    # Called when bottom is within screen. We still need to clamp the top if it’s above.
    if current_row_idx <= SCREEN_Y_MAX:
        # Top is not below screen; no special top clamp, proceed.
        init_output_and_mask_ptrs()
        return

    # Top is below or at bottom limit → clamp the start to first visible row
    # rows_to_skip = number of rows to throw away from the input
    rows_to_skip = current_row_idx - 1
    current_row_idx = 1         # clamp to top visible row index
    row_skip_needed = True         # flag that we must skip input rows

    # If, after clamping, the bottom is still above the start, nothing to render
    if last_row_idx < current_row_idx:
        return

    init_output_and_mask_ptrs()


procedure init_output_and_mask_ptrs():
    # Compute destination sprite row pointer for current_row_idx
    sprite_ptr = actor_sprite_base + sprite_row_offsets[current_row_idx]

    # Compute mask_row_ptr from tile row index (current_row_idx // 8)
    tile_row_index = current_row_idx // 8
    mask_row_ptr   = mask_row_base(tile_row_index) + mask_row_debug_offset

    # Read header bytes 4 and 5 (5th is unused, 6th is intercel vertical offset)
    header_read_index += 1
    fifth_byte = read_byte(cel_ptr + header_read_index)
    header_read_index += 1
    intercel_vertical_displacement = read_byte(cel_ptr + header_read_index)

    # Move cel_ptr from header start to first pixel byte
    # (header_read_index currently points to last header byte; add header_read_index+1)
    cel_ptr = cel_ptr + (header_read_index + 1)

    handle_row_skip()


procedure handle_row_skip():
    if not row_skip_needed:
        goto next_row

    # Skip 'rows_to_skip' rows of input pixels
    remaining_rows = rows_to_skip
    while remaining_rows > 0:
        # For each row, skip bytes_per_row bytes of cel data
        remaining_bytes = bytes_per_row
        while remaining_bytes > 0:
            cel_ptr += 1
            remaining_bytes -= 1

        remaining_rows -= 1

    # After skipping, proceed to first visible row
    next_row()


procedure next_row():
    # Enter the row-blitting loop
    blit_next_row()


procedure select_blit_variant(bytes_per_row, horizontal_offset, flip_flag):
    # Save header_read_index (Y) to restore later
    saved_read_ofs = header_read_index

    if flip_flag:
        # Configure flipped blit
        blit_dispatch = flip_entry_table[bytes_per_row]

        # Flipped layout is right-anchored: choose where the first byte lands
        horizontal_byte_offset = 2 - horizontal_offset
    else:
        # Configure normal (non-flipped) blit
        blit_dispatch = copy_entry_table[bytes_per_row]

        # Non-flipped layout: last cel byte + horizontal offset
        # (if bytes_per_row==0, we won’t use this; zero-width dispatch
        #  bypasses the write anyway).
        horizontal_byte_offset = (bytes_per_row - 1) + horizontal_offset

    # Restore header_read_index and store horizontal_byte_offset
    header_read_index = saved_read_ofs
    horizontal_byte_offset_global = horizontal_byte_offset

    # Configure mask dispatch based on width
    mask_dispatch = mask_entry_table[bytes_per_row]


# All three use shared state:
#   cel_ptr  : pointer to current cel row byte
#   header_read_index (Y-equivalent): index within cel data
#   X        : in-row byte index (0..2)
#   blit_bytes[0..2]: staging buffer for this row

procedure copy_row_3bytes():
    # Called when bytes_per_row == 3
    blit_bytes[ X ] = read_byte(cel_ptr + header_read_index); X += 1; header_read_index += 1
    # falls through to copy_row_2bytes

procedure copy_row_2bytes():
    # Called when bytes_per_row >= 2
    blit_bytes[ X ] = read_byte(cel_ptr + header_read_index); X += 1; header_read_index += 1
    # falls through to copy_row_1byte

procedure copy_row_1byte():
    # Called when bytes_per_row >= 1
    blit_bytes[ X ] = read_byte(cel_ptr + header_read_index); X += 1; header_read_index += 1

    # Save updated read index for next row
    cel_ofs = header_read_index

    # Then perform mask/composite for this row
    dispatch_mask_after_copy()


procedure dispatch_mask_after_copy():
    # Just reuse the flip dispatch’s mask entry; in code this jumps to the
    # common label used by flipped path as well.
    dispatch_mask_after_flip()


# For flipped rows, each source byte is looked up in flipped_patterns[]
# to get the horizontally mirrored bit pattern.

procedure flip_row_3bytes():
    # bytes_per_row == 3
    src = read_byte(cel_ptr + header_read_index)
    blit_bytes[2] = flipped_patterns[src]
    header_read_index += 1
    # falls through to flip_row_2bytes

procedure flip_row_2bytes():
    src = read_byte(cel_ptr + header_read_index)
    blit_bytes[1] = flipped_patterns[src]
    header_read_index += 1
    # falls through to flip_row_1byte

procedure flip_row_1byte():
    src = read_byte(cel_ptr + header_read_index)
    blit_bytes[0] = flipped_patterns[src]
    header_read_index += 1

    # Save updated read index for next row
    cel_ofs = header_read_index

    dispatch_mask_after_flip()


procedure dispatch_mask_after_flip():
    # Position within the sprite row where the rightmost cel byte is written
    y_index = horizontal_byte_offset_global

    # Call the appropriate mask routine chosen earlier
    mask_dispatch(y_index)


# Common conventions:
#   sprite_ptr points to the start of the destination sprite row
#   y_index    is the byte index within that row (0..2), initially set so that
#              blit_bytes[2] aligns with the rightmost destination byte.

procedure apply_mask_3bytes(y_index):
    # Rightmost byte
    src = blit_bytes[2]
    mask = sprite_mask_patterns[src]
    dst  = sprite_ptr[y_index]
    sprite_ptr[y_index] = (dst & mask) | src
    y_index -= 1

    # fall through to 2-byte version

procedure apply_mask_2bytes(y_index):
    # Middle byte
    src = blit_bytes[1]
    mask = sprite_mask_patterns[src]
    dst  = sprite_ptr[y_index]
    sprite_ptr[y_index] = (dst & mask) | src
    y_index -= 1

    # fall through to 1-byte version

procedure apply_mask_1byte(y_index):
    # Leftmost byte
    src = blit_bytes[0]
    mask = sprite_mask_patterns[src]
    dst  = sprite_ptr[y_index]
    sprite_ptr[y_index] = (dst & mask) | src

    # After masking, advance to next output row
    advance_output_row()


procedure advance_output_row():
    # If we've reached the last row, stop.
    if current_row_idx == last_row_idx:
        exit_blit()
        return

    # Recompute sprite_ptr for this row index
    row_index = current_row_idx
    sprite_ptr = actor_sprite_base + sprite_row_offsets[row_index]

    blit_next_row()


procedure blit_next_row():
    # Loop guard: stop if we've already rendered the last row
    if current_row_idx == last_row_idx:
        return

    # Move to the next row
    current_row_idx += 1

    # Reset in-row cursor and cel read offset
    in_row_byte_index = 0
    header_read_index = cel_ofs

    # Dispatch to copy/flip handler chosen earlier
    blit_dispatch()

*/