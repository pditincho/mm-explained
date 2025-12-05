/*
================================================================================
Conceptual overview — what masking is and how it works here
================================================================================
Summary
    “Masking” is the process of selectively hiding parts of an actor’s sprite
    wherever the foreground layer is in front of them, so the actor appears to
    walk behind walls, rails, and other scenery.

Mask layer
    • The foreground layer can be considered as a separate black-and-white image laid
      over the room: each tile cell says “foreground in front here” or “no
      foreground here”.
    • Instead of storing full pixels, the mask layer stores small indices that
      reference reusable 8-scanline patterns in a shared table.
    • For any (tile_x, tile_y), the engine can quickly fetch “how much of this
      8×8 area is foreground in front of actors?”.

Mask interaction with a sprite
    • Each actor’s sprite row is 3 bytes wide (24 pixels) in C64 multicolor:
      every pair of bits encodes one pixel.
    • For each scanline:
        - We find the 3 foreground mask cells that align with those 3 sprite
          bytes.
        - Each mask cell chooses one of the prebuilt 8-row patterns.
        - For the current scanline inside the tile (0..7), we take one mask
          byte that has 1 bit per logical pixel.
        - That byte is expanded into a multicolor “fat pixel” mask by
          duplicating each logical bit into its neighboring bit (so one logical
          1 covers a 2-bit pixel pair).

Pixel hiding
    • Once we have the multicolor mask, we simply:
        - AND it into the sprite byte:
              sprite_byte := sprite_byte AND mask_byte
        - Any pixel where the mask has 0s is forced to 0 in the sprite, which
          makes the sprite transparent at that position.
        - Pixels where the mask has 1s are left untouched, so they remain
          visible in front of the background.

Sweep order (bottom to top)
    • The actor’s visible vertical span depends on:
        - Their current on-screen Y position, and
        - Their sprite’s maximum height.
    • Masking only needs to touch the scanlines that can actually contain
      sprite pixels. The routine computes this span, clamps it to the screen
      bounds, and walks upward scanline by scanline, applying the foreground
      mask just where it matters.

================================================================================
Foreground masking system — actor sprites vs. tile-based masks
================================================================================
Summary
    Apply the foreground tile layer as a per-pixel AND mask over an actor’s
    composite sprite, so actors correctly hide “behind” foreground elements
    (e.g., pillars, railings, tree trunks) on a per-scanline basis.

Description
    • Vertical coverage:
        - Compute the sprite’s vertical span from its bottom Y and precomputed
          extent, then clamp that span against the visible screen range.
        - Sweep scanlines from the actor’s bottom Y upward toward a top limit,
          early-out when the actor is fully off-screen above or below.

    • Sprite row selection:
        - For each covered Y, resolve a pointer to the corresponding 3-byte
          multicolor sprite row using actor_sprite_base + sprite_row_offsets[].
        - This row is the working destination that will be masked against the
          foreground layer.

    • Tile-row and column mapping:
        - Map Y to a tile row index via Y >> 3 and use mask_row_ofs[] plus a
          global mask_base_ptr to find the current mask_row_ptr for that band.
        - Track the actor’s tile X coordinate and use it as a starting column
          when building mask pointers for the three sprite-byte columns.

    • Mask pattern lookup:
        - Each tile cell in the mask layer stores a small index into a shared
          mask_bit_patterns table.
        - build_mask_pattern_ptrs reads the per-cell index and converts it
          into a pointer to an 8-byte vertical pattern block (one byte per
          scanline within the tile), for three adjacent columns.

    • Per-scanline masking:
        - For each scanline, select the pattern byte for the current tile-row
          slice (tile_row_idx) from mask_ptr1/2/3.
        - Resolve that byte via a global mask_patterns table into a “1-bit per
          logical pixel” mask, then expand it into a C64 multicolor mask by
          duplicating even bits into their odd neighbors (2-bit “fat pixels”).
        - AND the resulting mask into each of the three sprite bytes, clearing
          sprite pixels where the foreground is opaque and leaving others
          unchanged.

    • Tile-row stepping:
        - Whenever Y crosses an 8-scanline boundary, step mask_row_ptr to the
          previous tile row and rebuild the three per-column mask pattern
          pointers so masking always uses the correct foreground tile band.

Notes
    • The logic is intentionally one-way: the mask layer is treated as fully
      opaque where set; no blending is performed beyond logical AND masking.
    • All pointer arithmetic is expressed via offset tables and per-row
      strides, keeping the masking loop compact enough to run every frame for
      multiple actors.
================================================================================
*/
#importonce
#import "globals.inc"
#import "constants.inc"
#import "render_actor.asm"

.label sprite_row_ptr              = $80    // ZP: pointer to current 3-byte sprite scanline (row buffer)
.label sprite_base_cached          = $CB68  // Cached low byte of actor’s sprite base address
.label y_coordinate                = $FC28  // Current sprite scanline Y position (bottom of actor, pixels)
.label y_top_limit                 = $FC29  // Topmost Y (exclusive) for masking loop
.label y_top_unclamped             = $FC3B  // Original unclamped top Y (saved if clamped to 0)
.label tile_row_idx                = $FD23  // Y & 7 → intra-tile row index (0–7)
.label mask_col_idx                = $FD24  // Current tile column index for foreground mask lookup
.label sprite_byte_idx             = $CB67  // Sprite byte selector within row (0–2 for 24-pixel span)

.label mask_ptr1                   = $43  // ZP: 16-bit pointer #1 → mask pattern row for column 1
.label mask_ptr2                   = $45  // ZP: 16-bit pointer #2 → mask pattern row for column 2
.label mask_ptr3                   = $47  // ZP: 16-bit pointer #3 → mask pattern row for column 3

.label pixel_mask                  = $CB6A  // Scratch byte: assembled 8-bit mask for current sprite byte

.const MASK_BYTES_PER_TILE         = $08    // 8 bytes per tile mask pattern (one per scanline)
.const MASK_BLOCK_HDR_SIZE         = $04    // Size of mask pattern table header before pattern data
.const MASK_EVEN_BITS              = %01010101  // Bitmask selecting even bits before duplication for multicolor

* = $D640
// Low-byte offsets into mask layer per tile row (Y>>3), relative to mask_base_ptr
mask_row_ofs_lo:
.byte $00,$28,$50,$78,$A0,$C8,$F0,$18
.byte $40,$68,$90,$B8,$E0,$08,$30,$58
.byte $80,$A8,$D0,$F8,$20,$48,$70,$98,$C0

* = $D659
// High-byte offsets into mask layer per tile row (Y>>3), relative to mask_base_ptr
mask_row_ofs_hi:
.byte $00,$00,$00,$00,$00,$00,$00,$01
.byte $01,$01,$01,$01,$01,$02,$02,$02
.byte $02,$02,$02,$02,$03,$03,$03,$03,$03


/*
==================================================================
  mask_actor_with_foreground_layer
==================================================================
Summary
	Mask one actor’s sprite against the room’s foreground layer, processing
	scanlines from the actor’s current Y down to a computed top bound. Uses
	per-tile row masks and builds three pattern pointers per scanline to
	AND-mask the 24-pixel sprite row (3 bytes). Supports multicolor sprites
	by duplicating logical bits to fat-pixel pairs in apply_mask.

Arguments
	actor_tile_x_coordinate		Starting tile column for this actor on the current mask row.

Global Inputs
	actor                           current actor index
	character_sprite_y[]            per-actor sprite Y (pixels, bottom)
	actor_sprite_index[]            per-actor sprite ID
	actor_sprite_y_extent[]         max upward extent (pixels)
	mask_base_ptr                   base pointer to mask layer
	mask_row_ofs_lo/hi[]            per-row offsets (y>>3) into mask layer
	actor_sprite_base               resolved sprite base (lo/hi)

Global Outputs
	sprite bytes at sprite_row_ptr  AND-masked in place per scanline

Description
	- Compute top bound: top = y − extent − 1, then handle visibility:
		• If fully below screen (both y and top > SCREEN_Y_MAX), return.
		• If y ≤ SCREEN_Y_MAX but top > SCREEN_Y_MAX, save y_top_unclamped and set top=0.
		• Clamp y to ≤ SCREEN_Y_MAX when needed.
	- Resolve and cache sprite base for this actor.
	- Select mask row: row_index = y >> 3, then mask_row_ptr = mask_base_ptr + offset[row].
	- Initialize mask_col_idx from actor_tile_x_coordinate and call
	build_mask_pattern_ptrs to compute mask_ptr1..3.
	- For each scanline from y down to top:
		• Compute sprite_row_ptr = sprite_base_cached + sprite_row_offsets[y].
		• If (y & 7) == 0, step mask_row_ptr back by VIEW_COLS and rebuild mask_ptr1..3.
		• Decrement y, set tile_row_idx = y & 7.
		• sprite_byte_idx := 0, then call apply_mask (updates all 3 bytes).
	- Exit when y == top.

Notes
	- Multicolor (“fat”) sprites are handled in apply_mask by duplicating
	even→odd bits so each logical pixel maps to a 2-bit pair.
==================================================================
*/
* = $30DB
mask_actor_with_foreground_layer:
		// ----------------------------------------------
        // Load the actor’s vertical sprite position into the working Y register
        //   X := actor index
        //   A := character_sprite_y[X]  → current sprite Y coordinate in pixels
        //   y_coordinate := A            → store as working vertical position (bottom)
		// ----------------------------------------------
        ldx     actor
        lda     character_sprite_y,x
        sta     y_coordinate


		// ----------------------------------------------
        // Compute upper bound of sprite coverage
        //   top = current Y − sprite vertical extent − 1
        //   Subtracts the actor’s maximum upward offset to get top row
		// ----------------------------------------------
        lda     y_coordinate
        sec                                 // prepare for subtraction
        sbc     actor_sprite_y_extent,x     // subtract actor’s sprite height offset
        sta     y_top_limit                 // store preliminary top coordinate
        dec     y_top_limit                 // adjust one pixel above extent

		// ----------------------------------------------
        // Early-exit test: skip masking if actor fully below visible screen
        //   Compare top and bottom coordinates against SCREEN_Y_MAX ($91)
        //   If both are greater than the screen limit, actor is offscreen → return
		// ----------------------------------------------
        lda     #SCREEN_Y_MAX
        cmp     y_top_limit                  // check if screen limit >= top
        bcs     clamp_y_to_screen_max        // if yes, actor may be partially visible
        cmp     y_coordinate                 // else check bottom edge (current Y)
        bcs     force_top_to_0_when_y_visible // if screen limit >= Y, actor still visible
        rts                                  // both beyond limit → no visible overlap


force_top_to_0_when_y_visible:
		// ----------------------------------------------
        // Visible-bottom, offscreen-top case:
        //   Save unclamped top, then clamp top to 0 so processing starts at row 0.
		// ----------------------------------------------
        lda     y_top_limit
        sta     y_top_unclamped              // preserve pre-clamp top
        lda     #$00
        sta     y_top_limit                  // clamp top to screen origin

clamp_y_to_screen_max:
		// ----------------------------------------------
        // Clamp bottom edge to the visible limit:
        //   If y > SCREEN_Y_MAX, set y := SCREEN_Y_MAX; else leave as-is.
		// ----------------------------------------------
        lda     #SCREEN_Y_MAX
        cmp     y_coordinate          			// is limit ≥ y?
        bcs     y_clamp_done                  	// yes → already within limit
        lda     #SCREEN_Y_MAX
        sta     y_coordinate                   	// no  → clamp to limit
		
y_clamp_done:
		// ----------------------------------------------
        // Resolve this actor’s sprite graphics base and cache it (lo/hi)
		// ----------------------------------------------
        ldx     actor                        // X := actor index
        ldy     actor_sprite_index,x         // Y := sprite ID for this actor
        jsr     set_actor_sprite_base        // sets actor_sprite_base (lo/hi)
        lda     actor_sprite_base
        sta     sprite_base_cached           // cache base low byte
        lda     actor_sprite_base + 1
        sta     sprite_base_cached + 1       // cache base high byte

		// ----------------------------------------------
        // Select mask row for this scanline: row_index = y >> 3
		// ----------------------------------------------
        lda     y_coordinate
        lsr                                 // y >> 1
        lsr                                 // y >> 2
        lsr                                 // y >> 3 (8-pixel tile rows)
        tax                                 // X := row_index
		
		// ----------------------------------------------
        // Compute absolute pointer to current mask-layer row: mask_row_ptr = mask_base_ptr + mask_row_ofs[X]
		// ----------------------------------------------
        clc
        lda     mask_row_ofs_lo,x            	// A := row offset (lo)
        adc     mask_base_ptr               	// add base lo
        sta     mask_row_ptr                	// store row ptr lo
        lda     mask_row_ofs_hi,x            	// A := row offset (hi)
        adc     mask_base_ptr + 1               // add base hi + carry
        sta     mask_row_ptr + 1                // store row ptr hi

		// ----------------------------------------------
        // Initialize tile-column cursor and build three pattern pointers for this row
		// ----------------------------------------------
        lda     actor_tile_x_coordinate      // A := starting tile column
        sta     mask_col_idx                 // seed column index used by the builder
        jsr     build_mask_pattern_ptrs      // compute mask_ptr1..3 for columns col..col+2

compute_row_ptrs:
		// ----------------------------------------------
        // Termination check for scanline loop: stop when current Y reaches top
		// ----------------------------------------------
        ldx     y_coordinate                 // X := current scanline (pixels)
        cpx     y_top_limit                  // compare against upper bound
        beq     exit_mask_actor                        // equal → no more rows to process


		// ----------------------------------------------
        // Point to this scanline’s 3-byte sprite row:
        //   sprite_row_ptr = sprite_base_cached + sprite_row_offsets[y]
		// ----------------------------------------------
        lda     sprite_row_offsets_lo,x      	// A := row offset lo for scanline y
        clc
        adc     sprite_base_cached          	// add base lo
        sta     sprite_row_ptr              	// store row pointer lo
        lda     sprite_row_offsets_hi,x      	// A := row offset hi for scanline y
        adc     sprite_base_cached + 1          // add base hi + carry
        sta     sprite_row_ptr + 1              // store row pointer hi

		// ----------------------------------------------
        // If at a tile-row boundary (y & 7 == 0), step mask row and rebuild pointers
		// ----------------------------------------------
        txa
        and     #$07                        	// test low 3 bits of y
        bne     scanline_loop_apply_mask    	// not boundary → keep current mask row

		// ----------------------------------------------
        // Move to previous mask row: subtract one screen row (VIEW_COLS bytes = 40)
		// ----------------------------------------------
        lda     mask_row_ptr               		// A := row_ptr_lo
        sec                                  	// prepare borrow for 16-bit subtract
        sbc     #VIEW_COLS                   	// lo := lo - 40
        sta     mask_row_ptr
        bcs     mask_row_stepped_reindex     	// no borrow → high byte unchanged
        dec     mask_row_ptr + 1                // borrow occurred → decrement high byte


mask_row_stepped_reindex:
		// ----------------------------------------------
        // Reinitialize mask-column cursor for the new row and rebuild pattern pointers
		// ----------------------------------------------
        ldx     actor_tile_x_coordinate      // X := starting tile column for this actor
        stx     mask_col_idx                 // seed column index on the new mask row
        jsr     build_mask_pattern_ptrs      // recompute mask_ptr1..3 for columns col..col+2

scanline_loop_apply_mask:
		// ----------------------------------------------
        // Loop guard: if current scanline is above the top bound, stop
		// ----------------------------------------------
        ldx     y_coordinate                 	// X := current scanline (pixels)
        cpx     y_top_limit                  	// compare against upper limit
        bcc     exit_mask_actor                 // y < top → no more rows to process


		// ----------------------------------------------
        // Advance to previous scanline and derive row index within the 8-pixel tile
		// ----------------------------------------------
        dex                                 // y := y - 1 (one scanline up)
        stx     y_coordinate                // store updated scanline
        txa                                 // A := y
        and     #$07                        // A := y & 7 → 0..7 within tile row
        sta     tile_row_idx                // cache tile-relative row index

		// ----------------------------------------------
        // Initialize per-scanline byte cursor, then mask the three sprite bytes
		// ----------------------------------------------
        ldy     #$00                        // sprite_byte_idx := 0
        sty     sprite_byte_idx             // select first of 3 bytes in the row
        jsr     apply_mask                  // apply foreground mask to this scanline

		// ----------------------------------------------
        // Iterate: jump back to compute pointers for the next scanline
		// ----------------------------------------------
        jmp     compute_row_ptrs

exit_mask_actor:
        rts                                 		
/*
==============================================================================
  build_mask_pattern_ptrs
==============================================================================
Summary
	Build three 16-bit pointers to 8-byte mask pattern rows for the three
	sprite bytes that overlap the current tile columns on this mask row.

Arguments
	mask_col_idx		Tile-column cursor for the current mask row (increments as used).
	mask_row_ptr		Pointer to the current mask-layer row (indexed by Y).

Global Outputs
	mask_col_idx		Advanced three times (one per column).
	mask_ptr1			Pointer to pattern row for column 1.
	mask_ptr2			Pointer to pattern row for column 2.
	mask_ptr3			Pointer to pattern row for column 3.

Description
	- Read mask index byte at mask_row_ptr[mask_col_idx], then ++mask_col_idx.
	- Compute byte offset = index * 8 + 4:
		* Use three rounds of 16-bit left shift over X:A to emulate ×8.
		* Add a 4-byte table header skip with carry into X if needed.
	- Form absolute pointer = mask_bit_patterns_base + offset.
	- Store pointer to mask_ptr1/2/3 for the three adjacent columns.

==============================================================================

Emulating 16-bit shift A <<=1 with carry into X (hi:lo)

This register dance simulates an absent "rol x" by routing the ASL carry from A
into X, yielding a 16-bit left shift over the pair X:A (X=high, A=low).

Sequence
	asl a   ; A <<= 1, C := old A7
	tay     ; Y := A (stash post-shift A)
	txa     ; A := X (bring high byte into A)
	rol a   ; A := (A<<1)|C  → X := (X<<1)|old A7
	tax     ; X := A (commit shifted high byte)
	tya     ; A := Y (restore shifted low byte)

Net effect per round
	Before: C=?, X=xxxx_xxxx, A=aaaa_aaaa
	After : C:=X7, X:=xxxx_xxxa7, A:=aaa a_aaa0
	(bit7 of pre-shift A becomes bit0 of X; A low shifts in 0)
	Result is a 16-bit value in X:A. One round = multiply by 2.

Repeat counts
	Run 1 time → ×2
	Run 3 times → ×8 (used to index 8-byte entries)

==============================================================================
*/
* = $3199
build_mask_pattern_ptrs:
		// ----------------------------------------------
        // Column 1: fetch mask index from the current mask-layer row
        //   Y = current tile-column index
        //   A = byte value (mask index) from that column on this row
		// ----------------------------------------------
        ldy     mask_col_idx
        inc     mask_col_idx                   // advance to next column for next fetch
        lda     (mask_row_ptr),y               // read mask index byte from row_ptr[Y]


		// ----------------------------------------------
		// 16-bit: (x:a) = a * MASK_BYTES_PER_TILE (×8 via three rounds)
        // Round 1 of 3: propagate A’s carry into X so (X:A) <<= 1
		// ----------------------------------------------
        ldx     #$00                    // X := 0 (high byte starts at zero)
        asl                             // A := A << 1, C := old A7
        tay                             // Y := shifted A (stash low byte)
        txa                             // A := X (bring high byte into A)
        rol                             // A := (A << 1) | C  → new high byte with carry-in
        tax                             // X := A (commit updated high byte)
        tya                             // A := Y (restore shifted low byte)
		
		// round 2 of 3: (X:A) <<= 1 with carry propagation
        asl     
		tay
		txa
		rol     
		tax
		tya
		
		// round 3 of 3: (X:A) <<= 1 with carry propagation
		asl     
		tay
		txa
		rol     
		tax
		tya

		// ----------------------------------------------
        // Add table header skip to 16-bit offset in X:A
        //   A = low byte, X = high byte
        //   offset := offset + MASK_BLOCK_HDR_SIZE
		// ----------------------------------------------
        clc                                 // clear carry for 16-bit add
        adc     #MASK_BLOCK_HDR_SIZE        // A := A + const, C=carry out
        bcc     store_ptr1                  // if no carry, high byte unchanged
        inx                                 // else carry → bump high byte (X := X+1)


store_ptr1:
		// ----------------------------------------------
        // Form pointer #1: base (mask_bit_patterns) + 16-bit offset in X:A
		// ----------------------------------------------
        clc                                 // prepare 16-bit add on A(low), X(high)
        adc     mask_bit_patterns_lo        // A := A + base_lo
        sta     mask_ptr1                   // store low byte
        txa                                 // A := X (bring high byte)
        adc     mask_bit_patterns_hi        // A := X + base_hi + carry
        sta     mask_ptr1 + 1               // store high byte


		// ----------------------------------------------
		// Repeat for column #2
		// ----------------------------------------------
		ldy     mask_col_idx
		inc     mask_col_idx
		lda     (mask_row_ptr),y

		ldx     #$00
		asl     
		tay
		txa
		rol     
		tax
		tya
		asl     
		tay
		txa
		rol     
		tax
		tya
		asl     
		tay
		txa
		rol     
		tax
		tya

		clc
		adc     #MASK_BLOCK_HDR_SIZE
		bcc     store_ptr2
		inx


store_ptr2:
		clc
		adc     mask_bit_patterns_lo
		sta     mask_ptr2
		txa
		adc     mask_bit_patterns_hi
		sta     mask_ptr2 + 1


		// ----------------------------------------------
		// Repeat for column #3
		// ----------------------------------------------
		ldy     mask_col_idx
		inc     mask_col_idx
		lda     (mask_row_ptr),y

		ldx     #$00
		asl     
		tay
		txa
		rol     
		tax
		tya
		asl     
		tay
		txa
		rol     
		tax
		tya
		asl     
		tay
		txa
		rol     
		tax
		tya

		clc
		adc     #MASK_BLOCK_HDR_SIZE
		bcc     store_ptr3
		inx


store_ptr3:
		clc
		adc     mask_bit_patterns_lo
		sta     mask_ptr3
		txa
		adc     mask_bit_patterns_hi
		sta     mask_ptr3 + 1
		rts

/*
==============================================================================
  apply_mask
==============================================================================
Summary
	Apply the foreground mask to one sprite row across its three bytes.
	Clears sprite pixels where the mask is solid so the actor renders behind
	foreground tiles.

Arguments
	tile_row_idx                0..7 row within an 8×8 tile
	mask_ptr1..mask_ptr3        pointers to per-column mask-index rows
	sprite_row_ptr              pointer to the actor’s sprite row (3 bytes)

Global Inputs
	global_mask_patterns_ptr    base pointer to 8-byte patterns per index

Description
	- For each of the three adjacent tile columns:
		* Fetch mask index for this tile row, then fetch the pattern byte.
		* Duplicate even bits into odd neighbors (pair each pixel's bit).
		* AND the result into the corresponding sprite byte.

==============================================================================

Bit duplication for C64 multicolor ("fat") sprites

C64 multicolor sprites display fat pixels: each visible pixel uses 2 adjacent
bits instead of 1. The VIC-II interprets bits in pairs (b2k, b2k+1) as one
color index (00,01,10,11). This halves horizontal resolution (12 pixels wide
instead of 24) but doubles pixel width.

The mask patterns here are generated in single-bit form, one bit per logical
pixel column. To apply them to multicolor sprites, each logical bit must cover
two adjacent hardware bits—duplicating each even-position bit into its odd
neighbor.

Instruction sequence:
	sta pixel_mask        	; save raw 8-bit pattern (P)
	and #MASK_EVEN_BITS     ; keep even bits b0,b2,b4,b6 → isolate logical pixels
	asl                     ; shift those bits into odd positions b1,b3,b5,b7
	ora pixel_mask        	; combine with original → duplicate each even bit

Result:
	Each pair (b2k,b2k+1) now holds identical values.
	If a logical pixel is masked (0), both bits become 0.
	If visible (1), both bits become 1.
	This ensures the mask aligns with the doubled-width multicolor pixels.

Without duplication, only half of the visual pixel pairs would be masked,
producing artifacts. This duplication step maintains consistent coverage for
fat-pixel multicolor sprite data.

==============================================================================
*/
* = $3224
apply_mask:
		// ----------------------------------------------
        // Column 1: fetch tile-row mask index and build bitmask for this row
		// ----------------------------------------------
        ldy     tile_row_idx
        lda     (mask_ptr1),y               	// A = mask index for tile_row_idx
        tay                                   	// Y = index into pattern table
        lda     (global_mask_patterns_ptr),y    // A = 8-bit pattern byte
        sta     pixel_mask                		// save raw pattern
        and     #MASK_EVEN_BITS              	// isolate even pixel bits (0,2,4,6)
        asl                                   	// move them into odd positions (1,3,5,7)
        ora     pixel_mask                		// duplicate each even bit into its paired odd bit
		
		// ----------------------------------------------
        // Apply mask to this sprite byte and advance to the next
		// ----------------------------------------------
        ldy     sprite_byte_idx              	// Y := 0/1/2 selects sprite_row_ptr[Y]
        and     (sprite_row_ptr),y           	// A := A & sprite_row_ptr[Y]
        sta     (sprite_row_ptr),y           	// write masked byte back to sprite row
        inc     sprite_byte_idx              	// move to next sprite byte (0→1→2)


		// ----------------------------------------------
		// Column 2: repeat for sprite byte 1
		// ----------------------------------------------
		ldy     tile_row_idx
		lda     (mask_ptr2),y
		tay
		lda     (global_mask_patterns_ptr),y
		sta     pixel_mask
		and     #MASK_EVEN_BITS
		asl     
		ora     pixel_mask
		ldy     sprite_byte_idx
		and     (sprite_row_ptr),y
		sta     (sprite_row_ptr),y
		inc     sprite_byte_idx

		// ----------------------------------------------
		// Column 3: repeat for sprite byte 2
		// ----------------------------------------------
		ldy     tile_row_idx
		lda     (mask_ptr3),y
		tay
		lda     (global_mask_patterns_ptr),y
		sta     pixel_mask
		and     #MASK_EVEN_BITS
		asl     
		ora     pixel_mask
		ldy     sprite_byte_idx
		and     (sprite_row_ptr),y
		sta     (sprite_row_ptr),y
		rts


/* 

procedure mask_actor_with_foreground_layer(actor, actor_tile_x_coordinate):
    # 1) Compute vertical coverage of this actor’s sprite
    y_bottom = character_sprite_y[actor]                 # working bottom Y
    extent   = actor_sprite_y_extent[actor]

    # Highest scanline the sprite can affect (just above top-most pixel)
    y_top = y_bottom - extent - 1

    # 2) Quick reject if entirely below the visible window
    if SCREEN_Y_MAX < y_top and SCREEN_Y_MAX < y_bottom:
        return    # sprite is fully below the screen

    # 3) If top is below screen but bottom is visible, clamp top to 0
    # (and remember original unclamped value)
    if SCREEN_Y_MAX < y_top and SCREEN_Y_MAX >= y_bottom:
        y_top_unclamped = y_top
        y_top = 0

    # 4) Clamp bottom to SCREEN_Y_MAX if needed
    if y_bottom > SCREEN_Y_MAX:
        y_bottom = SCREEN_Y_MAX

    # If, after clamping, nothing remains, bail out
    if y_bottom <= y_top:
        return

    # 5) Resolve and cache sprite base pointer for this actor
    sprite_index = actor_sprite_index[actor]
    sprite_base  = set_actor_sprite_base(sprite_index)   # returns base pointer
    sprite_base_cached = sprite_base

    # 6) Initialize mask row pointer for the starting scanline
    tile_row_index = y_bottom // 8
    mask_row_ptr   = mask_base_ptr + mask_row_ofs[tile_row_index]

    # 7) Build per-column mask pattern pointers for this tile row
    mask_col_idx = actor_tile_x_coordinate
    build_mask_pattern_ptrs()

    # 8) Main vertical sweep: from bottom up to (and including) y_top
    y = y_bottom
    while y > y_top:
        # Compute sprite row pointer for this scanline
        sprite_row_ptr = sprite_base_cached + sprite_row_offsets[y]

        # If we crossed a tile-row boundary, move to previous mask row and rebuild pointers
        # (conceptually: when integer_div(y,8) changes)
        current_tile_row = y // 8
        if current_tile_row != tile_row_index:
            tile_row_index = current_tile_row
            mask_row_ptr   = mask_base_ptr + mask_row_ofs[tile_row_index]
            mask_col_idx   = actor_tile_x_coordinate
            build_mask_pattern_ptrs()

        # Move to the next scanline upward and compute its tile-local row index
        y = y - 1
        tile_row_idx  = y & 7
        sprite_byte_idx = 0   # we always mask 3 bytes per row, starting at 0

        # Apply foreground mask to the three sprite bytes on this scanline
        apply_mask()

    return


procedure build_mask_pattern_ptrs():
    # We will build mask_ptr1, mask_ptr2, mask_ptr3
    # Each is an 8-byte pattern block for a given mask index.

    # Column 1
    index1 = mask_row_ptr[mask_col_idx]      # read mask index at current column
    mask_col_idx += 1
    offset1 = MASK_BLOCK_HDR_SIZE + index1 * 8
    mask_ptr1 = mask_bit_patterns_base + offset1

    # Column 2
    index2 = mask_row_ptr[mask_col_idx]
    mask_col_idx += 1
    offset2 = MASK_BLOCK_HDR_SIZE + index2 * 8
    mask_ptr2 = mask_bit_patterns_base + offset2

    # Column 3
    index3 = mask_row_ptr[mask_col_idx]
    mask_col_idx += 1
    offset3 = MASK_BLOCK_HDR_SIZE + index3 * 8
    mask_ptr3 = mask_bit_patterns_base + offset3


procedure apply_mask():
    # For readability, treat mask_ptr[0..2] and sprite byte indices 0..2.
    # tile_row_idx: which of the 8 rows inside the mask tile we’re on.
    # sprite_row_ptr: points at the 3-byte multicolor sprite row.
    # global_mask_patterns: table of 1-bit-per-pixel mask bytes.

    # Column 0 → sprite byte 0
    pattern_index = mask_ptr1[tile_row_idx]     # select pattern row for this tile row
    logical_mask  = global_mask_patterns[pattern_index]  # 1 bit per logical pixel

    # Expand logical mask to multicolor mask (duplicate each bit into its neighbor)
    multicolor_mask = expand_to_multicolor(logical_mask)

    # AND into sprite byte 0
    sprite_byte = sprite_row_ptr[0]
    sprite_row_ptr[0] = sprite_byte & multicolor_mask

    # Column 1 → sprite byte 1
    pattern_index = mask_ptr2[tile_row_idx]
    logical_mask  = global_mask_patterns[pattern_index]
    multicolor_mask = expand_to_multicolor(logical_mask)

    sprite_byte = sprite_row_ptr[1]
    sprite_row_ptr[1] = sprite_byte & multicolor_mask

    # Column 2 → sprite byte 2
    pattern_index = mask_ptr3[tile_row_idx]
    logical_mask  = global_mask_patterns[pattern_index]
    multicolor_mask = expand_to_multicolor(logical_mask)

    sprite_byte = sprite_row_ptr[2]
    sprite_row_ptr[2] = sprite_byte & multicolor_mask


function expand_to_multicolor(logical_mask_byte) -> byte:
    # logical_mask_byte: 1 bit per logical pixel (bit0 for pixel0, bit1 for pixel1, etc.)
    # Output: 2 bits per pixel (C64 multicolor), where each 1-bit becomes 11b in its 2-bit pair.

    base = logical_mask_byte
    even_bits = base & MASK_EVEN_BITS          # MASK_EVEN_BITS = %01010101 (even bit positions)
    shifted   = even_bits << 1                 # move them into odd positions
    return base | shifted                      # duplicate even bits into neighbors


==============================================================================

Text diagrams of bottom) and top vs. screen [0..SCREEN_TOP].
[] = sprite span before clamp. <> = span after clamp. X = culled.

------------------------------
1. Fully below screen → culled
------------------------------


0                                    SCREEN_TOP
|------------------------------------|
                                      [ top ........ bottom ]
Result: X  (both top and bottom > SCREEN_TOP)


------------------------------
2. Bottom clipped to SCREEN_TOP
------------------------------


0                                    SCREEN_TOP
|------------------------------------|
                   [ top ................. bottom ]
                   < top ..SCREEN_TOP>
Action: bottom := SCREEN_TOP


------------------------------
3. Fully visible (no clamp)
------------------------------


0                                    SCREEN_TOP
|------------------------------------|
            [ top ........ bottom ]
            < top ........ bottom >
Action: none


------------------------------
4. Top clipped to 0
------------------------------


	0                                    SCREEN_TOP
	|------------------------------------|
 [  top .................. bottom ]
    < 0 .................. bottom >
Action: top := 0


------------------------------
5. Fully above screen → culled
------------------------------


					0                                    SCREEN_TOP
					|------------------------------------|
[ top .... bottom ]
Result: X  (both top and y < 0)


*/