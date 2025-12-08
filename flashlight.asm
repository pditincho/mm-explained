/*
================================================================================
Flashlight window system
================================================================================

Summary
	This module renders a small “flashlight” window into the active frame buffer,
	either clearing the region to darkness or revealing pixels from the off-screen
	buffer. It converts beam coordinates into a clamped 4×6 character window and
	uses a self-modifying blit loop for per-frame updates.

Description
	- Maintains a pair of pointers (flashlight_src / flashlight_dest)
	  that track matching source/destination row bases in the two frame buffers.	  
	- Normalizes incoming beam coordinates (X,Y) so they represent the top-left
	  corner of the flashlight window, including a raster→row conversion for Y.
	- Clamps the window origin so the 4×6 region always stays within the visible
	  40×25 character grid, using bounds chosen to avoid table overruns.
	- Chooses source/destination frame bases based on frame_buffer, treating the
	  “current” buffer as the draw target and the “other” buffer as the light
	  source to copy from.
	- Computes per-row start addresses via screen_row_offsets[Y] and patches the
	  operand bytes of absolute,X LDA/STA instructions so the inner loop runs
	  with minimal per-pixel address arithmetic.
	- Implements a nested Y/X loop over a fixed 4×6 window: in copy mode it
	  copies bytes from the source frame; in clear mode it writes zeros, thus
	  carving a lit window out of an otherwise darkened scene.
	  
================================================================================
*/
#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"

.label mode                  = $1b      // Flashlight op mode latch; set on entry. Values: FLASH_MODE_CLEAR / FLASH_MODE_COPY
.label flashlight_dest       = $17      // ZP dest pointer (lo=$17, hi=$18); current on-screen frame base + row/col offset
.label flashlight_src        = $19      // ZP src pointer  (lo=$19, hi=$1A); off-screen frame base + row/col offset
.label inlined_src_addr      = $332e    // Self-modified absolute for inner-loop LDA $FFFF,X; patched each row to src row base
.label inlined_dest_addr     = $3331    // Self-modified absolute for inner-loop STA $FFFF,X; patched each row to dest row base

.const  FLASH_MODE_CLEAR     = $00      // Mode: clear window to darkness
.const  FLASH_MODE_COPY      = $01      // Mode: copy lit pixels from src to dest

.const  SCREEN_COLS_PER_ROW  = $0028    // 40 columns per C64 text row (stride added when advancing one row) (16-bit)

.const  FLASH_X_CLAMP_MIN    = $00      // Lowest allowed X (keeps window fully on-screen)
.const  FLASH_X_CLAMP_MAX    = $22      // Highest allowed X (X..X+5 ≤ 39 given 6-column window)
.const  FLASH_Y_CLAMP_MIN    = $01      // Lowest allowed Y (protects row-offset table access)
.const  FLASH_Y_CLAMP_MAX    = $0E      // Highest allowed Y (fits 4-row window within 25 text rows)

.const  FLASH_WIN_WIDTH      = $06      // Window width in columns
.const  FLASH_WIN_HEIGHT     = $04      // Window height in rows
.const  FLASH_WIN_LAST_COL_IDX = FLASH_WIN_WIDTH - 1   // Inner-loop last X index (used with LDX #$05)
.const  FLASH_WIN_LAST_ROW_IDX = FLASH_WIN_HEIGHT - 1  // Outer-loop last Y index (used with LDY #$03)

.const  FLASH_NORMALIZE_OFFSET = $03     // Offset subtracted from scaled inputs to anchor top-left near beam center

// ------------------------------------------------------------
// Clear the current flashlight window region.
// Loads beam coordinates and calls update_flashlight_window
// in FLASH_MODE_CLEAR mode (#$00).
// ------------------------------------------------------------
* = $3273
clear_flashlight_area:
        ldx     flashlight_beam_x
        ldy     flashlight_beam_y
        lda     #FLASH_MODE_CLEAR
        jsr     update_flashlight_window
        rts

// ------------------------------------------------------------
// Draw or refresh the current flashlight window region.
// Loads beam coordinates and calls update_flashlight_window
// in FLASH_MODE_COPY mode (#$01).
// ------------------------------------------------------------
* = $327F
draw_flashlight_area:
        ldx     flashlight_beam_x
        ldy     flashlight_beam_y
        lda     #FLASH_MODE_COPY
        jsr     update_flashlight_window
        rts

/*
================================================================================
  update_flashlight_window
================================================================================
Summary
	Copy or clear a 4×6 “flashlight” window in video RAM by selecting source and
	destination frame bases, clamping the window origin, and running a tight
	blit loop using self-modified absolute,X operands.

Arguments
	.A  Operation mode:
		- FLASH_MODE_CLEAR (#$00) → write zeros to destination window
		- FLASH_MODE_COPY  (#$01) → copy from source to destination
	.X  Beam X in character columns (routine subtracts FLASH_NORMALIZE_OFFSET)
	.Y  Beam Y in raster-derived units (routine applies Y >> 2, then subtracts
	FLASH_NORMALIZE_OFFSET)

Global Inputs
	frame_buffer                  selects dest/src base pairing

Global Outputs
	Video RAM at window region    4×6 block updated in current destination

Description
	- Latch mode from .A into mode.
	- Normalize coordinates:
		• Y := (Y >> 2) − FLASH_NORMALIZE_OFFSET, then clamp to
		[FLASH_Y_CLAMP_MIN .. FLASH_Y_CLAMP_MAX].
		• X := X − FLASH_NORMALIZE_OFFSET, then clamp to
		[FLASH_X_CLAMP_MIN .. FLASH_X_CLAMP_MAX].
	- Choose src/dst bases from frame_buffer:
		• fb==1 → dest=FRAMEBUF1_BASE (C800), src=FRAMEBUF2_BASE (CC00)
		• else  → dest=FRAMEBUF2_BASE (CC00), src=FRAMEBUF1_BASE (C800)
	- Form per-row base addresses by adding screen_row_offsets[ Y ] to each
	base, then self-modify absolute,X operands at inlined_* to those bases.
	- Add X column to both patched addresses.
	- For each of 4 rows:
		• If mode==FLASH_MODE_COPY: LDA (src_row_base),X → STA (dst_row_base),X
		• Else: LDA #$00 → STA (dst_row_base),X
		• Advance both patched addresses by SCREEN_COLS_PER_ROW (40).

Notes
	- Window dimensions: FLASH_WIN_WIDTH × FLASH_WIN_HEIGHT = 6 × 4.
	- X high clamp ensures X..X+5 ≤ 39. Row stride is 40 columns.
================================================================================
*/
* = $328B
update_flashlight_window:
		// Latch mode
        sta     mode

        // ----------------------------------------------------
        // Normalize coordinates
		//
        //  - Y := (Y >> 2) - 3
        //  - X := X - 3
        // ----------------------------------------------------
        tya
        lsr
        lsr
        sec
        sbc     #FLASH_NORMALIZE_OFFSET
        tay

        txa
        sec
        sbc     #FLASH_NORMALIZE_OFFSET
        tax

        // ----------------------------------------------------
        // Choose src/dst for current frame buffer
        // frame_buffer == #$01 → dst=C800, src=CC00
        // else                 → dst=CC00, src=C800
        // ----------------------------------------------------
        lda     frame_buffer
        cmp     #$01
        bne     frame_buffer_02

frame_buffer_01:
        lda     #<FRAMEBUF1_BASE
        sta     flashlight_dest
        lda     #>FRAMEBUF1_BASE
        sta     flashlight_dest + 1
        lda     #<FRAMEBUF2_BASE
        sta     flashlight_src
        lda     #>FRAMEBUF2_BASE
        sta     flashlight_src + 1
        jmp     clamp_x_low

frame_buffer_02:
        lda     #<FRAMEBUF1_BASE
        sta     flashlight_src
        lda     #>FRAMEBUF1_BASE
        sta     flashlight_src + 1
        lda     #<FRAMEBUF2_BASE
        sta     flashlight_dest
        lda     #>FRAMEBUF2_BASE
        sta     flashlight_dest + 1

        // ------------------------------------------------------------
        // Clamp beam coordinates to valid window bounds
		//
        // - X high clamp guarantees the 6-column window stays within 40 columns.
        // - Y clamp uses a legacy path that depends on V; end state matches
        //   Y ∈ [FLASH_Y_CLAMP_MIN..FLASH_Y_CLAMP_MAX] but logic is brittle.
        // ------------------------------------------------------------
clamp_x_low:
        // X clamp low → 0
        cpx     #FLASH_X_CLAMP_MIN
        bpl     clamp_x_high
        ldx     #FLASH_X_CLAMP_MIN

clamp_x_high:
        // X clamp high → $22, ensures 6-wide block stays within 40 cols
        cpx     #FLASH_X_CLAMP_MAX
        bcc     clamp_y_entry
        beq     clamp_y_entry
        ldx     #FLASH_X_CLAMP_MAX

clamp_y_entry:
        // Y lower-bound path. As written, this sequence relies on V.
        // It ultimately forces Y := #$01 when taken.
        cpy     #FLASH_Y_CLAMP_MIN
        bvs     branch_y_sign_path
        bpl     clamp_y_high
        bmi     clamp_y_low

branch_y_sign_path:
        bmi     clamp_y_high

clamp_y_low:
        ldy     #FLASH_Y_CLAMP_MIN

clamp_y_high:
        // Y clamp high → #$0E (ensures 4-row block stays within 25 rows)
        cpy     #FLASH_Y_CLAMP_MAX
        bcc     patch_row_base_addresses
        beq     patch_row_base_addresses
        ldy     #FLASH_Y_CLAMP_MAX

		// ------------------------------------------------------------
		// Patch absolute,X operands for src/dest at row base + column
		// ------------------------------------------------------------
patch_row_base_addresses:
        lda     screen_row_offsets_lo,y
        clc
        adc     flashlight_dest
        sta     inlined_dest_addr
        lda     screen_row_offsets_hi,y
        adc     flashlight_dest + 1
        sta     inlined_dest_addr + 1

        lda     screen_row_offsets_lo,y
        clc
        adc     flashlight_src
        sta     inlined_src_addr
        lda     screen_row_offsets_hi,y
        adc     flashlight_src + 1
        sta     inlined_src_addr + 1

        // Add X column to both patched pointers
        txa
        clc
        adc     inlined_src_addr
        sta     inlined_src_addr
        bcc     add_x_to_dest_base
        inc     inlined_src_addr + 1

add_x_to_dest_base:
        txa
        clc
        adc     inlined_dest_addr
        sta     inlined_dest_addr
        bcc     blit_window_rows
        inc     inlined_dest_addr + 1

		// ------------------------------------------------------------
		// Copy or clear a 4×6 flashlight window.
		//
		// Y-loop: 4 rows (Y:=3..0). X-loop: 6 cols (X:=5..0).
		// In copy mode:  LDA (src_row_base),X → STA (dst_row_base),X
		// In clear mode: LDA #$00 → STA (dst_row_base),X
		// Row stride: +$28 (40 columns) on both pointers.
		// ------------------------------------------------------------
blit_window_rows:
        ldy     #FLASH_WIN_LAST_ROW_IDX

blit_row_loop:
        ldx     #FLASH_WIN_LAST_COL_IDX

blit_col_loop:
        lda     mode
        bne     blit_copy_mode

        // Clear mode
        lda     #$00
        jmp     blit_store_to_dst

        // Self-modified absolute addresses below:
        //   inlined_src_addr  at $332e/$332f for LDA $FFFF,X
        //   inlined_dest_addr at $3331/$3332 for STA $FFFF,X
blit_copy_mode:
        lda     $FFFF,x        // LDA inlined_src_addr,X

blit_store_to_dst:
        sta     $FFFF,x        // STA inlined_dest_addr,X

        // Next column
        dex
        bpl     blit_col_loop

        // Next row: advance both src and dest by 40 columns
        clc
        lda     inlined_src_addr
        adc     #<SCREEN_COLS_PER_ROW
        sta     inlined_src_addr
        lda     inlined_src_addr + 1
        adc     #>SCREEN_COLS_PER_ROW
        sta     inlined_src_addr + 1

        clc
        lda     inlined_dest_addr
        adc     #<SCREEN_COLS_PER_ROW
        sta     inlined_dest_addr
        lda     inlined_dest_addr + 1
        adc     #>SCREEN_COLS_PER_ROW
        sta     inlined_dest_addr + 1

        // Row countdown
        dey
        bpl     blit_row_loop

        rts

/*
function clear_flashlight_area():
    // Read current beam position (in whatever units the engine uses)
    beamX = flashlight_beam_x
    beamY = flashlight_beam_y

    mode = FLASH_MODE_CLEAR   // clear window to darkness

    update_flashlight_window(mode, beamX, beamY)


function draw_flashlight_area():
    beamX = flashlight_beam_x
    beamY = flashlight_beam_y

    mode = FLASH_MODE_COPY    // copy lit pixels from source frame

    update_flashlight_window(mode, beamX, beamY)


function update_flashlight_window(mode, beamX, beamY):
    // ----------------------------------------------------------------
    // Step 1: normalize beam coordinates to window origin
    // ----------------------------------------------------------------
    // Convert Y from raster-ish units to row index
    normalizedY = (beamY >> 2) - FLASH_NORMALIZE_OFFSET
    normalizedX = beamX - FLASH_NORMALIZE_OFFSET

    // ----------------------------------------------------------------
    // Step 2: choose frame buffer pairing
    // ----------------------------------------------------------------
    if frame_buffer == 1:
        // Current visible frame is buffer 1; draw into it, read from 2
        destBase = FRAMEBUF1_BASE
        srcBase  = FRAMEBUF2_BASE
    else:
        // Current visible frame is buffer 2; draw into it, read from 1
        destBase = FRAMEBUF2_BASE
        srcBase  = FRAMEBUF1_BASE

    // ----------------------------------------------------------------
    // Step 3: clamp window origin so the 4×6 window stays on-screen
    // ----------------------------------------------------------------
    // Horizontal clamp: ensure [X .. X + (FLASH_WIN_WIDTH - 1)] is inside row
    if normalizedX < FLASH_X_CLAMP_MIN:
        normalizedX = FLASH_X_CLAMP_MIN
    else if normalizedX > FLASH_X_CLAMP_MAX:
        normalizedX = FLASH_X_CLAMP_MAX

    // Vertical clamp: ensure [Y .. Y + (FLASH_WIN_HEIGHT - 1)] is inside screen.
    // The real code uses a slightly odd signed/V-flag path; final effect is:
    if normalizedY < FLASH_Y_CLAMP_MIN:
        normalizedY = FLASH_Y_CLAMP_MIN
    else if normalizedY > FLASH_Y_CLAMP_MAX:
        normalizedY = FLASH_Y_CLAMP_MAX

    // ----------------------------------------------------------------
    // Step 4: compute starting addresses for top row
    // ----------------------------------------------------------------
    // screen_row_offsets[Y] gives the row’s byte offset from the buffer base.
    rowOffset = screen_row_offsets[normalizedY]

    // Top-left cell of the flashlight window in video RAM
    destRowBase = destBase + rowOffset + normalizedX
    srcRowBase  = srcBase  + rowOffset + normalizedX

    // ----------------------------------------------------------------
    // Step 5: blit the 4×6 window
    // ----------------------------------------------------------------
    for row = 0 to FLASH_WIN_HEIGHT - 1:
        // For each row, copy/clear FLASH_WIN_WIDTH consecutive bytes
        for col = 0 to FLASH_WIN_WIDTH - 1:
            if mode == FLASH_MODE_COPY:
                value = readByte(srcRowBase + col)
            else:  // FLASH_MODE_CLEAR
                value = 0

            writeByte(destRowBase + col, value)

        // Advance to next text row in both buffers (stride = SCREEN_COLS_PER_ROW)
        destRowBase += SCREEN_COLS_PER_ROW
        srcRowBase  += SCREEN_COLS_PER_ROW

*/