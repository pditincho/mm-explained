/*
================================================================================
Render actor - Overview

1. Prepare all pointers (costume, VRAM base, decode tables).
2. Clear only the rows we will redraw in the current bank.
3. Resolve each limb’s cel address from animation state, then blit+mask it at the current Y, updating coverage.
4. Publish per-bank Y and coverage so downstream steps and the next frame know where the sprite lives.
5. Publish sprite X and color for the PPU/VIC-II side to use this frame.

Rendering pipeline

	1. Costume preparation (setup_costume_for_actor)
		- Ensures per-actor graphics tables are current; caches offsets for cels and clips.

	2. Sprite VRAM resolution (set_actor_sprite_base)
		- Uses pre-multiplied (index*64) tables for fast pointer math.
		- Page-aligned bases at $E000 / $E800 minimize carry logic.

	3. Clearing (clear_sprite_visible_rows)
		- Computes top and bottom visible rows from per-bank buffers.
		- Zeros 3 bytes per row (multi-color sprite row width).

	4. Limb rendering (draw_actor_limbs)
		- Loops 8 limbs: computes cel pointer → calls external blit_and_mask_cel.
		- Tracks sprite_vertical_offset (max coverage).

	5. Cel pointer math (compute_cel_ptr)
		- Uses combined limb frame + base cel index to resolve into table of cel addresses.

	6. Blitting (blit_sprite_vthird)
		- Copies 7 rows per vertical third using copy_offsets lookup.
		- Writes three bytes per row (one sprite row).

	7. Commit (commit_sprite_coverage_for_bank)
		- Writes per-bank Y and offset; mirrors to actor_sprite_y_extent.
================================================================================

Core relationship: one frame = orchestrate → stage rows → per-limb blits → commit per-bank state → commit global sprite state.


How pieces depend on each other

	* setup_costume_for_actor → compute_cel_ptr
	  The costume setup populates actor_gfx_base, cel_seq_tbl, and the cel address tables’ offsets. 
	  The cel resolver relies on these to translate (actor, limb, frame) into src_cel_ptr.

	* set_actor_sprite_base → set_sprite_ptr_for_row → clear_sprite_visible_rows / blits
	  Sprite base comes from bank + sprite index. Row pointers add precomputed row offsets to that base. 
	  Both the clearer and the blitter use these row pointers to touch the correct VRAM bytes.

	* draw_actor_limbs ↔ blit_and_mask_cel ↔ update_actor_sprite_vertical_positions_for_buffer
	  Each blit reports vertical_offset. The limb loop tracks the max voffset for the actor. 
	  That max and the final current_sprite_y_pos are written into the active bank so later passes know 
	  which rows are occupied and where the sprite sits vertically.

	* Banked state couples frame N and N+1
	  sprite_voff_buf{0,1} and buffer_{00,01}_y_position_for_sprite are per-bank. 
	  Clearing, drawing, and later masking read/write the bank selected by sprite_bank_sel, enabling double-buffered sprite updates.

Key shared data

	* Geometry pointers
	  actor_sprite_base → base VRAM
	  sprite_row_offsets_* → row deltas
	  src_row_ptr/dest_row_ptr → per-row/per-cel addresses

	* Animation addressing
	  cel_seq_tbl (frame→cel id per limb), limb_frame_idx, limb_current_cel, ofs_cel_hi_tbl, gfx_base.

	* Visibility/masking
	  current_sprite_y_pos, sprite_vertical_offset, and banked Y/offset arrays coordinate clearing, blitting, and foreground masking.

================================================================================

draw_actor
 ├─ setup_costume_for_actor
 │    ↳ sets: actor_gfx_base, cel_seq_tbl, ofs_cel_hi_tbl
 │
 ├─ set_actor_sprite_base
 │    ↳ uses: sprite_bank_sel, actor_sprite_index
 │    ↳ sets: actor_sprite_base
 │
 ├─ clear_sprite_visible_rows
 │    ↳ uses: actor_sprite_base, sprite_bank_sel
 │    ↳ uses: buffer_{00,01}_y_position_for_sprite, sprite_voff_buf{0,1}
 │    ↳ sets: cleared rows in VRAM for this sprite slot
 │
 ├─ draw_actor_limbs         (loop 8 limbs)
 │    ├─ compute_cel_ptr
 │    │    ↳ uses: actor_gfx_base, cel_seq_tbl,
 │    │            actor_limb_slot_base(=actor*8), current_limb,
 │    │            limb_frame_idx, limb_current_cel,
 │    │            ofs_cel_hi_tbl
 │    │    ↳ sets: dest_row_ptr
 │    ├─ blit_and_mask_cel  (external)
 │    │    ↳ uses: dest_row_ptr, current_sprite_y_pos, flip
 │    │    ↳ sets: vertical_offset (coverage of this limb)
 │    └─ tracks max sprite_vertical_offset over limbs
 │
 ├─ update_actor_sprite_vertical_positions_for_buffer
 │    ↳ uses: sprite_bank_sel, actor_sprite_index
 │    ↳ uses: current_sprite_y_pos, sprite_vertical_offset
 │    ↳ sets: sprite_voff_buf{0,1}[slot], buffer_{00,01}_y_position_for_sprite[slot],
 │            actor_sprite_y_extent[actor]
 │
 ├─ commit X and color
 │    ↳ sets: actor_sprite_{x_hi,x_lo}[slot], sprite_0_color[slot]
 └─ return
  
================================================================================
*/

#importonce
#import "globals.inc"
#import "constants.inc"
#import "masking.asm"
#import "blit_cel.asm"

.const VISIBLE_ROW_MAX    = $91     		// last drawable row index (clipping threshold)
.const TOTAL_LIMBS        = $08		

// General vars
.label costume_base             = $17    	// costume resource base (lo/hi at $17/$18)
.label dest_row_ptr             = $7E    	// destination row pointer for blits (cel data)
.label src_row_ptr          	= $80    	// source row pointer (staged sprite row)
.label src_cel_ptr          	= $7E    	// alias: source cel pointer


// draw_actor
.label gfx_base   				= $82    	// graphics section base for active costume
.label global_mask_patterns_ptr = $8A    	// mask pattern table pointer
.label ofs_cel_lo_tbl           = $CB52  	// 8-bit offset (relative to *gfx base*) to the low-byte table of cel addresses, for the current actor - always set to $02
.label ofs_cel_hi_tbl 			= $CB53		// 8-bit offset (relative to *gfx base*) to the high-byte table of cel addresses, for the current actor

// draw_actor_limbs
.label current_limb_flip        = $FC25  	// cached flip flags for the current limb

// clear_sprite_visible_rows
.label bottom_row               = $FD53  	// clamped bottom row to clear
.label clear_row_idx            = $CB51  	// current row index during clear loop
.label sprite_voff_buf0         = $FD8D 	// per-sprite vertical coverage (bank 0)
.label sprite_voff_buf1         = $FD85  	// per-sprite vertical coverage (bank 1)
.label sprite_y_buf1            = $FD81  	// per-sprite Y position (bank 1)
.label sprite_y_buf0            = $FD89  	// per-sprite Y position (bank 0)

// compute_cel_ptr
.label offset_hi                = $FC3B  // temp: index into HI-byte cel-address table

// blit_sprite_vthird
.label vthird_row_idx           = $CB54  // loop counter 0..6 for 7-row blit
.label saved_x                  = $2501  // temp: saved X across the blit loop


* = $24FA
copy_offsets:
    // 7-entry LUT: row indices to copy for the current vertical third.
    // Used by blit routine to select 7 source rows (top/mid/bottom pass).
.byte $0B, $1F, $33, $47, $5B, $6F, $83

* = $24EA
multiple_64_lo:
    // Low bytes of (index * 64) for sprite VRAM base addressing (indices 0..7).
.byte $00, $40, $80, $C0, $00, $40, $80, $C0

* = $24F2
multiple_64_hi:
    // High bytes of (index * 64) for sprite VRAM base addressing (indices 0..7).
.byte $00, $00, $00, $00, $01, $01, $01, $01

.label sprite_row_offsets_hi = $d508   // points 8 bytes into HI table → entry i=0 maps logical row 8
.label sprite_row_offsets_lo = $d5a8   // points 8 bytes into LO table → entry i=0 maps logical row 8

* = $d5a0
    // Row byte-offset low nibbles stepping by +3 per row (lo bytes for off = 3*r).
    // Full table starts at r=0; label above starts at r=8 for engine convenience.
.byte $00, $03, $06, $09,  $0C, $0F, $12, $15,  $18, $1B, $1E, $21,  $24, $27, $2A, $2D
.byte $30, $33, $36, $39,  $3C, $00, $03, $06,  $09, $0C, $0F, $12,  $15, $18, $1B, $1E
.byte $21, $24, $27, $2A,  $2D, $30, $33, $36,  $39, $3C, $00, $03,  $06, $09, $0C, $0F
.byte $12, $15, $18, $1B,  $1E, $21, $24, $27,  $2A, $2D, $30, $33,  $36, $39, $3C, $00
.byte $03, $06, $09, $0C,  $0F, $12, $15, $18,  $1B, $1E, $21, $24,  $27, $2A, $2D, $30
.byte $33, $36, $39, $3C,  $00, $03, $06, $09,  $0C, $0F, $12, $15,  $18, $1B, $1E, $21
.byte $24, $27, $2A, $2D,  $30, $33, $36, $39,  $3C, $00, $03, $06,  $09, $0C, $0F, $12
.byte $15, $18, $1B, $1E,  $21, $24, $27, $2A,  $2D, $30, $33, $36,  $39, $3C, $00, $03
.byte $06, $09, $0C, $0F,  $12, $15, $18, $1B,  $1E, $21, $24, $27,  $2A, $2D, $30, $33
.byte $36, $39, $3C, $00,  $03, $06, $09, $0C,  $0F, $12, $15, $18,  $1B, $1E, $21, $24

* = $d500
    // Row byte-offset high bytes (hi bytes for off = 3*r), carrying page crossings.
    // Full table starts at r=0; label above starts at r=8 for engine convenience.
.byte $00, $00, $00, $00,  $00, $00, $00, $00,  $00, $00, $00, $00,  $00, $00, $00, $00
.byte $00, $00, $00, $01,  $01, $01, $01, $01,  $01, $01, $01, $01,  $01, $01, $01, $01
.byte $01, $01, $01, $01,  $01, $01, $01, $02,  $02, $02, $02, $02,  $02, $02, $02, $02
.byte $02, $02, $02, $02,  $02, $02, $02, $02,  $02, $02, $02, $03,  $03, $03, $03, $03
.byte $03, $03, $03, $03,  $03, $03, $03, $03,  $03, $03, $03, $03,  $03, $03, $03, $04
.byte $04, $04, $04, $04,  $04, $04, $04, $04,  $04, $04, $04, $04,  $04, $04, $04, $04
.byte $04, $04, $04, $05,  $05, $05, $05, $05,  $05, $05, $05, $05,  $05, $05, $05, $05
.byte $05, $05, $05, $05,  $05, $05, $05, $06,  $06, $06, $06, $06,  $06, $06, $06, $06
.byte $06, $06, $06, $06,  $06, $06, $06, $06,  $06, $06, $06, $07,  $07, $07, $07, $07
.byte $07, $07, $07, $07,  $07, $07, $07, $07,  $07, $07, $07, $07,  $07, $07, $07, $07

/*
Mask bit patterns

Note that the possible mask values are:
 00, 01, 04, 05
 10, 11, 14, 15
 40, 41, 44, 45
 50, 51, 54, 55

 This means that all the possible nibble values are: 0, 1, 4 and 5.
 0 = all bits clear
 1 = bit 0 set
 4 = bit 2 set
 5 = bits 0 and 2 set

 So for a low nibble, only bits 0 and 2 might be set.
 For the high nibble, bits 4 and 6 might be set accordingly.
 To sum up, only the even bits might be set in these masks.
 Doing the following AND operation: bitmask AND #55 ...
 ...doesn't have any net effect, as it keeps only even bits set.
 This will become relevant in the "apply_mask" subroutine, which uses these masks.
*/
* = $D400
mask_bit_patterns:
.byte $00, $00, $00, $01,  $00, $00, $00, $01,  $00, $00, $00, $01,  $04, $04, $04, $05
.byte $00, $00, $00, $01,  $00, $00, $00, $01,  $00, $00, $00, $01,  $04, $04, $04, $05
.byte $00, $00, $00, $01,  $00, $00, $00, $01,  $00, $00, $00, $01,  $04, $04, $04, $05
.byte $10, $10, $10, $11,  $10, $10, $10, $11,  $10, $10, $10, $11,  $14, $14, $14, $15
.byte $00, $00, $00, $01,  $00, $00, $00, $01,  $00, $00, $00, $01,  $04, $04, $04, $05
.byte $00, $00, $00, $01,  $00, $00, $00, $01,  $00, $00, $00, $01,  $04, $04, $04, $05
.byte $00, $00, $00, $01,  $00, $00, $00, $01,  $00, $00, $00, $01,  $04, $04, $04, $05
.byte $10, $10, $10, $11,  $10, $10, $10, $11,  $10, $10, $10, $11,  $14, $14, $14, $15
.byte $00, $00, $00, $01,  $00, $00, $00, $01,  $00, $00, $00, $01,  $04, $04, $04, $05
.byte $00, $00, $00, $01,  $00, $00, $00, $01,  $00, $00, $00, $01,  $04, $04, $04, $05
.byte $00, $00, $00, $01,  $00, $00, $00, $01,  $00, $00, $00, $01,  $04, $04, $04, $05
.byte $10, $10, $10, $11,  $10, $10, $10, $11,  $10, $10, $10, $11,  $14, $14, $14, $15
.byte $40, $40, $40, $41,  $40, $40, $40, $41,  $40, $40, $40, $41,  $44, $44, $44, $45
.byte $40, $40, $40, $41,  $40, $40, $40, $41,  $40, $40, $40, $41,  $44, $44, $44, $45
.byte $40, $40, $40, $41,  $40, $40, $40, $41,  $40, $40, $40, $41,  $44, $44, $44, $45
.byte $50, $50, $50, $51,  $50, $50, $50, $51,  $50, $50, $50, $51,  $54, $54, $54, $55

/*
================================================================================
  draw_actor
================================================================================
Summary
	Orchestrate one actor’s frame: prepare costume pointers, compute tile X,
	bind mask patterns, resolve sprite VRAM base, clear staged rows, draw
	limbs if visible, apply foreground masking if needed, blit staged rows,
	and commit sprite X and background color.

Global Inputs
	actor                               current actor index
	active_costume                      costume index bound to the actor
	actor_sprite_index[]                sprite slot per actor
	character_sprite_x_{hi,lo}[]        16-bit X in pixels
	actor_gfx_base_{lo,hi}[]            graphics section base per actor
	limb_cel_list_for_actor_{lo,hi}[]  	limb→cel list pointer per actor
	actor_ofs_cel_hi_tbl[]				cel HI-table offset per actor
	actor_vars[]                        flags (bit7 = invisible)
	actor_box_attr[]             		0 = in front, ≠0 = behind FG layer
	character_sprite_bkg_colors[]       per-costume background color
	mask_bit_patterns                   global mask pattern table

Global Outputs
	actor_tile_x_coordinate             (X >> 3) - 3 in tile space
	global_mask_patterns_ptr            → mask_bit_patterns
	gfx_base              				zp pointer to graphics section
	cel_seq_tbl                      	zp pointer to limb-cel list
	actor_limb_slot_base                     actor index × 8 stride
	vertical_offset                     cleared to 0 before limb draws
	actor_sprite_x_{hi,lo}[]            committed sprite X for slot
	sprite_0_color[]                    committed background color

Description
	- setup_costume_for_actor: ensure costume and per-actor pointers are valid.
	- X ← actor, Y ← actor_sprite_index[X].
	- Compute tile X: ((character_sprite_x >> 3) - 3) → actor_tile_x_coordinate.
	- Point global_mask_patterns_ptr at mask_bit_patterns.
	- set_actor_sprite_base: bind actor_sprite_base for the slot.
	- Load gfx_base and cel_seq_tbl from per-actor tables.
	- Copy cel-address HI offset and constant used by the decoder.
	- Compute actor_limb_slot_base = actor * 8.
	- clear_sprite_visible_rows: zero staged rows for this sprite slot.
	- Zero vertical_offset (and unused $FD15).
	- If actor visible (actor_vars[active_costume] & ACTOR_IS_INVISIBLE == 0):
		• draw_actor_limbs
		• If behind FG layer (actor_box_attr[actor] ≠ 0):
	mask_actor_with_foreground_layer
	Then blit staged rows: blit_sprite_vthird.
	- Commit sprite X and background color for this sprite slot.

Notes
	- Uses double-buffered sprite rows and per-bank Y/coverage managed by
	callee routines.
	- Bias “-3” aligns decode to the character grid.
================================================================================
*/
* = $2252
draw_actor:
        // ----------------------------------------
        // Ensure costume resources and per-actor pointers are ready
        // ----------------------------------------
        jsr     setup_costume_for_actor

        // ----------------------------------------
        // X := actor, Y := sprite_index(actor)
        // ----------------------------------------
        ldx     actor
        ldy     actor_sprite_index,x

        // ----------------------------------------
        // actor_tile_x_coordinate = (Xpx >> 3) - 3
		// (Xpx is the X pixel coordinate)
        // ----------------------------------------
        lda     actor_tgt_sprite_x_hi,x      // >>1 into carry
        lsr      
        lda     actor_tgt_sprite_x_lo,x
        ror                                  // now >>1 with hi carry
        lsr                                  // >>2
        lsr                                  // >>3
        sec
        sbc     #$03
        sta     actor_tile_x_coordinate

        // ----------------------------------------
        // global_mask_patterns_ptr = &mask_bit_patterns
        // ----------------------------------------
        lda     #<mask_bit_patterns
        sta     global_mask_patterns_ptr
        lda     #>mask_bit_patterns
        sta     global_mask_patterns_ptr + 1
		
        lda     actor_sprite_index,x
		tax
		tay

        // ----------------------------------------
        // Compute sprite VRAM base for this Y (sprite slot)
        // ----------------------------------------
        jsr     set_actor_sprite_base

        // ----------------------------------------
        // gfx_base ← actor_gfx_base[actor]
        // ----------------------------------------
        ldx     actor
        lda     actor_gfx_base_lo,x
        sta     gfx_base
        lda     actor_gfx_base_hi,x
        sta     gfx_base + 1

        // ----------------------------------------
        // Copy offset of cel HI table 
        // ----------------------------------------
        lda     actor_ofs_cel_hi_tbl,x
        sta     ofs_cel_hi_tbl
        // ----------------------------------------
        // Save #02 as the offset for the low table,
		// to skip the unused 16-bit pointer at the beginning
        // ----------------------------------------
        lda     #$02
        sta     ofs_cel_lo_tbl

        // ----------------------------------------
        // cel_seq_tbl ← limb_cel_list_for_actor[actor]
        // ----------------------------------------
        lda     actor_cel_seq_tbl_lo,x
        sta     cel_seq_tbl
        lda     actor_cel_seq_tbl_hi,x
        sta     cel_seq_tbl + 1

        // ----------------------------------------
        // Precompute actor_index * 8 (stride used elsewhere)
        // ----------------------------------------
        txa
        clc
        asl      
        asl      
        asl      
        sta     actor_limb_slot_base

        // ----------------------------------------
        // Clear staged sprite rows for this sprite slot (Y preserved by callee)
        // ----------------------------------------
        ldx     actor
        ldy     actor_sprite_index,x
        jsr     clear_sprite_visible_rows

        // ----------------------------------------
        // vertical_offset := 0 (and zero unused $FD15)
        // ----------------------------------------
        lda     #$00
        sta     $FD15                        // unused
        sta     vertical_offset

        // ----------------------------------------
        // Visible? if actor_vars[active_costume] & INVISIBLE == 0 → draw limbs
        // ----------------------------------------
        ldx     active_costume
        lda     actor_vars,x
        and     #ACTOR_IS_INVISIBLE
        bne     skip_draw

        // draw limbs
        ldx     actor
        jsr     draw_actor_limbs

        // ----------------------------------------
        // If behind foreground, apply mask
        // ----------------------------------------
        ldx     actor
        lda     actor_box_attr,x
        beq     copy_rows
        jsr     mask_actor_with_foreground_layer

copy_rows:
        jsr     blit_sprite_vthird

skip_draw:
        // ----------------------------------------
        // Commit sprite X position for this sprite slot
        // ----------------------------------------
        ldx     actor
        lda     actor_tgt_sprite_x_hi,x
        ldy     actor_sprite_index,x
        sta     actor_sprite_x_hi,y
        lda     actor_tgt_sprite_x_lo,x
        ldy     actor_sprite_index,x
        sta     actor_sprite_x_lo,y

        // ----------------------------------------
        // Commit background color for this sprite slot
        // ----------------------------------------
        ldx     active_costume
        lda     character_sprite_bkg_colors,x
        sta     sprite_0_color,y

        rts
/*
================================================================================
  setup_costume_for_actor
================================================================================
Summary
        Bind the active costume to its actor. If unchanged, exit. Otherwise
        cache the costume base pointer and per-actor derived pointers:
        graphics base, limb cel list, and animation set list.

Vars / State
        costume_base  			                 Costume resource base pointer (lo/hi).
        graphic_data_ptr_for_actor_{lo,hi}[]     Per-actor graphics base = base+9.
        actor_ofs_cel_hi_tbl[]     				 Offset to HI-byte table for cel addresses.
        limb_cel_list_for_actor_{lo,hi}[]        Absolute pointer to limb cel list.
        animation_sets_for_actor_{lo,hi}[]       Absolute pointer to animation sets.

Global Inputs
        active_costume
        actor_for_costume[]                      Signed: bit7 set → no actor assigned.
        costume_ptr_{lo,hi}_tbl[]                Costume resource base by costume index.
        rsrc_ptr_for_active_costume_{lo,hi}[]    Cached “current costume” base per costume.

Global Outputs
        rsrc_ptr_for_active_costume_{lo,hi}[]    Updated to new costume base when changed.


Description
        - Verify an actor is assigned to the active costume; else return.
        - Fast path: if cached costume base already equals the requested one,
          return.
        - Set costume_base (zp) to the costume resource base.
        - Compute per-actor graphics base = costume_base + 9 (skip header).
        - Update the “current costume” cache for this costume.
        - Using the actor index, read header-relative offsets at +4..+8 and
          materialize absolute pointers by adding graphics base:
            * +4: actor_ofs_cel_hi_tbl (1 byte)
            * +5..+6: limb_cel_list_for_actor (16-bit, gfx-base relative)
            * +7..+8: animation_sets_for_actor (16-bit, gfx-base relative)

Notes
        Header layout: 9-byte metadata, then graphics/data section.
        Offsets at +5..+8 are relative to graphics base, not file base.
        Carry from +9 adjustment is handled when computing graphics base.
================================================================================
*/
//RENAME to setup_actor_animation_tables or similar
* = $2470
setup_costume_for_actor:
        // ----------------------------------------
        // Actor present for active costume?
        // ----------------------------------------
        ldx     active_costume
        lda     actor_for_costume,x
        bpl     check_if_new_costume_for_actor   	// bit7 clear → valid actor id
        rts                                       	// no actor bound → exit

check_if_new_costume_for_actor:
        // ----------------------------------------
        // Is requested costume already active? If so, exit.
        // ----------------------------------------
        lda     costume_ptr_hi_tbl,x
        cmp     active_costume_rsrc_hi,x
        bne     compute_data_pointer
        lda     costume_ptr_lo_tbl,x
        cmp     active_costume_rsrc_lo,x
        bne     compute_data_pointer
        rts

compute_data_pointer:
        // ----------------------------------------
        // Stash costume index; set Y=actor id; cache base and gfx ptr
        // ----------------------------------------
        txa
        pha                                     // save costume index
        lda     costume_ptr_hi_tbl,x
        ldy     actor_for_costume,x             // Y := actor index
        sta     actor_gfx_base_hi,y
        sta     costume_base + 1
        lda     costume_ptr_lo_tbl,x
        sta     costume_base

        // costume graphics section starts at base + 9 (skip header)
        clc
        adc     #$09
        sta     actor_gfx_base_lo,y
        bcc     set_new_costume_ptr_for_actor
        lda     actor_gfx_base_hi,y
        adc     #$00
        sta     actor_gfx_base_hi,y

set_new_costume_ptr_for_actor:
        // ----------------------------------------
        // Update “current costume” pointer cache for this costume
        // ----------------------------------------
        lda     costume_ptr_hi_tbl,x
        sta     active_costume_rsrc_hi,x
        lda     costume_ptr_lo_tbl,x
        sta     active_costume_rsrc_lo,x

        // ----------------------------------------
        // Switch X=actor index, Y=header read offset
        // ----------------------------------------
        tya
        tax

        // ----------------------------------------
        // +4: cel-address HI table offset (1 byte)
        // ----------------------------------------
        ldy     #$04
        lda     (costume_base),y
        sta     actor_ofs_cel_hi_tbl,x

        // ----------------------------------------
        // +5..+6: limb cel list pointer = gfx_base + rel16
        // ----------------------------------------
        iny
        lda     (costume_base),y
        clc
        adc     actor_gfx_base_lo,x
        sta     actor_cel_seq_tbl_lo,x
        iny
        lda     (costume_base),y
        adc     actor_gfx_base_hi,x
        sta     actor_cel_seq_tbl_hi,x

        // ----------------------------------------
        // +7..+8: clip sets pointer = gfx_base + rel16
        // ----------------------------------------
        iny
        lda     (costume_base),y
        clc
        adc     actor_gfx_base_lo,x
        sta     actor_clip_tbl_lo,x
        iny
        lda     (costume_base),y
        adc     actor_gfx_base_hi,x
        sta     actor_clip_tbl_hi,x

        // ----------------------------------------
        // Restore costume index and return
        // ----------------------------------------
        pla
        tax
        rts
/*
================================================================================
  set_actor_sprite_base
================================================================================
Summary
        Compute the 16-bit VRAM pointer for an actor’s sprite:
        actor_sprite_base = sprite_bank_base + (sprite_index * 64).

Arguments
        Y						sprite_index
        sprite_bank_sel 		bank selector (0 → SPRITE_BASE_1, nonzero → SPRITE_BASE_0)

Vars / Tables
        multiple_64_lo / multiple_64_hi   Precomputed 64*n (lo/hi) for n=0..N.
        actor_sprite_base                 pointer (lo/hi) to the sprite in VRAM.
        sprite_bank_sel                     selects bank: 0→SPRITE_BASE_1, ≠0→SPRITE_BASE_0.

Returns
        actor_sprite_base 		VRAM address of the sprite

Description
        - Look up (sprite_index * 64) in stride tables multiple_64_{lo,hi}.
        - Store the low byte to actor_sprite_base.lo.
        - Add the bank’s high byte (from SPRITE_BASE_*), plus the high byte of
          (sprite_index * 64), to produce actor_sprite_base.hi.
        - Banks are page-aligned, so only the high byte needs the base added.

================================================================================

Technical: sprite address = base + (index * 64)

Context
        Sprites are stored as 64-byte records. Two page-aligned banks exist:
        $E000 and $E800. Selecting a bank yields a 16-bit base address.

Computation
        sprite_ptr = sprite_base + (sprite_index << 6)

Implementation detail
        - (index * 64) is split via lookup tables:
              multiple_64_lo[index] → low byte
              multiple_64_hi[index] → high byte
        - Because bases are page-aligned ($E000/$E800), only the HIGH byte
          needs the base added; the LOW byte comes directly from multiple_64_lo.
        - Avoids runtime shifts/multiplications on 6502.
================================================================================
*/
* = $2456
set_actor_sprite_base:
        // ----------------------------------------
        // Low byte: 64 * sprite_index
        // ----------------------------------------
        lda     multiple_64_lo,y
        sta     actor_sprite_base

        // ----------------------------------------
        // High byte: (64 * index).hi + base_hi
        // base_hi = $E8 if sprite_bank_sel==0 else $E0
        // ----------------------------------------
        lda     multiple_64_hi,y
        clc
        ldy     sprite_bank_sel
        beq     add_base_E800
        adc     #>SPRITE_BASE_0
        jmp     store_ptr_hi
add_base_E800:
        adc     #>SPRITE_BASE_1
store_ptr_hi:
        sta     actor_sprite_base + 1
        rts
/*
================================================================================
  set_sprite_ptr_for_row
================================================================================
Summary
        Compute src_row_ptr = actor_sprite_base + sprite_row_offsets[row].

Arguments
        Y  = row index (0..rows-1)

Inputs
        actor_sprite_base (zp lo/hi)        Base address of the sprite.
        sprite_row_offsets_lo/hi[]          16-bit per-row byte offsets.

Returns
        src_row_ptr (zp lo/hi)          	Pointer to start of row Y.

Description
        Add the 16-bit row offset to the sprite base. CLC before the low-byte
        add allows carry to ripple into the high-byte add, handling page
        crossings correctly.
================================================================================

Sprite row pointer math and tables (start at logical row 8)

Goal
    Compute per-row sprite pointers fast on 6502:
        src_row_ptr = actor_sprite_base + offset(row)

Sprite layout
    - Each sprite slot = 64 bytes.
    - One sprite row = 3 bytes.
    - Byte offset for row r: off(r) = 3 * r.
    - Decompose off(r) into lo/hi bytes for 16-bit add.

Tables
    - Two 160-entry tables (lo/hi) precompute off(r) for r = 8..167:
        sprite_row_offsets_lo[i] = (3*(i+8)) & $FF
        sprite_row_offsets_hi[i] = (3*(i+8)) >> 8
      The low table steps +3 and wraps $3C→$00; the high table counts wraps.
    - Starting at row 8 avoids a runtime +8 and aligns with an 8-pixel UI bar.

Why row 8
    - The UI reserves the top 8 pixels (one character row) for the message bar.
    - Engine draw order begins at screen y=8 (first drawable row).
    - Index i=0 maps directly to r=8, reducing per-scanline work and keeping
      sprite-to-character-grid alignment clean.

Runtime usage
    CLC
    src_row_ptr.lo = base.lo + sprite_row_offsets_lo[i]
    src_row_ptr.hi = base.hi + sprite_row_offsets_hi[i] + carry
    // base = actor_sprite_base, i = (row index starting at logical row 8)

Properties
    - No multiplies or shifts at runtime.
    - Page crossings handled implicitly via the hi table.
    - 160 entries cover 160 consecutive rows from r=8.
================================================================================

Table excerpts

  Low bytes (start):
    i : lo
    0 : $18
    1 : $1B
    2 : $1E
    3 : $21
       … +3 each entry …
   12 : $3C
   13 : $00   (wrap)
   14 : $03
   15 : $06
       …

  High bytes (start):
    i : hi
    0 : $00
    1 : $00
    2 : $00
    3 : $00
       … stays $00 until a 256-byte wrap …
   12 : $01
   13 : $01
       …

First few pointer results with base = $E000
  i   lo  hi   offset    src_row_ptr
  0  $18 $00   $0018  →  $E018
  1  $1B $00   $001B  →  $E01B
  2  $1E $00   $001E  →  $E01E
  3  $21 $00   $0021  →  $E021
  …
 12  $3C $01   $013C  →  $E13C
 13  $00 $01   $0100  →  $E100

Last entries
  Low bytes (end of table):
    …, $36, $39, $3C, $00,  $03, $06, $09, $0C,
        $0F, $12, $15, $18,  $1B, $1E, $21, $24

  High bytes (end of table):
    … (all $07 on the last line)

Last pointer examples with base = $E000
  i (last-3) : lo=$1E hi=$07 → offset $071E → ptr $E71E
  i (last-2) : lo=$21 hi=$07 → offset $0721 → ptr $E721
  i (last-1) : lo=$24 hi=$07 → offset $0724 → ptr $E724

================================================================================
*/
* = $240C
set_sprite_ptr_for_row:
        // ----------------------------------------
        // src_row_ptr.lo = base.lo + offset.lo
        // ----------------------------------------
        lda     sprite_row_offsets_lo,y
        clc
        adc     actor_sprite_base
        sta     src_row_ptr

        // ----------------------------------------
        // src_row_ptr.hi = base.hi + offset.hi + carry
        // ----------------------------------------
        lda     sprite_row_offsets_hi,y
        adc     actor_sprite_base + 1
        sta     src_row_ptr + 1
        rts
/*
================================================================================
  clear_sprite_visible_rows
================================================================================
Summary
	Clear visible VRAM rows for one sprite slot based on per-bank Y and
	vertical coverage. Computes top = bottom − voff, clips to the visible
	window, then zeros 3 bytes per row.

Arguments
	Y                               	sprite index for this actor

Global Inputs
	sprite_bank_sel                 	bank select (0 → bank0, ≠0 → bank1)
	sprite_y_buf0/1[]					per-sprite bottom row by bank
	sprite_voff_buf0/1[]  				per-sprite vertical coverage by bank
	sprite_row_offsets_lo/hi[]          row byte offsets
	actor_sprite_base                   base VRAM pointer for sprite

Global Outputs
	writes zeros through (src_row_ptr),Y

ZP / Temps
	src_row_ptr                 		computed row start (lo/hi)
	clear_row_idx                   	current row being cleared
	bottom_row                      	clamped bottom row

Description
	- Select banked bounds:
		• bottom_row ← sprite_y_buf{bank}[Y]
		• top ← bottom_row − sprite_voff_buf{bank}[Y]
	Store top in clear_row_idx.
	- Clip: if bottom_row > VISIBLE_ROW_MAX then
		• if top > VISIBLE_ROW_MAX → nothing to clear, return
		• else bottom_row ← VISIBLE_ROW_MAX
	If top > VISIBLE_ROW_MAX, clamp clear_row_idx ← 0.
	- Loop rows from clear_row_idx while row < bottom_row:
		• set_sprite_ptr_for_row → src_row_ptr
		• store $00 into bytes [2],[1],[0] of the row.

Notes
	- Loop condition is exclusive of bottom_row (clears rows top..bottom−1).
	- Each sprite row is 3 bytes; writes are (zp),Y with Y = 2,1,0.
================================================================================
*/
* = $22EC
clear_sprite_visible_rows:
		// ------------------------------------------------------------
		// Select per-bank bounds: bottom_row and top = bottom_row − voff
		// ------------------------------------------------------------
		lda     sprite_bank_sel
		beq     path_buf0_load_bounds

		// Bank 1 path: compute bottom/top from bank-1 buffers
		lda     sprite_y_buf1,y
		sta     bottom_row
		sec
		sbc     sprite_voff_buf1,y       // A := top = bottom_row − sprite_voff_buf1[Y]
		jmp     init_top_row             // A already holds top → proceed

path_buf0_load_bounds:
		// Bank 0 path: compute bottom_row and top from bank-0 buffers
		lda     sprite_y_buf0,y
		sta     bottom_row
		sec
		sbc     sprite_voff_buf0,y       // A := top = bottom_row - sprite_voff_buf0[Y]

		// ------------------------------------------------------------
		// Initialize top row index from computed A (top)
		// ------------------------------------------------------------
init_top_row:
		sta     clear_row_idx

		// ------------------------------------------------------------
		// Clip bottom to visible window; early-exit if sprite is fully below
		// bottom_row <= VISIBLE_ROW_MAX → continue checks
		// bottom_row  > VISIBLE_ROW_MAX → may be fully below or intersecting
		// ------------------------------------------------------------
		lda     bottom_row
		cmp     #VISIBLE_ROW_MAX
		bcc     validate_top_in_range          // bottom <  last visible row
		beq     validate_top_in_range          // bottom == last visible row

		// Bottom beyond window: if top <= last_row clamp; else nothing to clear
		lda     clear_row_idx
		cmp     #VISIBLE_ROW_MAX
		bcc     clamp_bottom_to_visible_max     // top < last_row → intersects → cap bottom
		beq     clamp_bottom_to_visible_max     // top == last_row → intersects → cap bottom
		rts                                     // top > last_row → fully below → done

clamp_bottom_to_visible_max:
		lda     #VISIBLE_ROW_MAX            // Cap bottom_row at the visible window maximum
		sta     bottom_row
		jmp		begin_clear_loop
		
		// ------------------------------------------------------------
		// Validate top: keep within [0 .. VISIBLE_ROW_MAX]
		// ------------------------------------------------------------
validate_top_in_range:
		lda     clear_row_idx
		cmp     #VISIBLE_ROW_MAX
		bcc     begin_clear_loop           // top <  last visible row → OK
		beq     begin_clear_loop           // top == last visible row → OK
		lda     #$00
		sta     clear_row_idx              // top >  last visible row → clamp to 0

        // ----------------------------------------
        // For each row from clear_row_idx to bottom_row (inclusive): clear 3 bytes
        // ----------------------------------------
begin_clear_loop:
		lda     clear_row_idx

clear_row_loop:
		tay                             // Y := current row index
		jsr     set_sprite_ptr_for_row  // src_row_ptr → start of row Y

		ldy     #$02                    // Zero row bytes [2],[1],[0]
		lda     #$00
		sta     (src_row_ptr),y
		dey
		sta     (src_row_ptr),y
		dey
		sta     (src_row_ptr),y

		ldy		active_costume			
		inc     clear_row_idx           // Advance to next row
		lda     clear_row_idx           // Load current row index
		cmp     bottom_row              // Compare against bottom bound
		bmi     clear_row_loop          // Continue while row < bottom_row
		rts                             // Done clearing rows
/*
================================================================================
  compute_cel_ptr
================================================================================
Summary
        Resolve absolute pointer to the current limb’s cel bitmap:
        src_cel_ptr = gfx_base + cel_address(cel_id)

Inputs
        actor_limb_slot_base            base offset for current actor’s limb bank
        current_limb              		limb selector within the actor
        limb_frame_idx[]    			per-(actor,limb): frame index
        limb_current_cel[]       		per-(actor,limb): current cel sequence index
        cel_seq_tbl                  	table of lists mapping frame→cel_id
        gfx_base          				graphics section base
        ofs_cel_hi_tbl        			offset to HI-bytes table within graphics section
        ofs_cel_lo_tbl 			        offset to LO-bytes table within graphics section

Outputs
        src_cel_ptr                 	absolute pointer to cel bitmap
================================================================================
*/
* = $23D8
compute_cel_ptr:
        // ----------------------------------------
        // X := limb index for this actor = actor_limb_slot_base + current_limb
        // ----------------------------------------
        lda     actor_limb_slot_base
        clc
        adc     current_limb
        tax

        // ----------------------------------------
        // Y := frame-to-cel index = anim_frame + base_cel
        // ----------------------------------------
        lda     limb_frame_idx,x
        clc
        adc     limb_current_cel,x
        tay

        // ----------------------------------------
        // A := cel_idx_offset = cel_seq_tbl[Y]
        // ----------------------------------------
        lda     (cel_seq_tbl),y
        tay

        // ----------------------------------------
        // offset_hi -> cel_hi_tbl
        // ----------------------------------------
        lda     (gfx_base),y
        tay                                     
        clc
        adc     ofs_cel_hi_tbl        
        sta     offset_hi

        // ----------------------------------------
        // Y -> cel_lo_tbl
        // ----------------------------------------
        tya                                     
        clc
        adc     ofs_cel_lo_tbl                     
        tay
		
        // ----------------------------------------
        // <src_cel_ptr = cel_lo_tbl[cel_idx] (we're actually using the cel_idx_offset relative to gfx_base)
        // ----------------------------------------
        lda     (gfx_base),y
        clc
        adc     gfx_base
        sta     src_cel_ptr

        // ----------------------------------------
        // >src_cel_ptr = cel_hi_tbl[cel_idx] (we're actually using the cel_idx_offset relative to gfx_base)
        // ----------------------------------------
        ldy     offset_hi
        lda     (gfx_base),y
        adc     gfx_base + 1
        sta     src_cel_ptr + 1
        rts
/*
================================================================================
  blit_sprite_vthird
================================================================================
Summary
	Copy one vertical third of a multicolor sprite: 7 rows × 3 bytes.
	For each selected row, build source and destination pointers, then copy
	bytes [2],[1],[0] from staged source to destination.

Global Inputs
	copy_offsets[]                  maps 0..6 → absolute row index
	src_row_ptr                 	source pointer (built per row)
	dest_row_ptr                 	destination pointer (src − $0100)
	saved_x                         temp used to preserve caller’s X

Description
	- Preserve caller’s X in saved_x.
	- Initialize X=0 and vthird_row_idx=0.
	- Loop 7 times:
		• Y ← copy_offsets[X] to select the absolute row.
		• set_sprite_ptr_for_row builds the source pointer for that row.
		• derive_dest_from_src derives the destination one page earlier.
		• Copy three bytes in order [2],[1],[0] using (zp),Y.
		• Increment vthird_row_idx and compare against 7 to continue.
	- Restore X from saved_x and return.
================================================================================
*/
* = $23A8
blit_sprite_vthird:
		stx     saved_x                 // Preserve caller X for restore after the loop
		
		ldx     #$00                    // Initialize loop: first of 7 rows in this vertical third
		stx     vthird_row_idx          // Save row index in a local var

row_loop:
		lda     copy_offsets,x          // Load absolute row index for this iteration
		tay                             // Y := row index consumed by pointer builders
		jsr     set_sprite_ptr_for_row  // Build src_row_ptr for row Y (source address)
		jsr     derive_dest_from_src 	// Derive dest: dest_row_ptr = src_row_ptr - $0100

		ldy     #$02                    // Set Y=2: copy three bytes of this row in order [2],[1],[0]
		lda     (src_row_ptr),y     
		sta     (dest_row_ptr),y     
		dey                             // Y=1
		lda     (src_row_ptr),y     
		sta     (dest_row_ptr),y     
		dey                             // Y=0
		lda     (src_row_ptr),y     
		sta     (dest_row_ptr),y     

		inc     vthird_row_idx          // Advance row counter for this vertical third
		ldx     vthird_row_idx          // Reload X with current row index (0..6)
		cpx     #$07                    // Have we processed 7 rows yet?
		bne     row_loop                // No → iterate

		ldx     saved_x                 // Restore caller’s X
		rts                              
/*
================================================================================
  derive_dest_from_src
================================================================================
Summary
        Update destination pointer one page before source:
        dest_row_ptr = src_row_ptr - $0100

Inputs
        src_row_ptr  

Outputs
        dest_row_ptr  
================================================================================
*/
* = $241E
derive_dest_from_src:
        // copy low byte, prepare subtract
        lda     src_row_ptr
        sec
        sbc     #$00
        sta     dest_row_ptr

        // subtract one from high byte (page back)
        lda     src_row_ptr + 1
        sbc     #$01
        sta     dest_row_ptr + 1
        rts
/*
================================================================================
  draw_actor_limbs
================================================================================
Summary
	Render all limbs for the current actor in sequence. Seed the per-draw
	Y position from the actor, reset per-draw state, resolve each limb’s
	cel, blit with masking/flip, track maximum vertical coverage, then
	commit per-bank Y and coverage.

Vars/State
	current_sprite_y_pos            	working Y used by the blitter
	intercel_vertical_displacement 		per-limb Y displacement accumulator
	sprite_vertical_offset          	max coverage seen during this draw
	current_limb                    	limb index
	current_limb_flip               	cached flip flags for active limb

Global Inputs
	actor                           	current actor index
	character_sprite_y[]            	base Y per actor
	limb_flip_tbl[]                 	per-(actor,limb) flip flags
	vertical_offset                 	coverage result set by blitter

Global Outputs
	current_limb_flip               	updated for each limb
	current_sprite_y_pos            	adjusted and consumed by blitter
	sprite_vertical_offset          	final max coverage for this actor

Description
	- Initialize current_sprite_y_pos from character_sprite_y[actor].
	- Clear per-draw temporaries: fifth_byte, intercel displacement,
	sprite_vertical_offset, and current_limb.
	- For each limb:
		• Compute (actor*8 + current_limb) index.
		• Cache flip flags into current_limb_flip.
		• Apply intercel_vertical_displacement to current_sprite_y_pos.
		• compute_cel_ptr → blit_and_mask_cel.
		• Update sprite_vertical_offset = max(sprite_vertical_offset,
		vertical_offset).
	- After the loop, write Y and coverage into the active sprite bank via
	commit_sprite_coverage_for_bank.
================================================================================
*/
* = $2356
draw_actor_limbs:
        // ----------------------------------------
        // Seed vertical position from actor
        // ----------------------------------------
        ldx     actor
        lda     character_sprite_y,x
        sta     current_sprite_y_pos

        // ----------------------------------------
        // Init per-draw state
        // ----------------------------------------
        lda     #$00
		sta		$3E
		sta		$FD4D
        sta     fifth_byte
        sta     intercel_vertical_displacement
        sta     sprite_vertical_offset
        sta     current_limb

draw_limb:
        // ----------------------------------------
        // X := actor*8 + current_limb
        // ----------------------------------------
        lda     actor
        asl
        asl
        asl
        clc
        adc     current_limb
        tax

        // ----------------------------------------
        // Cache flip state for this limb
        // ----------------------------------------
        lda     limb_flip_tbl,x
        sta     current_limb_flip

        // ----------------------------------------
        // Apply inter-cel vertical displacement
        // ----------------------------------------
        lda     current_sprite_y_pos
        sec
        sbc     intercel_vertical_displacement
        sta     current_sprite_y_pos

        // ----------------------------------------
        // Resolve cel pointer and blit with masking/flip
        // ----------------------------------------
        jsr     compute_cel_ptr
        jsr     blit_and_mask_cel

        // ----------------------------------------
        // Track max vertical coverage for this actor
        // ----------------------------------------
        lda     vertical_offset
        cmp     sprite_vertical_offset
        bcc     next_limb
        beq     next_limb
        sta     sprite_vertical_offset

next_limb:
        // ----------------------------------------
        // Advance limb; loop over 8 limbs
        // ----------------------------------------
        inc     current_limb
        lda     #TOTAL_LIMBS
        cmp     current_limb
        bne     draw_limb

        // ----------------------------------------
        // Finalize per-buffer sprite Y positions
        // ----------------------------------------
        jsr     commit_sprite_coverage_for_bank
        rts
/*
================================================================================
  commit_sprite_coverage_for_bank
================================================================================
Summary
	Write the actor’s per-sprite Y position and vertical coverage into the
	active sprite bank buffers, then mirror the maximum vertical offset for
	this actor.

Global Inputs
	actor                           current actor index
	actor_sprite_index[]            sprite slot for each actor
	sprite_bank_sel                 0 → bank 0 buffers, ≠0 → bank 1 buffers
	current_sprite_y_pos            final Y used for this draw
	sprite_vertical_offset          max vertical coverage from limb blits

Global Outputs
	sprite_y_buf0/1[]        		per-sprite Y by bank
	sprite_voff_buf0/1[]			per-sprite vertical offset by bank
	actor_sprite_y_extent[]         mirrored per-actor max offset

Description
	- Load X with the actor index and Y with its sprite slot.
	- If sprite_bank_sel ≠ 0, write Y and vertical offset to bank 1 buffers;
	else write to bank 0 buffers.
	- Mirror sprite_vertical_offset into actor_sprite_y_extent[X] so
	later stages can query the actor’s vertical coverage for this frame.
================================================================================
*/
* = $242C
commit_sprite_coverage_for_bank:
		// ------------------------------------------------------------
		// Index selection: X = actor, Y = sprite slot for that actor
		// ------------------------------------------------------------
		ldx     actor
		ldy     actor_sprite_index,x

		// ------------------------------------------------------------
		// Bank select and dispatch: 0 → bank 0, ≠0 → bank 1
		// Z set if sprite_bank_sel == 0; BEQ takes bank 0 path
		// ------------------------------------------------------------
		lda     sprite_bank_sel
		beq     sprite_bank_sel_00

		// ------------------------------------------------------------
		// Write vertical offset to bank 1 buffer at slot Y
		// ------------------------------------------------------------
		lda     sprite_vertical_offset
		sta     sprite_voff_buf1,y
		// ------------------------------------------------------------
		// Write final Y position to bank 1 buffer at slot Y
		// ------------------------------------------------------------
		lda     current_sprite_y_pos
		sta     sprite_y_buf1,y
		
		// Skip bank-0 path and join common epilogue
		jmp     save_vertical_offset_for_actor

sprite_bank_sel_00:
		// ------------------------------------------------------------
		// Write vertical offset to bank 0 buffer at slot Y
		// ------------------------------------------------------------
		lda     sprite_vertical_offset
		sta     sprite_voff_buf0,y

		// ------------------------------------------------------------
		// Write final Y position to bank 0 buffer at slot Y
		// ------------------------------------------------------------
		lda     current_sprite_y_pos
		sta     sprite_y_buf0,y


save_vertical_offset_for_actor:
		// ------------------------------------------------------------
		// Mirror maximum vertical coverage for this actor for later use
		// ------------------------------------------------------------
		lda 	sprite_vertical_offset
		sta 	actor_sprite_y_extent,x
		rts
		
/*		
procedure draw_actor():
    // Ensure costume tables and graphics pointers are up to date
    setup_costume_for_actor()

    actorIndex  = actor
    spriteSlot  = actor_sprite_index[actorIndex]

    // Compute a tile-space X coordinate from the sprite pixel X
    xPixels     = actor_tgt_sprite_x[actorIndex]      // 16-bit
    tileX       = (xPixels >> 3) - 3
    actor_tile_x_coordinate = tileX

    // Point global mask pointer at the shared mask table
    global_mask_patterns_ptr = &mask_bit_patterns

    // Bind sprite VRAM base for this slot and bank
    // (uses spriteBankSel and spriteSlot internally)
    set_actor_sprite_base(spriteSlot)

    // Load per-actor graphics base
    gfx_base = actor_gfx_base[actorIndex]

    // Configure cel address tables (offsets within gfx section)
    ofs_cel_hi_tbl = actor_ofs_cel_hi_tbl[actorIndex]
    ofs_cel_lo_tbl = constant 2        // skip initial unused entry

    // Bind limb→cel sequence table
    cel_seq_tbl = actor_cel_seq_tbl[actorIndex]

    // Precompute "actor * numberOfLimbs" stride for limb tables
    actor_limb_slot_base = actorIndex * TOTAL_LIMBS

    // Clear any previously drawn rows for this sprite slot
    clear_sprite_visible_rows(spriteSlot)

    // Reset vertical coverage accumulator
    vertical_offset = 0

    // If actor is marked invisible for this costume, skip drawing
    if (actor_vars[active_costume] has ACTOR_IS_INVISIBLE bit):
        goto commit_sprite_state

    // Draw all limbs, track coverage, commit per-bank Y/coverage
    draw_actor_limbs()

    // If actor is behind foreground layer, apply foreground mask
    if actor_box_attr[actorIndex] != 0:
        mask_actor_with_foreground_layer()

    // Copy staged rows into the final sprite memory region
    blit_sprite_vthird()

commit_sprite_state:
    // Commit sprite X for this slot (pixel position)
    spriteSlot  = actor_sprite_index[actorIndex]
    actor_sprite_x[spriteSlot] = actor_tgt_sprite_x[actorIndex]

    // Commit background color for this slot based on costume
    costumeIndex = active_costume
    sprite_0_color[spriteSlot] = character_sprite_bkg_colors[costumeIndex]
end procedure
		
		
procedure setup_costume_for_actor():
    costumeIndex = active_costume

    // Is there an actor bound to this costume?
    actorId = actor_for_costume[costumeIndex]
    if actorId is "no actor":
        return

    // Fast path: if cached resource pointer for this costume
    // already matches the requested costume resource, nothing to do
    if costume_ptr_tbl[costumeIndex] == active_costume_rsrc[costumeIndex]:
        return

    // Cache costume base and compute per-actor graphics base
    costumeBase = costume_ptr_tbl[costumeIndex]
    actorIndex  = actorId

    costume_base        = costumeBase
    actor_gfx_base[actorIndex] = costumeBase + headerSize(9 bytes)

    // Update "current costume resource" cache for this costume
    active_costume_rsrc[costumeIndex] = costumeBase

    // From the costume header, read offsets relative to graphics base:
    //  +4: cel-address HI table offset (1 byte)
    //  +5..+6: limb cel sequence table offset (16-bit)
    //  +7..+8: clip-set table offset (16-bit)
    header = memory_at(costume_base)

    celHiOffset   = header[4]
    celSeqOffset  = read16(header[5])
    clipTblOffset = read16(header[7])

    actor_ofs_cel_hi_tbl[actorIndex] = celHiOffset

    actor_cel_seq_tbl[actorIndex] =
        actor_gfx_base[actorIndex] + celSeqOffset

    actor_clip_tbl[actorIndex] =
        actor_gfx_base[actorIndex] + clipTblOffset
end procedure
		
		
procedure set_actor_sprite_base(spriteIndex):
    // Lookup (spriteIndex * 64) from tables
    offset = multiple_64[spriteIndex]   // 16-bit result

    // Low byte of base = low byte of offset
    actor_sprite_base.low = offset.low

    // High byte of base = offset.high + bankHigh
    if sprite_bank_sel == 0:
        bankHigh = highByte(SPRITE_BASE_1)
    else:
        bankHigh = highByte(SPRITE_BASE_0)

    actor_sprite_base.high = offset.high + bankHigh
end procedure
		
		
procedure set_sprite_ptr_for_row(rowIndex):
    // rowIndex is a logical row (starting at 8)
    // offset = byte offset of this row within the sprite record (3 * rowIndex)
    offset = sprite_row_offsets[rowIndex]    // 16-bit

    // src_row_ptr = actor_sprite_base + offset
    src_row_ptr = actor_sprite_base + offset
end procedure
		
		
procedure clear_sprite_visible_rows(spriteIndex):
    // Select per-bank bottom and vertical coverage for this sprite slot
    if sprite_bank_sel != 0:
        bottom = sprite_y_buf1[spriteIndex]      // last covered row (exclusive)
        height = sprite_voff_buf1[spriteIndex]   // number of rows covered
    else:
        bottom = sprite_y_buf0[spriteIndex]
        height = sprite_voff_buf0[spriteIndex]

    // Compute top row index
    top = bottom - height
    clear_row_idx = top

    // Clip bottom to visible window [0 .. VISIBLE_ROW_MAX]
    if bottom > VISIBLE_ROW_MAX:
        // If the sprite is entirely below the visible window, nothing to clear
        if top > VISIBLE_ROW_MAX:
            return

        // Otherwise, clamp bottom to last visible row
        bottom = VISIBLE_ROW_MAX

        // clear_row_idx already set from 'top', falls through to loop startup
    else:
        // bottom is within window; make sure top is not past window
        if top > VISIBLE_ROW_MAX:
            clear_row_idx = 0    // clamp to top of window

    // Clear rows from clear_row_idx up to (but not including) bottom
    row = clear_row_idx
    while row < bottom:
        // Build pointer for this row
        set_sprite_ptr_for_row(row)

        // Clear the 3 bytes that store this sprite's row
        clear 3 bytes starting at src_row_ptr

        row += 1
    end while
end procedure
		
		
procedure compute_cel_ptr():
    // Compute limb index for this actor
    limbIndex = actor_limb_slot_base + current_limb

    // Combine frame index and base cel index
    frame     = limb_frame_idx[limbIndex]
    baseCel   = limb_current_cel[limbIndex]
    seqIndex  = frame + baseCel

    // Map frame to cel-id offset using cel sequence table
    celIndexOffset = cel_seq_tbl[seqIndex]

    // celIndexOffset indexes into a table in the gfx section that holds
    // HI-byte pointers; add the HI-table offset to find the exact entry
    hiEntryIndex = gfx_base[celIndexOffset] + ofs_cel_hi_tbl

    // celIndexOffset also feeds into the LO-table: add lo-table offset
    loEntryIndex = celIndexOffset + ofs_cel_lo_tbl

    // Read relative cel address from LO and HI tables and add gfx_base
    relLo = gfx_base[loEntryIndex]
    relHi = gfx_base[hiEntryIndex]

    relativeAddress = combine(relLo, relHi)      // 16-bit offset from gfx_base
    src_cel_ptr     = gfx_base + relativeAddress
end procedure
		
		
procedure blit_sprite_vthird():
    vthird_row_idx = 0

    while vthird_row_idx < 7:
        // Map local index (0..6) to an absolute row index to copy
        absoluteRow = copy_offsets[vthird_row_idx]

        // Build source pointer for this row
        set_sprite_ptr_for_row(absoluteRow)

        // Derive destination pointer as one page before source
        derive_dest_from_src()

        // Copy 3 bytes of this row from staged buffer to final sprite memory
        for byteIndex in [2, 1, 0]:
            dest_row_ptr[byteIndex] = src_row_ptr[byteIndex]

        vthird_row_idx += 1
    end while

end procedure
		
		
procedure derive_dest_from_src():
    // dest_row_ptr points to the same offset as src_row_ptr,
    // but one page (256 bytes) earlier in memory
    dest_row_ptr = src_row_ptr - 256
end procedure


procedure draw_actor_limbs():
    actorIndex = actor

    // Seed vertical position from actor's base sprite Y
    current_sprite_y_pos = character_sprite_y[actorIndex]

    // Reset per-draw state
    intercel_vertical_displacement  = 0
    sprite_vertical_offset          = 0
    current_limb                    = 0

    // Loop over all limbs for this actor
    while current_limb < TOTAL_LIMBS:
        // Compute per-limb index for this actor
        limbIndex = actorIndex * TOTAL_LIMBS + current_limb

        // Cache flip state for this limb
        current_limb_flip = limb_flip_tbl[limbIndex]

        // Adjust working Y based on inter-cel vertical displacement
        current_sprite_y_pos -= intercel_vertical_displacement

        // Resolve cel pointer for this limb animation frame
        compute_cel_ptr()

        // Render limb with masking and flip flags, updating:
        //   - vertical_offset
        //   - intercel_vertical_displacement (by convention, in blitter)
        blit_and_mask_cel()

        // Track maximum vertical coverage so far
        if vertical_offset > sprite_vertical_offset:
            sprite_vertical_offset = vertical_offset

        current_limb += 1
    end while

    // Commit final per-bank Y position and vertical coverage for this actor
    commit_sprite_coverage_for_bank()
end procedure


procedure commit_sprite_coverage_for_bank():
    actorIndex = actor
    spriteSlot = actor_sprite_index[actorIndex]

    // Write Y and vertical coverage into the active sprite bank buffers
    if sprite_bank_sel != 0:
        sprite_voff_buf1[spriteSlot] = sprite_vertical_offset
        sprite_y_buf1[spriteSlot]    = current_sprite_y_pos
    else:
        sprite_voff_buf0[spriteSlot] = sprite_vertical_offset
        sprite_y_buf0[spriteSlot]    = current_sprite_y_pos

    // Mirror max vertical coverage per actor for later consumers
    actor_sprite_y_extent[actorIndex] = sprite_vertical_offset
end procedure
*/
		