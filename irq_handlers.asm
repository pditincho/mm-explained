/*
================================================================================
  Raster IRQ system overview
================================================================================

Purpose
	The IRQ chain manages all VIC-II timing-sensitive updates during each video
	frame. It divides the frame into small "bands" and updates sprite positions,
	shapes, and video memory mode exactly when the raster beam reaches those
	lines. This enables multiplexing (reusing a few hardware sprites to draw
	many logical actors).

Main structure
	irq_handler1 → sets up the frame and schedules the next handler.
	irq_handler2..16 → execute in sequence across the screen at fixed raster
	lines, updating sprite Y and shape data as the beam moves downward.
	The final handler loops back to irq_handler1 for the next frame.

irq_handler1: frame setup
	• Runs near the top of the screen (VBlank).
	• Saves CPU state, maps I/O, and clears interrupt locks.
	• Prepares the VIC-II defaults: 25-row mode, vertical scroll = 3,
	background color = black.
	• Checks if a full video update is required.
		- If not, it just refreshes sprite X-high bytes and chains raster IRQs.
		- If yes, it may copy color RAM or switch video layout as requested.
	• If a pending sprite-shape set is marked, updates all sprite base indices.
	• Finalizes frame book-keeping (sound, cursor, keyboard, message logic).
	• Arms the first raster interrupt (irq_handler2).

irq_handler2..16: per-band updates
	These are precisely timed raster interrupts spaced down the screen.
	Their purpose is to reuse the same four hardware sprites for multiple
	visible actors by repositioning them vertically (sprite multiplexing).

Helper routine:
  • compose_sprite_x_msb_and_chain_irq
    Build and write the combined sprite X-MSB mask and program the
    next raster IRQ vector and line.

Two alternating handler types:
    - Position/Shape handlers:
        • Update X positions using relative-sprite tables.
        • Change sprite shape pointers for sprites 0–3.
        • Refresh sprite colors and cursor properties.
        • Set up next raster interrupt vector.
    - Y-step handlers:
        • Move sprite Y positions downward by 21 pixels.
        • Schedule the next handler to fire at the following raster line.

The sequence continues down the screen:
    irq_handler2:  first band, sets sprite_shape_1, Y=$32 → next irq#3
    irq_handler3:  step down +21 → next irq#4
    irq_handler4:  sprite_shape_2, Y=$47 → next irq#5
    irq_handler5:  step down +21 → next irq#6
    ...
    irq_handler14: sprite_shape_7, Y=$B0 → next irq#15
    irq_handler15: final Y-step, Y=$C5 → next irq#16 (wrap)

	Handler 16 = last band’s shape update and handoff.

		* Writes sprite_shape_8 into the four active hardware sprite shape slots using the current vic_sprite_idx_0..3.
		* Arms raster line #$C3 and swaps the vector to irq_handler17.
		* Acknowledges VIC IRQ and restores mapped I/O and saved regs, then RTI. 

	Handler 17 = late-frame UI/input/sound housekeeping.

		* If not in cutscene, updates cursor and interaction area and writes cursor sprite X/Y; 
			otherwise or if hide_cursor_flag_flag is set, moves the cursor sprite off-screen.
		* Optionally reloads sound resource pointers and calls sound_irq_handler, 
			then clears the semaphore so this section can run next frame.
		* Restores cpu_port and CPU regs, then RTI. 
			Runs at the armed raster #$C3. 
		• Pushes all sprites off-screen (Y=$FD).
		• Sets raster to the end-of-frame line.
		• Switches the interrupt vector back to irq_handler1.

Relative sprite mapping
	Hardware sprite registers are reused by logical actors in order of
	priority. The relative-sprite table defines which actor occupies which
	hardware slot in each band. This lets sorting systems change priority
	without touching the IRQ code.

Relationship with actor system
	• The actor module prepares tables with X/Y positions, shapes, and
	relative order before raster processing starts.
	• The IRQ handlers only read those tables and push values to the VIC-II
	registers in sync with the beam.
	• Video memory switching and color RAM copies are triggered here only
	at safe times (top of frame) to avoid tearing.

Mental model
	1. irq_handler1 - director: prepare the frame, and, via 
	compose_sprite_x_msb_and_chain_irq, emit $D010 and arm H2
	2. irq_handler2..16 - performers: alternate shape changes and Y steps.
	3. Final handler - curtain call: hide sprites, restart from top.

================================================================================

Why a relative-sprite map
	Sorting and priority decisions happen in the actor system. The IRQ code
	only consumes an indirection table:
	
		actor → relative_sprite_rank → hardware sprite #
	
	This lets us reorder actors by Y (and apply special priorities like the
	"plant" override) without touching IRQ logic. The band handlers always
	write the correct hardware registers for "who is rank 0..3 right now."

Link to the actor pipeline
	- The actor code computes visibility, chooses which actors receive one of
	the four sprite "tickets", generates shape indices for each band, and
	fills tables for X-lo/X-hi and ranks.
	- The IRQ code streams those tables to the VIC-II on schedule. The result
	is stable layering, no register contention, and no tearing at edges.

Edge cases and stability hooks
	- Cursor is handled as a special sprite with its own X and Y updates.
	- Lights-off or similar states map to a constrained color set here.
	- Full-layout changes and color RAM copies are synchronized at the top of
	frame to avoid mid-frame artifacts.

================================================================================

Compact timeline for raster IRQ chain

Handler     	Raster  Main actions                                 	Y set
----------- 	------	-------------------------------------------- 	------
irq_handler1  	top     Baseline VIC-II; optional video/layout work;
						seed sprite X-lo/hi & ranks; UI/sound block;   	$29
						call compose_sprite_x_msb_and_chain_irq

irq_handler2  	$29     X-lo for sprites 0–6 via relative map;
						shapes #1 for slots 0–3;
						colors, cursor X/color; Y := $32              	$32

irq_handler3  	$3A     Enable multicolor text; Y := $47             	$47

irq_handler4  	$45     Shapes #2 for slots 0–3                      	-
irq_handler5  	$4F     Y := $5C                                     	$5C
irq_handler6  	$59     Shapes #3 for slots 0–3                      	-
irq_handler7  	$63     Y := $71                                     	$71
irq_handler8  	$6D     Shapes #4 for slots 0–3                      	-
irq_handler9  	$77     Y := $86                                     	$86
irq_handler10 	$81     Shapes #5 for slots 0–3                      	-
irq_handler11 	$8B     Y := $9B                                     	$9B
irq_handler12 	$95     Shapes #6 for slots 0–3                     	-
irq_handler13 	$9F     Y := $B0                                     	$B0
irq_handler14 	$A9     Shapes #7 for slots 0–3                      	-
irq_handler15 	$B1     Y := $C5                                     	$C5
irq_handler16 	$BD     Shapes #8 for slots 0–3                      	-
irq_handler17 	$C3     Switch to text UI; sprites 0–6 offscreen;
						vector → irq_handler1; Y := $FD               	$FD

Notes
  • Alternation pattern: shape-band → Y-step → shape-band → Y-step 
  • Each Y-step advances +21 scanlines from previous visible row.
  • Cursor handled by irq_handler2 and UI logic in irq_handler1.
  • irq_handler17 terminates the frame and restarts the sequence.
================================================================================
*/
#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "input_scan.asm"
#import "ui_messages.asm"
#import "ui_interaction.asm"
#import "misc.asm"
#import "script_primitives.asm"
#import "sound_engine.asm"

* = $166F
cursor_color_period:
	.byte $06
	
* = $1670
cursor_color_idx_first:
	.byte $05
	
* = $1671
// ------------------------------------------------------------
// Cursor color cycling table
// Defines the repeating color sequence used for the animated
// blinking cursor. The pattern alternates light and dark tones
// to create a pulsing visual effect.
// ------------------------------------------------------------
cursor_colors:
        .byte $01, $0F, $0C, $0B, $0C, $0F    

.label temp_y                      = $0FEC   // Saved Y during IRQ
.label temp_a                      = $0FED   // Saved A during IRQ
.label temp_x                      = $0FEE   // Saved X during IRQ

.label cpu_port_shadow     = $26   // ZP copy of $01


// ------------------------------------------------------------
// Raster line compare values for chained IRQ handlers
// ------------------------------------------------------------
.const NEXT_RASTER_LINE_H2       = $29    // Raster compare value for irq_handler2
.const NEXT_RASTER_LINE_H3       = $3A    // Raster compare value for irq_handler3
.const NEXT_RASTER_LINE_H4       = $45    // Raster compare value for irq_handler4
.const NEXT_RASTER_LINE_H5       = $4F    // Raster compare value for irq_handler5
.const NEXT_RASTER_LINE_H6       = $59    // Raster compare value for irq_handler6
.const NEXT_RASTER_LINE_H7       = $63    // Raster compare value for irq_handler7
.const NEXT_RASTER_LINE_H8       = $6D    // Raster compare value for irq_handler8
.const NEXT_RASTER_LINE_H9       = $77    // Raster compare value for irq_handler9
.const NEXT_RASTER_LINE_H10      = $81    // Raster compare value for irq_handler10
.const NEXT_RASTER_LINE_H11      = $8B    // Raster compare value for irq_handler11
.const NEXT_RASTER_LINE_H12      = $95    // Raster compare value for irq_handler12
.const NEXT_RASTER_LINE_H13      = $9F    // Raster compare value for irq_handler13
.const NEXT_RASTER_LINE_H14      = $A9    // Raster compare value for irq_handler14
.const NEXT_RASTER_LINE_H15      = $B1    // Raster compare value for irq_handler15
.const NEXT_RASTER_LINE_H16      = $BD    // Raster compare value for irq_handler16
.const NEXT_RASTER_LINE_H17      = $C3    // Raster compare value for irq_handler17
.const NEXT_RASTER_LINE_H1       = $FB    // Raster compare value to restart chain at irq_handler1

// ------------------------------------------------------------
// Sprite Y-position rows (21-pixel increments)
// ------------------------------------------------------------
.const SPRITES_Y_ROW1            = $32    // Sprite Y for band 1 (top visible row)
.const SPRITES_Y_ROW2            = $47    // Sprite Y for band 2 (= $32 + 21)
.const SPRITES_Y_ROW3            = $5C    // Sprite Y for band 3 (= $47 + 21)
.const SPRITES_Y_ROW4            = $71    // Sprite Y for band 4 (= $5C + 21)
.const SPRITES_Y_ROW5            = $86    // Sprite Y for band 5 (= $71 + 21)
.const SPRITES_Y_ROW6            = $9B    // Sprite Y for band 6 (= $86 + 21)
.const SPRITES_Y_ROW7            = $B0    // Sprite Y for band 7 (= $9B + 21)
.const SPRITES_Y_ROW8            = $C5    // Sprite Y for band 8 (= $B0 + 21)
.const SPRITES_Y_OFFSCREEN       = $FD    // Hide sprites: park Y below visible area

// ------------------------------------------------------------
// Sprite color and index constants
// ------------------------------------------------------------
.const COL_DARK_GRAY             = $0B    // Sprite color nibble for dark gray
.const SPR_LAST_ACTOR            = $06    // Highest actor sprite index in loop (0..6)

// ------------------------------------------------------------
// Cursor sprite slots in screen banks
// ------------------------------------------------------------
.const CURSOR_SHAPE_SLOT_BANK0   = $CBFF  // Cursor shape index slot (screen bank 0)
.const CURSOR_SHAPE_SLOT_BANK1   = $CFFF  // Cursor shape index slot (screen bank 1)


// ------------------------------------------------------------
// Sprite shape index presets (8 entries, +$04 stride)
// ------------------------------------------------------------
.const SHAPE_SET1_START         = $80   // Shape set #1 indices: $80,$84,$88,$8C,$90,$94,$98,$9C
.const SHAPE_SET2_START         = $A0   // Shape set #2 indices: $A0,$A4,$A8,$AC,$B0,$B4,$B8,$BC
.const SHAPE_STRIDE             = $04   // Increment between consecutive shape indices
.const SHAPE_COUNT              = $08   // Number of sprite shapes in each set

// Target sprite shape-set identifiers
.const SPRITE_TGT_SHAPESET_NONE = $00   // No pending shapeset change
.const SPRITE_TGT_SHAPESET_1    = $01   // Select shapeset #1 ($CBF8)
.const SPRITE_TGT_SHAPESET_2    = $02   // Select shapeset #2 ($CFF8)

// ------------------------------------------------------------
// Cursor off-screen coordinate sentinel
// ------------------------------------------------------------
.const CURSOR_OFFSCREEN_COORDS  = $00   // Hide cursor: park hi/lo/Y at coordinate 0


/*
================================================================================
  irq_handler1 - Top-of-frame raster “director”
================================================================================

Summary
	Frame entry IRQ that sets the VIC-II baseline, handles optional video
	layout/colour updates, seeds sprite shapes/positions for the multiplexed
	bands, and gates a small UI/cursor/sound service block before returning.
	
Global Inputs
	irq_sync_lock                  One-shot gate: if set, handler clears it and exits early
	video_update_signal             Main-thread request for full video/layout update this frame
	target_sprite_shapeset          Pending sprite shape-set to apply (NONE/SET1/SET2)
	actor_sprite_x/y_lo[]           Precomputed on-screen X/Y low bytes for up to 4 visible sprites
	relative_sprite_for_actor[]     Actor→rank map (0..3) determining draw order / HW sprite mapping
	video_setup_mode                Layout/copy mode selector (NONE, C800, CC00, COPY_COLORS)
	room_bg0_shadow                 Shadow of background color 0 to mirror after layout changes
	game_paused_flag                Nonzero when gameplay updates should be skipped
	cursor_color_wait_ticks         Frames remaining before next cursor color change
	cursor_color_period             Reload value for the cursor color wait counter
	cursor_color_idx                Current index into cursor_colors[] sequence
	cursor_color_idx_first          first/lowest valid index in cursor_colors[] (wrap target)
	cursor_colors[]                 Palette sequence for animated cursor color cycling
	control_mode                    Input/cutscene mode; CUTSCENE hides/parks cursor
	hide_cursor_flag                Forces cursor off-screen regardless of mode when nonzero
	reload_sound_ptrs_flag          If nonzero, music pointers already valid → skip reload
	selected_music_idx                     Currently selected music entry to resolve pointers from
	sound_ptr_hi/lo_tbl[]           Lookup tables to build music_to_start_ptr (HI/LO bytes)

Global Outputs
	vic_screen_control_reg_1        $D011 baseline preset (display on, 25 rows, V-scroll=3)
	vic_bg0_reg                     $D021 background color 0 (set to black on entry)
	irq_entry_count                 Incremented each frame to track handler #1 entries
	color_ram_copy_done             Cleared at start; set after color RAM copy completes
	sprite_shape_1..8				Eight sprite shape indices (set by selected shapeset)
	sprite_0..3_x_lo				Seed X low bytes for the four multiplexed hardware sprites
	sprite_0..3_x_hi				Seed X high bits for the four multiplexed hardware sprites
	vic_sprite_idx_0..3			Active rank→HW-sprite mapping written for this frame
	target_sprite_shapeset          Cleared to NONE after applying a shapeset switch
	vic_memory_layout_shadow        Shadow of $D018 updated to selected screen/charset layout
	sprite_shape_data_ptr           Pointer to sprite shape base (CBF8/CFF8) for this layout
	room_bg1_shadow                 Mirrored from BG0 shadow to keep UI/status colors consistent
	video_update_signal             Cleared to 0 after handling frame’s video update request
	new_sound_instructions_allowed  Set to 1 to let the sound system process new commands
	video_processed_signal          Cleared to signal the main thread that video work is done
	ui_section_semaphore            Incremented/ decremented to gate single-entry UI/cursor/sound block
	cursor_sprite_color             Updated from cursor_colors[] to animate cursor hue
	cursor_sprite_x_hi/lo           Cursor sprite X position (hi/lo), updated or parked off-screen
	cursor_sprite_y                 Cursor sprite Y position, updated or parked off-screen
	music_to_start_ptr              Resolved music start address written before calling sound IRQ

Description
	• Save/Map: Save A/X/Y and the processor port, then map I/O.
	• Lock gate: If irq_sync_lock is set, clear it and exit (one-shot unlock).
	• VIC baseline: Write $D011 preset (display on, 25 rows, V-scroll=3) and
	set background colour 0 to black.
	• Book-keeping: Increment entry counter and clear the “colour RAM copied”
	latch for this frame.
	• Fast path: If video_update_signal == 0, refresh sprites X-hi and chain
	the mid-frame band IRQs, enable IRQs, and jump to the “processed” clear.
	• Shapes (optional): If a shape-set switch is pending, write eight sprite
	shape indices from SHAPE_SET{1,2}_START (stride +4).
	• Seed sprite state: Copy X-lo/X-hi for sprites 0–3 and the relative
	sprite ranks (0..3), then clear the shapeset target flag.
	• Chain bands: Call compose_sprite_x_msb_and_chain_irq and enable IRQs.
	• Layout modes: Apply video_setup_mode:
		- C800 → $D018 shadow := VIC_LAYOUT_C800; shapes @ SPRITE_SHAPE_SET1_ADDR
		- CC00 → $D018 shadow := VIC_LAYOUT_CC00; shapes @ SPRITE_SHAPE_SET2_ADDR
		- COPY_COLORS → copy colour RAM only
	Then mirror BG0 shadow into BG1 and reset video_setup_mode.
	• Signals: Clear video_update_signal; allow new sound instructions; clear
	video_processed_signal for the main thread.
	• UI block (gated by ui_section_semaphore):
		- If not paused: update top-bar messages; tick paused scripts; step the
		cursor colour using cursor_colors[] with a wait counter and wrap.
		- Scan keyboard.
		- If no mid-frame colour copy: update cursor position/IA unless in
		CONTROL_MODE_CUTSCENE; optionally hide cursor if hide_cursor_flag.
		- Click latch: propagate room_scene_clicked_flag into the verb trigger
		if one isn’t already latched.
		- Sound: optionally reload music pointer from tables and call the sound
		IRQ handler.
		Finally, release the semaphore.
================================================================================
*/
* = $167A
irq_handler1:
        // ------------------------------------------------------------
        // Save A, X, Y, and processor port; map I/O
        // ------------------------------------------------------------
        pha
        txa
        pha
        tya
        pha
        lda     cpu_port
        pha
        ldy     #MAP_IO_IN
        sty     cpu_port

        // ------------------------------------------------------------
        // IRQ lock gate: if set, clear and exit
        // ------------------------------------------------------------
        lda     irq_sync_lock
        beq     h1_enabled
		
        lda     #LOCK_CLEAR
        sta     irq_sync_lock
        jmp     h1_exit

h1_enabled:
        // ------------------------------------------------------------
        // VIC-II baseline: 25 rows, vscroll=3; background=black
        // ------------------------------------------------------------
        lda     #VIC_CTRL_DEFAULT_1
        sta     vic_screen_control_reg_1
        lda     #COLOR_BLACK
        sta     vic_bg0_reg

        // ------------------------------------------------------------
        // Book-keeping: entry count and color RAM copy flag
        // ------------------------------------------------------------
        inc     irq_entry_count
        lda     #FALSE
        sta     color_ram_copy_done

        // ------------------------------------------------------------
        // Check whether a full video update is needed.
		//
        // If video_update_signal == 0 → skip heavy frame work and take
        // the fast path; otherwise branch to h1_video_update.
        // ------------------------------------------------------------
        lda     video_update_signal          // load frame-update request flag
        bne     h1_video_update              // nonzero → perform full video update

        // ------------------------------------------------------------
		// Fast path
		//
        // Refresh sprite X-hi and chain band handlers, enable interrupts,
		// then jump to clear the “video processed” signal for the main thread.
        // ------------------------------------------------------------
        jsr     compose_sprite_x_msb_and_chain_irq
        cli
        jmp     h1_clear_video_processed

        // ============================================================
        // Handle pending sprite shape-set switch.
		//
        // If target_sprite_shapeset == NONE → skip switch and proceed.
        // Otherwise, select between SHAPESET_1 and SHAPESET_2.
        // ============================================================
h1_video_update:
        lda     target_sprite_shapeset
        bne     h1_check_shapes
        jmp     h1_prepare_next

h1_check_shapes:
        cmp     #SPRITE_TGT_SHAPESET_1
        bne     h1_set_shapes_2

        // ------------------------------------------------------------
        // Shape set #1: $80,$84,$88,$8C,$90,$94,$98,$9C
        // ------------------------------------------------------------
h1_set_shapes_1:
        lda     #SHAPE_SET1_START
        sta     sprite_shape_1
        lda     #SHAPE_SET1_START + 4
        sta     sprite_shape_2
        lda     #SHAPE_SET1_START + 8
        sta     sprite_shape_3
        lda     #SHAPE_SET1_START + 12
        sta     sprite_shape_4
        lda     #SHAPE_SET1_START + 16
        sta     sprite_shape_5
        lda     #SHAPE_SET1_START + 20
        sta     sprite_shape_6
        lda     #SHAPE_SET1_START + 24
        sta     sprite_shape_7
        lda     #SHAPE_SET1_START + 28
        sta     sprite_shape_8
        jmp     h1_seed_x_and_ranks

        // ------------------------------------------------------------
        // Shape set #2: $A0,$A4,$A8,$AC,$B0,$B4,$B8,$BC
        // ------------------------------------------------------------
h1_set_shapes_2:
        lda     #SHAPE_SET2_START
        sta     sprite_shape_1
        lda     #SHAPE_SET2_START + 4
        sta     sprite_shape_2
        lda     #SHAPE_SET2_START + 8
        sta     sprite_shape_3
        lda     #SHAPE_SET2_START + 12
        sta     sprite_shape_4
        lda     #SHAPE_SET2_START + 16
        sta     sprite_shape_5
        lda     #SHAPE_SET2_START + 20
        sta     sprite_shape_6
        lda     #SHAPE_SET2_START + 24
        sta     sprite_shape_7
        lda     #SHAPE_SET2_START + 28
        sta     sprite_shape_8

        // ------------------------------------------------------------
        // Seed sprite coordinates and ranking.
		//
        // Load X-low bytes for sprites 0–3 from actor tables,
        // followed by relative sprite indices (render order) and
        // X-high bytes. Establishes initial on-screen positions for
        // the active sprites before chaining the next IRQ.
        // ------------------------------------------------------------
h1_seed_x_and_ranks:
        // X-lo
        lda     actor_sprite_x_lo
        sta     sprite_0_x_lo
        lda     actor_sprite_x_lo+1
        sta     sprite_1_x_lo
        lda     actor_sprite_x_lo+2
        sta     sprite_2_x_lo
        lda     actor_sprite_x_lo+3
        sta     sprite_3_x_lo

        // relative ranks (0..3)
        lda     relative_sprite_for_actor
        sta     vic_sprite_idx_0
        lda     relative_sprite_for_actor+1
        sta     vic_sprite_idx_1
        lda     relative_sprite_for_actor+2
        sta     vic_sprite_idx_2
        lda     relative_sprite_for_actor+3
        sta     vic_sprite_idx_3

        // X-hi
        lda     actor_sprite_x_hi
        sta     sprite_0_x_hi
        lda     actor_sprite_x_hi+1
        sta     sprite_1_x_hi
        lda     actor_sprite_x_hi+2
        sta     sprite_2_x_hi
        lda     actor_sprite_x_hi+3
        sta     sprite_3_x_hi

        // clear shape-set switch flag
        lda     #SPRITE_TGT_SHAPESET_NONE
        sta     target_sprite_shapeset

        // ------------------------------------------------------------
        // Refresh X-hi + chain band IRQs; enable IRQs
        // ------------------------------------------------------------
h1_prepare_next:
        jsr     compose_sprite_x_msb_and_chain_irq
        cli
        // ============================================================

        // ============================================================
        // Apply video_setup_mode
        //
        // Selects the video memory layout and sprite-shape base for the
        // next frame, depending on the mode:
        //
        //   VID_SETUP_NONE
        //       → no change; retain current VIC layout.
        //
        //   VID_SETUP_C800
        //       → character data at $D800,
        //         screen matrix at $C800,
        //         sprite shape data starting at $CBF8,
        //         then copy color RAM to match.
        //
        //   VID_SETUP_CC00
        //       → character data at $D800,
        //         screen matrix at $CC00,
        //         sprite shape data starting at $CFF8,
        //         then copy color RAM to match.
        //
        //   VID_SETUP_COPY_COLORS
        //       → only refresh color RAM, no layout change.
        // ============================================================
        lda     video_setup_mode
        beq     h1_video_done
        cmp     #VID_SETUP_COPY_COLORS
        bne     h1_check_case_c800

        // ------------------------------------------------------------
        // Handle VID_SETUP_COPY_COLORS.
		//
        // Only copy color RAM to refresh the visible palette data;
        // no change to VIC screen or character memory layout.
        // ------------------------------------------------------------
        jsr     copy_vic_color_ram
        jmp     h1_layout_complete

h1_check_case_c800:
        cmp     #VID_SETUP_C800
        bne     h1_case_cc00

        // ------------------------------------------------------------
        // Handle VID_SETUP_C800.
		//
        // Configure VIC memory layout and sprite-shape base:
        //   - $D018 (shadow) := VIC_LAYOUT_C800  → chars @ $D800, screen @ $C800
        //   - sprite_shape_data_ptr := SPRITE_SHAPE_SET1_ADDR (CBF8)
        //   - Then copy color RAM so colors match the new screen layout
        // ------------------------------------------------------------
        lda     #VIC_LAYOUT_C800
        sta     vic_memory_layout_shadow
        lda     #<SPRITE_SHAPE_SET1_ADDR
        sta     sprite_shape_data_ptr
        lda     #>SPRITE_SHAPE_SET1_ADDR
        sta     sprite_shape_data_ptr + 1
        jsr     copy_vic_color_ram
        jmp     h1_layout_complete

        // ------------------------------------------------------------
        // Handle VID_SETUP_CC00.
        // Configure VIC memory layout and sprite-shape base:
        //   - $D018 (shadow) := VIC_LAYOUT_CC00  → chars @ $D800, screen @ $CC00
        //   - sprite_shape_data_ptr := SPRITE_SHAPE_SET2_ADDR (CFF8)
        //   - Then copy color RAM so colors match the new screen layout
        // ------------------------------------------------------------
h1_case_cc00:
        lda     #VIC_LAYOUT_CC00
        sta     vic_memory_layout_shadow
        lda     #<SPRITE_SHAPE_SET2_ADDR
        sta     sprite_shape_data_ptr
        lda     #>SPRITE_SHAPE_SET2_ADDR
        sta     sprite_shape_data_ptr + 1 
        jsr     copy_vic_color_ram

        // ------------------------------------------------------------
        // Finalize layout change.
        // - Mirror BG0 shadow into BG1 shadow for UI/status consistency.
        // - Clear video_setup_mode so no further layout work is pending.
        // ------------------------------------------------------------
h1_layout_complete:
        // Mirror background color across register shadows
        lda     room_bg0_shadow
        sta     room_bg1_shadow

        // Reset mode selector: no pending layout next frame
        lda     #VID_SETUP_NONE
        sta     video_setup_mode
        // ============================================================

h1_video_done:
        // ------------------------------------------------------------
        // Clear update signal and allow new sound instructions
        // ------------------------------------------------------------
        lda     #SIGNAL_CLEAR
        sta     video_update_signal
        lda     #TRUE
        sta     new_sound_instructions_allowed

        // ------------------------------------------------------------
        // Clear video_processed signal for main thread
        // ------------------------------------------------------------
h1_clear_video_processed:
        lda     #SIGNAL_CLEAR
        sta     video_processed_signal

        // ------------------------------------------------------------
        // Single-entry gate for the UI/cursor/sound section:
        // if semaphore == 0 → enter section; otherwise skip to exit.
        // ------------------------------------------------------------
        lda     ui_section_semaphore          // load gate flag (0 = free, ≠0 = busy)
        beq     h1_section_entry              // free → enter and claim inside
        jmp     h1_exit                       // busy → skip this work this frame		

h1_section_entry:
        inc     ui_section_semaphore          // claim semaphore; mark UI/cursor/sound section as busy

        // If the game is not paused, perform regular per-frame updates:
        // - update top-bar messages
        // - tick paused scripts
        // - handle cursor color cycling
        lda     game_paused_flag              // check pause state (0 = running)
        bne     h1_keyboard                   // if paused → skip these updates
		
        jsr     tick_topbar_message
        jsr     handle_paused_tasks

        // Cursor color stepper
        dec     cursor_color_wait_ticks       // decrement frame wait counter before next color change
        bpl     h1_keyboard                   // if still positive → not yet time, skip update

        lda     cursor_color_period           // reload full wait period
        sta     cursor_color_wait_ticks       // reset countdown timer

        dec     cursor_color_idx              // advance to next color in the sequence
        bpl     h1_set_cursor_color           // if still ≥ 0 → valid index, apply color
        lda     cursor_color_idx_first        // wrap around to first color index
        sta     cursor_color_idx              // reset color sequence

h1_set_cursor_color:
        ldx     cursor_color_idx              // X := current color index
        lda     cursor_colors,x               // load color value from palette table
        sta     cursor_sprite_color           // update cursor sprite’s visible color

h1_keyboard:
        // keyboard scan
        jsr     kbd_scan

        // ------------------------------------------------------------
        // Ensure I/O mapped; reset click; cursor update or cutscene hide
        // ------------------------------------------------------------
        ldy     #MAP_IO_IN
        sty     cpu_port
        lda     #FALSE                         // clear click latch for this frame (no click yet)
        sta     room_scene_clicked_flag        // room_scene_clicked_flag := False

        lda     color_ram_copy_done            // was color RAM just copied this frame?
        bne     h1_check_hide_cursor_flag      // yes → skip cursor updates to avoid mid-frame artifacts

        lda     control_mode                   // load current control/input mode
        cmp     #CONTROL_MODE_CUTSCENE         // are we in cutscene mode?
        beq     h1_cutscene                    // yes → park/hide the cursor via cutscene path

        // cursor + interaction area
        jsr     step_cursor_and_dispatch_hotspot
        jsr     compute_corrected_cursor_pos
        sta     cursor_sprite_x_hi
        stx     cursor_sprite_x_lo
        sty     cursor_sprite_y
        jmp     h1_check_hide_cursor_flag

h1_cutscene:
        // park cursor off-screen
        lda     #CURSOR_OFFSCREEN_COORDS
        sta     cursor_sprite_x_hi
        sta     cursor_sprite_x_lo
        sta     cursor_sprite_y

        // hide cursor if requested
h1_check_hide_cursor_flag:
        lda     hide_cursor_flag
        beq     h1_click_latch
		
        lda     #CURSOR_OFFSCREEN_COORDS
        sta     cursor_sprite_x_hi
        sta     cursor_sprite_x_lo
        sta     cursor_sprite_y

        // ------------------------------------------------------------
        // Click latch and sound handling
        // ------------------------------------------------------------
h1_click_latch:
        lda     forced_sentence_trigger        // A := current click/verb trigger state (0 = none, 1 = pending)
        cmp     #$01                          // is a trigger already latched?
        beq     h1_sound                      // yes → keep existing trigger; skip latching

        lda     room_scene_clicked_flag       // A := per-frame room click latch (0/1)
        sta     forced_sentence_trigger        // propagate click into verb system trigger

h1_sound:
        // map I/O; optionally reload music pointers; call sound IRQ
        ldy     #MAP_IO_IN
        sty     cpu_port
        lda     reload_sound_ptrs_flag        // reload needed? (≠0 → pointers already valid)
        bne     h1_clear_semaphore            // if set, skip reloading and proceed to exit

        ldx     selected_music_idx                   // X := current music selection
        lda     sound_ptr_hi_tbl,x            // A := high byte of music start address
        sta     music_to_start_ptr_hi           // store into pointer (engine’s chosen slot for HI)
        lda     sound_ptr_lo_tbl,x            // A := low byte of music start address
        sta     music_to_start_ptr_lo           // store into pointer (engine’s chosen slot for LO)

        jsr     sound_irq_handler             // kick per-frame sound IRQ with updated music pointer

        // ------------------------------------------------------------
        // Release semaphore and exit
        // ------------------------------------------------------------
h1_clear_semaphore:
        dec     ui_section_semaphore

        // ------------------------------------------------------------
        // Restore CPU state and return from IRQ.
        // Reinstall cpu_port mapping, then restore Y, X, and A in the
        // reverse save order before executing RTI.
        // ------------------------------------------------------------
h1_exit:
        pla
        sta     cpu_port
        pla
        tay
        pla
        tax
        pla
        rti
/*
================================================================================
  irq_handler2 - Raster IRQ at top band: sprite X/Y, shapes, colors, next IRQ
================================================================================

Summary
	Service the early-frame raster interrupt. Update hardware sprite X for slots
	0–6, cursor X, horizontal expansion, shapes for sprites 0–3, colors for 0–6,
	cursor color, and Y positions. Acknowledge the VIC interrupt, program the
	next raster line, and switch the IRQ vector to irq_handler3.

Global Inputs
	cpu_port_shadow                 saved cpu_port image
	vic_sprite_idx_0..6             actor→VIC slot mapping (0–7)
	sprite_0_x_lo..sprite_6_x_lo    X low bytes for actor sprites
	cursor_sprite_x_lo              cursor X low byte
	sprite_horizontal_expansion     shadow of $D01D
	sprite_shape_1                  base shape index for 0–3 strip
	sprite_shape_cursor             cursor shape index
	sprite_0_color..sprite_6_color  per-sprite color nibbles
	cursor_sprite_color             cursor color nibble
	cursor_sprite_y                 cursor Y position
	sprite_shape_data_ptr           pointer to sprite shape table
	global_lights_state                   0=dark mode, ≠0=normal

Global Outputs
	irq_handler						next IRQ handler vector (set to irq_handler3)

Description
	* Save A/X/Y and snapshot cpu_port, then map I/O by writing MAP_IO_IN to cpu_port.
	* For each vic_sprite_idx_0..6:
	  • compute VIC X register offset (index*2) and write sprite_n_x_lo.
	* Write cursor X low byte and horizontal expansion control.
	* Write shape indices for the 0–3 strip via (sprite_shape_data_ptr),Y.
	* Write the cursor shape into both screen-bank cursor slots.
	* If global_lights_state==0, set sprites 0–6 to dark gray; else apply per-sprite
	  colors. Cursor color set unconditionally afterward.
	* Set cursor Y; set sprites 0–6 Y to SPRITES_Y_ROW1.
	* Acknowledge the VIC IRQ, set next raster line to NEXT_RASTER_LINE_H3, read
	  CIA1 IRQ status to clear edge, and update the IRQ vector to irq_handler3.
	* Restore cpu_port, restore A/X/Y, and RTI.

Notes
	* Uses packed VIC color registers at $D027+slot and X-low at $D000+slot*2.
	* Dark-mode color constant COL_DARK_GRAY=$0B. Raster ack value $81.
	* Assumes vic_sprite_idx_* never maps an actor to slot 7 (cursor).
================================================================================
*/
* = $10A1
irq_handler2:
        // ------------------------------------------------------------
        // Save A/X/Y and current cpu_port, then map I/O
        // ------------------------------------------------------------
        sta     temp_a
        stx     temp_x
        sty     temp_y
        ldy     cpu_port
        sty     cpu_port_shadow
        ldy     #MAP_IO_IN
        sty     cpu_port

        // ------------------------------------------------------------
        // Sprite 0–6: write X low bytes using relative sprite indices
        // ------------------------------------------------------------
        lda     vic_sprite_idx_0
        asl                                      // multiply sprite index by 2 → select proper VIC X register
        tay
        lda     sprite_0_x_lo                    // load X low byte for actor’s assigned sprite
        sta     vic_sprite0_x_lo_reg,y           // write into VIC sprite X low register for that slot

        lda     vic_sprite_idx_1
        asl
        tay
        lda     sprite_1_x_lo
        sta     vic_sprite0_x_lo_reg,y

        lda     vic_sprite_idx_2
        asl
        tay
        lda     sprite_2_x_lo
        sta     vic_sprite0_x_lo_reg,y

        lda     vic_sprite_idx_3
        asl
        tay
        lda     sprite_3_x_lo
        sta     vic_sprite0_x_lo_reg,y

        lda     vic_sprite_idx_4
        asl
        tay
        lda     sprite_4_x_lo
        sta     vic_sprite0_x_lo_reg,y

        lda     vic_sprite_idx_5
        asl
        tay
        lda     sprite_5_x_lo
        sta     vic_sprite0_x_lo_reg,y

        lda     vic_sprite_idx_6
        asl
        tay
        lda     sprite_6_x_lo
        sta     vic_sprite0_x_lo_reg,y

        // ------------------------------------------------------------
        // Cursor (sprite 7): X low byte and horizontal expansion
        // ------------------------------------------------------------
        ldy     cursor_sprite_x_lo
        sty     vic_sprite7_x_lo_reg

        ldy     sprite_horizontal_expansion
        sty     vic_sprite_x_expand_reg

        // ------------------------------------------------------------
        // Update shapes for sprites 0–3 from sprite_shape_1..+3
        // ------------------------------------------------------------
        ldx     sprite_shape_1                    // X := base shape index for sprites 0–3

        ldy     vic_sprite_idx_0                  // Y := VIC sprite slot for first sprite in strip
        txa                                       // A := current shape index
        sta     (sprite_shape_data_ptr),y         // write shape index to shape table for slot Y
        inx                                       // X := next shape index for subsequent sprite

        ldy     vic_sprite_idx_1
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_2
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_3
        txa
        sta     (sprite_shape_data_ptr),y

        // ------------------------------------------------------------
        // Cursor shape: write to CURSOR_SHAPE_SLOT_BANK0/1 for both screen banks
        // ------------------------------------------------------------
        ldy     sprite_shape_cursor
        sty     CURSOR_SHAPE_SLOT_BANK0                 // bank 0 cursor slot
        sty     CURSOR_SHAPE_SLOT_BANK1                 // bank 1 cursor slot

        // ------------------------------------------------------------
        // Colors: lights off → force dark gray; else per-sprite colors
        // ------------------------------------------------------------
        lda     global_lights_state
        beq     set_all_sprites_to_dark_gray

        // ------------------------------------------------------------
        // Lights normal: write colors for sprites 0–6
        // ------------------------------------------------------------
        lda     vic_sprite_idx_0                  // A := hardware sprite slot for actor 0 (0–7)
        tay                                       // Y := slot index for VIC color register offset
        lda     sprite_0_color                    // A := color nibble for actor 0
        sta     vic_sprite0_color_reg,y           // write color to VIC sprite-n color ($D027+Y)

        lda     vic_sprite_idx_1
        tay
        lda     sprite_1_color
        sta     vic_sprite0_color_reg,y

        lda     vic_sprite_idx_2
        tay
        lda     sprite_2_color
        sta     vic_sprite0_color_reg,y

        lda     vic_sprite_idx_3
        tay
        lda     sprite_3_color
        sta     vic_sprite0_color_reg,y

        lda     vic_sprite_idx_4
        tay
        lda     sprite_4_color
        sta     vic_sprite0_color_reg,y

        lda     vic_sprite_idx_5
        tay
        lda     sprite_5_color
        sta     vic_sprite0_color_reg,y

        lda     vic_sprite_idx_6
        tay
        lda     sprite_6_color
        sta     vic_sprite0_color_reg,y
        jmp     set_cursor_color

set_all_sprites_to_dark_gray:
        // ------------------------------------------------------------
        // Force sprites 0–6 color to dark gray
		//
        // Loops from X=6 down to 0, writing $0B to each sprite color register.
        // Cursor (sprite 7) excluded; its color set separately afterward.
        // ------------------------------------------------------------
        ldx     #SPR_LAST_ACTOR                   // X := 6 (last actor sprite index)
loop_dark:
        lda     #COL_DARK_GRAY                    // A := dark gray color value ($0B)
        sta     vic_sprite0_color_reg,x           // write to VIC sprite-n color register ($D027+X)
        dex                                       // decrement X → previous sprite
        bpl     loop_dark                         // loop until all 7 actor sprites are updated

set_cursor_color:
        // ------------------------------------------------------------
        // Cursor color independent of lights
        // ------------------------------------------------------------
        ldy     cursor_sprite_color
        sty     vic_sprite7_color_reg

        // ------------------------------------------------------------
        // Y positions: cursor dynamic; sprites 0–6 fixed at first row
        // ------------------------------------------------------------
        ldy     cursor_sprite_y
        sty     vic_sprite7_y_reg

        ldy     #SPRITES_Y_ROW1
        sty     vic_sprite0_y_reg
        sty     vic_sprite1_y_reg
        sty     vic_sprite2_y_reg
        sty     vic_sprite3_y_reg
        sty     vic_sprite4_y_reg
        sty     vic_sprite5_y_reg
        sty     vic_sprite6_y_reg

        // ------------------------------------------------------------
        // Ack current IRQ, program next raster line and handler vector
        // ------------------------------------------------------------
        ldy     #VIC_IRQ_RASTER_ACK                  // Y := bitmask to acknowledge current raster IRQ
        sty     vic_irq_flag_reg                     // clear VIC raster interrupt flag

        ldy     #NEXT_RASTER_LINE_H3                 // Y := target raster line for next interrupt
        sty     vic_raster_line_reg                  // program next raster compare line
        ldy     cia1_irq_status_reg                  // dummy read → clear pending CIA1 IRQ edge latch

		//Set next IRQ handler as irq_handler3
        ldy  #<irq_handler3                        
        sty     irq_handler                         
        ldy  #>irq_handler3                        
        sty     irq_handler + 1

        // ------------------------------------------------------------
        // Restore cpu_port and A/X/Y; return from interrupt
        // ------------------------------------------------------------
        ldy     cpu_port_shadow
        sty     cpu_port

        lda     temp_a
        ldx     temp_x
        ldy     temp_y
        rti

/*
================================================================================
irq_handler3 - Raster IRQ at $3A: switch to multicolor text, step sprites, arm H4
================================================================================

Summary
  Handle raster line $3A. Enable multicolor text mode, then move sprites 0–6
  down by one sprite height to Y=$47. Acknowledge the IRQ and arm the next
  handler (irq_handler4) at raster line $45.

Global Inputs
  vic_memory_layout_shadow         shadow of vic_memory_layout_reg (video matrix + charset)
  irq_handler4                     address of next IRQ handler

Global Outputs
  irq_handler				       IRQ vector updated to irq_handler4

Description
  - Save A and Y, snapshot cpu_port, and map I/O.
  - Program vic_screen_control_reg_1 = CTRL1_ROOM_REGION_PRESET.
  - Wait 3 cycles to align the multicolor switch window.
  - Copy vic_memory_layout_shadow → vic_memory_layout_reg
  - Set vic_screen_control_reg_2 = CTRL2_ROOM_REGION_PRESET
  - Wait 10 cycles, then set sprites 0–6 Y to SPRITES_Y_ROW2 ($47).
  - Ack VIC raster IRQ, set next raster to NEXT_RASTER_LINE_H4 ($45).
  - Read CIA1 IRQ status to clear edge. Point vector to irq_handler4.
  - Restore cpu_port, Y, A, and rti.

Notes
  - Multicolor text requires vic_screen_control_reg_2 bit4=1 while 
	vic_screen_control_reg_1 bit5=0.
  - The Y step from $32 to $47 equals 21 scanlines (sprite height).
================================================================================
*/
* = $11D2
irq_handler3:
        // ------------------------------------------------------------
        // Save A and Y. Snapshot cpu_port and map I/O ($01 := $25).
        // ------------------------------------------------------------
        sty     temp_y
        sta     temp_a
        ldy     cpu_port
        sty     cpu_port_shadow
        ldy     #MAP_IO_IN
        sty     cpu_port

        // ------------------------------------------------------------
        // $D011: 25 rows, vertical fine scroll = 4, display enabled
        // ------------------------------------------------------------
        ldy     #CTRL1_ROOM_REGION_PRESET
        sty     vic_screen_control_reg_1

        // ------------------------------------------------------------
        // Align to safe window for multicolor switch
        // ------------------------------------------------------------
        nop
        nop
        nop

        // ------------------------------------------------------------
        // Switch to multicolor text and apply memory layout
        //   $D018 := vic_memory_layout_shadow
        //   $D016 := multicolor on, 40 cols, hscroll 0
        // ------------------------------------------------------------
        ldy     vic_memory_layout_shadow
        lda     #CTRL2_ROOM_REGION_PRESET
        sty     vic_memory_layout_reg
        sta     vic_screen_control_reg_2

        // ------------------------------------------------------------
        // Wait to align sprite Y burst
        // ------------------------------------------------------------
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop

        // ------------------------------------------------------------
        // Sprites 0–6: set Y to $47 (21-dot step from $32)
        // ------------------------------------------------------------
        ldy     #SPRITES_Y_ROW2
        sty     vic_sprite0_y_reg
        sty     vic_sprite1_y_reg
        sty     vic_sprite2_y_reg
        sty     vic_sprite3_y_reg
        sty     vic_sprite4_y_reg
        sty     vic_sprite5_y_reg
        sty     vic_sprite6_y_reg

        // ------------------------------------------------------------
        // Acknowledge raster IRQ and arm next raster line
        // ------------------------------------------------------------
        ldy     #VIC_IRQ_RASTER_ACK
        sty     vic_irq_flag_reg
        ldy     #NEXT_RASTER_LINE_H4
        sty     vic_raster_line_reg
        ldy     cia1_irq_status_reg            // clear CIA1 edge latch

        // ------------------------------------------------------------
        // Install next handler: irq_handler4
        // ------------------------------------------------------------
        ldy  #<irq_handler4
        sty     irq_handler
        ldy  #>irq_handler4
        sty     irq_handler + 1

        // ------------------------------------------------------------
        // Restore cpu_port and registers; return from interrupt
        // ------------------------------------------------------------
        ldy     cpu_port_shadow
        sty     cpu_port
        ldy     temp_y
        lda     temp_a
        rti
		
/*
================================================================================
irq_handler4 - Raster IRQ at $45: update sprite shapes (slots 0–3), arm H5
================================================================================

Summary
  Handle raster line $45. Write a consecutive strip of four shape indices to the
  sprite shape table for hardware slots 0–3 (as mapped by vic_sprite_idx_0..3).
  Acknowledge the VIC IRQ, program the next raster line, and install irq_handler5.

Arguments

Returns
  None

Global Inputs
  sprite_shape_2                  base shape index for this band’s 4-sprite strip
  vic_sprite_idx_0..3             actor→VIC hardware slot mapping (0–7)
  sprite_shape_data_ptr           pointer to sprite shape table

Global Outputs
  irq_handler					  vector set to irq_handler5
  vic_raster_line_reg             NEXT_RASTER_LINE_H5

Description
  - Save A/X/Y.
  - X := sprite_shape_2. For each vic_sprite_idx_0..3:
      • Y := slot, A := X, store A at (sprite_shape_data_ptr),Y, then INX.
  - Snapshot cpu_port, map I/O.
  - Program next raster line = NEXT_RASTER_LINE_H5 and set handler = irq_handler5.
  - Acknowledge current raster IRQ and clear CIA1 edge latch.
  - Restore cpu_port and A/X/Y, then rti.

Notes
  - Y indexes the hardware sprite slot, not the actor index.
  - Unrolled stores minimize jitter on the raster line.
================================================================================
*/
* = $1235
irq_handler4:
        // ------------------------------------------------------------
        // Save A, X, Y
        // ------------------------------------------------------------
        sta     temp_a
        stx     temp_x
        sty     temp_y

        // ------------------------------------------------------------
        // Update sprite shapes for hardware slots 0–3
        //   X starts at sprite_shape_2 and increments per slot
        // ------------------------------------------------------------
        ldx     sprite_shape_2

        ldy     vic_sprite_idx_0
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_1
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_2
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_3
        txa
        sta     (sprite_shape_data_ptr),y

        // ------------------------------------------------------------
        // Map I/O for VIC/CIA register access
        // ------------------------------------------------------------
        ldy     cpu_port
        sty     cpu_port_shadow
        ldy     #MAP_IO_IN
        sty     cpu_port

        // ------------------------------------------------------------
        // Program next raster line and install next handler
        // ------------------------------------------------------------
        ldy     #NEXT_RASTER_LINE_H5
        sty     vic_raster_line_reg

        ldy  #<irq_handler5
        sty     irq_handler
        ldy  #>irq_handler5
        sty     irq_handler + 1

        // ------------------------------------------------------------
        // Acknowledge raster IRQ and clear CIA1 edge latch
        // ------------------------------------------------------------
        ldy     #VIC_IRQ_RASTER_ACK
        sty     vic_irq_flag_reg
        ldy     cia1_irq_status_reg

        // ------------------------------------------------------------
        // Restore cpu_port and registers, return from interrupt
        // ------------------------------------------------------------
        ldy     cpu_port_shadow
        sty     cpu_port

        lda     temp_a
        ldx     temp_x
        ldy     temp_y
        rti
		
/*
================================================================================
irq_handler5 - Raster IRQ at $4F: step sprites to row 3, arm H6
================================================================================

Summary
  Handle raster line $4F. Move sprites 0–6 down by one sprite height to Y=$5C.
  Program next raster line to $59 and install irq_handler6. Acknowledge the
  current VIC raster interrupt.
  
  Same as irq_handler3 in other details.

================================================================================
*/
* = $1289
irq_handler5:
        // ------------------------------------------------------------
        // Save Y, cpu_port and map I/O ($01 := MAP_IO_IN)
        // ------------------------------------------------------------
        sty     temp_y
        ldy     cpu_port
        sty     cpu_port_shadow
        ldy     #MAP_IO_IN
        sty     cpu_port

        // ------------------------------------------------------------
        // Sprites 0–6: set Y to $5C (row 3)
        // ------------------------------------------------------------
        ldy     #SPRITES_Y_ROW3
        sty     vic_sprite0_y_reg
        sty     vic_sprite1_y_reg
        sty     vic_sprite2_y_reg
        sty     vic_sprite3_y_reg
        sty     vic_sprite4_y_reg
        sty     vic_sprite5_y_reg
        sty     vic_sprite6_y_reg

        // ------------------------------------------------------------
        // Program next raster and next handler (irq_handler6)
        // ------------------------------------------------------------
        ldy     #NEXT_RASTER_LINE_H6
        sty     vic_raster_line_reg
        ldy  #<irq_handler6
        sty     irq_handler
        ldy  #>irq_handler6
        sty     irq_handler + 1

        // ------------------------------------------------------------
        // Acknowledge raster IRQ and clear CIA1 edge latch
        // ------------------------------------------------------------
        ldy     #VIC_IRQ_RASTER_ACK
        sty     vic_irq_flag_reg
        ldy     cia1_irq_status_reg

        // ------------------------------------------------------------
        // Restore cpu_port and Y; return from interrupt
        // ------------------------------------------------------------
        ldy     cpu_port_shadow
        sty     cpu_port
        ldy     temp_y
        rti		
/*
================================================================================
irq_handler6 - Raster IRQ at $59: update sprite shapes (slots 0–3), arm H7
================================================================================

Summary
  Handle raster line $59. Write a 4-entry consecutive shape strip starting at
  sprite_shape_3 into the sprite shape table for hardware slots selected by
  vic_sprite_idx_0..3. Program next raster to $63 and install irq_handler7.
  Acknowledge the current raster IRQ.
  
  Same as irq_handler4 in other details.
================================================================================
*/
* = $12CA
irq_handler6:
        // ------------------------------------------------------------
        // Save A, X, Y
        // ------------------------------------------------------------
        sta     temp_a
        stx     temp_x
        sty     temp_y

        // ------------------------------------------------------------
        // Write shapes for hardware slots 0–3 starting at sprite_shape_3
        // ------------------------------------------------------------
        ldx     sprite_shape_3

        ldy     vic_sprite_idx_0
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_1
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_2
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_3
        txa
        sta     (sprite_shape_data_ptr),y

        // ------------------------------------------------------------
        // Map I/O for VIC/CIA access
        // ------------------------------------------------------------
        ldy     cpu_port
        sty     cpu_port_shadow
        ldy     #MAP_IO_IN
        sty     cpu_port

        // ------------------------------------------------------------
        // Program next raster and next handler (irq_handler7)
        // ------------------------------------------------------------
        ldy     #NEXT_RASTER_LINE_H7
        sty     vic_raster_line_reg

        ldy  #<irq_handler7
        sty     irq_handler
        ldy  #>irq_handler7
        sty     irq_handler + 1

        // ------------------------------------------------------------
        // Acknowledge raster IRQ and clear CIA1 edge latch
        // ------------------------------------------------------------
        ldy     #VIC_IRQ_RASTER_ACK
        sty     vic_irq_flag_reg
        ldy     cia1_irq_status_reg

        // ------------------------------------------------------------
        // Restore cpu_port and registers; return from interrupt
        // ------------------------------------------------------------
        ldy     cpu_port_shadow
        sty     cpu_port

        lda     temp_a
        ldx     temp_x
        ldy     temp_y
        rti
/*
================================================================================
irq_handler7 - Raster IRQ at $63: step sprites to row 4, arm H8
================================================================================

Summary
  Move sprites 0–6 to Y=$71 (+21 lines from $5C). Program next raster to $6D
  and install irq_handler8. Acknowledge the current raster IRQ.

  Same as irq_handler3 in other details.
================================================================================
*/
* = $131E
irq_handler7:
        // ------------------------------------------------------------
        // Save Y and map I/O ($01 := MAP_IO_IN)
        // ------------------------------------------------------------
        sty     temp_y
        ldy     cpu_port
        sty     cpu_port_shadow
        ldy     #MAP_IO_IN
        sty     cpu_port

        // ------------------------------------------------------------
        // Sprites 0–6: set Y to $71 (row 4)
        // ------------------------------------------------------------
        ldy     #SPRITES_Y_ROW4
        sty     vic_sprite0_y_reg
        sty     vic_sprite1_y_reg
        sty     vic_sprite2_y_reg
        sty     vic_sprite3_y_reg
        sty     vic_sprite4_y_reg
        sty     vic_sprite5_y_reg
        sty     vic_sprite6_y_reg

        // ------------------------------------------------------------
        // Program next raster and next handler (irq_handler8)
        // ------------------------------------------------------------
        ldy     #NEXT_RASTER_LINE_H8
        sty     vic_raster_line_reg
        ldy  #<irq_handler8
        sty     irq_handler
        ldy  #>irq_handler8
        sty     irq_handler + 1

        // ------------------------------------------------------------
        // Acknowledge raster IRQ and clear CIA1 edge latch
        // ------------------------------------------------------------
        ldy     #VIC_IRQ_RASTER_ACK
        sty     vic_irq_flag_reg
        ldy     cia1_irq_status_reg

        // ------------------------------------------------------------
        // Restore cpu_port and Y; return from interrupt
        // ------------------------------------------------------------
        ldy     cpu_port_shadow
        sty     cpu_port
        ldy     temp_y
        rti
/*
================================================================================
irq_handler8 - Raster IRQ at $6D: update sprite shapes (slots 0–3), arm H9
================================================================================

Summary
  Handle raster line $6D. Write four consecutive shape indices starting at
  sprite_shape_4 into the sprite shape table at offsets given by
  vic_sprite_idx_0..3. Program the next raster line to $77 and install
  irq_handler9. Acknowledge the current raster IRQ.

  Same as irq_handler4 in other details.
================================================================================
*/
* = $135F
irq_handler8:
        // ------------------------------------------------------------
        // Save A, X, Y
        // ------------------------------------------------------------
        sta     temp_a
        stx     temp_x
        sty     temp_y

        // ------------------------------------------------------------
        // Write shapes for hardware slots 0–3 starting at sprite_shape_4
        // ------------------------------------------------------------
        ldx     sprite_shape_4

        ldy     vic_sprite_idx_0
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_1
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_2
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_3
        txa
        sta     (sprite_shape_data_ptr),y

        // ------------------------------------------------------------
        // Map I/O for VIC/CIA access
        // ------------------------------------------------------------
        ldy     cpu_port
        sty     cpu_port_shadow
        ldy     #MAP_IO_IN
        sty     cpu_port

        // ------------------------------------------------------------
        // Program next raster and next handler (irq_handler9)
        // ------------------------------------------------------------
        ldy     #NEXT_RASTER_LINE_H9
        sty     vic_raster_line_reg

        ldy  #<irq_handler9
        sty     irq_handler
        ldy  #>irq_handler9
        sty     irq_handler + 1

        // ------------------------------------------------------------
        // Acknowledge raster IRQ and clear CIA1 edge latch
        // ------------------------------------------------------------
        ldy     #VIC_IRQ_RASTER_ACK
        sty     vic_irq_flag_reg
        ldy     cia1_irq_status_reg

        // ------------------------------------------------------------
        // Restore cpu_port and registers; return from interrupt
        // ------------------------------------------------------------
        ldy     cpu_port_shadow
        sty     cpu_port

        lda     temp_a
        ldx     temp_x
        ldy     temp_y
        rti		
/*
================================================================================
irq_handler9 - Raster IRQ at $77: step sprites to row 5, arm H10
================================================================================

Summary
  Move sprites 0–6 to Y=$86 (+21 from $71). Program next raster to $81 and
  install irq_handler10. Acknowledge the current VIC raster interrupt.

  Same as irq_handler3 in other details.
================================================================================
*/
* = $13B3
irq_handler9:
        // ------------------------------------------------------------
        // Save Y and map I/O ($01 := MAP_IO_IN)
        // ------------------------------------------------------------
        sty     temp_y
        ldy     cpu_port
        sty     cpu_port_shadow
        ldy     #MAP_IO_IN
        sty     cpu_port

        // ------------------------------------------------------------
        // Sprites 0–6: set Y to $86 (row 5)
        // ------------------------------------------------------------
        ldy     #SPRITES_Y_ROW5
        sty     vic_sprite0_y_reg
        sty     vic_sprite1_y_reg
        sty     vic_sprite2_y_reg
        sty     vic_sprite3_y_reg
        sty     vic_sprite4_y_reg
        sty     vic_sprite5_y_reg
        sty     vic_sprite6_y_reg

        // ------------------------------------------------------------
        // Program next raster and next handler (irq_handler10)
        // ------------------------------------------------------------
        ldy     #NEXT_RASTER_LINE_H10
        sty     vic_raster_line_reg
        ldy  #<irq_handler10
        sty     irq_handler
        ldy  #>irq_handler10
        sty     irq_handler + 1

        // ------------------------------------------------------------
        // Acknowledge raster IRQ and clear CIA1 edge latch
        // ------------------------------------------------------------
        ldy     #VIC_IRQ_RASTER_ACK
        sty     vic_irq_flag_reg
        ldy     cia1_irq_status_reg

        // ------------------------------------------------------------
        // Restore cpu_port and Y; return from interrupt
        // ------------------------------------------------------------
        ldy     cpu_port_shadow
        sty     cpu_port
        ldy     temp_y
        rti

/*
================================================================================
irq_handler10 - Raster IRQ at $81: update sprite shapes (slots 0–3), arm H11
================================================================================

Summary
  Handle raster line $81. Write four consecutive shape indices starting at
  sprite_shape_5 to the sprite shape table for hardware slots selected by
  vic_sprite_idx_0..3. Program next raster to $8B and install irq_handler11.
  Acknowledge the current raster IRQ.

  Same as irq_handler4 in other details.
================================================================================
*/
* = $13F4
irq_handler10:
        // ------------------------------------------------------------
        // Save A, X, Y
        // ------------------------------------------------------------
        sta     temp_a
        stx     temp_x
        sty     temp_y

        // ------------------------------------------------------------
        // Write shapes for hardware slots 0–3 starting at sprite_shape_5
        // ------------------------------------------------------------
        ldx     sprite_shape_5

        ldy     vic_sprite_idx_0
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_1
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_2
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_3
        txa
        sta     (sprite_shape_data_ptr),y

        // ------------------------------------------------------------
        // Map I/O for VIC/CIA access
        // ------------------------------------------------------------
        ldy     cpu_port
        sty     cpu_port_shadow
        ldy     #MAP_IO_IN
        sty     cpu_port

        // ------------------------------------------------------------
        // Program next raster and next handler (irq_handler11)
        // ------------------------------------------------------------
        ldy     #NEXT_RASTER_LINE_H11
        sty     vic_raster_line_reg

        ldy  #<irq_handler11
        sty     irq_handler
        ldy  #>irq_handler11
        sty     irq_handler + 1

        // ------------------------------------------------------------
        // Acknowledge raster IRQ and clear CIA1 edge latch
        // ------------------------------------------------------------
        ldy     #VIC_IRQ_RASTER_ACK
        sty     vic_irq_flag_reg
        ldy     cia1_irq_status_reg

        // ------------------------------------------------------------
        // Restore cpu_port and registers; return from interrupt
        // ------------------------------------------------------------
        ldy     cpu_port_shadow
        sty     cpu_port

        lda     temp_a
        ldx     temp_x
        ldy     temp_y
        rti
/*
================================================================================
irq_handler11 - Raster IRQ at $8B: step sprites to row 6, arm H12
================================================================================

Summary
  Move sprites 0–6 to Y=$9B (+21 from $86). Program next raster to $95 and
  install irq_handler12. Acknowledge the current raster IRQ.

  Same as irq_handler3 in other details.
================================================================================
*/
* = $1448
irq_handler11:
        // ------------------------------------------------------------
        // Save Y and map I/O ($01 := MAP_IO_IN)
        // ------------------------------------------------------------
        sty     temp_y
        ldy     cpu_port
        sty     cpu_port_shadow
        ldy     #MAP_IO_IN
        sty     cpu_port

        // ------------------------------------------------------------
        // Sprites 0–6: set Y to $9B (row 6)
        // ------------------------------------------------------------
        ldy     #SPRITES_Y_ROW6
        sty     vic_sprite0_y_reg
        sty     vic_sprite1_y_reg
        sty     vic_sprite2_y_reg
        sty     vic_sprite3_y_reg
        sty     vic_sprite4_y_reg
        sty     vic_sprite5_y_reg
        sty     vic_sprite6_y_reg

        // ------------------------------------------------------------
        // Program next raster and next handler (irq_handler12)
        // ------------------------------------------------------------
        ldy     #NEXT_RASTER_LINE_H12
        sty     vic_raster_line_reg
        ldy  #<irq_handler12
        sty     irq_handler
        ldy  #>irq_handler12
        sty     irq_handler + 1

        // ------------------------------------------------------------
        // Acknowledge raster IRQ and clear CIA1 edge latch
        // ------------------------------------------------------------
        ldy     #VIC_IRQ_RASTER_ACK
        sty     vic_irq_flag_reg
        ldy     cia1_irq_status_reg

        // ------------------------------------------------------------
        // Restore cpu_port and Y; return from interrupt
        // ------------------------------------------------------------
        ldy     cpu_port_shadow
        sty     cpu_port
        ldy     temp_y
        rti
/*
================================================================================
irq_handler12 - Raster IRQ at $95: update sprite shapes (slots 0–3), arm H13
================================================================================

Summary
  Handle raster line $95. Write four consecutive shape indices starting at
  sprite_shape_6 into the sprite shape table for hardware slots selected by
  vic_sprite_idx_0..3. Program next raster to $9F and install irq_handler13.
  Acknowledge the current raster IRQ.

  Same as irq_handler4 in other details.
================================================================================
*/
* = $1489
irq_handler12:
        // ------------------------------------------------------------
        // Save A, X, Y
        // ------------------------------------------------------------
        sta     temp_a
        stx     temp_x
        sty     temp_y

        // ------------------------------------------------------------
        // Write shapes for hardware slots 0–3 starting at sprite_shape_6
        // ------------------------------------------------------------
        ldx     sprite_shape_6

        ldy     vic_sprite_idx_0
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_1
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_2
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_3
        txa
        sta     (sprite_shape_data_ptr),y

        // ------------------------------------------------------------
        // Map I/O for VIC/CIA access
        // ------------------------------------------------------------
        ldy     cpu_port
        sty     cpu_port_shadow
        ldy     #MAP_IO_IN
        sty     cpu_port

        // ------------------------------------------------------------
        // Program next raster and next handler (irq_handler13)
        // ------------------------------------------------------------
        ldy     #NEXT_RASTER_LINE_H13
        sty     vic_raster_line_reg
        ldy  #<irq_handler13
        sty     irq_handler
        ldy  #>irq_handler13
        sty     irq_handler + 1

        // ------------------------------------------------------------
        // Acknowledge raster IRQ and clear CIA1 edge latch
        // ------------------------------------------------------------
        ldy     #VIC_IRQ_RASTER_ACK
        sty     vic_irq_flag_reg
        ldy     cia1_irq_status_reg

        // ------------------------------------------------------------
        // Restore cpu_port and registers; return from interrupt
        // ------------------------------------------------------------
        ldy     cpu_port_shadow
        sty     cpu_port

        lda     temp_a
        ldx     temp_x
        ldy     temp_y
        rti
/*
================================================================================
irq_handler13 - Raster IRQ at $9F: step sprites to row 7, arm H14
================================================================================

Summary
  Move sprites 0–6 to Y=$B0 (+21 from $9B). Program next raster to $A9 and
  install irq_handler14. Acknowledge the current VIC raster interrupt.

  Same as irq_handler3 in other details.
================================================================================
*/
* = $14DD
irq_handler13:
        // ------------------------------------------------------------
        // Save Y and map I/O ($01 := MAP_IO_IN)
        // ------------------------------------------------------------
        sty     temp_y
        ldy     cpu_port
        sty     cpu_port_shadow
        ldy     #MAP_IO_IN
        sty     cpu_port

        // ------------------------------------------------------------
        // Sprites 0–6: set Y to $B0 (row 7)
        // ------------------------------------------------------------
        ldy     #SPRITES_Y_ROW7
        sty     vic_sprite0_y_reg
        sty     vic_sprite1_y_reg
        sty     vic_sprite2_y_reg
        sty     vic_sprite3_y_reg
        sty     vic_sprite4_y_reg
        sty     vic_sprite5_y_reg
        sty     vic_sprite6_y_reg

        // ------------------------------------------------------------
        // Program next raster and next handler (irq_handler14)
        // ------------------------------------------------------------
        ldy     #NEXT_RASTER_LINE_H14
        sty     vic_raster_line_reg
        ldy  #<irq_handler14
        sty     irq_handler
        ldy  #>irq_handler14
        sty     irq_handler + 1

        // ------------------------------------------------------------
        // Acknowledge raster IRQ and clear CIA1 edge latch
        // ------------------------------------------------------------
        ldy     #VIC_IRQ_RASTER_ACK
        sty     vic_irq_flag_reg
        ldy     cia1_irq_status_reg

        // ------------------------------------------------------------
        // Restore cpu_port and Y; return from interrupt
        // ------------------------------------------------------------
        ldy     cpu_port_shadow
        sty     cpu_port
        ldy     temp_y
        rti
/*
================================================================================
irq_handler14 - Raster IRQ at $A9: update sprite shapes (slots 0–3), arm H15
================================================================================

Summary
  Handle raster line $A9. Write four consecutive shape indices starting at
  sprite_shape_7 into the sprite shape table for hardware slots selected by
  vic_sprite_idx_0..3. Program next raster to $B1 and install irq_handler15.
  Acknowledge the current raster IRQ.

  Same as irq_handler4 in other details.
================================================================================
*/
* = $151E
irq_handler14:
        // ------------------------------------------------------------
        // Save A, X, Y
        // ------------------------------------------------------------
        sta     temp_a
        stx     temp_x
        sty     temp_y

        // ------------------------------------------------------------
        // Write shapes for hardware slots 0–3 starting at sprite_shape_7
        // ------------------------------------------------------------
        ldx     sprite_shape_7

        ldy     vic_sprite_idx_0
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_1
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_2
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_3
        txa
        sta     (sprite_shape_data_ptr),y

        // ------------------------------------------------------------
        // Map I/O for VIC/CIA access
        // ------------------------------------------------------------
        ldy     cpu_port
        sty     cpu_port_shadow
        ldy     #MAP_IO_IN
        sty     cpu_port

        // ------------------------------------------------------------
        // Program next raster and next handler (irq_handler15)
        // ------------------------------------------------------------
        ldy     #NEXT_RASTER_LINE_H15
        sty     vic_raster_line_reg
        ldy  #<irq_handler15
        sty     irq_handler
        ldy  #>irq_handler15
        sty     irq_handler + 1

        // ------------------------------------------------------------
        // Acknowledge raster IRQ and clear CIA1 edge latch
        // ------------------------------------------------------------
        ldy     #VIC_IRQ_RASTER_ACK
        sty     vic_irq_flag_reg
        ldy     cia1_irq_status_reg

        // ------------------------------------------------------------
        // Restore cpu_port and registers; return from interrupt
        // ------------------------------------------------------------
        ldy     cpu_port_shadow
        sty     cpu_port

        lda     temp_a
        ldx     temp_x
        ldy     temp_y
        rti
/*
================================================================================
irq_handler15 - Raster IRQ at $B1: step sprites to row 8, arm H16
================================================================================

Summary
  Move sprites 0–6 to Y=$C5 (+21 from $B0). Program next raster to $BD and
  install irq_handler16. Acknowledge the current VIC raster interrupt.

  Same as irq_handler3 in other details.
================================================================================
*/
* = $1572
irq_handler15:
        // ------------------------------------------------------------
        // Save Y and map I/O ($01 := MAP_IO_IN)
        // ------------------------------------------------------------
        sty     temp_y
        ldy     cpu_port
        sty     cpu_port_shadow
        ldy     #MAP_IO_IN
        sty     cpu_port

        // ------------------------------------------------------------
        // Sprites 0–6: set Y to $C5 (row 8)
        // ------------------------------------------------------------
        ldy     #SPRITES_Y_ROW8
        sty     vic_sprite0_y_reg
        sty     vic_sprite1_y_reg
        sty     vic_sprite2_y_reg
        sty     vic_sprite3_y_reg
        sty     vic_sprite4_y_reg
        sty     vic_sprite5_y_reg
        sty     vic_sprite6_y_reg

        // ------------------------------------------------------------
        // Program next raster and next handler (irq_handler16)
        // ------------------------------------------------------------
        ldy     #NEXT_RASTER_LINE_H16
        sty     vic_raster_line_reg
        ldy  #<irq_handler16
        sty     irq_handler
        ldy  #>irq_handler16
        sty     irq_handler + 1

        // ------------------------------------------------------------
        // Acknowledge raster IRQ and clear CIA1 edge latch
        // ------------------------------------------------------------
        ldy     #VIC_IRQ_RASTER_ACK
        sty     vic_irq_flag_reg
        ldy     cia1_irq_status_reg

        // ------------------------------------------------------------
        // Restore cpu_port and Y; return from interrupt
        // ------------------------------------------------------------
        ldy     cpu_port_shadow
        sty     cpu_port
        ldy     temp_y
        rti
/*
================================================================================
irq_handler16 - Raster IRQ at $BD: update sprite shapes (slots 0–3), arm H17
================================================================================

Summary
  On raster line $BD, write four consecutive shape indices starting at
  sprite_shape_8 into the sprite shape table for hardware slots selected by
  vic_sprite_idx_0..3. Program next raster to $C3 and install irq_handler17.
  Acknowledge the current raster IRQ.

  Same as irq_handler4 in other details.
================================================================================
*/
* = $15B3
irq_handler16:
        // ------------------------------------------------------------
        // Save A, X, Y
        // ------------------------------------------------------------
        sta     temp_a
        stx     temp_x
        sty     temp_y

        // ------------------------------------------------------------
        // Write shapes for hardware slots 0–3 starting at sprite_shape_8
        // ------------------------------------------------------------
        ldx     sprite_shape_8

        ldy     vic_sprite_idx_0
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_1
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_2
        txa
        sta     (sprite_shape_data_ptr),y
        inx

        ldy     vic_sprite_idx_3
        txa
        sta     (sprite_shape_data_ptr),y

        // ------------------------------------------------------------
        // Map I/O for VIC/CIA access
        // ------------------------------------------------------------
        ldy     cpu_port
        sty     cpu_port_shadow
        ldy     #MAP_IO_IN
        sty     cpu_port

        // ------------------------------------------------------------
        // Program next raster and next handler (irq_handler17)
        // ------------------------------------------------------------
        ldy     #NEXT_RASTER_LINE_H17
        sty     vic_raster_line_reg
        ldy  #<irq_handler17
        sty     irq_handler
        ldy  #>irq_handler17
        sty     irq_handler + 1

        // ------------------------------------------------------------
        // Acknowledge raster IRQ and clear CIA1 edge latch
        // ------------------------------------------------------------
        ldy     #VIC_IRQ_RASTER_ACK
        sty     vic_irq_flag_reg
        ldy     cia1_irq_status_reg

        // ------------------------------------------------------------
        // Restore cpu_port and registers; return from interrupt
        // ------------------------------------------------------------
        ldy     cpu_port_shadow
        sty     cpu_port

        lda     temp_a
        ldx     temp_x
        ldy     temp_y
        rti
/*
================================================================================
irq_handler17 - Raster IRQ at $C3: switch to text UI, hide sprites, arm H1
================================================================================

Summary
  Leave the multicolor scene band. Switch VIC into normal text mode using the
  F800 charset and CC00 video matrix, hide sprites 0–6 by moving them offscreen,
  acknowledge the IRQ, program the next raster line to $FB, and restore the
  vector to irq_handler1.

Global Outputs
  irq_handler					 vector := irq_handler1
  vic_screen_control_reg_1        CTRL1_INT_REGION_PRESET
  vic_memory_layout_reg           TEXT_CHARSET_LAYOUT
  vic_screen_control_reg_2        CTRL2_INT_REGION_PRESET
  vic_sprite0..6_y_reg			  sprites 0–6 Y := $FD
  vic_raster_line_reg             NEXT_RASTER_LINE_H1

Description
  - Save A/Y, snapshot cpu_port and map I/O ($01 := MAP_IO_IN).
  - Program:
      • vic_screen_control_reg_1 = 25 rows, VSCROLL=5 (visible)
      • vic_memory_layout_reg = video matrix $CC00, charset $F800
      • vic_screen_control_reg_2 = multicolor OFF, 40 cols, HSCROLL=1
  - Move sprites 0–6 offscreen (Y=$FD).
  - Ack raster IRQ; set next compare line to $FB; clear CIA1 edge.
  - Point IRQ vector back to irq_handler1.
  - Restore cpu_port and A/Y; rti.
================================================================================
*/
* = $1607
irq_handler17:
        // ------------------------------------------------------------
        // Save Y and A; snapshot cpu_port and map I/O ($01 := $25)
        // ------------------------------------------------------------
        sty     temp_y
        sta     temp_a
        ldy     cpu_port
        sty     cpu_port_shadow
        ldy     #MAP_IO_IN
        sty     cpu_port

        // ------------------------------------------------------------
        // Switch from multicolor text scene band to single-color text UI
        // ------------------------------------------------------------
        lda     #CTRL1_INT_REGION_PRESET        // set 25-row text mode
        sta     vic_screen_control_reg_1

        // ------------------------------------------------------------
        // Load memory and control presets for text mode
        // Four NOPs provide stable timing before committing to registers
        // ------------------------------------------------------------
        lda     #TEXT_CHARSET_LAYOUT            // select charset and screen memory layout
        ldy     #CTRL2_INT_REGION_PRESET        // preset scroll and color settings
        nop                                     // timing alignment for VIC stability
        nop
        nop
        nop
        sta     vic_memory_layout_reg           // commit memory layout (screen/charset base)
        sty     vic_screen_control_reg_2        // commit control register 2 (scroll/mode bits)

        // ------------------------------------------------------------
        // Hide sprites 0–6 by moving them offscreen (Y=$FD)
        // ------------------------------------------------------------
        ldy     #SPRITES_Y_OFFSCREEN
        sty     vic_sprite0_y_reg
        sty     vic_sprite1_y_reg
        sty     vic_sprite2_y_reg
        sty     vic_sprite3_y_reg
        sty     vic_sprite4_y_reg
        sty     vic_sprite5_y_reg
        sty     vic_sprite6_y_reg

        // ------------------------------------------------------------
        // Ack current IRQ; arm next raster; clear CIA edge
        // ------------------------------------------------------------
        ldy     #VIC_IRQ_RASTER_ACK
        sty     vic_irq_flag_reg

        ldy     #NEXT_RASTER_LINE_H1
        sty     vic_raster_line_reg
        ldy     cia1_irq_status_reg

        // ------------------------------------------------------------
        // Return to start of chain: install irq_handler1
        // ------------------------------------------------------------
        ldy  #<irq_handler1
        sty     irq_handler
        ldy  #>irq_handler1
        sty     irq_handler + 1

        // ------------------------------------------------------------
        // Restore cpu_port and registers; return from interrupt
        // ------------------------------------------------------------
        ldy     cpu_port_shadow
        sty     cpu_port

        ldy     temp_y
        lda     temp_a
        rti

/*
================================================================================
  compose_sprite_x_msb_and_chain_irq
================================================================================

Summary
	Build the combined VIC-II sprite X MSB byte from per-sprite bit-8 flags and
	their current bit positions, include sprite 7 (cursor), write the result to
	hardware, then acknowledge the raster IRQ, program the next raster line, and
	chain to the next IRQ handler.

Global Inputs
	sprite_0..6_x_hi			   per-sprite X MSB flags (0/1)
	cursor_sprite_x_hi             cursor sprite X MSB flag (0/1)
	cursor_sprite_x_lo             cursor sprite low X byte
	vic_sprite_idx_0..6			   per-sprite MSB bit positions (0..7)

Global Outputs
	sprites_x_hi_combined          aggregated MSB mask (scratch/telemetry)
	vic_sprite_x_msb_reg           written with combined MSB mask
	vic_sprite7_x_lo_reg           updated with cursor low X
	vic_raster_line_reg            next raster line set
	irq_handler (lo/hi)            IRQ vector updated to irq_handler2

Description
	- Clear the combined MSB accumulator.
	- For sprites 0..6:
		• Load that sprite’s X-MSB flag (0/1).
		• Shift it left by its current bit index (vic_sprite_idx_n) with a guard
		for index 0, then OR it into the accumulator.
	- For sprite 7 (cursor):
		• Align its MSB to bit7 and OR with the accumulator.
		• Write the final mask to vic_sprite_x_msb_reg and update the cursor low X.
	- IRQ tail:
		• Acknowledge VIC raster IRQ, set the next raster line, dummy-read CIA1
		IRQ status to clear it, then install irq_handler2 in the IRQ vector.
================================================================================
*/
* = $0FEF
compose_sprite_x_msb_and_chain_irq:
        // ----------------------------------------------------
        // Reset combined MSB accumulator
        // ----------------------------------------------------
        lda     #$00                    // A := 0 (clear working accumulator)
        sta     sprites_x_hi_combined   // combined := 0 (no bits set yet)

        // ----------------------------------------------------
        // Merge bit8 (MSB) of sprite 0 into combined byte
        // ----------------------------------------------------
        lda     sprite_0_x_hi           // A := sprite0.x_msb (0 or 1)
        ldx     vic_sprite_idx_0        // X := current bit position for sprite0 (0..7)
        beq     skip_shift_0            // if X==0 → bit goes in position 0, skip shifts
shift_loop_0:
        asl                              // shift bit left by 1 toward its target position
        dex                              // X := X-1 (count remaining shifts)
        bne     shift_loop_0             // loop until X becomes 0
skip_shift_0:
        ora     sprites_x_hi_combined    // OR into combined mask at the computed position
        sta     sprites_x_hi_combined    // write back updated combined mask

        // ----------------------------------------------------
        // Merge bit8 (MSB) of sprite 1
        // ----------------------------------------------------
        lda     sprite_1_x_hi
        ldx     vic_sprite_idx_1
        beq     skip_shift_1
shift_loop_1:
        asl
        dex
        bne     shift_loop_1
skip_shift_1:
        ora     sprites_x_hi_combined
        sta     sprites_x_hi_combined

        // ----------------------------------------------------
        // Merge bit8 (MSB) of sprite 2
        // ----------------------------------------------------
        lda     sprite_2_x_hi
        ldx     vic_sprite_idx_2
        beq     skip_shift_2
shift_loop_2:
        asl
        dex
        bne     shift_loop_2
skip_shift_2:
        ora     sprites_x_hi_combined
        sta     sprites_x_hi_combined

        // ----------------------------------------------------
        // Merge bit8 (MSB) of sprite 3
        // ----------------------------------------------------
        lda     sprite_3_x_hi
        ldx     vic_sprite_idx_3
        beq     skip_shift_3
shift_loop_3:
        asl
        dex
        bne     shift_loop_3
skip_shift_3:
        ora     sprites_x_hi_combined
        sta     sprites_x_hi_combined

        // ----------------------------------------------------
        // Merge bit8 (MSB) of sprite 4
        // ----------------------------------------------------
        lda     sprite_4_x_hi
        ldx     vic_sprite_idx_4
        beq     skip_shift_4
shift_loop_4:
        asl
        dex
        bne     shift_loop_4
skip_shift_4:
        ora     sprites_x_hi_combined
        sta     sprites_x_hi_combined

        // ----------------------------------------------------
        // Merge bit8 (MSB) of sprite 5
        // ----------------------------------------------------
        lda     sprite_5_x_hi
        ldx     vic_sprite_idx_5
        beq     skip_shift_5
shift_loop_5:
        asl
        dex
        bne     shift_loop_5
skip_shift_5:
        ora     sprites_x_hi_combined
        sta     sprites_x_hi_combined

        // ----------------------------------------------------
        // Merge bit8 (MSB) of sprite 6
        // ----------------------------------------------------
        lda     sprite_6_x_hi
        ldx     vic_sprite_idx_6
        beq     skip_shift_6
shift_loop_6:
        asl
        dex
        bne     shift_loop_6
skip_shift_6:
        ora     sprites_x_hi_combined
        sta     sprites_x_hi_combined

        // ----------------------------------------------------
        // Update cursor sprite (sprite 7)
        // ----------------------------------------------------
        ldx     #$07
        lda     cursor_sprite_x_hi
        beq     finalize_sprite_mask
shift_loop_cursor:
        asl
        dex
        bne     shift_loop_cursor

finalize_sprite_mask:
        ora     sprites_x_hi_combined
        sta     vic_sprite_x_msb_reg

        // Update sprite 7 low X register
        ldy     cursor_sprite_x_lo
        sty     vic_sprite7_x_lo_reg

        // ----------------------------------------------------
        // IRQ management: acknowledge VIC-II and CIA interrupts
        // ----------------------------------------------------
        ldy     #VIC_IRQ_RASTER_ACK
        sty     vic_irq_flag_reg
        ldy     #NEXT_RASTER_LINE_H2
        sty     vic_raster_line_reg
        ldy     cia1_irq_status_reg    // dummy read acknowledges CIA IRQ

        // ----------------------------------------------------
        // Chain to next raster IRQ handler
        // ----------------------------------------------------
        ldy     #<irq_handler2
        sty     irq_handler
        ldy     #>irq_handler2
        sty     irq_handler + 1

        rts

//===========================================
//NMI handler - return immediately
//===========================================
* = $18EA
nmi_handler:
		rti

/*
================================================================================
  init_raster_irq_env
================================================================================
Summary
	Initialize the raster IRQ environment once, then disable further setup runs.

Global Inputs
	raster_irq_init_pending_flag		nonzero means initialization must run
	sid_volfilt_shadow 	last known SID master volume/filter nibble
	$3465						nonzero flag to restore SID master volume

Global Outputs
	$FFFE/$FFFF (irq_handler): 		set to <irq_handler1, >irq_handler1.
	VIC-II: 	sprite enable, screen control #1, raster line, IRQ flag/mask,
				border color are configured for first IRQ stage.
	CIA1/CIA2: 	pending IRQ/status latched-and-cleared by dummy reads.
	SID: 		master volume optionally restored from sid_volfilt_shadow.
	cpu_port: 	mapped to I/O for setup, then restored to mapped-out state.
	raster_irq_init_pending_flag: cleared to 0 after successful initialization.

Description
	* Exit early if initialization is not pending.
	* Map in I/O, enable all sprites, select 25-row mode with Y-scroll=3.
	* Program raster line ($FA) and point IRQ vector to irq_handler1.
	* Acknowledge any stale raster IRQ, then enable raster IRQ as the only
	  VIC-II source.
	* Quench CIA1 IRQ latch and stabilize CIA2 by reading their registers.
	* Force border to black baseline.
	* Optionally restore SID master volume based on $3465 flag.
	* Map out I/O, enable maskable IRQs, and clear the pending-init flag.

Notes
	* Assumes IRQ vector fetch reads RAM at $FFFE/$FFFF, not KERNAL ROM.
	* Enabling all sprites is safe if Y positions keep unseen sprites offscreen.
================================================================================
*/
* = $341C
init_raster_irq_env:
        // ------------------------------------------------------------
        // Initialize raster interrupt environment if required
        //
        // If raster_irq_init_pending_flag = 0, this routine exits immediately.
        // Otherwise it configures VIC-II raster IRQs, sprite enables,
        // screen mode, and SID volume, then marks setup as complete.
        // ------------------------------------------------------------
        lda     raster_irq_init_pending_flag
        beq     early_exit_no_setup

        // ------------------------------------------------------------
        // Map in I/O to access VIC/CIA/SID registers
        // ------------------------------------------------------------
        ldy     #MAP_IO_IN
        sty     cpu_port

        // ------------------------------------------------------------
        // Enable all sprites (0–7)
        // ------------------------------------------------------------
        lda     #SPRITES_ENABLE_ALL 
        sta     vic_sprite_enable_reg

        // ------------------------------------------------------------
        // Set IRQ handler vector to irq_handler1
        // Writes <irq_handler1 to $FFFE and >irq_handler1 to $FFFF.
        // ------------------------------------------------------------
        lda     #<irq_handler1
        sta     irq_handler
        lda     #>irq_handler1
        sta     irq_handler+1

        // ------------------------------------------------------------
        // Configure screen: 25 rows, vertical scroll = 3
        // ------------------------------------------------------------
        lda     #VIC_CTRL_DEFAULT_1 
        sta     vic_screen_control_reg_1

        // ------------------------------------------------------------
        // Set next raster line to trigger interrupt (#$FA = line 250)
        // ------------------------------------------------------------
        lda     #RASTER_IRQ_LINE_INIT
        sta     vic_raster_line_reg

        // ------------------------------------------------------------
        // Acknowledge any pending raster interrupt and enable raster IRQ
        // ------------------------------------------------------------
        lda     #VIC_IRQ_RASTER_ACK
        sta     vic_irq_flag_reg
        sta     vic_irq_mask_reg

        // ------------------------------------------------------------
        // Clear pending CIA1 IRQ and stabilize CIA2 state
        // ------------------------------------------------------------
        lda     cia1_irq_status_reg
        lda     cia2_pra

        // ------------------------------------------------------------
        // Set border color to black
        // ------------------------------------------------------------
        lda     #COLOR_BLACK
        sta     vic_border_color_reg

        // ------------------------------------------------------------
        // Optionally restore SID master volume if flag set
        // ------------------------------------------------------------
        lda     $3465
        beq     finish_setup_unmap_io
        lda     sid_volfilt_shadow
        sta     sid_master_volume

finish_setup_unmap_io:
        // ------------------------------------------------------------
        // Map out I/O, re-enable interrupts, and clear setup-needed flag
        // ------------------------------------------------------------
        ldy     #MAP_IO_OUT
        sty     cpu_port
        cli
        lda     #FALSE
        sta     raster_irq_init_pending_flag

early_exit_no_setup:
        rts

