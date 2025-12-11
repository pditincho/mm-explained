/*
================================================================================
 init_engine.asm
================================================================================
Summary
    Low-level bring-up and relocation for the Maniac Mansion engine. This module
    clears and normalizes key memory regions, installs interrupt vectors and
    drive code, configures VIC/CIA/SID state, relocates runtime code/data into
    their final addresses, and seeds core engine globals such as the sentence
    system, camera, and actor/costume tables.

Responsibilities
    • Hardware and IRQ baseline
      - Program IRQ/NMI vectors and install custom drive/IRQ handlers.
      - Configure the 6510 I/O port and DDR for predictable RAM/I/O mapping.
      - Establish a known VIC-II layout (bank selection, screen/charset base,
        raster IRQ line, sprite enables, colors, and border state).
      - Coordinate with the raster IRQ via a lock/handshake and apply a safe
        VIC/SID baseline, optionally muting audio while voices are active.

    • Memory clearing and layout
      - Clear zero-page variables from $0015..$00FF without disturbing core
        KERNAL/BASIC cells.
      - Zero the main engine/game variable region and a secondary work buffer
        used by later subsystems.
      - Clear both scene frame buffers, dot-data, sprite registers, and sprite
        memory, then paint an initial color bar in color RAM.
      - Provide reusable byte-fill and block-copy helpers used by other modules
        to clear or relocate memory ranges.

    • Code and data relocation
      - Copy character set and UI arrow tiles into their runtime character ROM
        window.
      - Relocate engine graphics and script-handler code from loader addresses
        into their final execution bank.
      - Relocate engine constant tables, the room-graphics decompressor, and a
        small initialization block used by runtime code.
      - Leave all relocated blocks in the layout expected by the script engine,
        room renderer, and resource loader.

    • Engine and rendering state initialization
      - Initialize actor→sprite mappings, actor sprite base page, and the room
        mask pointer used by masking/clipping logic.
      - Seed the sentence stack, default verb, message timing, lights mode,
        active disk side, and camera position.
      - Normalize per-costume default clips and per-actor facing/costume
        mappings to a “no actor / no costume” baseline.
      - Seed the pseudo-random generator with stable nonzero values for
        deterministic startup behavior.

    • Raster and sound interaction
      - Provide a reusable raster/SID init entry that can be called once the
        IRQ environment is live, guarded by a flag and by a “music active”
        check to avoid disrupting playback.
      - Snapshot logical voice activity and mute SID master volume while
        applying raster/video initialization.

Key flows
    1) Power-on / loader handoff:
         • Loader jumps to game_code_entry (in entry_point.asm).
         • game_code_entry calls init_memory_sound_and_video to:
             - Install vectors and drive code.
             - Clear graphics, zero page, and engine/game regions.
             - Configure VIC/CIA layout, sprite state, and color RAM.
             - Relocate core runtime blocks via relocate_memory.
         • init_game_engine seeds high-level engine globals.
         • A separate call to init_raster_and_sound_state syncs with the first
           raster IRQ and establishes a safe video/audio baseline.

    2) Runtime reuse:
         • mem_fill_x_3 and copy_memory are used by other modules for clears
           and relocations.
         • init_actor_render_state is the canonical “reset” for actor sprite
           mappings when rooms or scenes are reinitialized.
         • init_game_engine is the canonical “reset” for sentence/verb/camera
           state when starting a new game.

Notes
    • All hard-coded source/destination addresses and sizes are coupled to the disk
      image layout and loader’s copy strategy.
    • init_raster_and_sound_state is deliberately idempotent and guards against
      both active music and double-initialization; callers must rely on the
      raster_irq_init_pending_flag to avoid reprogramming VIC/SID mid-game.
    • Zero-page and engine-variable clears assume that no active IRQ handlers
      are using the cleared ranges; callers are expected to invoke this module
      only during early startup or controlled reinitialization windows.
================================================================================
*/
#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "ui_messages.asm"
#import "irq_handlers.asm"
#import "cursor.asm"
#import "drive_setup.asm"
#import "sound_engine.asm"

.label copy_counter_lo = $15    // ZP: copy byte count, low
.label copy_counter_hi = $16    // ZP: copy byte count, high

.label copy_source     = $19    // ZP: 16-bit source pointer (alias of copy_src_lo/hi)
.label copy_src_lo     = $19    // ZP: source pointer, low
.label copy_src_hi     = $1A    // ZP: source pointer, high

.label copy_dest       = $1B    // ZP: 16-bit destination pointer (alias of copy_dest_lo/hi)
.label copy_dest_lo    = $1B    // ZP: destination pointer, low
.label copy_dest_hi    = $1C    // ZP: destination pointer, high

.label fill_ptr_lo     = $19    // ZP: fill destination pointer, low (aliases copy_src_lo)
.label fill_ptr_hi     = $1A    // ZP: fill destination pointer, high (aliases copy_src_hi)
.label fill_count_lo   = $1B    // ZP: fill byte count, low (aliases copy_dest_lo)
.label fill_count_hi   = $1C    // ZP: fill byte count, high (aliases copy_dest_hi)
.label fill_pointer    = $19    // ZP: 16-bit fill destination pointer (alias of fill_ptr_lo/hi)

.const ZP_CLEAR_START              = $15      // ZP start address for clear loop
.const ZP_CLEAR_BASE               = $0000    // ZP base used by indexed STA
.const ZP_CLEAR_VALUE              = $00      // Fill byte for ZP clear

.const CLEAR_GAME_START            = $FC00    // Main RAM clear start
.const CLEAR_GAME_SIZE             = $03F3    // Bytes to clear (4035)
.const CLEAR_VALUE_ZERO            = $00      // Fill byte = zero

.const ZFILL_CAD0_START            = $CAD0    // Start address
.const ZFILL_CAD0_SIZE             = $00C0    // Size in bytes
.const ZFILL_VALUE_ZERO            = $00      // Fill byte = zero

.const FRAMEBUF1_START             = $C800    // Screen RAM buffer 1
.const FRAMEBUF2_START             = $CC00    // Screen RAM buffer 2
.const FRAMEBUF_SIZE               = $03E8    // 1000 bytes (40×25 chars)
.const DOTDATA_START               = $D800    // Color RAM base
.const DOTDATA_SIZE                = $0800    // 2048 nibbles (mirrored as bytes)
.const COLOR_BAR_LEN               = $0028    // 40 bytes (one row)
.const COLOR_BAR_FILL              = $0E      // Fill color value

.const STD_MSG_DEFAULT_TICKS       = $001E    // Message bar countdown (16-bit)
.const TEXT_DELAY_DEFAULT          = $06      // Per-character delay factor
.const DISK_ID_SIDE_2              = $32      // Game disk ID for side 2
.const CAMERA_INIT_POS             = $C8      // Initial camera position
.const RNG_SEED_1                  = $4D      // RNG seed byte 1
.const RNG_SEED_2                  = $97      // RNG seed byte 2

.const ROOM_MASK_PTR               = $6AE1    // Room scene mask layer pointer

.const SPR_MC_ENABLE_0_TO_6        = $7F      // Enable multicolor for sprites 0–6
.const SPRITE_MEM_SIZE             = $1000    // Bytes to clear ($E000–$EFFF)

.const COLOR_LIGHT_RED             = $0A      // Palette color: light red / skin tone

.const BLOCK_CHARS_SRC_START       = $7BFF    // Copy src: charset + arrows
.const BLOCK_CHARS_DST_START       = $F800    // Copy dst: $F800–$FBFF
.const BLOCK_CHARS_SIZE_BYTES      = $0400    // Copy size in bytes

.const BLOCK_GFX_CODE_SRC_START    = $86A4    // Copy src: gfx + script code
.const BLOCK_GFX_CODE_DST_START    = $D000    // Copy dst: $D000–$D7FF (RAM banked)
.const BLOCK_GFX_CODE_SIZE_BYTES   = $0800    // Copy size in bytes

.const BLOCK_CONSTS_SRC_START      = $8EA0    // Copy src: engine constants
.const BLOCK_CONSTS_DST_START      = $F0C0    // Copy dst start
.const BLOCK_CONSTS_SIZE_BYTES     = $073F    // Copy size in bytes

.const BLOCK_DECOMP_SRC_START      = $95E0    // Copy src: decompressor
.const BLOCK_DECOMP_DST_START      = $0100    // Copy dst: stack page
.const BLOCK_DECOMP_SIZE_BYTES     = $009B    // Copy size in bytes

.const BLOCK_INIT_SRC_START        = $85E4    // Copy src: init block
.const BLOCK_INIT_DST_START        = $CAD0    // Copy dst start
.const BLOCK_INIT_SIZE_BYTES       = $00C0    // Copy size in bytes

.const CURSOR_INIT_X               = $0D      // Initial cursor X
.const CURSOR_INIT_Y               = $1C      // Initial cursor Y
.const INTERACTION_REGION_DEFAULT  = $00      // Initial region id
.const HIDE_CURSOR_FLAG_ON         = $01      // Hide cursor flag value

.const SCREEN_OFF_MASK             = $EF      // $D011 mask to blank screen (clear bit4)
.const NMI_STUB_ADDR               = $18EA    // NMI handler address (RTS stub)

.const VIC_MEM_LAYOUT_INIT         = $26      // $D018: screen=$0400, charset=$2000
.const VIC_CTRL2_INIT              = $18      // $D016: MCM text, 40 cols, hscroll=0
.const VIC_IRQ_BITS_RASTER         = $81      // $D019/$D01A: raster bit mask

.const CIA2_VIC_BANK_MASK          = $FC      // $DD00: clear bits 0–1 (bank select)
.const CIA2_VIC_BANK_C000          = $00      // $DD00: bank $C000–$FFFF

.const CIA1_TIMER_STOP_ON_UF       = $08      // $DC0E/$DC0F: stop on underflow


.const INIT_COPY_SRC_START         = $7B7F    // Copy src start
.const INIT_COPY_DST_START         = $F040    // Copy dst start
.const INIT_COPY_SIZE_BYTES        = $0080    // Copy size in bytes

/*
================================================================================
  init_raster_and_sound_state
================================================================================
Summary
	Coordinate with the raster IRQ handler, then blank the screen, disable
	sprites, and conditionally mute SID while establishing a known VIC/SID
	baseline for subsequent raster-driven effects.

Arguments
	None (uses global flags and hardware-mapped registers)

Returns
	None (state changes are applied via globals and hardware registers)

Global Inputs
	music_playback_in_progress 		Non-zero if music playback is currently active
	raster_irq_init_pending_flag 	Non-zero if raster IRQ init is already pending
	irq_sync_lock 					Handshake flag cleared by IRQ handler
	voice_instr_active_mask 		Bitmask of currently active logical voices

Global Outputs
	vic_screen_control_reg_1 		Set to CTRL1_BLANK_PRESET baseline
	vic_sprite_enable_reg 			Cleared to disable all sprites
	vic_border_color_reg 			Set to COLOR_BLACK
	voice_active_mask_snapshot 		Snapshot copy of voice_instr_active_mask
	sid_master_volume 				Set to 0 when any voice is active (mute)
	raster_irq_init_pending_flag 	Set TRUE to request/mark raster IRQ init

Description
	- Exits immediately if music is active to avoid disrupting ongoing audio.
	- Skips initialization if the raster IRQ init flag is already set.
	- Maps I/O into the CPU address space via cpu_port for VIC/SID access.
	- Uses irq_sync_lock to wait until the raster IRQ handler has run once.
	- Disables interrupts and applies a known VIC screen-control preset.
	- Disables all sprites and forces the border color to black.
	- Snapshots the current voice-activity mask and, if any voices are
	active, mutes SID master volume to prevent artifacts during setup.
	- Restores the normal memory configuration and sets the raster init flag.
================================================================================
*/
* = $33E0
init_raster_and_sound_state:
        // Skip if music playback is active
        lda     music_playback_in_progress
        beq     check_raster_init_guard
        rts

check_raster_init_guard:
        // Skip if raster-IRQ init already pending/in progress
        lda     raster_irq_init_pending_flag
        bne     init_raster_and_sound_exit

        // Map I/O in (memory config = $25)
        ldy     #MAP_IO_IN
        sty     cpu_port

        // Handshake with IRQ: set lock and wait until IRQ clears it
        lda     #LOCK_SET
        sta     irq_sync_lock
wait_for_irq_entry_unlock:
        lda     irq_sync_lock
        bne     wait_for_irq_entry_unlock

        // IRQ has just returned → disable interrupts
        sei

        // Blank screen / 25 rows / scroll settings
        lda     #CTRL1_BLANK_PRESET
        sta     vic_screen_control_reg_1

        // Disable all sprites
        lda     #SPRITES_DISABLE_ALL
        sta     vic_sprite_enable_reg

        // Set border color = black
        lda     #COLOR_BLACK
        sta     vic_border_color_reg

        // Snapshot voice-activity mask and mute SID if any voices active
        lda     voice_instr_active_mask
        sta     voice_active_mask_snapshot
        beq     restore_memmap_after_io
        lda     #$00
        sta     sid_master_volume

restore_memmap_after_io:
        // Restore normal memory map (I/O out)
        ldy     #MAP_IO_OUT
        sty     cpu_port

        // Mark raster-IRQ init as pending
        lda     #TRUE
        sta     raster_irq_init_pending_flag

init_raster_and_sound_exit:
        rts

/*
================================================================================
  init_memory_sound_and_video
================================================================================
Summary
	Sets interrupt vectors and the 6510 DDR, blanks the screen, clears key memory
	regions, initializes default VIC-II colors and sprite state, installs a benign
	NMI stub, primes actor rendering and SID voices, configures IRQs and video
	layout, performs a small ROM→RAM copy, then jumps to relocate the main runtime
	blocks.

Description
	* Initialize vectors and custom drive code.
	* Disable IRQs, set 6510 DDR, and blank the screen.
	* Map out I/O and clear graphics, zero page, and game/engine variables.
	* Initialize video mode defaults and sprite registers.
	* Install an RTS NMI stub and initialize actor render state and SID voices.
	* Map in I/O and program IRQ vector, $D018 layout, $D016/$D011, raster line,
	  and IRQ flags/mask. Select VIC bank $C000 via CIA2 PRA and stop CIA1 timers
	  on underflow. Enable all sprites.
	* Map out I/O and copy a small initialization block, then jump to the main
	  relocation routine to finish setup.
================================================================================
*/
* = $82F1
init_memory_sound_and_video:
        // ------------------------------------------------------------
        // Setup interrupt vectors and drive code
        // ------------------------------------------------------------
        jsr     setup_vectors_and_drive_code

        // ------------------------------------------------------------
        // Disable interrupts and configure CPU port
        // ------------------------------------------------------------
        sei
        lda     #CPU_PORT_DDR_INIT
        sta     cpu_port_ddr

        // ------------------------------------------------------------
        // Blank the screen during memory initialization
        // ------------------------------------------------------------
        lda     vic_screen_control_reg_1
        and     #SCREEN_OFF_MASK
        sta     vic_screen_control_reg_1

        // ------------------------------------------------------------
        // Clear memory regions and initialize video defaults
        // ------------------------------------------------------------
        ldy     #MAP_IO_OUT
        sty     cpu_port                       
        jsr     clear_gfx_memory
        jsr     clear_zero_page_vars
        jsr     clear_game_and_engine_vars
        jsr     init_video_mode_and_click_flag
        jsr     clear_gfx_memory
        jsr     setup_border_and_sprite_colors
        jsr     clear_sprite_regs_and_sprite_memory

        // ------------------------------------------------------------
        // Set NMI handler to $18EA (dummy RTS routine)
        // ------------------------------------------------------------
        lda     #$EA
        sta     nmi_handler_lo
        lda     #$18
        sta     nmi_handler_hi

        // ------------------------------------------------------------
        // Initialize actor rendering and sprite mappings
        // ------------------------------------------------------------
        jsr     init_actor_render_state

        // ------------------------------------------------------------
        // Initialize sound voices
        // ------------------------------------------------------------
        ldy     #MAP_IO_IN
        sty     cpu_port                       
        jsr     reset_sound_engine_state
        ldy     #MAP_IO_OUT
        sty     cpu_port                       

        // ------------------------------------------------------------
        // Configure IRQ and VIC-II video layout
        // ------------------------------------------------------------
        sei
        ldy     #MAP_IO_IN
        sty     cpu_port                       // map in I/O to access VIC/CIA

        lda     #<irq_handler1
        sta     irq_handler                   // set IRQ vector low byte ($FFFE)
        lda     #>irq_handler1
        sta     irq_handler + 1                   // set IRQ vector high byte ($FFFF)

        lda     #CTRL1_BLANK_PRESET
        sta     vic_screen_control_reg_1       // blank screen, 25 rows, vscroll=3

        lda     #VIC_MEM_LAYOUT_INIT
        sta     $21                            // shadow/temp: selected $D018 value
        sta     vic_memory_layout_shadow       // engine shadow of $D018
        sta     vic_memory_layout_reg          // screen=$0400, charset=$2000 within VIC bank

        lda     #$01
        sta     frame_buffer                   // select framebuffer index 1

        lda     #CTRL2_ROOM_REGION_PRESET
        sta     vic_screen_control_reg_2       // multicolor text on, 40 cols, hscroll=0

        lda     #RASTER_IRQ_LINE_INIT
        sta     vic_raster_line_reg            // trigger raster IRQ at line 250

        lda     #VIC_IRQ_RASTER_ACK
        sta     vic_irq_flag_reg               // acknowledge pending raster IRQs
        sta     vic_irq_mask_reg               // enable raster IRQ source

        lda     cia1_irq_status_reg            // read to clear CIA1 IRQ flags (side effect)

        lda     cia2_pra
        and     #CIA2_VIC_BANK_MASK
        ora     #CIA2_VIC_BANK_C000
        sta     cia2_pra                       // set VIC bank to $C000–$FFFF

        lda     #CIA1_TIMER_STOP_ON_UF
        sta     cia1_timer_a_ctrl_reg          // stop timer A on underflow (no start)
        lda     #CIA1_TIMER_STOP_ON_UF
        sta     cia1_timer_b_ctrl_reg          // stop timer B on underflow (no start)

        lda     #SPRITES_ENABLE_ALL
        sta     vic_sprite_enable_reg          // enable sprites 0–7 (visibility mask)

        // ------------------------------------------------------------
        // Copy small init block ($7B7F → $F040, #$80 bytes)
        // ------------------------------------------------------------
        ldy     #MAP_IO_OUT
        sty     cpu_port                       // map out I/O
        lda     #<INIT_COPY_SRC_START
        sta     copy_src_lo
        lda     #>INIT_COPY_SRC_START
        sta     copy_src_hi
        lda     #<INIT_COPY_DST_START
        sta     copy_dest_lo
        lda     #>INIT_COPY_DST_START
        sta     copy_dest_hi
        lda     #<INIT_COPY_SIZE_BYTES
        sta     copy_counter_lo
        lda     #>INIT_COPY_SIZE_BYTES
        sta     copy_counter_hi
        jsr     copy_memory

        // ------------------------------------------------------------
        // Relocate main code and finish setup
        // ------------------------------------------------------------
        jmp     relocate_memory

/*
================================================================================
  copy_memory
================================================================================
Summary
	Copy a block of memory forward from source to destination for exactly
	byte_counter bytes. Uses zero-page 16-bit pointers and a 16-bit counter.
	Not safe for overlapping regions where destination starts inside source.

Arguments
	copy_src_lo/copy_src_hi ($19/$1A): 16-bit source pointer (start address)
	copy_dest_lo/copy_dest_hi ($1B/$1C): 16-bit destination pointer
	copy_counter_lo/copy_counter_hi ($15/$16): total byte count (> $0000)

Global Inputs
	copy_src_lo/copy_src_hi
	copy_dest_lo/copy_dest_hi
	copy_counter_lo/copy_counter_hi

Description
	* Load a byte from (copy_source), store to (copy_dest).
	* Increment 16-bit copy_source and copy_dest pointers (carry into high bytes).
	* Decrement 16-bit byte counter (borrow from high when low is $00).
	* Loop until counter becomes $0000 (tested via OR of low|high).
	* Precondition: count must be > $0000. For overlapping copies where dest lies
	within copy_source and dest > copy_source, use a reverse-direction routine.
================================================================================
*/
* = $83A3
copy_memory:
        ldy     #$00                          // reset index within page
        lda     (copy_source),y               // load byte from source
        sta     (copy_dest),y                 // store to destination

        inc     copy_src_lo                   // advance source pointer (lo)
        bne     cm_advance_dest_ptr
        inc     copy_src_hi                   // carry into high byte if wrapped

cm_advance_dest_ptr:
        inc     copy_dest_lo                  // advance destination pointer (lo)
        bne     cm_decrement_count16
        inc     copy_dest_hi                  // carry into high byte if wrapped

cm_decrement_count16:
        lda     copy_counter_lo               // load low count
        bne     cm_decrement_count_lo
        dec     copy_counter_hi               // borrow from high byte if low=0
cm_decrement_count_lo:
        dec     copy_counter_lo               // decrement low count

        lda     copy_counter_lo
        ora     copy_counter_hi               // both zero? finished
        bne     copy_memory		              // no → continue

cm_copy_done:
        rts                                   // done copying

/*
================================================================================
 relocate_memory
================================================================================
Summary
	Copies five code/data blocks from load-time addresses into RAM regions used at
	runtime. Initializes cursor position and interaction region. Hides the cursor,
	enables interrupts, and installs raster IRQ handlers.

Description
	* Issue five copy jobs via copy_memory using (copy_src, copy_dest, count):
		1. Characters/UI arrows to $F800 for #$0400 bytes
		2. Graphics and script handlers to $D000 for #$0800 bytes
		3. Engine constants to $F0C0 for #$073F bytes
		4. Decompression routines to $0100 for #$009B bytes
		5. Init block to $CAD0 for #$00C0 bytes
	* Set cursor_x_pos, cursor_y_pos, and hotspot_entry_ofs to initial values.
	* Call update_cursor_physics_from_hotspot to sync cursor motion parameters.
	* Hide the cursor, enable IRQs with CLI, then call init_raster_irq_env.
================================================================================
*/

* = $83C4		
relocate_memory:
        // ------------------------------------------------------------
        // Copy character set and UI arrows ($7BFF → $F800, #$0400 bytes)
        // ------------------------------------------------------------
        lda     #<BLOCK_CHARS_SRC_START
        sta     copy_src_lo
        lda     #>BLOCK_CHARS_SRC_START
        sta     copy_src_hi
        lda     #<BLOCK_CHARS_DST_START
        sta     copy_dest_lo
        lda     #>BLOCK_CHARS_DST_START
        sta     copy_dest_hi
        lda     #<BLOCK_CHARS_SIZE_BYTES
        sta     copy_counter_lo
        lda     #>BLOCK_CHARS_SIZE_BYTES
        sta     copy_counter_hi
        jsr     copy_memory

        // ------------------------------------------------------------
        // Copy graphics and script handler code ($86A4 → $D000, #$0800 bytes)
        // ------------------------------------------------------------
        lda     #<BLOCK_GFX_CODE_SRC_START
        sta     copy_src_lo
        lda     #>BLOCK_GFX_CODE_SRC_START
        sta     copy_src_hi
        lda     #<BLOCK_GFX_CODE_DST_START
        sta     copy_dest_lo
        lda     #>BLOCK_GFX_CODE_DST_START
        sta     copy_dest_hi
        lda     #<BLOCK_GFX_CODE_SIZE_BYTES
        sta     copy_counter_lo
        lda     #>BLOCK_GFX_CODE_SIZE_BYTES
        sta     copy_counter_hi
        jsr     copy_memory

        // ------------------------------------------------------------
        // Copy game engine constants ($8EA0 → $F0C0, #$073F bytes)
        // ------------------------------------------------------------
        lda     #<BLOCK_CONSTS_SRC_START
        sta     copy_src_lo
        lda     #>BLOCK_CONSTS_SRC_START
        sta     copy_src_hi
        lda     #<BLOCK_CONSTS_DST_START
        sta     copy_dest_lo
        lda     #>BLOCK_CONSTS_DST_START
        sta     copy_dest_hi
        lda     #<BLOCK_CONSTS_SIZE_BYTES
        sta     copy_counter_lo
        lda     #>BLOCK_CONSTS_SIZE_BYTES
        sta     copy_counter_hi
        jsr     copy_memory

        // ------------------------------------------------------------
        // Copy decompression routines ($95E0 → $0100, #$009B bytes)
        // ------------------------------------------------------------
        lda     #<BLOCK_DECOMP_SRC_START
        sta     copy_src_lo
        lda     #>BLOCK_DECOMP_SRC_START
        sta     copy_src_hi
        lda     #<BLOCK_DECOMP_DST_START
        sta     copy_dest_lo
        lda     #>BLOCK_DECOMP_DST_START
        sta     copy_dest_hi
        lda     #<BLOCK_DECOMP_SIZE_BYTES
        sta     copy_counter_lo
        lda     #>BLOCK_DECOMP_SIZE_BYTES
        sta     copy_counter_hi
        jsr     copy_memory

        // ------------------------------------------------------------
        // Copy initialization block ($85E4 → $CAD0, #$00C0 bytes)
        // ------------------------------------------------------------
        lda     #<BLOCK_INIT_SRC_START
        sta     copy_src_lo
        lda     #>BLOCK_INIT_SRC_START
        sta     copy_src_hi
        lda     #<BLOCK_INIT_DST_START
        sta     copy_dest_lo
        lda     #>BLOCK_INIT_DST_START
        sta     copy_dest_hi
        lda     #<BLOCK_INIT_SIZE_BYTES
        sta     copy_counter_lo
        lda     #>BLOCK_INIT_SIZE_BYTES
        sta     copy_counter_hi
        jsr     copy_memory

        // ------------------------------------------------------------
        // Initialize cursor and interaction region
        // ------------------------------------------------------------
        lda     #CURSOR_INIT_X
        sta     cursor_x_pos
        lda     #CURSOR_INIT_Y
        sta     cursor_y_pos
        lda     #INTERACTION_REGION_DEFAULT
        sta     hotspot_entry_ofs
        jsr     update_cursor_physics_from_hotspot

        // ------------------------------------------------------------
        // Hide cursor and enable interrupts
        // ------------------------------------------------------------
        lda     #TRUE
        sta     hide_cursor_flag
        cli

        // ------------------------------------------------------------
        // Initialize raster interrupt handlers
        // ------------------------------------------------------------
        jsr     init_raster_irq_env

        rts
/*
================================================================================
  clear_zero_page_vars
================================================================================
Summary
	Clear zero-page addresses $0015..$00FF by writing $00 to each byte. Leaves
	$0000..$0014 untouched.

Global Outputs
	* Zero page $0015..$00FF set to $00.

Description
	* Initialize Y to ZP_CLEAR_START and A to $00.
	* Loop: STA $0000,Y; INY; branch while Y ≠ $00.
	* Completion occurs when Y wraps from $FF to $00 after writing $00FF.

Notes
	* Clears 235 bytes total ($EB).
	* Must be called with interrupts disabled if ISR uses any bytes in the cleared
	  range.
================================================================================
*/
* = $8467
clear_zero_page_vars:
        ldy     #ZP_CLEAR_START              // Y := starting offset
        lda     #ZP_CLEAR_VALUE              // A := 0

zp_clear_loop:
        sta     ZP_CLEAR_BASE,y              // clear one byte
        iny                                  // next address
        bne     zp_clear_loop                // loop until Y wraps to 0
        rts                                  
/*
================================================================================
  clear_game_and_engine_vars
================================================================================
Summary
	Zero-fill the main engine range at CLEAR_GAME_START for CLEAR_GAME_SIZE bytes,
	then tail-jump to clear_cad0_cb90 to zero the secondary work area. Uses the
	byte-fill helper mem_fill_x_3.
================================================================================
*/
* = $8472
clear_game_and_engine_vars:
        lda     #<CLEAR_GAME_START             
        sta     fill_ptr_lo
        lda     #>CLEAR_GAME_START             
        sta     fill_ptr_hi
        lda     #<CLEAR_GAME_SIZE              
        sta     fill_count_lo
        lda     #>CLEAR_GAME_SIZE              
        sta     fill_count_hi
        ldx     #CLEAR_VALUE_ZERO              
        jsr     mem_fill_x_3                   
        jmp     clear_cad0_cb90                
/*
================================================================================
  mem_fill_x_3
================================================================================
Summary
	Fill a memory region with a constant byte value held in X. Copies exactly
	byte_counter bytes to the destination by advancing a 16-bit pointer each loop.
	Forward-only. Requires nonzero length.

Arguments
	X                fill value (0..255)
	fill_pointer     destination pointer (lo/hi)
	byte_counter     total byte count (> $0000)

Description
	- Keep Y at 0 and write A (from X) to (fill_pointer),Y each pass.
	- Increment destination pointer lo, carry into hi on wrap.
	- Decrement 16-bit byte_counter (low then borrow to high).
	- Loop until (copy_counter_lo | copy_counter_hi) == 0.

Notes
	- If byte_counter == $0000 on entry, behavior is invalid (will not terminate).
	- Safe for overlapping regions because only destination is written.
================================================================================
*/
* = $848A
mem_fill_x_3:
        ldy     #$00                          // keep index in-page at 0
        txa                                   // A := fill value from X
        sta     (fill_pointer),y              // write one byte

        inc     fill_ptr_lo                   // advance dest pointer (lo)
        bne     update_counters_3             // no wrap → skip hi
        inc     fill_ptr_hi                   // wrap → carry into hi

update_counters_3:
        lda     fill_count_lo                 // load low counter
        bne     counter_lo_decrement_3        // if not zero, skip borrow
        dec     fill_count_hi                 // borrow from high

counter_lo_decrement_3:
        dec     fill_count_lo                 // dec low counter

        lda     fill_count_lo
        ora     fill_count_hi                 // both zero?
        bne     mem_fill_x_3                  // no → continue
        rts                                   // yes → done
/*
================================================================================
  clear_cad0_cb90
================================================================================
Summary
	Zero-fill a contiguous memory region starting at ZFILL_CAD0_START for
	ZFILL_CAD0_SIZE bytes using the byte value ZFILL_VALUE_ZERO. Delegates the
	actual write loop to `mem_fill_x_3`.
================================================================================
*/
* = $84A4
clear_cad0_cb90:
        lda     #<ZFILL_CAD0_START             
        sta     fill_ptr_lo                    
        lda     #>ZFILL_CAD0_START             
        sta     fill_ptr_hi                    
        lda     #<ZFILL_CAD0_SIZE              
        sta     fill_count_lo               
        lda     #>ZFILL_CAD0_SIZE
        sta     fill_count_hi
        ldx     #ZFILL_VALUE_ZERO              
        jsr     mem_fill_x_3                   
        rts
		
/*
================================================================================
  setup_border_and_sprite_colors 
================================================================================
Summary
	Sets the VIC-II border color to black and programs the global sprite multicolor
	values (MC1 = black, MC0 = light red). Leaves I/O banking restored.

Description
	* Map in I/O to access VIC-II registers.
	* Set border color to black.
	* Set sprite multicolor 1 to black and multicolor 0 to light red.
	* Map out I/O to restore the normal memory configuration.

Notes
	* MC0 corresponds to pixel bit-pair 01 and MC1 to bit-pair 11 in multicolor
	  sprite pixels.
================================================================================
*/
* = $84BA		
setup_border_and_sprite_colors:
        // ------------------------------------------------------------
        // Map in I/O to access VIC-II registers
        // ------------------------------------------------------------
        ldy     #MAP_IO_IN
        sty     cpu_port

        // ------------------------------------------------------------
        // Set border color to black
        // ------------------------------------------------------------
        lda     #$00
        sta     vic_border_color_reg

        // ------------------------------------------------------------
        // Set global sprite multicolor values
        // ------------------------------------------------------------
        lda     #COLOR_BLACK
        sta     vic_sprite_mcolor1_reg          
        lda     #COLOR_LIGHT_RED
        sta     vic_sprite_mcolor0_reg          

        // ------------------------------------------------------------
        // Map out I/O and restore memory configuration
        // ------------------------------------------------------------
        ldy     #MAP_IO_OUT
        sty     cpu_port

        rts
/*
================================================================================
 clear_sprite_regs_and_sprite_memory
================================================================================
Summary
	Maps in I/O, clears sprite control and collision registers, enables multicolor
	for sprites 0–6, sets the engine sprite-shape table pointer, maps I/O out, and
	zeros the sprite memory region.

Description
	* Map in I/O by writing MAP_IO_IN to cpu_port.
	* Clear X-expand, Y-expand, priority, and both collision latches with $00.
	* Enable multicolor for sprites 0–6 by writing SPR_MC_ENABLE_0_TO_6.
	* Point sprite_shape_data_ptr at SPRITE_SHAPE_SET1_ADDR for engine lookups.
	* Map out I/O by writing MAP_IO_OUT to cpu_port.
	* Prepare a zero fill over SPRITE_BASE_0 for SPRITE_MEM_SIZE bytes.
	* Call mem_fill_x_3 to clear the sprite memory region.

Notes
	* Writing $00 to $D01E and $D01F clears collision latches.
	* Bit7 of $D01C left clear keeps sprite 7 in hires while 0–6 are multicolor.
================================================================================
*/
* = $84D2
clear_sprite_regs_and_sprite_memory:
        // ------------------------------------------------------------
        // Map in I/O area to access VIC-II registers
        // ------------------------------------------------------------
        ldy     #MAP_IO_IN
        sty     cpu_port

        // ------------------------------------------------------------
        // Clear sprite control and collision registers
        // ------------------------------------------------------------
        lda     #$00
        sta     vic_sprite_x_expand_reg                      // sprite double width
        sta     vic_sprite_y_expand_reg                      // sprite double height
        sta     vic_sprite_priority_reg                      // sprite priority
        sta     vic_coll_spr_spr_reg                         // sprite-sprite collisions
        sta     vic_coll_spr_bg_reg                          // sprite-background collisions

        // ------------------------------------------------------------
        // Enable multicolor mode for sprites 0–6 (bit7=0 keeps sprite 7 hires)
        // ------------------------------------------------------------
        lda     #SPR_MC_ENABLE_0_TO_6
        sta     vic_sprite_mc_enable_reg

        // ------------------------------------------------------------
        // Set engine sprite shape data pointer ($CBF8)
        // ------------------------------------------------------------
        lda     #<SPRITE_SHAPE_SET1_ADDR
        sta     sprite_shape_data_ptr
        lda     #>SPRITE_SHAPE_SET1_ADDR
        sta     sprite_shape_data_ptr + 1

        // ------------------------------------------------------------
        // Map out I/O and restore memory configuration
        // ------------------------------------------------------------
        ldy     #MAP_IO_OUT
        sty     cpu_port

        // ------------------------------------------------------------
        // Fill sprite memory region ($E000–$EFFF) with zero
        // ------------------------------------------------------------
        lda     #<SPRITE_BASE_0
        sta     fill_ptr_lo
        lda     #>SPRITE_BASE_0
        sta     fill_ptr_hi
        lda     #<SPRITE_MEM_SIZE
        sta     fill_count_lo
        lda     #>SPRITE_MEM_SIZE
        sta     fill_count_hi

        ldx     #$00
        jsr     mem_fill_x_3                   // clear 4 KB of sprite memory

        rts		
/*
================================================================================
  init_video_mode_and_click_flag
================================================================================
Summary
	Sets the video setup mode to the default value and sets the room scene “clicked”
	flag to TRUE so downstream UI logic starts in an active or ready state.
================================================================================
*/
* = $850E		
init_video_mode_and_click_flag:
        lda     #VID_SETUP_NONE
        sta     video_setup_mode                // default video setup mode

        lda     #TRUE
        sta     room_scene_clicked_flag         // mark scene click flag active

        rts
/*
================================================================================
clear_gfx_memory
================================================================================
Summary
	Clear both scene frame buffers and dot-data, then paint the top color-RAM bar.
	Uses mem_fill_x_3 for RAM clears and writes $COLOR_BAR_FILL to $D800–$D827.

Description
	1. For each frame buffer: set fill_ptr_* to start, fill_count_* to FRAMEBUF_SIZE,
	   X=$00, call mem_fill_x_3.
	2. Clear dot-data similarly with DOTDATA_*.
	3. Map I/O in via cpu_port, write COLOR_BAR_FILL across COLOR_BAR_LEN bytes at
	   vic_color_ram (indexed by Y), then map I/O out.

Notes
	* Assumes RAM under I/O/ROM is already visible when clearing frame/dot-data.
	* COLOR_BAR_LEN is written from end to start (Y decrements from LEN-1 to 0).
	* mem_fill_x_3 requires a nonzero count; constants must not be zero.
================================================================================
*/
* = $8518
clear_gfx_memory:
        // ------------------------------------------------------------
        // Clear frame buffer 1 ($C800–$CBE7)
        // ------------------------------------------------------------
        lda     #<FRAMEBUF1_START
        sta     fill_ptr_lo
        lda     #>FRAMEBUF1_START
        sta     fill_ptr_hi
        lda     #<FRAMEBUF_SIZE
        sta     fill_count_lo
        lda     #>FRAMEBUF_SIZE
        sta     fill_count_hi
        ldx     #$00
        jsr     mem_fill_x_3

        // ------------------------------------------------------------
        // Clear frame buffer 2 ($CC00–$CFE7)
        // ------------------------------------------------------------
        lda     #<FRAMEBUF2_START
        sta     fill_ptr_lo
        lda     #>FRAMEBUF2_START
        sta     fill_ptr_hi
        lda     #<FRAMEBUF_SIZE
        sta     fill_count_lo
        lda     #>FRAMEBUF_SIZE
        sta     fill_count_hi
        ldx     #$00
        jsr     mem_fill_x_3

        // ------------------------------------------------------------
        // Clear dot-data ($D800–$DFFF)
        // ------------------------------------------------------------
        lda     #<DOTDATA_START
        sta     fill_ptr_lo
        lda     #>DOTDATA_START
        sta     fill_ptr_hi
        lda     #<DOTDATA_SIZE
        sta     fill_count_lo
        lda     #>DOTDATA_SIZE
        sta     fill_count_hi
        ldx     #$00
        jsr     mem_fill_x_3

        // ------------------------------------------------------------
        // Fill color RAM ($D800–$D827) with color $0E
        // ------------------------------------------------------------
        ldy     #MAP_IO_IN
        sty     cpu_port                     // map in I/O to access color RAM
        ldy     #COLOR_BAR_LEN - 1
fill_color_loop:
        lda     #COLOR_BAR_FILL
        sta     vic_color_ram,y
        dey
        bpl     fill_color_loop
        ldy     #MAP_IO_OUT
        sty     cpu_port                     // restore normal mapping
        rts
/*
================================================================================
 init_actor_render_state
================================================================================
Summary
	Sets the actor sprite base high byte, initializes the room mask base pointer,
	and clears all actor→sprite assignments to a sentinel state.

Global Outputs
	* actor_sprite_base+1           updated with >SPRITE_BASE_0
	* mask_base_ptr                 set to ROOM_MASK_PTR
	* actor_sprite_index[0..N-1]    set to NO_SPRITE for N=ACTOR_COUNT_TOTAL

Description
	* Write the sprite memory base page into the high byte of actor_sprite_base.
	* Program the default room mask pointer as a 16-bit address.
	* Iterate over all actors and mark their sprite index as unassigned (NO_SPRITE)
	  to prevent stale bindings before the allocator runs.
================================================================================
*/
* = $856A		
init_actor_render_state:
        // ------------------------------------------------------------
        // Set actor sprite memory base high byte
        // ------------------------------------------------------------
        lda     #>SPRITE_BASE_0
        sta     actor_sprite_base + 1             // base page for actor sprite data

        // ------------------------------------------------------------
        // Set default room scene mask layer pointer
        // ------------------------------------------------------------
        lda     #<ROOM_MASK_PTR
        sta     mask_base_ptr
        lda     #>ROOM_MASK_PTR
        sta     mask_base_ptr + 1

        // ------------------------------------------------------------
        // Clear actor→sprite mapping table (4 entries)
        // ------------------------------------------------------------
        ldy     #$00
clear_actor_sprite_loop:
        lda     #NO_SPRITE
        sta     actor_sprite_index,y           // mark no sprite assigned
        iny
        cpy     #ACTOR_COUNT_TOTAL
        bne     clear_actor_sprite_loop

        rts
/*
================================================================================
  init_game_engine — initialize baseline UI, actor, and runtime state
================================================================================

Summary
	Initializes the sentence/UI system, default verb, timers, lighting mode, loader
	disk ID, camera position, costume↔actor tables, and RNG seed. Leaves the engine
	ready for input with the sentence bar marked for refresh.

Global Outputs
	 sentstk_top_idx            set to SENTENCE_STACK_EMPTY_IDX (empty stack)
	 sentstk_free_slots         set to SENTENCE_STACK_SIZE
	 std_msg_countdown          set to STD_MSG_DEFAULT_TICKS (16-bit)
	 text_delay_factor          set to TEXT_DELAY_DEFAULT
	 current_verb_id            set to VERB_WALK_TO
	 sentence_bar_needs_refresh set to TRUE
	 global_lights_state        set to LIGHTS_FLASHLIGHT_ONLY
	 active_side_id             set to GAME_DISK_ID_SIDE2
	 cam_current_pos            set to CAMERA_INIT_POS
	 costume_clip_set[0..COSTUME_MAX_INDEX] = CLIP_STAND_DOWN
	 actor_for_costume[0..COSTUME_MAX_INDEX] = NO_ACTOR
	 actor_cur_facing_direction[0..ACTOR_MAX_INDEX] = DIRECTION_DOWN
	 costume_for_actor[0..ACTOR_MAX_INDEX] = NO_COSTUME
	 random_1, random_2         set to RNG_SEED_1 / RNG_SEED_2

Description
	* Reset sentence stack: empty top index and restore full free-slot count.
	* Load standard message-bar delay into the 16-bit countdown timer.
	* Set per-character text speed factor.
	* Force default verb to “Walk to” and request a sentence-bar refresh.
	* Program global lighting mode for initial scene rules.
	* Select the desired loader disk-side identifier.
	* Initialize camera position to the engine’s default.
	* Normalize costume tables: default standing clip and no actor mapping.
	* Normalize actor tables: default direction and no costume assigned.
	* Seed the RNG to a stable nonzero state for deterministic startup behavior.
================================================================================
*/
* = $8586
init_game_engine:
        // ------------------------------------------------------------
        // Initialize sentence stack system
        // ------------------------------------------------------------
        lda     #SENTENCE_STACK_EMPTY_IDX 
        sta     sentstk_top_idx                // mark stack as empty
        lda     #SENTENCE_STACK_SIZE 
        sta     sentstk_free_slots             // 6 free slots available

        // ------------------------------------------------------------
        // Set message bar standard delay
        // ------------------------------------------------------------
        lda     #<STD_MSG_DEFAULT_TICKS
        sta     std_msg_countdown_lo             // low byte (30 ticks)
        lda     #>STD_MSG_DEFAULT_TICKS
        sta     std_msg_countdown_hi             // high byte

        // ------------------------------------------------------------
        // Set initial text speed factor
        // ------------------------------------------------------------
        lda     #TEXT_DELAY_DEFAULT
        sta     text_delay_factor

        // ------------------------------------------------------------
        // Set current verb to "Walk to"
        // ------------------------------------------------------------
        lda     #VERB_WALK_TO
        sta     current_verb_id

        // ------------------------------------------------------------
        // Enable refreshing the sentence bar
        // ------------------------------------------------------------
        lda     #TRUE
        sta     sentence_bar_needs_refresh

        // ------------------------------------------------------------
        // Set environment lights on
        // ------------------------------------------------------------
        lda     #LIGHTS_FLASHLIGHT_ONLY
        sta     global_lights_state

        // ------------------------------------------------------------
        // Set desired disk ID = game disk side 2
        // ------------------------------------------------------------
        lda     #GAME_DISK_ID_SIDE2
        sta     active_side_id

        // ------------------------------------------------------------
        // Initialize camera position
        // ------------------------------------------------------------
        lda     #CAMERA_INIT_POS
        sta     cam_current_pos

        // ------------------------------------------------------------
        // Initialize all costumes and their actor mappings
        // ------------------------------------------------------------
        ldx     #COSTUME_MAX_INDEX                            
loop_init_costumes:
        lda     #CLIP_STAND_DOWN
        sta     costume_clip_set,x              // default clip: standing looking down
        lda     #NO_ACTOR 
        sta     actor_for_costume,x             // unassigned actor
        dex
        bpl     loop_init_costumes

        // ------------------------------------------------------------
        // Reset actor attributes and "in use" settings
        // ------------------------------------------------------------
        ldx     #ACTOR_MAX_INDEX                            
reset_actors:
        lda     #DIRECTION_DOWN
        sta     actor_cur_facing_direction,x      // default direction: down
        lda     #NO_COSTUME
        sta     costume_for_actor,x             // mark actor unused
        dex
        bpl     reset_actors

        // ------------------------------------------------------------
        // Initialize engine temporary variables (unused)
        // ------------------------------------------------------------
        lda     #TRUE
        sta     $FE82
        sta     $FE81

        // ------------------------------------------------------------
        // Initialize random number generator seed
        // ------------------------------------------------------------
        lda     #RNG_SEED_1
        sta     random_1
        lda     #RNG_SEED_2
        sta     random_2

        rts

/*
function init_raster_and_sound_state():
    // If music is currently playing, do not touch raster/SID state.
    if music_playback_in_progress:
        return

    // If raster IRQ init is already pending/in progress, do nothing.
    if raster_irq_init_pending_flag:
        return

    // Handshake with raster IRQ: set a lock and wait until IRQ clears it.
    irq_sync_lock = LOCK_SET
    while irq_sync_lock != 0:
        ;  // spin until IRQ handler clears the lock

    // Immediately after the IRQ returns, disable interrupts.
    disable_interrupts()

    // Apply a known VIC screen-control preset (blank screen, fixed rows/scroll).
    vic_screen_control_reg_1 = CTRL1_BLANK_PRESET

    // Disable all sprites.
    vic_sprite_enable_reg = SPRITES_DISABLE_ALL

    // Set border color to black.
    vic_border_color_reg = COLOR_BLACK

    // Snapshot current voice activity mask.
    voice_active_mask_snapshot = voice_instr_active_mask

    // If any logical voices are active, mute SID master volume temporarily.
    if voice_active_mask_snapshot != 0:
        sid_master_volume = 0

    // Mark that raster-IRQ initialization is now pending/in progress.
    raster_irq_init_pending_flag = TRUE


function init_memory_sound_and_video():
    // 1) Setup interrupt vectors and disk drive code.
    setup_vectors_and_drive_code()

    // 2) Disable IRQs and configure CPU I/O port DDR.
    disable_interrupts()
    cpu_port_ddr = CPU_PORT_DDR_INIT

    // 3) Blank screen during memory initialization.
    vic_screen_control_reg_1 &= SCREEN_OFF_MASK

    // 4) Clear memory regions and set video defaults.
    cpu_set_memory_config(MAP_IO_OUT)

    clear_gfx_memory()                 // frame buffers, dot data, top color bar
    clear_zero_page_vars()             // ZP $0015..$00FF
    clear_game_and_engine_vars()       // engine/game region + CAD0–CB90
    init_video_mode_and_click_flag()   // video_setup_mode + room_scene_clicked_flag
    clear_gfx_memory()                 // second clear to ensure clean buffers
    setup_border_and_sprite_colors()   // border and sprite multicolors
    clear_sprite_regs_and_sprite_memory()

    // 5) Install an RTS NMI handler at a fixed address.
    nmi_handler = address(rts_stub)       // RTS stub

    // 6) Initialize actor rendering state (sprite base + room mask + mapping table).
    init_actor_render_state()

    // 7) Initialize sound engine state
    reset_sound_engine_state()

    // 8) Configure IRQ vector and VIC/CIA video layout.
    disable_interrupts()

    irq_vector = address(irq_handler1)

    vic_screen_control_reg_1 = CTRL1_BLANK_PRESET
    vic_memory_layout_shadow = VIC_MEM_LAYOUT_INIT
    vic_memory_layout_reg    = VIC_MEM_LAYOUT_INIT

    frame_buffer = 1                   // select framebuffer #1
    vic_screen_control_reg_2 = CTRL2_ROOM_REGION_PRESET

    vic_raster_line_reg = RASTER_IRQ_LINE_INIT
    vic_irq_flag_reg     = VIC_IRQ_RASTER_ACK
    vic_irq_mask_reg     = VIC_IRQ_RASTER_ACK   // enable raster IRQ

    dummy = cia1_irq_status_reg        // read to clear CIA1 IRQ flags

    // Select VIC bank $C000–$FFFF via CIA2 PRA.
    tmp = cia2_pra
    tmp = (tmp & CIA2_VIC_BANK_MASK) | CIA2_VIC_BANK_C000
    cia2_pra = tmp

    // Stop CIA1 timers on underflow (do not start them).
    cia1_timer_a_ctrl_reg = CIA1_TIMER_STOP_ON_UF
    cia1_timer_b_ctrl_reg = CIA1_TIMER_STOP_ON_UF

    // Enable all sprites.
    vic_sprite_enable_reg = SPRITES_ENABLE_ALL

    // 9) Copy a small init block from loader area into high RAM.
    copy_memory(
        src  = INIT_COPY_SRC_START,
        dest = INIT_COPY_DST_START,
        size = INIT_COPY_SIZE_BYTES
    )

    // 10) Relocate the main runtime blocks, then return.
    relocate_memory()


function copy_memory(src, dest, size):
    // Copies `size` bytes from src to dest forwards.
    // Precondition: size > 0 and dest is not inside the source range in a way
    // that would require backward copying.

    while size > 0:
        byte = read_byte(src)
        write_byte(dest, byte)

        src  = src  + 1
        dest = dest + 1
        size = size - 1


function relocate_memory():
    // 1) Copy character set and UI arrows.
    copy_memory(
        src  = BLOCK_CHARS_SRC_START,
        dest = BLOCK_CHARS_DST_START,
        size = BLOCK_CHARS_SIZE_BYTES
    )

    // 2) Copy graphics and script handler code.
    copy_memory(
        src  = BLOCK_GFX_CODE_SRC_START,
        dest = BLOCK_GFX_CODE_DST_START,
        size = BLOCK_GFX_CODE_SIZE_BYTES
    )

    // 3) Copy engine constants.
    copy_memory(
        src  = BLOCK_CONSTS_SRC_START,
        dest = BLOCK_CONSTS_DST_START,
        size = BLOCK_CONSTS_SIZE_BYTES
    )

    // 4) Copy decompression routines.
    copy_memory(
        src  = BLOCK_DECOMP_SRC_START,
        dest = BLOCK_DECOMP_DST_START,
        size = BLOCK_DECOMP_SIZE_BYTES
    )

    // 5) Copy initialization block used by runtime.
    copy_memory(
        src  = BLOCK_INIT_SRC_START,
        dest = BLOCK_INIT_DST_START,
        size = BLOCK_INIT_SIZE_BYTES
    )

    // 6) Initialize cursor position and interaction region.
    cursor_x_pos        = CURSOR_INIT_X
    cursor_y_pos        = CURSOR_INIT_Y
    hotspot_entry_ofs   = INTERACTION_REGION_DEFAULT
    update_cursor_physics_from_hotspot()

    // 7) Hide cursor and enable interrupts.
    hide_cursor_flag = TRUE
    enable_interrupts()

    // 8) Initialize raster interrupt environment.
    init_raster_irq_env()

    return


function clear_zero_page_vars():
    // Clear $0015 .. $00FF to zero, leave $0000..$0014 intact.
    addr = ZP_CLEAR_START   // $0015
    while addr <= 0x00FF:
        write_byte(addr, ZP_CLEAR_VALUE)   // 0
        addr = addr + 1


function clear_game_and_engine_vars():
    // Zero main engine range using mem_fill_x_3.
    fill_pointer = CLEAR_GAME_START
    fill_count   = CLEAR_GAME_SIZE
    fill_value   = CLEAR_VALUE_ZERO
    mem_fill_x_3(fill_pointer, fill_count, fill_value)

    // Then clear the CAD0–CB90 work area.
    clear_cad0_cb90()

function mem_fill_x_3(dest, size, value):
    // Fill `size` bytes at `dest` with `value`.
    // Precondition: size > 0.

    ptr  = dest
    remaining = size

    while remaining > 0:
        write_byte(ptr, value)
        ptr       = ptr + 1
        remaining = remaining - 1


function clear_cad0_cb90():
    fill_pointer = ZFILL_CAD0_START
    fill_count   = ZFILL_CAD0_SIZE
    fill_value   = ZFILL_VALUE_ZERO
    mem_fill_x_3(fill_pointer, fill_count, fill_value)


function setup_border_and_sprite_colors():
    // Border color = black.
    vic_border_color_reg = COLOR_BLACK

    // Global sprite multicolor 1 = black, 0 = light-red/skin.
    vic_sprite_mcolor1_reg = COLOR_BLACK
    vic_sprite_mcolor0_reg = COLOR_LIGHT_RED


function clear_sprite_regs_and_sprite_memory():
    // Clear sprite control and collision registers.
    vic_sprite_x_expand_reg  = 0
    vic_sprite_y_expand_reg  = 0
    vic_sprite_priority_reg  = 0
    vic_coll_spr_spr_reg     = 0
    vic_coll_spr_bg_reg      = 0

    // Enable multicolor mode for sprites 0–6 (7 stays hi-res).
    vic_sprite_mc_enable_reg = SPR_MC_ENABLE_0_TO_6

    // Point engine sprite shape lookup at the default shape set.
    sprite_shape_data_ptr = SPRITE_SHAPE_SET1_ADDR

    // Clear sprite memory region ($E000–$EFFF) to zero.
    fill_pointer = SPRITE_BASE_0
    fill_count   = SPRITE_MEM_SIZE
    fill_value   = 0
    mem_fill_x_3(fill_pointer, fill_count, fill_value)


function init_video_mode_and_click_flag():
    video_setup_mode        = VID_SETUP_NONE
    room_scene_clicked_flag = TRUE


function clear_gfx_memory():
    // 1) Clear frame buffer 1.
    mem_fill_x_3(
        dest  = FRAMEBUF1_START,
        size  = FRAMEBUF_SIZE,
        value = 0
    )

    // 2) Clear frame buffer 2.
    mem_fill_x_3(
        dest  = FRAMEBUF2_START,
        size  = FRAMEBUF_SIZE,
        value = 0
    )

    // 3) Clear dot-data region.
    mem_fill_x_3(
        dest  = DOTDATA_START,
        size  = DOTDATA_SIZE,
        value = 0
    )

    // 4) Paint top color-RAM bar with COLOR_BAR_FILL.
    for i from 0 to COLOR_BAR_LEN - 1:
        vic_color_ram[i] = COLOR_BAR_FILL


function init_actor_render_state():
    // Set actor sprite base to high byte of sprite memory base.
    actor_sprite_base_high = high_byte(SPRITE_BASE_0)

    // Set default room mask pointer.
    mask_base_ptr = ROOM_MASK_PTR

    // Initialize actor→sprite mapping table to NO_SPRITE.
    for actor_index from 0 to ACTOR_COUNT_TOTAL - 1:
        actor_sprite_index[actor_index] = NO_SPRITE


function init_game_engine():
    // Sentence stack initialization.
    sentstk_top_idx    = SENTENCE_STACK_EMPTY_IDX
    sentstk_free_slots = SENTENCE_STACK_SIZE

    // Standard message bar delay (16-bit).
    std_msg_countdown = STD_MSG_DEFAULT_TICKS

    // Text speed (characters per tick).
    text_delay_factor = TEXT_DELAY_DEFAULT

    // Default verb = "Walk to".
    current_verb_id = VERB_WALK_TO

    // Sentence bar needs initial refresh.
    sentence_bar_needs_refresh = TRUE

    // Lighting mode = flashlight-only.
    global_lights_state = LIGHTS_FLASHLIGHT_ONLY

    // Active disk side = game disk side 2.
    active_side_id = GAME_DISK_ID_SIDE2

    // Initialize camera position.
    cam_current_pos = CAMERA_INIT_POS

    // Normalize all costumes and their actor mappings.
    for costume_index from COSTUME_MAX_INDEX down to 0:
        costume_clip_set[costume_index] = CLIP_STAND_DOWN
        actor_for_costume[costume_index] = NO_ACTOR

    // Normalize all actors and their costume mappings.
    for actor_index from ACTOR_MAX_INDEX down to 0:
        actor_cur_facing_direction[actor_index] = DIRECTION_DOWN
        costume_for_actor[actor_index]          = NO_COSTUME

    // Seed RNG with fixed non-zero values.
    random_1 = RNG_SEED_1
    random_2 = RNG_SEED_2

*/