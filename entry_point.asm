#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "init_engine.asm"
#import "start_game.asm"

/*
================================================================================
  game_code_entry - Main game entry and heap initialization
================================================================================
Summary
	Entry point after the loaders finish. Sets baseline VIC/CPU state,
	initializes core subsystems, primes raster IRQ init, and clears the
	heap region before starting the main game loop.

Returns
	Does not return (tail-jumps into the main game loop via start_game)

Global Inputs
	None directly (assumes loaders have prepared memory layout and vectors)

Global Outputs
	raster_irq_init_pending_flag – Set TRUE to request raster IRQ setup
	fill_dest_ptr – Initialized to start of heap region
	fill_byte_cnt – Initialized to heap size in bytes

Vars/State
	fill_dest_ptr Zero-page pointer used by mem_fill_x_1
	fill_byte_cnt Zero-page byte count used by mem_fill_x_1

Description
	- Establishes a known stack pointer and blank VIC text mode.
	- Disables interrupts and restores normal RAM/I/O mapping.
	- Calls init_memory_sound_and_video and init_game_engine to prepare
	low-level subsystems and game state.
	- Flags raster IRQ initialization as required, then runs a synchronized
	raster/sound init routine and resets logical voices.
	- Clears the heap region [$7B7B–$C800) to zero using mem_fill_x_1,
	overwriting temporary setup code in the upper memory range.
	- Jumps into start_game to begin the main game loop.
================================================================================
*/
* = $0400
game_code_entry:
        // Initialize stack pointer
        ldx     #$ff
        txs

        // Set blank screen, 24 rows, vertical scroll 0
        lda     #VIC_CTRL_BLANK_0
        sta     vic_screen_control_reg_1

        // Disable interrupts
        sei

        // Map I/O out (memory config = $24)
        ldy     #MAP_IO_OUT
        sty     cpu_port

        // Initialize memory, sound, and video subsystems
        jsr     init_memory_sound_and_video

        // Initialize core game engine state
        jsr     init_game_engine

reset_game_engine:
        // Mark raster-IRQ initialization as needed
        lda     #TRUE
        sta     raster_irq_init_pending_flag

        // Perform synchronized raster/sound initialization
        jsr     init_raster_and_sound_state

        // Reset logical voices and related sound state
        jsr     reset_logical_voices_and_state

        // Fill "heap" ($7B7B–$C800) with #$00
        // Note: this overwrites setup code in the range $7FFF–$85E3
        lda     #<HEAP_FREE_HDR_ADDR
        sta     fill_dest_ptr
        lda     #>HEAP_FREE_HDR_ADDR
        sta     fill_dest_ptr + 1
        lda     #<HEAP_INIT_SIZE_BYTES
        sta     fill_byte_cnt
        lda     #>HEAP_INIT_SIZE_BYTES
        sta     fill_byte_cnt + 1
        ldx     #$00
        jsr     mem_fill_x_1

        // Enter main game loop / start game
        jmp     start_game


/*
function game_code_entry():
    // Initialize CPU basics
    set_stack_pointer(0xFF)

    // Put VIC into a blank, known text mode (24/25 rows, scroll=0)
    vic_set_screen_control(VIC_CTRL_BLANK_0)

    // Disable interrupts while we reconfigure hardware
    disable_interrupts()

    // Map I/O out so RAM under I/O/ROM is visible (memory config = MAP_IO_OUT)
    cpu_set_memory_config(MAP_IO_OUT)

    // Bring up low-level subsystems: memory, video, sprites, sound baseline
    init_memory_sound_and_video()

    // Initialize high-level game engine state: UI, verbs, camera, actors, RNG, etc.
    init_game_engine()

reset_game_engine:
    // Mark that we need to (re)initialize raster IRQ-related state
    raster_irq_init_pending_flag = TRUE

    // Coordinate with raster IRQ, blank screen, mute SID if needed
    // and establish a known VIC/SID baseline
    init_raster_and_sound_state()

    // Reset logical-voice bookkeeping and related sound engine state
    reset_logical_voices_and_state()

    // Clear heap region by filling it with zeroes
    fill_dest_ptr  = HEAP_FREE_HDR_ADDR
    fill_byte_cnt  = HEAP_INIT_SIZE_BYTES
    fill_value_x   = 0
    mem_fill_x_1(fill_dest_ptr, fill_byte_cnt, fill_value_x)

    // Tail-jump into main game startup sequence
    start_game()      // does not return

*/