/*
================================================================================
 start_game.asm
================================================================================

Summary
	Initialize core engine state and load boot-time tables from disk.
	Configure framebuffer, player and camera, clear memory regions,
	lock and point embedded sounds, set up raster IRQs, then launch
	the start-game global script. Includes a helper to clear the
	inventory table and tail-jump into heap initialization.

	Control falls through into the main loop after launching the
	start-game script. Helper routine tail-jumps to heap setup.

Notes
	Depends on high-level disk helpers, IRQ handlers, and global
	constants included by this module. Keep sizes and addresses in
	sync with disk image layout and memory map.
================================================================================
*/

#importonce

#import "globals.inc"
#import "constants.inc"
#import "disk_high_level.asm"
#import "irq_handlers.asm"

.const OBJ_ATTR_LEN          = $0100      // Size of object attribute block loaded from disk
.const STORAGE_META_LEN      = $03A2      // Size of resource storage metadata block
.const ENGINE_VARS_LEN       = $68        // Number of engine variable bytes to clear
.const GAME_VARS_LEN         = $8B        // Number of game state variable bytes to clear
.const INVENTORY_AREA_LEN    = $0B4A      // Length of memory block for inventory objects
.const SOUND_LOCK_MASK       = $80        // Bit7 flag used to mark sound resources as locked
.const SOUND3_PTR            = $53CD      // Address of embedded sound data #3
.const SOUND4_PTR            = $53FA      // Address of embedded sound data #4
.const SOUND5_PTR            = $5427      // Address of embedded sound data #5
.const SCRIPT_IDX_START_GAME = $01        // Global script index for game startup sequence

/*
================================================================================
  start_game
================================================================================

Summary
	Boot the game runtime: ensure correct disk side, load object attributes
	and storage metadata from T1,S0/S1, configure framebuffer, initialize
	player and camera state, clear engine and game variables, lock core
	sounds and set embedded sound pointers, initialize raster IRQs, and
	launch the start-game global script. Execution continues into the main
	loop.

Returns
	Does not return to a caller. Execution falls through into the main loop
	after launching the start-game script.

Global Outputs
	frame_buffer                  preset to initial layout ID
	frame_buffer_base             set to base address of active framebuffer
	current_kid_idx               initialized to default kid
	cam_mode                      set to “follow actor” mode
	cam_follow_costume_id         set to current_kid_idx
	task_cur_idx                  set to “no active script”
	engine_vars[...]              cleared to zero
	game_vars[...]                cleared to zero
	sound_liveness_tbl[3,4,5,6,8,9]  bit7 set to lock resources
	sound_ptr_{lo,hi}_tbl[3..5]   set to embedded track pointers
	(via calls) raster IRQ environment initialized
	(via calls) global start-game script launched

Description
	- Ensure the correct game disk side is active.
	- Read from track 1:
		• Sector 0 @ offset 2 → copy the object attribute table to RAM.
		• Sector 1 @ offset 2 → copy resource storage metadata to RAM.
	- Configure framebuffer preset and base address.
	- Initialize player selection, camera mode, and follow target.
	- Mark no current script as active.
	- Clear engine and game variable blocks.
	- Lock key sound resources and point tracks 3–5 to embedded data.
	- Initialize raster interrupts for display timing.
	- Launch the start-game global script; proceed into the main loop.
================================================================================
*/
* = $044F
start_game:
        jsr     clear_inventory_memory

        // Ensure game disk side 1 is in the drive
        lda     #GAME_DISK_ID_SIDE1
        jsr     disk_ensure_correct_side

        // Setup disk read: track #$01, offset #$00; then seek to sector #$00, offset #$02
        ldx     #$00
        ldy     #$01
        jsr     disk_init_chain
        ldx     #$02
        ldy     #$00
        jsr     disk_seek_and_read_sector

        // Load object attributes block
        ldx     #<object_attributes
        ldy     #>object_attributes
        lda     #<OBJ_ATTR_LEN
        sta     disk_copy_count_lo
        lda     #>OBJ_ATTR_LEN
        sta     disk_copy_count_hi
        jsr     disk_stream_copy

        // Next sector: track #$01, sector #$01, offset #$02
        ldx     #$02
        ldy     #$01
        jsr     disk_seek_and_read_sector

        // Load resource storage metadata
        ldx     #<room_disk_side_tbl
        ldy     #>room_disk_side_tbl
        lda     #<STORAGE_META_LEN
        sta     disk_copy_count_lo
        lda     #>STORAGE_META_LEN
        sta     disk_copy_count_hi
        jsr     disk_stream_copy

        // Frame buffer preset → #$01, base := $CC28
        lda     #FRAMEBUFFER_INIT
        sta     frame_buffer
        lda     #<VIEW_FRAME_BUF_1
        sta     frame_buffer_base
        lda     #>VIEW_FRAME_BUF_1
        sta     frame_buffer_base + 1

        // Current kid := Dave
        lda     #COSTUME_INDEX_DAVE
        sta     current_kid_idx

        // Camera mode := follow costume
        lda     #CAM_MODE_FOLLOW_ACTOR
        sta     cam_mode

        // Follow costume := current kid
        lda     current_kid_idx
        sta     cam_follow_costume_id

        // No current script slot
        lda     #TASK_IDX_NONE
        sta     task_cur_idx

        // Clear engine variables
        ldx     #ENGINE_VARS_LEN
clear_engine_vars:
        lda     #$00
        sta     engine_vars,x
        dex
        bne     clear_engine_vars

        // Clear game state variables
        ldx     #GAME_VARS_LEN
clear_game_variables:
        lda     #$00
        sta     game_vars,x
        dex
        bne     clear_game_variables

        // Lock sounds 3,4,5,6,8,9 (set bit7 in sound_liveness_tbl)
        lda     #SOUND_LOCK_MASK
        sta     sound_liveness_tbl + 3
        sta     sound_liveness_tbl + 4
        sta     sound_liveness_tbl + 5
        sta     sound_liveness_tbl + 6
        sta     sound_liveness_tbl + 8
        sta     sound_liveness_tbl + 9

        // Embedded sound pointers for tracks 3..5
        lda     #<SOUND3_PTR
        sta     sound_ptr_lo_tbl + 3
        lda     #>SOUND3_PTR
        sta     sound_ptr_hi_tbl + 3

        lda     #<SOUND4_PTR
        sta     sound_ptr_lo_tbl + 4
        lda     #>SOUND4_PTR
        sta     sound_ptr_hi_tbl + 4

        lda     #<SOUND5_PTR
        sta     sound_ptr_lo_tbl + 5
        lda     #>SOUND5_PTR
        sta     sound_ptr_hi_tbl + 5

        // Raster IRQ environment
        jsr     init_raster_irq_env

        // Start global script for game start 
        lda     #SCRIPT_IDX_START_GAME
        jsr     launch_global_script

        // Fall through to main loop
/*
================================================================================
  clear_inventory_memory
================================================================================
Summary
	Zero-fill the contiguous inventory object memory region and then
	tail-jump to heap initialization.

Returns
	Does not return to caller from this label; tail-jumps to
	init_heap_free_list.

Description
	- Set destination pointer to the inventory object table base.
	- Set byte-count to the full inventory table length.
	- Load X with #$00 and call mem_fill_x to clear the region.
	- Tail-jump to init_heap_free_list to proceed with allocator setup.
================================================================================
*/
* = $5D1A
clear_inventory_memory:
        lda     #<inventory_objects
        sta     fill_dest_ptr                         
        lda     #>inventory_objects
        sta     fill_dest_ptr + 1                         

        lda     #<INVENTORY_AREA_LEN
        sta     fill_byte_cnt    
        lda     #>INVENTORY_AREA_LEN
        sta     fill_byte_cnt + 1

        ldx     #$00             
        jsr     mem_fill_x                  

        jmp     init_heap_free_list         // continue with heap initialization
	