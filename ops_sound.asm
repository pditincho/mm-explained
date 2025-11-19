/*
================================================================================
  ops_sound.asm
================================================================================
Summary
        Implements script-level opcodes for starting and stopping sound effects
        and background music, and for selecting music based on disk side or
        preloaded slots.

Opcodes
        #$3C, #$BC              → op_stop_sound
        #$02                    → op_play_music
        #$1C, #$5C, #$9C, #$DC  → op_start_sound
        #$20                    → op_stop_music

Description
        - Bridges the script engine with the sound engine via small opcode
          handlers.
        - Maps I/O in and out around hardware-facing calls to stop/start sound
          and music.
        - Ensures sound and music resources are resident before starting them.
        - Maintains music pointer state and invokes per-music initialization
          code before playback.
================================================================================
*/

#importonce
#import "globals.inc"
#import "constants.inc"
#import "script_engine.asm"
#import "script_primitives.asm"
#import "sound_engine.asm"

/*
================================================================================
  op_stop_sound - Opcodes #$3C, #$BC
================================================================================

Summary
        Stop a specific sound effect by index while I/O is mapped in, then
        restore the normal memory configuration.

Arguments
        (script) byte0  → sound index to stop (hi from opcode bit7)

Returns
        None (requested sound instance is stopped if active)

Description
        - Maps I/O into the CPU address space so SID/sound hardware is visible.
        - Reads the sound index operand using script_load_operand_bit7.
        - Calls stop_sound to halt the sound associated with that index.
        - Restores the normal memory mapping used by the script engine.
================================================================================
*/
* = $65B9		
op_stop_sound:
		// Map I/O in so SID registers are accessible
        ldy     #MAP_IO_IN                   
        sty     cpu_port                     
	
		// Resolve sound index into A
        jsr     script_load_operand_bit7     
		
		// Stop the sound instance identified by A
        jsr     stop_sound                   

		// Map I/O out
        ldy     #MAP_IO_OUT                  
        sty     cpu_port                     
        rts                                  		
/*
================================================================================
  op_play_music - Opcode #02
================================================================================

Summary
        Select and start background music, preferring preloaded tracks and
        falling back to a disk-side default. Ensure required music resources
        are resident and run their initialization code.

Arguments
        (script) byte0  → music index to ensure resident (hi from opcode bit7)

Vars/State
        selected_music_idx    Chosen music index to start
        music_to_start_ptr    Pointer to selected music resource base
        music_init_code_ptr   Entry point for music-specific initialization code

Global Inputs
        sound_ptr_hi_tbl      Per-sound high-byte pointer table; non-zero → resident
        sound_ptr_lo_tbl      Per-sound low-byte pointer table for music resources
        sound_attr_tbl        Attribute/refcount table for sound resources
        active_side_id        Raw disk side identifier used to derive default music

Global Outputs
        music_to_start_ptr    Updated to base address of selected music resource
        music_init_code_ptr   Updated to point at music init routine (after header)
        sound_attr_tbl        Refcount temporarily incremented/decremented for music

Returns
        None (starts music playback via inline initialization code)

Description
        - Tries to use music slot #1 if its resource is already resident.
        - Falls back to music slot #2 if slot #1 is not resident.
        - If neither is resident, derives a music index from the active disk
          side ID, loads that music resource, and refreshes script pointers.
        - Temporarily bumps the selected music's refcount while
          ensuring the operand-specified music resource is resident.
        - Initializes music_to_start_ptr and music_init_code_ptr for the chosen
          music and calls its initialization routine with I/O mapped in.
        - Restores the normal memory mapping and refreshes script pointers
          after initialization, then returns to the script engine.
================================================================================
*/	
.label music_init_code_ptr = $6621
* = $65C8
op_play_music:
        // ------------------------------------------------------------
        // Prefer music #1 if loaded; else music #2; else load based on
        // active disk side.
        // ------------------------------------------------------------
        ldx     #MUSIC_IDX_1                 // X := try music slot #1 first
        lda     sound_ptr_hi_tbl,x           // A := hi byte of music #1 base; non-zero → resident
        bne     start_selected_music         // If music #1 is resident, use it

        ldx     #MUSIC_IDX_2                 // X := fall back to music slot #2
        lda     sound_ptr_hi_tbl,x           // A := hi byte of music #2 base; non-zero → resident
        bne     start_selected_music         // If music #2 is resident, use it

        // Musics #1 and #2 not resident: map side ID (31/32 → 1/2)
        lda     active_side_id               // A := raw disk side ID (#$31 or #$32)
        sec                                  // Prepare for subtract to normalize side ID
        sbc     #DISK_SIDE_ID_BASE           // A := normalized music index (1 or 2)
        pha                                  // Save normalized index on stack for later
		
        // Ensure this music resource is loaded
		jsr     rsrc_cache_sound   
		// Fix script pointers if resource load relocated memory
        jsr     refresh_script_addresses_if_moved 
		
        pla                                  // Restore normalized music index
        tax                                  // X := music index selected from disk side

start_selected_music:
        // ------------------------------------------------------------
        // X := music index to use; increase refcount during load
        // and then restore it.
        // ------------------------------------------------------------
        stx     selected_music_idx           // Remember chosen music index in selected_music_idx
        inc     sound_attr_tbl,x             // Increase refcount of music rsrc

        // Resolve music index operand (but keep current selected_music_idx in X)
        jsr     script_load_operand_bit7     // A := operand music index (hi from opcode bit7)
        pha                                  // Save script-specified index (used for loading)
		
        // Ensure operand-specified music is resident
		jsr     rsrc_cache_sound   

		//Decrease refcount of music rsrc
        ldx     selected_music_idx           // Restore logical music we actually want to start into X
        dec     sound_attr_tbl,x             // Dec refcount

        // ------------------------------------------------------------
        // Initialize music_to_start_ptr for this music
        // ------------------------------------------------------------
        lda     sound_ptr_hi_tbl,x           // A := hi byte of chosen music base address
        sta     music_to_start_ptr_hi          
        lda     sound_ptr_lo_tbl,x           
        sta     music_to_start_ptr_lo
		
		// Initialize internal music pointer state from this base
        jsr     setup_music_pointers         

        // ------------------------------------------------------------
        // Compute inlined init-code pointer: sound_ptr + 4 (skip header)
        // ------------------------------------------------------------
        ldx     selected_music_idx           // X := chosen music index again (for pointer math)
        lda     sound_ptr_lo_tbl,x           // A := lo byte of music base
        clc                                  
        adc     #MEM_HDR_LEN                 // skip resource header
        sta     music_init_code_ptr         // music_init_code_ptr.lo := low part of init entry point
		
        lda     sound_ptr_hi_tbl,x           
        adc     #$00                         
        sta     music_init_code_ptr + 1

        // ------------------------------------------------------------
        // Map in I/O, run music startup code via inlined JSR, then
        // restore mapping and refresh script addresses.
        // ------------------------------------------------------------
        pla                                  // Restore script operand (music index) for init code, if used
		
		// Map I/O in
        ldy     #MAP_IO_IN                   
        sty     cpu_port                     

        jsr     $FFFF // music_init_code_ptr - Call per-music initialization routine at computed entry (inlined)

		// Map I/O out
        ldy     #MAP_IO_OUT                  
        sty     cpu_port                     
		
		// Fix script pointers if init caused relocation
        jsr     refresh_script_addresses_if_moved 
        rts                                  
/*
================================================================================
  op_start_sound - Opcodes #$1C, #$5C, #$9C, #$DC
================================================================================

Summary
        Stop any currently playing instance of a given sound index, ensure the
        sound resource is resident, then start playback of that sound.

Arguments
        (script) byte0  → sound index (lo)
        (script) bit7   → sound index hi (from opcode bit7)

Returns
        None (starts or restarts the requested sound effect).

Description
        - Reads a sound index operand from the script using
          script_load_operand_bit7.
        - Maps I/O in via cpu_port to safely call stop_sound for this index,
          ensuring any prior instance is terminated.
        - Restores the normal memory mapping, then calls
          rsrc_cache_sound to load the sound resource if needed.
        - Maps I/O in again and calls start_sound to begin playback.
        - Restores the memory map and calls refresh_script_addresses_if_moved
          to fix up any script pointers if relocation occurred.

================================================================================
*/
.label sound_index = $d0
* = $662B
op_start_sound:		
		// Resolve sound index
        jsr     script_load_operand_bit7

start_sound_with_loaded_index:
        sta     sound_index

        // ------------------------------------------------------------
        // Map I/O in, stop any existing instance of this sound
        // ------------------------------------------------------------
        ldy     #MAP_IO_IN
        sty     cpu_port

        lda     sound_index
        jsr     stop_sound

        // ------------------------------------------------------------
        // Restore normal memory map
        // ------------------------------------------------------------
        ldy     #MAP_IO_OUT
        sty     cpu_port

        // ------------------------------------------------------------
        // Ensure sound resource is resident
        // ------------------------------------------------------------
        lda     sound_index
        jsr     rsrc_cache_sound

        // ------------------------------------------------------------
        // Map I/O in again to start the sound
        // ------------------------------------------------------------
        lda     sound_index
        ldy     #MAP_IO_IN
        sty     cpu_port
        jsr     start_sound

        // ------------------------------------------------------------
        // Restore memory map and refresh script pointers if moved
        // ------------------------------------------------------------
        ldy     #MAP_IO_OUT
        sty     cpu_port
        jsr     refresh_script_addresses_if_moved
        rts
/*
================================================================================
  op_stop_music - Opcode #$20
================================================================================

Summary
        Stop the music subsystem and reset music-related state by calling the
        shared stop_music routine.

Description
        - Invokes stop_music to halt any music currently playing.
        - Clears music voice usage and playback-in-progress flags via stop_music.
        - Leaves overall script execution state unchanged (no operands consumed).
================================================================================
*/
* = $6A57		
op_stop_music:
        jsr     stop_music
        rts
