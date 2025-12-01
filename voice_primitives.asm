/*================================================================================
  Voice Primitives — How This System Works (Approachable Technical Overview)
================================================================================

This file implements the sound engine’s “voice allocation layer.”  
It sits between two worlds:

    (1) Sounds — abstract resources with priorities and per-voice data streams
    (2) Voices — actual execution slots (real SID voices, virtual voices, and
        the special filter/voice-3 control slot)

Its job is to decide:  
        “Can this sound start right now? If yes, on which voices?”  
and later:  
        “How do we safely stop this sound and release its voices?”

The whole system centers around these main ideas:
--------------------------------------------------------------------------------
1.  **Sounds specify what voices they need**
    Each sound resource contains:
       • a priority value  
       • a “voice requirements” bitmask  
       • a list of 16-bit offsets: one per requested voice

    From the bitmask the engine derives:
       • how many real SID voices (0–2) the sound needs  
       • whether it requires special voice-3  
       • how many virtual voices (4–7) it should receive

    All this information determines the final configuration of per-voice
    base pointers and offsets into the sound’s data tables.

--------------------------------------------------------------------------------
2.  **Voices are limited and may need to evict lower-priority sounds**
    When a sound wants to start, the engine checks:

       • Are the requested real voices already free?
       • If not, can they be freed by stopping other sounds?
       • Are those other sounds lower priority than the new one?
       • Is voice-3 available, or can it be taken based on priority?

    The eviction system repeatedly:
       • identifies the “lowest priority active sound”  
       • stops all of its voices  
       • rechecks whether enough real voices are now free  

    Eviction stops as soon as the requested number of voices becomes available.
    If availability is still insufficient, the start fails.

    Special rule for music:
       If music is playing and uses all three real voices, NO other sound may
       start unless it needs zero real voices.

--------------------------------------------------------------------------------
3.  **Starting a sound = allocate voices + bind per-voice data streams**
    Once enough voices are free, the engine proceeds:

       1) It allocates the first voice (real or special)  
       2) It walks through the bitmask again, and for each set bit:
            • allocates a real or special voice, or  
            • assigns a virtual voice (logical slot only)  
            • reads the next pair of bytes (16-bit offset) from the sound  
              resource  
            • stores the sound’s base pointer + offset into the voice tables

    End result: each logical voice slot X is pointed at the correct data inside
    the sound resource, and “sound_id_to_start_on_voice[X]” identifies which
    sound owns that slot. The sound is now active.

--------------------------------------------------------------------------------
4.  **Stopping a sound safely releases its voices**
    Stopping is broken into layers:

       • Simple stop: free the voices but don’t modify refcounts, don’t clear
         alternate settings, and don’t touch multiplexings.
       • Full stop: used when a sound was the “starting sound”; this also:
            – decrements its reference counter (unless music is active)
            – clears alternate filter settings
            – ends multiplexings
            – resets internal state tracking which sound is “starting”

    At the voice level, stopping:
       • clears execution flags and repeat counters  
       • releases real voices 0–2  
       • releases virtual voices (paired with real ones)  
       • may release special voice-3 if the filter no longer uses it  
       • optionally clears SID gate/waveform bits (only if safe to do so)

    This ensures logical consistency: no leftover state, no ghosts, and the
    SID chip is left in a clean, correct condition.

--------------------------------------------------------------------------------
5.  **Safety mechanisms prevent inconsistent state**
    During the entire start/stop process the engine:
       • disables sound processing to prevent reentry  
       • saves/restores A/X/Y  
       • tracks “current starting sound” to decide whether cleanup should be
         full or partial  
       • ensures all voice/offset tables are updated atomically  

    The logic is carefully arranged so that a sound can only start if:
       • its resource is loaded  
       • the required voices can be guaranteed  
       • voice-3 is available (or can be taken legally)  
       • and music voice usage allows it  

--------------------------------------------------------------------------------
In short:
    This file is the traffic-controller of the audio subsystem.  
    It arbitrates voice usage based on priority, music policies, and resource
    ownership; performs controlled eviction; and sets up all per-voice pointers
    needed for the playback engine to execute each voice’s instruction stream.

================================================================================*/

#importonce

#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "sid_voice_controller.asm"
#import "voice_allocation.asm"
#import "sound_engine.asm"

.label sound_data_base            = $bc     // Zero-page pointer: base address of the current sound resource data

.label first_voice_index          = $4cf3   // Index of the first allocated voice slot (real or special) for this sound
.label voice3_requested_flag   	  = $4cf4   // Non-zero when the sound’s requirements mask requests voice 3 via bit 5
.label voice3_conflict_flag_b     = $4cf6   // Non-zero if voice 3 is requested and already in use by another sound
.label real_voices_needed         = $4cfe   // Count of real SID voices (0–2) required by the pending sound
.label insufficient_voices_flag_b = $4d00   // 0 if current real voices suffice; $ff if more real voices are needed
.label voice_requirements_mask    = $4d02   // Bitfield describing which real, virtual, and voice-3 slots this sound uses
.label start_sound_result         = $4d04   // Holds start_sound’s final return code (A) across register restore
.label saved_voice_slot_index     = $4d09   // Saved logical voice index while iterating/temporarily stopping voices


.const OFS_VOICE_REQS = $05

.label tmp_voice_sound_base_lo = $4815 //lo
.label tmp_voice_sound_base_hi = $4814 // hi


* = $4C1C
update_voice_stream_pointers:
        // Ensure the sound resource bound to this voice is actually loaded
        ldy     voice_sound_id_tbl,x
        lda     sound_ptr_hi_tbl,y
        bne     resource_loaded

        // Resource not in memory → stop sound and clear low-priority/multiplexing tracking if needed
        tya
        jsr     stop_sound_full_cleanup
        cmp     tracked_lowest_priority_sound
        bne     exit_sound_not_in_memory

        lda     #$00
        sta     tracked_lowest_priority_sound
        sta     lowest_priority_sound_starting_flag
        sta     vmux_primary_active_flag_bff
        sta     vmux_secondary_active_flag_bff

exit_sound_not_in_memory:
        lda     #$01                       // Return #$01 → sound resource is not in memory
        rts

resource_loaded:
        // Copy the current sound resource pointer into tmp_voice_sound_base_lo (temporary 16-bit pointer)
        sta     tmp_voice_sound_base_hi
        lda     sound_ptr_lo_tbl,y
        sta     tmp_voice_sound_base_lo

        // Check whether cached base address matches the current resource base (relocation test)
        cmp     voice_base_addr_lo,x
        bne     repair_after_relocation

        lda     tmp_voice_sound_base_hi
        cmp     voice_base_addr_hi,x
        bne     repair_after_relocation

        lda     #$00                       // Return #$00 → no relocation, pointers already correct
        rts

repair_after_relocation:
        // Relocation detected → update cached base and recompute instruction pointer
        lda     tmp_voice_sound_base_hi
        sta     voice_base_addr_hi,x

        lda     tmp_voice_sound_base_lo
        sta     voice_base_addr_lo,x

        // Recalculate voice_instruction_ptr := tmp_voice_sound_base + voice_instruction_offset
        clc
        adc     voice_instr_loop_ofs_lo,x
        sta     voice_instr_pc_lo,x

        lda     tmp_voice_sound_base_hi
        adc     voice_instr_loop_ofs_hi,x
        sta     voice_instr_pc_hi,x

        lda     #$ff                       // Return #$FF → relocation occurred, pointers adjusted
        rts

/*
;===========================================
; Starts a sound
;
;   Arguments:  .A  sound index
;
;   Returns:    #01 if sound is not in memory
;               #FE if all voices are in used by music (which has higher priority)
;               #FF if sound can't be started because there are not enough voices available
;
; This subroutine performs the following logic:
;
;   - validates that the sound resource is effectively in memory
;   - determines how many voices are needed to play the sound
;   - checks if there are enough voices available
;   - if there aren't enough, checks if there are lower-priority sounds
;     whose voices can be evicted to make room
;   - if that's the case, it stops those sounds to evict voices and make room
;   - allocates voices needed
;   - sets up base and offset read pointers for each of those voices
;===========================================
*/
* = $4D0A
start_sound:
        sta     pending_sound_idx
        pha
        txa
        pha
        tya
        pha

        // Disable sound processing while we load and reconfigure sound data
        lda     #BTRUE
        sta     sound_processing_disabled_flag

        lda     music_playback_in_progress
        beq     check_sound_resource_loaded

        // Music active: if it uses all three real voices (0–2), we cannot start a new real-voice sound
        lda     music_voices_in_use
        and     #$07
        cmp     #$07
        bne     check_sound_resource_loaded

        // All 3 real voices are in use by music → return #$FE
        lda     #START_SOUND_ERR_MUSIC_ALL_VOICES
        jmp     start_sound_exit

// Check if the sound resource is actually loaded in memory - if not, exit
check_sound_resource_loaded:
        ldy     pending_sound_idx
        lda     sound_ptr_hi_tbl,y
        bne     init_sound_read_pointer

        // Sound not in memory - return #$01
        lda     #START_SOUND_ERR_NOT_LOADED
        jmp     start_sound_exit

// Set the resource memory address as the sound data base address
init_sound_read_pointer:
        sta     sound_data_base + 1
        lda     sound_ptr_lo_tbl,y
        sta     sound_data_base

        // Set offset to #$04 (to skip the resource header)
        ldy     #MEM_HDR_LEN

        // Read the sound priority from byte +4
        lda     (sound_data_base),y
        sta     sound_priority
        cmp     #MIN_SOUND_PRIORITY
        bne     compute_real_voice_requirements

        // sound_priority == MIN_SOUND_PRIORITY → record bookkeeping flags for priority-1 sound
        lda     pending_sound_idx
        sta     tracked_lowest_priority_sound
        lda     #TRUE
        sta     lowest_priority_sound_starting_flag

// Compute real voice requirements
//
// This section counts how many real voices (0–2) are required.
// The result is stored in real_voices_needed.
//
// If bit 6 is set, the count will be #$00.
// If bit 6 is clear, the count will be 1 + value of bit 3 + value of bit 1.
compute_real_voice_requirements:
        // Read voice requirements bitmask from the next byte (offset +5)
        iny
        lda     (sound_data_base),y

        // Save voice requirements bitmask in Y
        tay

        // Count contributing bits into X
        ldx     #$00

        // Test bit 6; if set, X remains 0
        and     #$40
        bne     store_real_voice_count

        // Bit 6 clear → base requirement: 1 real voice
        ldx     #$01
        tya

        // Test bit 1: rotate right twice, then check carry
        ror     
        ror     
        bcc     ss_test_bit3_for_count

        // Bit 1 set → add one more real voice
        inx

ss_test_bit3_for_count:
        // Test bit 3: rotate right twice from the bit-1 position, then check carry
        ror     
        ror     
        bcc     store_real_voice_count

        // Bit 3 set → add one more real voice
        inx

store_real_voice_count:
        // Store real-voice count
        stx     real_voices_needed

        // Restore the original bitmask into A
        tya

        // Test bit 5 to see if special voice #3 is requested; store raw mask into voice3_requested_flag
        and     #$20
        sta     voice3_requested_flag

        // Determine if enough real voices are available right now:
        // insufficient_voices_flag_b := 0 if enough, $ff if not
        ldy     #FALSE
        lda     total_real_voices_available
        cmp     real_voices_needed
        bpl     store_real_voice_availability_flag
        ldy     #BTRUE

store_real_voice_availability_flag:
        sty     insufficient_voices_flag_b

        // Check if there is a “voice 3 conflict”:
        // conflict if (voice3_in_use != 0) AND (voice3_requested_flag != 0)
        ldy     #FALSE
        lda     voice3_in_use
        beq     store_voice3_conflict_flag_b_flag
        lda     voice3_requested_flag
        beq     store_voice3_conflict_flag_b_flag
        ldy     #BTRUE

store_voice3_conflict_flag_b_flag:
        sty     voice3_conflict_flag_b

        // If there is a voice 3 conflict, or if there are not enough real voices,
        // we must consider eviction. Otherwise we can go straight to allocation.
        bne     verify_voices_attainable_or_fail

        lda     insufficient_voices_flag_b
        bne     verify_voices_attainable_or_fail

        // The voices needed are all available right now → go straight to allocation
        jmp     allocate_and_configure_needed_voices

// There are not enough voices available right now, or there is a voice 3 conflict.
// See whether we could free enough voices by evicting lower-priority sounds.
verify_voices_attainable_or_fail:
        jsr     count_evictable_voices

        // Check total_real_voices_available + evictable_voice_count >= real_voices_needed
        clc
        lda     total_real_voices_available
        adc     evictable_voice_count
        cmp     real_voices_needed
        bpl     enforce_voice3_priority_constraint

        // There are not enough real voices even with eviction → return error #$FF
        lda     #START_SOUND_ERR_INSUFFICIENT_VOICES
        jmp     start_sound_exit

enforce_voice3_priority_constraint:
        // If the new sound needs voice 3, and voice 3 is in use, consult
        // voice3_priority_is_higher_flag_b to see whether eviction is allowed.
        lda     voice3_requested_flag
        beq     dispatch_voice3_conflict_flag_b_resolution

        lda     voice3_in_use
        beq     dispatch_voice3_conflict_flag_b_resolution

        lda     voice3_priority_is_higher_flag_b
        bne     dispatch_voice3_conflict_flag_b_resolution

        // Voice 3 needed, but in-use priority blocks eviction → error #$FF
        lda     #START_SOUND_ERR_INSUFFICIENT_VOICES
        jmp     start_sound_exit

dispatch_voice3_conflict_flag_b_resolution:
        // If there is no voice 3 conflict, fall back to generic “find sound to stop”.
        lda     voice3_conflict_flag_b
        beq     evict_lowest_priority_sounds_loop

        // There is a conflict: compare new sound priority to voice 3 priority
        lda     sound_priority
        cmp     voice_priority_3
        bmi     return_error_insufficient_priority

        // New sound has higher priority → evict the sound currently using voice 3
        lda     rsrc_for_voice_3
        jmp     stop_sound_by_resource_id

        // Unreachable after the direct jump to stop_sound_by_resource_id
        jmp    $4de1

return_error_insufficient_priority:
        lda     #START_SOUND_ERR_INSUFFICIENT_VOICES
        jmp     start_sound_exit

// Voices needed can be attained by evicting voices in use.
// Find which sounds to stop to free enough voices, respecting priorities.
evict_lowest_priority_sounds_loop:
        jsr     count_evictable_voices

        // Sanity check: the lowest_alloc_voice_priority among evictable voices must be < sound_priority
        lda     lowest_alloc_voice_priority
        cmp     sound_priority
        bpl     return_error_no_lower_priority_voice

        // Stop sound owning the lowest-priority evictable voice
        ldx     lowest_alloc_voice_idx
        lda     voice_sound_id_tbl,x

stop_sound_by_resource_id:
        jsr     stop_sound_voices_only

        // Check if enough real voices are free now
        lda     total_real_voices_available
        cmp     real_voices_needed
        bpl     have_enough_real_voices

        // Not enough yet → loop and stop another lowest-priority sound
        jmp     evict_lowest_priority_sounds_loop

// There are enough real voices now; if voice 3 is required, verify it is free.
have_enough_real_voices:
        lda     voice3_requested_flag
        beq     branch_to_voice_allocation

        lda     voice3_in_use
        beq     branch_to_voice_allocation

        // Voice 3 needed and still in use → continue stopping sounds
        jmp     evict_lowest_priority_sounds_loop

branch_to_voice_allocation:
        jmp     allocate_and_configure_needed_voices

return_error_no_lower_priority_voice:
        lda     #START_SOUND_ERR_INSUFFICIENT_VOICES
        jmp     start_sound_exit

// This section allocates and sets up all voices needed for the sound.
// Real voices (0–2) are allocated so no other sound can use them concurrently.
// Virtual voices (4–7) are used for multiplexings.
// Special voice 3 controls filter and volume.
allocate_and_configure_needed_voices:
        // Offset +5 encodes which voices are needed (voice_requirements_mask bitmask)
        ldy     #OFS_VOICE_REQS
        lda     (sound_data_base),y
        sta     voice_requirements_mask

        // Test bit 6 (one of the ways to request voice 3)
        and     #$40
        beq     crvr_bit6_clear

        // Bit 6 set → allocate special voice #3
        jsr     allocate_special_voice_3
        jmp     save_primary_voice_index

crvr_bit6_clear:
        // Bit 6 clear → allocate one real voice (base voice)
        jsr     allocate_available_real_voice

// Save the index of the first allocated voice; its data will be set up last
save_primary_voice_index:
        stx     first_voice_index

        // Reload voice_requirements_mask bitmask for per-bit tests and advance Y past it
        lda     voice_requirements_mask
        iny

        ror     
        bcc     ss_test_bit1

        // Bit 0 set → set up virtual voice X+4
        inx
        inx
        inx
        inx
        jsr     set_voice_data_base_and_offset

ss_test_bit1:
        ror     
        bcc     ss_test_bit2

        // Bit 1 set → allocate an additional real voice and set its data
        jsr     allocate_available_real_voice
        jsr     set_voice_data_base_and_offset

ss_test_bit2:
        ror     
        bcc     ss_test_bit3

        // Bit 2 set → set up another virtual voice X+4
        inx
        inx
        inx
        inx
        jsr     set_voice_data_base_and_offset

ss_test_bit3:
        ror     
        bcc     ss_test_bit4

        // Bit 3 set → allocate another real voice and set its data
        jsr     allocate_available_real_voice
        jsr     set_voice_data_base_and_offset

ss_test_bit4:
        ror     
        bcc     ss_test_bit5

        // Bit 4 set → set up another virtual voice X+4
        inx
        inx
        inx
        inx
        jsr     set_voice_data_base_and_offset

ss_test_bit5:
        ror     
        bcc     finalize_primary_voice_base_and_offset

        // Bit 5 set → allocate special voice #3 and set its data
        jsr     allocate_special_voice_3
        jsr     set_voice_data_base_and_offset

// Set up the first voice we allocated (back when we tested bit 6).
// At this point Y points to the start of that voice's data.
finalize_primary_voice_base_and_offset:
        // Recover first allocated voice index into X
        ldx     first_voice_index

        // Set base address for this voice’s data
        lda     sound_data_base
        sta     voice_data_base_lo,x
        lda     sound_data_base + 1
        sta     voice_data_base_hi,x

        // Set offset: low := Y (current read offset), high := 0
        tya
        sta     voice_data_offsets_lo,x
        lda     #$00
        sta     voice_data_offsets_hi,x

        // Associate the sound ID with this voice
        lda     pending_sound_idx
        sta     sound_id_to_start_on_voice,x

        // Prepare return value: on success, A := pending_sound_idx
        lda     pending_sound_idx

start_sound_exit:
        sta     start_sound_result

        // Re-enable sound processing
        lda     #FALSE
        sta     sound_processing_disabled_flag

        // Restore Y, X, A
        pla
        tay
        pla
        tax
        pla

        lda     start_sound_result
        rts

* = $4E99
set_voice_data_base_and_offset:
        // ----------------------------------------------------------------------
        // For logical voice X:
        //
        //   • Capture the current sound_data_base as this voice’s data base.
        //   • Read a 16-bit little-endian offset from the sound data stream
        //     at (sound_data_base + Y), storing it into the per-voice
        //     voice_data_offsets table.
        //   • Increment Y twice (caller continues reading after this field).
        //   • Record pending_sound_idx as the sound-ID associated with this voice.
        //
        // Register behavior:
        //   • A is preserved (saved/restored with PHA/PLA).
        //   • X is preserved.
        //   • Y exits incremented by 2.
        // ----------------------------------------------------------------------
        pha                                     // Preserve A for caller

        lda     sound_data_base                 // Base pointer → per-voice storage
        sta     voice_data_base_lo,x
        lda     sound_data_base + 1
        sta     voice_data_base_hi,x

        lda     (sound_data_base),y              // Read offset low byte
        sta     voice_data_offsets_lo,x
        iny

        lda     (sound_data_base),y              // Read offset high byte
        sta     voice_data_offsets_hi,x
        iny                                     // Y advanced by 2 total

        lda     pending_sound_idx                  // Bind voice X to current sound-ID
        sta     sound_id_to_start_on_voice,x

        pla                                     // Restore caller’s A
        rts

* = $5031
stop_sound_core:
        // ----------------------------------------------------------------------
        // Stop playback of a sound whose index is passed in A.
        //
        // Observable behavior from code:
        //
        //   1. sound_to_stop := A
        //   2. Save A, X, Y
        //   3. Always call stop_all_voices_for_sound once
        //        – if sound_to_stop == tracked_lowest_priority_sound:
        //               stop_all_voices_for_sound is called before mode checks
        //        – if sound_to_stop != tracked_lowest_priority_sound:
        //               branch skips the deref/clear block but still calls stop_all_voices_for_sound
        //
        //   4. Only when BOTH conditions hold:
        //          • sound_to_stop == tracked_lowest_priority_sound
        //          • stop_sound_cleanup_mode == #$FF
        //      …the routine additionally performs:
        //          • dec_sound_refcount_if_no_music(sound_to_stop)
        //          • tracked_lowest_priority_sound = 0
        //          • lowest_priority_sound_starting_flag = 0
        //          • vmux_primary_active_flag_bff = 0
        //          • vmux_secondary_active_flag_bff = 0
        //          • clear_all_alternate_settings()
        //
        //   5. All registers A,X,Y are restored before return.
        //
        // NOTE: The older comments inverted the interpretation of stop_sound_cleanup_mode.
        //       The code shows clearly that ONLY stop_sound_cleanup_mode == $FF
        //       triggers deref + multiplexing clears, and ONLY when stopping the
        //       current tracked_lowest_priority_sound.
        // ----------------------------------------------------------------------

        sta     sound_to_stop                   // Set sound_to_stop := A

        pha                                     // Save A
        txa
        pha                                     // Save X
        tya
        pha                                     // Save Y

        lda     sound_to_stop
        cmp     tracked_lowest_priority_sound
        bne     ssp_nonstarting_sound

        // ------------------------------------------------------------
        // Sound matches the currently-starting sound.
        // Always stop voices for this sound first.
        // ------------------------------------------------------------
        jsr     stop_all_voices_for_sound

        lda     stop_sound_cleanup_mode
        cmp     #STOP_SOUND_MODE_FULL_CLEANUP
        bne     ssp_exit_2                    // If != STOP_SOUND_MODE_FULL_CLEANUP → done

        // ------------------------------------------------------------
        // stop_sound_cleanup_mode == STOP_SOUND_MODE_FULL_CLEANUP AND this is the starting sound.
        // Dereference + clear state.
        // ------------------------------------------------------------
        ldx     sound_to_stop
        ldy     #$00
        jsr     dec_sound_refcount_if_no_music

        lda     #$00
        sta     tracked_lowest_priority_sound
        sta     lowest_priority_sound_starting_flag
        sta     vmux_primary_active_flag_bff
        sta     vmux_secondary_active_flag_bff

        jsr     clear_all_alternate_settings
ssp_exit_2:		
        jmp     ssp_exit

ssp_nonstarting_sound:
        jsr     stop_all_voices_for_sound            // Always performed for mismatch

ssp_exit:
        pla
        tay                                     // Restore Y
        pla
        tax                                     // Restore X
        pla                                     
        rts                                     // Restore A and exit

* = $5070
stop_all_voices_for_sound:
        // ----------------------------------------------------------------------
        // Stop every logical voice slot whose resource ID matches sound_to_stop.
        //
        // Mechanics:
        //   - Scans X = 3,2,1,0.
        //   - For each X where voice_sound_id_tbl[X] == sound_to_stop:
        //         • A := X
        //         • jsr stop_logical_voice
        //     (stop_logical_voice restores A,X,Y for its caller, so X must be saved.)
        //
        //   - The loop continues after each match; stopping *all* voices mapped
        //     to this sound index, not just one.
        //
        // Registers:
        //   - A,X,Y are not preserved by this routine; caller should not expect
        //     meaningful return values.
        // ----------------------------------------------------------------------
        ldx     #$03                            // Begin scanning at highest slot

svfs_scan_slot:
        lda     sound_to_stop                   // Compare target sound
        cmp     voice_sound_id_tbl,x
        bne     svfs_prev_slot                      // Skip if this slot not mapped to sound

        stx     saved_voice_slot_index                          // Save loop X
        txa                                     // A := logical voice index
        jsr     stop_logical_voice                      // Stop this individual voice
        ldx     saved_voice_slot_index                          // Restore loop X

svfs_prev_slot:
        dex                                     // Previous voice slot
        bpl     svfs_scan_slot                     // Continue until X < 0

        rts
		
		
* = $5088
stop_sound_voices_only:
        // ----------------------------------------------------------------------
        // Wrapper that stops the sound whose index is in A, using
        // stop_sound_cleanup_mode = #$01.
        //
        // IMPORTANT (actual behavior from code):
        //
        //   stop_sound_cleanup_mode = #$01  **does NOT** cause dereference,
        //   does NOT clear multiplexings, and does NOT clear alternate settings.
        //
        //   Those actions only occur when stop_sound_cleanup_mode == #$FF and the sound
        //   being stopped matches tracked_lowest_priority_sound.
        //
        //   Therefore, this routine performs:
        //        • stop_all_voices_for_sound(sound)
        //        • NO refcount decrement
        //        • NO multiplexing shutdown
        //        • NO clearing of tracked_lowest_priority_sound / priority1 flags
        //        • NO alternate-setting reset
        //
        //   A, X, Y are preserved for the caller.
        // ----------------------------------------------------------------------
        pha                                     // Save A (sound index)

        lda     #STOP_SOUND_MODE_VOICES_ONLY
        sta     stop_sound_cleanup_mode                 // Mode #01 = "simple stop"

        pla                                     // Restore A (sound index)
        jsr     stop_sound_core             // Perform stop logic

        rts

* = $5093
stop_sound:
        // ----------------------------------------------------------------------
        // Remove all occurrences of a given sound-ID (A) from the
        // sound_id_to_start_on_voice[] table.
        //
        // Behavior (actual code logic):
        //
        //   • X iterates from 6 down to 0.
        //   • For each slot X:
        //         if sound_id_to_start_on_voice[X] == A:
        //               sound_id_to_start_on_voice[X] := $FF
        //
        //   • ALL matching entries are cleared, not just the first.
        //   • A is preserved across the routine.
        //   • X ends at $FF, Y unchanged.
        //
        // NOTE: This routine does NOT stop audio, does NOT halt voices,
        //       does NOT change refcounts, and does NOT affect multiplexings.
        //       It only clears pending “start sound on voice” assignments.
        // ----------------------------------------------------------------------
        ldx     #$06                            // Scan slots 6 → 0

stop_sound_scan_slot:
        cmp     sound_id_to_start_on_voice,x    // Compare slot’s pending sound ID
        bne     stop_sound_step_prev_slot       // Not a match → skip

        pha                                     // Save A (sound ID)
        lda     #SOUND_IDX_NONE                 // $FF = "no pending sound"
        sta     sound_id_to_start_on_voice,x    // Clear this pending assignment
        pla                                     // Restore A

stop_sound_step_prev_slot:
        dex                                     // Next lower slot
        bpl     stop_sound_scan_slot            // Continue while X >= 0

        // Fall through to stop_sound_full_cleanup
		
* = $50A4
stop_sound_full_cleanup:
        // ----------------------------------------------------------------------
        // Stop a sound using stop_sound_cleanup_mode = $FF.
        //
        // Actual behavior (from code + stop_sound_core analysis):
        //
        //   • stop_sound_cleanup_mode := $FF
        //   • Call stop_sound_core(A = sound index)
        //
        // In stop_sound_core:
        //   If (sound_to_stop == tracked_lowest_priority_sound) AND (stop_sound_cleanup_mode == $FF):
        //        → decrement refcount of that sound
        //        → clear tracked_lowest_priority_sound
        //        → clear lowest_priority_sound_starting_flag
        //        → clear vmux_primary_active_flag_bff and vmux_secondary_active_flag_bff
        //        → clear_all_alternate_settings
        //
        //   Regardless:
        //        → stop_all_voices_for_sound is always executed once
        //
        // Registers A, X, Y are fully preserved for the caller.
        // ----------------------------------------------------------------------
        pha                                     // Save sound index

        lda     #STOP_SOUND_MODE_FULL_CLEANUP
        sta     stop_sound_cleanup_mode                 // Mode $FF: allows deref & multiplexing clear

        pla                                     // Restore A (sound index)
        jsr     stop_sound_core             // Perform full stop logic (if conditions match)

        rts

* = $50C7
stop_logical_voice:
        // ----------------------------------------------------------------------
        // Stop a logical voice/slot whose index is passed in A.
        //
        // Behavior summary (exact from code):
        //
        //   1. A is saved; X := A.
        //   2. set_voice_to_unused(X) is always called first.
        //
        //   3. If X ≥ 4:
        //         → no further action; return.
        //
        //   4. If X == 3:
        //         → release_voice(3), then return.
        //
        //   5. If X is 0–2 (real voices):
        //         → snapshot sid_filter_control_shadow
        //         → clear_waveform_on_full_stop(X)
        //         → release_voice(X)
        //         → if filter control changed and its low 3 bits are now zero:
        //               • if voice3_in_use ≠ 0:
        //                     release_voice(3)
        //                     set_voice_to_unused(3)
        //         → convert real voice index to its paired “virtual” slot (X += 4)
        //         → set_voice_to_unused(X) on that secondary slot
        //
        //   6. Restore A and return.
        //
        // Registers on return:
        //      A restored to original logical index
        //      X =
        //         - original A     if A ≥ 4
        //         - 3              if A = 3
        //         - A + 4          if A = 0–2
        //      Y untouched
        // ----------------------------------------------------------------------
        pha                                     // Save logical index
        tax                                     // X := logical index

        jsr     set_voice_to_unused             // Always clear per-slot execution state

        cpx     #$04                            // X ≥ 4 → no further action
        bpl     sv_exit

        cpx     #$03                            // X == 3 → skip waveform logic
        bpl     sv_release_primary_slot

        // ------------------------------------------------------------------
        // Real voices 0–2: snapshot filter state and try to clear gate/waveform
        // ------------------------------------------------------------------
        lda     sid_filter_control_shadow
        sta     filter_control_snapshot

        jsr     clear_waveform_on_full_stop    // Conditional gate/waveform clear

sv_release_primary_slot:
        jsr     release_voice                   // Release primary voice (0–3)

        cpx     #$03                            // Voices 3+ skip the rest
        bpl     sv_exit

        // ------------------------------------------------------------------
        // Additional logic for real voices 0–2:
        // Decide whether to release voice 3 based on filter state change.
        // ------------------------------------------------------------------
        lda     sid_filter_control_shadow
        cmp     filter_control_snapshot
        beq     sv_release_paired_virtual_slot            // unchanged → skip

        and     #$07                            // check low 3 bits (filter routing mask)
        bne     sv_release_paired_virtual_slot            // non-zero → skip

        // filter changed and low bits now zero → maybe release voice 3
        txa
        pha                                      // save original real voice index (0–2)

        lda     voice3_in_use
        beq     sv_after_voice3_release

        ldx     #$03
        jsr     release_voice
        jsr     set_voice_to_unused

sv_after_voice3_release:
        pla
        tax                                      // restore real voice index (0–2)

sv_release_paired_virtual_slot:
        inx
        inx
        inx
        inx                                      // X := X + 4 (virtual slot 4–6)
        jsr     set_voice_to_unused             // Clear the paired virtual slot

sv_exit:
        pla                                      // restore original A (logical index)
        rts

* = $510A
clear_waveform_on_full_stop:
        // ----------------------------------------------------------------------
        // Conditionally clear waveform bits + gate bit in voice_ctrl_shadow[X].
        //
        // Behavior:
        //   - Always loads and saves the current control byte for this voice.
        //   - If music_playback_in_progress ≠ 0 → no changes are applied.
        //   - If stop_sound_cleanup_mode ≠ $FF → no changes are applied.
        //   - Otherwise (no music playing AND stop_sound_cleanup_mode == $FF):
        //         voice_ctrl_shadow[X] := (original_control & $0E)
        //         apply_voice_control_to_sid is called to commit the change.
        //
        // On return:
        //   - A is restored to the original control byte.
        //   - X and Y are preserved.
        // ----------------------------------------------------------------------
        lda     voice_ctrl_shadow,x                // Fetch original control byte
        pha                                     // Save original control on stack
        tay                                     // Copy to Y for later restore

        lda     music_playback_in_progress      // Skip modification if music is active
        bne     cwast_exit

        lda     stop_sound_cleanup_mode                 // Skip unless stop mode is $FF
        cmp     #STOP_SOUND_MODE_FULL_CLEANUP
        bne     cwast_exit

        // ------------------------------------------------------------
        // Allowed path: clear gate + waveform bits
        // Keep only bits 1–3 of the original control.
        // ------------------------------------------------------------
        tya                                     // Restore original control into A
        and     #$0E                            // Mask: clear bit0 and bits4–7
        sta     voice_ctrl_shadow,x                // Store new control byte
        jsr     apply_voice_control_to_sid            // Commit update

cwast_exit:
        pla                                     // Restore original control into A
        rts


* = $5126
set_voice_to_unused:
        // ------------------------------------------------------------
        // Mark logical voice/slot X as unused:
        // - Clear its instruction repeat counter.
        // - Clear its bit in voice_instr_active_mask.
        // - For slots X ≥ 4, also clear the associated resource ID.
        // ------------------------------------------------------------
        lda     #$00
        sta     voice_instr_repcount,x    // Reset repeat counter for this slot

        lda     voice_instr_active_mask    // Clear "executing" bit for this slot
        and     voice_alloc_clear_mask_tbl,x
        sta     voice_instr_active_mask

        cpx     #$04                            // Slots 0–3: no resource entry to clear
        bmi     svtu_exit                            // X < 4 → done

        lda     #$00                            // X ≥ 4 → clear resource binding
        sta     voice_sound_id_tbl,x

svtu_exit:
        rts


