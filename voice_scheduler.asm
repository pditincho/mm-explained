/*================================================================================
  Voice Scheduler
================================================================================

This file implements the sound engine’s “voice scheduling layer.”  
It sits between two worlds:

    (1) Sounds — abstract resources with priorities and per-voice data streams
    (2) Voices — actual execution slots (real SID voices, virtual voices, and
        the special filter control slot)

Its job is to decide:  
        “Can this sound start right now? If yes, on which voices?”  
and later:  
        “How do we safely stop this sound and release its voices?”

The whole system centers around these main ideas:
--------------------------------------------------------------------------------
1.  Sounds specify what voices they need

    Each sound resource contains:
       • a priority value  
       • a “voice requirements” bitmask  
       • a list of 16-bit offsets: one per requested voice

    From the bitmask the engine derives:
       • how many real SID voices (0–2) the sound needs  
       • whether it requires the filter voice  
       • how many virtual voices (4–7) it should receive

    All this information determines the final configuration of per-voice
    base pointers and offsets into the sound’s data tables.

--------------------------------------------------------------------------------
2.  Voices are limited and may need to evict lower-priority sounds

    When a sound wants to start, the engine checks:
       • Are the requested real voices already free?
       • If not, can they be freed by stopping other sounds?
       • Are those other sounds lower priority than the new one?
       • Is the filter voice available, or can it be taken based on priority?

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
3.  Starting a sound = allocate voices + bind per-voice data streams

    Once enough voices are free, the engine proceeds:
       1) It allocates the first voice (real or special)  
       2) It walks through the bitmask again, and for each set bit:
            • allocates a real or special voice, or  
            • assigns a virtual voice (logical slot only)  
            • reads the next pair of bytes (16-bit offset) from the sound  
              resource  
            • stores the sound’s base pointer + offset into the voice tables

    End result: each logical voice slot X is pointed at the correct data inside
    the sound resource, and “snd_to_start_on_voice[X]” identifies which
    sound owns that slot. The sound is now active.

--------------------------------------------------------------------------------
4.  Stopping a sound safely releases its voices

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
       • may release the filter voice if the filter is no longer used
       • optionally clears SID gate/waveform bits (only if safe to do so)

--------------------------------------------------------------------------------
5.  Safety mechanisms prevent inconsistent state

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
#import "sound_constants.inc"
#import "sid_voice_controller.asm"
#import "voice_allocation.asm"
#import "sound_engine.asm"

.label sound_data_base_lo            = $BC     // Zero-page pointer: base address of the current sound resource data
.label sound_data_base_hi            = $BD     

.label first_voice_index          = $4CF3   // Index of the first allocated voice slot (real or special) for this sound
.label filter_voice_needed   	  = $4CF4   // Non-zero when the sound’s requirements mask requests voice 3 via bit 5
.label filter_conflict_flag_b     = $4CF6   // Non-zero if voice 3 is requested and already in use by another sound
.label real_voices_needed         = $4CFE   // Count of real SID voices (0–2) required by the pending sound
.label insufficient_voices_flag_b = $4D00   // 0 if current real voices suffice; $ff if more real voices are needed
.label voice_requirements_mask    = $4D02   // Bitfield describing which real, virtual, and voice-3 slots this sound uses
.label start_sound_result         = $4D04   // Holds start_sound’s final return code (A) across register restore
.label saved_voice_slot_index     = $4D09   // Saved logical voice index while iterating/temporarily stopping voices
.label tmp_voice_sound_base_lo = $4815 //lo
.label tmp_voice_sound_base_hi = $4814 // hi


/*
================================================================================
  update_voice_stream_pointers
================================================================================
Summary
  Validate and maintain the stream state for a single logical voice: ensure the
  owning sound resource is resident, stop and clean up if it is not, and repair
  the cached base address and instruction PC if the resource has been relocated.

Arguments
  X                       Logical voice index whose stream pointers are checked

Global Inputs
  voice_sound_id_tbl      		Map: logical voice index → owning sound index
  sound_ptr_lo/hi_tbl     		Current low-byte base pointers for sound resources
  voice_base_addr_lo/hi   		Cached low-byte base pointers per voice
  voice_instr_loop_ofs_lo/hi	Per-voice loop-entry instruction offset
  pri1_snd_idx 					Sound index with priority 1

Global Outputs
  voice_base_addr_lo      Updated cached base low byte when relocation detected
  voice_base_addr_hi      Updated cached base high byte when relocation detected
  voice_instr_pc_lo       Recomputed instruction PC low byte on relocation
  voice_instr_pc_hi       Recomputed instruction PC high byte on relocation
  pri1_snd_idx            Cleared when the stopped sound was a priority-1 sound
  pri1_snd_starting_flag  Set to FALSE when the stopped sound was a priority-1 sound
  vmux_primary_active_flag_bff		Cleared when the stopped sound was a priority-1 sound
  vmux_secondary_active_flag_bff	Cleared when the stopped sound was a priority-1 sound

Returns
  A                       
		UPD_VCE_RSLT_NOT_IN_MEMORY 		if the owning sound resource is not resident and has been fully stopped
        UPD_VCE_RSLT_NO_RELOCATION 		if the resource is resident and the cached base matches its current address
        UPD_VCE_RSLT_RELOCATED 			if the resource is resident but moved and the voice PC was rebuilt

Description
  • If the owning sound resource is not resident, perform a full stop for that
    sound and, if it's the priority-1 sound starter, clear the
    associated priority and multiplexing flags before returning “not in
    memory”.
  • If the resource is resident and the cached base matches the current
    pointer, return “no relocation” without touching the cached base or the
    instruction PC.
  • If the resource is resident but the base address has changed, update the
    cached base and rebuild the voice instruction PC as base + loop offset,
    then return “relocated” so callers can distinguish the repair case.
================================================================================
*/
* = $4C1C
update_voice_stream_pointers:
        // Sound resource for this voice resident? If so, continue
        ldy     voice_sound_id_tbl,x
        lda     sound_ptr_hi_tbl,y
        bne     resource_loaded

        // Sound resource not in memory → stop sound 
        tya
        jsr     stop_sound_full_cleanup
		
		// Is it the priority-1 sound? If not, exit
        cmp     pri1_snd_idx
        bne     exit_sound_not_in_memory

		// It's the lowest priority sound, clear relevant flags
        lda     #$00
        sta     pri1_snd_idx
        sta     pri1_snd_starting_flag
        sta     vmux_primary_active_flag_bff
        sta     vmux_secondary_active_flag_bff

exit_sound_not_in_memory:
        lda     #UPD_VCE_RSLT_NOT_IN_MEMORY                       
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

		// Address matches fully, no relocation
        lda     #UPD_VCE_RSLT_NO_RELOCATION                       
        rts

repair_after_relocation:
        // Address change, relocation detected 
		// Update cached base for this voice 
        lda     tmp_voice_sound_base_hi
        sta     voice_base_addr_hi,x

        lda     tmp_voice_sound_base_lo
        sta     voice_base_addr_lo,x

        // Recalculate voice instruction PC := tmp_voice_sound_base + voice_instruction_offset
        clc
        adc     voice_instr_loop_ofs_lo,x
        sta     voice_instr_pc_lo,x

        lda     tmp_voice_sound_base_hi
        adc     voice_instr_loop_ofs_hi,x
        sta     voice_instr_pc_hi,x

		// Relocation occurred, pointers adjusted
        lda     #UPD_VCE_RSLT_RELOCATED                       
        rts
/*
================================================================================
  start_sound
================================================================================
Summary
	Attempt to start playing a sound effect identified in A. 
	
	Enforces music/voice protection rules, validates resource residency, determines 
	real/filter/virtual	voice requirements, and assigns voices according to priority. 
	
	If necessary, evicts lower-priority sounds to free required voices. 
	
	On success, binds one or more logical voices to the sound’s data stream. 
	On failure, returns a specific START_SOUND_ERR_* code.

Arguments
	A    							Sound index to start.

Returns
	A    							On success: the same sound index.
									On failure: one of the START_SOUND_ERR_* codes.

Global Inputs
	music_playback_in_progress       Whether music is active.
	music_voices_in_use              Bitmask of real voices consumed by music.
	sound_ptr_{lo,hi}_tbl[]          Pointers to sound resources.
	total_real_voices_available      Count of free real voices (0–3).
	filter_voice_in_use_flag         Nonzero if filter voice is allocated.
	filter_priority                  Priority of sound currently owning filter.
	filter_priority_is_higher_flag_b Comparison of new sound priority vs filter.
	snd_for_filter                   Sound index currently owning filter.

Global Outputs
	pending_sound_idx                Sound being started.
	sound_processing_disabled_flag   Temporarily disables updates during setup.
	sound_priority                   Priority read from resource.
	filter_conflict_flag_b           TRUE if filter is needed and already in use.
	pri1_snd_idx                     Active priority-1 sound ID.
	pri1_snd_starting_flag           Set TRUE if starting a priority-1 sound.
	snd_to_start_on_voice[]          Per-voice sound bindings.
	voice_data_base_{lo,hi}[]        Per-voice base pointers for sound data.
	voice_data_offsets_{lo,hi}[]     Per-voice data offsets.

Description
	• Verifies that music playback does not prohibit starting a new sound
	(all three real voices reserved).
	• Validates that the sound resource is loaded; otherwise returns
	START_SOUND_ERR_NOT_LOADED.
	• Reads the sound’s priority and sets priority-1 bookkeeping.
	• Parses the voice-requirements bitmask from the sound data, computing:
		– real_voices_needed (bits 6/3/1),
		– filter_voice_needed (bit 5).
	• Determines whether current real-voice and filter-voice availability
	satisfies requirements. If not, checks whether eviction could free enough.
	• Enforces filter-priority rules: a sound may evict the filter only if its
	priority is strictly higher than the current filter sound.
	• Iteratively evicts the lowest-priority evictable sounds until:
		– sufficient real voices exist, and
		– filter is free if required.
	• Allocates required voices (real, virtual, and/or filter) per bitmask and
	configures their data streams by advancing through the resource.
	• Finalizes the primary voice’s base address and offset from the current Y.
	• Restores registers, re-enables processing, and returns status in A.

Notes
	• Voice allocation and stream configuration tightly couple real voices
	(0–2), virtual voices (4–7), and the filter logical voice (3).
	• Eviction logic is priority-driven and guarantees that no higher-priority
	sound is displaced by a lower-priority request.
	• Success returns the sound ID itself; all error exits unify through
	start_sound_result.
================================================================================
*/
* = $4D0A
start_sound:
		// Save index of pending sound
        sta     pending_sound_idx
		
		// Save registers
        pha
        txa
        pha
        tya
        pha

        // Disable sound processing while we load and reconfigure sound data
        lda     #BTRUE
        sta     sound_processing_disabled_flag

		// Is music in progress? If not, skip the next part
        lda     music_playback_in_progress
        beq     verify_sound_resource_resident

        // ------------------------------------------------------------
        // Music active
        // ------------------------------------------------------------
		// Is it using all real voices?
        lda     music_voices_in_use
        and     #$07
        cmp     #$07
        bne     verify_sound_resource_resident

        // ------------------------------------------------------------
        // All 3 real voices are in use by music; reject starting this sound
        // ------------------------------------------------------------
        lda     #START_SOUND_ERR_MUSIC_ALL_VOICES
        jmp     start_sound_finish_with_status

verify_sound_resource_resident:
		// Check if the sound resource is actually loaded in memory - if not, exit
        ldy     pending_sound_idx
        lda     sound_ptr_hi_tbl,y
        bne     init_sound_resource_stream_base

        // ------------------------------------------------------------
        // Sound not in memory
        // ------------------------------------------------------------
        lda     #START_SOUND_ERR_NOT_LOADED
        jmp     start_sound_finish_with_status

init_sound_resource_stream_base:
		// Cache the sound base address in local vars
        sta     sound_data_base_hi
        lda     sound_ptr_lo_tbl,y
        sta     sound_data_base_lo

        // Set a memory offset to skip the resource header
        ldy     #MEM_HDR_LEN

        // Read the sound priority
        lda     (sound_data_base_lo),y
        sta     sound_priority
		
		// Is it a priority-1 sound?
        cmp     #MIN_SOUND_PRIORITY
        bne     compute_real_voice_requirements

        // It is, update priority-1 bookkeeping
        lda     pending_sound_idx
        sta     pri1_snd_idx
        lda     #TRUE
        sta     pri1_snd_starting_flag

compute_real_voice_requirements:
        // ------------------------------------------------------------
		// Compute real voice requirements
		//
		// This section counts how many real voices are required.
		// The result is stored in real_voices_needed.
		//
		// If bit 6 is set, the count will be #$00.
		// If bit 6 is clear, the count will be 1 + value of bit 3 + value of bit 1.
        // ------------------------------------------------------------
        // Read voice requirements bitmask from the next byte
        iny
        lda     (sound_data_base_lo),y

        // Save voice requirements bitmask in Y
        tay

        // Count contributing bits into X
        ldx     #$00

        // Test bit 6; if set, X remains 0
        and     #$40
        bne     finalize_real_voice_count

        // Bit 6 clear → base requirement: 1 real voice
        ldx     #$01
        tya

        // Test bit 1: rotate right twice, then check carry
        ror     
        ror     
        bcc     count_real_voices_test_bit3

        // Bit 1 set → add one more real voice
        inx

count_real_voices_test_bit3:
        // Test bit 3: rotate right twice from the bit-1 position, then check carry
        ror     
        ror     
        bcc     finalize_real_voice_count

        // Bit 3 set → add one more real voice
        inx

finalize_real_voice_count:
        // ------------------------------------------------------------
        // Store real-voice count (0-3)
        // ------------------------------------------------------------
        stx     real_voices_needed

        // Restore the original bitmask into A
        tya

        // ------------------------------------------------------------
		// Check if filter voice requested
        // ------------------------------------------------------------
        // Test bit 5 to see if the filter voice is requested; store raw mask into filter_voice_needed
        and     #$20
        sta     filter_voice_needed

        // ------------------------------------------------------------
		// Check if enough real voices are available
        // ------------------------------------------------------------
        ldy     #FALSE
        lda     total_real_voices_available
        cmp     real_voices_needed
        bpl     set_insufficient_voices_flag
        ldy     #BTRUE

set_insufficient_voices_flag:
        sty     insufficient_voices_flag_b

        // ------------------------------------------------------------
		// Check if there is a filter voice conflict
        // ------------------------------------------------------------
        ldy     #FALSE
		// Filter in use?
        lda     filter_voice_in_use_flag
        beq     set_filter_conflict_flag
		// And filter needed?
        lda     filter_voice_needed
        beq     set_filter_conflict_flag
		// In use and needed, we have a conflict
        ldy     #BTRUE

set_filter_conflict_flag:
        sty     filter_conflict_flag_b

        // ------------------------------------------------------------
		// Real voices and filter requirements met? If not, try eviction
        // ------------------------------------------------------------
        // Filter conflict?
        bne     verify_requirements_attainable_or_fail

		// Insufficient real voices?
        lda     insufficient_voices_flag_b
        bne     verify_requirements_attainable_or_fail

        // ------------------------------------------------------------
        // All voice/filter requirements are met
		// Go straight to allocation
        // ------------------------------------------------------------
        jmp     allocate_and_configure_required_voices

verify_requirements_attainable_or_fail:
        // ------------------------------------------------------------
		// Voice/filter requirements NOT met
		// See whether we could free enough voices by evicting lower-priority sounds.
        // ------------------------------------------------------------
		// Count how many real voices are evictable
        jsr     count_evictable_voices

		// If we evict those evictable, can we meet the voice requirements?
        clc
        lda     total_real_voices_available
        adc     evictable_voice_count
        cmp     real_voices_needed
        bpl     enforce_filter_eviction_priority

        // ------------------------------------------------------------
        // There are not enough real voices even with eviction
        // ------------------------------------------------------------
        lda     #START_SOUND_ERR_INSUFFICIENT_VOICES
        jmp     start_sound_finish_with_status

enforce_filter_eviction_priority:
        // If we have a filter conflict, check if
        // filter_priority_is_higher_flag_b to see whether eviction is allowed.
        lda     filter_voice_needed
        beq     resolve_filter_conflict_or_evict

        lda     filter_voice_in_use_flag
        beq     resolve_filter_conflict_or_evict

        lda     filter_priority_is_higher_flag_b
        bne     resolve_filter_conflict_or_evict

        // ------------------------------------------------------------
        // Filter needed, but sound priority blocks eviction
        // ------------------------------------------------------------
        lda     #START_SOUND_ERR_INSUFFICIENT_VOICES
        jmp     start_sound_finish_with_status

resolve_filter_conflict_or_evict:
        // If there is no filter conflict, skip this part
        lda     filter_conflict_flag_b
        beq     evict_lower_priority_sounds_loop

        // There is a filter conflict: compare new sound priority to filter priority
        lda     sound_priority
        cmp     filter_priority
        bmi     return_insufficient_filter_priority_error

        // New sound has higher priority → evict the sound currently using the filter
        lda     snd_for_filter
        jmp     stop_sound_by_index_voices_only

        // Unreachable code after the previous jmp - kept from the original
        jmp    $4de1

return_insufficient_filter_priority_error:
        // ------------------------------------------------------------
		// Filter needed but sound priority blocks eviction
        // ------------------------------------------------------------
        lda     #START_SOUND_ERR_INSUFFICIENT_VOICES
        jmp     start_sound_finish_with_status

evict_lower_priority_sounds_loop:
        // ------------------------------------------------------------
		// Voice requirements CAN be attained by evicting voices in use.
		//
		// Loop to evict all evictable voices until fulfilling requirements
        // ------------------------------------------------------------
		// Count how many real voices are evictable now
        jsr     count_evictable_voices

        // Sanity check: the lowest priority among all evictable voices must be < sound_priority
		// Otherwise, fail and exit
        lda     lowest_alloc_voice_priority
        cmp     sound_priority
        bpl     return_no_lower_priority_voice_error

        // Stop the sound owning the lowest-priority evictable voice
        ldx     lowest_alloc_voice_idx
        lda     voice_sound_id_tbl,x
stop_sound_by_index_voices_only:
        jsr     stop_sound_voices_only

        // Check if enough real voices are free now
        lda     total_real_voices_available
        cmp     real_voices_needed
        bpl     have_sufficient_real_voices

        // Not enough yet → loop and stop another lowest-priority sound
        jmp     evict_lower_priority_sounds_loop

have_sufficient_real_voices:
        // ------------------------------------------------------------
		// There are enough real voices now; if filter 3 is required, verify it is free.
        // ------------------------------------------------------------
		// Filter needed?
        lda     filter_voice_needed
        beq     branch_to_voice_allocation_and_setup

		// Filter in use?
        lda     filter_voice_in_use_flag
        beq     branch_to_voice_allocation_and_setup

        // Filter needed and still in use → continue stopping sounds
        jmp     evict_lower_priority_sounds_loop

branch_to_voice_allocation_and_setup:
		// No filter issues, skip to the allocation section
        jmp     allocate_and_configure_required_voices

return_no_lower_priority_voice_error:
        // ------------------------------------------------------------
		// Priority prevents eviction
        // ------------------------------------------------------------
        lda     #START_SOUND_ERR_INSUFFICIENT_VOICES
        jmp     start_sound_finish_with_status

allocate_and_configure_required_voices:
        // ------------------------------------------------------------
		// This section allocates and sets up all voices needed for the sound.
		//
		// Real voices (0–2) are allocated so no other sound can use them concurrently.
		// Virtual voices (4–7) are used for multiplexings.
		// Special voice 3 controls filter.
        // ------------------------------------------------------------
        // Set offset to the voice requirements byte
        ldy     #OFS_VOICE_REQS
		
		// Read requirements and copy it to a local bitmask
        lda     (sound_data_base_lo),y
        sta     voice_requirements_mask

        // ------------------------------------------------------------
        // Test bit 6 for filter or real voice
        // ------------------------------------------------------------
        and     #$40
        beq     crvr_bit6_clear

        // Bit 6 set → allocate filter
        jsr     allocate_special_voice_3
        jmp     save_primary_allocated_voice_index

crvr_bit6_clear:
        // Bit 6 clear → allocate one real voice (base voice)
        jsr     allocate_available_real_voice

save_primary_allocated_voice_index:
		// Save the index of the first allocated voice; its data will be set up last
        stx     first_voice_index

        // ------------------------------------------------------------
		// Test bit 0 for virtual voice
        // ------------------------------------------------------------
        // Reload voice_requirements_mask bitmask for per-bit tests and advance Y past it
        lda     voice_requirements_mask
        iny

        ror     
        bcc     test_bit1_alloc_real_voice

        // Bit 0 set → set up virtual voice X+4
        inx
        inx
        inx
        inx
        jsr     configure_voice_data_stream

test_bit1_alloc_real_voice:
        // ------------------------------------------------------------
		// Test bit 1 for real voice
        // ------------------------------------------------------------
        ror     
        bcc     test_bit2_alloc_virtual_voice

        // Bit 1 set → allocate an additional real voice and set its data
        jsr     allocate_available_real_voice
        jsr     configure_voice_data_stream

test_bit2_alloc_virtual_voice:
        // ------------------------------------------------------------
		// Test bit 2 for virtual voice
        // ------------------------------------------------------------
        ror     
        bcc     test_bit3_alloc_real_voice

        // Bit 2 set → set up another virtual voice X+4
        inx
        inx
        inx
        inx
        jsr     configure_voice_data_stream

test_bit3_alloc_real_voice:
        // ------------------------------------------------------------
		// Test bit 3 for real voice
        // ------------------------------------------------------------
        ror     
        bcc     test_bit4_alloc_virtual_voice

        // Bit 3 set → allocate another real voice and set its data
        jsr     allocate_available_real_voice
        jsr     configure_voice_data_stream

test_bit4_alloc_virtual_voice:
        // ------------------------------------------------------------
		// Test bit 4 for virtual voice
        // ------------------------------------------------------------
        ror     
        bcc     test_bit5_alloc_filter_voice

        // Bit 4 set → set up another virtual voice X+4
        inx
        inx
        inx
        inx
        jsr     configure_voice_data_stream

test_bit5_alloc_filter_voice:
        // ------------------------------------------------------------
		// Test bit 5 for filter
        // ------------------------------------------------------------
        ror     
        bcc     finalize_primary_voice_stream_binding

        // Bit 5 set → allocate filter and set its data
        jsr     allocate_special_voice_3
        jsr     configure_voice_data_stream

finalize_primary_voice_stream_binding:
        // ------------------------------------------------------------
		// Set up the the first voice we allocated (back when we tested bit 6),
		// for base address and offset.
		//
		// At this point Y points to the start of that voice's data.
        // ------------------------------------------------------------
        // Recover first allocated voice index into X
        ldx     first_voice_index

        // Set base address for this voice’s data
        lda     sound_data_base_lo
        sta     voice_data_base_lo,x
        lda     sound_data_base_hi
        sta     voice_data_base_hi,x

        // Set offset: low := Y (current read offset), high := 0
        tya
        sta     voice_data_offsets_lo,x
        lda     #$00
        sta     voice_data_offsets_hi,x

        // Associate the sound ID with this voice
        lda     pending_sound_idx
        sta     snd_to_start_on_voice,x

        // Prepare return value: on success, A := pending_sound_idx
        lda     pending_sound_idx

start_sound_finish_with_status:
        sta     start_sound_result

        // Re-enable sound processing
        lda     #FALSE
        sta     sound_processing_disabled_flag

        // Restore registers
        pla
        tay
        pla
        tax
        pla

        lda     start_sound_result
        rts
/*
================================================================================
  configure_voice_data_stream
================================================================================
Summary
	Initialize per-voice data pointers for logical voice X by capturing the current
	sound_data_base, reading a 16-bit offset from the sound’s data stream, and
	recording which sound is bound to this voice for startup.

Arguments
	X              				Logical voice index being configured
	Y                       	Offset within the sound data stream from which the
								16-bit little-endian offset is read

Global Inputs
	sound_data_base_lo/hi   	Base pointer to the current sound resource’s data
	pending_sound_idx        	Sound ID currently being prepared for startup

Global Outputs
	voice_data_base_lo/hi   	Per-voice copy of the sound’s data base
	voice_data_offsets_lo/hi	Per-voice 16-bit offset into the sound data stream
	snd_to_start_on_voice   	Marks voice X as scheduled to start this sound ID
								when the voice allocator runs next

Description
	• Saves A to preserve the caller’s value.
	• Copies sound_data_base into the per-voice base table so later routines know
	where this voice’s data stream begins.
	• Reads a 16-bit little-endian offset from (sound_data_base + Y) and stores it
	in voice_data_offsets[x], advancing Y past the two bytes.
	• Records pending_sound_idx into snd_to_start_on_voice[x], marking that this
	logical voice slot should begin executing the sound once allocation and
	startup processing occur.
	• Restores A and returns with Y advanced, ready for the caller to continue
	parsing the sound’s data structure.
================================================================================
*/
* = $4E99
configure_voice_data_stream:
        pha                                     

		// Publish the sound stream base address to the voice-data base address table
        lda     sound_data_base_lo                 
        sta     voice_data_base_lo,x
        lda     sound_data_base_hi
        sta     voice_data_base_hi,x

		// Resolve the voice data offset
        lda     (sound_data_base_lo),y   
        sta     voice_data_offsets_lo,x
        iny

        lda     (sound_data_base_lo),y  
        sta     voice_data_offsets_hi,x
		
		// Move Y past the offset
        iny                      

		// Bind voice X to current sound-ID
        lda     pending_sound_idx                  
        sta     snd_to_start_on_voice,x

        pla                                     
        rts
/*
================================================================================
  stop_sound_core
================================================================================
Summary
	Core stop routine for a sound index: always stops all of its voices and, when
	invoked in full-cleanup mode for the current priority-1 sound, also performs
	refcount decrement, priority bookkeeping reset, virtual multiplexing shutdown,
	and alternate-settings cleanup.

Arguments
	A            			Sound index to stop; copied into sound_to_stop on
							entry

Global Inputs
	sound_to_stop           Target sound index to be stopped (set from A on entry)
	pri1_snd_idx            Sound index currently tracked as the “priority-1”
							starting sound
	stop_sound_cleanup_mode Current stop mode selector

Global Outputs
	sound_to_stop           Updated to the sound index passed in A
	
	If the sound stopped is a priority-1 sound:
		pri1_snd_idx            		Cleared to 0
		pri1_snd_starting_flag  		Set to FALSE
		vmux_primary_active_flag_bff 	Set to FALSE
		vmux_secondary_active_flag_bf	Set to FALSE

Description
	• Copies the caller-supplied sound index from A into sound_to_stop and saves
	A, X, and Y on the stack.
	• If sound_to_stop matches the current priority-1 sound (pri1_snd_idx):
		– Calls stop_all_voices_for_sound once to tear down all voices owned by
		  that sound.
		– If stop_sound_cleanup_mode is STOP_SOUND_MODE_FULL_CLEANUP, also:
			  · Decrements the sound’s refcount (subject to music state),
			  · Clears pri1_snd_idx and pri1_snd_starting_flag,
			  · Clears vmux_primary_active_flag_bff and
				vmux_secondary_active_flag_bff,
			  · Calls clear_all_alternate_settings to reset any auxiliary
				filter/voice configuration.
	• If sound_to_stop does not match pri1_snd_idx, calls
	stop_all_voices_for_sound once and performs no additional cleanup.
	• Restores Y, X, and A from the stack before returning, so callers can treat
	this as a non-destructive core stop primitive keyed by the global cleanup
	mode.
================================================================================
*/
* = $5031
stop_sound_core:
		// Save the sound to be stopped
        sta     sound_to_stop          

		// Save registers
        pha              
        txa
        pha              
        tya
        pha              

		// Are we stopping the priority-1 sound?
        lda     sound_to_stop
        cmp     pri1_snd_idx
        bne     ssp_nonstarting_sound

        // ------------------------------------------------------------
        // Sound is the priority-1 sound
        // ------------------------------------------------------------
        // Stop all voices for it
        jsr     stop_all_voices_for_sound

		// Are we doing a 'voices only' cleanup? If so, exit
        lda     stop_sound_cleanup_mode
        cmp     #STOP_SOUND_MODE_FULL_CLEANUP
        bne     ssp_exit_2                    

        // ------------------------------------------------------------
		// Full cleanup requested
        // ------------------------------------------------------------
		// Decrement the refcount
        ldx     sound_to_stop
        ldy     #$00
        jsr     dec_sound_refcount_if_no_music

		// Clear the priority-1 index and flags
        lda     #$00
        sta     pri1_snd_idx
        sta     pri1_snd_starting_flag
		
		// Clear voice multiplexing flags
        sta     vmux_primary_active_flag_bff
        sta     vmux_secondary_active_flag_bff

		// Clear all alternate settings
        jsr     clear_all_alternate_settings
ssp_exit_2:		
        jmp     ssp_exit

ssp_nonstarting_sound:
        // ------------------------------------------------------------
		// Not a priority-1 sound - stop all voices for it and exit
        // ------------------------------------------------------------
        jsr     stop_all_voices_for_sound            

ssp_exit:
        pla
        tay                                     
        pla
        tax                                     
        pla                                     
        rts                                     
/*
================================================================================
  stop_all_voices_for_sound
================================================================================
Summary
	Stop every logical voice slot currently bound to the sound index stored in
	sound_to_stop by iterating over all primary and filter slots and invoking
	stop_logical_voice for each match.

Global Inputs
  sound_to_stop           Sound index whose voices should be stopped
  voice_sound_id_tbl      Map: logical voice index → owning sound index

Description
  • Initializes X to FILTER_VOICE_INDEX and scans logical voice slots downward
    to 0.
  • For each slot X whose voice_sound_id_tbl entry matches sound_to_stop:
        – Saves X into saved_voice_slot_index.
        – Calls stop_logical_voice with A := X, stopping that logical voice and
          any associated hardware/virtual state.
        – Restores X from saved_voice_slot_index to continue scanning from the
          same index.
  • Ensures that all logical voices currently owned by sound_to_stop are
    stopped, including real voices, the filter voice (if used), and their
    associated multiplexed slots, before returning.
================================================================================
*/
* = $5070
stop_all_voices_for_sound:
        ldx     #FILTER_VOICE_INDEX                            

svfs_scan_slot:
		// Sound to stop is using this voice? If not, continue
        lda     sound_to_stop                   
        cmp     voice_sound_id_tbl,x
        bne     svfs_prev_slot                  

		// It's using this voice - stop this voice
        stx     saved_voice_slot_index  
        txa                             
        jsr     stop_logical_voice      
        ldx     saved_voice_slot_index  

svfs_prev_slot:
		// Next voice
        dex          
        bpl     svfs_scan_slot                     
        rts		
		
/*
================================================================================
  stop_sound_voices_only
================================================================================
Summary
	Stop a sound in “voices only” mode by setting the stop mode to
	STOP_SOUND_MODE_VOICES_ONLY and delegating to stop_sound_core, which will
	stop all of the sound’s voices but skip refcount, priority, multiplexing, and
	alternate-settings cleanup.

Arguments
	A                       Sound index to stop; passed directly through to
						  stop_sound_core

Global Outputs
	stop_sound_cleanup_mode Set to STOP_SOUND_MODE_VOICES_ONLY for the duration
						  of the call, instructing stop_sound_core to perform
						  a voices-only stop without refcount or global-state
						  teardown

Description
	• Saves the incoming sound index on the stack, sets stop_sound_cleanup_mode
	to STOP_SOUND_MODE_VOICES_ONLY, and restores A so the sound index is
	passed intact to stop_sound_core.
	• Calls stop_sound_core, which:
		– Stops all logical voices currently owned by the sound, but
		– Does not decrement refcounts, does not clear priority-1 tracking,
		  does not clear multiplexing flags, and does not reset alternate
		  settings under this mode.
	• Intended for callers that need to silence a sound’s voices while leaving
	higher-level bookkeeping, priorities, and filter/multiplexing configuration
	untouched.
================================================================================
*/
* = $5088
stop_sound_voices_only:
		// Set cleanup mode to "voices only"
        pha                                     
        lda     #STOP_SOUND_MODE_VOICES_ONLY
        sta     stop_sound_cleanup_mode         
        pla                                     
		
		// Perform a voices-only stop
        jsr     stop_sound_core 
        rts
/*
================================================================================
  stop_sound_and_clear_pending_starts
================================================================================
Summary
	Clear all pending “start sound on voice” assignments for a given sound index
	from the snd_to_start_on_voice[] table without affecting any active
	playback or reference counts.

Arguments
	A                     Sound index whose pending start mappings should be
						  removed

Global Inputs
	snd_to_start_on_voice	Table of pending sound indices for each logical voice

Global Outputs
  snd_to_start_on_voice		All entries equal to the specified sound index are
							replaced with SOUND_IDX_NONE; other slots are
							unchanged

Description
	• Scans the pending-start table from the highest index down to zero, looking
	for entries equal to the sound index in A.
	• For each matching slot, replaces the entry with SOUND_IDX_NONE so that no
	start will be scheduled for that voice slot.
	• Leaves all non-matching slots untouched, does not silence any active
	voices, and does not modify reference counts, priorities, or multiplexing
	state.
================================================================================
*/
* = $5093
stop_sound_and_clear_pending_starts:
        ldx     #MAX_VOICE_INDEX

stop_sound_scan_slot:
		// Voice sound matches the target sound? If not, continue
        cmp     snd_to_start_on_voice,x    
        bne     stop_sound_step_prev_slot       

		// Voice sound matches, clear sound to start
        pha                                     
        lda     #SOUND_IDX_NONE                 
        sta     snd_to_start_on_voice,x    
        pla                                     

stop_sound_step_prev_slot:
		// Next voice
        dex                                     
        bpl     stop_sound_scan_slot            
        // Fall through to stop_sound_full_cleanup
/*
================================================================================
  stop_sound_full_cleanup
================================================================================
Summary
	Stop a sound with full cleanup semantics by setting the stop mode to
	STOP_SOUND_MODE_FULL_CLEANUP and delegating to stop_sound_core, which will
	stop all voices and, when appropriate, perform refcount and global-state
	cleanup.

Arguments
	A                       Sound index to stop; passed directly through to
						  stop_sound_core

Global Outputs
	stop_sound_cleanup_mode Set to STOP_SOUND_MODE_FULL_CLEANUP for the duration
						  of the call, enabling full cleanup behavior inside
						  stop_sound_core and related routines

Description
	• Saves the incoming sound index on the stack, sets stop_sound_cleanup_mode
	to STOP_SOUND_MODE_FULL_CLEANUP, and restores A so the sound index is
	passed intact to stop_sound_core.
	• Calls stop_sound_core, which:
		– Always stops all voices for the given sound index, and
		– When the sound matches the current priority-1 sound, also performs
		  refcount decrement, priority bookkeeping reset, multiplexing shutdown,
		  and alternate-settings cleanup under full-cleanup mode.
	• Serves as the high-level entry point for callers that want the strongest
	stop semantics (voices + bookkeeping).
================================================================================
*/
* = $50A4
stop_sound_full_cleanup:
		// Set cleanup mode to "full cleanup"
        pha                                     
        lda     #STOP_SOUND_MODE_FULL_CLEANUP
        sta     stop_sound_cleanup_mode      
        pla                                     
		
		// Perform a full stop
        jsr     stop_sound_core             
        rts
/*
================================================================================
  stop_logical_voice
================================================================================
Summary
	Stop a single logical voice slot by clearing its software execution state and,
	when applicable, releasing the corresponding SID voice, silencing it during a
	full-stop cleanup, optionally releasing the shared filter voice, and clearing
	any paired multiplexed slot. Multiplexed voices are cleared logically only.

Arguments
	A                       Logical voice index to stop (0–6)
	X                       After entry via PHA/TAX, X = logical voice index

Global Inputs
  sid_filter_control_shadow	Software shadow of the SID filter control register
  filter_voice_in_use_flag				Flag indicating whether the voice filter is in use
  stop_sound_cleanup_mode  	Determines whether full cleanup is in effect

Description
	• All slots begin by clearing logical per-voice state via set_voice_to_unused.
	• Multiplexed voices (X ≥ FIRST_MULTIPLEXED_VOICE_INDEX) perform only this
	logical clear and return without interacting with SID hardware.

	• The filter voice (X == FILTER_VOICE_INDEX) skips real-voice silencing and
	filter-release checks, releases the filter voice directly, and returns.

	• Real SID voices (0–2) perform the following:
		– Snapshot the current SID filter-control shadow for later comparison.
		– If full cleanup is enabled and no music is playing, call
		  silence_voice_if_full_stop to clear gate/waveform bits.
		– Release the real SID voice via release_voice.
		– If the filter-control shadow changed and the lower three routing bits
		  are now zero (no voices routed to the filter), release the filter
		  logical voice if it is in use.
		– Compute the paired multiplexed slot (X+4) and clear it via
		  set_voice_to_unused.

	• The routine ensures consistent deactivation of the real voice, the filter
	controller when needed, and any associated virtual/multiplexed slot while
	preserving the caller-visible logical index in A.
================================================================================
*/
* = $50C7
stop_logical_voice:
		// Save logical voice index in stack
        pha                                     
        tax                                     

		// Clear voice usage
        jsr     set_voice_to_unused             

		// Is it a multiplexed voice? If so, exit
        cpx     #FIRST_MULTIPLEXED_VOICE_INDEX 
        bpl     sv_exit

		// Is it the filter voice? If so, skip the voice silencing
        cpx     #FILTER_VOICE_INDEX                            
        bpl     sv_release_primary_slot

        // ------------------------------------------------------------------
        // Real voices 0–2
        // ------------------------------------------------------------------
		// Snapshot filter state
        lda     sid_filter_control_shadow
        sta     filter_control_snapshot

		// Silence the voice (if full stop)
        jsr     silence_voice_if_full_stop    

sv_release_primary_slot:
		// Release the voice
        jsr     release_voice                   

		// Is it the filter voice? If so, exit
        cpx     #FILTER_VOICE_INDEX
        bpl     sv_exit

        // ------------------------------------------------------------------
        // Additional logic for real voices 0–2
        // Decide whether to release voice 3 based on filter state change.
        // ------------------------------------------------------------------
		// Did the filter state change? If not, continue
        lda     sid_filter_control_shadow
        cmp     filter_control_snapshot
        beq     sv_release_paired_virtual_slot            

		// It did change
		// Is any voice being routed to the filter? If not, skip
        and     #FILTER_ROUTING_MASK                            
        bne     sv_release_paired_virtual_slot            

        // ------------------------------------------------------------------
        // Filter state changed and no voice is being routed to the filter
        // ------------------------------------------------------------------
		// Save voice index in stack
        txa
        pha                                      

		// Is the filter voice in use? If not, skip
        lda     filter_voice_in_use_flag
        beq     sv_after_voice3_release

		// Filter voice in use, release the voice
        ldx     #FILTER_VOICE_INDEX
        jsr     release_voice
        jsr     set_voice_to_unused

sv_after_voice3_release:
		// Restore voice index
        pla
        tax                                      

sv_release_paired_virtual_slot:
		// Resolve the multiplexed voice index (X + 4) (virtual slot 4–6)
        inx
        inx
        inx
        inx                                     
		// Clear the paired multiplexed voice
        jsr     set_voice_to_unused             

sv_exit:
        pla                                      
        rts
/*
================================================================================
  silence_voice_if_full_stop
================================================================================
Summary
	Conditionally clear the waveform and gate bits for a real voice’s control
	register during a full stop, but only when no music is playing and the stop
	mode requests full cleanup. Otherwise, leave the control byte unchanged.

Arguments
	X                       Logical voice index

Global Inputs
	voice_ctrl_shadow       	Cached SID control bytes per voice slot
	music_playback_in_progress	Flag indicating whether music playback is active
	stop_sound_cleanup_mode		Current stop mode

Description
	• Loads the cached control byte for voice X and saves it on the stack while
	mirroring it into Y for later use.
	• If music is currently playing, exits immediately and does not modify the
	SID control register, ensuring music voices are not disturbed.
	• If the active stop mode is not STOP_SOUND_MODE_FULL_CLEANUP, exits without
	making any changes; partial stops do not clear waveform and gate bits.
	• Only when both conditions are satisfied (no music playing and full cleanup
	mode active) restores the original control byte to A, masks it with $0E to
	clear the gate and waveform bits, and invokes apply_voice_control_to_sid so
	the SID control register sees the masked value.
	• Finally, restores the original control byte from the stack into A so
	callers can still observe the pre-masked control value if needed.
	
================================================================================

 Clearing GATE + waveform bits forces the SID voice into a silent state:
   • GATE=0 enters the Release phase of the ADSR envelope (note-off).
   • Waveform bits=0 disables all oscillators, ensuring no residual audio.
   • Bits 1–3 (SYNC/RING/TEST) are preserved so inter-voice modulation state
     remains valid for the next note.

================================================================================
*/
* = $510A
silence_voice_if_full_stop:
		// Fetch original control byte, save it in stack
        lda     voice_ctrl_shadow,x                
        pha                           
		
		// Copy to Y for later restore
        tay                                     

		// Is music in progress? If so, exit
        lda     music_playback_in_progress      
        bne     cwast_exit

		// Are we doing a full cleanup? If not, exit
        lda     stop_sound_cleanup_mode                 
        cmp     #STOP_SOUND_MODE_FULL_CLEANUP
        bne     cwast_exit

        // ------------------------------------------------------------
        // Full cleanup: clear gate + waveform bits
        // Keep only bits 1–3 of the original control.
        // ------------------------------------------------------------
		// Restore original control into A
        tya                               
		// Clear bits 0, 4-7
        and     #VOICE_CTRL_CLEAR_GATE_WAVE_MASK                            
        jsr     apply_voice_control_to_sid          

cwast_exit:
        pla                                     
        rts
/*
================================================================================
  set_voice_to_unused
================================================================================
Summary
	Mark a logical voice slot as inactive by clearing its execution state and, for
	multiplexed (secondary) slots, removing the sound-ID binding. This routine does
	not release real SID voices; it only resets the software-side bookkeeping for
	the specified logical slot.

Arguments
	X                       	Logical voice index being marked unused

Global Inputs
	voice_instr_active_mask 	Bitmask of currently executing voice instruction slots
	voice_sound_id_tbl      	Sound-ID mapping for logical voices (used only for
								multiplexed slots)

Global Outputs
	voice_instr_repcount    	The repeat counter for slot X is cleared
	voice_instr_active_mask 	Bit for slot X is cleared, marking it as inactive
	voice_sound_id_tbl      	For multiplexed slots (X ≥ FIRST_MULTIPLEXED_VOICE_INDEX),
								the sound-ID binding is cleared to 0

Description
	• Clears the per-slot instruction repeat counter so the voice has no ongoing
	instruction sequencing.
	• Clears the slot’s bit in voice_instr_active_mask using the corresponding
	clear-mask entry from voice_alloc_clear_mask_tbl.
	• If the slot index corresponds to a multiplexed voice (typically 4–6), clears
	the voice_sound_id_tbl entry for that slot so no sound is associated with
	it.
	• Real and special SID voices (slots below FIRST_MULTIPLEXED_VOICE_INDEX) keep
	their sound-ID bindings, since their ownership is controlled elsewhere.
================================================================================
*/
* = $5126
set_voice_to_unused:
		// Reset repeat counter for this voice
        lda     #$00
        sta     voice_instr_repcount,x    

		// Clear voice from the active voices bitmask
        lda     voice_instr_active_mask    
        and     voice_alloc_clear_mask_tbl,x
        sta     voice_instr_active_mask

		// Is it a multiplexed voice? If so, clear the sound ID
        cpx     #FIRST_MULTIPLEXED_VOICE_INDEX                            
        bmi     svtu_exit                       // Not multiplexed voice → exit

		// Multiplexed voice - clear sound binding
        lda     #$00                            
        sta     voice_sound_id_tbl,x

svtu_exit:
        rts


