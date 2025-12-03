/*
================================================================================
Voice allocation — mapping sounds to logical and physical voices
================================================================================

Summary
    This module manages how sounds claim and release logical voices, how those
    logical voices map onto the three physical SID voices and the filter slot,
    and how per-sound reference counts are maintained. It implements the core
    priority and eviction policy used by the sound scheduler to decide which
    voices can be reused when a new sound starts.

Core responsibilities
    • Real-voice allocation
        - allocate_available_real_voice scans the three real SID voices and
          returns a free one when available.
        - voices_allocated is a bitmask of which real voices (0–2) are in use;
          count_available_real_voices converts this into a numeric count used
          by higher-level scheduling logic.
        - allocate_voice marks a chosen voice as allocated, records the sound’s
          priority, and increments the sound’s reference count unless music is
          currently active.

    • Filter voice and extended voices
        - Voice index 3 is treated as the special “filter voice”; it doesn’t
          have its own oscillator but represents ownership of the SID filter
          slot. allocate_filter_voice and release_voice coordinate this.
        - Voice indices ≥4 are extended / virtual voices used by the
          multiplexing system. They share hardware with the real voices but
          still participate in priority and refcount bookkeeping via
          allocate_voice and deallocate_voice.

    • Priority and eviction
        - count_evictable_voices examines all allocated real voices and
          determines:
            · how many of them are strictly lower priority than the new sound,
            · which allocated voice currently has the lowest priority, and
            · how the new sound’s priority compares to the current filter
              owner when there is a filter conflict.
        - This information drives the scheduler’s decision about whether
          enough lower-priority real voices can be evicted to meet a new
          sound’s voice requirements.
        - release_voice enforces the rules for deallocation vs repurposing:
            · MIN-priority voices can be released or muted outright depending
              on cleanup mode.
            · Higher-priority voices may be repurposed during the start of a
              MIN-priority sound when primary multiplexing is active, allowing
              the new sound to “take over” a voice while keeping multiplexing
              state coherent.
            · When multiplexing is active, releasing voice 0 may also clear
              alternate settings and multiplex flags.

    • Repurposing and multiplex-aware release
        - Under specific conditions (a MIN-priority sound starting while
          primary multiplexing is active and filter constraints satisfied),
          release_voice calls a small repurposing path:
            · marks the voice’s instruction stream active,
            · applies multiplexing/filter configuration for that voice,
            · re-triggers the gate bit and commits control to SID,
            · decrements the refcount for the old sound (if music is not
              responsible for lifetime).
        - Otherwise, voices go through the normal deallocate_voice path,
          which clears priority, updates voices_allocated for real voices,
          recomputes the free-count, and clears the voice→sound binding.

    • Reference-count management
        - dec_sound_refcount and inc_sound_refcount_if_no_music update the
          “liveness” table for sounds, adjusting only the refcount bits while
          preserving a separate lock bit used by the resource manager.
        - dec_sound_refcount_if_no_music and dv_dec_sound_refcount_and_return
          are helpers that skip refcount changes whenever music playback is
          active, so that music retains ownership of resources during songs.
        - clear_refcount_of_sounds_1_and_2 and set_refcount_of_sounds_1_and_2
          provide special-case handling for two core sounds whose lifetimes
          need explicit seeding or clearing at engine init/reset.

Interaction with other modules
    • Voice scheduler
        - Uses allocate_available_real_voice, count_evictable_voices, and
          allocate_voice to implement the “can we start this sound?” logic in
          start_sound.
        - Calls release_voice, stop_sound_voices_only, and related helpers
          during stop_sound_* flows to free or repurpose voices according to
          priority and multiplexing state.

    • Sound engine and multiplexing
        - Multiplexing code sets vmux_* flags and relies on release_voice to
          respect them, clearing alternate settings and repurposing voices
          only when it is safe to do so.
        - SID control helpers (in the voice controller module) are called by
          release_voice and its repurposing path to mute or retrigger voices
          at the right moments when a voice is released from or reassigned to
          a sound.

================================================================================
*/

#importonce

#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "sound_constants.inc"
#import "sid_voice_controller.asm"
#import "voice_scheduler.asm"

.label filter_priority_is_higher_flag_b        = $4cf7   // Result of comparing sound_priority vs. filter_priority (00 or FF)
.label lowest_alloc_voice_priority = $4cfc   // Lowest priority value found among allocated voices 0–2
.label lowest_alloc_voice_idx      = $4cfd   // Voice index (0–2) holding the lowest allocated priority
.label evictable_voice_count    = $4cf9   // Number of allocated voices whose priority is lower than sound_priority
.label sound_1_liveness = $7952
.label sound_2_liveness = $7953

.const LOCK_BIT                 = %1000_0000   // Bit mask for locking a sound (bit 7)

/*
================================================================================
  allocate_available_real_voice
================================================================================
Summary
        Scan the real voices and try to allocate the first free one, 
		using the voices_allocated bitmask. Delegates the actual allocation work 
		to allocate_voice.

Returns
        X			#$00–#$02 		on success (index of allocated real voice)
					#$FF 			if no free real voice is available

Global Inputs
        voices_allocated          	Bitmask of currently allocated real voices (0–2)

Description
        - Initializes X to 2 and tests voices 2, 1, then 0 in descending order.
        - For each voice, checks the corresponding bit in voices_allocated via
          voice_alloc_set_mask_tbl to determine whether the voice is free.
        - On the first free voice, calls allocate_voice, which marks the voice
          allocated, updates priority, and increments the sound’s refcount, then
          returns with X still set to that voice index.
        - If all three real voices are already allocated, the loop completes and
          X underflows to #$FF; the routine returns with X = #$FF to signal that
          no real voice is available.
================================================================================
*/
* = $4EB8
allocate_available_real_voice:
		// Save A
		pha                                     

		// Load bitmask of allocated real voices
        lda     voices_allocated                
		
		// Begin scanning from highest real voice
        ldx     #$02                            

scan_real_voice_slot:
		// Current voice allocated? If not, continue
        and     voice_alloc_set_mask_tbl,x      
        bne     step_prev_real_voice_or_fail    

        // ------------------------------------------------------------
        // Voice is free → allocate it and return
        // ------------------------------------------------------------
        jsr     allocate_voice                  
		
		// Restore A and return with X = allocated voice index
        pla                                     
        rts                                     

step_prev_real_voice_or_fail:
		// Reload bitmask for next iteration
        lda     voices_allocated                
		// Next voice index
        dex                                     
        bpl     scan_real_voice_slot           

        // ------------------------------------------------------------
        // No free voice found → X has underflowed to $FF
        // ------------------------------------------------------------
        // Restore A
		pla                   
		
		// Return with X = $FF (no available real voice)
        rts                                     
/*
================================================================================
  dec_sound_refcount
================================================================================
Summary
	Decrement the reference count for a sound resource while preserving the locking state.

Arguments
	X       				Sound resource ID

Global Inputs
	sound_liveness_tbl     	Per-sound memory attribute table (refcount + locking)

Global Outputs
	sound_liveness_tbl     	Updated memory attribute for the selected sound

Description
	- Loads the memory attribute byte for sound X and saves the original
	flags so the locking bit (bit 7) can be restored later.
	- Clears bit 7 and decrements the 7-bit magnitude; if the result would
	underflow, corrects it so the magnitude remains at zero.
	- Restores the original sign bit from the saved flags and writes the
	combined value back into the memory attribute table.
================================================================================
*/
* = $4CDA
dec_sound_refcount:
		// Resolve sound liveness
		lda     sound_liveness_tbl,x
		
		// Save flags (keeping locking bit7 state in the saved flags)
		php                        
		
		// Clear locking bit to get the pure refcount
		and     #MSK_LOW7BITS               
		
		// Subtract 1 from refcount
		sec                                 
		sbc     #$01           
		
		//Refcount underflowed?
		bpl     restore_original_sign       

		// Underflowed below 0: clamp it to 0
		clc
		adc     #$01                        

restore_original_sign:
		//Restore flags (and locking bit)
		plp                              
		
		// Locking set? If not, skip		
		bpl     store_final_mem_attr        
		
		// Locking was set, restore locking
		ora     #LOCK_BIT                  

store_final_mem_attr:
		// Commit liveness
		sta     sound_liveness_tbl,x           
		rts		
/*
================================================================================
  allocate_filter_voice
================================================================================
Summary
        Wrapper that allocates voice index 3 using the generic allocate_voice
        routine. Delegates allocation to allocate_voice.

Description
        - Saves A on the stack.
        - Forces X to #$03 and calls allocate_voice, which:
              • Sets filter_voice_in_use_flag = True.
              • Stores sound_priority for the voice.
              • Increments the sound refcount for pending_sound_idx.
        - Restores A from the stack.
================================================================================
*/
* = $4ED0
allocate_filter_voice:
		// Save A
        pha                                     
		
		// Allocate voice #3
        ldx     #$03                            
        jsr     allocate_voice                  
		
		// Restore A
        pla                                     
        rts
/*
================================================================================
  count_evictable_voices
================================================================================
Summary
        Scan real voices 0–2, count how many are evictable for a new sound
        based on priority, track the lowest-priority allocated voice, and
        optionally compare the new sound’s priority against voice #3.

Arguments
        None

Returns
        None (results are written to global outputs; A, X, Y are clobbered)

Global Inputs
        voices_allocated          Voice allocation bitmask (bits 0–2 for voices 0–2)
        voice_alloc_set_mask_tbl              Per-voice bitmasks used to test voices_allocated
        voice_priority_0          Priority values for voices 0–2
        sound_priority            Priority of the sound being considered for start
        filter_conflict_flag_b           Non-zero → also evaluate comparison vs. voice #3
        filter_priority          Priority value for voice #3

Global Outputs
        evict_min_priority        Lowest priority among allocated voices 0–2
        evict_min_voice_idx       Index (0–2) of lowest-priority allocated voice
        evictable_voice_count     Count of voices with priority < sound_priority
        filter_priority_is_higher_flag_b         #$FF if sound_priority >= filter_priority, else #$00

Description
        - Initializes the current minimum priority and evictable-voice counter.
        - Iterates voices X = 2..0; skips free voices via voices_allocated.
        - For each allocated voice, treats it as evictable when its priority is
          strictly less than sound_priority and increments evictable_voice_count.
        - Tracks the minimum priority and corresponding voice index across all
          allocated voices 0–2, regardless of evictability.
        - Stores the final evictable_voice_count and leaves the min-priority
          voice info in evict_min_priority and evict_min_voice_idx for later use.
        - When filter_conflict_flag_b is non-zero, compares sound_priority to
          filter_priority and updates filter_priority_is_higher_flag_b to reflect whether the
          new sound’s priority is at least as high as voice #3.
================================================================================
*/
* = $4ED8
count_evictable_voices:
        lda     #MAX_SND_PRIORITY              // Initialize current minimum priority
        sta     lowest_alloc_voice_priority

        ldy     #$00                           // Y := evictable voice count
        ldx     #$02                           // X := start from voice #2 and count down

		// Check if voice X is allocated and, if so, whether it is evictable
check_if_voice_is_allocated:
		// Is the current voice allocated? If not, skip it
        lda     voices_allocated               
        and     voice_alloc_set_mask_tbl,x     
        beq     next_voice                     

		// Is sound_priority <= voice_priority? If so, this voice is not evictable, skip it
        lda     voice_priority_0,x             
        cmp     sound_priority                 
        bpl     update_lowest_alloc_voice_priority      

		// This voice is evictable, update count
        iny                                    

update_lowest_alloc_voice_priority:
		// Keep lowest_alloc_voice_priority/idx candidates updated
		// Voice priority < lowest? If so, update lowest and keep voice index
        cmp     lowest_alloc_voice_priority             
        bpl     next_voice                     
        sta     lowest_alloc_voice_priority    
        stx     lowest_alloc_voice_idx         

next_voice:
		// Continue with next voice (while X >= 0)
        dex                                    
        bpl     check_if_voice_is_allocated    

		// Persist total evictable voices
        sty     evictable_voice_count          

        lda     sound_priority                 // Prepare for comparison vs. voice #3
        ldy     filter_conflict_flag_b                // Y := conflict flag
        beq     count_evictable_voices_exit    // No conflict → skip comparison, exit

        cmp     filter_priority               // Compare sound vs. voice #3 priority
        bmi     set_voice3_cmp_lt_flag         // sound_priority < voice3 → set 00

        ldx     #BTRUE                  // sound_priority >= voice3 → set FF
        jmp     store_filter_priority_is_higher_flag_b

        // ------------------------------------------------------------
        // Handle the "sound_priority < filter_priority" case.
        // Sets comparison result to 0. The extra CMP/STA logic evaluates
        // an index (0–2) against a priority value (0–$7F), which yields
        // no meaningful state change—lowest_alloc_voice_idx is effectively
        // written back unchanged.
        // ------------------------------------------------------------
set_voice3_cmp_lt_flag:
        ldx     #FALSE                            // Prepare comparison result: flag "sound < voice3" as 0
        lda     lowest_alloc_voice_idx             // A := index (0–2) of lowest-priority allocated voice
        cmp     filter_priority                // Compare that index against voice3’s PRIORITY byte (value, not index)
        bpl     store_filter_priority_is_higher_flag_b         // If index ≥ priority value (normally impossible/irrelevant), just skip next write
        sta     lowest_alloc_voice_idx             // Otherwise, rewrite the same index back (no net change in practice)

store_filter_priority_is_higher_flag_b:
        stx     filter_priority_is_higher_flag_b      			// Store boolean comparison result

count_evictable_voices_exit:
        rts
/*
================================================================================
  count_available_real_voices
================================================================================
Summary
        Count how many physical real voices (0–2) are currently free by examining
        the voices_allocated bitmask. Bits set indicate allocated voices; clear
        bits are free. Updates total_real_voices_available accordingly.

Description
        - Loads voices_allocated bitmask (bits 0–2 correspond to real voices).
        - Rotates each bit into carry and increments a counter for each clear bit.
        - Stores the final count into total_real_voices_available.
================================================================================
*/
* = $4F26
count_available_real_voices:
		// Save registers in stack
        pha                       
        txa
        pha                                     
        tya
        pha                                     

		// Init count at #0, load voices_allocated in .A
        lda     voices_allocated                
        ldx     #$00                            
		
		// Rotate bit 0 into carry
        ror                                     
		// Bit set? Skip. Bit clear? Increment count.
        bcs     test_voice1_allocation_bit           
        inx                                     

test_voice1_allocation_bit:
        // ------------------------------------------------------------
        // Test bit 1
        // ------------------------------------------------------------
        ror                
        bcs     test_voice2_allocation_bit                       
        inx                                     

test_voice2_allocation_bit:
        // ------------------------------------------------------------
        // Test bit 2
        // ------------------------------------------------------------
        ror                                     
        bcs     store_free_real_voice_count     
        inx                                     

store_free_real_voice_count:
		// Persist free-voice count
        stx     total_real_voices_available     

		// Restore registers
        pla
        tay
        pla
        tax
        pla
        rts		
/*
================================================================================
  release_voice
================================================================================
Summary
        Release or repurpose a voice indexed in X. The routine saves A, X, and Y
        on entry, then conditionally either fully deallocates the voice or
        reconfigures it for continued playback under specific conditions. All
        exit paths funnel into the deallocate_voice tail (dv_clear_voice_resource_id
        or dv_dec_sound_refcount_and_return), which restores registers and returns.

Arguments
        X  								index of the voice to release or repurpose

Returns
        None (A, X, Y restored via deallocate_voice tail)

Global Inputs
        vmux_active_flag_bff            		Non-zero if any multiplexing is active
        vmux_primary_active_flag_bff    		Multiplexing-1 activity flag
        vmux_secondary_active_flag_bff  		Multiplexing-2 activity flag
        voice_priority_*            			Priority values for voices
        stop_sound_cleanup_mode         		Sound-stop mode selector
        pri1_snd_starting_flag  	Non-zero if a new sound of priority 1 is starting
        vmux_filter_enabled_flag_bff          	Global filter enable flag
        filter_voice_in_use_flag               			Flag indicating voice 3 use status
        voice_sound_id_tbl            			Resource IDs for voices 0–2

Global Outputs
        filter_voice_in_use_flag               			Cleared when releasing voice 3 with no multiplexing
        voice_instr_active_mask 				Updated when repurposing a real voice
        voice_ctrl_shadow              			GATE bit set when retriggering a voice

Description
        - If vmux_active_flag_bff is non-zero:
              • For voice 0: disable vmux_primary_active_flag_bff and clear alternate settings.
              • Disable vmux_secondary_active_flag_bff for all voices.
              • Fully deallocate the voice via dv_clear_voice_resource_id.
        - If no multiplexing:
              • For voice 3 only, clear filter_voice_in_use_flag.
              • Inspect voice_priority[X]:
                    · If == 1:
                          - In full-stop mode, process multiplexings then deallocate.
                          - Otherwise, clear note for voices 0–2 and deallocate.
                    · If != 1:
                          - Unless pri1_snd_starting_flag and
                            vmux_primary_active_flag_bff are both active, deallocate.
                          - Voice index must be < 3 or deallocate.
                          - If filter disabled, or filter_voice_in_use_flag is zero,
                                repurpose the voice:
                                · Save its resource index for refcount adjustment.
                                · Mark the voice executing.
                                · Adjust waveform and filter.
                                · Set GATE bit and update control byte.
                                · Decrement resource refcount, restore regs, return.
                          - Otherwise, deallocate the voice.
================================================================================
*/
* = $4F45
release_voice:
        // ------------------------------------------------------------
        // All exit paths jump into deallocate_voice / dv_clear_voice_resource_id /
        // dv_dec_sound_refcount_and_return, which restore registers.
        // ------------------------------------------------------------
		// Save registers in stack
        pha                                     
        txa
        pha                                     
        tya
        pha                                     

		// Multiplexing active? If not, skip this section
        lda     vmux_active_flag_bff
        beq     rv_no_multiplexing_path                 

        // ------------------------------------------------------------
		// Multiplexing is active
        // ------------------------------------------------------------
		// Voice 0? If not, skip
        cpx     #$00                            
        bne     rv_disable_arp2_and_free
		
        // ------------------------------------------------------------
		// Voice 0
        // ------------------------------------------------------------
		// Clear primary multiplexing
        stx     vmux_primary_active_flag_bff               
		
		// Clear alternate multiplexing settings
        jsr     clear_all_alternate_settings    

rv_disable_arp2_and_free:
		// Clear secondary multiplexing
        lda     #FALSE
        sta     vmux_secondary_active_flag_bff    
		
		// Tail-call to fully deallocate the voice
        jmp     dv_clear_voice_resource_id        

rv_no_multiplexing_path:
        // ------------------------------------------------------------
        // No multiplexing active
        // ------------------------------------------------------------
		// Voice 3?
        cpx     #$03
        bne     rv_check_priority_eq_min
		
        // ------------------------------------------------------------
		// Voice 3
        // ------------------------------------------------------------
		// Mark voice 3 as not in use 
        lda     #FALSE
        sta     filter_voice_in_use_flag         

rv_check_priority_eq_min:
		// Is this voice running with minimum sound priority?
        lda     voice_priority_0,x              
        cmp     #MIN_SOUND_PRIORITY
        bne     rv_priority_ne_min_path         

        // ------------------------------------------------------------
        // Min priority
        // ------------------------------------------------------------
        // Voices only cleanup mode?
        lda     stop_sound_cleanup_mode
        cmp     #STOP_SOUND_MODE_VOICES_ONLY
        bne     rv_full_stop_real_voices      

        // ------------------------------------------------------------
        // Voices only: update multiplexing
        // ------------------------------------------------------------
        jsr     process_multiplexing
		
		// Finalize the deallocation
        jmp     deallocate_voice

rv_full_stop_real_voices:
        // ------------------------------------------------------------
        // Full cleanup (min priority)
        // ------------------------------------------------------------
		// Voice 3? If so, skip
        cpx     #$03
        bpl     rv_deallocate_tail            // Voice 3+ → skip clear waveform
		
		// Real voices 0-2, clear waveform to stop the not
        jsr     silence_voice_if_full_stop    

rv_deallocate_tail:
		// Finalize the deallocation
        jmp     deallocate_voice                

rv_priority_ne_min_path:
        // ------------------------------------------------------------
        // Priority not minimum - check if we can repurpose the voice
		// for something else.
        // ------------------------------------------------------------
		// Is there an incoming min priority sound? If not, skip and deallocate voice
        lda     pri1_snd_starting_flag
        beq     deallocate_voice                

		// Is primary multiplexing active? If not, skip and deallocate voice
        lda     vmux_primary_active_flag_bff
        beq     deallocate_voice                

		// Is it a virtual voice? If so, skip and deallocate voice
        cpx     #$03
        bpl     deallocate_voice                

        // ------------------------------------------------------------
        // At this point:
        // Deciding whether to repurpose voice or fully release.
        // ------------------------------------------------------------		
		// Filter enabled ? If not, repurpose voice
        lda     vmux_filter_enabled_flag_bff
        beq     repurpose_voice                

		// Is voice 3 in use? If not, repurpose voice
        lda     filter_voice_in_use_flag
        beq     repurpose_voice                

		// Deallocate voice
        jmp     deallocate_voice                

repurpose_voice:
        // ------------------------------------------------------------
        // Repurpose the voice
		//
		// At this point:
		// 		-We're releasing a "high" priority voice
		// 		-We have a "min" priority sound starting
		// 		-Primary voice multiplexing is active
		// 		-The voice we're releasing is real (physical)
		//
		//	AND at least one of these is true:
		//		-Multiplexing filter is not enabled
		//		-Voice 3 is not in use
        // ------------------------------------------------------------
		// Save the current sound resource (for later refcount update)
        lda     voice_sound_id_tbl,x              
        pha                                     

        // Mark this voice as active
		lda     voice_instr_active_mask
        ora     voice_alloc_set_mask_tbl,x                  
        sta     voice_instr_active_mask

		// Apply mux'ing and filter
        jsr     apply_multiplexing_and_filter_for_voice

		// Set the gate bit for this voice to retrigger a note and apply
        lda     voice_ctrl_shadow,x
        ora     #$01                      
        sta     voice_ctrl_shadow,x
        jsr     apply_voice_control_to_sid          

		// Restore the sound resource into .X, dec its refcount
        pla                          
        tax                          
        ldy     #$00
        jmp     dv_dec_sound_refcount_and_return       
/*
================================================================================
  deallocate_voice (subsection of previous routine)
================================================================================
Summary
        Release a voice slot. Clears the voice’s priority, updates the global
        voices-allocated bitmask, clears the associated resource ID, decreases
        the sound resource’s reference count, and updates the count of available
        real voices.

Arguments
        X  				voice index to release

Description
        - Clears priority for the selected voice.
        - Unsets the bit for this voice in the voices_allocated mask.
        - Recomputes the number of available real voices.
        - Loads the sound resource ID mapped to this voice.
        - Clears the resource ID entry for this voice.
        - Decreases the resource’s memory attribute reference count.
        - Restores registers and exits.
================================================================================
*/
* = $4FC5
deallocate_voice:
		// Clear voice priority
        lda     #$00
        sta     voice_priority_0,x             

        // Clear voice bit in the "voices_allocated" bitmask
		lda     voices_allocated               
        and     voice_alloc_clear_mask_tbl,x
        sta     voices_allocated

		// Recompute number of free real voices
        jsr     count_available_real_voices    

dv_clear_voice_resource_id:
		// Copy voice index from X to Y
        txa                                     
        tay                       
		
		// Resolve sound ID into X
        ldx     voice_sound_id_tbl,y            
		
		// Clear sound ID for this voice
        lda     #$00                            
        sta     voice_sound_id_tbl,y

		// Unnecessary copy - kept from the original
        tay                                     

dv_dec_sound_refcount_and_return:
		// Decrease the refcount for the associated sound
        jsr     dec_sound_refcount_if_no_music

		// Restore registers / rebalance stack
        pla                                     
        tay
        pla                                     
        tax
        pla                                     
        rts
/*
================================================================================
  dec_sound_refcount_if_no_music
================================================================================
Summary
        Decrease the reference count of a sound resource, unless music playback 
		is currently in progress. 

Arguments
    X       						Sound resource index

Global Inputs
	music_playback_in_progress    	Whether music is being played back or not
	sound_liveness_tbl              Sound liveness table

Global Outputs
	sound_liveness_tbl              Refcount of entry X decremented (unless music active)

Description
	- If music playback is active, the decrement is skipped entirely to
	avoid interfering with music-managed resource lifetimes.
	- Otherwise, delegates to dec_sound_refcount.
================================================================================
*/
* = $4FEA
dec_sound_refcount_if_no_music:
		// Music playback in progress? If so, exit
		lda     music_playback_in_progress   
		beq     do_decrease                  
		rts                                  

do_decrease:
		// No music in progress
		// Decrement refcount (preserving locking)
		jsr     dec_sound_refcount           
		rts                                  
/*
================================================================================
  inc_sound_refcount_if_no_music
================================================================================
Summary
	Increment the reference count of a sound resource, unless music playback 
	is currently in progress.

Arguments
    X       						Sound resource index

Global Inputs
	music_playback_in_progress    	Whether music is being played back or not
	sound_liveness_tbl              Sound liveness table

Global Outputs
	sound_liveness_tbl              Refcount of entry X incremented (unless music active)

Description
	- If music playback is active, the increment is skipped entirely to
	avoid interfering with music-managed resource lifetimes.
	- Otherwise increments sound_liveness_tbl[X], increasing the resource’s
	reference count.
================================================================================
*/
* = $4FF4
inc_sound_refcount_if_no_music:
		// Music playback in progress? If so, exit
		lda     music_playback_in_progress   
		beq     do_increase                  
		rts                                  

do_increase:
		// No music in progress
		// Increase refcount
		inc     sound_liveness_tbl,x
		rts                         
/*
================================================================================
  allocate_voice
================================================================================
Summary
        Allocate or mark usage of a voice based on the index in X. Updates the
        proper global tables for real voices (0–2), the special voice 3 flag, and
        always applies per-voice priority and increments the refcount for the
        sound identified by pending_sound_idx.

Arguments
        X 							voice index to allocate

Global Inputs
        voices_allocated            Bitmask of real-voice allocation (voices 0–2)
        sound_priority              Priority value for the sound being assigned
        pending_sound_idx           Resource index of sound associated with voice

Global Outputs
        voices_allocated            Updated when X is 0–2
        total_real_voices_available Updated via count_available_real_voices
        filter_voice_in_use_flag               Set to #BTRUE when X = 3
        voice_priority_0[X]         Updated with sound_priority

Description
        - Distinguishes three categories of voice index:
              • X = 0–2: real hardware voices
                    - Marks the bit in voices_allocated.
                    - Recomputes total_real_voices_available.
              • X = 3: special voice
                    - Sets filter_voice_in_use_flag to #$FF.
              • X ≥ 4: extended / non-real indices
                    - No allocation flags are touched.
        - For all categories (0–2, 3, or ≥4):
              • Stores sound_priority into voice_priority_0,X.
              • Calls inc_sound_refcount_if_no_music using pending_sound_idx as the resource ID.
================================================================================
*/
* = $4FFE
allocate_voice:
		// Save registers on stack
        pha                                     
        txa
        pha                                     
        tya
        pha                                     

		// Voice 3? If so, mark it in use with a special flag
        cpx     #$03
        bne     av_dispatch_non3_index
		
		// Mark voice 3 in use
        lda     #BTRUE
        sta     filter_voice_in_use_flag                   
		
		// Skip handling for real voices (0-2)
        jmp     av_set_priority_and_inc_refcount              

av_dispatch_non3_index:
        // ------------------------------------------------------------
        // X = 0–2 (negative from CPX) → handle as real voice.
        // X ≥ 4 (non-negative) → skip to priority tail.
        // ------------------------------------------------------------
		// Virtual voice? If so, skip
        bpl     av_set_priority_and_inc_refcount              

        // ------------------------------------------------------------
        // Real voice (X = 0,1,2)
        // ------------------------------------------------------------
		// Mark this voice as allocated in the bitmask
        lda     voices_allocated
        ora     voice_alloc_set_mask_tbl,x                  
        sta     voices_allocated

		// Recompute real-voice availability
        jsr     count_available_real_voices     

av_set_priority_and_inc_refcount:
        // ------------------------------------------------------------
        // Common tail for all voices
        // ------------------------------------------------------------
		// Store sound priority for this voice
        lda     sound_priority
        sta     voice_priority_0,x              

        ldy     #$00                            

		// Inc refcount of the sound resource
        ldx     pending_sound_idx            
        jsr     inc_sound_refcount_if_no_music              

		// Restore registers
        pla                 
        tay
        pla                 
        tax
        pla                                     
        rts
/*
================================================================================
  clear_refcount_of_sounds_1_and_2
================================================================================
Summary
        Reset the reference-count fields for sounds #1 and #2 while preserving
        only bit 7 of each sound’s memory-attribute byte.
		
Description
        - Loads sound_1_liveness, masks it with #$80, and writes it back.
        - Loads sound_2_liveness, masks it with #$80, and writes it back.
        - Bit 7 is preserved; bits 0–6 (the refcount) are cleared.
================================================================================
*/
* = $513E
clear_refcount_of_sounds_1_and_2:
        // ------------------------------------------------------------
        // Clear the low 7 bits of the reference count for sounds #1 and #2.
        // Preserves bit 7 (e.g., lock/flag) and zeroes bits 0–6.
        // ------------------------------------------------------------
        lda     sound_1_liveness
        and     #LOCK_BIT                           
        sta     sound_1_liveness

        lda     sound_2_liveness
        and     #LOCK_BIT                           
        sta     sound_2_liveness
        rts
/*
================================================================================
  set_refcount_of_sounds_1_and_2
================================================================================
Summary
        Mark sounds #1 and #2 as having a non-zero reference count by setting
        bit 0 of their memory-attribute bytes.

Description
        - Loads sound_1_liveness, sets bit 0, and writes it back.
        - Loads sound_2_liveness, sets bit 0, and writes it back.
        - Other bits (including bit 7) are preserved.
================================================================================
*/
* = $514F
set_refcount_of_sounds_1_and_2:
        // ------------------------------------------------------------
        // Force the reference count non-zero for sounds #1 and #2 by
        // setting bit 0. Other bits (including bit 7) are preserved.
        // ------------------------------------------------------------
        lda     sound_1_liveness
        ora     #SOUND_REFCOUNT_BIT                           
        sta     sound_1_liveness

        lda     sound_2_liveness
        ora     #SOUND_REFCOUNT_BIT                           
        sta     sound_2_liveness
        rts

/*
function allocate_available_real_voice() -> int
    // voices_allocated: bitmask for real voices 0–2
    mask = voices_allocated

    // Scan real voices in order 2, 1, 0
    for voice_index from 2 down to 0:
        if bit_for_voice(voice_index) in mask is CLEAR then
            // Free slot → allocate and return it
            allocate_voice(voice_index)
            return voice_index
        end if
    end for

    // No free real voice available
    return -1      // represented by $FF in the original code
end function


procedure dec_sound_refcount(sound_id)
    // sound_liveness_tbl[sound_id]:
    //   bit 7 = LOCK_BIT (lock flag)
    //   bits 0–6 = reference count (0–127)

    mem_attr = sound_liveness_tbl[sound_id]

    // Preserve original lock flag
    lock_set = (mem_attr & LOCK_BIT) != 0

    // Strip lock bit to get refcount-only value
    refcount = mem_attr & 0x7F

    // Decrement refcount, but do not go below 0
    if refcount > 0:
        refcount -= 1
    else:
        refcount = 0

    // Recombine with original lock bit
    if lock_set:
        mem_attr = refcount | LOCK_BIT
    else:
        mem_attr = refcount
    end if

    sound_liveness_tbl[sound_id] = mem_attr
end procedure


procedure allocate_filter_voice()
    // Wrapper: allocate logical voice 3 (filter voice)
    allocate_voice(3)
end procedure


procedure count_evictable_voices()
    // Init: minimum priority as “maximal,” zero evictable count
    lowest_alloc_voice_priority = MAX_SND_PRIORITY
    lowest_alloc_voice_idx      = 0      // default; updated on first candidate
    evictable_voice_count       = 0

    // Scan real voices 2,1,0
    for v in [2, 1, 0]:
        // Skip if this voice is free
        if voices_allocated has_bit_cleared_for(v):
            continue

        voice_pri = voice_priority_0[v]

        // Evictable if strictly lower priority than new sound
        if voice_pri < sound_priority:
            evictable_voice_count += 1
        end if

        // Track lowest priority among allocated voices regardless of evictability
        if voice_pri < lowest_alloc_voice_priority:
            lowest_alloc_voice_priority = voice_pri
            lowest_alloc_voice_idx      = v
        end if
    end for

    // Persist count
    evictable_voice_count = evictable_voice_count

    // Optionally compare new sound vs filter priority
    if not filter_conflict:
        // No filter conflict → skip comparison
        return
    end if

    if sound_priority >= filter_priority:
        // New sound is >= as important as current filter owner
        filter_priority_is_higher = TRUE
    else:
        // New sound is lower priority than filter owner
        filter_priority_is_higher = FALSE

        // The extra index-vs-priority comparison in the original code
        // does not change any real state; effectively lowest_alloc_voice_idx
        // remains what we already computed.
    end if
end procedure


procedure count_available_real_voices()
    // voices_allocated bits 0–2: 1 = allocated, 0 = free
    free_count = 0

    for v in [0, 1, 2]:
        if bit_for_voice(v) in voices_allocated is CLEAR:
            free_count += 1
        end if
    end for

    total_real_voices_available = free_count
end procedure


procedure release_voice(voice_index)
    // High-level: either fully deallocate the voice or repurpose it under
    // certain multiplexing rules. Any full-release paths eventually go through
    // deallocate_voice() / dv_clear_voice_resource_id().

    if vmux_active_flag then
        // --- Multiplexing is active ---

        if voice_index == 0 then
            // Clearing primary multiplexing and all alternate settings
            vmux_primary_active = FALSE
            clear_all_alternate_settings()
        end if

        // Always disable secondary multiplexing
        vmux_secondary_active = FALSE

        // Fully deallocate this voice (clear ID, priority, refcount, etc.)
        dv_clear_voice_resource_id(voice_index)
        return
    end if

    // --- No multiplexing active ---

    if voice_index == 3 then
        // Releasing filter voice slot
        filter_voice_in_use = FALSE
    end if

    pri = voice_priority_0[voice_index]

    if pri == MIN_SOUND_PRIORITY then
        // ----- MIN priority voice -----
        if stop_sound_cleanup_mode == STOP_SOUND_MODE_VOICES_ONLY then
            // Voices-only cleanup: update multiplexing then release
            process_multiplexing()
            deallocate_voice(voice_index)
            return
        else
            // Full cleanup. Real voices 0–2 mute their waveform; voice 3 just releases.
            if voice_index <= 2 then
                silence_voice_if_full_stop(voice_index)
            end if
            deallocate_voice(voice_index)
            return
        end if
    end if

    // ----- Priority > MIN: see if we can repurpose -----

    // Only repurposed when a MIN-priority sound is starting and primary mux is active
    if not pri1_snd_starting:
        deallocate_voice(voice_index)
        return
    end if

    if not vmux_primary_active:
        deallocate_voice(voice_index)
        return
    end if

    // Virtual voices (>=3) are never repurposed; they are just deallocated
    if voice_index >= 3:
        deallocate_voice(voice_index)
        return
    end if

    // At this point:
    //   - We’re releasing a higher-priority real voice (0–2)
    //   - A MIN-priority sound is starting
    //   - Primary multiplexing is active
    //
    // Decide whether to repurpose based on filter usage
    if not vmux_filter_enabled == 0 or not filter_voice_in_use then
        // Filter not enabled or not in use → repurpose real voice
        repurpose_voice(voice_index)
    else
        // Filter is in use and enabled → just deallocate
        deallocate_voice(voice_index)
    end if
end procedure


procedure repurpose_voice(voice_index)
    // Save which sound this voice belonged to; we’ll adjust its refcount later
    old_sound_id = voice_sound_id_tbl[voice_index]

    // Mark this voice’s instruction stream as active
    voice_instr_active_mask |= bit_for_voice(voice_index)

    // Apply multiplexing and filter configuration for this voice
    apply_multiplexing_and_filter_for_voice(voice_index)

    // Retrigger note: set GATE bit and push control byte to SID
    voice_ctrl_shadow[voice_index] |= GATE_BIT
    apply_voice_control_to_sid(voice_index)

    // Decrement refcount for the old sound, but only if music isn’t playing
    dec_sound_refcount_if_no_music(old_sound_id)

    // Finally restore all CPU registers (handled by the shared tail in assembly)
end procedure


procedure deallocate_voice(voice_index)
    // 1) Clear the voice’s priority
    voice_priority_0[voice_index] = 0

    // 2) For real voices 0–2, clear allocation bit and recompute free-count
    if voice_index in {0, 1, 2} then
        voices_allocated &= ~bit_for_voice(voice_index)
        count_available_real_voices()
    end if

    // 3) Clear the voice’s sound binding and decrement the sound refcount
    dv_clear_voice_resource_id(voice_index)
end procedure


procedure dv_clear_voice_resource_id(voice_index)
    // Look up which sound this logical voice was associated with
    sound_id = voice_sound_id_tbl[voice_index]

    // Clear mapping
    voice_sound_id_tbl[voice_index] = 0

    // Decrement the sound’s refcount if music is not playing
    dec_sound_refcount_if_no_music(sound_id)
end procedure



procedure dec_sound_refcount_if_no_music(sound_id)
    if music_playback_in_progress then
        // Music owns resource lifetimes right now; do not touch refcounts
        return
    end if

    dec_sound_refcount(sound_id)
end procedure


procedure inc_sound_refcount_if_no_music(sound_id)
    if music_playback_in_progress then
        // Music owns resource lifetimes; skip increments
        return
    end if

    // Increment reference count field of sound_liveness_tbl[sound_id],
    // preserving the lock bit in the high bit.
    mem_attr = sound_liveness_tbl[sound_id]

    lock_set = (mem_attr & LOCK_BIT) != 0
    refcount = mem_attr & 0x7F

    // Increment refcount (the original code doesn’t clamp here; the refcount
    // field is small enough that wrap is unlikely in practice.)
    refcount = (refcount + 1) & 0x7F

    if lock_set:
        mem_attr = refcount | LOCK_BIT
    else:
        mem_attr = refcount
    end if

    sound_liveness_tbl[sound_id] = mem_attr
end procedure


procedure allocate_voice(voice_index)
    // For all categories we will:
    //   - store sound_priority into voice_priority_0[voice_index]
    //   - bump refcount of pending_sound_idx (unless music active)

    // 1) Handle index-specific allocation markers
    if voice_index == 3 then
        // Special filter voice
        filter_voice_in_use = TRUE

    else if voice_index in {0, 1, 2} then
        // Real hardware voice
        voices_allocated |= bit_for_voice(voice_index)
        count_available_real_voices()

    else
        // voice_index >= 4: extended / virtual voices
        // No allocation bitmask changes are needed.
        // (They still get priority and refcount updates below.)
    end if

    // 2) Store the sound’s priority in the per-voice priority table
    voice_priority_0[voice_index] = sound_priority

    // 3) Increment refcount for the sound being assigned to this voice,
    //    unless music is active
    inc_sound_refcount_if_no_music(pending_sound_idx)
end procedure


procedure clear_refcount_of_sounds_1_and_2()
    // Zero refcounts (bits 0–6) but preserve lock bit (bit 7) for sounds 1 and 2
    for sound_id in [1, 2]:
        mem_attr = sound_liveness_value_for(sound_id)   // sound_1_liveness / sound_2_liveness
        lock_bit = mem_attr & LOCK_BIT
        sound_liveness_value_for(sound_id) = lock_bit
    end for
end procedure


procedure set_refcount_of_sounds_1_and_2()
    // Force refcount to be non-zero by turning on bit 0; keep all other bits.
    for sound_id in [1, 2]:
        mem_attr = sound_liveness_value_for(sound_id)
        mem_attr |= SOUND_REFCOUNT_BIT      
        sound_liveness_value_for(sound_id) = mem_attr
    end for
end procedure

*/