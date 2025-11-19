#importonce

#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "sid_voice_controller.asm"
#import "voice_primitives.asm"

.label voice3_priority_is_higher_flag_b        = $4cf7   // Result of comparing sound_priority vs. voice_priority_3 (00 or FF)
.label min_alloc_voice_priority = $4cfc   // Lowest priority value found among allocated voices 0–2
.label min_alloc_voice_idx      = $4cfd   // Voice index (0–2) holding the lowest allocated priority
.label evictable_voice_count    = $4cf9   // Number of allocated voices whose priority is lower than sound_priority
.label sound_1_memory_attr = $7952
.label sound_2_memory_attr = $7953

.const LOCK_BIT                 = %1000_0000   // Bit mask for locking a sound (bit 7)

/*
================================================================================
  allocate_available_real_voice
================================================================================
Summary
        Scan the real voices (indices 2, 1, 0) and try to allocate the first
        free one, using the voices_allocated bitmask. Delegates the actual
        allocation work to allocate_voice.

Arguments
        None

Returns
        X  → #$00–#$02 on success (index of allocated real voice)
             #$FF if no free real voice is available

Global Inputs
        voices_allocated          Bitmask of currently allocated real voices (0–2)
        voice_alloc_set_mask_tbl              Per-voice bitmasks used to probe allocation bits

Global Outputs
        voices_allocated          Updated indirectly via allocate_voice when a
                                  free voice is claimed
        total_real_voices_available
                                  Recomputed indirectly by allocate_voice after
                                  marking a real voice as allocated
        voice_priority_0          Priority entry for the allocated voice updated
                                  indirectly by allocate_voice

Description
        - Preserves A and Y across the call; X is used for scanning and as the
          return value.
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
        // ----------------------------------------------------------------------
        // Find and allocate the first free real voice among indices 2, 1, 0.
        //
        // Scans in order X = 2 → 1 → 0. For the first voice whose allocation bit
        // is clear, calls allocate_voice (which preserves caller registers) and
        // returns with X = that allocated voice index.
        //
        // If all three real voices are already allocated, X underflows to $FF and
        // the routine returns with X = $FF as the “no free voice” indicator.
        //
        // A and Y are preserved across the call; X carries the result.
        // ----------------------------------------------------------------------
        pha                                     // Save A

        lda     voices_allocated                // Load bitmask of allocated real voices
        ldx     #$02                            // Begin scanning from highest real voice (2)

scan_real_voice_slot:
        and     voice_alloc_set_mask_tbl,x                  // Test allocation bit for voice X
        bne     step_prev_real_voice_or_fail                      // Non-zero → allocated → try next lower voice

        // ------------------------------------------------------------
        // Voice X is free → allocate it and return
        // ------------------------------------------------------------
        jsr     allocate_voice                  // allocate_voice preserves A,X,Y for caller
        pla                                     // Restore A
        rts                                     // X = allocated voice (0,1,2)

step_prev_real_voice_or_fail:
        lda     voices_allocated                // Reload bitmask for next iteration
        dex                                     // X := X - 1
        bpl     scan_real_voice_slot           // Loop while X >= 0 (voices 2,1,0)

        // ------------------------------------------------------------
        // No free voice found → X has underflowed to $FF
        // ------------------------------------------------------------
        pla                                     // Restore A
        rts                                     // X = $FF (no available real voice)
/*
================================================================================
  dec_sound_refcount
================================================================================
Summary
	Decrement the memory attribute counter for a sound resource while
	preserving the original sign bit (bit 7) and clamping the numeric
	magnitude at zero (no underflow wraparound).

Arguments
	X       Sound resource ID whose memory attribute should be decremented

Global Inputs
	sound_liveness_tbl     Per-sound memory attribute table (counter + sign flag)

Global Outputs
	sound_liveness_tbl     Updated memory attribute for the selected sound

Returns
	None (updates the memory attribute in-place for sound X)

Description
	- Loads the memory attribute byte for sound X and saves the original
	flags so the sign bit (bit 7) can be restored later.
	- Clears bit 7 and decrements the 7-bit magnitude; if the result would
	underflow, corrects it so the magnitude remains at zero.
	- Restores the original sign bit from the saved flags and writes the
	combined value back into the memory attribute table.
	- Ensures the counter never wraps below zero while keeping any
	high-bit semantics (such as locked/pinned flags) intact.
================================================================================
*/
* = $4CDA
dec_sound_refcount:
		lda     sound_liveness_tbl,x           // A := current memory attribute for sound X
		php                                 // Save original flags (keep original sign bit7)
		and     #MSK_LOW7BITS               // Clear bit7 so we can safely decrement magnitude
		sec                                 // Prepare for subtract
		sbc     #$01                        // Decrement attribute (without sign bit)
		bpl     restore_original_sign       // If result ≥ 0, no wraparound from 0 → -1

		// Wrapped below 0: prior value must have been 0, so undo to keep it clamped at 0
		clc
		adc     #$01                        // Restore value back to 0

restore_original_sign:
		plp                                 // Restore original flags; N carries original sign bit7
		bpl     store_final_mem_attr        // If original sign was clear, keep result as-is
		ora     #LOCK_BIT                   // If original sign was set, restore bit7 on result

store_final_mem_attr:
		sta     sound_liveness_tbl,x           // Commit updated attribute back to table
		rts		
/*
================================================================================
  allocate_special_voice_3
================================================================================
Summary
        Wrapper that allocates voice index 3 using the generic allocate_voice
        routine. Marks voice 3 as in use, sets its priority entry, and increments
        the refcount for the sound identified by pending_sound_idx.

Arguments
        None (voice index is forced to 3 internally)

Returns
        A  → preserved
        X  → #$03 (special voice index)
        Y  → preserved

Global Inputs
        sound_priority                Priority for the sound being assigned
        pending_sound_idx                Resource index of sound whose refcount increases
        voice_priority_0              Per-voice priority table
        voice3_in_use                 Flag that indicates voice 3 usage

Global Outputs
        voice3_in_use                 Set to #$FF to mark voice 3 as in use
        voice_priority_0,3            Updated with sound_priority
        (inc_sound_refcount modifies the sound resource table externally)

Description
        - Saves A on the stack.
        - Forces X to #$03 and calls allocate_voice, which:
              • Sets voice3_in_use = True.
              • Stores sound_priority into voice_priority_0,3.
              • Increments the sound refcount for pending_sound_idx.
        - Restores A from the stack and returns with X left as #$03 and Y unchanged.
================================================================================
*/
* = $4ED0
allocate_special_voice_3:
        // ----------------------------------------------------------------------
        // Allocate voice index 3 by delegating to allocate_voice.
        //
        // This wrapper preserves A and Y for the caller. It forces X = 3,
        // calls allocate_voice (which marks voice3_in_use, updates priority,
        // and increments the sound refcount), then returns with:
        //
        //       A → preserved
        //       Y → preserved
        //       X → #$03
        // ----------------------------------------------------------------------
        pha                                     // Save A
        ldx     #$03                            // Force special voice index
        jsr     allocate_voice                  // allocate_voice preserves A,X,Y
        pla                                     // Restore A
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
        voice3_conflict_flag_b           Non-zero → also evaluate comparison vs. voice #3
        voice_priority_3          Priority value for voice #3

Global Outputs
        evict_min_priority        Lowest priority among allocated voices 0–2
        evict_min_voice_idx       Index (0–2) of lowest-priority allocated voice
        evictable_voice_count     Count of voices with priority < sound_priority
        voice3_priority_is_higher_flag_b         #$FF if sound_priority >= voice_priority_3, else #$00

Description
        - Initializes the current minimum priority and evictable-voice counter.
        - Iterates voices X = 2..0; skips free voices via voices_allocated.
        - For each allocated voice, treats it as evictable when its priority is
          strictly less than sound_priority and increments evictable_voice_count.
        - Tracks the minimum priority and corresponding voice index across all
          allocated voices 0–2, regardless of evictability.
        - Stores the final evictable_voice_count and leaves the min-priority
          voice info in evict_min_priority and evict_min_voice_idx for later use.
        - When voice3_conflict_flag_b is non-zero, compares sound_priority to
          voice_priority_3 and updates voice3_priority_is_higher_flag_b to reflect whether the
          new sound’s priority is at least as high as voice #3.
================================================================================
*/
* = $4ED8
count_evictable_voices:
        lda     #MAX_SND_PRIORITY              // Initialize current minimum priority
        sta     min_alloc_voice_priority

        ldy     #$00                           // Y := evictable voice count
        ldx     #$02                           // X := start from voice #2 and count down

// Check if voice X is allocated and, if so, whether it is evictable
check_if_voice_is_allocated:
        lda     voices_allocated               // A := allocation mask
        and     voice_alloc_set_mask_tbl,x                 // Mask for this voice
        beq     next_voice                     // Bit clear → voice free, skip

        lda     voice_priority_0,x             // A := this voice's priority
        cmp     sound_priority                 // Compare voice priority vs. new sound
        bpl     update_min_alloc_voice_priority      // sound_priority <= voice_priority → not evictable

        iny                                    // sound_priority > voice_priority → evictable

// Maintain min_alloc_voice_priority and min_alloc_voice_idx among candidates
update_min_alloc_voice_priority:
        cmp     min_alloc_voice_priority             // Compare voice priority with current minimum
        bpl     next_voice                     // If ≥ smallest, keep existing minimum
        sta     min_alloc_voice_priority             // New minimum priority found
        stx     min_alloc_voice_idx            // Track voice index for that minimum

next_voice:
        dex                                    // Test previous voice index
        bpl     check_if_voice_is_allocated    // Loop while X >= 0

        sty     evictable_voice_count          // Persist total evictable voices

        lda     sound_priority                 // Prepare for comparison vs. voice #3
        ldy     voice3_conflict_flag_b                // Y := conflict flag
        beq     count_evictable_voices_exit    // No conflict → skip comparison, exit

        cmp     voice_priority_3               // Compare sound vs. voice #3 priority
        bmi     set_voice3_cmp_lt_flag         // sound_priority < voice3 → set 00

        ldx     #BTRUE                  // sound_priority >= voice3 → set FF
        jmp     store_voice3_priority_is_higher_flag_b

        // ------------------------------------------------------------
        // Handle the "sound_priority < voice_priority_3" case.
        // Sets comparison result to 0. The extra CMP/STA logic evaluates
        // an index (0–2) against a priority value (0–$7F), which yields
        // no meaningful state change—min_alloc_voice_idx is effectively
        // written back unchanged.
        // ------------------------------------------------------------
set_voice3_cmp_lt_flag:
        ldx     #FALSE                            // Prepare comparison result: flag "sound < voice3" as 0
        lda     min_alloc_voice_idx             // A := index (0–2) of lowest-priority allocated voice
        cmp     voice_priority_3                // Compare that index against voice3’s PRIORITY byte (value, not index)
        bpl     store_voice3_priority_is_higher_flag_b         // If index ≥ priority value (normally impossible/irrelevant), just skip next write
        sta     min_alloc_voice_idx             // Otherwise, rewrite the same index back (no net change in practice)

store_voice3_priority_is_higher_flag_b:
        stx     voice3_priority_is_higher_flag_b      			// Store boolean comparison result

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

Arguments
        None

Returns
        None (writes the computed count into total_real_voices_available)

Description
        - Saves A, X, Y on the stack.
        - Loads voices_allocated bitmask (bits 0–2 correspond to real voices).
        - Rotates each bit into carry and increments a counter for each clear bit.
        - Stores the final count into total_real_voices_available.
        - Restores A, X, Y and returns.
================================================================================
*/
* = $4F26
count_available_real_voices:
        // ----------------------------------------------------------------------
        // Recompute total_real_voices_available by counting how many physical
        // real voices are currently free. Tests bits 0..2 of voices_allocated
        // (0 = free, 1 = allocated). Preserves A, X, and Y via the stack.
        // ----------------------------------------------------------------------
        pha                                     // Save A
        txa
        pha                                     // Save X
        tya
        pha                                     // Save Y

        lda     voices_allocated                // A := allocation bitmask
        ldx     #$00                            // X := free-voice counter

        // ------------------------------------------------------------
        // Test bit 0: rotate into carry, increment X if bit is 0
        // ------------------------------------------------------------
        ror                                     // C := bit0, A := shifted
        bcs     test_voice1_allocation_bit                       // bit0=1 → allocated, skip
        inx                                     // bit0=0 → free voice

test_voice1_allocation_bit:
        // ------------------------------------------------------------
        // Test bit 1
        // ------------------------------------------------------------
        ror                                     // C := next bit, A := shifted
        bcs     test_voice2_allocation_bit                       // bit1=1 → allocated, skip
        inx                                     // bit1=0 → free voice

test_voice2_allocation_bit:
        // ------------------------------------------------------------
        // Test bit 2
        // ------------------------------------------------------------
        ror                                     // C := next bit, A := shifted
        bcs     store_free_real_voice_count                      // bit2=1 → allocated, skip
        inx                                     // bit2=0 → free voice

store_free_real_voice_count:
        // ------------------------------------------------------------
        // Store number of free voices and restore registers
        // ------------------------------------------------------------
        stx     total_real_voices_available     // Persist free-voice count

        pla                                     // Restore Y
        tay
        pla                                     // Restore X
        tax
        pla                                     // Restore A
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
        X  → index of the voice to release or repurpose

Returns
        None (A, X, Y restored via deallocate_voice tail)

Global Inputs
        arpeggio_ongoing            Non-zero if any arpeggio is active
        arpeggio_primary_active_flag           Arpeggio-1 activity flag
        arpeggio_secondary_active_flag           Arpeggio-2 activity flag
        voice_priority_0            Priority value for voices 0–2
        voice_priority_3            Priority for voice 3
        stop_sound_cleanup_mode             Sound-stop mode selector (#$01 = full-stop)
        lowest_priority_sound_starting_flag  Non-zero if a new sound of priority 1 is starting
        filter_arpeggio_enabled_flag          Global filter enable flag
        voice3_in_use               Flag indicating voice 3 use status
        voice_sound_id_tbl            Resource IDs for voices 0–2
        voice_instr_active_mask Per-voice instruction-execution mask
        voice_alloc_set_mask_tbl                Per-voice bit masks for OR operations
        voice_ctrl_shadow              Current control bytes for SID voices 0–2

Global Outputs
        voice3_in_use               Cleared when releasing voice 3 with no arpeggio
        voice_instr_active_mask Updated when repurposing a real voice
        voice_ctrl_shadow              GATE bit set when retriggering a voice
        (Additional outputs written by the deallocate_voice tail, including
         priority clearing, allocation mask updates, resource clearing, and
         refcount adjustments.)

Description
        - Saves A, X, Y on the stack.
        - If arpeggio_ongoing is non-zero:
              • For voice 0: disable arpeggio_primary_active_flag and clear alternate settings.
              • Disable arpeggio_secondary_active_flag for all voices.
              • Fully deallocate the voice via dv_clear_voice_resource_id.
        - If no arpeggio:
              • For voice 3 only, clear voice3_in_use.
              • Inspect voice_priority[X]:
                    · If == 1:
                          - In full-stop mode, process arpeggios then deallocate.
                          - Otherwise, clear note for voices 0–2 and deallocate.
                    · If != 1:
                          - Unless lowest_priority_sound_starting_flag and
                            arpeggio_primary_active_flag are both active, deallocate.
                          - Voice index must be < 3 or deallocate.
                          - If filter disabled, or voice3_in_use is zero,
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
        // Release (or repurpose) a voice indexed by X.
        // Saves A,X,Y on entry. All exit paths jump into
        // deallocate_voice / dv_clear_voice_resource_id /
        // dv_dec_sound_refcount_and_return, which restore registers.
        // ------------------------------------------------------------
        pha                                     // Save A
        txa
        pha                                     // Save X
        tya
        pha                                     // Save Y

        // ------------------------------------------------------------
        // If any arpeggio is active, disable arpeggio components
        // and fully deallocate this voice.
        // ------------------------------------------------------------
        lda     arpeggio_ongoing
        beq     rv_no_arpeggio_path                     // No arpeggio → continue normal path

        cpx     #$00                            // Voice 0 has special handling
        bne     rv_disable_arp2_and_free
        stx     arpeggio_primary_active_flag               // Voice 0 → clear arpeggio_primary_active_flag
        jsr     clear_all_alternate_settings    // Clear alternate arpeggio settings

rv_disable_arp2_and_free:
        lda     #FALSE
        sta     arpeggio_secondary_active_flag               // Disable arpeggio #2 globally
        jmp     dv_clear_voice_resource_id        // Fully deallocate (via deallocate_voice tail)

        // ------------------------------------------------------------
        // No arpeggio active. Optional handling for voice 3.
        // ------------------------------------------------------------
rv_no_arpeggio_path:
        cpx     #$03
        bne     rv_check_priority_eq_min
        lda     #FALSE
        sta     voice3_in_use                   // Voice 3 marked not in use

        // ------------------------------------------------------------
        // Branch on voice priority == MIN_SOUND_PRIORITY
        // ------------------------------------------------------------
rv_check_priority_eq_min:
        lda     voice_priority_0,x              // A := priority for this voice
        cmp     #MIN_SOUND_PRIORITY
        bne     rv_priority_ne_min_path           // Priority != MIN_SOUND_PRIORITY → different handling

        // ------------------------------------------------------------
        // Priority == MIN_SOUND_PRIORITY
        // Full stop mode?
        // ------------------------------------------------------------
        lda     stop_sound_cleanup_mode
        cmp     #STOP_SOUND_MODE_VOICES_ONLY
        bne     rv_full_stop_real_voices      // Not voices only → full stop

        // voices only: update arpeggios then fully deallocate
        jsr     process_arpeggio
        jmp     deallocate_voice

        // ------------------------------------------------------------
        // Full stop (priority == MIN_SOUND_PRIORITY)
        // ------------------------------------------------------------
rv_full_stop_real_voices:
        cpx     #$03
        bpl     rv_deallocate_tail            // Voice 3+ → skip explicit stop-note
        jsr     clear_waveform_on_full_stop    // Real voices 0–2: stop the note

rv_deallocate_tail:
        jmp     deallocate_voice                // Fully release via tail routine

        // ------------------------------------------------------------
        // Priority != MIN_SOUND_PRIORITY
        // ------------------------------------------------------------
rv_priority_ne_min_path:
        lda     lowest_priority_sound_starting_flag
        beq     deallocate_voice                // No priority-1 incoming → free voice

        lda     arpeggio_primary_active_flag
        beq     deallocate_voice                // No arpeggio-1 active → free voice

        cpx     #$03
        bpl     deallocate_voice                // Voice index ≥3 → free voice

        // ------------------------------------------------------------
        // At this point:
        //   - priority != 1
        //   - lowest_priority_sound_starting_flag != 0
        //   - arpeggio_primary_active_flag != 0
        //   - voice index is 0–2
        // Deciding whether to repurpose voice or fully release.
        // ------------------------------------------------------------
        lda     filter_arpeggio_enabled_flag
        beq     set_voice_in_use                // Filter disabled → repurpose this voice

        lda     voice3_in_use
        beq     set_voice_in_use                // Filter enabled but voice3 not in use → repurpose

        jmp     deallocate_voice                // Otherwise: fully release

        // ------------------------------------------------------------
        // Repurpose the voice: mark it executing, adjust waveform,
        // retrigger note, then decrement resource refcount once.
        // ------------------------------------------------------------
set_voice_in_use:
        lda     voice_sound_id_tbl,x              // Load current resource index
        pha                                     // Save for later refcount decrement

        lda     voice_instr_active_mask
        ora     voice_alloc_set_mask_tbl,x                  // Mark this voice as executing
        sta     voice_instr_active_mask

        jsr     apply_arpeggio_and_filter_for_voice

        lda     voice_ctrl_shadow,x
        ora     #$01                            // Set GATE bit
        sta     voice_ctrl_shadow,x
        jsr     update_voice_control            // Commit control changes

        pla                                     // Restore resource index
        tax                                     // X := resource id
        ldy     #$00
        jmp     dv_dec_sound_refcount_and_return       // Decrement refcount, restore regs, return
/*
================================================================================
  deallocate_voice
================================================================================
Summary
        Release a voice slot. Clears the voice’s priority, updates the global
        voices-allocated bitmask, clears the associated resource ID, decreases
        the sound resource’s reference count, and updates the count of available
        real voices.

Arguments
        X  → voice index to release

Returns
        None

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
        // ----------------------------------------------------------------------
        // Deallocate a voice.
        //
        // Arguments:
        //     X → voice index to release
        //
        // Also clears the resource associated with the voice, decreases the
        // sound resource reference count, and updates the count of available
        // real voices.
        // ----------------------------------------------------------------------
        lda     #$00
        sta     voice_priority_0,x             // Clear voice priority

        lda     voices_allocated               // Clear voice bit in allocation mask
        and     voice_alloc_clear_mask_tbl,x
        sta     voices_allocated

        jsr     count_available_real_voices    // Recompute number of free real voices

dv_clear_voice_resource_id:
        // ------------------------------------------------------------
        // Detach the sound resource from this voice slot.
        // X, Y on entry: X = voice index, Y = voice index (mirrored)
        // After loading the resource ID into X, clear the voice→resource
        // mapping and keep resource ID in X / Y for refcount update.
        // ------------------------------------------------------------
        txa                                     // A := voice index
        tay                                     // Y := voice index
        ldx     voice_sound_id_tbl,y              // X := resource ID for this voice

        lda     #$00                            // Clear resource ID for this voice
        sta     voice_sound_id_tbl,y

        tay                                     // Y := resource ID (mirrored from X via A)

dv_dec_sound_refcount_and_return:
        // ------------------------------------------------------------
        // Decrease reference count for the sound resource (ID in X/Y),
        // unless it is flagged as "no music" in its attributes.
        // Then restore saved registers and return.
        // ------------------------------------------------------------
        jsr     dec_sound_refcount_if_no_music

        pla                                     // Restore Y
        tay
        pla                                     // Restore X
        tax
        pla                                     // Restore A
        rts
/*
================================================================================
  dec_sound_refcount_if_no_music
================================================================================
Summary
        Decrease the memory-attribute (reference count) of a sound resource,
        but only when music playback is not active. Music mode manages its
        own resource lifetimes and must not have its reference counts altered.

Arguments
        X       Sound resource index whose memory attribute should be decremented

Global Inputs
        music_playback_in_progress    Nonzero when music engine is active
        sound_liveness_tbl               Table of per-sound memory attributes

Global Outputs
        sound_liveness_tbl               Entry X decremented via dec_sound_refcount
                                      (only when music is not playing)

Returns
        None (may early-exit with no change if music playback is active)

Description
        - Checks whether the music playback engine is currently running.
        - If active, exits immediately without modifying the sound’s memory
          attribute; music playback retains full control of its resource usage.
        - If not active, calls dec_sound_refcount—this performs a saturating
          decrement while preserving the original sign bit of the attribute.
        - Used by the voice allocator/release logic to maintain correct
          residency tracking for non-music sound effects.
================================================================================
*/
* = $4FEA
dec_sound_refcount_if_no_music:
		lda     music_playback_in_progress   // If music playback is active, do not touch refcount
		beq     do_decrease                  // Z=1 → no music → safe to decrement
		rts                                  // Music playing → exit with refcount unchanged

do_decrease:
		jsr     dec_sound_refcount           // Saturating decrement of sound_liveness_tbl[X], preserve flags
		rts                                  // Done updating reference count
/*
================================================================================
  increment_sound_memory_attr
================================================================================
Summary
	Increment the memory-attribute (reference count) of a sound resource,
	unless music playback is currently in progress. Music mode protects
	its own resource usage and prevents refcount changes during playback.

Arguments
	X       Sound resource index whose memory attribute is to be incremented

Global Inputs
	music_playback_in_progress    Nonzero when music engine is active
	sound_liveness_tbl               Table of per-sound memory attributes

Global Outputs
	sound_liveness_tbl               Entry X incremented (unless music active)

Returns
	None (may early-exit without updating when music is playing)

Description
	- If music playback is active, the increment is skipped entirely to
	avoid interfering with music-managed resource lifetimes.
	- Otherwise increments sound_liveness_tbl[X], increasing the resource’s
	reference count.
	- This routine pairs with the corresponding decrement routine to
	maintain per-sound residency and usage tracking.
================================================================================
*/
* = $4FF4
inc_sound_refcount:
		lda     music_playback_in_progress   // If music playback is active, do not touch refcount
		beq     do_increase                  // Z=1 → no music → safe to increment
		rts                                  // Music playing → exit with refcount unchanged

do_increase:
		inc     sound_liveness_tbl,x            // Bump memory attribute/refcount for sound X
		rts                                  // Done updating reference count
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
        X → voice index to allocate

Returns
        None (A, X, Y are restored before return)

Global Inputs
        voices_allocated              Bitmask of real-voice allocation (voices 0–2)
        voice_alloc_set_mask_tbl                  Per-voice bitmasks for allocation/update
        sound_priority                Priority value for the sound being assigned
        pending_sound_idx                Resource index of sound whose refcount increases
        voice_priority_0              Table of per-voice priorities
        voice3_in_use                 Usage flag for voice index 3

Global Outputs
        voices_allocated              Updated when X is 0–2
        total_real_voices_available   Updated via count_available_real_voices
        voice3_in_use                 Set to #$FF when X = 3
        voice_priority_0,X            Updated with sound_priority
        (inc_sound_refcount modifies the sound resource table externally)

Description
        - Saves A, X, Y at entry and restores them before returning.
        - Distinguishes three categories of voice index:
              • X = 0–2: real hardware voices
                    - Marks the bit in voices_allocated.
                    - Recomputes total_real_voices_available.
              • X = 3: special voice
                    - Sets voice3_in_use to #$FF.
              • X ≥ 4: extended / non-real indices
                    - No allocation flags are touched.
        - For all indices (0–2, 3, or ≥4):
              • Stores sound_priority into voice_priority_0,X.
              • Calls inc_sound_refcount using pending_sound_idx as the resource ID.
        - No voice→resource mapping is changed here; only allocation bits,
          special-voice flag, priority, and sound refcount are affected.
================================================================================
*/
* = $4FFE
allocate_voice:
        // ----------------------------------------------------------------------
        // Allocate or mark usage for a voice indexed by X.
        //
        // A, X, Y are saved on entry and restored on exit. All effects occur
        // through global tables/flags. Voice indices fall into three categories:
        //
        //   X = 0–2  → real voices: mark allocated and update free-voice count
        //   X = 3    → special voice: mark voice3_in_use
        //   X ≥ 4    → neither real nor voice3: only priority + refcount updated
        //
        // In all cases, the per-voice priority table is updated and the sound
        // resource referenced by pending_sound_idx has its refcount incremented.
        // ----------------------------------------------------------------------
        pha                                     // Save A
        txa
        pha                                     // Save X
        tya
        pha                                     // Save Y

        // ------------------------------------------------------------
        // X == 3 → treat as special "voice 3" and mark as in use.
        // ------------------------------------------------------------
        cpx     #$03
        bne     av_dispatch_non3_index
        lda     #BTRUE
        sta     voice3_in_use                   // Mark voice 3 in use
        jmp     av_set_priority_and_inc_refcount              // Skip real-voice handling

av_dispatch_non3_index:
        // ------------------------------------------------------------
        // X = 0–2 (negative from CPX) → handle as real voice.
        // X ≥ 4 (non-negative) → skip to priority tail.
        // ------------------------------------------------------------
        bpl     av_set_priority_and_inc_refcount              // X ≥ 4 → skip real-voice block

        // ------------------------------------------------------------
        // Real voice (X = 0,1,2): mark allocated and update availability count.
        // ------------------------------------------------------------
        lda     voices_allocated
        ora     voice_alloc_set_mask_tbl,x                  // Mark this voice as allocated
        sta     voices_allocated

        jsr     count_available_real_voices     // Recompute real-voice availability

        // ------------------------------------------------------------
        // Common tail: store priority and increment sound refcount.
        // ------------------------------------------------------------
av_set_priority_and_inc_refcount:
        lda     sound_priority
        sta     voice_priority_0,x              // Store priority for this voice index

        ldy     #$00                            // Y := 0 for refcount routine

        ldx     pending_sound_idx                  // X := sound/resource index
        jsr     inc_sound_refcount              // Boost refcount for pending_sound_idx

        pla                                     // Restore Y
        tay
        pla                                     // Restore X
        tax
        pla                                     // Restore A
        rts
/*
================================================================================
  clear_refcount_of_sounds_1_and_2
================================================================================
Summary
        Reset the reference-count fields for sounds #1 and #2 while preserving
        only bit 7 of each sound’s memory-attribute byte.

Arguments
        None

Returns
        None (A is clobbered)

Description
        - Loads sound_1_memory_attr, masks it with #$80, and writes it back.
        - Loads sound_2_memory_attr, masks it with #$80, and writes it back.
        - Bit 7 is preserved; bits 0–6 (the refcount) are cleared.
================================================================================
*/
* = $513E
clear_refcount_of_sounds_1_and_2:
        // ------------------------------------------------------------
        // Clear the low 7 bits of the reference count for sounds #1 and #2.
        // Preserves bit 7 (e.g., lock/flag) and zeroes bits 0–6.
        // ------------------------------------------------------------
        lda     sound_1_memory_attr
        and     #LOCK_BIT                           // Keep bit 7, clear refcount bits
        sta     sound_1_memory_attr

        lda     sound_2_memory_attr
        and     #LOCK_BIT                           // Keep bit 7, clear refcount bits
        sta     sound_2_memory_attr
        rts
/*
================================================================================
  set_refcount_of_sounds_1_and_2
================================================================================
Summary
        Mark sounds #1 and #2 as having a non-zero reference count by setting
        bit 0 of their memory-attribute bytes.

Arguments
        None

Returns
        None (A is clobbered)

Description
        - Loads sound_1_memory_attr, sets bit 0, and writes it back.
        - Loads sound_2_memory_attr, sets bit 0, and writes it back.
        - Other bits (including bit 7) are preserved.
================================================================================
*/
* = $514F
set_refcount_of_sounds_1_and_2:
        // ------------------------------------------------------------
        // Force the reference count non-zero for sounds #1 and #2 by
        // setting bit 0. Other bits (including bit 7) are preserved.
        // ------------------------------------------------------------
        lda     sound_1_memory_attr
        ora     #SOUND_REFCOUNT_BIT                           // Ensure refcount flag is non-zero
        sta     sound_1_memory_attr

        lda     sound_2_memory_attr
        ora     #SOUND_REFCOUNT_BIT                           // Ensure refcount flag is non-zero
        sta     sound_2_memory_attr
        rts
