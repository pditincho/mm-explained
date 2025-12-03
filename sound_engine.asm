/*
================================================================================
Sound engine — voice scripts, decoding, and IRQ driver
================================================================================

Summary
    This module is the “inner loop” of the audio subsystem. It runs once per
    interrupt, advances all active voice scripts, and pushes the resulting
    control/frequency/filter state into the SID. Sounds are represented as
    compact per-voice instruction streams; this file defines how those streams
    are interpreted and executed.

Core responsibilities
    • IRQ driver
        - Entry point that runs every tick.
        - Starts any pending sounds that the scheduler bound to logical voices.
        - Advances per-voice timers and glissando envelopes.
        - Executes multiplexing slices for voices that are time-shared.
        - Commits final control/filter state for each active logical voice.
        - Hands off to the music engine to run one slice of the current tune.

    • Voice start / stream binding
        - start_sound_for_voice takes a logical voice index and a pending sound
          id, verifies that the sound resource is resident, and binds:
            · voice_data_base[*]   → sound resource base
            · voice_instr_pc[*]    → base + per-voice offset
            · voice_instr_loop_ofs[*] → same offset (for repetition logic)
        - Sets up initial control/filter state depending on voice role:
            · real SID voice (0–2): control + filter-routing may be read unless
              the voice is reserved by music.
            · filter voice (3): filter control + master volume are refreshed.
            · virtual / multiplexed voices (≥4): skip the hardware-facing
              parameters; they are driven via multiplexing.

    • Instruction decoding VM
        - Each voice executes a compact script of “instruction headers”
          followed by optional data blocks. The header’s bits encode:
            · gate control, stop requests
            · frequency / duration / glissando updates
            · ADSR changes (for real SID voices)
            · operand sub-actions (PWM, filter, duration override)
            · waveform selection
            · repetition (backward jump + repeat count)
        - decode_voice_instruction:
            · Validates that the owning sound resource is still resident; if
              not, performs a full cleanup.
            · Sets up a read pointer from the current per-voice PC.
            · Interprets the header bits in a fixed order, consuming only the
              operand bytes implied by the active bits.
            · Updates per-voice cached state (frequency, duration, gliss,
              ADSR, control, filter, PWM) and, where appropriate, immediately
              pushes changes into SID registers.
            · Supports short loops via a backward offset and a per-voice
              repeat counter stored alongside the instruction state.

    • PC and loop-offset maintenance
        - update_instruction_ptr_and_offset keeps two notions in sync:
            · voice_instr_pc[*]       → current “program counter” into the
              stream.
            · voice_instr_loop_ofs[*] → offset of this instruction relative to
              the sound’s base, used when a resource is relocated in memory.
        - After each decode, both values are advanced by exactly the number
          of bytes consumed, so later relocations and repetition jumps can
          rebuild or adjust the PC correctly.

    • State reset
        - reset_sound_engine_state:
            · Silences all SID voices and filter.
            · Clears all logical voices via stop_logical_voice.
            · Resets music playback flags and reference counts for the core
              sound resources.
            · Marks all real voices as available and clears multiplexing flags.
            · Aligns “music in progress” pointers with the currently selected
              music resource.

Multiplexing and filter handling
    • Multiplexing
        - process_multiplexing and swap_voice_settings coordinate a pair of
          alternate state blocks that snapshot per-voice fields (PC, frequency,
          duration, gliss, ADSR, control, base, loop offset, etc.).
        - The IRQ driver temporarily swaps these snapshots into real SID
          voices during a multiplex slice, runs the normal “tick and decode”
          path for those snapshots, then swaps the base state back.

    • Filter routing
        - A dedicated “filter voice” holds the state that drives the filter
          cutoff and mode bits.
        - apply_multiplexing_and_filter_for_voice combines:
            · the primary voice’s multiplexed state,
            · the filter voice snapshot, and
            · the saved filter mode mask
          to route the filter to the correct logical/physical voice while
          keeping the master volume/filter register and its shadow coherent.

Music integration
    • The IRQ driver also arbitrates music playback:
        - Tracks which music resource should be running via
          music_to_start_ptr_* and music_in_progress_ptr_*.
        - When the selection changes, setup_music_pointers updates the
          trampoline used by jump_to_music_code so future ticks enter the
          correct music routine.
        - On each tick with music enabled, the IRQ driver calls the trampoline
          to execute one slice of the active music program.

================================================================================
  Voice instruction bit layout
================================================================================

The instruction byte encodes multiple independent actions. A copy is shifted
right in instr_bit_cursor to expose bits 0–5 and 7 in carry, while the
unmodified value in instr_header_raw is used to test bit 6 via BIT.

* Bit 0 (gate):

	* Checked first (via LSR → C).
	* If is_physical_voice_flag_bff has bit 6 set (V=1), carry controls gate:
		* C=1 → set bit 0 of voice_ctrl_shadow (trigger note).
		* C=0 → clear bit 0 of voice_ctrl_shadow (release note).
	* If V=0, gate handling is skipped (no SID-voice update for this slot).

* Bit 1 (stop voice):

	* Next LSR moves bit 1 into C.
	* If C=1 → sets stop_sound_cleanup_mode to #$FF and calls
	stop_logical_voice for this logical voice, then returns.
	* This is an immediate “stop this voice” instruction.

* Bit 2 (frequency/duration/glissando, together with bit 6):

	* Next LSR moves bit 2 into C; BIT on instr_header_raw sets V to bit 6.
	* If C=0 (bit 2 clear) → call reset_voice_duration_and_glissando and skip
	operand reads; frequency is unchanged.
	* If C=1 (bit 2 set) → always read 2 bytes and update frequency:
		* If V=1 (bit 6 set) → clear duration and glissando, do not read them.
		* If V=0 (bit 6 clear) → read 2 bytes for duration and 2 bytes for
		  glissando, updating both.

* Bit 3 (ADSR, gated by SID-range flag):

	* BIT is_physical_voice_flag_bff sets V from its bit 6, then another LSR moves
	bit 3 into C.
	* If V=0 or C=0 → ADSR handling is skipped.
	* If V=1 and C=1:
		* Temporarily force gate off (clear bit 0 of voice_ctrl_shadow and call
		  apply_voice_control_to_sid).
		* Read 2 bytes to update ADSR (attack/decay and sustain/release).
		* Re-apply the original gate state from voice_instr_header bit 0
		  (re-trigger or re-release with the new ADSR).

* Bit 4 (extended operand sub-commands: PWM, filter, duration):

	* If bit 4 set, the next byte is loaded into instr_op_flags; its low
	three bits encode sub-actions:
	* Operand bit 0 (PWM; only when SID-range flag V=1):
		* If set → read 2 bytes and update this voice’s PWM via
		voice_pwm_reg_ofs_tbl.
	* Operand bit 1 (filter cutoff):
		* If set → call update_filter_and_volume, then read 2 bytes into
	filter cutoff registers.
	* Operand bit 2 (duration):
		* If set → clear glissando and read 2 bytes to set a new duration.

            If set, the next operand byte carries waveform bits in its high
            nibble. The low nibble is ORed into the existing control byte’s low
            nibble (it can only add bits, not clear them), so valid operands
            should normally have low 4 bits = 0 to avoid toggling gate/flags.

* Bit 5 (waveform):

	* If set, the next operand byte carries waveform bits in its high
	nibble. The low nibble is ORed into the existing control byte’s low
	nibble (it can only add bits, not clear them), so valid operands
	should normally have low 4 bits = 0 to avoid toggling gate/flags.

* Bit 6 (mode modifier for bit 2):

	* Not consumed via instr_bit_cursor for control flow; instead tested
	via BIT on instr_header_raw to decide whether the Bit-2 section reads
	duration/glissando (bit 6 clear) or just clears them (bit 6 set) after
	updating frequency.

* Bit 7 (repetition and backward offset):

	* After processing bits 0–5, instr_bit_cursor is shifted two more times;
	the second LSR’s carry contains bit 7.
	* If bit 7 clear → no repetition; the routine calls
	update_instruction_ptr_and_offset and returns.
	* If bit 7 set → repetition loop:
		* On each pass, read an 8-bit offset and subtract it from both
		  voice_instr_ptr and voice_instr_offset, moving the instruction
		  pointer backwards.
		* If voice_instr_repcount is zero, read a new count from the stream.
		  Otherwise, just decrement the existing count.
		* When the count reaches #$01, the last pass decrements it to #$00, skips
		  over the stored offset and count, advances pointers past this instruction
		  via update_instruction_ptr_and_offset, and then continues with the next
		  instruction.

In summary, the byte is a packed bitfield: bits 0–5 and 6 control local voice
actions (gate, frequency, timing, ADSR, PWM/filter/duration, waveform), while
bit 7 + trailing operands implement a small backward-loop mechanism with an
explicit repeat counter and offset.

================================================================================

Bit 0  – Gate on/off:
          If SID-range flag (V=1): C=1→trigger, C=0→release. Else ignored.

Bit 1  – Stop voice:
          Immediately stops the logical voice and returns.

Bit 2  – Frequency/duration/glissando (uses bit 6 as modifier):
          0 → clear duration & glissando.
          1 → read 2 bytes: set frequency.
                If bit 6=1 → clear duration & glissando.
                If bit 6=0 → read 4 more bytes: set duration & glissando.

Bit 3  – ADSR (only when SID-range flag V=1):
          Read 2 bytes (ADSR).
          Gate is forced off, ADSR updated, then original gate action reapplied.

Bit 4  – Operand sub-actions (next byte = operand bits):
          Operand bit 0 (and V=1): set PWM (read 2 bytes).
          Operand bit 1: update filter/volume and cutoff (read 4 bytes total).
          Operand bit 2: set duration (clear glissando, read 2 bytes).


Bit 5  – Waveform:
          Next byte’s high nibble replaces waveform bits.

Bit 6  – Modifier for Bit 2:
          0 → enable duration+glissando reads.
          1 → suppress duration/glissando (clear them).

Bit 7  – Repetition:
          Read offset; subtract from instruction pointer each pass.
          First pass loads repeat count; later passes decrement it.
          Last pass clears counter, skips operands, and advances past instruction.

================================================================================

Voice instruction encoding: byte + operand layouts

Byte 0: instruction header
--------------------------------
  b7  b6  b5  b4  b3  b2  b1  b0
  |   |   |   |   |   |   |   +-- Gate: 1=trigger, 0=release (SID-range only)
  |   |   |   |   |   |   +------ Stop voice: 1=stop+RTS
  |   |   |   |   |   +---------- F/D/G block (see b6)
  |   |   |   |   +-------------- ADSR block (SID-range only)
  |   |   |   +------------------ Operand sub-block (PWM/filter/duration)
  |   |   +---------------------- Waveform update
  |   +-------------------------- Modifier for F/D/G (b2)
  +------------------------------ Repetition (offset + count present)

Special case
------------
  INS = $00:
    - Layout: [00]
    - No operands. Clears repeat counter and cuts voice.

General operand stream layout (non-zero instruction)
----------------------------------------------------
After the instruction byte come zero or more operand blocks in this fixed order:

  [F-block] [A-block] [O-block] [W-block] [R-block]

Each block’s presence and size:

1) F-block (Frequency / Duration / Glissando) - controlled by b2,b6
   - Present only if b2=1.
   - Layout:

     If b2=0:
       F-block absent, 0 bytes (duration+gliss cleared if b2=0).

     If b2=1 and b6=1:
       F-block = [freq_lo] [freq_hi]                 (2 bytes)

     If b2=1 and b6=0:
       F-block = [freq_lo] [freq_hi]                 (2 bytes)
                 [dur_lo]  [dur_hi]                  (2 bytes)
                 [glis_lo] [glis_hi]                 (2 bytes)
                 → 6 bytes total

2) A-block (ADSR envelope) - controlled by b3 and SID-range flag
   - Present only if b3=1 AND this voice is in SID range (V=1 from
     is_physical_voice_flag_bff).
   - Layout:

     A-block = [adsr_attack_decay] [adsr_sustain_release]   (2 bytes)

3) O-block (Operand sub-actions: PWM / filter / duration) - controlled by b4
   - Present only if b4=1.
   - First, a 1-byte operand bitfield:

     O-block header:
       [op_bits]
         bit 0 → PWM update (SID-range only)
         bit 1 – Filter cutoff + filter/volume pair
            If set, the O-block contains four bytes in this order:
                [filter_control, master_volume, cutoff_lo, cutoff_hi].
            The first two bytes are consumed by update_filter_and_volume,
            which merges filter_control into sid_filter_control_shadow and
            updates sid_volfilt_shadow/sid_master_volume. The next two
            bytes are written directly into the SID filter cutoff registers.
		 
         bit 2 → Duration update (clear gliss, set duration)

    - Then, depending on op_bits:

      If bit 0=1 and SID-range V=1:
        + [pwm_lo] [pwm_hi]                          (2 bytes)

      If bit 1=1:
        + [filter_control] [master_volume]           (2 bytes)
        + [cutoff_lo] [cutoff_hi]                    (2 bytes)
        → 4 bytes total

      If bit 2=1:
        + [dur2_lo] [dur2_hi]                        (2 bytes, clears gliss)

    - So O-block layouts:

      b4=0:
        O-block absent, 0 bytes.

      b4=1:
        Minimum (op_bits=00000000):
          [op_bits]                                  (1 byte)

        Maximum (op_bits bits 0,1,2 all set, SID-range):
          [op_bits]
          [pwm_lo] [pwm_hi]
          [filter_control] [master_volume] [cutoff_lo] [cutoff_hi]
          [dur2_lo] [dur2_hi]
          (1 + 8 = 9 bytes)

4) W-block (Waveform) - controlled by b5
   - Present only if b5=1.
   - Layout:

     W-block = [waveform_mask]                    (1 byte)
       (high nibble replaces waveform bits, low nibble of control preserved)

5) R-block (Repetition) - controlled by b7
   - Present only if b7=1.
   - Layout (always 2 bytes at the very end):

     R-block = [offset] [repeat_count]            (2 bytes)
       offset       → 8-bit backwards delta applied to instruction pointer
       repeat_count → initial or updated repeat counter

   - On first use for this instruction, repeat_count is loaded from the
     stream; subsequent passes just decrement it. When it reaches #$01, the
     final pass decrements to #$00 and then skips these 2 bytes.

Example layouts
---------------
Let INS be a non-zero instruction byte:

  Case A: Only repetition (b7=1, all others 0)
    Layout: [INS]
            [offset] [repeat_count]

  Case B: F only (b2=1,b6=1; all others 0)
    Layout: [INS]
            [freq_lo] [freq_hi]

  Case C: F + full D/G (b2=1,b6=0; all others 0)
    Layout: [INS]
            [freq_lo] [freq_hi]
            [dur_lo]  [dur_hi]
            [glis_lo] [glis_hi]

  Case D: F + ADSR + waveform + repetition (SID-range, b2=1,b6=0,b3=1,b5=1,b7=1)
    Layout: [INS]
            [freq_lo] [freq_hi]
            [dur_lo]  [dur_hi]
            [glis_lo] [glis_hi]
            [adsr_AD] [adsr_SR]
            [waveform_mask]
            [offset]  [repeat_count]

  Case E: F + O-block (all sub-bits set) + waveform + repetition
          (SID-range, b2=1,b6=0,b4=1, op_bits bits 0..2=1, b5=1,b7=1)
    Layout: [INS]
            [freq_lo]   [freq_hi]
            [dur_lo]    [dur_hi]
            [glis_lo]   [glis_hi]
            [op_bits]
            [pwm_lo]    [pwm_hi]
            [filter_lo] [filter_hi]
            [dur2_lo]   [dur2_hi]
            [waveform_mask]
            [offset]    [repeat_count]

Maximum-length instruction (SID-range)
--------------------------------------
  b2=1, b6=0, b3=1, b4=1 (op_bits bits 0,1,2=1), b5=1, b7=1:

  Operand bytes:
    F-block : 6
    A-block : 2
    O-block : 1 + 8
    W-block : 1
    R-block : 2
    → Total = 6 + 2 + 9 + 1 + 2 = 20 operand bytes

  Full instruction layout:
    [INS] + 20 operand bytes  → 21 bytes total in the stream.

================================================================================
*/

#importonce

#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "sound_constants.inc"
#import "sid_voice_controller.asm"
#import "voice_scheduler.asm"


.label voice_instr_saved          = $4813    // Saved Y during ADSR handling for the current instruction
.label logical_voice_idx          = $480d    // Logical voice index currently being decoded (mirrors X at entry)
.label instr_bit_cursor           = $480e    // Shifting copy of the instruction header used for per-bit tests
.label instr_op_flags             = $480f    // Operand sub-action flags (PWM/filter/duration) from bit-4 block
.label instr_header_raw           = $4810    // Raw, unshifted instruction header for BIT tests (e.g., bit 6)
.label voice_being_modified = $5165

.const ALT_VOICE_BLOCK_LAST_OFFSET  = $3C    // Highest byte offset in alternate-voice state block (0..$3C = 61 bytes)
.const NO_SOUND_PENDING				= $FF
.const DEFAULT_VOLUME_AND_FILTER = $0F

/*
================================================================================
  sound_irq_handler
================================================================================
Summary
	Per-frame sound engine IRQ entry point. Starts pending sounds on
	logical voices, advances per-voice duration/glissando (including
	multiplexed slices), updates SID voice control/filter routing, and
	runs a slice of the music engine when active.

Global Inputs
	sound_processing_disabled_flag 			Non-zero to suspend all sound processing for this tick
	new_sound_instructions_allowed 			Non-zero → start any pending sounds	this tick
	snd_to_start_on_voice[] 				Per-voice queue of sounds pending start
	voice_instr_active_mask 				Bitmask of voices currently executing instruction streams
	vmux_primary_active_flag_bff 			Primary multiplexing activity flag
	vmux_secondary_active_flag_bff 			Secondary multiplexing activity flag
	music_playback_in_progress 				Non-zero when the music engine should be called this tick
	selected_music_idx 						Sound index of the currently selected music resource
	sound_ptr_lo/hi_tbl[] 					Per-sound base addresses
	music_in_progress_ptr_lo/hi 			Current music code entry pointer

Global Outputs
	new_sound_instructions_allowed 			Cleared to FALSE after processing pending sound starts
	vmux_active_flag_bff 					Set while multiplexing slices are being processed; cleared afterwards
	music_to_start_ptr_lo/hi				Published music entry pointer for the slice to run this tick

Description
        - Early-out when sound_processing_disabled_flag is TRUE so
          start/stop logic can run atomically outside the IRQ driver.

        - When new_sound_instructions_allowed is TRUE:
            · Scans all logical voices for active entries in
              snd_to_start_on_voice[].
            · For each entry found, calls start_sound_for_voice to bind the
              sound to that logical voice.
            · Clears new_sound_instructions_allowed so no further starts occur
              until the flag is raised again.

        - If no voices are active, exits the IRQ early with no further work.

        - For each active voice:
            · Calls tick_duration_and_glissando to advance 16-bit duration
              counters and apply glissando or decode the next instruction when
              a duration expires.

        - If vmux_primary_active is TRUE:
            · Performs a primary multiplexing slice on voice 0 using alternate
              slot 0: swap_voice_settings, tick_duration_and_glissando, then
              swap back.
            · If vmux_secondary_activ is also TRUE, performs a secondary multiplexing 
			  slice on voice 4 using alternate slot 1 in the same pattern.
            · Clears vmux_active_flag at the end of the multiplexing pass.

        - For each active voice, calls apply_voice_control_to_sid so GATE,
          waveform, and filter-routing control bytes are committed to SID for
          this tick.

        - If music_playback_in_progress is TRUE:
            · Publishes the base pointer of the selected music resource into
              music_to_start_ptr_{lo,hi}.
            · If this pointer differs from music_in_progress_ptr_{lo,hi}, calls
              setup_music_pointers to refresh the internal music entry pointer
              and patch the jump_to_music_code trampoline, then calls
              jump_to_music_code to start the new music program.
            · If the pointers already match (same music as the previous tick),
              calls jump_to_music_code directly to run the next slice of the
              current music without reinitializing pointers.

================================================================================
*/
* = $481B
sound_irq_handler:
		// Sound processing disabled? If so, exit
        lda     sound_processing_disabled_flag
        beq     sound_irq_dispatch
        rts

sound_irq_dispatch:
        // ------------------------------------------------------------
		// New sounds allowed? If not, skip this section
        // ------------------------------------------------------------
        lda     new_sound_instructions_allowed
        beq     bail_if_no_active_voices

        // ------------------------------------------------------------
		// New sounds allowed - check if there's any pending sound to start
        // ------------------------------------------------------------
        ldx     #MAX_VOICE_INDEX
scan_pending_sound_starts:
		// Is there a sound pending to start for this voice?
        lda     snd_to_start_on_voice,x
        cmp     #NO_SOUND_PENDING
        beq     next_voice_pending_start
		
		// There is, start it on this voice
        jsr     start_sound_for_voice
next_voice_pending_start:
		// Next voice
        dex
        bpl     scan_pending_sound_starts

        // ------------------------------------------------------------
        // Clear “allow new sound instructions” until next tick
        // ------------------------------------------------------------
        lda     #FALSE
        sta     new_sound_instructions_allowed

bail_if_no_active_voices:
        // ------------------------------------------------------------
		// Any voices actively executing instructions? If not, exit
        // ------------------------------------------------------------
        lda     voice_instr_active_mask
        bne     update_voices_durations
        rts

update_voices_durations:
        // ------------------------------------------------------------
        // Tick duration and glissando for each active voice
        // ------------------------------------------------------------
        ldx     #MAX_VOICE_INDEX
scan_active_voices_for_duration:
		// Voice active? If not, skip it
        lda     voice_instr_active_mask
        and     voice_alloc_set_mask_tbl,x
        beq     next_voice_for_duration
		
		// Voice active, tick duration and glissando
        jsr     tick_duration_and_glissando
        ldx     x_saved
next_voice_for_duration:
		// Next voice
        dex
        bpl     scan_active_voices_for_duration

        // ------------------------------------------------------------
        // Multiplex processing (primary + optional secondary)
        // ------------------------------------------------------------
		// Multiplex active? If not, skip it
        lda     vmux_primary_active_flag_bff
        beq     apply_voice_control_to_sid_for_active_voices

        // ---- Primary multiplexing (voice 0, alt slot 0)
		// Swap voice 0 with alt slot 0
        ldx     #$00
        ldy     #$00
        jsr     swap_voice_settings
		
		// Mark voice multiplexing active
        lda     #TRUE
        sta     vmux_active_flag_bff
		
		// Tick duration and glissando of voice 0
        ldx     #$00
        jsr     tick_duration_and_glissando
		
		// Swap voices again
        ldx     #$00
        ldy     #$00
        jsr     swap_voice_settings

        // ---- Secondary multiplexing (voice 4, alt slot 1)
		// Secondary multiplexing active? If not, skip this section
        lda     vmux_secondary_active_flag_bff
        beq     end_multiplexing_and_reenable_freq

		// Swap voice 4 with alt slot 1
        ldx     #$04
        ldy     #$01
        jsr     swap_voice_settings
				
		// Tick duration and glissando of voice 4
        ldx     #$04
        jsr     tick_duration_and_glissando
		
		// Swap voices again
        ldx     #$04
        ldy     #$01
        jsr     swap_voice_settings

end_multiplexing_and_reenable_freq:
        // ------------------------------------------------------------
        // Clear multiplexing active for this tick
        // ------------------------------------------------------------
        lda     #FALSE
        sta     vmux_active_flag_bff

apply_voice_control_to_sid_for_active_voices:
        // ------------------------------------------------------------
        // Update voice control + filter routing for active voices
        // ------------------------------------------------------------
        ldx     #MAX_VOICE_INDEX
scan_active_voices_for_control:
        stx     x_saved
		
		// Voice active? If not, skip it
        lda     voice_instr_active_mask
        and     voice_alloc_set_mask_tbl,x
        beq     next_voice_for_control
		
		// Voice active, apply voice control
        jsr     apply_voice_control_to_sid
next_voice_for_control:
		// Next voice
        ldx     x_saved
        dex
        bpl     scan_active_voices_for_control

        // ------------------------------------------------------------
        // Music engine: if active, publish pointer and run a music slice
        // ------------------------------------------------------------
        // Music active? If not, exit
        lda     music_playback_in_progress
        beq     sound_irq_exit

        // Publish base pointer of selected music resource into
        // music_to_start_ptr_{lo,hi}
        ldx     selected_music_idx
        lda     sound_ptr_hi_tbl,x
        sta     music_to_start_ptr_hi
        lda     sound_ptr_lo_tbl,x
        sta     music_to_start_ptr_lo

        // Has the selected music resource changed since last tick?
        // If so, reinitialize music pointers and then run the new program.
        lda     music_to_start_ptr_hi
        cmp     music_in_progress_ptr_hi
        bne     dispatch_music_playback
        lda     music_to_start_ptr_lo
        cmp     music_in_progress_ptr_lo
        bne     dispatch_music_playback

        // Same music as previous tick: run one slice at the current PC
        jsr     jump_to_music_code
        jmp     sound_irq_exit

dispatch_music_playback:
        // New music selected: refresh internal pointers, then run initial slice
        jsr     setup_music_pointers
        jsr     jump_to_music_code


sound_irq_exit:
        rts
/*
================================================================================
  jump_to_music_code
================================================================================
Summary
        Indirect trampoline used to call the currently active music routine.
        The JSR target operand is patched at runtime whenever the music
        resource changes.

Global Inputs
        (Patched JSR operand at $48D5/$48D6)
                Contains the dynamically installed entry address of the current
                music routine.

Description
        - Performs a JSR through an operand that is rewritten at runtime to
          point at the active music engine routine.
        - After that target routine returns, a final RTS returns control to the
          original caller of jump_to_music_code.
================================================================================
*/
* = $48D4
jump_to_music_code:
        jsr     $0000                  // Patched at runtime to real entry
        rts
/*
================================================================================
  reset_sound_engine_state
================================================================================
Summary
	Reset SID hardware and all core sound-engine state to a clean baseline:
	silence voices, restore default volume/filter, clear music/multiplexing
	state, and reinitialize music pointers.

Global Inputs
	music_to_start_ptr_lo/hi 			Pending music engine entry pointer

Global Outputs
	voice1/2/3_control_register 		Cleared (voices off)
	sid_filter_control 					Cleared (no filter routing/modes)
	filter_cutoff_freq_lo/hi_reg 		Cleared
	sid_master_volume 					Set to DEFAULT_VOLUME_AND_FILTER
	sid_volfilt_shadow 					Shadow updated to DEFAULT_VOLUME_AND_FILTER
	music_playback_in_progress 			Set to FALSE
	music_voices_in_use 				Cleared (no real voices reserved by music)
	music_voices_in_use_2 				Cleared (auxiliary music voice mask)
	total_real_voices_available 		Set to 3 (all SID voices available)
	vmux_primary_active_flag_bff 		Set to FALSE
	vmux_secondary_active_flag_bff 		Set to FALSE
	vmux_filter_enabled_flag_bff 		Set to FALSE
	music_in_progress_ptr_lo/hi			Initialized from music_to_start_ptr_lo/hi

Description
        - Turns off all three SID voices, disables filter routing, and zeroes
          the filter cutoff registers so the hardware begins in a silent state.

        - Writes DEFAULT_VOLUME_AND_FILTER to the SID vol/filt register and its
          shadow, restoring maximum volume with no filter modes enabled.

        - Calls setup_music_pointers so the internal music-resource pointers are
          refreshed before any playback restarts.

        - Stops all logical voices by repeatedly calling
          stop_logical_voice, ensuring no instruction streams remain active.

        - Clears music_playback_in_progress and calls
          clear_refcount_of_sounds_1_and_2 so music-related refcounts and
          activity flags are reset.

        - Clears music_voices_in_use and music_voices_in_use_2, then sets
          total_real_voices_available to 3, declaring all three SID voices free
          for allocation.

        - Clears all multiplexing and filter-multiplexing flags (primary,
          secondary, and filter) plus the unnamed engine state byte at $5163 so
          the next IRQ tick starts from a clean, non-multiplexed baseline.

        - Copies music_to_start_ptr into music_in_progress_ptr so the music
          engine will begin execution from the currently selected music
          resource on its next invocation.
================================================================================
*/
* = $48D8
reset_sound_engine_state:
        // ------------------------------------------------------------
        // Reset SID hardware and software sound state:
        //   - Turn off all 3 SID voices and disable filter/cutoff
        //   - Set master volume to max (no filter modes enabled)
        //   - Initialize music pointer state
        //   - Stop all logical voices (0–6)
        //   - Clear music/multiplexing/multiplex flags and counters
        //   - Set music_in_progress_ptr := music_to_start_ptr
        // ------------------------------------------------------------
        lda     #$00
        sta     voice1_control_register
        sta     voice2_control_register
        sta     voice3_control_register
        sta     sid_filter_control
        sta     filter_cutoff_freq_lo_reg
        sta     filter_cutoff_freq_hi_reg

        // ------------------------------------------------------------
        // Max volume, no filter modes; update shadow copy too
        // ------------------------------------------------------------
        lda     #DEFAULT_VOLUME_AND_FILTER
        sta     sid_master_volume
        sta     sid_volfilt_shadow

        // ------------------------------------------------------------
        // Prepare internal pointers used by the music system
        // ------------------------------------------------------------
        jsr     setup_music_pointers

reset_logical_voices_and_state:
        // ------------------------------------------------------------
        // Stop all logical voices by calling stop_logical_voice(A)
        // ------------------------------------------------------------
        ldx     #MAX_VOICE_INDEX
stop_all_logical_voices_loop:
        txa
        jsr     stop_logical_voice
        tax
        dex
        bpl     stop_all_logical_voices_loop

        // ------------------------------------------------------------
        // Reset core music- and sound-engine state variables
        // ------------------------------------------------------------
        lda     #FALSE
        sta     music_playback_in_progress
        jsr     clear_refcount_of_sounds_1_and_2

        lda     #$00
        sta     music_voices_in_use_2
        lda     #$00
        sta     music_voices_in_use

        // ------------------------------------------------------------
        // Declare 3 SID hardware voices available
        // ------------------------------------------------------------
        lda     #$03
        sta     total_real_voices_available

        // ------------------------------------------------------------
        // Clear multiplexing/filter-multiplexing/multiplex flags
        // ------------------------------------------------------------
        lda     #FALSE
        sta     vmux_primary_active_flag_bff
        lda     #FALSE
        sta     vmux_filter_enabled_flag_bff
        lda     #FALSE
        sta     vmux_secondary_active_flag_bff
        lda     #$00
        sta     $5163                //; (unnamed engine state byte)

        // ------------------------------------------------------------
        // Initialize music_in_progress_ptr from music_to_start_ptr
        // ------------------------------------------------------------
        lda     music_to_start_ptr_lo
        sta     music_in_progress_ptr_lo
        lda     music_to_start_ptr_hi
        sta     music_in_progress_ptr_hi

        rts


/*
================================================================================
  start_sound_for_voice
================================================================================
Summary
	Bind a pending sound to a logical voice slot, initialize its per-voice
	instruction state (PC, loop offset, control/filter flags), and decode
	the first instruction, returning an error code if the sound resource is
	not resident.

Arguments
	X Logical voice index to start the sound on
	A, Y On entry, contain a copy of the pending sound ID for this voice;
	the authoritative ID is taken from snd_to_start_on_voice[X]

Global Inputs
	snd_to_start_on_voice 			Per-voice pending sound IDs
	voice_data_offsets_lo/hi 		Per-voice instruction entry offsets
	tmp_voice_sound_base_lo/hi 		Temporary base address resolved by update_voice_stream_pointers
	music_voices_in_use 			Bitmask of voices reserved by music
	sid_filter_control_shadow 		Shadow of SID filter-control register

Global Outputs
	voice_sound_id_tbl 				Updated with the started sound ID for X
	snd_to_start_on_voice 			Cleared to NO_SOUND_PENDING for X
	voice_instr_repcount 			Cleared for X (no pending repetitions)
	is_physical_voice_flag_bff 		Set TRUE for X < FILTER_VOICE_INDEX, FALSE otherwise
	voice_instr_loop_ofs_lo/hi 		Initialized from voice_data_offsets_[X]
	voice_data_base_lo/hi 			Set from resolved tmp_voice_sound_base_
	voice_base_addr_lo/hi 			Cached base address for this voice
	voice_instr_pc_lo/hi 			Instruction PC initialized to base + loop_offset
	voice_read_ptr 					Pointer to current parameter/instruction stream for this voice
	sid_filter_control_shadow 		Updated to include or clear this voice’s filter bit 
									based on resource flags
	sid_filter_control 				Hardware filter-control register updated from shadow
	voice_ctrl_shadow 				Updated with initial control byte 
	voice_instr_active_mask 		Bit for X set to mark voice as active

Returns
	A 			$00 				on success (first instruction decoded and voice active)
				$01 				if the backing sound resource is not resident 

Description
        - Copies the pending sound ID from snd_to_start_on_voice[X] into
          voice_sound_id_tbl[X] and clears the pending slot back to
          NO_SOUND_PENDING.

        - Clears voice_instr_repcount[X] so the new instruction stream starts
          with no active repetition state.

        - Sets is_physical_voice according to X:
            · TRUE when X < FILTER_VOICE_INDEX (real SID voice),
            · FALSE for filter and virtual/multiplexed voices.

        - Stores X into logical_voice_idx, loads the per-voice instruction
          loop offset from voice_data_offsets_[X] into voice_instr_loop_ofs_[X],
          and calls update_voice_stream_pointers to resolve the current resource
          base address.

        - Copies tmp_voice_sound_base_* into voice_data_base_*[X], then computes
          voice_instr_pc_lo = base_lo + loop_ofs_lo and sets voice_read_ptr to
          that low-byte address.

        - If voice_data_base_hi[X] is zero after resolution, treats the sound as
          not resident:
            · Calls stop_sound_full_cleanup (using X as index),
            · Returns immediately with A = $01.

        - Otherwise finalizes the high-byte setup by:
            · Writing base_hi into voice_base_addr_hi[X],
            · Adding loop_ofs_hi to form voice_instr_pc_hi[X],
            · Mirroring the result into the high byte of voice_read_ptr.

        - Classifies the voice by index:
            · X >= FIRST_MULTIPLEXED_VOICE_INDEX:
                · Treat as virtual voice, leave Y = $FF so no initial
                  control/filter bytes are read, and jump to the PC/offset
                  advance step.
            · X == FILTER_VOICE_INDEX (filter voice):
                · Sets Y = 0 and calls update_filter_and_volume to pull
                  filter/volume parameters from the stream, then advances
                  PC/offset.
            · 0 <= X < FILTER_VOICE_INDEX (real SID voice):
                · If the voice bit is set in music_voices_in_use, skips initial
                  control/filter reads and advances PC/offset.
                · Otherwise reads two bytes from (voice_read_ptr),Y:
                    · byte 0 → voice_ctrl_shadow[X],
                    · byte 1 → filter-enable nibble for this voice.
                · Updates sid_filter_control_shadow (and thus
                  sid_filter_control) by OR-ing this voice’s bit when the low
                  nibble is non-zero (enable filter) or AND-ing with
                  voice_alloc_clear_mask_tbl[X] when zero (disable filter).

        - Calls update_instruction_ptr_and_offset so both PC and stored loop
          offset advance past any control/filter parameter bytes consumed.

        - Marks the voice active by OR-ing its bit into voice_instr_active_mask
          and calls decode_voice_instruction so the first instruction header
          and its payload are interpreted before returning to the caller.
================================================================================
*/
* = $4939
start_sound_for_voice:
        tay

        // ------------------------------------------------------------
        // Copy pending sound ID → live table; clear pending slot
        // ------------------------------------------------------------
        lda     snd_to_start_on_voice,x
        sta     voice_sound_id_tbl,x
        lda     #NO_SOUND_PENDING
        sta     snd_to_start_on_voice,x

        // ------------------------------------------------------------
        // Reset voice instruction repeat counter
        // ------------------------------------------------------------
        lda     #$00
        sta     voice_instr_repcount,x

        // ------------------------------------------------------------
        // Set real voice flag based on voice index (<3 → TRUE, >=3 → FALSE)
        // ------------------------------------------------------------
        lda     #BTRUE
        cpx     #FILTER_VOICE_INDEX
        bmi     set_is_physical_voice_flag_bff
        lda     #FALSE
set_is_physical_voice_flag_bff:
        sta     is_physical_voice_flag_bff

        // ------------------------------------------------------------
        // Set active voice and initialize its instruction offset
        // ------------------------------------------------------------
        stx     logical_voice_idx 
        lda     voice_data_offsets_hi,x
        sta     voice_instr_loop_ofs_hi,x
        lda     voice_data_offsets_lo,x
        sta     voice_instr_loop_ofs_lo,x

        // ------------------------------------------------------------
        // Resolve resource base and update instruction pointer
        // ------------------------------------------------------------
        jsr     update_voice_stream_pointers

        // ------------------------------------------------------------
        // Set voice_data_base[X] from resolved voice_base
        // ------------------------------------------------------------
        lda     tmp_voice_sound_base_lo
        sta     voice_data_base_lo,x
        lda     tmp_voice_sound_base_hi
        sta     voice_data_base_hi,x

        // ------------------------------------------------------------
        // Compute PC low byte and read-ptr low byte
        // ------------------------------------------------------------
        lda     voice_data_base_lo,x
        sta     voice_base_addr_lo,x
        clc
        adc     voice_instr_loop_ofs_lo,x
        sta     voice_instr_pc_lo,x
        sta     voice_read_ptr

        // ------------------------------------------------------------
        // If high byte of base is zero → resource not in memory
        // ------------------------------------------------------------
        lda     voice_data_base_hi,x
        bne     set_hi_bytes

		// Resource not in memory - stop and do a full cleanup
        txa
        jsr     stop_sound_full_cleanup
        lda     #$01
        rts

set_hi_bytes:
        // ------------------------------------------------------------
        // Finalize PC and read-ptr high bytes
        // ------------------------------------------------------------
        sta     voice_base_addr_hi,x
        adc     voice_instr_loop_ofs_hi,x
        sta     voice_instr_pc_hi,x
        sta     voice_read_ptr + 1

        // ------------------------------------------------------------
        // Determine parameter-byte count based on X
        //   X >= 4 → Virtual voice - Y=#$ff (skip control logic)
        //   X == 3 → Filter voice - do filter/volume setup
        //   X <  3 → Real voice - voice-control + filter-bit update
        // ------------------------------------------------------------
        ldy     #$FF
        cpx     #FIRST_MULTIPLEXED_VOICE_INDEX
        bpl     advance_voice_instr_pointers       // X >= 4

		// X < 4
        iny                         			   // Y := 0

        cpx     #FILTER_VOICE_INDEX
        bne     check_voice_conflict

        // ------------------------------------------------------------
        // Filter Voice: update volume and filter parameters
        // ------------------------------------------------------------
        jsr     update_filter_and_volume
        jmp     advance_voice_instr_pointers

check_voice_conflict:
        // ------------------------------------------------------------
        // Real voice - If voice is in use by music, skip control/filter read
        // ------------------------------------------------------------
        lda     music_voices_in_use
        and     voice_alloc_set_mask_tbl,x
        beq     apply_control_and_filter_flags

		// Voice in use, continue
        iny
        jmp     advance_voice_instr_pointers

apply_control_and_filter_flags:
        // ------------------------------------------------------------
		// Voice not in use by music
		//
        // Read two bytes:
        //   byte0 → voice control
        //   byte1 → filter enable/disable for this voice
        // ------------------------------------------------------------
        lda     (voice_read_ptr),y
        sta     voice_ctrl_shadow,x

        iny
        lda     (voice_read_ptr),y
		
		// Low nibble is zero? If so, disable filter for this voice
        and     #MSK_LOW_NIBBLE
        beq     clear_filter_bit

        // ------------------------------------------------------------
        // Enable filter for this voice
        // ------------------------------------------------------------
        lda     sid_filter_control_shadow
        ora     voice_alloc_set_mask_tbl,x
        jmp     commit_filter_control_value

clear_filter_bit:
        // ------------------------------------------------------------
        // Disable filter for this voice
        // ------------------------------------------------------------
        lda     sid_filter_control_shadow
        and     voice_alloc_clear_mask_tbl,x

commit_filter_control_value:
		// Publish filter control changes
        sta     sid_filter_control_shadow
        sta     sid_filter_control

advance_voice_instr_pointers:
        // ------------------------------------------------------------
        // Advance PC and offset according to Y (parameter count)
        // ------------------------------------------------------------
        jsr     update_instruction_ptr_and_offset

        // ------------------------------------------------------------
        // Mark voice X as executing, decode first sound instruction
        // ------------------------------------------------------------
        lda     voice_instr_active_mask
        ora     voice_alloc_set_mask_tbl,x
        sta     voice_instr_active_mask

        jsr     decode_voice_instruction
        rts
/*
================================================================================
  update_instruction_ptr_and_offset
================================================================================
Summary
        Advance the instruction program counter and loop-offset for the current
        logical voice by (Y+1) bytes after consuming an instruction payload.

Arguments
        Y       On entry, (bytes_to_advance - 1); this routine computes
                bytes_read = Y+1 and uses it as the increment.

Global Inputs
        logical_voice_idx               Index of the logical voice whose 
										instruction stream is being advanced
        voice_instr_pc_lo/hi			Current instruction PC for that voice
        voice_instr_loop_ofs_lo/hi		Stored loop-entry offset for that voice

Global Outputs
        voice_instr_pc_lo/hi            PC advanced by bytes_read
        voice_instr_loop_ofs_lo/hi      Loop offset advanced by the same bytes_read

Description
        - Loads X from logical_voice_idx so the routine operates on the current
          logical voice’s instruction state.
        - Increments Y to compute bytes_read = (Y+1), representing the number
          of bytes to step past the current instruction payload.
        - Adds bytes_read to voice_instr_pc_{lo,hi} with correct 8-bit carry
          handling, advancing the program counter for the voice.
        - Adds the same bytes_read to voice_instr_loop_ofs_{lo,hi}, keeping the
          stored loop-entry offset in sync with the updated PC so repetition
          and loop logic remain aligned.
================================================================================
*/
* = $49FC
update_instruction_ptr_and_offset:
        ldx     logical_voice_idx

        // ------------------------------------------------------------
        // Compute bytes_read = Y+1
        // ------------------------------------------------------------
        iny

        // ------------------------------------------------------------
        // Add bytes_read to instruction PC (lo/hi)
        // ------------------------------------------------------------
        tya
        clc
        adc     voice_instr_pc_lo,x
        sta     voice_instr_pc_lo,x
        bcc     update_offset
        inc     voice_instr_pc_hi,x

update_offset:
        // ------------------------------------------------------------
        // Add bytes_read to instruction loop-offset (lo/hi)
        // ------------------------------------------------------------
        tya
        clc
        adc     voice_instr_loop_ofs_lo,x
        sta     voice_instr_loop_ofs_lo,x
        bcc     update_instr_ptr_and_ofs_exit
        inc     voice_instr_loop_ofs_hi,x

update_instr_ptr_and_ofs_exit:
        rts
/*
================================================================================
  decode_voice_instruction
================================================================================
Summary
        Decode and apply a single instruction header for the current logical
        voice, updating gate/ADSR, frequency, duration, glissando, PWM, filter,
        waveform, and optional repetition state, with support for backward
        loops in the instruction stream.


Global Inputs
        logical_voice_idx               Index of the logical voice being decoded
        is_physical_voice_flag_bff      TRUE when this logical voice is
                                        mapped to a physical SID voice
        voice_instr_pc_lo/hi            Current instruction PC for this voice
        voice_instr_loop_ofs_lo/hi      Loop-offset bookkeeping for this voice
        voice_ctrl_shadow               Per-voice SID control shadow (gate, wf)
        voice_freq_lo/hi                Per-voice frequency cache
        voice_duration_lo/hi            Per-voice duration cache
        voice_gliss_lo/hi               Per-voice glissando delta
        voice_adsr_attack_decay         Per-voice ADSR attack/decay
        voice_adsr_sustain_release      Per-voice ADSR sustain/release
        voice_instr_repcount            Per-voice repetition counter
        voice_pwm_reg_ofs_tbl           Mapping: logical voice → PWM register
                                        offset in the SID register map
        voice_instr_active_mask         Bitmask of voices with active streams
        sid_volfilt_shadow              Shadow of SID vol/filt register
        voice1_freq_reg_lo/hi           Base addresses for SID register writes
                                        (PWM mapped through offsets)

Global Outputs
        voice_instr_pc_lo/hi            Advanced or rewound PC after decoding
        voice_instr_loop_ofs_lo/hi      Kept in sync with PC for loops
        voice_ctrl_shadow               Updated gate/waveform control bits
        voice_freq_lo/hi                Updated frequency when bit 2 requests it
        voice_duration_lo/hi            Updated/cleared duration as requested
        voice_gliss_lo/hi               Updated/cleared glissando as requested
        voice_adsr_attack_decay         Updated ADSR attack/decay on bit 3
        voice_adsr_sustain_release      Updated ADSR sustain/release on bit 3
        voice_instr_repcount            Updated repetition counter and loop
                                        bookkeeping when bit 7 is set
        filter_cutoff_freq_lo_reg       Updated filter cutoff low byte
                                        (operand sub-block)
        filter_cutoff_freq_hi_reg       Updated filter cutoff high byte
                                        (operand sub-block)
        stop_sound_cleanup_mode         Set to STOP_SOUND_MODE_FULL_CLEANUP
                                        when stopping via this decoder
        voice_read_ptr                  Updated to match the current PC before
                                        operand fetches

Returns
        On successful decode:
            - Returns with updated per-voice state and PC advanced past all
              operands for this instruction (unless repetition causes a
              backward jump and re-entry).
        On resource-missing or explicit stop:
            - Stops the voice via stop_sound_full_cleanup or
              stop_logical_voice and returns early.

Description
        - Loads X from logical_voice_idx, calls update_voice_stream_pointers to
          ensure the backing resource is resident; if the helper signals a
          missing resource, performs a full cleanup via stop_sound_full_cleanup
          and returns.
        - Mirrors voice_instr_pc into voice_read_ptr and fetches the next
          instruction header byte; if the header is zero, clears the repeat
          counter, treats it as an immediate stop, and tail-calls
          stop_voice_immediate.
        - For a non-zero header, latches it into voice_instr_header[X],
          instr_bit_cursor, and instr_header_raw and then walks bits in order:
            · Bit 0 (with is_physical_voice_flag_bff): update gate trigger /
              release in voice_ctrl_shadow[X] for SID-mapped voices only.
            · Bit 1: immediate stop flag; when set, selects full cleanup mode,
              calls stop_logical_voice, and returns without decoding further.
            · Bit 2 (+ bit 6 as modifier): timing/frequency block:
                · If bit 2 = 0: clears duration and glissando, leaves frequency.
                · If bit 2 = 1:
                    - Always reads a new 16-bit frequency.
                    - If bit 6 = 0: also reads new duration and glissando.
                    - If bit 6 = 1: clears duration/glissando after updating
                      frequency.
            · Bit 3 (with is_physical_voice_flag_bff): ADSR update:
                - Forces a temporary gate-off via apply_voice_control_to_sid,
                  reads new attack/decay and sustain/release, then restores the
                  original gate bit from the header into voice_ctrl_shadow[X].
            · Bit 4: operand sub-block presence:
                - If set, reads an operand bitfield into instr_op_flags and
                  processes:
                    · Operand bit 0 (with physical-voice flag): PWM update;
                      reads 16-bit PWM and writes it to the mapped SID regs.
                    · Operand bit 1: filter cutoff update; refreshes global
                      filter/volume state via update_filter_and_volume, then
                      reads and programs a new 16-bit cutoff.
                    · Operand bit 2: duration override; clears any existing
                      duration/glissando and reads a new 16-bit duration.
            · Bit 5: waveform update; if set, consumes one operand and replaces
              the high nibble of voice_ctrl_shadow[X] while preserving gate and
              low control bits.
            · Bits 6–7: repetition handling:
                - If bit 7 = 0: no repetition; advances PC/offset past operands
                  via update_instruction_ptr_and_offset and returns.
                - If bit 7 = 1:
                    · Applies a backward offset to voice_instr_pc and
                      voice_instr_loop_ofs using the offset operand.
                    · Initializes voice_instr_repcount[X] from the count
                      operand on first use, then decrements it on subsequent
                      passes.
                    · When the counter reaches #$01, clears it, skips the
                      offset/count operands, advances PC/offset, and jumps to
                      decode the next instruction.
                    · Otherwise tail-calls decode_voice_instruction again at the
                      adjusted PC to re-execute the repeated instruction.
        - For non-repeating instructions, finishes by advancing PC and loop
          offset past all consumed operands via update_instruction_ptr_and_offset
          and returns to the caller.
================================================================================
*/
* = $4A6B
decode_voice_instruction:
		// X := current logical voice index for this decode pass
        ldx     logical_voice_idx                     	
		
		// Ensure voice’s sound resource is loaded and base/PC are valid
        jsr     update_voice_stream_pointers 			
		
		// Sound resident? If not, stop and do a full cleanup
        cmp     #$01                                  	
        bne     voice_instr_resource_loaded           	
        jsr     stop_sound_full_cleanup               	
        rts                                           

voice_instr_resource_loaded:
		// ------------------------------------------------------------
		// Set up the per-voice instruction read pointer from the current
		// voice instruction PC so subsequent operand fetches use (voice_read_ptr),Y.
		// ------------------------------------------------------------
        lda     voice_instr_pc_lo,x                   
        sta     voice_read_ptr                       
        lda     voice_instr_pc_hi,x                   
        sta     voice_read_ptr + 1

		// Store debug/status flag = non-zero instruction header
        lda     #$ff                                  
        sta     $4812                                 

		// ------------------------------------------------------------
		// Fetch the next instruction header byte from the stream and
		// branch into the bitfield decoder only when it is non-zero.
		// ------------------------------------------------------------
        ldy     #$00                                  // Y := 0 (start reading at current PC)
        lda     (voice_read_ptr),y                    
        bne     instr_header_nonzero                  // Non-zero header → decode bits

		// ------------------------------------------------------------
		// Handle zero instruction header: clear this voice’s repeat counter
		// and treat the instruction as an immediate stop (no further decoding).
		// ------------------------------------------------------------
        sta     $4812                                 // Debug/status flag = zero instruction header
        sta     voice_instr_repcount,x                // Clear repeat counter for this voice
        jmp     stop_voice_immediate                  // Treat #$00 as “stop voice now”

instr_header_nonzero:
		// ------------------------------------------------------------
		// Latch the non-zero instruction header in both raw and “bit cursor”
		// form so later tests can both shift through individual bits and
		// re-check the original flags (notably bit 6) as needed.
		// ------------------------------------------------------------
        sta     voice_instr_header,x                  // Latched copy of instruction header for this voice
        sta     instr_bit_cursor                      // Shifting copy used to walk bits 0–7 via LSR
        sta     instr_header_raw                      // Raw, unmodified header (for BIT tests on bit 6)

		// ------------------------------------------------------------
        // Bit 0 - gate trigger/release (SID-range only, via is_physical_voice_flag_bff)
		//
		// Decode bit 0 of the instruction header as a gate trigger/release,
		// but only for voices marked as mapped to a physical SID slot by
		// is_physical_voice_flag_bff (via V from the BIT test).
		// ------------------------------------------------------------
        bit     is_physical_voice_flag_bff            // Set V from SID-range flag (bit 6:=voice in physical SID range)
        lsr     instr_bit_cursor                      // Shift bit 0 of header into C (gate: 1=trigger, 0=release)
        bvc     check_stop_voice_bit                  // If V=0 → voice not in SID range, skip gate changes

		// ------------------------------------------------------------
		// Update this voice’s control shadow gate bit from decoded header bit 0,
		// mapping it to either a trigger (set gate) or release (clear gate),
		// while leaving the change staged in the shadow copy (not yet pushed to SID).
		// ------------------------------------------------------------
        lda     voice_ctrl_shadow,x                   // A := shadow control register for this voice
        bcc     clear_voice_gate_bit                  // If C=0 → header bit0=0 → clear gate (release)
        ora     #VOICE_GATE_MASK                      // C=1 → header bit0=1 → set gate bit (trigger)
        jmp     commit_voice_control_shadow           // Store updated gate state in shadow
clear_voice_gate_bit:
        and     #VOICE_GATE_CLEAR_MASK                // Clear gate bit 0 to force note-off
commit_voice_control_shadow:
        sta     voice_ctrl_shadow,x                   // Save updated control shadow (later pushed to SID)

		// ------------------------------------------------------------
		// Bit 1 - Stop sound
		//
		// Decode bit 1 of the instruction header as an immediate stop flag;
		// if set, the voice is terminated right away and no further bits of
		// this instruction are processed.
		// ------------------------------------------------------------
check_stop_voice_bit:
        lsr     instr_bit_cursor                      // Shift bit 1 of header into C (stop flag)
        bcc     check_freq_dur_gliss_bit              // If bit1=0 → no immediate stop, continue decoding

stop_voice_immediate:
		// ------------------------------------------------------------
		// Handle an instruction that requests an immediate voice stop: select
		// full cleanup mode, stop the logical voice X, and return without
		// decoding any remaining bits or operands for this instruction.
		// ------------------------------------------------------------
        lda     #STOP_SOUND_MODE_FULL_CLEANUP         // Select full cleanup when stopping this voice
        sta     stop_sound_cleanup_mode               
        txa                                           // Copy voice index into A for stop routine
        jsr     stop_logical_voice                    // Stop logical voice X (and free resources)
        tax                                           
        rts                                           

check_freq_dur_gliss_bit:
		// ------------------------------------------------------------
        // Bit 2 (+ bit 6) - frequency / duration / glissando
		//
		// Decode bit 2 (with bit 6 as a modifier) to control the timing fields:
		// either clear duration/glissando outright or, if set, read a new
		// frequency and optionally new duration and glissando values.
		// ------------------------------------------------------------
        lsr     instr_bit_cursor                      // Shift bit 2 of header into C
        bit     instr_header_raw                      // Copy original bit 6 into V (used as modifier)
        bcc     clear_dur_gliss_as_bit2_clear         // If bit2=0 → just clear duration/glissando and skip reads

		// ------------------------------------------------------------
        // Bit 2 set - read frequency (always)
		//
		// For bit 2 set, always consume and apply a new 16-bit frequency value
		// for this voice; bit 6 (via V) then decides whether we also refresh
		// duration/glissando or simply clear them after updating frequency.
		// ------------------------------------------------------------
        iny                                           // advance to freq_lo operand
        lda     (voice_read_ptr),y                    // Read freq_lo
        sta     voice_freq_lo,x                       // Store voice frequency low byte
        iny                                           // freq_hi operand
        lda     (voice_read_ptr),y                    
        sta     voice_freq_hi,x                       

        bvs     clear_dur_gliss_after_freq            // If bit6=1 → clear dur/gliss, do not read them

		// ------------------------------------------------------------
        // Bit 2 set, bit 6 clear - read duration
		//
		// When bit 2 is set and bit 6 is clear, extend the timing update by
		// reading a 16-bit duration from the stream and storing it as this
		// voice’s new playback duration.
		// ------------------------------------------------------------
        iny                                           // advance to duration_lo
        lda     (voice_read_ptr),y                    // Read duration_lo
        sta     voice_duration_lo,x                   // Store voice duration low byte
        iny                                           // duration_hi
        lda     (voice_read_ptr),y                    
        sta     voice_duration_hi,x                   

		// ------------------------------------------------------------
		// Consume and apply the 16-bit glissando delta for this voice, then
		// continue into the ADSR (bit 3) handling for this same instruction header.
		// ------------------------------------------------------------
        iny                                           // advance to glissando_lo
        lda     (voice_read_ptr),y                    // Read glissando_lo
        sta     voice_gliss_lo,x                      // Store glissando low byte
        iny                                           // glissando_hi
        lda     (voice_read_ptr),y                    
        sta     voice_gliss_hi,x                      

        jmp     check_adsr_bit                        // Continue with Bit 3 (ADSR) handling

		// ------------------------------------------------------------
		// For bit2 set with bit6 set, leave the new frequency in place but
		// actively clear any existing duration/glissando so timing reverts
		// to “no explicit duration/glide” for this voice.
		// ------------------------------------------------------------
clear_dur_gliss_after_freq:
        jsr     reset_voice_duration_and_glissando    // Bit2=1, bit6=1 → clear duration and glissando
        jmp     check_adsr_bit                        // Continue with ADSR bit check

		// ------------------------------------------------------------
		// Clear duration and glissando when header bit 2 is clear, preserving
		// the current frequency so only the timing/glide state is reset.
		// ------------------------------------------------------------
clear_dur_gliss_as_bit2_clear:
        jsr     reset_voice_duration_and_glissando    // Bit2=0 → clear duration and glissando, leave frequency alone

		// ------------------------------------------------------------
        // Bit 3 - ADSR update (SID-range only)
		//
		// Decode bit 3 as an ADSR update request, but only for voices marked
		// as mapped to a physical SID slot; if either the SID-range flag (V)
		// is clear or bit 3 itself is clear, the current ADSR envelope is left
		// untouched and decoding continues with the operand sub-block.
		// ------------------------------------------------------------
check_adsr_bit:
        bit     is_physical_voice_flag_bff            // Set V from SID-range flags again
        lsr     instr_bit_cursor                      // Shift bit 3 of header into C
        bvc     check_operand_block_bit               // V=0 → no physical SID voice → skip ADSR changes
        bcc     check_operand_block_bit               // Bit3=0 → no ADSR update requested

		// ------------------------------------------------------------
		// Clear the gate bit in voice_ctrl_shadow and push the change to SID
		// via apply_voice_control_to_sid (note-off side-effect).
		// ------------------------------------------------------------		
		// Temporarily force a note-off for this voice
		sty     voice_instr_saved                     // Save current Y (operand index) for later restore

        lda     voice_ctrl_shadow,x                   // A := current control shadow
        and     #VOICE_GATE_CLEAR_MASK                // Clear gate bit to force note-off
        sta     voice_ctrl_shadow,x                   // Save gate-off state in shadow
        jsr     apply_voice_control_to_sid            // Push gate-off state to SID registers

        ldy     voice_instr_saved                     // Restore Y (instruction operand position)
        ldx     logical_voice_idx                     // Restore logical voice index into X

		// ------------------------------------------------------------
        // Read new ADSR envelope (2 bytes)
		//
		// Load the two-byte ADSR envelope from the stream (attack/decay and
		// sustain/release) and cache it for this voice so subsequent gate
		// changes use the freshly programmed envelope shape.
		// ------------------------------------------------------------
        iny                                           // Y → ADSR attack/decay
        lda     (voice_read_ptr),y                    // Read ADSR attack/decay byte
        sta     voice_adsr_attack_decay,x             // Store ADSR attack/decay for this voice
        iny                                           // ADSR sustain/release
        lda     (voice_read_ptr),y                    
        sta     voice_adsr_sustain_release,x          

		// ------------------------------------------------------------
        // Re-apply original gate bit from voice_instr_header
		//
		// Reconstruct the control shadow’s gate bit based on the original
		// instruction header so, after the ADSR change, the voice resumes
		// (or remains released) with the exact same trigger/release state.
		// The actual re-trigger/hold on SID happens when
		// apply_voice_control_to_sid is called again, not immediately here.
		// ------------------------------------------------------------
        lda     voice_instr_header,x                  // A := original instruction header for this voice
        and     #VOICE_GATE_MASK                      // Isolate original gate bit (bit 0)
        ora     voice_ctrl_shadow,x                   // Merge original gate bit back into control shadow
        sta     voice_ctrl_shadow,x                   // Save updated control shadow (with new ADSR)

		// ------------------------------------------------------------
        // Bit 4 - operand sub-actions: PWM / filter / duration
		//
		// Decode bit 4 to decide whether an extra operand bitfield follows;
		// if set, the next byte encodes optional PWM, filter, and duration
		// updates that are applied before waveform and repetition handling.
		// ------------------------------------------------------------
check_operand_block_bit:
        lsr     instr_bit_cursor                      // Shift bit 4 of header into C
        bcc     check_waveform_bit                    // Bit4=0 → no operand sub-block, skip to waveform

		// ------------------------------------------------------------
		// Consume the operand bitfield that encodes sub-actions for this
		// instruction (PWM, filter cutoff, and/or duration), caching it in
		// instr_op_flags so individual bits can be tested in turn.
		// ------------------------------------------------------------
        iny                                           // Y  → operand bitfield
        lda     (voice_read_ptr),y                    // Read operand bitfield for PWM/filter/duration
        sta     instr_op_flags                        // Save operand flags for subsequent tests

        bit     is_physical_voice_flag_bff            // Set V from SID-range flags (PWM only applies in-range)

		// ------------------------------------------------------------
        // Operand bit 0 - PWM (SID-range only)
		//
		// Decode operand bit 0 as a PWM update request, but only honor it for
		// voices marked as SID-backed; if either the SID-range flag (V) is
		// clear or the bit is 0, PWM registers are left unchanged.
		// ------------------------------------------------------------
check_operand_pwm_bit:
        lsr     instr_op_flags                        // Shift op bit0 into C
        bvc     check_operand_filter_bit              // V=0 → no PWM support for this voice
        bcc     check_operand_filter_bit              // Bit0=0 → no PWM update requested

		// ------------------------------------------------------------
		// Apply a new 16-bit PWM width to the physical SID voice mapped from
		// this logical voice by using the per-voice PWM register offset, then
		// restore X to the logical voice index for subsequent decoding.
		// ------------------------------------------------------------
        iny                                           // Y → PWM lo operand
        lda     voice_pwm_reg_ofs_tbl,x               // A := SID register offset for this voice’s PWM pair
        tax                                           // X := PWM register offset (physical SID index)

        lda     (voice_read_ptr),y                    // Read PWM low byte
        sta     voice1_freq_reg_lo,x                  // Write to corresponding SID low register (PWM mapped)
        iny                                           // Y := Y+1 → PWM hi operand
        lda     (voice_read_ptr),y                    // PWM high byte
        sta     voice1_freq_reg_hi,x                  
        ldx     logical_voice_idx                     

		// ------------------------------------------------------------
        // Operand bit 1 - filter cutoff
		//
		// Decode operand bit 1 as a filter cutoff update request; if set, the
		// instruction will refresh global filter state and program a new
		// cutoff value before continuing.
		// ------------------------------------------------------------
check_operand_filter_bit:
        lsr     instr_op_flags                        // Shift op bit1 into C
        bcc     check_operand_duration_bit            // Bit1=0 → no filter update requested

		// ------------------------------------------------------------
		// Advance past the sub-bitfield header, refresh the shared filter/volume
		// state, then read and apply a new 16-bit cutoff value into the SID’s
		// filter cutoff registers.
		// ------------------------------------------------------------
        iny                                           // Y → prepare for filter update side-effects
        jsr     update_filter_and_volume              // Refresh filter/volume state before writing cutoff
		
        iny                                           // Y → filter_lo operand
        lda     (voice_read_ptr),y                    // Read filter cutoff low byte
        sta     filter_cutoff_freq_lo_reg             // Store into filter cutoff low register
        iny                                           // filter_hi operand
        lda     (voice_read_ptr),y                    
        sta     filter_cutoff_freq_hi_reg             

		// ------------------------------------------------------------
        // Operand bit 2 - duration (clears glissando)
		//
		// Decode operand bit 2 as a duration override request; if set, the
		// instruction clears any existing glissando and programs a new
		// 16-bit duration for this voice before moving on to waveform logic.
		// ------------------------------------------------------------
check_operand_duration_bit:
        lsr     instr_op_flags                        // Shift op bit2 into C
        bcc     check_waveform_bit                    // Bit2=0 → no additional duration update

		// ------------------------------------------------------------
		// Clear any previous timing/glide state, then read and apply a fresh
		// 16-bit duration for this voice so the instruction’s operand can
		// override whatever duration was previously active.
		// ------------------------------------------------------------
        jsr     reset_voice_duration_and_glissando    // Clear any existing duration/glissando before update
		
        iny                                           // Y → new duration_lo
        lda     (voice_read_ptr),y                    // Read new duration low byte
        sta     voice_duration_lo,x                   // Store duration low for this voice
        iny                                           // duration_hi
        lda     (voice_read_ptr),y                    
        sta     voice_duration_hi,x                   

		// ------------------------------------------------------------
        // Bit 5 - waveform: replace high nibble of voice_ctrl_shadow
		//
		// Decode bit 5 as a waveform update request; if set, the instruction
		// will consume one operand byte to replace only the high nibble of
		// the control shadow, leaving gate and low control flags intact.
		// ------------------------------------------------------------
check_waveform_bit:
        lsr     instr_bit_cursor                      // Shift bit 5 of header into C
        bcc     check_repeat_bit                      // Bit5=0 → keep existing waveform bits

		// ------------------------------------------------------------
		// Replace this voice’s waveform bits by merging in the operand byte’s
		// high nibble while preserving the existing gate state and low flags.
		// ------------------------------------------------------------
        lda     voice_ctrl_shadow,x                   // A := current control shadow
        and     #$0f                                  // Preserve low nibble (gate and low flags)
        iny                                           // Y := Y+1 → waveform operand
        ora     (voice_read_ptr),y                    // OR in high nibble from operand (waveform selection)
        sta     voice_ctrl_shadow,x                   // Save updated control shadow with new waveform

		// ------------------------------------------------------------
        // Bit 7 - repetition (offset + repeat count)
		//
		// Decode bit 7 as a “repeat this instruction” flag; when set, a trailing
		// offset/count pair will drive a backward loop in the instruction
		// stream, otherwise execution just advances to the next instruction.
		// ------------------------------------------------------------
check_repeat_bit:
        lsr     instr_bit_cursor                      // Shift bit 6 (ignored here) out of cursor
        lsr     instr_bit_cursor                      // Shift bit 7 of header into C (repeat flag)
        bcc     decode_voice_instruction_exit                              // Bit7=0 → no repetition; fall through to exit

		// ------------------------------------------------------------
		// Check whether this is the final scheduled repetition for the current
		// instruction; if the counter equals #$01, handle the “last pass” path,
		// otherwise continue with the generic repeat-iteration logic.
		// ------------------------------------------------------------
        lda     voice_instr_repcount,x                // A := current repeat counter for this instruction
        cmp     #$01                                  // Is this the last pending repetition?
        bne     handle_repeat_iteration               // If not #$01 → normal repeat handling

		// ------------------------------------------------------------
        // Last repetition: clear counter, skip offset+count, advance and continue
		//
		// Handle the final repetition for this instruction by zeroing its counter,
		// skipping over the stored offset/count operands, advancing the PC/offset
		// to the next instruction in the stream, and immediately decoding that
		// next instruction.
		// ------------------------------------------------------------
        dec     voice_instr_repcount,x                // Set counter to #$00 (no further repeats)
        iny                                           // Y → skip offset byte
        iny                                           // Y → skip repeat-count byte
        jsr     update_instruction_ptr_and_offset     // Advance PC/offset past this instruction and its operands
        jmp     loop_to_next_voice_instruction        // Decode next instruction at the updated PC

		// ------------------------------------------------------------
        // Repetition in progress or just starting (counter may still be zero)
		//
		// Apply the repeat offset for this iteration by subtracting the encoded
		// backward delta from the per-voice instruction PC, effectively jumping
		// execution back in the stream before re-decoding the instruction body.
		// ------------------------------------------------------------
handle_repeat_iteration:
        iny                                           // Y → offset operand
        lda     voice_instr_pc_lo,x                   // A := current instruction PC low byte
        sec                                           
        sbc     (voice_read_ptr),y                    // Subtract offset from PC.lo
        sta     voice_instr_pc_lo,x                   // Store adjusted PC.lo
        bcs     apply_repeat_offset_to_instr_pc       // If no borrow → hi-byte unchanged
        dec     voice_instr_pc_hi,x                   // Borrow → decrement PC.hi

		// ------------------------------------------------------------
		// Mirror the same backward offset into the loop-offset bookkeeping
		// variables so any diagnostic or relative offset tracking stays aligned
		// with the adjusted instruction PC.
		// ------------------------------------------------------------
apply_repeat_offset_to_instr_pc:
        lda     voice_instr_loop_ofs_lo,x             // A := loop-offset tracking low byte
        sec                                           
        sbc     (voice_read_ptr),y                    // Apply same offset to loop-offset low byte
        sta     voice_instr_loop_ofs_lo,x             // Store adjusted loop-offset low byte
        bcs     load_or_advance_repeat_count          // No borrow → hi byte unchanged
        dec     voice_instr_loop_ofs_hi,x             // Borrow → decrement loop-offset high byte

		// ------------------------------------------------------------
		// Consume the repeat-count operand and either initialize the per-voice
		// repeat counter on first use or, if already non-zero, fall through to
		// simply decrement it for subsequent iterations.
		// ------------------------------------------------------------
load_or_advance_repeat_count:
        iny                                           // Y → repeat-count operand
        lda     voice_instr_repcount,x                // A := current repeat counter
        bne     decrement_repeat_counter              // If non-zero → just count down

		// ------------------------------------------------------------
        // First time: read repeat count from stream
		//
		// Initialize the per-voice repeat counter from the stream on first use
		// and immediately loop back to decode the same instruction at its
		// newly adjusted position.
		// ------------------------------------------------------------
        lda     (voice_read_ptr),y                    // Read initial repeat count
        sta     voice_instr_repcount,x                // Initialize repeat counter for this instruction
        jmp     loop_to_next_voice_instruction        // Re-enter decoder to execute repeated instruction

		// ------------------------------------------------------------
		// Finalize repeat bookkeeping (count down if needed) and re-enter the
		// decoder at the updated instruction pointer to process the next pass.
		// ------------------------------------------------------------
decrement_repeat_counter:
        dec     voice_instr_repcount,x                // Decrement existing repeat counter by 1

loop_to_next_voice_instruction:
        jmp     decode_voice_instruction              // Tail-call decoder for the next/looped instruction

		// ------------------------------------------------------------
		// Finalize a non-repeating instruction by advancing the per-voice
		// instruction PC/offset past all operands, then return to the caller.
		// ------------------------------------------------------------
decode_voice_instruction_exit:
        jsr     update_instruction_ptr_and_offset     
        rts                                           
		
		
/*
================================================================================
  swap_voice_settings
================================================================================
Summary
	Exchange the full per-voice execution state between a “main” logical
	voice slot X and an alternate-voice slot Y, so multiplexing can swap
	which voice is currently active on the SID.

Arguments
	X 		Main logical voice index whose state is being swapped
	Y 		Alternate slot index whose state is being exchanged with X

Global Inputs
	voice_adsr_attack_decay 		ADSR attack/decay for main voices
	voice_adsr_sustain_release 		ADSR sustain/release for main voices
	voice_data_base_{lo,hi} 		Base pointers to per-voice sound data
	voice_priority_0 				Priority values for main voices
	voice_sound_id_tbl 				Sound IDs bound to main voices
	voice_instr_header 				Current instruction header per voice
	voice_instr_repcount 			Per-voice repetition counter
	voice_ctrl_shadow 				SID control-register shadow per voice
	voice_instr_pc_{lo,hi} 			Instruction PC per voice
	voice_freq_{lo,hi} 				Cached frequency/cutoff per voice
	voice_duration_{lo,hi} 			16-bit duration counters per voice
	voice_gliss_{lo,hi} 			16-bit glissando deltas per voice
	voice_base_addr_{lo,hi} 		Cached resource base address per voice
	voice_instr_loop_ofs_{lo,hi} 	Loop-entry instruction offset per voice
	alt_voice_* 					Parallel “alternate” copies of the same fields 
									(ADSR, pointers, etc.)

Global Outputs
	voice_* 						Swapped with prior alt_voice_* state
	alt_voice_* 					Swapped with prior voice_* state

Description
	- If X < 3 (real SID voice), swap ADSR attack/decay and	sustain/release between 
	the main and alternate slots; for X >= 3, ADSR is left unchanged.
	- Swap the per-voice data base pointer (lo/hi) so each side now	points at the 
	other’s sound data region.
	- Swap priority and sound ID, preserving which sound and priority are associated 
	with each slot after the exchange.
	- Swap instruction header and repetition counter so the execution state and repeat 
	loops follow the swapped voice.
	- Swap control-register shadow and instruction PC (lo/hi) so gate and program counter 
	state move with the active voice.
	- Swap cached frequency, duration, and glissando values, ensuring pitch and timing 
	state are consistent with the new owner of each	slot.
	- Swap cached base address and loop-entry instruction offsets, keeping relocation and 
	loop semantics aligned with the moved voice	state.
================================================================================
*/
* = $51A5
swap_voice_settings:
		// Is this a filter or virtual voice? If so, skip the ADSR swap
        cpx     #FILTER_VOICE_INDEX
        bpl     skip_adsr

        // ------------------------------------------------------------
        // Swap ADSR attack/decay
        // ------------------------------------------------------------
        lda     voice_adsr_attack_decay,x
        pha
        lda     alt_voice_adsr_attack_decay,y
        sta     voice_adsr_attack_decay,x
        pla
        sta     alt_voice_adsr_attack_decay,y

        // ------------------------------------------------------------
        // Swap ADSR sustain/release
        // ------------------------------------------------------------
        lda     voice_adsr_sustain_release,x
        pha
        lda     alt_voice_adsr_sustain_release,y
        sta     voice_adsr_sustain_release,x
        pla
        sta     alt_voice_adsr_sustain_release,y

skip_adsr:
        // ------------------------------------------------------------
        // Swap data-base pointer (lo/hi)
        // ------------------------------------------------------------
        lda     voice_data_base_lo,x
        pha
        lda     alt_voice_data_ptrs_lo,y
        sta     voice_data_base_lo,x
        pla
        sta     alt_voice_data_ptrs_lo,y

        lda     voice_data_base_hi,x
        pha
        lda     alt_voice_data_ptrs_hi,y
        sta     voice_data_base_hi,x
        pla
        sta     alt_voice_data_ptrs_hi,y

        // ------------------------------------------------------------
        // Swap priority
        // ------------------------------------------------------------
        lda     voice_priority_0,x
        pha
        lda     alt_voice_priority_0,y
        sta     voice_priority_0,x
        pla
        sta     alt_voice_priority_0,y

        // ------------------------------------------------------------
        // Swap sound ID
        // ------------------------------------------------------------
        lda     voice_sound_id_tbl,x
        pha
        lda     alt_voice_sound_id_tbl,y
        sta     voice_sound_id_tbl,x
        pla
        sta     alt_voice_sound_id_tbl,y

        // ------------------------------------------------------------
        // Swap instruction header
        // ------------------------------------------------------------
        lda     voice_instr_header,x
        pha
        lda     alt_voice_instr_header,y
        sta     voice_instr_header,x
        pla
        sta     alt_voice_instr_header,y

        // ------------------------------------------------------------
        // Swap instruction repcount
        // ------------------------------------------------------------
        lda     voice_instr_repcount,x
        pha
        lda     alt_voice_instr_repcount,y
        sta     voice_instr_repcount,x
        pla
        sta     alt_voice_instr_repcount,y

        // ------------------------------------------------------------
        // Swap control-register shadow
        // ------------------------------------------------------------
        lda     voice_ctrl_shadow,x
        pha
        lda     alt_voice_ctrl_shadow,y
        sta     voice_ctrl_shadow,x
        pla
        sta     alt_voice_ctrl_shadow,y

        // ------------------------------------------------------------
        // Swap instruction PC (lo/hi)
        // ------------------------------------------------------------
        lda     voice_instr_pc_lo,x
        pha
        lda     alt_voice_instr_pc_lo,y
        sta     voice_instr_pc_lo,x
        pla
        sta     alt_voice_instr_pc_lo,y

        lda     voice_instr_pc_hi,x
        pha
        lda     alt_voice_instr_pc_hi,y
        sta     voice_instr_pc_hi,x
        pla
        sta     alt_voice_instr_pc_hi,y

        // ------------------------------------------------------------
        // Swap frequency (lo/hi)
        // ------------------------------------------------------------
        lda     voice_freq_lo,x
        pha
        lda     alt_voice_freq_lo,y
        sta     voice_freq_lo,x
        pla
        sta     alt_voice_freq_lo,y

        lda     voice_freq_hi,x
        pha
        lda     alt_voice_freq_hi,y
        sta     voice_freq_hi,x
        pla
        sta     alt_voice_freq_hi,y

        // ------------------------------------------------------------
        // Swap duration (lo/hi)
        // ------------------------------------------------------------
        lda     voice_duration_lo,x
        pha
        lda     alt_voice_duration_lo,y
        sta     voice_duration_lo,x
        pla
        sta     alt_voice_duration_lo,y

        lda     voice_duration_hi,x
        pha
        lda     alt_voice_duration_hi,y
        sta     voice_duration_hi,x
        pla
        sta     alt_voice_duration_hi,y

        // ------------------------------------------------------------
        // Swap glissando (lo/hi)
        // ------------------------------------------------------------
        lda     voice_gliss_lo,x
        pha
        lda     alt_voice_gliss_lo,y
        sta     voice_gliss_lo,x
        pla
        sta     alt_voice_gliss_lo,y

        lda     voice_gliss_hi,x
        pha
        lda     alt_voice_gliss_hi,y
        sta     voice_gliss_hi,x
        pla
        sta     alt_voice_gliss_hi,y

        // ------------------------------------------------------------
        // Swap base address (lo/hi)
        // ------------------------------------------------------------
        lda     voice_base_addr_lo,x
        pha
        lda     alt_voice_base_addr_lo,y
        sta     voice_base_addr_lo,x
        pla
        sta     alt_voice_base_addr_lo,y

        lda     voice_base_addr_hi,x
        pha
        lda     alt_voice_base_addr_hi,y
        sta     voice_base_addr_hi,x
        pla
        sta     alt_voice_base_addr_hi,y

        // ------------------------------------------------------------
        // Swap instruction loop offset (lo/hi)
        // ------------------------------------------------------------
        lda     voice_instr_loop_ofs_lo,x
        pha
        lda     alt_voice_instr_loop_ofs_lo,y
        sta     voice_instr_loop_ofs_lo,x
        pla
        sta     alt_voice_instr_loop_ofs_lo,y

        lda     voice_instr_loop_ofs_hi,x
        pha
        lda     alt_voice_instr_loop_ofs_hi,y
        sta     voice_instr_loop_ofs_hi,x
        pla
        sta     alt_voice_instr_loop_ofs_hi,y

        rts
/*
================================================================================
  clear_all_alternate_settings
================================================================================
Summary
	Reset the entire block of “alternate” voice state (used by multiplexing
	and similar effects) to zero, without disturbing caller registers.

Global Inputs
	alt_voice_adsr_attack_decay				Base address of the alternate-voice 
											state block to be cleared

Global Outputs
	alt_voice_adsr_attack_decay..+$3C		Entire alternate-voice state block cleared to $00

Description
	- Saves A, X, and Y on the stack so the routine is fully register-transparent to callers.
	- Clears a contiguous 61-byte region starting at alt_voice_adsr_attack_decay 
	(offsets $00 through $3C) by walking X	downward and storing zero at each location.
	- Restores Y, X, and A in reverse order of saving so the caller’s register state is preserved, 
	while the alternate-voice block is left	fully reset.
================================================================================
*/
* = $52D0
clear_all_alternate_settings:
		// Save registers
        pha
        txa
        pha
        tya
        pha

        // ------------------------------------------------------------
        // Zero contiguous region alt_voice_adsr_attack_decay+$00..$3C
        // ------------------------------------------------------------
        ldx     #ALT_VOICE_BLOCK_LAST_OFFSET
        lda     #$00
clear_alt_voice_block_loop:
        sta     alt_voice_adsr_attack_decay,x
        dex
        bpl     clear_alt_voice_block_loop

        // Restore registers
        pla
        tay
        pla
        tax
        pla
        rts

/*
================================================================================
  process_multiplexing
================================================================================
Summary
	Prepare per-voice multiplexing state for the current logical voice.
	For real voices (0–2), assigns alternate slots for primary/secondary
	multiplexing. For the filter voice (3), captures filter mode bits,
	installs an alternate filter state in slot #2, and enables filter
	multiplexing. Marks primary multiplexing active when done.

Arguments
	X 		Logical voice index being considered for multiplexing

Global Inputs
	alt_voice_slots_cleared_flag 		Flag: non-zero → alternates already cleared this tick
	voice_instr_active_mask 			Bitmask of logical voices executing instructions
	sid_volfilt_shadow 					Shadow of SID vol/filt register

Global Outputs
	alt_voice_slots_cleared_flag 		Set TRUE when alternates are cleared
	filter_mode_mask 					Captured filter-mode bits from sid_volfilt_shadow
	vmux_secondary_active_flag_bff 		Set TRUE when a secondary (partner) voice has an 
										alternate slot assigned
	vmux_filter_enabled_flag_bff 		Set TRUE when filter multiplexing is configured 
										for the filter voice
	vmux_primary_active_flag_bff 		Set TRUE whenever primary multiplexing is active this tick

Description
	- Rejects non-real voices (X >= FIRST_MULTIPLEXED_VOICE_INDEX) early by
	returning immediately, since only voices 0–3 participate in this
	multiplexing scheme.

    - For voices 0–2 (real SID voices):
        · If alt_voice_slots_cleared_flag is FALSE, calls
          clear_all_alternate_settings once to zero the entire alternate
          state block and leave a clean baseline for multiplexing.
        · Swaps the current voice X with alternate slot #0 (Y = 0) via
          swap_voice_settings, installing its state as the primary
          multiplexed slice.
        · Advances X to its partner voice X+4 (virtual/multiplexed slot)
          and checks whether that partner is active in
          voice_instr_active_mask.
        · If the partner voice is active:
            · Swaps it with alternate slot #1 (Y = 1) so its state can be
              used as the secondary multiplexed slice.
            · Sets vmux_secondary_active_flag_bff = BTRUE to record that a
              secondary multiplexing pass should be executed.

    - For X == FILTER_VOICE_INDEX (filter voice 3):
        · Captures the current filter-mode bits by AND-ing
          sid_volfilt_shadow with SID_FILTER_MODE_MASK and storing the
          result in filter_mode_mask.
        · Calls clear_all_alternate_settings to reset all alternate slots
          to zeroed state, then sets alt_voice_slots_cleared_flag = BTRUE
          so other paths know the alternates are clean.
        · Swaps the filter voice state with alternate slot #2 (X forced to
          FILTER_VOICE_INDEX, Y = 2), storing a snapshot of the filter
          voice in that slot for later use.
        · Sets vmux_filter_enabled_flag_bff = BTRUE so the filter
          multiplexing path can install the alternate filter state and
          cutoff during the apply step.

    - On either path (voices 0–2 or filter voice 3), sets
      vmux_primary_active_flag_bff = BTRUE to signal that primary
      multiplexing should run on this tick.

================================================================================
*/
* = $52E5
process_multiplexing:
		// Is this a real voice? If not, exit
        cpx     #FIRST_MULTIPLEXED_VOICE_INDEX
        bmi     voice_index_validated
        rts

voice_index_validated:
		// Save registers
        pha
        txa
        pha
        tya
        pha

        // ------------------------------------------------------------
        // Split paths:
        //   X = 0..2 → use alternate-setting logic (alt #0 / alt #1).
        //   X = 3    → use filter / alt #2 logic.
        // ------------------------------------------------------------
        cpx     #FILTER_VOICE_INDEX
        bpl     filter_voice3_path

        // ------------------------------------------------------------
        // Voices 0–2
        // ------------------------------------------------------------
		// Are alternate slots cleared? If not, clear them
        lda     alt_voice_slots_cleared_flag
        bne     swap_voice_with_alt_slot0
		
        // Clear all alternate settings
        jsr     clear_all_alternate_settings

swap_voice_with_alt_slot0:
		// Swap the current voice with alternate #0
        ldy     #$00
        jsr     swap_voice_settings

        // ------------------------------------------------------------
        // Use partner voice X+4 for a potential secondary multiplexing
        // ------------------------------------------------------------
        inx
        inx
        inx
        inx

		// If that voice is not executing instructions, skip secondary setup.
        lda     voice_instr_active_mask
        and     voice_alloc_set_mask_tbl,x
        beq     exit_after_partner_voice_check

		// Otherwise, use alternate #1 and mark secondary active.
        ldy     #$01
        jsr     swap_voice_settings
        lda     #BTRUE
        sta     vmux_secondary_active_flag_bff

exit_after_partner_voice_check:
        jmp     process_multiplexing_exit

filter_voice3_path:
		// If not the filter voice, exit
        bne     process_multiplexing_exit

        // ------------------------------------------------------------
        // Filter Voice
        // ------------------------------------------------------------
        // Preserve current filter mode bits
        lda     sid_volfilt_shadow
        and     #SID_FILTER_MODE_MASK
        sta     filter_mode_mask

        // Clear all alternate settings
        jsr     clear_all_alternate_settings

		// Mark all alternates cleared
        lda     #BTRUE
        sta     alt_voice_slots_cleared_flag

		// Swap with alternate #2
        ldx     #FILTER_VOICE_INDEX
        ldy     #$02
        jsr     swap_voice_settings

		// Enable filter multiplexing
        lda     #BTRUE
        sta     vmux_filter_enabled_flag_bff

process_multiplexing_exit:
        // Mark primary multiplexing active
        lda     #BTRUE
        sta     vmux_primary_active_flag_bff

		// Restore registers
        pla
        tay
        pla
        tax
        pla
        rts
/*
================================================================================
  apply_multiplexing_and_filter_for_voice
================================================================================
Summary
	Apply one frame of voice multiplexing and optional filter multiplexing
	for a real SID voice. Swaps main voice state with alternate slots,
	commits frequency/envelope to SID, and updates filter routing, mode,
	and cutoff according to the current multiplexing configuration.

Arguments
	X 		Logical voice index to process 

Global Inputs
	vmux_primary_active_flag_bff 		Primary multiplexing activity flag
	vmux_secondary_active_flag_bff 		Secondary multiplexing activity flag
	vmux_filter_enabled_flag_bff 		Filter multiplexing enable flag
	filter_control_snapshot 			Latched filter-control byte before per-voice routing is applied
	filter_mode_mask 					Latched filter-mode bits (upper nibble for vol/filt register)
	filter_cutoff_freq_lo/hi			Latched filter cutoff
	sid_volfilt_shadow 					Shadow copy of $D418 (volume/filter)
	sid_filter_control_shadow 			Shadow copy of $D417 (filter control)
	alt_voice_slots_cleared_flag 		Flag used by multiplexing to trackwhether alternate 
										slots have been reset

Global Outputs
	voice_being_modified 				Index of the primary voice that drove this multiplexing pass
	vmux_primary_active_flag_bff 		Cleared when this routine finishes
	vmux_secondary_active_flag_bff 		Cleared when this routine finishes
	vmux_filter_enabled_flag_bff 		Cleared when this routine finishes
	alt_voice_slots_cleared_flag 		Cleared when this routine finishes
	filter_mode_mask 					Cleared when this routine finishes
	sid_filter_control_shadow 			Updated filter-control shadow
	sid_filter_control 					Updated hardware filter-control register
	sid_volfilt_shadow 					Updated vol/filt shadow (volume preserved,
											filter-mode bits refreshed)
	sid_master_volume 					Updated hardware vol/filt register
	filter_cutoff_freq_lo_reg 			Updated hardware filter cutoff low byte
	filter_cutoff_freq_hi_reg 			Updated hardware filter cutoff high byte

Description
	- Rejects non-real voices early: if X is at or beyond the filter voice
	index, returns immediately with no side effects.
	- Saves A, X, and Y, then records X into voice_being_modified so the
	primary voice index can be reused after temporary X changes.
	- Primary multiplexing:
		• Uses alternate slot #0 (Y = 0) to swap main-voice state via
		swap_voice_settings.
		• Calls apply_voice_freq_and_env_to_sid so the swapped-in state
		is committed to SID (frequency and envelope).
	- Secondary multiplexing (when vmux_secondary_active_flag_bff is non-
	zero):
		• Bumps X by +4 to address the partner logical voice, selects
		alternate slot #1 (Y = 1), and swaps its state with the
		alternate copy.
		• Calls apply_voice_freq_and_env_to_sid again to commit the
		partner’s state to SID.
	- Filter multiplexing:
		• If vmux_filter_enabled_flag_bff is TRUE:
			· Swaps filter voice state (X = 3) with alternate slot #2
			(Y = 2), pulling in a precomputed filter snapshot.
			· Re-enables filtering for voice_being_modified by:
				– Starting from filter_control_snapshot,
				– Clearing its lower nibble (filter routing),
				– OR-ing in the voice bit from voice_alloc_set_mask_tbl
				for the recorded voice index,
				– Writing the result to sid_filter_control_shadow and
				sid_filter_control.
			· Restores the upper nibble of the vol/filt register by
			combining the preserved low nibble (volume) from
			sid_volfilt_shadow with filter_mode_mask, then writes
			both sid_volfilt_shadow and sid_master_volume.
			· Commits filter cutoff frequency to the hardware cutoff
			registers using filter_cutoff_freq_lo/hi.
		• If vmux_filter_enabled_flag_bff is FALSE:
			· Clears this voice’s routing bit from
			filter_control_snapshot using voice_alloc_clear_mask_tbl,
			then updates sid_filter_control_shadow and
			sid_filter_control so the voice is no longer filtered.
	- At exit, clears all multiplexing-related flags and filter_mode_mask so
	the next update cycle starts from a clean state, restores A, X, and Y,
	and returns.
================================================================================
*/
* = $5342
apply_multiplexing_and_filter_for_voice:
		// Is this a real voice? If not (filter or virtual one), exit
        cpx     #FILTER_VOICE_INDEX
        bmi     apply_vmux_entry
        rts

apply_vmux_entry:
		// Save registers
        pha
        txa
        pha
        tya
        pha
		// Save voice being modified
        stx     voice_being_modified

        // ------------------------------------------------------------
        // Primary multiplexing: swap logical voice X with alternate slot #0
        // and apply that state to SID via frequency/envelope update.
        // ------------------------------------------------------------
        ldy     #$00
        jsr     swap_voice_settings
        jsr     apply_voice_freq_and_env_to_sid

		// Is secondary multiplexing active? If not, skip this section
        lda     vmux_secondary_active_flag_bff
        beq     check_filter_multiplexing_state

		//Secondary active
		// Update partner voice X+4 using alternate slot #1 and commit that state to SID
        inx
        inx
        inx
        inx

        ldy     #$01
        jsr     swap_voice_settings
        jsr     apply_voice_freq_and_env_to_sid

check_filter_multiplexing_state:
        // ------------------------------------------------------------
        // If filter multiplexing is enabled, update filter from alternate
        // slot #2 and configure filter routing, filter mode, and cutoff.
        // Otherwise ensure the current voice is removed from filtering.
        // ------------------------------------------------------------
		// Is filter multiplexing enabled?
        lda     vmux_filter_enabled_flag_bff
        cmp     #BTRUE
        bne     disable_filter_for_current_voice

		// Filter multiplexing enabled
        // Swap filter with alternate slot #2
        ldx     #$03
        ldy     #$02
        jsr     swap_voice_settings

enable_filter_for_current_voice:
        // ------------------------------------------------------------
        // Enable filtering for voice_being_modified using the snapshot
        // of the filter control value. Voice-bit is ORed in.
        // ------------------------------------------------------------
        lda     filter_control_snapshot
        ldx     voice_being_modified
		
		// Clear lower 4 bits of filter control (filter routing)
        and     #MSK_HIGH_NIBBLE
		
		// Route filter for this voice
        ora     voice_alloc_set_mask_tbl,x
        sta     sid_filter_control_shadow
        sta     sid_filter_control

        // ------------------------------------------------------------
        // Update SID volume/filter register: preserve low nibble (volume)
        // and restore previously latched filter-mode bits.
        // ------------------------------------------------------------
        lda     sid_volfilt_shadow
		
		// Clear upper 4 bits of vol/filt register (filter settings)
        and     #MSK_LOW_NIBBLE
		
		// Set the filter settings bits
        ora     filter_mode_mask
        sta     sid_volfilt_shadow
        sta     sid_master_volume

        // ------------------------------------------------------------
        // Commit filter cutoff frequency to hardware registers.
        // ------------------------------------------------------------
        lda     filter_cutoff_freq_lo
        sta     filter_cutoff_freq_lo_reg
        lda     filter_cutoff_freq_hi
        sta     filter_cutoff_freq_hi_reg
        jmp     amaffv_exit

disable_filter_for_current_voice:
        // ------------------------------------------------------------
        // Disable filtering for this voice by clearing its bit in the
        // filter-control snapshot.
        // ------------------------------------------------------------
        lda     filter_control_snapshot
        ldx     voice_being_modified
        and     voice_alloc_clear_mask_tbl,x
        sta     sid_filter_control_shadow
        sta     sid_filter_control

amaffv_exit:
        // ------------------------------------------------------------
        // Clear all multiplexing and filter state for the next update cycle
        // ------------------------------------------------------------
        lda     #FALSE
        sta     vmux_primary_active_flag_bff
        sta     vmux_secondary_active_flag_bff
        sta     alt_voice_slots_cleared_flag
        sta     filter_mode_mask
        sta     vmux_filter_enabled_flag_bff

		// Restore registers
        pla
        tay
        pla
        tax
        pla
        rts

procedure sound_irq_handler():
    if sound_processing_disabled_flag:
        return

    # Start any newly scheduled sounds once per tick
    if new_sound_instructions_allowed:
        for each logical_voice in all_voices:
            if there is a pending sound for that voice:
                start_sound_for_voice(logical_voice)
        new_sound_instructions_allowed = false

    # If no voices are executing instructions, nothing to do
    if voice_instr_active_mask == 0:
        return

    # Advance timers and glissando for active voices
    for each logical_voice in all_voices:
        if that voice is marked active in voice_instr_active_mask:
            tick_duration_and_glissando(logical_voice)

    # Handle multiplexed slices (primary and optional secondary)
    if vmux_primary_active_flag_bff:
        # Primary multiplex slice on the primary voice
        perform_multiplex_slice(primary_voice)

        if vmux_secondary_active_flag_bff:
            # Secondary multiplex slice on the secondary voice
            perform_multiplex_slice(secondary_voice)

        vmux_active_flag_bff = false

    # Push updated control/filter bits for all active voices to SID
    for each logical_voice in all_voices:
        if that voice is marked active:
            apply_voice_control_to_sid(logical_voice)

    # Run a slice of the music engine if music is active
    if not music_playback_in_progress:
        return

    # Compute which music resource should be playing
    selected = selected_music_idx
    music_to_start_ptr = sound_ptr[selected]

    if music_to_start_ptr == music_in_progress_ptr:
        # Continue current music program
        jump_to_music_code()
    else:
        # Transition to new music program
        setup_music_pointers()   # install new code pointer into trampoline
        jump_to_music_code()


procedure jump_to_music_code():
    # Call whatever address the trampoline currently points to
    call current_music_entrypoint()


procedure reset_sound_engine_state():
    # Silence all SID voices and filter
    clear_all_voice_control_registers()
    clear_filter_routing_and_cutoff()

    # Restore default master volume/filter-mode state
    sid_master_volume  = DEFAULT_VOLUME_AND_FILTER
    sid_volfilt_shadow = DEFAULT_VOLUME_AND_FILTER

    # Refresh internal music pointers (current selection → in-progress)
    setup_music_pointers()

    # Stop all logical voices uniformly
    for each logical_voice in all_voices:
        stop_logical_voice(logical_voice)

    # Reset music flags and critical refcounts
    music_playback_in_progress = false
    clear_refcount_of_sounds_1_and_2()

    music_voices_in_use   = 0
    music_voices_in_use_2 = 0

    # All real SID voices are now available
    total_real_voices_available = 3

    # Clear multiplex / engine state flags
    vmux_primary_active_flag_bff   = false
    vmux_filter_enabled_flag_bff   = false
    vmux_secondary_active_flag_bff = false

    # “Music in progress” now matches “music to start”
    music_in_progress_ptr = music_to_start_ptr


/*

procedure start_sound_for_voice(logical_voice):
    sound_id = snd_to_start_on_voice[logical_voice]
    voice_sound_id_tbl[logical_voice] = sound_id
    snd_to_start_on_voice[logical_voice] = NO_SOUND_PENDING

    # Reset repetition state for this voice
    voice_instr_repcount[logical_voice] = 0

    # Classify as real vs filter vs virtual
    is_physical_voice_flag_bff =
        (logical_voice is a real SID voice index)

    # Initialize loop-offset from precomputed offsets for this sound/voice
    voice_instr_loop_ofs[logical_voice] = voice_data_offsets[logical_voice]

    logical_voice_idx = logical_voice

    # Ensure the sound resource is resident and find its base address
    result = update_voice_stream_pointers()
    if result == NOT_IN_MEMORY:
        # Resource vanished: clean up the sound and abort start
        stop_sound_full_cleanup(sound_id)
        return ERROR_NOT_LOADED

    base = current_sound_base_for_this_voice()  # from update_voice_stream_pointers

    # Initialize the current PC and read pointer as base + loop-offset
    voice_base_addr[logical_voice] = base
    voice_instr_pc[logical_voice]  = base + voice_instr_loop_ofs[logical_voice]
    voice_read_ptr                 = voice_instr_pc[logical_voice]

    # Handle per-voice initial parameters:
    if logical_voice is virtual_voice:
        # Virtual voices skip initial control/filter setup
        param_bytes_consumed = 0

    else if logical_voice is filter_voice:
        # Filter slot: refresh filter control + volume from the stream
        update_filter_and_volume(start_offset = 0)
        param_bytes_consumed = bytes_used_by_filter_update

    else:
        # Real SID voice
        if this voice is reserved by music:
            # Do not disturb music’s control/filter; just skip a dummy byte
            param_bytes_consumed = 1
        else:
            # Read control byte and per-voice filter routing nibble
            control, filter_nibble = read_initial_voice_parameters()
            voice_ctrl_shadow[logical_voice] = control

            update_filter_routing_for_voice(logical_voice, filter_nibble)

            param_bytes_consumed = 2

    # Move PC/loop-offset past the parameter bytes we just consumed
    advance_instruction_pc_and_loop_offset(param_bytes_consumed)

    # Mark this voice as actively executing instructions
    mark_voice_active(logical_voice)

    # Decode and apply the first instruction for this voice
    decode_voice_instruction()

    return SUCCESS


procedure update_instruction_ptr_and_offset(bytes_consumed):
    v = logical_voice_idx

    # The voice’s current PC and loop-offset both advance by the
    # same number of bytes the decoder just consumed for this instruction.
    voice_instr_pc[v]        += bytes_consumed
    voice_instr_loop_ofs[v]  += bytes_consumed


procedure decode_voice_instruction():
    v = logical_voice_idx

    # Ensure the backing sound resource is still in memory; fix pointers if moved
    result = update_voice_stream_pointers()
    if result == NOT_IN_MEMORY:
        stop_sound_full_cleanup(voice_sound_id_tbl[v])
        return
    # If result == RELOCATED, the PC and base are already adjusted

    # Read from the current PC
    voice_read_ptr = voice_instr_pc[v]

    # Fetch instruction header byte
    header = read_byte(voice_read_ptr, 0)

    # Special case: header=0 => stop voice immediately
    if header == 0:
        voice_instr_repcount[v] = 0
        stop_voice_now(v)      # clear active mask, gate, etc.
        return

    # Keep two copies: one stable, one for shifting bits out
    voice_instr_header[v] = header
    instr_header_raw      = header
    instr_bit_cursor      = header

    bytes_consumed = 1  # header itself

    # ---------------- Gate (bit 0, real voices only) ----------------
    gate_bit = header.bit0
    if is_physical_voice_flag_bff:
        apply_gate_bit_to_control(v, gate_bit)

    # ---------------- Stop (bit 1) ----------------
    if header.bit1 == 1:
        stop_sound_cleanup_mode = FULL_CLEANUP
        stop_logical_voice(v)
        return

    # ---------------- Freq/duration/gliss (bit 2 + bit 6) ----------
    if header.bit2 == 1:
        # Always update frequency from the stream
        freq = read_next_16bit_value(voice_read_ptr, bytes_consumed)
        voice_freq[v] = freq
        bytes_consumed += 2

        if header.bit6 == 1:
            # No duration/gliss; just clear them
            clear_voice_duration_and_gliss(v)
        else:
            # Read duration and glissando vectors
            duration = read_next_16bit_value(voice_read_ptr, bytes_consumed)
            gliss    = read_next_16bit_value(voice_read_ptr, bytes_consumed + 2)
            voice_duration[v] = duration
            voice_gliss[v]    = gliss
            bytes_consumed    += 4
    else:
        # bit2 = 0 → no new freq; clear duration/gliss
        clear_voice_duration_and_gliss(v)

    # ---------------- ADSR (bit 3, real voices only) ---------------
    if is_physical_voice_flag_bff and header.bit3 == 1:
        # Temporarily drop gate, push control, then read new ADSR
        temporarily_gate_off_and_commit(v)
        adsr = read_two_bytes(voice_read_ptr, bytes_consumed)
        store_adsr_for_voice(v, adsr)
        bytes_consumed += 2
        restore_gate_from_header_and_commit(v, header)

    # ---------------- Operand sub-block (bit 4) --------------------
    if header.bit4 == 1:
        op_bits = read_byte(voice_read_ptr, bytes_consumed)
        bytes_consumed += 1

        # PWM (op bit 0, real voices only)
        if is_physical_voice_flag_bff and op_bits.bit0 == 1:
            pwm = read_next_16bit_value(voice_read_ptr, bytes_consumed)
            program_pwm_for_voice(v, pwm)
            bytes_consumed += 2

        # Filter/volume/cutoff (op bit 1)
        if op_bits.bit1 == 1:
            # This helper reads filter control + volume,
            # then we read and apply a cutoff pair
            bytes_used = update_filter_and_volume(bytes_consumed)
            bytes_consumed += bytes_used

            cutoff = read_next_16bit_value(voice_read_ptr, bytes_consumed)
            set_filter_cutoff(cutoff)
            bytes_consumed += 2

        # Duration override (op bit 2)
        if op_bits.bit2 == 1:
            clear_gliss_only(v)
            duration2 = read_next_16bit_value(voice_read_ptr, bytes_consumed)
            voice_duration[v] = duration2
            bytes_consumed += 2

    # ---------------- Waveform (bit 5) -----------------------------
    if header.bit5 == 1:
        wave_byte = read_byte(voice_read_ptr, bytes_consumed)
        bytes_consumed += 1
        merge_waveform_into_control(v, wave_byte)

    # ---------------- Repetition (bit 7) ---------------------------
    if header.bit7 == 0:
        # No repetition: just advance PC/offset and exit
        update_instruction_ptr_and_offset(bytes_consumed)
        return

    # Bit 7 = 1 → repeated instruction with backward jump
    # At the tail of this instruction we have:
    #   [offset][repeat_count]
    offset       = read_byte(voice_read_ptr, bytes_consumed)
    repeat_count = read_byte(voice_read_ptr, bytes_consumed + 1)

    if voice_instr_repcount[v] == 0:
        # First time we see this instruction’s R-block: initialize counter
        voice_instr_repcount[v] = repeat_count
    else:
        # Subsequent pass: decrement remaining count
        voice_instr_repcount[v] -= 1

    if voice_instr_repcount[v] == 0:
        # All repetitions done → skip offset+count and move on
        bytes_consumed += 2
        update_instruction_ptr_and_offset(bytes_consumed)
        # Decode next instruction normally
        decode_voice_instruction()
        return
    else:
        # More repetitions left → jump backwards by offset bytes
        move_pc_and_loop_offset_backwards(v, offset)
        # Re-decode this same instruction at the new PC
        decode_voice_instruction()
        return


procedure swap_voice_settings(main_voice, alt_slot):
    # Swap all per-voice state fields between:
    #   main_voice  ↔  alternate slot alt_slot

    if main_voice is real SID voice:
        swap(voice_adsr[main_voice], alt_adsr[alt_slot])

    swap(voice_data_base[main_voice],   alt_data_base[alt_slot])
    swap(voice_priority[main_voice],    alt_priority[alt_slot])
    swap(voice_sound_id_tbl[main_voice], alt_sound_id_tbl[alt_slot])

    swap(voice_instr_header[main_voice],   alt_instr_header[alt_slot])
    swap(voice_instr_repcount[main_voice], alt_instr_repcount[alt_slot])

    swap(voice_ctrl_shadow[main_voice], alt_ctrl_shadow[alt_slot])

    swap(voice_instr_pc[main_voice],      alt_instr_pc[alt_slot])
    swap(voice_freq[main_voice],          alt_freq[alt_slot])
    swap(voice_duration[main_voice],      alt_duration[alt_slot])
    swap(voice_gliss[main_voice],         alt_gliss[alt_slot])
    swap(voice_base_addr[main_voice],     alt_base_addr[alt_slot])
    swap(voice_instr_loop_ofs[main_voice], alt_instr_loop_ofs[alt_slot])


procedure clear_all_alternate_settings():
    # Zero out the entire alternate-voice state region
    for each byte_in_alt_block in all_bytes_of_alt_state:
        byte_in_alt_block = 0


procedure process_multiplexing(logical_voice):
    # Only voices 0–3 are relevant; skip others
    if logical_voice >= FIRST_MULTIPLEXED_VOICE_INDEX:
        return

    if logical_voice < FILTER_VOICE_INDEX:
        # Real SID voices (0–2)

        # Clear alt slots once per tick if not already done
        if not alt_voice_slots_cleared_flag:
            clear_all_alternate_settings()
            alt_voice_slots_cleared_flag = true

        # Primary multiplex: snapshot main voice into alt slot 0
        swap_voice_settings(logical_voice, alt_slot = 0)

        # Secondary multiplex: if the paired virtual voice is active,
        # snapshot it into alt slot 1 and mark secondary multiplex active
        partner = logical_voice + 4
        if voice_is_active(partner):
            swap_voice_settings(partner, alt_slot = 1)
            vmux_secondary_active_flag_bff = true

    else:
        # Filter voice (logical index 3)

        # Save current filter mode bits (upper nibble of volume/filter register)
        filter_mode_mask = extract_filter_mode_from_volume_shadow()

        clear_all_alternate_settings()
        alt_voice_slots_cleared_flag = true

        # Snapshot filter voice into alt slot 2
        swap_voice_settings(filter_voice_index, alt_slot = 2)

        # Mark filter multiplexing active
        vmux_filter_enabled_flag_bff = true

    # In either case, primary multiplex is now active for this tick
    vmux_primary_active_flag_bff = true



procedure apply_multiplexing_and_filter_for_voice(logical_voice):
    # Only real SID voices 0–2 participate
    if logical_voice >= FILTER_VOICE_INDEX:
        return

    voice_being_modified = logical_voice

    # Primary multiplex slice:
    #   Use snapshot in alt slot 0, then restore
    swap_voice_settings(logical_voice, alt_slot = 0)
    apply_voice_freq_and_env_to_sid(logical_voice)   # commit pitch & env
    swap_voice_settings(logical_voice, alt_slot = 0) # restore base state

    # Secondary multiplex slice (if active):
    if vmux_secondary_active_flag_bff:
        partner = logical_voice + 4
        swap_voice_settings(partner, alt_slot = 1)
        apply_voice_freq_and_env_to_sid(partner)
        swap_voice_settings(partner, alt_slot = 1)

    # Filter multiplexing logic
    if vmux_filter_enabled_flag_bff:
        # Swap filter snapshot from alt slot 2 into the filter voice
        swap_voice_settings(filter_voice_index, alt_slot = 2)

        # Route filter to the primary voice that initiated multiplexing
        route_filter_to_voice(voice_being_modified)

        # Apply stored filter mode bits and cached cutoff/volume
        apply_filter_mode_and_cutoff(filter_mode_mask)
    else:
        # Ensure this voice is not routed through the filter
        clear_filter_routing_for_voice(voice_being_modified)

*/