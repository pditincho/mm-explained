#importonce

.const STOP_SOUND_MODE_FULL       = $FF     // Full stop/cleanup mode value for stop_sound_cleanup_mode
.const VOICE_GATE_MASK            = $01     // Gate bit mask for voice_ctrl_shadow (1=trigger, 0=release)
.const VOICE_GATE_CLEAR_MASK      = $FE     // Inverted gate mask (AND to clear gate bit in voice_ctrl_shadow)
.const ARPEGGIO_VOICE_LIMIT         = $04    // Voices with X >= this are not processed for multiplexing
.const FILTER_VOICE_INDEX           = $03    // Logical voice index that uses the filter/multiplexing path
.const SID_FILTER_MODE_MASK         = $70    // Mask for SID filter mode bits (low/band/high-pass in $D418)

.const MAX_VOICE_INDEX  = $06    
.const FIRST_MULTIPLEXED_VOICE_INDEX = $04
.const OFS_VOICE_REQS = $05
.const UPD_VCE_RSLT_NO_RELOCATION   = $00    // Resource present, base unchanged; PC already valid
.const UPD_VCE_RSLT_NOT_IN_MEMORY   = $01    // Resource missing; sound stopped and bookkeeping updated
.const UPD_VCE_RSLT_RELOCATED       = $FF    // Resource present but moved; base and PC recomputed
.const VOICE_CTRL_CLEAR_GATE_WAVE_MASK      = $0E    // Mask to clear gate + waveform bits (keep only bits 1–3)
.const FILTER_ROUTING_MASK              = $07    // SID filter-control mask: bits 0–2 (voice1/2/3 → filter)

* = $4755
voice_freq_reg_ofs_tbl:
        // Per-voice base offsets into the SID frequency register pairs.
        // Indexed by logical voice index; used to locate the correct
        // SID FREQ LO/HI register pair for that voice.
        .byte $00, $07, $0E, $15

* = $4759
voice_pwm_reg_ofs_tbl:
        // Per-voice base offsets into the SID PWM (pulse width) register
        // pairs. Indexed by logical voice index when programming PWM.
        .byte $02, $09, $10

* = $475C
voice_alloc_set_mask_tbl:
        // Bitmasks for marking voices as allocated. Entry N corresponds
        // to (1 << N); AND/OR with voices_allocated to set the slot’s bit.
        .byte $01, $02, $04, $08, $10, $20, $40

* = $4763
voice_alloc_clear_mask_tbl:
        // Bitmasks for clearing voice allocation bits. Entry N is the
        // complement of (1 << N), so ANDing with it frees that voice.
        // The final $00 entry is used as a sentinel / “no mask” value.
        .byte $FE, $FD, $FB, $F7, $EF, $DF, $BF, $00
