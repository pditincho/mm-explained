/*
================================================================================
SID voice controller — per-voice state → SID registers
================================================================================

Summary
    This module is the thin bridge between the sound engine’s per-voice state
    and the actual SID hardware registers. It owns the routines that translate
    cached voice parameters (frequency, ADSR, control bits, filter/volume) into
    concrete writes to the SID, and the per-tick logic that advances duration
    counters and glissando for each logical voice.

Core responsibilities
    • Filter control and master volume
        - Read filter-control and master volume bytes from the current voice
          script stream.
        - Merge filter flags into a software shadow of the SID filter-control
          register and commit it to hardware.
        - Cache the master volume/filter byte and write it to the SID’s
          volume/filter register so later code can reason about both volume
          and filter mode bits.

    • Duration and glissando timing
        - Maintain a per-voice duration counter that determines how long the
          current instruction’s settings remain in effect.
        - Maintain a per-voice glissando delta that is added to the cached
          frequency each tick while the duration is non-zero.
        - On each tick, decrement the duration; if it is still positive,
          apply one glissando step and update the SID pitch. When the counter
          expires, trigger a decode of the next instruction and then refresh
          frequency/ADSR on the SID.

    • Per-voice reset helpers
        - Provide a helper to clear both duration and glissando in one step,
          used by the instruction decoder when a script explicitly resets
          timing and pitch-slide state for a voice.

    • Frequency and envelope commit
        - Given a logical voice index, locate the corresponding SID register
          pair via a small offset table.
        - Push the cached per-voice frequency into the correct SID frequency
          registers (or into the filter cutoff path for the special filter
          voice).
        - For real SID voices, also push the cached ADSR values into the
          SID’s ADSR registers, keeping the software copies and hardware in
          sync.

    • Control register commit
        - For real SID voices, push the cached control byte (gate, waveform
          selection, and related control bits) into the voice’s SID control
          register.
        - Respect multiplexing flags so that, when time-multiplexing is
          active, control and pitch updates are performed by the multiplexing
          layer rather than by the normal per-voice path.

Interaction with other modules
    • The sound engine:
        - Uses the duration/glissando helpers on each IRQ tick to advance
          voice timing, apply glissando, and trigger instruction decoding
          when timers expire.
        - Calls the commit helpers after decoding to make sure any new pitch,
          envelope, or control settings are reflected on the SID.

    • The voice scheduler / multiplexing:
        - Marks which logical voices map to real SID voices and which are
          virtual or filter-only, so this module can skip ADSR/control writes
          where no physical SID voice exists.
        - Uses multiplexing flags to control when frequency/ADSR updates are
          driven directly by this module vs when they are owned by the
          multiplexing layer.
================================================================================
*/

#importonce

#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "sound_constants.inc"
#import "sound_engine.asm"
#import "music.asm"

/*
================================================================================
  update_filter_and_volume
================================================================================
Summary
	Read filter and master volume parameters from the voice script
	stream and apply them to the SID filter control and master volume
	registers, keeping software shadows in sync.

Arguments
	Y       Offset from voice_read_ptr for the filter/volume parameter bytes in 
			the current voice script

Global Inputs
	voice_read_ptr             	Base pointer to current voice script
	sid_filter_control_shadow   Previous cached SID filter control byte
	sid_volfilt_shadow    	   	Previous cached SID master volume byte

Global Outputs
	sid_master_volume          	SID master volume hardware register
	sid_filter_control    	 	SID filter control hardware register
	sid_volfilt_shadow    	   	Updated cached master volume byte
	sid_filter_control_shadow   Updated merged filter control shadow

Description
	- Reads a filter/control byte from the voice script via (voice_read_ptr),Y
	and merges it (OR) into sid_filter_control_shadow.
	- Writes the merged filter byte both to the shadow copy and to the SID filter 
	control register.
	- Advances Y and reads a master volume/filter control byte from the script, 
	storing it into sid_volfilt_shadow.
	- Applies the same byte to the SID master volume register so hardware and 
	shadow remain consistent.
================================================================================
*/
* = $49E7
update_filter_and_volume:
		// ------------------------------------------------------------
		// Update SID filter control using next byte from voice script
		// (merge new flags with existing shadow and commit to SID)
		// ------------------------------------------------------------
		lda     (voice_read_ptr),y      	// Read filter/flags byte from current voice script position
		ora     sid_filter_control_shadow 	// Combine new filter bits with existing shadow state
		sta     sid_filter_control_shadow 	// Update shadow copy used by subsequent updates
		sta     sid_filter_control   		// Commit combined value to SID filter control register
		iny                             	// Advance to next script byte (master volume)
		
		// ------------------------------------------------------------
		// Update SID master volume from voice script and cache in shadow
		// ------------------------------------------------------------
		lda     (voice_read_ptr),y      	// Read master volume byte from voice script
		sta     sid_volfilt_shadow   		// Cache volume in software copy for later reference
		sta     sid_master_volume       	// Apply volume to SID master volume hardware register
		rts                               
/*
================================================================================
  tick_duration_and_glissando
================================================================================
Summary
	Tick the per-voice duration timer for logical voice X and either
	apply a glissando step to its cached frequency while time remains
	or, when the timer expires, advance the voice script and refresh
	its frequency/envelope instead.

Arguments
	X       					Logical voice index

Vars/State
	is_physical_voice_flag_bff	Cached boolean flag indicating X < LOGICAL_VOICE_LIMIT
	x_saved          			Last logical voice index processed and saved for reuse

Global Inputs
	voice_duration_lo        	Per-voice duration counter low byte table
	voice_duration_hi        	Per-voice duration counter high byte table
	voice_gliss_lo             	Per-voice glissando delta low byte table
	voice_gliss_hi             	Per-voice glissando delta high byte table
	voice_freq_lo     			Cached per-voice frequency low byte table
	voice_freq_hi     			Cached per-voice frequency high byte table

Global Outputs
	voice_duration_lo        	Updated 16-bit duration counter low byte
	voice_duration_hi        	Updated 16-bit duration counter high byte
	voice_freq_lo     			Glissando-adjusted frequency low byte
	voice_freq_hi     			Glissando-adjusted frequency high byte

Description
	- Updates a 16-bit duration counter for logical voice X by decrementing 
	the low byte and propagating borrow into the high byte when needed.
	- If the resulting duration is still non-zero, applies one glissando delta 
	step to the cached 16-bit frequency and then commits the new pitch 
	via apply_voice_freq_and_env_to_sid.
	- If the duration underflows to zero, calls	decode_voice_instruction to 
	advance the voice script and then refreshes the voice’s frequency/envelope 
	instead of applying	further glissando.
	- Records whether X is within the primary SID voice range in is_physical_voice_flag_bff 
	and saves X into x_saved for use by other engine code that needs to know 
	which voice was last processed.
================================================================================
*/
* = $4A1B
tick_duration_and_glissando:
		// ------------------------------------------------------------
		// Flag whether voice X is a physical voice
		// ------------------------------------------------------------
		lda     #BTRUE                			// Provisional “true” for X < 3
		cpx     #LOGICAL_VOICE_LIMIT        	// Compare logical voice index against 3
		bmi     store_voice_index_flag      	// If X < 3 → keep true
		
		lda     #FALSE                      	// Else X >= 3 → flag false
store_voice_index_flag:
		sta     is_physical_voice_flag_bff       

		// Preserve caller’s X (logical voice index) across any helper calls
		stx     x_saved

		// ------------------------------------------------------------
		// Duration countdown (16-bit)
		//
		// Decrement countdown as a 16-bit value. If the final result is still
		// non-negative (C=1), the duration remains > 0 and we continue by
		// applying a glissando step; otherwise the timer has just expired.
		// ------------------------------------------------------------
		// Decrement low duration byte; carry tells whether it underflowed
		lda     voice_duration_lo,x           
		sec                                   
		sbc     #$01                          
		sta     voice_duration_lo,x           
		bcs     apply_glissando_step          	// If no borrow (C=1) → duration still > 0

		// Low byte underflowed → propagate borrow into high duration byte
		lda     voice_duration_hi,x           
		sbc     #$00                          
		sta     voice_duration_hi,x           
		bcs     apply_glissando_step          	// If still non-negative → duration not yet expired

		// ------------------------------------------------------------
		// Duration reached zero
		//
		// Advance this voice’s script to the next instruction and then 
		// recompute/commit its new frequency and envelope to the SID chip. 
		// No further glissando is applied on this tick.
		// ------------------------------------------------------------
		jsr     decode_voice_instruction    	// Fetch and execute next voice instruction(s)
		jsr     apply_voice_freq_and_env_to_sid // Apply new frequency/ADSR after instruction change
		rts                                   

apply_glissando_step:
		// ------------------------------------------------------------
		// Duration non-zero - countdown still ongoing
		// ------------------------------------------------------------
		// Apply one signed glissando step to the cached frequency
		lda     voice_freq_lo,x        
		clc                                   
		adc     voice_gliss_lo,x                
		sta     voice_freq_lo,x        

		lda     voice_freq_hi,x        
		adc     voice_gliss_hi,x                
		sta     voice_freq_hi,x        

		// Push new frequency to SID (and ADSR if applicable)
		jsr     apply_voice_freq_and_env_to_sid 
		rts                                   	
/*
================================================================================
  reset_voice_duration_and_glissando
================================================================================
Summary
	Reset the per-voice duration counter and glissando delta so the
	current logical voice X has no remaining play time and no active
	pitch slide.

Arguments
	X       				Logical voice index whose duration and glissando are cleared

Global Outputs
	voice_duration_lo/hi       Cleared to zero for voice X
	voice_gliss_lo/hi          Cleared to zero for voice X

Description
	- Writes zero into the 16-bit duration counter for logical voice X.
	- Writes zero into the 16-bit glissando delta for logical voice X.
	- Effectively disables both countdown timing and pitch slide for
	this voice so subsequent ticks see no residual state.
================================================================================
*/
* = $4A5C
reset_voice_duration_and_glissando:
		// ------------------------------------------------------------
		// Clear duration and glissando for current voice slot X
		// ------------------------------------------------------------
		lda     #$00
		sta     voice_duration_lo,x
		sta     voice_duration_hi,x
		sta     voice_gliss_lo,x
		sta     voice_gliss_hi,x
		rts

/*
================================================================================
  apply_voice_freq_and_env_to_sid
================================================================================
Summary
        Commit the cached 16-bit frequency (or filter cutoff for the special
        logical slot) and ADSR envelope for logical voice X to the SID.
        When voice multiplexing is active, the routine performs no updates because the
        multiplexing engine owns all pitch changes during its operation.

Arguments
        X       Logical voice index

Global Inputs
        vmux_active_flag_bff       	Indicates whether multiplexing is currently active
        voice_freq_reg_ofs_tbl  	Per-voice SID base register offsets
        voice_freq_lo         		Cached low-byte frequency/cutoff table
        voice_freq_hi         		Cached high-byte frequency/cutoff table
        voice_adsr_attack_decay     Cached attack/decay envelope values
        voice_adsr_sustain_release  Cached sustain/release envelope values

Global Outputs
        voice1_freq_reg_lo          SID frequency low-byte registers (voice-relative)
        voice1_freq_reg_hi          SID frequency high-byte registers (voice-relative)
        voice1_attack_delay_reg     SID attack/decay registers (voice-relative)
        voice1_sustain_release_reg  SID sustain/release registers (voice-relative)

Description
        - If vmux_active_flag_bff is nonzero, exit immediately to preserve the
          multiplexing engine’s pitch updates.
        - Resolve the SID register base offset for logical voice X.
        - Write the cached 16-bit frequency into the SID registers; for the
          special logical slot (X == 3), this is interpreted as a filter cutoff 
		  instead of a tone frequency.
        - For logical voices below LOGICAL_VOICE_LIMIT, also update the ADSR
          envelope using the cached attack/decay and sustain/release values.
================================================================================
*/
* = $4BE6
apply_voice_freq_and_env_to_sid:
		// ------------------------------------------------------------
		// Multiplexing guard
		//
		// Skip frequency/ADSR commit when multiplexing is active; multiplexing logic
		// owns pitch changes while the effect is running, so only update
		// registers when no multiplexing is in progress
		// ------------------------------------------------------------
		// Multiplexing active? If so, exit
		lda     vmux_active_flag_bff           
		beq     update_freq_env_no_vmux
		rts                                   	

update_freq_env_no_vmux:
		// Resolve voice register offset
		lda     voice_freq_reg_ofs_tbl,x 		// A := base offset for this voice in SID register map
		tay                                   	// Y := per-voice register offset

		// ------------------------------------------------------------
		// Commit frequency/filter cutoff
		//
		// Push cached 16-bit pitch value for logical voice X into SID:
		// for voices 0–2 this is the tone frequency; for the special
		// logical slot (X == 3) the same value is used as filter cutoff
		// ------------------------------------------------------------
		lda     voice_freq_lo,x        
		sta     voice1_freq_reg_lo,y          
		lda     voice_freq_hi,x        
		sta     voice1_freq_reg_hi,y          

		// ------------------------------------------------------------
		// Physical voice guard
		//
		// Only voices < 3 have a full ADSR envelope mapped; 
		// treat X >= 3 as a filter-only slot and exit
		// ------------------------------------------------------------
		// Is the voice a physical voice? If not, exit
		cpx     #LOGICAL_VOICE_LIMIT          
		bpl     exit_voice_freq_env           	// X >= 3 → skip ADSR transfer and exit

		// ------------------------------------------------------------
		// Commit envelope (ADSR)
		// ------------------------------------------------------------
		lda     voice_adsr_attack_decay,x         
		sta     voice1_attack_delay_reg,y     
		lda     voice_adsr_sustain_release,x      
		sta     voice1_sustain_release_reg,y  

exit_voice_freq_env:
		rts                                   
/*
================================================================================
  apply_voice_control_to_sid
================================================================================
Summary
	Write the cached control byte for logical voice X to the appropriate
	SID voice control register, if that logical voice is mapped to a
	physical SID channel.

Arguments
	X       Logical voice index

Global Inputs
	voice_freq_reg_ofs_tbl   	Per-voice SID register base offsets
	voice_ctrl_shadow           Cached SID control bytes per logical voice

Global Outputs
	voice1_control_register     SID voice control register block

Description
	- Rejects logical voices X >= LOGICAL_VOICE_LIMIT (no mapped SID voice).
	- Looks up the base SID register offset for logical voice X.
	- Writes the cached control value for X into the corresponding
	SID voice control register.
================================================================================
*/
* = $4C0D
apply_voice_control_to_sid:
		// ------------------------------------------------------------
		// Physical voice guard
		//
		// Voice >= 3? If so, exit
		// ------------------------------------------------------------
		cpx     #LOGICAL_VOICE_LIMIT           
		bpl     exit_apply_voice_control_to_sid 

		// Resolve voice register offset
		lda     voice_freq_reg_ofs_tbl,x  		// A := base offset for this voice’s SID register block
		tay                                    	// Y := offset into SID register space

		// ------------------------------------------------------------
		// Commit control value
		// ------------------------------------------------------------
		lda     voice_ctrl_shadow,x            	
		sta     voice1_control_register,y      	

exit_apply_voice_control_to_sid:
		rts                                    

/*
procedure update_filter_and_volume(offset_from_voice_read_ptr):
    # Read filter/control flags from the current voice script byte
    filter_flags = read_script_byte(voice_read_ptr, offset_from_voice_read_ptr)

    # Merge new filter flags into the existing filter-control shadow
    sid_filter_control_shadow |= filter_flags

    # Commit merged filter flags to the SID filter-control register
    sid_filter_control = sid_filter_control_shadow

    # Move to the next byte in the script (volume / vol+filter byte)
    offset_from_voice_read_ptr += 1

    # Read master volume/filter-mode byte from the script
    volfilt_byte = read_script_byte(voice_read_ptr, offset_from_voice_read_ptr)

    # Cache and commit master volume/filter state
    sid_volfilt_shadow = volfilt_byte
    sid_master_volume  = volfilt_byte


procedure tick_duration_and_glissando(logical_voice):
    # Flag whether this logical voice maps to a physical SID voice
    is_physical_voice = (logical_voice < LOGICAL_VOICE_LIMIT)

    # Decrement the voice’s duration counter
    new_duration = voice_duration[logical_voice] - 1

    if new_duration > 0:
        # Duration still running → store it and apply one glissando step
        voice_duration[logical_voice] = new_duration

        # Apply signed frequency delta
        voice_freq[logical_voice] += voice_gliss[logical_voice]

        # Commit updated pitch (and envelope, if applicable) to SID
        apply_voice_freq_and_env_to_sid(logical_voice)
        return

    # Duration reached zero or became negative:
    #   clear or ignore the old value; advance script and refresh state
    voice_duration[logical_voice] = 0

    # Decode the next instruction(s) for this voice, which may change
    # frequency, envelope, gate, etc.
    decode_voice_instruction()

    # After script advancement, commit the new frequency/envelope
    apply_voice_freq_and_env_to_sid(logical_voice)


procedure reset_voice_duration_and_glissando(logical_voice):
    # Clear duration countdown for this voice
    voice_duration[logical_voice] = 0

    # Clear glissando delta so no further pitch sliding occurs
    voice_gliss[logical_voice] = 0


procedure apply_voice_freq_and_env_to_sid(logical_voice):
    # When multiplexing is active, pitch/ADSR updates are owned by
    # the multiplexing engine; do nothing here.
    if vmux_active:
        return

    # Look up the SID register base for this logical voice
    sid_offset = voice_freq_reg_ofs_tbl[logical_voice]

    # Commit cached frequency (or filter cutoff) to SID
    freq = voice_freq[logical_voice]
    write_sid_frequency_or_cutoff(sid_offset, freq)

    # Only “physical” voices (0–2) have ADSR envelopes
    if logical_voice >= LOGICAL_VOICE_LIMIT:
        # For the filter voice, stop here
        return

    # Commit cached ADSR envelope to SID
    attack_decay   = voice_adsr_attack_decay[logical_voice]
    sustain_release = voice_adsr_sustain_release[logical_voice]

    write_sid_adsr(sid_offset, attack_decay, sustain_release)


procedure apply_voice_control_to_sid(logical_voice):
    # Only physical voices (0–2) have a mapped SID control register
    if logical_voice >= LOGICAL_VOICE_LIMIT:
        return

    # Resolve SID register base for this voice
    sid_offset = voice_freq_reg_ofs_tbl[logical_voice]

    # Write cached control byte (gate, waveform bits, etc.) to SID
    ctrl = voice_ctrl_shadow[logical_voice]
    write_sid_control(sid_offset, ctrl)
*/