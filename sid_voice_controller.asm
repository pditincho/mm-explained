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
