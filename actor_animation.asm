/*
================================================================================
Actor animation system
================================================================================
Terminology
		- A limb is an independently animated body part.
		- A cel is one static drawing for a limb (a single frame).
		- A cel sequence is an ordered list of cels that form one animation
		  cycle for that limb.
		- A clip set (or simply clip) combines one cel sequence per limb
		  to create a complete full-body motion or stance, such as "walk" or "stand".

================================================================================

Summary
    Drives per-limb cel animation for actors using clip sets. Each clip set
    selects one cel sequence per limb, optionally flipped, and the module steps
    those sequences frame by frame with configurable loop semantics while
    keeping actor facing and speaking state in sync. 

    The module turns high-level "clip sets" (walk, stand, speak, etc.) into
    per-limb cel animation for each actor. A clip set is an 8-entry record that
    says, for each limb, which cel sequence to use and whether that limb should
    be horizontally flipped. At runtime, the system binds a clip to an actor,
    initializes limb state from that clip, and then steps each limb’s animation
    once per frame.


Data model
    • Clips and variants
        - A clip ID identifies a pose/motion for the whole body.
        - Each clip expands into 8 limb entries in clip_tbl:
            · bit7 = flip flag (0 = normal, 1 = horizontally flipped)
            · bits6–0 = cel-sequence index for that limb
            · $FF = limb inactive for this clip
        - Direction (left/right/up/down) and mouth (open/closed) are expressed
          as small "variant" components added to a base clip ID.

    • Cel sequences
        - cel_seq_tbl encodes all cel sequences for the active actor.
        - For each limb:
            · limb_current_cel_seq selects which cel sequence to use.
            · limb_frame_idx is the frame index inside that sequence.
            · limb_current_cel is the resolved cel index for rendering.
        - Sequences use END_FRAME_SENTINEL ($FF) to mark the end frame.

    • Looping and stop semantics
        - limb_cur_loop_cnt tracks how many loops remain for that limb:
            · 0      → animation stops at the last valid frame.
            · $FF    → ANIM_LOOP_FOREVER (loop indefinitely).
            · other  → finite loop count, decremented each time the sequence ends.

    • Ownership and facing
        - Costumes are "owned" by actors via actor_for_costume.
        - Clips are attached at actor level (actor_target_clip, actor_current_clip).
        - Standing clips update facing_direction_for_actor so facing is in sync
          with the chosen standing pose.

================================================================================

    Each actor is made of up to 8 limbs (head, arms, legs, etc.). 
	A "clip" is just a pose or animation (like "stand", "walk", "talk"), with
	four direction variants (left/right/up/down), that tells each limb 
	which mini-animation (cel sequence) it should play and whether it should be flipped.

    Every frame, this file’s code:
        1) Decides which clip the actor should be using.
        2) Makes sure all tables and pointers for that actor are ready.
        3) Walks through each limb and nudges its animation one step forward.
        4) Marks the actor as "needs redraw" if anything actually changed.

High-level step-by-step
    1) step_actor_limb_animation
        • Called once per frame for the current actor.
        • Makes sure the costume and clip data for this actor are set up.
        • Picks a speaking clip variant based on:
            - which way the actor is facing
            - whether the mouth is open or closed
        • Caches the cel sequence table pointer so all limbs can look up frames.
        • Computes a base index so all limb state for this actor can be found
          with (base + limb_index).
        • Loops over all 8 limbs and, for each one:
            - applies any pending change of cel sequence
            - steps the animation frame forward by one

    2) init_limb_state_from_clip_set
        • Takes the chosen clip ID for this actor and expands it into per-limb
          settings:
            - which cel sequence each limb should use
            - whether that limb is flipped or not
            - how many times it’s allowed to loop
        • It does this by looking at the clip table, which has 8 entries per clip
          (one entry per limb).
        • If the clip didn’t actually change, it does nothing.

    3) apply_pending_cel_sequence
        • For the limb currently being processed, checks if there is a new
          sequence requested (target) that is different from the current one.
        • If not, it returns immediately.
        • If yes:
            - it switches to the new sequence
            - seeds the limb’s loop counter
            - resolves which cel index to show first
            - resets the limb’s frame index to 0
        • It also sets a flag so the renderer knows this actor must be redrawn.

    4) step_limb_frame
        • Takes the limb’s current frame index and tries to move to the next one.
        • Uses a combination of:
            - the limb’s base cel index
            - the frame index
          to look up which cel should be shown from the cel sequence table.
        • If the lookup does NOT hit the end-of-sequence marker:
            - the new frame index is kept
            - if the frame index changed, the actor gets flagged for redraw.
        • If the lookup DOES hit the end marker:
            - If the limb’s loop count is 0:
                · animation stops on the last valid frame
                · the actor’s clip is cleared
                · this limb’s sequences are marked inactive
            - If the loop count is "forever":
                · the frame index wraps back to the start and keeps looping.
            - If the loop count is a normal positive number:
                · the loop count is decremented
                · the frame index wraps back to the start
            - If the final frame index is different from the previous snapshot
              the actor is flagged for redraw.

    5) apply_speaking_clip + assign_clip_to_costume
        • apply_speaking_clip:
            - Computes a clip ID that blends:
                · direction variant (left/right/up/down)
                · mouth variant (open/closed)
            - Requests that this clip should loop forever while the actor is
              "speaking."
        • assign_clip_to_costume:
            - If the costume belongs to an actor:
                · stores the chosen clip + loop count into that actor’s tables,
                · calls init_limb_state_from_clip_set to push it down to limbs,
                · updates facing direction if the clip is a "standing" one.
            - If the costume has no actor attached:
                · accepts only standing clips and caches them as a static pose.

Redraw signaling
    • The animation system never draws directly. Instead, it:
        - Maintains per-limb state (sequences, frame index, cel index, flip).
        - Sets ACTOR_RENDER_REFRESH in actor_render_flags whenever:
            · A limb’s sequence changes and restarts.
            · A limb’s frame index changes after stepping.
        - The renderer can then query per-limb tables and redraw only actors
          whose render flag is set, making the animation system purely state-
          driven and decoupled from rendering.

================================================================================

Responsibilities
    • Clip selection and binding
        - Represent full-body motions/stances as clip sets (8 limb entries).
        - Bind a target clip ID and loop count to the actor that owns the active
          costume (assign_clip_to_costume).
        - Initialize per-limb state (sequence index, flip flag, loop count) from
          the chosen clip (init_limb_state_from_clip_set).

    • Speaking and facing variants
        - Track actor facing direction and map it to a direction variant used in
          clip IDs (map_facing_direction_to_clip_variant).
        - Combine facing variant with mouth state (open/closed) to select the
          correct speaking clip variant and request it in loop-forever mode
          (apply_speaking_clip).

    • Per-frame limb stepping
        - For the current actor, prepare costume/clip tables, cel sequence base,
          and per-limb index base, then iterate all limbs once per tick
          (step_actor_limb_animation).
        - For each limb, apply any pending cel-sequence change and reset its
          local animation state when the base sequence changes
          (apply_pending_cel_sequence).
        - Advance the limb’s frame index, interpret the end-of-sequence sentinel,
          and enforce loop/stop policy based on per-limb loop counters
          (step_limb_frame).

    • Cel sequence resolution
        - Cache the actor’s cel sequence table pointer (cache_actor_cel_sequence_table).
        - Compute the actor’s base slot into shared per-limb tables
          (compute_actor_limb_slot_base).
        - Resolve the current cel index for a limb by combining its per-limb
          base index with the current sequence step and reading from the cel
          sequence table (resolve_current_cel).

    • Render invalidation and teardown
        - When a limb’s frame index changes, mark the owning actor as needing a
          render refresh so the renderer will redraw it.
        - When an animation finishes (loop count exhausted), deactivate the
          actor’s clip and the limb’s cel sequences, leaving the limb on its
          last valid frame.
		  
step_actor_limb_animation
├── setup_costume_for_actor          
├── apply_speaking_clip
│   ├── map_path_direction_to_clip_offset
│   └── assign_clip_to_costume
│       └── init_limb_state_from_clip_set
│           ├── setup_costume_for_actor
│           └── compute_actor_limb_slot_base
├── cache_actor_cel_sequence_table
├── compute_actor_limb_slot_base
└── (per-limb loop: current_limb = 0..7)
    ├── apply_pending_cel_sequence
    │   └── resolve_current_cel
    └── step_limb_frame		  
================================================================================
*/

#importonce
#import "globals.inc"
#import "constants.inc"
#import "render_actor.asm"

.label clip_tbl                  = $17      // ZP pointer to clip-set table for the active actor
.label actor_limb_temp           = $FC3B    // Scratch: preserves limb slot index (aliased with prev_limb_frame)
.label prev_limb_frame           = $FC3B    // Scratch: previous frame index snapshot for change detection
.label target_clip               = $FDE9    // Fully resolved clip ID to apply for the active actor/costume
.label limb_index                = $2503    // Limb loop counter for the current actor (0..7)
.label actor_limb_temp_2         = $2502    // Scratch: preserves limb slot index across actor lookups

.const INACTIVE_CEL_SEQ          = $FF      // Sentinel: no cel / end-of-sequence for a cel sequence entry
.const INACTIVE_CLIP_SET         = $FF      // Sentinel: no active clip assigned to actor/costume
.const FLIP_SET                  = $80      // Bit7=1: limb rendered with horizontal flip enabled
.const FLIP_CLEAR                = $00      // Bit7=0: limb rendered without horizontal flip
.const CLIP_SET_STANDING_L       = $04      // Clip ID for standing facing left
.const CLIP_SET_STANDING_R       = $05      // Clip ID for standing facing right
.const CLIP_SET_STANDING_D       = $06      // Clip ID for standing facing down
.const CLIP_SET_STANDING_U       = $07      // Clip ID for standing facing up
.const CLIP_MOUTH_VARIANT_CLOSED = $10      // Clip variant delta for closed-mouth
.const CLIP_MOUTH_VARIANT_OPEN   = $0C      // Clip variant delta for open-mouth
.const CLIP_DIR_VARIANT_LEFT     = $00      // Direction variant index for facing left
.const CLIP_DIR_VARIANT_RIGHT    = $01      // Direction variant index for facing right
.const CLIP_DIR_VARIANT_UP       = $03      // Direction variant index for facing up
.const CLIP_DIR_VARIANT_DOWN     = $02      // Direction variant index for facing down
.const ANIM_LOOP_FOREVER         = $FF      // Loop count meaning "repeat animation indefinitely"
.const CEL_SEQUENCE_INDEX_MASK   = MSK_LOW7BITS // Mask to extract 7-bit cel-sequence index from clip entry
.const END_FRAME_SENTINEL        = $FF      // Cel value marking end-of-sequence frame in cel tables


/*
================================================================================
  step_actor_limb_animation
================================================================================
Summary
    Drive a full animation tick for all limbs of the current actor. Prepares all
    actor-level animation state (costume, speaking clip, cel tables, limb-slot
    base), then iterates over the eight limb slots and updates each limb’s
    animation frame and sequence state.

Global Inputs
    actor                           Current actor index.

Global Outputs
    All limb-level tables updated indirectly via apply_pending_cel_sequence
    and step_limb_frame.

Description
    • Prepare all high-level animation resources for the actor:
        – ensure costume/clip metadata is valid
        – compute speaking clip variant
        – cache the cel-sequence table pointer
        – compute the per-limb index base
		
    • Iterate over all 8 limbs:
        – apply_pending_cel_sequence commits any pending sequence changes and
          resets limb state when a new cel sequence is selected.
        – step_limb_frame advances the limb’s animation, handles loop/stop
          semantics, and flags the actor for redraw if the frame changes.
================================================================================
*/
* = $2504                        
step_actor_limb_animation:
        // ------------------------------------------------------------
        // Prepare actor resources and per-limb bookkeeping
        // ------------------------------------------------------------
		// Ensure actor’s costume/clip metadata and tables are ready
        jsr     setup_costume_for_actor      
		
		// Pick speaking clip variant (direction + mouth) and apply it
        jsr     apply_speaking_clip          
		
		// Cache cel sequence table pointer for this actor into cel_seq_tbl
        jsr     cache_actor_cel_sequence_table 
		
		// Compute base limb slot index for per-limb tables
        jsr     compute_actor_limb_slot_base 
		
		// Initialize current_limb loop counter
        lda     #$00                         
        sta     current_limb              
		
animate_actor_limb:
        // ------------------------------------------------------------
        // Animate current limb
        // ------------------------------------------------------------
		// Commit any pending cel-sequence change and reset limb state as needed
        jsr     apply_pending_cel_sequence   
		
		// Advance this limb’s frame, handling looping/stop logic and refresh flag
        jsr     step_limb_frame         
		
        // ------------------------------------------------------------
        // Loop over all limbs
        // ------------------------------------------------------------
        inc     current_limb                 
        lda     #MAX_LIMB_IDX + 1            
        cmp     current_limb                 
        bne     animate_actor_limb           
        rts                                  		
/*
================================================================================
  step_limb_frame
================================================================================
Summary
    Advance the current limb’s animation by one frame, handling end-of-sequence
    loop semantics and marking the owning actor dirty if the frame changes.

Global Inputs
    actor                       Current actor index used to resolve clip ownership.
    actor_limb_slot_base        Base slot index into all per-limb tables.
    current_limb                Limb index (0–7) currently being processed.
    cel_seq_tbl                 Pointer to the active actor’s cel-sequence table.
    limb_frame_idx              Per-limb frame index within the current cel sequence.
    limb_current_cel            Per-limb base index into cel_seq_tbl for this sequence.
    limb_cur_loop_cnt           Remaining loop count for this limb’s animation.
    limb_target_cel_seq         Target cel-sequence index for this limb.
    limb_current_cel_seq        Active cel-sequence index for this limb.
    actor_target_clip           Per-actor requested clip ID.
    actor_current_clip          Per-actor committed clip ID.
    actor_render_flags          Per-actor render flags bitfield.

Global Outputs
    limb_frame_idx              Updated to the new frame index (or rolled back).
    limb_cur_loop_cnt           Decremented or left unchanged based on loop rules.
    actor_target_clip           Cleared to INACTIVE_CLIP_SET when animation stops.
    actor_current_clip          Cleared to INACTIVE_CLIP_SET when animation stops.
    limb_target_cel_seq         Cleared to INACTIVE_CEL_SEQ when animation stops.
    limb_current_cel_seq        Cleared to INACTIVE_CEL_SEQ when animation stops.
    actor_render_flags          Updated with ACTOR_RENDER_REFRESH when frame changes.

Description
    • Compute the limb’s table slot index from actor_limb_slot_base + current_limb.
    • Snapshot the current frame index, then increment it to propose a new frame.
    • Use limb_current_cel + frame index to look up the candidate cel from
      cel_seq_tbl.
    • If the END_FRAME_SENTINEL is not reached, keep the new frame and, if the
      frame index changed, flag the actor for a render refresh.
    • If the sentinel is reached, apply loop semantics based on limb_cur_loop_cnt:
      – Zero: stop the animation, deactivate the actor’s clip and the limb’s
        cel sequences, and stay on the last valid frame.
      – ANIM_LOOP_FOREVER: wrap back to frame 0.
      – Positive: decrement the loop count and wrap back to frame 0.
    • Only when the frame index actually changes, set ACTOR_RENDER_REFRESH in
      actor_render_flags so the renderer knows to redraw this actor.
================================================================================
*/
* = $2526
step_limb_frame:
        // ------------------------------------------------------------
        // Compute per-limb table index in X: actor_limb_slot_base + current_limb
        // ------------------------------------------------------------
        lda     actor_limb_slot_base
        clc
        adc     current_limb
        tax

        // ------------------------------------------------------------
		// Advance to the next frame. If this step reaches the end of the
        // cel sequence, determine whether to loop, decrement loop count,
        // or stop the animation.		
        // ------------------------------------------------------------
        // Snapshot current frame index, in case we can't advance it
        lda     limb_frame_idx,x
        sta     prev_limb_frame
		
		// Increment to candidate next frame
        inc     limb_frame_idx,x

        // ------------------------------------------------------------
        // Lookup cel id for candidate frame; Y = limb_current_cel + frame index
        // ------------------------------------------------------------
        lda     limb_frame_idx,x
        clc
        adc     limb_current_cel,x
        tay
        lda     (cel_seq_tbl),y

        // ------------------------------------------------------------
        // Did we reach the end frame of the sequence?
        // ------------------------------------------------------------
        cmp     #END_FRAME_SENTINEL
        bne     move_animation_state_if_needed

        // ------------------------------------------------------------
        // End frame reached - handle loop/stop semantics based on limb_cur_loop_cnt
        // ------------------------------------------------------------
		// Loop count == 0? If so, stop
        lda     limb_cur_loop_cnt,x
        beq     stop_animation               
		
		// Loop count == loop forever? If so, loop from the first frame
        cmp     #ANIM_LOOP_FOREVER
        beq     loop_from_first              
		
		// Loop count normal - decrement it
        dec     limb_cur_loop_cnt,x      
		
loop_from_first:
		// Loop from the first frame
        lda     #$00
        jmp     set_new_frame

        // ------------------------------------------------------------
		// Animation must be stopped
        // ------------------------------------------------------------
stop_animation:
        // Go back to the last valid frame
        dec     limb_frame_idx,x
		
		// Save per-limb table index
        stx     actor_limb_temp_2

        // ------------------------------------------------------------
        // Deactivate target and current animation clips for the actor
        // ------------------------------------------------------------
        ldx     actor
        lda     #INACTIVE_CLIP_SET
        sta     actor_target_clip,x
        lda     #INACTIVE_CLIP_SET
        sta     actor_current_clip,x

		// Restore per-limb table index
        ldx     actor_limb_temp_2
		
        // ------------------------------------------------------------
        // Deactivate target and current cel sequences for this limb
        // ------------------------------------------------------------
        lda     #INACTIVE_CEL_SEQ
        sta     limb_target_cel_seq,x
        lda     #INACTIVE_CEL_SEQ
        sta     limb_current_cel_seq,x

        // Load the last valid frame
        lda     limb_frame_idx,x
set_new_frame:
        sta     limb_frame_idx,x

move_animation_state_if_needed:
        // ------------------------------------------------------------
        // Did the frame change? If not, exit
        // ------------------------------------------------------------
        lda     prev_limb_frame
        cmp     limb_frame_idx,x
        beq     slf_exit
		
        // ------------------------------------------------------------
        // Frame changed, flag actor render refresh
        // ------------------------------------------------------------
		ldx     actor
        lda     actor_render_flags,x
        ora     #ACTOR_RENDER_REFRESH
        sta     actor_render_flags,x

slf_exit:
        rts
/*
================================================================================
  apply_pending_cel_sequence
================================================================================
Summary
    Commit any pending cel-sequence change for the current limb and reset its
    animation state when the target differs from the active sequence.

Global Inputs
    actor                       Index of the actor whose limb is being updated.
    actor_limb_slot_base        Base slot index into per-limb tables.
    current_limb                Limb index currently being processed.
    limb_target_cel_seq         Per-limb desired cel-sequence index.
    limb_current_cel_seq        Per-limb active cel-sequence index.
    limb_tgt_loop_cnt           Per-limb target loop-count seed.
    actor_render_flags          Per-actor render flag bitfield.

Global Outputs
    limb_current_cel_seq        Updated to match limb_target_cel_seq when applied.
    limb_cur_loop_cnt           Seeded from limb_tgt_loop_cnt on sequence change.
    limb_frame_idx              Reset to 0 when a new sequence is applied.
    limb_current_cel            Resolved current cel index via resolve_current_cel.
    actor_render_flags          Updated with ACTOR_RENDER_REFRESH set for this actor.

Description
    • Compute the per-limb table index as actor_limb_slot_base + current_limb.
    • Exit early if the target cel sequence is inactive or already matches the
      current cel sequence.
    • When a new cel sequence is pending:
      - Copy limb_target_cel_seq into limb_current_cel_seq.
      - Seed limb_cur_loop_cnt from limb_tgt_loop_cnt.
      - Call resolve_current_cel to update limb_current_cel to the first cel
        in the new sequence.
      - Reset limb_frame_idx to 0.
      - Set ACTOR_RENDER_REFRESH for the owning actor so the renderer will
        redraw the updated limb.
================================================================================
*/
* = $258E
apply_pending_cel_sequence:
        // ------------------------------------------------------------
        // Compute per-limb table index in X: actor_limb_slot_base + current_limb
        // ------------------------------------------------------------
        lda     actor_limb_slot_base
        clc
        adc     current_limb
        tax

        // ------------------------------------------------------------
        // If target cel sequence is inactive, exit
        // ------------------------------------------------------------
        lda     limb_target_cel_seq,x
        cmp     #INACTIVE_CEL_SEQ
        bne     check_for_new_value
        rts

check_for_new_value:
        // ------------------------------------------------------------
        // If target equals current, exit
        // ------------------------------------------------------------
        cmp     limb_current_cel_seq,x
        bne     assign_new_base_cel
        rts

assign_new_base_cel:
        // ------------------------------------------------------------
        // Commit target cel sequence and loop count
        // ------------------------------------------------------------
        lda     limb_target_cel_seq,x
        sta     limb_current_cel_seq,x
        lda     limb_tgt_loop_cnt,x
        sta     limb_cur_loop_cnt,x
		
        // ------------------------------------------------------------
		// Resolve the current cel in the cel sequence
        // ------------------------------------------------------------
        jsr     resolve_current_cel

        // ------------------------------------------------------------
        // Reset limb frame index
        // ------------------------------------------------------------
        lda     #$00
        sta     limb_frame_idx,x
		
        // ------------------------------------------------------------
		// Flag actor render refresh
        // ------------------------------------------------------------
        ldx     actor
        lda     actor_render_flags,x
        ora     #ACTOR_RENDER_REFRESH
        sta     actor_render_flags,x
        rts
/*
================================================================================
  resolve_current_cel
================================================================================
Summary
    Resolve the limb’s current cel index by combining the limb’s base cel-sequence
    offset with the per-limb current sequence index, then reading the final cel
    value from the actor’s cel sequence table.

Arguments
    X                       Limb slot index (actor_limb_slot_base + limb_index).

Global Inputs
    current_limb            Index of the limb currently being processed (0–7).
    cel_seq_tbl             Pointer to the active actor’s cel sequence table.
    limb_current_cel_seq    Per-limb base cel-sequence index.

Global Outputs
    limb_current_cel        Final cel index for this limb after resolving the
                            base index + sequence index.

Description
    • Load the limb’s base cel index from cel_seq_tbl[current_limb].
    • Add limb_current_cel_seq[X] to produce the effective sequence index.
    • Fetch cel_seq_tbl[effective_index] to obtain the resolved cel index.
    • Store this resolved cel into limb_current_cel[X].
	
Example:
        • Assume:
            - current_limb           = 2
            - cel_seq_tbl[2]         = $10    ; base offset for limb 2
            - limb_current_cel_seq,X = $03    ; this limb is on sequence step 3
            - cel_seq_tbl[$13]       = $2A    ; cel index for that sequence step
        • Then:
            - effective index  = $10 + $03 = $13
            - limb_current_cel,X is set to $2A.	
================================================================================
*/
* = $25C3
resolve_current_cel:
        // ------------------------------------------------------------
        // limb_current_cel[X] := cel_seq_tbl[ base_offset + seq_offset ]
        // ------------------------------------------------------------
        ldy     current_limb
        lda     (cel_seq_tbl),y        // base cel offset for this limb
        clc
        adc     limb_current_cel_seq,x // add limb’s current sequence index
        tay
        lda     (cel_seq_tbl),y        // fetch resolved cel index
        sta     limb_current_cel,x
        rts
/*
================================================================================
  compute_actor_limb_slot_base
================================================================================
Summary
    Compute the base slot index for the current actor’s limbs in the per-limb tables
	(global tables prefixed with "limb_").

Global Inputs
    actor                   	Index of the currently active actor.

Global Outputs
    actor_limb_slot_base        Base slot index into per-limb tables.

Description
    • Multiply the current actor index by 8 to derive the first limb slot.
    • Store the result in actor_limb_slot_base so other routines can index per-limb
      tables for this actor using actor_limb_slot_base + limb_index.
================================================================================
*/
* = $25D3
compute_actor_limb_slot_base:
		//Compute actor index * 8
        lda     actor                  
        clc                            
        asl                            
        asl                            
        asl                      
		
		// Stash result in actor_limb_slot_base
        sta     actor_limb_slot_base       
        rts
/*
================================================================================
  cache_actor_cel_sequence_table
================================================================================
Summary
    Cache the current actor’s cel-sequence table pointer into cel_seq_tbl so
    limb animation routines can index cel sequences via a single indirect base.

Global Inputs
    actor                   	Index of the actor whose cel sequence table is active.
    actor_cel_seq_tbl_lo/hi    	Per-actor pointer to cel sequence table.

Global Outputs
    cel_seq_tbl             	Cached pointer to the active actor’s cel sequence table.

Description
    • Use the current actor index to select that actor’s cel-sequence table
      pointer from actor_cel_seq_tbl_lo/hi.
    • Store the pointer into cel_seq_tbl so subsequent animation code can read
      cel sequence bytes via a common base pointer.
================================================================================
*/
* = $25DD
cache_actor_cel_sequence_table:
        // ------------------------------------------------------------
        // cel_seq_tbl := actor_cel_seq_tbl[actor]
        // ------------------------------------------------------------
        ldx     actor
        lda     actor_cel_seq_tbl_lo,x
        sta     cel_seq_tbl
        lda     actor_cel_seq_tbl_hi,x
        sta     cel_seq_tbl + 1
        rts
/*
================================================================================
  init_limb_state_from_clip_set
================================================================================
Summary
    Initialize all limb-level animation parameters for the actor’s newly selected
    clip set. Expands the 8-entry clip definition into per-limb cel-sequence
    targets, flip flags, and loop counts, and commits the new clip as the
    actor’s current clip.

Global Inputs
    actor                       Index of the actor whose clip is being initialized.
    actor_target_clip           Target clip ID selected for this actor.
    actor_current_clip          Current clip ID for this actor.
    actor_clip_tbl_lo/hi        Pointer to the actor’s clip set table.
    actor_clip_loop_cnt         Per-actor loop-count argument.

Global Outputs
    actor_current_clip          Updated to the new committed clip ID.
    limb_target_cel_seq         Per-limb target cel-sequence index.
    limb_current_cel_seq        Invalidated when flip mode changes.
    limb_flip_tbl               Horizontal flip flag per limb.
    limb_tgt_loop_cnt           Limb-level loop count copied from actor-level count.

Description
    • Confirm that a valid target clip is set; exit if clip is inactive.
    • Cache the clip-table pointer for this actor.
    • Skip work if target clip matches the actor’s current clip.
    • Commit the new clip and compute Y = (clip_id * 8) to index its 8 limb entries.
    • Compute actor_limb_slot_base = actor * 8 for per-limb table indexing.
    • For each of the 8 limbs:
        – Read the limb’s clip-table entry (a packed cel sequence index/flip flag): 
				bit7=flip flag, bits0–6=sequence index.
        – If entry is INACTIVE_CEL_SEQ, skip the limb.
        – Otherwise assign limb_target_cel_seq and set/unset flip flags.
        – Invalidate limb_current_cel_seq when flipping mode changes.
        – Copy actor-level loop count into limb_tgt_loop_cnt.
    • Optional tail: for clip IDs < 8, store (clip_id & 3) into $FD0E,X
      (appears unused but preserved).
================================================================================
*/
* = $25FF
init_limb_state_from_clip_set:
		// Ensure actor clips and cel sequences tables are ready
		jsr     setup_costume_for_actor

		// Resolve target clip for this actor
		ldx     actor
		lda     actor_target_clip,x
		
		// Is there a valid target clip? If not, exit
		cmp     #INACTIVE_CLIP_SET
		bne     check_for_new_clip
		rts

        // ------------------------------------------------------------
		// Cache actor's clip table into local vars
        // ------------------------------------------------------------
check_for_new_clip:
		lda     actor_clip_tbl_lo,x
		sta     clip_tbl
		lda     actor_clip_tbl_hi,x
		sta     clip_tbl + 1

        // ------------------------------------------------------------
		// Target clip already equal to current clip? If so, exit
        // ------------------------------------------------------------
		lda     actor_target_clip,x
		cmp     actor_current_clip,x
		bne     commit_new_clip
		rts

        // ------------------------------------------------------------
		// Commit new current clip
        // ------------------------------------------------------------
commit_new_clip:
		sta     actor_current_clip,x        

        // ------------------------------------------------------------
		// Compute clip set index into clip set table
		// Y := clip_set * 8 (8 limb entries per clip)
        // ------------------------------------------------------------
		asl
		asl
		asl
		tay

		// Compute the slot base index, for this actor, for per-limb tables
		// 	-> actor_limb_slot_base
		jsr     compute_actor_limb_slot_base

		// Init limb loop
		ldx     #$00
		stx     limb_index

prepare_limb_frame:
        // ------------------------------------------------------------
		// Resolve limb table index in X := actor_limb_slot_base + limb_index 
        // ------------------------------------------------------------
		clc
		lda     limb_index
		adc     actor_limb_slot_base
		tax

		lda     limb_index // Redundant code, result discarded immediately
		
        // ------------------------------------------------------------
		// Read cel sequence index
		//
		// Bit 7 set = flip cels, clear = normal (unflipped) cels
		// Bits 6-0 = cel sequence index
		// INACTIVE_CEL_SEQ means an inactive cel sequence
        // ------------------------------------------------------------
		lda     (clip_tbl),y
		bpl     unflipped_cel_sequence

		//Inactive cel sequence? If so, skip
		cmp     #INACTIVE_CEL_SEQ
		beq     adv_limb_2                  

        // ------------------------------------------------------------
		// Bit 7 set: flipped cel sequence
        // ------------------------------------------------------------
		// Resolve cel sequence index
		and     #CEL_SEQUENCE_INDEX_MASK
		
		// Store in target cel sequence
		sta     limb_target_cel_seq,x

		// If limb flip was not already set, invalidate the current cel sequence
		lda     limb_flip_tbl,x
		cmp     #FLIP_SET
		beq     set_horizontal_flip
		lda     #INACTIVE_CEL_SEQ
		sta     limb_current_cel_seq,x

set_horizontal_flip:
		// Set flipping for this limb
		lda     #FLIP_SET
		sta     limb_flip_tbl,x
		jmp     copy_loop_count
		
adv_limb_2:		
		jmp		advance_limb
		jmp		copy_loop_count				//Unreachable code

unflipped_cel_sequence:
        // ------------------------------------------------------------
		// Bit 7 clear: unflipped cel sequence
        // ------------------------------------------------------------
		//Store cel sequence index -> target cel sequence
		sta     limb_target_cel_seq,x

		// If limb flip is currently set, invalidate the current cel sequence
		lda     limb_flip_tbl,x
		cmp     #FLIP_CLEAR
		beq     clear_horizontal_flip
		lda     #INACTIVE_CEL_SEQ
		sta     limb_current_cel_seq,x

clear_horizontal_flip:
		// Clear flipping for this limb
		lda     #FLIP_CLEAR
		sta     limb_flip_tbl,x

        // ------------------------------------------------------------
		// Copy actor-level loop count to limb-level target loop count
        // ------------------------------------------------------------
copy_loop_count:
		// Save limb index
		stx     actor_limb_temp              
		
		// Fetch clip loop count for current actor
		ldx     actor
		lda     actor_clip_loop_cnt,x
		
		// Restore limb index
		ldx     actor_limb_temp              
		
		// Set target loop count for limb
		sta     limb_tgt_loop_cnt,x

        // ------------------------------------------------------------
		// Advance to next limb (8 limbs in total)
        // ------------------------------------------------------------
advance_limb:
		iny                                 // next limb entry in clip table
		inc     limb_index					// next limb index in per-limb tables
		lda     limb_index
		cmp     #MAX_LIMB_IDX + 1
		bne     prepare_limb_frame

        // ------------------------------------------------------------
		// Optional tail: write (clip_set & 3) when clip_set < 8 (kept as-is)
		// The values written seem to be unused
        // ------------------------------------------------------------
		ldx     actor
		lda     actor_current_clip,x
		cmp     #$08
		bcs     ilsfcs_exit
		and     #$03
		sta     $FD0E,x                     // direction code (apparently unused)

ilsfcs_exit:
		rts
/*
================================================================================
  assign_clip_to_costume
================================================================================
Summary
    Bind the current target clip and loop count to the active costume’s actor (if
    any), initialize limb state from that clip, and update facing direction for
    standing clips; otherwise cache standing clips on actor-less costumes.

Global Inputs
    active_costume              Index of the costume whose clip is being updated.
    actor_for_costume           Maps costume index → actor index (negative = none).
    target_clip                 Fully resolved clip ID to apply.
    clip_loop_cnt               Desired loop count for the clip (0, finite, or forever).

Global Outputs
    actor_target_clip           Per-actor target clip ID.
    actor_clip_loop_cnt         Per-actor clip loop count argument.
    facing_direction_for_actor  Per-actor facing direction (for standing clip sets).
    costume_clip_set            Per-costume standing clip cached for actor-less costumes.

Description
    • Look up the actor assigned to the active costume; if none, only accept and
      cache standing clips on the costume itself.
    • When an actor is assigned, copy target_clip and clip_loop_cnt into the
      actor’s clip state tables and call init_limb_state_from_clip_set to seed
      all limb animation parameters from the clip definition.
    • For standing clip sets, map the clip ID to a facing direction constant and
      store it in facing_direction_for_actor for that actor.
    • For costumes without an assigned actor, accept only standing clip sets and
      cache them in costume_clip_set.
================================================================================
*/
* = $2720
assign_clip_to_costume:
		// ------------------------------------------------------------
		// Is there an assigned actor for the active costume?
		// ------------------------------------------------------------
		ldx     active_costume           		
		lda     actor_for_costume,x      		
		
		// If no actor assigned, skip
		bmi     actor_unassigned         		

		// ------------------------------------------------------------
		// Actor assigned: stash target clip and loop count into per-actor tables
		// ------------------------------------------------------------		
		// Stash actor index for later use
		sta     actor                         	
		
		// X := actor index for per-actor tables
		tax                                   	
		
		// Stash target clip
		lda     target_clip               	  	
		sta     actor_target_clip,x       	  	
		
		// Stash loop count
		lda     clip_loop_cnt                 	
		sta     actor_clip_loop_cnt,x         	

		// ------------------------------------------------------------
		// Init limb states 
		// ------------------------------------------------------------
		jsr     init_limb_state_from_clip_set

		// ------------------------------------------------------------
		// Map target clip set to facing direction
		// ------------------------------------------------------------
		lda     target_clip              	
		cmp     #CLIP_SET_STANDING_L
		bne     is_clip_right
		lda     #DIRECTION_LEFT              
		jmp     commit_direction_and_exit

is_clip_right:
		cmp     #CLIP_SET_STANDING_R
		bne     is_clip_down
		lda     #DIRECTION_RIGHT             
		jmp     commit_direction_and_exit

is_clip_down:
		cmp     #CLIP_SET_STANDING_D
		bne     is_clip_up
		lda     #DIRECTION_DOWN              
		jmp     commit_direction_and_exit

is_clip_up:
		cmp     #CLIP_SET_STANDING_U
		bne     actc_exit_2
		lda     #DIRECTION_UP                
		jmp     commit_direction_and_exit

actc_exit_2:
		rts                                  	

		// ------------------------------------------------------------
		// Store computed facing direction for this actor and exit
		// ------------------------------------------------------------
commit_direction_and_exit:
		// Stash facing direction in the per-actor table
		ldx     actor                        	
		sta     facing_direction_for_actor,x   	
		jmp     actc_exit                       

		// ------------------------------------------------------------
		// Actor unassigned for this costume: accept only standing clip sets
		// ------------------------------------------------------------
actor_unassigned:
		// Range-check for standing clip sets
		// Clip set needs to be in the [CLIP_SET_STANDING_L; CLIP_SET_STANDING_U] range
		lda     target_clip            	
		cmp     #CLIP_SET_STANDING_L
		bcc     actc_exit                      	// < CLIP_SET_STANDING_L → not standing → ignore
		cmp     #CLIP_SET_STANDING_U		
		beq     set_standing_clip_for_costume   // == CLIP_SET_STANDING_U → accept standing
		bcs     actc_exit                 		// > CLIP_SET_STANDING_U → not standing → ignore
		
		// ------------------------------------------------------------
		// Store the clip set for this costume
		// ------------------------------------------------------------
set_standing_clip_for_costume:
		ldx     active_costume           		
		sta     costume_clip_set,x 				
		
actc_exit:
		rts
/*
================================================================================
  map_facing_direction_to_clip_variant
================================================================================
Summary
    Map the actor’s current facing direction to a clip direction variant suitable
    for indexing direction-specific clip variants.

Arguments
    X                       Actor index whose facing direction is being mapped.

Global Inputs
    facing_direction_for_actor Per-actor facing direction; encoded as DIRECTION_*.

Returns
    A                       Clip direction variant (0..3) used in clip ID math.

Description
    • Read the actor’s current facing direction from facing_direction_for_actor[X].
    • Compare it against the four cardinal DIRECTION_* constants.
    • Load the matching CLIP_OFS_DIR_* value into Y.
    • Return the chosen variant in A for downstream clip selection.
================================================================================
*/
* = $277E
map_facing_direction_to_clip_variant:
        lda     facing_direction_for_actor,x
        cmp     #DIRECTION_LEFT
        bne     mfdtcv_check_right
        ldy     #CLIP_DIR_VARIANT_LEFT

mfdtcv_check_right:
        cmp     #DIRECTION_RIGHT
        bne     mfdtcv_check_down
        ldy     #CLIP_DIR_VARIANT_RIGHT

mfdtcv_check_down:
        cmp     #DIRECTION_DOWN
        bne     mfdtcv_check_up
        ldy     #CLIP_DIR_VARIANT_DOWN

mfdtcv_check_up:
        cmp     #DIRECTION_UP
        bne     mfdtcv_exit
        ldy     #CLIP_DIR_VARIANT_UP

mfdtcv_exit:
        tya
        rts
/*
================================================================================
  apply_speaking_clip
================================================================================
Summary
    Select the speaking clip variant (direction + mouth open/closed) for the
    active costume’s actor and request it to play in loop-forever mode.

Global Inputs
    active_costume          Active costume index whose speaking animation is updated.
    costume_anim_attrs      Per-costume attribute flags; bit7 set disables speaking.
    actor                   Current actor index used to read facing/path direction.
    actor_mouth_state       Per-actor mouth state; bit7=1 → mouth open, bit7=0 → closed.

Global Outputs
    target_clip             Resolved speaking clip ID (direction + mouth variant).
    clip_loop_cnt           Loop mode argument for the speaking clip (set to loop forever).

Description
    • Check whether the active costume supports speaking animations and exit early
      if not.
    • Map the actor’s current path direction to a directional clip variant.
    • Inspect actor_mouth_state to choose closed vs open mouth clip variant.
    • Combine direction and mouth offsets into a final speaking clip ID and store
      it in target_clip.
    • Set clip_loop_cnt to loop forever and call assign_clip_to_costume so the
      speaking clip is applied to the active costume/actor.
================================================================================
*/
* = $26F9
apply_speaking_clip:
        // ------------------------------------------------------------
        // If the costume supports a speaking animation (bit7 clear), continue
		// Else, exit
        // ------------------------------------------------------------
        ldx     active_costume
        lda     costume_anim_attrs,x
        bpl     speaking_anim_supported
        rts

speaking_anim_supported:
        // ------------------------------------------------------------
        // Get clip variant for the current facing direction
        // ------------------------------------------------------------
        ldx     actor
        jsr     map_facing_direction_to_clip_variant

        // ------------------------------------------------------------
        // Resolve the mouth clip variant: closed or open
		// Closed if bit7 clear, open if set
        // ------------------------------------------------------------
        clc                                 // prep for later mouth variant add
        ldy     actor_mouth_state,x     	// bit7 set → open
        bmi     mouth_is_open

		// Mouth closed - add its clip variant
        adc     #CLIP_MOUTH_VARIANT_CLOSED                        
        jmp     commit_speaking_clip

mouth_is_open:
		// Mouth open - add its clip variant
        adc     #CLIP_MOUTH_VARIANT_OPEN                        

commit_speaking_clip:
        // ------------------------------------------------------------
		// Assign the fully resolved speaking clip to the costume
        // ------------------------------------------------------------
		// Set the "mouth open/closed in the correct direction" clip ID as target clip set
        sta     target_clip
		
		// Set a desired "loop forever" mode
        lda     #ANIM_LOOP_FOREVER            
        sta     clip_loop_cnt
		
		// Assign the clip to the active costume
        jsr     assign_clip_to_costume               
        rts


/*
Pseudo-code

procedure step_actor_limb_animation()
    // Prepare per-actor state needed to drive limb animation this tick.
    setup_costume_for_actor()             // ensure costume + clip metadata is valid
    apply_speaking_clip()                 // choose speaking clip variant (direction + mouth)
    cache_actor_cel_sequence_table()      // cache cel_seq_tbl pointer for this actor
    compute_actor_limb_slot_base()        // actor_limb_slot_base = actor_index * 8

    current_limb := 0

    // Iterate over all limbs for this actor.
    while current_limb < (MAX_LIMB_IDX + 1) do
        apply_pending_cel_sequence()      // commit any new sequence, reset limb state
        step_limb_frame()                 // advance animation frame, loop/stop, mark dirty
        current_limb := current_limb + 1
    end while
end procedure

procedure step_limb_frame()
    // Compute this limb’s index into the per-limb tables.
    limb_slot := actor_limb_slot_base + current_limb

    // Snapshot current frame index.
    prev_frame := limb_frame_idx[limb_slot]

    // Propose advancing to the next frame.
    limb_frame_idx[limb_slot] := limb_frame_idx[limb_slot] + 1

    // Look up the cel ID referenced by (base_cel_index + frame_index).
    cel_index_offset := limb_current_cel[limb_slot] + limb_frame_idx[limb_slot]
    next_cel := cel_seq_tbl[cel_index_offset]

    // If not at the end-of-sequence sentinel, keep frame and exit if unchanged.
    if next_cel != END_FRAME_SENTINEL then
        // Frame index is already stored in limb_frame_idx.
        if limb_frame_idx[limb_slot] != prev_frame then
            // Frame changed; mark actor as needing a redraw.
            actor_render_flags[actor] := actor_render_flags[actor] OR ACTOR_RENDER_REFRESH
        end if
        return
    end if

    // End-of-sequence: apply loop/stop semantics based on per-limb loop count.
    loop_count := limb_cur_loop_cnt[limb_slot]

    if loop_count = 0 then
        // Stop animation for this clip and limb.
        // Revert frame index to last valid frame.
        limb_frame_idx[limb_slot] := limb_frame_idx[limb_slot] - 1

        // Deactivate the actor’s clip.
        actor_target_clip[actor]  := INACTIVE_CLIP_SET
        actor_current_clip[actor] := INACTIVE_CLIP_SET

        // Deactivate the limb’s cel sequence.
        limb_target_cel_seq[limb_slot]   := INACTIVE_CEL_SEQ
        limb_current_cel_seq[limb_slot]  := INACTIVE_CEL_SEQ

        // Frame index already rolled back; nothing more to do.
    else if loop_count = ANIM_LOOP_FOREVER then
        // Loop forever: wrap back to frame 0.
        limb_frame_idx[limb_slot] := 0
    else
        // Finite loops remaining: decrement and wrap back to frame 0.
        limb_cur_loop_cnt[limb_slot] := loop_count - 1
        limb_frame_idx[limb_slot] := 0
    end if

    // Optional: last frame index is now updated in limb_frame_idx.
    // If the frame index changed, flag the actor for redraw.
    if limb_frame_idx[limb_slot] != prev_frame then
        actor_render_flags[actor] := actor_render_flags[actor] OR ACTOR_RENDER_REFRESH
    end if
end procedure


procedure apply_pending_cel_sequence()
    // Compute per-limb slot index for this actor + limb.
    limb_slot := actor_limb_slot_base + current_limb

    target_seq := limb_target_cel_seq[limb_slot]

    // If there is no target sequence, nothing to do.
    if target_seq = INACTIVE_CEL_SEQ then
        return
    end if

    // If the target sequence is already current, nothing to do.
    if target_seq = limb_current_cel_seq[limb_slot] then
        return
    end if

    // Commit the new cel sequence and seed loop count.
    limb_current_cel_seq[limb_slot] := target_seq
    limb_cur_loop_cnt[limb_slot]    := limb_tgt_loop_cnt[limb_slot]

    // Resolve the first cel in this sequence.
    resolve_current_cel(limb_slot)

    // Reset frame index so animation starts at the beginning.
    limb_frame_idx[limb_slot] := 0

    // Mark actor as needing a redraw due to sequence change.
    actor_render_flags[actor] := actor_render_flags[actor] OR ACTOR_RENDER_REFRESH
end procedure


procedure resolve_current_cel(limb_slot)
    // current_limb is the limb number 0..7 being processed.
    // cel_seq_tbl points to an array encoding base offsets and cel indices.

    // Read base offset or start index for this limb’s cel sequence.
    base_offset := cel_seq_tbl[current_limb]

    // Add this limb’s current sequence step (frame within the cel list).
    effective_index := base_offset + limb_current_cel_seq[limb_slot]

    // Fetch the actual cel index for that step.
    resolved_cel := cel_seq_tbl[effective_index]

    // Store as the limb’s current cel.
    limb_current_cel[limb_slot] := resolved_cel
end procedure


procedure compute_actor_limb_slot_base()
    // Per actor, per-limb tables are organized in groups of 8 entries.
    actor_limb_slot_base := actor * 8
end procedure


procedure cache_actor_cel_sequence_table()
    // Per-actor, a pointer to that actor’s cel sequence table is stored in
    // actor_cel_seq_tbl_lo/hi (or equivalent).
    cel_seq_tbl := actor_cel_seq_tbl[actor]   // conceptual: load full pointer
end procedure


procedure init_limb_state_from_clip_set()
    // Make sure costume + clip metadata for this actor is initialized.
    setup_costume_for_actor()

    // Use current actor index to look up its target clip.
    clip_id := actor_target_clip[actor]

    // If there is no target clip, nothing to do.
    if clip_id = INACTIVE_CLIP_SET then
        return
    end if

    // Cache the clip table pointer for this actor.
    clip_tbl := actor_clip_tbl[actor]   // points to packed per-clip entries

    // If target clip already matches current clip, nothing to update.
    if actor_current_clip[actor] = clip_id then
        return
    end if

    // Commit this clip as the active one.
    actor_current_clip[actor] := clip_id

    // Each clip provides one entry per limb (8 limbs).
    // Compute starting index in the clip table: clip_id * 8.
    table_index := clip_id * 8

    // Compute base per-limb slot for this actor’s limbs.
    compute_actor_limb_slot_base()      // sets actor_limb_slot_base = actor * 8

    // Initialize all 8 limbs from this clip definition.
    for limb_index from 0 to MAX_LIMB_IDX do
        limb_slot := actor_limb_slot_base + limb_index

        // Read the packed entry for this limb.
        entry := clip_tbl[table_index]

        // entry layout:
        //   bit7 = flip flag (1 = flipped)
        //   bits0..6 = cel-sequence index
        //   INACTIVE_CEL_SEQ ($FF) = limb disabled

        if entry = INACTIVE_CEL_SEQ then
            // Limb inactive for this clip; skip.
            // (limb_target_cel_seq is left as-is or cleared elsewhere as needed).
        else
            sequence_index := entry AND CEL_SEQUENCE_INDEX_MASK

            // Set per-limb target cel sequence.
            limb_target_cel_seq[limb_slot] := sequence_index

            // Handle flip flag and invalidate current cel sequence if flip changes.
            if (entry has bit7 set) then
                // Flipped sequence.
                if limb_flip_tbl[limb_slot] != FLIP_SET then
                    limb_current_cel_seq[limb_slot] := INACTIVE_CEL_SEQ
                end if
                limb_flip_tbl[limb_slot] := FLIP_SET
            else
                // Unflipped sequence.
                if limb_flip_tbl[limb_slot] != FLIP_CLEAR then
                    limb_current_cel_seq[limb_slot] := INACTIVE_CEL_SEQ
                end if
                limb_flip_tbl[limb_slot] := FLIP_CLEAR
            end if

            // Copy actor-level loop count into per-limb target loop count.
            limb_tgt_loop_cnt[limb_slot] := actor_clip_loop_cnt[actor]
        end if

        // Advance to next clip-table entry (next limb).
        table_index := table_index + 1
    end for

    // Optional tail:
    // If clip_id < 8, write (clip_id & 3) to a small debug/direction slot in memory.
    if clip_id < 8 then
        debug_value := clip_id AND 3
        some_debug_table[actor] := debug_value   // semantics are apparently unused
    end if
end procedure


procedure assign_clip_to_costume()
    // Resolve which actor (if any) owns the active costume.
    costume := active_costume
    owning_actor := actor_for_costume[costume]

    // If there is an owning actor, bind clip + loop count at actor level.
    if owning_actor >= 0 then
        actor := owning_actor

        // Copy the chosen clip and loop count into per-actor tables.
        actor_target_clip[actor]   := target_clip
        actor_clip_loop_cnt[actor] := clip_loop_cnt

        // Initialize per-limb state for this clip.
        init_limb_state_from_clip_set()

        // If this clip is one of the standing clip sets, update the actor’s facing.
        if    target_clip = CLIP_SET_STANDING_L then
            facing_direction_for_actor[actor] := DIRECTION_LEFT
        elseif target_clip = CLIP_SET_STANDING_R then
            facing_direction_for_actor[actor] := DIRECTION_RIGHT
        elseif target_clip = CLIP_SET_STANDING_D then
            facing_direction_for_actor[actor] := DIRECTION_DOWN
        elseif target_clip = CLIP_SET_STANDING_U then
            facing_direction_for_actor[actor] := DIRECTION_UP
        else
            // Non-standing clips do not update facing.
        end if

        return
    end if

    // If there is no actor assigned to this costume, only accept standing clips.
    clip := target_clip

    // Accept only clips within [STANDING_L .. STANDING_U].
    if clip < CLIP_SET_STANDING_L or clip > CLIP_SET_STANDING_U then
        // Non-standing clip for an actor-less costume: ignore.
        return
    end if

    // Cache standing clip on the costume itself so it can show a static pose.
    costume_clip_set[costume] := clip
end procedure


function map_facing_direction_to_clip_variant(actor_index) -> clip_dir_variant
    dir := facing_direction_for_actor[actor_index]

    if dir = DIRECTION_LEFT then
        return CLIP_DIR_VARIANT_LEFT
    elseif dir = DIRECTION_RIGHT then
        return CLIP_DIR_VARIANT_RIGHT
    elseif dir = DIRECTION_DOWN then
        return CLIP_DIR_VARIANT_DOWN
    elseif dir = DIRECTION_UP then
        return CLIP_DIR_VARIANT_UP
    else
        // Fallback / unexpected direction: default variant (e.g. left)
        return CLIP_DIR_VARIANT_LEFT
    end if
end function


procedure apply_speaking_clip()
    // Check if the active costume allows speaking animation.
    c := active_costume
    attrs := costume_anim_attrs[c]

    // If bit7 is set, speaking animation is disabled.
    if attrs has bit7 set then
        return
    end if

    // Use the current actor to decide direction + mouth variant.
    actor_index := actor

    // Map facing direction to a clip direction variant (0..3).
    dir_variant := map_facing_direction_to_clip_variant(actor_index)

    // Start from the directional clip variant.
    // (In the real code this comes back in A and then mouth variant is added).
    speaking_clip_id := dir_variant

    // Determine whether the mouth is open or closed based on bit7 of actor_mouth_state.
    mouth_state := actor_mouth_state[actor_index]

    if mouth_state has bit7 set then
        // Mouth open variant.
        speaking_clip_id := speaking_clip_id + CLIP_MOUTH_VARIANT_OPEN
    else
        // Mouth closed variant.
        speaking_clip_id := speaking_clip_id + CLIP_MOUTH_VARIANT_CLOSED
    end if

    // Store the final speaking clip ID.
    target_clip := speaking_clip_id

    // Speaking animations should loop forever while the actor is talking.
    clip_loop_cnt := ANIM_LOOP_FOREVER

    // Bind and apply this clip to the active costume (and its actor, if any).
    assign_clip_to_costume()
end procedure
*/