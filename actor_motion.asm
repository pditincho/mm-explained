/*
================================================================================
Actor Motion
================================================================================

Key Relationships:

1. Destination Setup
   - Routine: set_actor_destination
   - Function: Determines if an actor is off-screen or on-screen.
       • Off-screen → stores destination for later without pathing.
       • On-screen → snaps destination to valid walkbox and publishes targets.
   - Feeds the waypoint system used by motion routines.

2. Waypoint & Motion State
   - Routine: update_actor_motion
   - Function: Compares desired vs active waypoint.
       • Installs new active waypoint if changed.
       • Updates actor_motion_state (low nibble for motion, high nibble for turning/facing).
       • Snapshots current position and initializes DDA deltas.
       • Sets traversal masks for dominant axis and direction.
       • Resolves facing and selects initial limb set.
   - Tail-calls step_actor_along_path if movement/animation is required.
   - Consumes output from set_actor_destination.

3. Per-Frame Stepping
   - Routine: step_actor_along_path
   - Function: Performs actual motion per frame along the active waypoint.
       • Calls step_actor_x_dda / step_actor_y_dda for pixel-level movement.
       • Calls traverse_horizontally / traverse_vertically if a box boundary is crossed.
       • Calls resolve_turning_and_facing to update facing flags.
       • Applies standing or walking clips depending on motion state.
   - Triggered by update_actor_motion for ongoing movement.

4. DDA & Axis Handling
   - Routine: init_dda_for_path
   - Function: Computes deltas to active waypoint, dominant axis, and direction bits.
   - Feeds both update_actor_motion and step_actor_along_path for proper movement resolution.

5. Walkbox Traversal
   - Routines: traverse_horizontally / traverse_vertically
   - Function: Ensure actor stays within valid walkboxes when stepping across box boundaries.
   - Called by step_actor_x_dda / step_actor_y_dda.

6. Facing & Limb Selection
   - Routine: resolve_turning_and_facing
       • Adjusts actor_motion_state high nibble based on motion deltas or overrides.
   - Routines: apply_standing_clip / apply_walking_clip
       • Update limb graphics according to motion state and facing.
   - Called by update_actor_motion and step_actor_along_path to ensure animation reflects motion.

7. Animation Reset
   - Routine: reset_actor_render_flags
   - Function: Resets actor to standing/stopped state; flags redraw.
   - Can be called independently to force reset of motion and animation.

Notes:
- Destination setup routines feed motion routines but do not move actors directly.
- Motion routines update both the numeric state (DDA, deltas, traversal masks) and visual state (limbs, facing).
- Stepping routines enforce per-pixel movement, box boundaries, and trigger facing/limb updates.
- Traversal, facing, and limb routines work together to produce smooth, visually consistent actor motion.

set_actor_destination
        │
        └─> snap_and_stage_path_update
                 │
                 └─> update_actor_motion
                          │
                          ├─> init_dda_for_path
                          │
                          └─> step_actor_along_path
                                   │
                                   ├─> step_actor_x_dda / step_actor_y_dda
                                   │        └─> traverse_horizontally / traverse_vertically
                                   │
                                   └─> resolve_turning_and_facing
                                            ├─> apply_standing_clip
                                            └─> apply_walking_clip

reset_actor_render_flags → may reset actor_motion_state and limb selection independently

================================================================================
 Digital Differential Analyzer (DDA) - integer stepping for smooth motion
================================================================================

	DDA generates proportionate X/Y steps toward a waypoint using only integer
	arithmetic. It maintains per-axis accumulators seeded with the absolute
	deltas to the target. Each frame it compares the accumulator(s) against the
	dominant delta magnitude and issues a one-pixel step on an axis when the
	threshold is met, then subtracts the dominant magnitude to preserve ratio.

Why it exists

	* Smooth diagonal motion without floats.
	* Fixed cost per frame; deterministic on 8-bit CPUs.
	* Preserves the ΔX:ΔY ratio at pixel granularity.

Core quantities

	* |ΔX|, |ΔY|: absolute deltas to the waypoint.
	* dominant := max(|ΔX|, |ΔY|) for scale.
	* accum_x, accum_y: step accumulators.
	* dir_x, dir_y: 1-bit signs (X: 0=left,1=right; Y: 0=up,1=down).

Algorithm (per waypoint activation)

	1. Compute |ΔX|, |ΔY| and dir_x/dir_y from position vs target.
	2. dominant := max(|ΔX|, |ΔY|).
	3. Seed accum_x := |ΔX|, accum_y := |ΔY|.
	4. Each frame:
	   a) accum_x += |ΔX|; if accum_x ≥ dominant → step ±1 on X; accum_x -= dominant.
	   b) accum_y += |ΔY|; if accum_y ≥ dominant → step ±1 on Y; accum_y -= dominant.
	   c) Stop when position == target on both axes.

Properties

	* If |ΔX|=0 → vertical motion only; if |ΔY|=0 → horizontal only.
	* If both zero → no motion needed.
	* The larger delta defines cadence (dominant axis sets the period).
	* Order of X/Y tests can be tuned around collision/walkbox checks.

Mapping to variables

	* |ΔX| → dda_dx_abs; |ΔY| → dda_dy_abs.
	* dominant → dda_dominant_abs.
	* accum_x → actor_dx_accum; accum_y → actor_dy_accum.
	* dir_x → actor_dir_x_bit; dir_y → actor_dir_y_bit.
	* Horizontal/vertical dominance flag → dda_dominant_axis.

Edge cases

	* Reaching one axis early: continue stepping the other until both match.
	* Walkbox invalidation: revert tentative step and treat as box-change.
	* Zero-motion guard: return “no move” before seeding accumulators.

Complexity

	* O(1) time and memory per frame; all integer ops, branch-predictable on 6502.
	
================================================================================

Example of walkboxes attributes

Room 1 - Front of mansion (where mansion, mailbox and part of the fence is shown)

	10 walkboxes, numbered 0 to 9
		0 L: 00 R: 07 T: 3A B: 43 Attr: 03	- Grass area, right behind the mailbox/package
		1 L: 08 R: 12 T: 3A B: 43 Attr: 00	- Grass area, between the mailbox and the left edge of the mansion
		2 L: 13 R: 2C T: 3D B: 43 Attr: 00	- Grass area, left of the steps, front of the mansion bricks
		3 L: 17 R: 5D T: 2D B: 2D Attr: 03	- Porch (one-line tall walkbox)
		4 L: 37 R: 3E T: 2E B: 42 Attr: 00	- Rectangular area on the steps
		5 L: 2D R: 47 T: 43 B: 43 Attr: 00	- Grass area, in front of steps (one-line tall walkbox)
		6 L: 48 R: 61 T: 3D B: 43 Attr: 00	- Grass area, right of the steps, front of the mansion bricks
		7 L: 62 R: 7E T: 3D B: 43 Attr: 00	- Grass area, right of the mansion bricks
		8 L: 7F R: 9D T: 3A B: 3E Attr: 03	- Grass area, behind the metal fence
		9 L: 7F R: 9D T: 41 B: 43 Attr: 00	- Graas area, in front of metal fence

		Attr: 
			00 = normal walkbox - no special behavior, actor not masked by background
			03 = "background walkbox" - no special behavior, actor masked by background

		Adjacency map

					|-----3------|				Porch (3)
						  4  					Steps (4)
		0 <-> 1 <-> 2 <-> 5 <-> 6 <-> 7 <-> 8	Behind fence (8)
									  ^---> 9	In front of fence (9)

		Adjacency list
			0: [1]                                                                                                                                                          
			1: [2, 0]                                                                                                                                                       
			2: [5, 1]                                                                                                                                                       
			3: [4]                                                                                                                                                          
			4: [5, 3]                                                                                                                                                       
			5: [6, 2, 4]                                                                                                                                                    
			6: [7, 5]                                                                                                                                                       
			7: [6, 8, 9]                                                                                                                                                    
			8: [7]                                                                                                                                                          
			9: [7]                                                                                                                                                          
            
Room 2 - bottom of pool

		0 L: 00 R: 04 T: 35 B: 3A Attr: 88		Next to the left wall - diagonal slope
		1 L: 00 R: 05 T: 3B B: 43 Attr: 00		Next to the left wall - square area
		2 L: 06 R: 0D T: 41 B: 43 Attr: 00		In front of floating chair + radio
		3 L: 05 R: 0D T: 35 B: 3A Attr: 03		Behind floating chair
		4 L: 0E R: 18 T: 35 B: 43 Attr: 00		In front of ladder, on top of grating
		5 L: 19 R: 27 T: 3C B: 43 Attr: 00		In front of reactor part
		6 L: 19 R: 22 T: 35 B: 37 Attr: 03		Behind reactor part
		7 L: 12 R: 12 T: 03 B: 34 Attr: 84 		One-line wide ladder box

================================================================================

Walkbox attribute byte (#4) - semantic map and bit layout

Context
	The fourth byte in each walkbox record is a mixed “attribute tag” consumed by
	the motion and layering system. The tag may either be a plain depth/masking
	value or a behavior-bearing code that also implies a depth. Code paths that
	read and act on this byte: box adoption and masking rules, and the layer
	handler dispatcher. 

Bit layout
	bit7 = 0 → plain attribute (no handler dispatch)
	bit7 = 1 → behavior handler present; index = (attr & $7C) >> 2
	bits1..0 = depth/mask sub-bits (interpretation below)

Canonical values and meanings:

	00  : normal walkbox, not masked by foreground (FG). Depth/mask sub-bits=0.
	
	03  : normal walkbox, masked by FG. Depth/mask sub-bits=3.

	84  : ladder behavior. Forces facing “Up” while on the box.
		  handler index = 1 → handler writes facing override = $C1,
		  which resolves to direction mask $81 (Up). Not FG-masked.
		  Depth/mask sub-bits=0.

	88  : diagonal slope (left/down), not masked by FG.
		  handler index = 2 → handler clears prog index flag; slope semantics
		  are provided by this handler class. Depth/mask sub-bits=0.

	8B  : diagonal slope (left/down), masked by FG.
		  handler index = 2, FG-masked variant. Depth/mask sub-bits=3.

	8C  : diagonal slope (right/down), not masked by FG.
		  handler index = 3 → alternate slope handler; not FG-masked.
		  Depth/mask sub-bits=0. 

	8F  : diagonal slope (right/down), masked by FG.
		  handler index = 3, FG-masked variant. Depth/mask sub-bits=3.


Operational notes

	* Plain attributes (bit7=0) do not enter the handler dispatcher; they only set
	  depth/masking for the current box. 
	* Programmed attributes (bit7=1 and attr≠$FF) extract a 5-bit index, may clear
	  or refresh per-actor box attribute flags, and then retain only the low sub-bits for
	  depth/masking after the handler returns. 
	* The ladder “Up” facing comes from writing $C1 into facing override, which masks to
	  $81 when applied as a one-shot facing override. 
	  
================================================================================
Actor Motion — techniques and algorithms used
================================================================================

• Destination/waypoint staging:
    Scripted destinations are snapped into valid walkbox coordinates; off-screen
    actors cache destinations, on-screen actors enter the path/waypoint system.

• Integer DDA stepping:
    init_dda_for_path derives |ΔX|, |ΔY|, dominant axis, and sign bits; per-frame
    stepping uses integer accumulators to emit 1-pixel moves that preserve the
    ΔX:ΔY ratio with no multiplies or divides.

• Packed motion state machine:
    actor_motion_state’s low nibble encodes walk/stop/traversal modes, while
    high bits encode turning/facing; update_actor_motion branches on this packed
    state to coordinate motion, facing, and transitions.

• Direction and traversal masks:
    DDA sign bits are compressed into small axis/sign masks; traversal masks
    normalize left/right/up/down handling and drive both stepping logic and
    animation state.

• Walkbox traversal integration:
    When a DDA step crosses a walkbox boundary, horizontal/vertical traversal
    handlers cooperate with the walkbox/pathing layer to keep motion constrained
    to valid boxes and update box-derived attributes.

• Facing/animation resolution:
    resolve_turning_and_facing merges DDA direction with box overrides (e.g.,
    ladders) to select canonical facing; standing/walking clips map these states
    to animation limb sets.

• Attribute-driven dispatch:
    High bits of walkbox attributes select optional behavior handlers (ladder,
    slopes, etc.) via a small table-driven JSR stub; low bits provide depth/mask
    info for layering.

================================================================================
*/
#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "pathing.asm"
#import "actor_animation.asm"

.label box_attr_ptr              = $17  // ZP: box-attribute table base pointer (lo=$17, hi=$18)
.label dbg_gate                  = $2F  // Debug gate: bit7=1 bypass waits; write #$00 to enable waits

.label box_attr_handler_jsr_op   = $2E0D  // Code-site: JSR operand for selected box-attr handler

.label motion_needed             = $FC3B  // Alias of shared scratch: #$00=motion, #$01=no motion (from init_dda_for_path)
.label axis_bit_scratch          = $FC3B  // Alias of shared scratch: holds bit7 result for axis compare
.label selected_box_attr         = $FC3B  // Alias of shared scratch: selected box attribute value
.label motion_nibble_tmp         = $FC3B  // Alias of shared scratch: temp for low-nibble motion code
// Note: $FC3B is a shared scratch slot; lifetime is call-local only.

.label selected_handler_idx      = $FC3D  // Scratch: index of chosen box-attribute handler
.label last_box_vedge            = $FC3F  // Scratch: cached vertical edge from last tested box
.label scan_box_idx              = $FDAC  // Scratch: index of box currently under consideration

.label dda_dy_abs                = $CB61  // |ΔY| to waypoint (pixels)
.label dda_dx_abs                = $CB62  // |ΔX| to waypoint (pixels)
.label dda_dominant_abs          = $CB63  // max(|ΔX|,|ΔY|) used for DDA scaling
.label dda_x_accum_seed          = $CB64  // DDA X accumulator seed
.label dda_y_accum_seed          = $CB65  // DDA Y accumulator seed

.const INIT_SCAN_IDX               = $FF    // Initial box scan index; pre-increments to 0
.const EDGE_INVALID                = $FF    // Sentinel: last_box_vedge is invalid
.const BREAK_SCAN                  = $00    // Control: stop scanning boxes
.const CONTINUE_SCAN               = $FF    // Control: continue scanning boxes

.const BOX_CODE_INDEX_MASK         = $7C    // Bits 2..6 of attr: handler index
.const BOX_ATTR_DEPTH_MASK         = $03    // Bits 0..1 of attr: depth value

.const PATH_RET_MOVE_NEEDED        = $00    // DDA init: motion required (any delta ≠ 0)
.const PATH_RET_NO_MOVE            = $01    // DDA init: no motion (both deltas = 0)
.const DOM_AXIS_H                  = $00    // Dominant axis flag: horizontal (|ΔX| ≥ |ΔY|)
.const DOM_AXIS_V                  = $01    // Dominant axis flag: vertical   (|ΔY| >  |ΔX|)

.const MOTION_WALKING              = $00    // Low nibble: regular walking
.const MOTION_ABOUT_TO_STOP        = $01    // Low nibble: about to stop
.const MOTION_STOPPED              = $02    // Full state value: fully stopped
.const MOTION_TRAVERSE_HORIZ       = $03    // Low nibble: traverse horizontally
.const MOTION_TRAVERSE_VERT        = $04    // Low nibble: traverse vertically

.const MSK_TURN_STAND              = $80    // Flag: standing/turning (bit7)

// Traversal direction mask byte (DIR_AXIS_BIT|DIR_SIGN_BIT encoding)
.const TRV_DN_MASK                 = $80    // Vertical, down
.const TRV_UP_MASK                 = $81    // Vertical, up
.const TRV_RT_MASK                 = $00    // Horizontal, right
.const TRV_LT_MASK                 = $01    // Horizontal, left

.const FACING_OVERRIDE_BIT         = $40    // actor_box_facing_override: apply override (bit6)
.const FACING_OVERRIDE_CLEAR_MASK  = $BF    // AND mask to clear override bit6
.const FG_MASKING_CODE             = $03    // actor_box_fg_masking: FG-occluded value
.const FG_MASKING_CLEAR            = $00    // actor_box_fg_masking: clear FG masking
.const BOX_UNRESOLVED_CODE         = $FF    // update_actor_walkbox_state: box undetermined

.const MOTION_FLAG_TURNING         = $80    // motion_state_for_actor: turning flag (bit7)
.const MOTION_FLAG_CLEAR_TURNING   = $7F    // AND mask: clear turning flag

.const DIR_AXIS_BIT                = $80    // Direction mask: axis bit (0=H, 1=V)
.const DIR_SIGN_BIT                = $01    // Direction mask: sign (0=+ right/down, 1=− left/up)
.const DIR_SIGN_CLEAR_MASK         = $FE    // AND mask: clear sign bit
.const DIR_RIGHT_MASK              = $00    // Encoded facing: right
.const DIR_LEFT_MASK               = $01    // Encoded facing: left
.const DIR_DOWN_MASK               = $80    // Encoded facing: down
.const DIR_UP_MASK                 = $81    // Encoded facing: up

.const BOX_RESULT_UNCHANGED        = $00    // Return A: current box unchanged
.const BOX_RESULT_CHANGED          = $01    // Return A: current box changed
.const BOX_RESULT_UNDETERMINED     = $FF    // Return A: box could not be determined
.const ATTR_INVALID                = $FF    // Walkbox attribute sentinel: no box

.const XAXIS_RESULT_CONTINUE       = $00    // X step: not reached, no box change
.const XAXIS_RESULT_REACHED        = $01    // X step: waypoint reached
.const XAXIS_RESULT_WBOX_CHANGED   = $03    // X step: walkbox changed
.const YAXIS_RESULT_CONTINUE       = $00    // Y step: not reached, no box change
.const YAXIS_RESULT_REACHED        = $01    // Y step: waypoint reached
.const YAXIS_RESULT_WBOX_CHANGED   = $04    // Y step: walkbox changed

.const FACING_OVERRIDE_UP          = $C1    // actor_box_facing_override encoded “up”
.const FG_OCCLUDED                 = $03    // Actor rendered behind foreground
.const BORDER_WAIT_FG_MASK_COLOR   = $05    // VIC border color during debug wait

/*
================================================================================
set_actor_destination
================================================================================

Summary:
    Prepares an actor to move toward a specified destination. Differentiates
    between off-screen actors (stores destination directly) and on-screen actors
    (snaps destination to valid walkboxes and stages path updates).

Arguments:
     active_costume                 Index of the currently active costume
     actor_for_costume[]            Maps costume indices to actor indices
     dest_x, dest_y                 Desired destination coordinates (pixels)

Returns:
    None

Global Inputs:
     actor_for_costume[]            Lookup table from costume → actor
     dest_x, dest_y                 Requested destination

Global Outputs:
     costume_dest_{x,y}[]           Off-screen actor target
     actor                          Active actor index (ZP)
     actor_{x,y}_dest[]             Snapped destination for on-screen actor

Description:
    • Checks if the actor is on-screen by reading actor_for_costume.
        – If off-screen (negative index), copies dest_x/dest_y into
          costume_dest_x/y for later processing and exits immediately.
        – If on-screen (non-negative), proceeds to snap the destination to
          the nearest valid walkbox.
    • Stores snapped coordinates in actor_x_dest/y_dest for the pathfinding
      system.
    • Tail-calls snap_and_stage_path_update to initiate path traversal.

Notes:
    • Off-screen actors skip pathing logic and are assumed to be teleported
      directly when they become active.
    • On-screen snapping ensures that the actor’s destination is always valid
      within the environment’s walkable areas.
    • This routine bridges high-level destination requests with the per-actor
      pathing and DDA system.
================================================================================
*/
* = $1C1F
set_actor_destination:
        ldx     active_costume                 // X := index of the currently active costume
        lda     actor_for_costume,x            // A := actor index corresponding to this costume
        bpl     actor_onscreen                 // If actor index ≥ 0 → on-screen actor, branch

	// ------------------------------------------------------------
	// Off-screen actor: store destination for later use
	//
	// - No pathing or snapping performed
	// - Returns immediately
	// ------------------------------------------------------------
actor_offscreen:
        lda     dest_x
        sta     costume_dest_x,x                // save target X for costume
        lda     dest_y
        sta     costume_dest_y,x                // save target Y for costume
        rts                                     // exit routine

	// ------------------------------------------------------------
	// On-screen actor: snap destination and stage path
	//
	// - Prepares actor for path traversal within valid walkboxes
	// ------------------------------------------------------------
actor_onscreen:
        tax                                     // X := actor index for array access
        stx     actor                           // store active actor index in ZP

        jsr     snap_coords_to_walkbox          // clamp dest_x/dest_y to valid walkbox

        lda     dest_x
        sta     actor_x_dest,x                  // store snapped X destination for actor
        lda     dest_y
        sta     actor_y_dest,x                  // store snapped Y destination for actor

        jmp     snap_and_stage_path_update      // tail-call path staging routine
/*
================================================================================
update_actor_motion
================================================================================

Summary:
    Drives actor motion toward the current desired waypoint. If the desired
    waypoint differs from the active one, it installs the new waypoint, resets
    the actor’s motion state, computes path deltas using DDA logic, and stages
    directional traversal and limb selection. If the actor is already at the
    waypoint, it evaluates whether animation is still required (e.g., due to
    MOTION_ABOUT_TO_STOP or mismatched position).

Arguments:
     actor                          Current actor index (ZP)
     actor_tgt_waypoint_{x,y}[]     Per-actor destination waypoint (pixels)
     actor_active_wpt_{x,y}[]       Current installed waypoint (pixels)
     actor_{x,y}_pos[]              Actor current screen position (pixels)
     actor_motion_state[]           Actor motion/turning state

Returns:
    None

Global Inputs:
     dda_dominant_axis              DDA: 0=X-dominant, ≠0=Y-dominant
     dir_x_bit                      DDA: 1=right, 0=left
     dir_y_bit                      DDA: 1=down, 0=up

Global Outputs:
     actor_active_wpt_{x,y}[]       Updated to match desired waypoint
     position_to_waypoint_{x,y}_for_actor[]		Current position snapshot for DDA stepping
     actor_motion_state[]           Updated low nibble with motion-needed flag
     path_delta_directions_for_actor[]			Traversal direction mask

Description:
    • If the desired waypoint differs from the current one, install it as active.
    • Clear the actor’s motion state low nibble and stage a new path:
        – Snapshot current position.
        – Initialize DDA step deltas and extract direction flags.
        – Merge result into motion state.
        – Compute and store traversal direction mask.
    • Select appropriate limbs based on facing/turning status.
        – If turning, exit.
        – If ready to walk, select walking limbs and exit.
    • If desired == active:
        – If MOTION_ABOUT_TO_STOP is set, animate.
        – If current pos ≠ target pos, animate.
        – Else exit without action.

Notes:
    • Returns immediately when already at destination and no further animation needed.
    • Uses bit 7 of actor_motion_state to differentiate turning vs walking.
    • Tail-calls step_actor_along_path when animation is required.

================================================================================

This routine manages an on-screen actor’s motion toward a “desired waypoint” in
a grid or walkbox-based environment. Its goal is to keep the actor moving 
smoothly and updating its animation according to whether it is walking or turning.

1. Waypoint Comparison
   - First, it checks if the actor’s desired waypoint differs from the currently
     active waypoint. If so, it needs to set up a new path; otherwise, it may
     just continue animation along the existing path.

2. Motion State & DDA Setup
   - If a new waypoint is installed:
       • The actor’s motion state is prepared by clearing the low nibble (motion
         sub-state) while preserving turning/facing info (high nibble).
       • The actor’s current X/Y is copied as the origin for the DDA (Digital
         Differential Analyzer) computation.
       • init_dda_for_path calculates step deltas and determines which axis
         is dominant (X or Y) along with directional bits.

3. Traversal Mask
   - The dominant axis and direction bits are converted into a single byte
     mask that guides the actor’s movement along the path. X- or Y-dominant
     movement is encoded separately, making downstream motion logic simpler.

4. Facing and Limbs
   - The routine resolves whether the actor is turning or facing a new direction
     (resolve_turning_and_facing) and sets a neutral (standing) limb set.
   - If the actor is not turning, walking limbs are activated. Otherwise, the
     routine exits early, leaving the actor in a standing/turning state.

5. Animation Handling
   - If the actor is already at the waypoint but flagged as “about-to-stop” or
     not yet at the exact X/Y position, it jumps to step_actor_along_path
     to continue the animation without altering path data.

Summary:
	- Handles both updating active waypoints and walking animation.
	- Uses bit flags and DDA outputs to coordinate movement and facing.
	- Ensures the actor’s limbs, facing, and motion state are consistent with
	  current path progress.
	- Efficiently skips work if actor is already at the target and not moving.
================================================================================
*/
* = $29f8
update_actor_motion:
        // ------------------------------------------------------------
        // Waypoint comparison section
		//
        // Verify if the desired waypoint differs from the active one.
        // The comparison proceeds X first, then Y. If any mismatch
        // is detected, control jumps to set_new_active_waypoint to
        // reinitialize motion and DDA parameters.
        // ------------------------------------------------------------
        ldx     actor                           // X := actor index
        lda     actor_tgt_waypoint_x,x          // A := desired X
        cmp     actor_active_wpt_x,x            
        bne     set_new_active_waypoint         // X differs → install new active
        lda     actor_tgt_waypoint_y,x          // A := desired Y
        cmp     actor_active_wpt_y,x            
        bne     set_new_active_waypoint         // Y differs → install new active


        // ------------------------------------------------------------
        // Active waypoint unchanged. Check if actor should animate:
		//
        // - If low 7 bits = MOTION_ABOUT_TO_STOP → begin animation.
		// - Else fall through to position checks
        // ------------------------------------------------------------
        lda     actor_motion_state,x            // A := actor’s motion state
        and     #MSK_LOW7BITS                   // mask off turning flag (bit7)
        cmp     #MOTION_ABOUT_TO_STOP           // compare with “about to stop” code
        beq     begin_actor_animation           // equal → trigger walking animation

        // ------------------------------------------------------------
        // Verify if actor has reached active waypoint
		//
        // - Compare Y then X; branch to animation if position differs
        // - Return if already at target and not about-to-stop
        // ------------------------------------------------------------
        lda     actor_tgt_waypoint_y,x          // A := desired Y
        cmp     actor_y_pos,x                   
        bne     begin_actor_animation           // Y differs → advance animation
        lda     actor_tgt_waypoint_x,x          // A := desired X
        cmp     actor_x_pos,x                   
        bne     begin_actor_animation           // X differs → advance animation
        rts                                     // At waypoint and not about-to-stop → done

begin_actor_animation:
        jmp     step_actor_along_path      // Tail-call walking animator to advance motion; no return here

        // ------------------------------------------------------------
        // Install new active waypoint
		//
        // - Copy desired X/Y into active waypoint registers
        // - Prepares actor for motion/DDA toward new target
        // ------------------------------------------------------------		
set_new_active_waypoint:
        lda     actor_tgt_waypoint_x,x          
        sta     actor_active_wpt_x,x            // active_wpt_x := desired X
        lda     actor_tgt_waypoint_y,x          
        sta     actor_active_wpt_y,x            // active_wpt_y := desired Y

        // ------------------------------------------------------------
        // Set actor base state to “moving”
		//
        // - Preserve high nibble (turning/facing)
        // - Clear low nibble to mark movement start
        // ------------------------------------------------------------		
set_state_to_moving:
        ldx     actor                           // X := actor index
        lda     actor_motion_state,x            // A := current motion state
        and     #MSK_HIGH_NIBBLE                // keep high nibble (turning/facing), zero low nibble
        sta     actor_motion_state,x            // update motion state to “moving” baseline


        // ------------------------------------------------------------
        // Snapshot current position as origin for DDA traversal
		//
        // - X/Y copied to position_to_waypoint registers
        // ------------------------------------------------------------
        lda     actor_x_pos,x                   // A := current X
        sta     position_to_waypoint_x_for_actor,x
        lda     actor_y_pos,x                   // A := current Y
        sta     position_to_waypoint_y_for_actor,x

        // ------------------------------------------------------------
        // Initialize DDA for path to active waypoint
		//
        // - Computes step deltas, dominant axis, and direction bits
        // - Returns A = $00 if motion needed, $01 if already at target
        // - Save result to motion_needed for later merge into motion state
        // ------------------------------------------------------------
        jsr     init_dda_for_path               // derive ΔX, ΔY, dir bits, dominant axis
        ldx     actor                           // restore actor index
        sta     motion_needed                   // save motion-needed flag for state merge

        // ------------------------------------------------------------
        // Merge motion_needed flag into low nibble of motion state
		//
        // - Preserve high nibble (turning/facing)
        // - Low nibble now indicates whether movement is required
        // ------------------------------------------------------------
        lda     actor_motion_state,x            // A := current state
        and     #MSK_HIGH_NIBBLE                // clear low nibble
        ora     motion_needed                   // set low nibble from computed flag
        sta     actor_motion_state,x            // commit updated state

        // ------------------------------------------------------------
        // Determine traversal mask based on dominant axis and direction
		//
        // - dda_dominant_axis = 0 → X-dominant, else Y-dominant
        // - Branch to Y-dominant handling if necessary
        // ------------------------------------------------------------
        lda     dda_dominant_axis              // A := dominant-axis flag
        bne     process_y_dominant_direction   // ≠0 → handle Y-dominant

        // ------------------------------------------------------------
        // X-dominant: select traversal mask based on horizontal direction
		//
        // - dir_x_bit = 0 → left, else right
        // ------------------------------------------------------------
        lda     dir_x_bit                      // test X direction
        beq     x_direction_left               // 0 → left

        lda     #TRV_RT_MASK                   // X right mask
        jmp     set_delta_directions_for_actor
x_direction_left:
        lda     #TRV_LT_MASK                   // X left mask
        jmp     set_delta_directions_for_actor

        // ------------------------------------------------------------
        // Y-dominant: select traversal mask based on vertical direction
		//
        // - dir_y_bit = 0 → up, else down
        // ------------------------------------------------------------
process_y_dominant_direction:
        lda     dir_y_bit                      // test Y direction bit
        beq     y_direction_up                 // 0 → moving up

        lda     #TRV_DN_MASK                   // Y down mask
        jmp     set_delta_directions_for_actor
y_direction_up:
        lda     #TRV_UP_MASK                   // Y up mask

set_delta_directions_for_actor:
        // ------------------------------------------------------------
        // Store computed traversal mask for actor
		//
        // - path_delta_directions_for_actor[x] receives the mask
        // ------------------------------------------------------------
        sta     path_delta_directions_for_actor,x   // Store traversal mask for this actor (indexed by X)

		// Redundant code
		lda actor_box_fg_masking,X	
		cmp #$03		
		bne dummy_dn		
dummy_dn:
		jsr do_nothing_2C22

        // ------------------------------------------------------------
        // Resolve facing/turning and apply standing limb set
		//
        // - Updates motion_state for current orientation
        // - Sets neutral limbs before determining walking animation
        // ------------------------------------------------------------
        jsr     resolve_turning_and_facing         // update facing/turning flags in motion_state
        jsr     apply_standing_clip       // apply baseline limbs before deciding to walk

        // ------------------------------------------------------------
        // Check if actor is turning/standing
		//
        // - Bit7 = MOTION_FLAG_TURNING
        // - If clear → start walking clip
        // - If set → return without changing limbs
        // ------------------------------------------------------------
        lda     actor_motion_state,x               // A := motion_state
        and     #MOTION_FLAG_TURNING               // isolate turning flag (bit7)
        beq     init_walking_clip                  // 0 → not turning → start walking animation
        rts                                        // turning/standing → exit

init_walking_clip:
        // ------------------------------------------------------------
        // Initialize walking limbs for actor
		//
        // - Sets walking clip based on current facing/direction
        // ------------------------------------------------------------
        jsr     apply_walking_clip       // Select walking clip/limbs for current facing
		// Fall through to step_actor_along_path
/*
================================================================================
  step_actor_along_path
================================================================================
Summary
	Drive per-frame walking behavior for the current actor. Handles stop and
	about-to-stop gates, stand/turn resolution, entry into walking pose, regular
	in-box DDA stepping, and mode switches into walkbox traversal (horizontal or
	vertical) when a boundary is crossed.

Arguments
	None (routine uses the global actor index and per-actor state)

Vars/State
	 actor                                 current actor index
	 actor_motion_state                    per-actor motion mode/flags
	 path_delta_directions_for_actor       per-actor seeded traverse mask
	 motion_nibble_tmp                     scratch: holds low-nibble result
	 actor_dir_x_bit                       X direction bit: 0=left, ≠0=right
	 actor_dir_y_bit                       Y direction bit: 0=up,   ≠0=down

Global Inputs
	actor, actor_motion_state[actor], actor_dir_x_bit[actor], actor_dir_y_bit[actor]

Global Outputs
	actor_motion_state[actor] low nibble set to one of:
		path_delta_directions_for_actor[actor] seeded with:
			Actor limbs/facing updated via called helpers.

Returns
	Typical fall-through “still walking” path returns with A = #$00.
	Other exits may leave A unspecified.

Description
	• If motion == MOTION_STOPPED → return.
	• If low nibble == MOTION_ABOUT_TO_STOP → reset_actor_render_flags and return.
	• If standing/turning (bit7 set):
		– resolve_turning_and_facing, set standing limbs;
		– if still turning → return; else fall into walking pose setup.
	• Enter walking pose → apply_walking_clip.
	• Fast-path dispatch by low nibble:
		– MOTION_TRAVERSE_HORIZ → traverse_horizontally.
		– MOTION_TRAVERSE_VERT  → traverse_vertically.
		– MOTION_WALKING        → regular in-box step.
		– otherwise → exit.
	• Regular in-box step:
		– Step X via DDA. If reached, step Y; if either axis reports walkbox
		change, switch to corresponding traverse mode; if both reached, set
		low nibble to MOTION_ABOUT_TO_STOP.
		– When switching to traverse, seed path_delta_directions_for_actor using
		the orthogonal direction bit (Y for horizontal traverse, X for vertical),
		update facing and limbs, and jump to the traverse handler.

Notes
	• Bit layout assumptions:
		– Low nibble encodes motion sub-mode.
		– Bit7 (MSK_TURN_STAND) marks stand/turn state.
		
================================================================================

This routine manages one walking update for a single actor. It keeps three
things aligned: the motion state (what phase they are in), the facing
direction (which way the sprite should point), and the animation clip
(standing vs walking). The result is movement that looks smooth instead of
snapping.

Entry checks:

	* If the motion state says "stopped" (#$02), nothing happens and the routine
	  returns immediately.
	* If the low nibble is "about to stop" (#$01), the routine resets animation
	  to a standing pose. This is the final frame before becoming fully stopped.

Standing and turning:

	* If the "standing/turning" flag (bit7) is set, the actor is not walking yet.
	  The routine calls resolve_turning_and_facing to reconcile the intended move
	  direction with the current facing. That logic may enforce a 90° pivot when
	  the actor tries to reverse along the same axis, which prevents abrupt 180°
	  flips. After that, a standing clip is applied. If the actor is still marked
	  as turning, the routine exits; otherwise it proceeds to walking.

Walking pose and dispatch:

	* When transitioning into motion, the routine sets the walking clip that
	  matches the current facing. It then inspects the low nibble of the motion
	  state to decide what kind of step to run:
		  * #$03 → traverse horizontally across a walkbox boundary
		  * #$04 → traverse vertically across a walkbox boundary
		  * #$00 → regular walking inside the current walkbox

Regular walking (inside one box):

	* The routine advances X first via step_actor_x_dda. That function returns:
		  * #$00: still moving on X
		  * #$01: X reached its target
		  * #$03: X crossed a walkbox boundary
		  
	* If X reached its target, the routine advances Y via step_actor_y_dda:
		  * If Y also reached, it marks "about to stop" (low nibble ← #$01) and
			returns. This sets up the next frame to become fully stopped.
		  * If Y reported a walkbox change, the routine resets the animation state
			to re-enter cleanly.
		  * Otherwise it returns, indicating partial progress this frame.
		  
	* If X reported a walkbox change, the routine switches to "horizontal
	  traversal" mode (low nibble ← #$03). It seeds a Y-based direction mask
	  (up or down) into path_delta_directions_for_actor, calls
	  resolve_turning_and_facing to update the facing, reapplies the walking
	  clip, and jumps to the horizontal traversal handler.
	  
	* If X is still in progress, the routine advances Y. If Y reports a walkbox
	  change, it switches to "vertical traversal" mode (low nibble ← #$04),
	  seeds an X-based direction mask (left or right), updates facing, reapplies
	  the walking clip, and jumps to the vertical traversal handler. If not, the
	  routine returns with "still walking" (#$00 in A).

Why the axis-based masks during traversal:

	* On boundary crossings the actor must travel along a specific axis to reach
	  the next walkable region. The routine writes a constrained direction mask
	  (e.g., up or down for horizontal boundary handling) to drive facing updates
	  and animation consistently during the traversal.

State integrity:

	* When writing the low nibble to switch modes, the routine preserves the high
	  nibble of actor_motion_state. This prevents unrelated flags from being
	  clobbered while it selects between regular walking, horizontal traversal,
	  vertical traversal, and about-to-stop.

Effect:

	* Characters pivot naturally, adopt the correct walking or standing clips,
	  and cross between walkboxes without visual pops. Movement, facing, and
	  animation stay synchronized frame to frame.
 ================================================================================
*/
* = $2A9A
step_actor_along_path:
        // ------------------------------------------------------------
        // Early stop check
		//
        //   Load motion state and test for MOTION_STOPPED.
        //   If stopped, return; otherwise continue to about-to-stop gate.
        // ------------------------------------------------------------
        ldx     actor                          // X := current actor index
        lda     actor_motion_state,x            // load actor’s motion state
        cmp     #MOTION_STOPPED                 // test for fully stopped state (#$02)
        bne     check_if_about_to_stop          // if not stopped → continue walking logic
        rts                                     // stopped → no update needed this frame

        // ------------------------------------------------------------
        // About-to-stop check
		//
        //   Examine the low nibble of the motion state.
        //   If it equals MOTION_ABOUT_TO_STOP, reset animation
        //   to a standing pose; otherwise continue to the stand/turn gate.
        // ------------------------------------------------------------
check_if_about_to_stop:
        and     #MSK_LOW_NIBBLE                 // isolate low nibble of motion state
        cmp     #MOTION_ABOUT_TO_STOP           // equals #$01 → transition to standing
        bne     stand_turn_gate                 // not about to stop → handle stand/turn gate
        jmp     reset_actor_render_flags     // about-to-stop → reset limbs/flags to standing

        // ------------------------------------------------------------
        // Stand/turn gate
		//
        //   Test bit7 of motion state to detect standing/turning.
        //   Set path for turn resolution; otherwise take walking fast path.
        // ------------------------------------------------------------
stand_turn_gate:
        lda     actor_motion_state,x           // fetch motion flags
        and     #MSK_TURN_STAND                // test standing/turning bit (bit7)
        beq     walking_fastpath               // clear → not standing/turning; take walking path

        // ------------------------------------------------------------
        // Standing/turning update
		//
        //   Actor is stationary or mid-turn.
        //   Resolve current facing relative to movement delta and
        //   apply the proper standing animation until the turn completes.
        // ------------------------------------------------------------
        jsr     resolve_turning_and_facing     // reconcile facing with delta; may pivot or mark turning
        jsr     apply_standing_clip   // apply standing clip while turning/idle

        // ------------------------------------------------------------
        // Post-turn evaluation
		//
        //   After resolving facing and pose, check if actor remains
        //   in a standing or turning state. If still turning, exit and
        //   wait for next frame; otherwise continue to walking pose.
        // ------------------------------------------------------------
        lda     actor_motion_state,x           // reload motion state after facing/pose update
        and     #MSK_TURN_STAND                // test if bit7 (turn/stand) still set
        beq     enter_walking_pose             // clear → transition to walking pose
        rts                                    // still turning → wait next frame

        // ------------------------------------------------------------
        // Enter walking pose
		//
        //   Actor finished turning and is ready to walk.
        //   Apply the appropriate walking animation clip based on
        //   current facing direction, transitioning from idle/turn
        //   to active motion.
        // ------------------------------------------------------------
enter_walking_pose:
        jsr     apply_walking_clip    // apply walking animation clip based on facing

        // ------------------------------------------------------------
        // Walking fast-path dispatch
		//
        //   Read motion state low nibble and route:
        //     #MOTION_TRAVERSE_HORIZ → traverse_horizontally
        //     #MOTION_TRAVERSE_VERT → traverse_vertically
		//     #MOTION_WALKING → regular in-box walking step logic
        //     else → leave routine.
		//
        //   Keeps high-level walking loop tight when a traversal is active.
        // ------------------------------------------------------------
walking_fastpath:
        ldx     actor                          // X := current actor index
		
		//Redundant code - replicated from original
		lda 	actor_box_fg_masking,x
		cmp		#$03
		bne		dummy_wfp
		//
		
dummy_wfp:		
        lda     actor_motion_state,x           // load motion state
        and     #MSK_LOW_NIBBLE                // isolate low nibble selector

        cmp     #MOTION_TRAVERSE_HORIZ         //check for horizontal traversal mode
        bne     dispatch_traverse_vert         // no → test vertical traversal
        jmp     traverse_horizontally      // yes → run horizontal traversal handler

dispatch_traverse_vert:
        cmp     #MOTION_TRAVERSE_VERT          // check for vertical traversal mode
        bne     dispatch_regular_motion        // not vertical → test for regular walking
        jmp     traverse_vertically        // vertical traversal → handle boundary crossing

dispatch_regular_motion:
        cmp     #MOTION_WALKING                // check for regular walking mode
        beq     step_regular_motion            // yes → run per-axis DDA stepping
        jmp     exit_saap                      // no → leave routine (no walking step this frame)

        // ------------------------------------------------------------
        // Regular in-box walking step
		//
        //   Advance X first via DDA:
        //     00 → still moving on X
        //     01 → X reached; then step Y
        //     03 → X walkbox change → handled later
		//
        //   If X reached, step Y:
        //     00 → Y still moving
        //     01 → both axes reached → mark about-to-stop elsewhere
        //     04 → Y walkbox change → handled later
        // ------------------------------------------------------------
step_regular_motion:
        jsr     step_actor_x_dda               // step X axis; A ∈ {00=cont, 01=reached, 03=wbox change}
        cmp     #XAXIS_RESULT_REACHED          // X reached target?
        bne     x_not_reached_path             // no → handle X in-progress or X wbox change

        jsr     step_actor_y_dda               // X done; step Y axis; A ∈ {00=cont, 01=reached, 04=wbox change}
        cmp     #YAXIS_RESULT_REACHED          // Y reached target too?
        bne     y_not_reached_after_x          // no → handle Y in-progress or Y wbox change

        // ------------------------------------------------------------
        // Both axes reached → transition to about-to-stop
		//
        //   Preserve high nibble of motion state and set low nibble
        //   to MOTION_ABOUT_TO_STOP (#$01). Commit and return.
        // ------------------------------------------------------------
        ldx     actor                          // X := current actor index
        sta     motion_nibble_tmp              // save Y result (#$01)
        and     #MSK_HIGH_NIBBLE               // keep high nibble of motion state
        ora     motion_nibble_tmp              // combine with low nibble = about-to-stop
        sta     actor_motion_state,x           // commit updated motion state
        rts                                    // done for this frame

        // ------------------------------------------------------------
        // Y-axis progress after X reached
		//
        //   X target reached; evaluate Y step result.
        //   If Y crossed a walkbox boundary, reset animation to reenter
        //   cleanly. Otherwise return with partial progress (Y only).
        // ------------------------------------------------------------
y_not_reached_after_x:
        // X reached, Y still in progress
        cmp     #YAXIS_RESULT_WBOX_CHANGED     // did Y cross a walkbox boundary?
        bne     return_vertical_progress_only  // no → keep current walking state
        jmp     reset_actor_render_flags    // yes → reenter cleanly with standing reset

return_vertical_progress_only:
        rts                                     // partial progress this frame (Y only)

        // ------------------------------------------------------------
        // X-axis evaluation and horizontal traversal switch
		//
        //   If X step reports a walkbox change, enter horizontal
        //   traversal mode (low nibble = #$03) while preserving the
        //   high nibble flags. Otherwise continue with Y updates.
        // ------------------------------------------------------------
x_not_reached_path:
        // X not yet at target
        cmp     #XAXIS_RESULT_WBOX_CHANGED     // boundary crossed on X?
        bne     no_horiz_traverse_continue_y   // no → keep updating Y within box

        // X walkbox changed → enter horizontal traversal (low nibble = #$03)
        sta     motion_nibble_tmp              // save X result code (#$03)
        lda     actor_motion_state,x           // fetch current motion state
        and     #MSK_HIGH_NIBBLE               // preserve high nibble flags
        ora     motion_nibble_tmp              // set low nibble to traverse-horiz
        sta     actor_motion_state,x           // commit new mode

        // ------------------------------------------------------------
        // Seed traversal direction from Y bit
		//
        //   Read actor_dir_y_bit to choose UP or DOWN mask, write the
        //   mask into path_delta_directions_for_actor, then proceed to
        //   update facing and continue horizontal traversal.
        // ------------------------------------------------------------
        lda     actor_dir_y_bit,x               // read Y direction bit (0=up, ≠0=down)
        beq     set_dirmask_up                  // 0 → set UP mask

        lda     #TRV_DN_MASK                    // Y bit set → choose DOWN traversal mask
        sta     path_delta_directions_for_actor,x // seed path delta with DOWN
        jmp     commit_y_dirmask_and_walk       // update facing and continue walking

set_dirmask_up:
        lda     #TRV_UP_MASK                   // choose UP traversal mask
        sta     path_delta_directions_for_actor,x // seed path delta with UP

        // ------------------------------------------------------------
        // Commit Y-based traversal and continue horizontally
        //   Use the seeded UP/DOWN mask to resolve facing, apply the
        //   walking clip for the new facing, then jump to horizontal
        //   walkbox traversal to complete the boundary crossing.
        // ------------------------------------------------------------
commit_y_dirmask_and_walk:
        jsr     resolve_turning_and_facing     // adjust facing based on new Y-axis direction
        jsr     apply_walking_clip    // apply walking animation for updated facing
        jmp     traverse_horizontally      // proceed with horizontal walkbox traversal

        // ------------------------------------------------------------
        // Continue Y update when X didn't trigger traversal
		//
        //   Step Y within current box. If Y crosses a walkbox boundary,
        //   branch to vertical traversal handler; otherwise report
        //   "still walking" status and return.
        // ------------------------------------------------------------
no_horiz_traverse_continue_y:
        // Continue with Y-axis step inside current box
        jsr     step_actor_y_dda               // A ∈ {00=cont, 01=reached, 04=wbox change}
        cmp     #YAXIS_RESULT_WBOX_CHANGED     // boundary crossed on Y?
        beq     y_walkbox_changed              // yes → handle vertical traversal

        // No boundary and not at goal → report "still walking"
        lda     #$00                           // status: continue walking
        rts

		//Unreachable code - replicated from original
		jmp 	commit_x_dirmask_and_walk
		
        // ------------------------------------------------------------
        // Y walkbox boundary crossing
		//
        //   Enter vertical traversal mode (low nibble ← #$04) while
        //   preserving high-nibble flags. Store result code, then
        //   update actor_motion_state accordingly.
        // ------------------------------------------------------------
y_walkbox_changed:
        // Y walkbox changed → enter vertical traversal (low nibble = #$04)
        sta     motion_nibble_tmp              // save Y result code (#$04)
        lda     actor_motion_state,x           // fetch current motion state
        and     #MSK_HIGH_NIBBLE               // preserve high nibble flags
        ora     motion_nibble_tmp              // set low nibble to traverse-vert
        sta     actor_motion_state,x           // commit new mode

        // ------------------------------------------------------------
        // Seed traversal direction from X bit
		//
        //   Read actor_dir_x_bit to choose LEFT or RIGHT mask, write
        //   it into path_delta_directions_for_actor, then proceed to
        //   resolve facing and continue vertical traversal.
        // ------------------------------------------------------------
        lda     actor_dir_x_bit,x               // read X direction bit (0=left, ≠0=right)
        beq     set_dirmask_left                // 0 → choose LEFT mask

        lda     #TRV_RT_MASK                    // X bit set → choose RIGHT traversal mask
        sta     path_delta_directions_for_actor,x // seed path delta with RIGHT
        jmp     commit_x_dirmask_and_walk       // update facing and continue walking

set_dirmask_left:
        lda     #TRV_LT_MASK                   // choose LEFT traversal mask
        sta     path_delta_directions_for_actor,x // seed path delta with LEFT

        // ------------------------------------------------------------
        // Commit X-based traversal and continue vertically
		//
        //   Use the seeded LEFT/RIGHT mask to resolve facing, apply the
        //   walking clip for the updated facing, then jump to vertical
        //   walkbox traversal to complete the boundary crossing.
        // ------------------------------------------------------------
commit_x_dirmask_and_walk:
        jsr     resolve_turning_and_facing     // update facing based on new X-axis direction
        jsr     apply_walking_clip    // apply walking animation for updated facing
        jmp     traverse_vertically        // proceed with vertical walkbox traversal
exit_saap:
        rts                                     // unreachable (kept for alignment/consistency)
/*
================================================================================
  resolve_turning_and_facing
================================================================================

Summary
	Reconcile an actor’s facing with step-delta input. Apply a one-shot
	override if present, clear turning when aligned, or set turning and
	pick a transition (snap or 90° pivot) when directions differ.

Arguments
	None

Global Inputs
	actor                                Current actor index selector
	actor_box_facing_override[*]         One-shot override: flag + dir bits
	path_delta_directions_for_actor[*]   Intended facing from path step
	facing_direction_for_actor[*]          Current facing attribute
	actor_motion_state[*]                Motion flags (bit7 = turning)

Global Outputs
	path_delta_directions_for_actor[*]   Updated from override when used
	facing_direction_for_actor[*]          New facing attribute committed
	actor_motion_state[*]                Turning bit set/cleared accordingly

Description
	- If override flag set: copy masked override into delta and continue.
	- If delta equals current facing: clear turning and exit.
	- If already turning: snap facing to delta and keep turning set.
	- If not turning:
		• Same axis (e.g., L↔R or U↔D): force perpendicular pivot by
		toggling axis and clearing sign, then mark turning.
		• Different axis: adopt delta directly, then mark turning.

Notes
	- Clobbers: A, X, flags.
	- Uses axis_bit_scratch as a transient compare byte.
	- Zero/invalid deltas resolve via the same mask logic.
================================================================================
*/
* = $2B70
resolve_turning_and_facing:
        // ------------------------------------------------------------
        // One-shot facing override:
        //   If FACING_OVERRIDE_BIT set in actor_box_facing_override[X],
        //   clear that bit (mask with FACING_OVERRIDE_CLEAR_MASK) and copy
        //   the masked value into path_delta_directions_for_actor[X].
        //   Otherwise skip to direction compare.
        // ------------------------------------------------------------
        ldx     actor                          // X := actor index
        lda     actor_box_facing_override,x    // read override attr
        and     #FACING_OVERRIDE_BIT           // test one-shot flag
        beq     compare_dir_masks              // flag clear → no override

        lda     actor_box_facing_override,x    // load override attr (contains one-shot flag + direction bits)
        and     #FACING_OVERRIDE_CLEAR_MASK    // clear one-shot flag; keep direction bits to propagate
        sta     path_delta_directions_for_actor,x // push masked direction into this step’s delta input


		// ------------------------------------------------------------
		// If delta equals current, clear turning bit and exit
		// ------------------------------------------------------------
compare_dir_masks:
        lda     path_delta_directions_for_actor,x // load intended facing (delta)
        cmp     facing_direction_for_actor,x        // compare with current facing attribute
        beq     clear_actor_turning               // equal → no change; clear turning flag


        // ------------------------------------------------------------
        // Directions differ:
        //   If already turning, snap facing to delta.
        //   Else choose transition and mark turning.
        // ------------------------------------------------------------
        lda     actor_motion_state,x           // load motion flags
        bmi     begin_turning_transition       // bit7=1 → already turning

        // ------------------------------------------------------------
        // Not turning yet
		//
        // Compare direction axes to decide turning transition
        //   If current and intended directions share the same axis
        //   (both horizontal or both vertical), enforce a 90° pivot
        //   by toggling the axis bit. Otherwise, adopt the new delta.
        // ------------------------------------------------------------
        lda     facing_direction_for_actor,x     // load current facing attribute
        and     #DIR_AXIS_BIT                  // isolate axis (0=horizontal, 1=vertical)
        sta     axis_bit_scratch               // save attribute’s axis for comparison
        lda     path_delta_directions_for_actor,x // load intended facing (delta)
        and     #DIR_AXIS_BIT                     // isolate axis: 0=horizontal, 1=vertical
        cmp     axis_bit_scratch                  // compare to current attribute’s axis
        beq     force_axis_flip                   // same axis → enforce 90° pivot
        lda     path_delta_directions_for_actor,x // adopt delta as new facing value
        jmp     write_facing_attribute            // commit facing and set turning flag


        // ------------------------------------------------------------
        // Force perpendicular axis flip
		//
        //   Used when actor tries to reverse along the same axis
        //   (e.g., left↔right or up↔down). Toggles DIR_AXIS_BIT to
        //   pivot 90° and clears the sign bit to ensure a consistent
        //   facing direction before committing the new attribute.
        // ------------------------------------------------------------
force_axis_flip:
        lda     facing_direction_for_actor,x // load current facing attribute
        eor     #DIR_AXIS_BIT              // flip axis bit to force perpendicular pivot
        and     #DIR_SIGN_CLEAR_MASK       // clear sign bit → canonical positive direction
        jmp     write_facing_attribute     // commit facing; turning flag set in callee

        // ------------------------------------------------------------
        // Handle active turning state
        //   If actor is already turning, snap facing direction directly
        //   to the delta. Then write the resulting facing attribute.
        // ------------------------------------------------------------
begin_turning_transition:
        lda     path_delta_directions_for_actor,x // already turning → snap facing to delta

write_facing_attribute:
        sta     facing_direction_for_actor,x        // commit new facing attribute

        // ------------------------------------------------------------
        // Mark actor as turning
        //   Sets MOTION_FLAG_TURNING in actor_motion_state[X] to
        //   indicate the actor is mid-turn. Then jumps to the exit.
        // ------------------------------------------------------------
        lda     actor_motion_state,x           // load motion flags
        ora     #MOTION_FLAG_TURNING           // set turning bit
        sta     actor_motion_state,x           // store updated flags
        jmp     exit_update_motion_state       // unified exit

        // ------------------------------------------------------------
        // Clear turning flag and exit
        //   Resets MOTION_FLAG_TURNING in actor_motion_state[X] when
        //   facing direction already matches delta. Exits immediately
        //   afterward via a unified return point.
        // ------------------------------------------------------------
clear_actor_turning:
        lda     actor_motion_state,x           // load motion flags
        and     #MOTION_FLAG_CLEAR_TURNING     // clear turning bit
        sta     actor_motion_state,x           // store updated flags

exit_update_motion_state:
        rts                                     // done
/*
================================================================================
  apply_standing_clip
================================================================================

Summary
	Select a standing clip for the actor based on path direction and apply it,
	looping indefinitely.

Arguments
	X                            Actor index on entry

Global Inputs
	facing_direction_for_actor[*]  Direction mask per actor
	actor                        Actor index selector for post-call restore

Global Outputs
	target_clip_id               Selected standing clip ID
	clip_loop_cnt                Set to ANIM_LOOP_FOREVER
	(via assign_clip_to_costume)         Applies clip to limbs for the active costume

Returns
	.X                           Restored to actor

Description
	- Map direction mask to standing clip:
			DIR_RIGHT_MASK → CLIP_STAND_RIGHT
			DIR_LEFT_MASK → CLIP_STAND_LEFT
			DIR_UP_MASK → CLIP_STAND_UP
			otherwise → CLIP_STAND_DOWN
	- Store chosen clip and set loop count.
	- Call assign_clip_to_costume to realize the clip on limbs.
	- Restore X from actor and return.

Notes
	- Clobbers: A, X (X restored). Flags per last LDX.
================================================================================
*/
* = $2BCA
apply_standing_clip:
        // ------------------------------------------------------------
        // Map path direction mask to standing animation clip ID
        // ------------------------------------------------------------
        lda     facing_direction_for_actor,x
        bne     check_dir_left

        lda     #CLIP_STAND_RIGHT       // facing right
        jmp     commit_standing_clip

check_dir_left:
        cmp     #DIR_LEFT_MASK
        bne     check_dir_up

        lda     #CLIP_STAND_LEFT        // facing left
        jmp     commit_standing_clip

check_dir_up:
        cmp     #DIR_UP_MASK
        bne     map_dir_down

        lda     #CLIP_STAND_UP          // facing up
        jmp     commit_standing_clip

map_dir_down:
        lda     #CLIP_STAND_DOWN        // facing down (default)

commit_standing_clip:
        sta     target_clip_id          // commit target clip
        lda     #ANIM_LOOP_FOREVER		// set clip to loop forever
        sta     clip_loop_cnt                 
        jsr     assign_clip_to_costume			// apply clip
        ldx     actor                   // restore actor index
        rts
/*
================================================================================
  apply_walking_clip
================================================================================

Summary
	Select a walking clip for the actor based on path direction and apply it,
	looping indefinitely.

Arguments
	X                            Actor index on entry

Global Inputs
	facing_direction_for_actor[*]  Direction mask for each actor
	actor                        Actor index selector for post-call restore

Global Outputs
	actor_target_clip[*]         Walking clip ID per actor
	actor_clip_loop_cnt[*]       Set to ANIM_LOOP_FOREVER
	(via init_limb_state_from_clip_set)         Applies clip to limbs for the active costume

Returns
	.X                           Restored to actor

Description
	- Map direction mask to walking clip:
			DIR_RIGHT_MASK → CLIP_WALK_RIGHT
			DIR_LEFT_MASK → CLIP_WALK_LEFT
			DIR_UP_MASK → CLIP_WALK_UP
			otherwise → CLIP_WALK_DOWN
	- Store chosen clip in actor_target_clip[X].
	- Set loop count to ANIM_LOOP_FOREVER.
	- Call init_limb_state_from_clip_set to realize the clip on limbs.
	- Restore X from actor and return.

Notes
	- Clobbers: A, X (X restored), processor flags per last LDX.
	- Expects a single-direction mask or 0; any other non-zero maps to DOWN.
========================================================================
*/
* = $2BF6
apply_walking_clip:
        // ------------------------------------------------------------
        // Map path direction mask to walking animation clip ID
        // ------------------------------------------------------------
        lda     facing_direction_for_actor,x
        bne     check_dir_left_2

        lda     #CLIP_WALK_RIGHT		// facing right
        jmp     commit_walking_clip

check_dir_left_2:
        cmp     #DIR_LEFT_MASK
        bne     check_dir_up_2

        lda     #CLIP_WALK_LEFT         // facing left
        jmp     commit_walking_clip

check_dir_up_2:
        cmp     #DIR_UP_MASK
        bne     map_dir_down_2

        lda     #CLIP_WALK_UP           // facing up
        jmp     commit_walking_clip

map_dir_down_2:
        lda     #CLIP_WALK_DOWN         // facing down

commit_walking_clip:
        sta     actor_target_clip,x     // commit target clip
        lda     #ANIM_LOOP_FOREVER		// set clip to loop forever
        sta     actor_clip_loop_cnt,x         
        jsr     init_limb_state_from_clip_set			// setup clip
        ldx     actor                   // restore actor index
        rts
		
/*
================================================================================
  do_nothing_2C22
================================================================================
*/
* = $2C22		
do_nothing_2C22:		
		rts		
/*
================================================================================
  reset_actor_render_flags
================================================================================

Summary
	Reset the current actor’s motion state to a standing, stopped state 
	and flag a redraw.

Arguments
	actor                       Current actor index (global selects X)

Global Inputs
	actor                       Actor index selector
	actor_render_flags[*]    	Read to preserve existing bits before OR

Global Outputs
	actor_motion_state[*]       Set to MOTION_STOPPED_CODE
	actor_render_flags[*]    	OR with ACTOR_RENDER_VISIBLE_AND_REFRESH to request redraw

Description
	- Select standing limbs for the current actor.
	- Set motion state to stopped.
	- Mark animation state for refresh and request a redraw via OR flags.
	- No position or box state is modified.

Notes
	- Clobbers: A, X.
	- A ends holding the updated actor_render_flags[X] value.
	- Processor flags reflect the final ORA.
================================================================================
*/
* = $2C23
reset_actor_render_flags:
		// ------------------------------------------------------------
		// Select standing pose for current actor
		// ------------------------------------------------------------
		ldx     actor
		jsr     apply_standing_clip


		// ------------------------------------------------------------
		// Set motion state to stopped
		// ------------------------------------------------------------
		lda     #MOTION_STOPPED_CODE
		sta     actor_motion_state,x

		// ------------------------------------------------------------
		// Mark animation refresh and request redraw
		// ------------------------------------------------------------
		lda     actor_render_flags,x
		ora     #ACTOR_RENDER_VISIBLE_AND_REFRESH
		sta     actor_render_flags,x
		rts
/*
================================================================================
traverse_horizontally
================================================================================

Summary
    Move the actor one pixel along X, attempt to resolve the new walkbox, apply
    a foreground-masking gate, and if unresolved nudge Y toward the waypoint and
    retry. On success continue motion; on failure reset animation.

Arguments
     actor                      current actor index (zeropage)

Vars/State
     actor_x_pos,x              actor X position (pixels)
     actor_dir_x_bit,x          horiz dir bit (0=left, ≠0=right)
     actor_dir_y_bit,x          vert  dir bit (0=up,   ≠0=down)
     actor_active_wpt_y,x       target waypoint Y (pixels)
     position_to_waypoint_y_for_actor,x  working Y for box tests
     actor_box_fg_masking,x     FG masking state for actor
     actor_path_update_flag,x   set when walkbox change occurred

Global Outputs
     actor_x_pos,x
     position_to_waypoint_y_for_actor,x
     actor_box_fg_masking,x

Returns
    .A    unspecified (propagated from called routines)
    Flow  either falls through to RTS on success or tail-jumps to
          set_state_to_moving / reset_actor_render_flags.

Description
    - Snapshot path state for safe probing.
    - Step X by ±1 based on actor_dir_x_bit.
    - Call update_actor_walkbox_state, then apply FG-masking gate:
        • If actor_box_fg_masking == FG_MASKING_CODE:
            · If actor_path_update_flag == 0 → force unresolved (A:=BOX_UNRESOLVED_CODE).
            · Else clear FG masking and proceed.
    - If resolved → restore snapshot and continue moving.
    - If unresolved:
        • Restore snapshot.
        • If already aligned on Y with waypoint → reset animation.
        • Else nudge working Y one pixel toward waypoint and retry resolution.
            · If resolved → return.
            · If unresolved → restore snapshot and reset animation.

Notes
    - Snapshot/restore prevents committing tentative coordinates when resolution
      fails.
    - The FG-masking gate defers traversal until a path update occurs, then
      clears the gate.
================================================================================
*/
* = $2C36
traverse_horizontally:
		// ------------------------------------------------------------
		// Load actor index and snapshot path state for safe probing
		// ------------------------------------------------------------
		ldx     actor
		jsr     snapshot_actor_path_state


        // ------------------------------------------------------------
        // Move actor horizontally one pixel based on direction bit
        //   actor_dir_x_bit,x = 0 → move left (DEC)
        //   actor_dir_x_bit,x ≠ 0 → move right (INC)
        // ------------------------------------------------------------
        lda     actor_dir_x_bit,x              // test horizontal direction
        bne     step_right_in_box              // if set → moving right

        dec     actor_x_pos,x                  // move one pixel left
        jmp     resolve_box_after_horizontal_move

step_right_in_box:
        inc     actor_x_pos,x                  // move one pixel right

resolve_box_after_horizontal_move:
		// ------------------------------------------------------------
		// Try to resolve the current box after the horizontal step
		// A = #BOX_UNRESOLVED_CODE if unresolved
		// ------------------------------------------------------------
		jsr     update_actor_walkbox_state

        // ------------------------------------------------------------
        // Foreground masking gate logic
		//
        //   If actor_box_fg_masking == FG_MASKING_CODE:
        //       - If actor_path_update_flag == 0 → force unresolved (A := $FF)
        //       - Else clear masking flag and continue
        //   Otherwise skip masking gate
        // ------------------------------------------------------------
        ldy     actor_box_fg_masking,x          // Y := current FG masking state
        cpy     #FG_MASKING_CODE                // equal → actor is masked by FG layer
        bne     handle_fg_masking               // not equal → skip masking logic

        lda     actor_path_update_flag,x        // check if path update occurred
        beq     force_box_unresolved            // no update → force unresolved path
        lda     #FG_MASKING_CLEAR               // clear FG masking
        sta     actor_box_fg_masking,x
        jmp     handle_fg_masking               // resume with updated state

		// ------------------------------------------------------------
		// Apply FG-masking gate outcome and branch
		//   - Optionally force unresolved (A := BOX_UNRESOLVED_CODE)
		//   - If unresolved → handle recovery path
		//   - If resolved → restore snapshot and continue moving
		// ------------------------------------------------------------
force_box_unresolved:
        lda     #BOX_UNRESOLVED_CODE           // force unresolved state (A := $FF)

handle_fg_masking:
        cmp     #BOX_UNRESOLVED_CODE           // check if box resolution failed
        beq     handle_unresolved_box          // unresolved → handle recovery path

        // Resolved case: restore snapshot and continue actor movement
        jsr     restore_actor_path_snapshot
        jmp     set_state_to_moving

handle_unresolved_box:
        // ------------------------------------------------------------
        // Horizontal step failed: rollback and attempt a vertical nudge
        // ------------------------------------------------------------
        jsr     restore_actor_path_snapshot      // undo tentative move

        // If vertically aligned with waypoint, stop and refresh animation
        lda     actor_active_wpt_y,x             // target Y
        cmp     position_to_waypoint_y_for_actor,x
        bne     check_vertical_alignment         // not aligned → choose nudge dir
        jmp     reset_actor_render_flags      // aligned → stop/refresh

		// ------------------------------------------------------------
		// Choose vertical nudge direction toward waypoint
		//   - Use actor_dir_y_bit to pick up vs down
		//   - Adjust position_to_waypoint_y_for_actor by ±1
		//   - Then retry resolution
		// ------------------------------------------------------------
check_vertical_alignment:
        lda     actor_dir_y_bit,x                  // test vertical direction
        bne     step_down_in_box                   // ≠0 → nudge downward

        dec     position_to_waypoint_y_for_actor,x // nudge one pixel up toward waypoint
        jmp     recheck_box_after_vertical_move

step_down_in_box:
        inc     position_to_waypoint_y_for_actor,x // nudge one pixel down toward waypoint

recheck_box_after_vertical_move:
        // ------------------------------------------------------------
        // Retry resolution after Y nudge
		//
        //   - Snapshot adjusted state
        //   - Probe walkbox again with new Y
        // ------------------------------------------------------------
        jsr     snapshot_actor_path_state        // preserve nudged coordinates
        jsr     update_actor_walkbox_state       // attempt resolution post-nudge

        // Box resolved? If yes, finish; else rollback and stop animation
        cmp     #BOX_UNRESOLVED_CODE
        bne     dummy_fht       // resolved → done
        jsr     restore_actor_path_snapshot       // unresolved → restore prior state
        jmp     reset_actor_render_flags       // stop/refresh animation

		//Redundant section of code - replicated from original
		// (This was an evident copy/paste of the section above)
dummy_fht:
        cmp     #BOX_UNRESOLVED_CODE
        bne     finish_horizontal_traversal       // resolved → done
        jsr     restore_actor_path_snapshot       // unresolved → restore prior state
        jmp     reset_actor_render_flags       // stop/refresh animation
		

finish_horizontal_traversal:
        rts
/*
================================================================================
traverse_vertically
================================================================================

Summary
    Advance the actor one pixel vertically according to direction, attempt to
    resolve the current walkbox, and if unresolved nudge horizontally toward the
    waypoint and retry. On success resume motion; on failure reset animation.

Arguments
     actor                      current actor index (zeropage)

Vars/State
     actor_y_pos,x              actor Y position (pixels)
     actor_dir_y_bit,x          vertical dir bit (0=up, ≠0=down)
     actor_dir_x_bit,x          horizontal dir bit (0=left, ≠0=right)
     actor_active_wpt_x,x       target waypoint X
     position_to_waypoint_x_for_actor,x  working X used for path tests

Description
    - Snapshot current path state.
    - Move 1 px vertically: DEC if up, INC if down.
    - Call update_actor_walkbox_state:
        • If resolved → restore snapshot and set state to moving.
        • If unresolved → restore snapshot, check horizontal alignment to waypoint:
            · If aligned → reset animation state.
            · If not aligned → nudge working X one px toward waypoint, snapshot,
              retry box resolution:
                · If resolved → return.
                · If still unresolved → restore snapshot and reset animation.

Notes
    - Snapshot/restore brackets tentative coordinate edits to avoid committing
      moves when walkbox resolution fails.
    - Horizontal nudge disambiguates edge/overlap cases after a vertical step.
	
================================================================================
*/
* = $2CA3
traverse_vertically:
        // ------------------------------------------------------------
        // Load actor index and snapshot current path state
		//
        //   Preserve position/waypoint fields before coordinate edits
        // ------------------------------------------------------------
        ldx     actor                          // X := actor index
        jsr     snapshot_actor_path_state      // save path/waypoint state for possible rollback


        // ------------------------------------------------------------
        // Move actor vertically one pixel according to direction bit
		//
        //   actor_dir_y_bit,x = 0 → move up (DEC)
        //   actor_dir_y_bit,x ≠ 0 → move down (INC)
        // ------------------------------------------------------------
        lda     actor_dir_y_bit,x              // test vertical direction
        bne     step_down_in_box_vert          // if set → moving downward

        dec     actor_y_pos,x                  // move one pixel upward
        jmp     resolve_box_after_vstep

step_down_in_box_vert:
        inc     actor_y_pos,x                  // move one pixel downward

resolve_box_after_vstep:
		// ------------------------------------------------------------
		// Attempt to resolve current box; A = #BOX_UNRESOLVED_CODE if unresolved
		// ------------------------------------------------------------
		jsr     update_actor_walkbox_state
		cmp     #BOX_UNRESOLVED_CODE
		beq     handle_unresolved_box_vert

		//Redundant code - replicated from original
		cmp     #BOX_UNRESOLVED_CODE
		beq     handle_unresolved_box_vert
		
		// Box resolved → restore state and continue moving
		jsr     restore_actor_path_snapshot
		jmp     set_state_to_moving

handle_unresolved_box_vert:
        // ------------------------------------------------------------
        // Restore snapshot and test horizontal alignment to waypoint
		//
        //   If aligned on X → stop animation (cannot resolve box here)
        //   Else nudge X toward waypoint and retry
        // ------------------------------------------------------------
		jsr     restore_actor_path_snapshot
		lda     actor_active_wpt_x,x
		cmp     position_to_waypoint_x_for_actor,x
		bne     check_horizontal_dir
		jmp     reset_actor_render_flags

check_horizontal_dir:
        lda     actor_dir_x_bit,x                  // test horizontal direction
        bne     nudge_right_toward_waypoint        // ≠0 → nudge right

        dec     position_to_waypoint_x_for_actor,x // nudge one pixel left
        jmp     recheck_box_after_horizontal_nudge

nudge_right_toward_waypoint:
        inc     position_to_waypoint_x_for_actor,x // nudge one pixel right

recheck_box_after_horizontal_nudge:
        // ------------------------------------------------------------
        // Snapshot state and retry box resolution after X nudge
        // ------------------------------------------------------------
        jsr     snapshot_actor_path_state        // preserve adjusted X for retry
        jsr     update_actor_walkbox_state       // attempt resolution with nudged X

        // Box resolved? If yes, finish; else restore and stop animation
        cmp     #BOX_UNRESOLVED_CODE
        bne     finish_vertical_traversal         // resolved → done
        jsr     restore_actor_path_snapshot       // unresolved → rollback
        jmp     reset_actor_render_flags       // stop animation

finish_vertical_traversal:
        rts
/*
================================================================================
  update_actor_walkbox_state
================================================================================

Summary
    Determine the walkbox currently occupied by the actor and update related
    state (attributes, handlers, or path recalculation flags). Performs a
    quick containment test on the cached box, then scans all walkboxes if
    necessary.

Arguments
     actor                      current actor index (in zero page)

Vars/State
     actor_box_cur,x            cached current box index
     actor_box_prev,x           previous box index (for stability)
     actor_box_attr,x           attribute/depth of current box
     actor_box_handler_idx,x    handler index controlling behavior
     actor_path_update_flag,x   flag to trigger path recomputation
     scan_box_idx               current box being tested
     selected_box_attr          latched prior attribute
     selected_handler_idx       latched prior handler index
     last_box_vedge             cached vertical edge value
     box_ptr                    pointer to walkbox table base

Global Outputs
     actor_box_cur,x
     actor_box_attr,x
     actor_path_update_flag,x

Returns
    .A = BOX_RESULT_UNCHANGED         $00 → actor still in same box
    .A = BOX_RESULT_CHANGED           $01 → box or handler state changed
    .A = BOX_RESULT_UNDETERMINED      $FF → geometry invalid or aborted scan

Description
    - Attempt a fast-path containment test using actor_box_cur.
    - If failed, scan each walkbox sequentially:
        • Test whether the actor’s position lies inside each box.
        • On hit, read its attribute:
            · If $FF (sentinel) → exit loop.
            · If index matches actor_box_prev → select immediately.
            · Else continue scanning for stability.
        • On miss, cache vertical-edge byte for potential abort test.
    - On selection:
        • Write actor_box_cur and actor_box_attr.
        • If bit7 of attribute set → invoke select_actor_box_handler.
        • Else if previous handler index nonzero → clear walkbox attributes.
    - Final .A reports result per constants above.

Notes
    - Stability rule favors remaining in actor_box_prev when overlap exists.
    - last_box_vedge guards against corrupted geometry ($FF → abort).
    - The loop uses .A as a control flag: $FF=continue, $00=break.

================================================================================

 This routine decides which walkbox the actor is currently in and whether any
 box-driven behavior must run. It tries a cheap check first, then falls back to
 a full scan if needed. It also encodes a stability rule to avoid flicker when
two boxes overlap.

 Fast path
	* Use the cached box (actor_box_cur). Compute its table offset and test if the
	  actor’s point lies inside it. If yes, return unchanged (A=$00).

 Scan setup
	* scan_box_idx starts at $FF so the first INC targets box 0.
	* selected_box_attr and selected_handler_idx latch the actor’s prior values for
	  later comparison.
	* actor_box_attr is cleared to $00. last_box_vedge is reset to $00.

 Per-box probe
	* For each index, compute Y to the box entry and test containment.
	* Miss: advance Y to the vertical-edge field and copy it into last_box_vedge.
	  Continue scanning.
	* Hit: advance Y to the attribute byte and read it.
		* If attribute == $FF, current code breaks the scan immediately (A=$00).
		* Else apply the stability rule: if this index equals actor_box_prev, select
		it now; otherwise continue scanning to try to land on the previous box.

 Loop control via A
	* A=$FF means “continue scanning” (BNE branches).
	* A=$00 means “break” (fall through to finalize and attribute handling).

 Finalize on break
	* Copy attribute at (box_ptr),Y into actor_box_attr.
	* If attribute != $FF, write actor_box_cur to the selected index.

 Post-selection effects
	* If bit7 of actor_box_attr is set and the attribute != $FF, call
	  select_actor_box_handler and return changed (A=$01).
	* Else if selected_handler_idx != 0, clear per-actor walkbox attrs and return
	  changed (A=$01).
	* Else return unchanged (A=$00).

 Guard and undetermined exit
	* At the top of each iteration, if last_box_vedge == $FF, return with
	  BOX_RESULT_UNDETERMINED (A=$FF). This avoids looping when geometry appears
	  invalid.

 Why this shape
	* The fast path handles the common case in constant time.
	* The stability rule prefers the previous box when overlaps exist, which keeps
	  motion consistent across boundaries.
	* The A-register loop protocol keeps the hot path branchy but compact: no extra
	  flags or counters are needed to tell the loop to continue or break.
	  
================================================================================

 Pseudo-code:

	if not room_walkboxes_loaded(): return A=$00
	if point_in_box(actor_box_cur): return A=$00

	init_scan()
	for i in boxes:
	  if not point_in_box(i):
		cache_vertical_edge(i)
		continue
	  attr = read_attr(i)
	  if attr == $FF: break    ; current code: stop here
	  if i == actor_box_prev:  ; stability
		choose(i); break
	  else:
		continue

	finalize:
	copy_attr()
	if attr != $FF: actor_box_cur = i

	if attr.bit7 && attr != $FF: select_handler(); return A=$01
	if selected_handler_idx != 0: clear_attrs(); return A=$01
	return A=$00
================================================================================
*/
* = $2CF4
update_actor_walkbox_state:
		// ------------------------------------------------------------
		// Load and validate active walkbox table
		//
		//   - Select current actor index
		//   - Fetch walkboxes for its active costume
		//   - Abort if resource not resident in memory
		// ------------------------------------------------------------
        ldx     actor                           // X := current actor index (select actor context)
        jsr     get_walkboxes_for_costume       // Load active room’s walkbox table for this actor
        cmp     #WALKBOX_NOT_RESIDENT           // A == WALKBOX_NOT_RESIDENT → room resource not loaded in memory
        bne     check_existing_box              // If valid, proceed with box containment test
        rts                                     // Otherwise exit: no walkboxes available

check_existing_box:
        // ------------------------------------------------------------
        // Fast path: test whether the cached box still contains the actor
		//
        // X=actor
		// actor_box_cur,x=cached index
        // is_actor_pos_inside_box → A=$00 hit; Y advanced on hit
		//							 A=$FF miss
        // ------------------------------------------------------------
        lda     actor_box_cur,x              // A := cached box index
        jsr     get_walkbox_offset           // Y := offset to this box’s entry
        jsr     is_actor_pos_inside_box      // inside? A=$00; outside? A=$FF
        bne     begin_box_scan               // outside → fall back to full scan
        rts                                  // inside → unchanged (A=$00)

begin_box_scan:
        // ------------------------------------------------------------
        // Initialize scan state for full walkbox search
		//
        //   scan_box_idx := $FF (pre-increment to 0 on first iteration)
        //   Latch prior attribute/handler to compare against after selection
        // ------------------------------------------------------------
        lda     #INIT_SCAN_IDX               // A := $FF → pre-increment start
        sta     scan_box_idx                 // scan_box_idx := pre-increment start

        lda     actor_box_attr,x             // read actor’s last applied attribute
        sta     selected_box_attr            // snapshot for later comparison

        lda     actor_box_handler_idx,x      // read actor’s last handler index
        sta     selected_handler_idx         // snapshot for post-scan decisions

		// ------------------------------------------------------------
		// Clear temporary state before scanning
		//
		//   - Reset walkbox offset scratch register (Y)
		//   - Clear actor’s current attribute/depth placeholder
		//   - Reset cached vertical edge marker
		// ------------------------------------------------------------
        ldy     #$00                      	// reset walkbox-offset scratch (Y := 0)
        lda     #$00
        sta     actor_box_attr,x          	// clear computed attribute/depth until a box is chosen
        sta     last_box_vedge            	// reset cached vertical edge to EDGE_UNSET

try_next_box:
		// ------------------------------------------------------------
		// Advance scan index and validate cached vertical edge
		//
		//   - Increment scan_box_idx to probe next box
		//   - Abort early if last_box_vedge == EDGE_INVALID
		// ------------------------------------------------------------		
        inc     scan_box_idx

        lda     last_box_vedge              // test cached V-edge from last miss
        cmp     #EDGE_INVALID               // EDGE_INVALID → cannot resolve a box
        bne     test_current_box            // valid → probe this box

        lda     #BOX_RESULT_UNDETERMINED    // report: undetermined
        rts
		jmp		dummy_tcb 

test_current_box:
        // ------------------------------------------------------------
        // Probe candidate box for containment
		//
        //   - Compute entry offset for scan_box_idx
        //   - Test actor position against box
        //   - On miss: cache vertical edge and continue
		//
        // is_actor_pos_inside_box → A=$00 hit (Y advanced), A=$FF miss
        // ------------------------------------------------------------
        lda     scan_box_idx
        jsr     get_walkbox_offset          // Y := offset for scan_box_idx
        jsr     is_actor_pos_inside_box     // test actor position against this box
dummy_tcb:		
        bne     cache_vertical_edge         // miss → cache V-edge and continue

		// ------------------------------------------------------------
		// Handle inside-hit case
		//
		//   - Actor is inside this box
		//   - Advance Y to the attribute byte (entry+3)
		//   - Read and validate attribute
		// ------------------------------------------------------------
        iny                                   // Y -> attribute byte
        lda     (box_ptr),y                   // A := attribute at this box
        cmp     #ATTR_INVALID                 // ATTR_INVALID = sentinel/end → treat as invalid
        bne     process_box_attribute         // valid attribute → process

        // Attribute is invalid/sentinel
		// Set A to BREAK_SCAN to break the loop
        lda     #BREAK_SCAN
        jmp     loop_control

process_box_attribute:
		// ------------------------------------------------------------
		// Process attribute and apply stability rule
		//
		//   - If scan_box_idx == actor_box_prev → select immediately
		//   - Else set A = CONTINUE_SCAN and keep scanning via loop_control
		// ------------------------------------------------------------
        lda     scan_box_idx                 // A := candidate box index
        cmp     actor_box_prev,x             // Z=1 if same as previous box (prefer stability)
        beq     select_current_box           // same as previous → commit immediately

        lda     #CONTINUE_SCAN              // not the previous box → continue scanning
        jmp     loop_control                // fall through common loop control path

select_current_box:
		// ------------------------------------------------------------
		// Commit chosen box as current
		//
		//   - Set actor_path_update_flag to trigger path recompute
		//   - Store scan_box_idx as new actor_box_cur
		//   - Set A = BREAK_SCAN to exit loop via loop_control
		// ------------------------------------------------------------
        lda     #PATH_UPDATE_REQUIRED                          // mark path update needed
        sta     actor_path_update_flag,x

        lda     scan_box_idx                  // A := chosen box index
        sta     actor_box_cur,x               // commit as current box for this actor

        lda     #BREAK_SCAN                   // A := $00 to exit scan loop (must be $00)
        jmp     loop_control                  // fall through common loop terminator

cache_vertical_edge:
        // ------------------------------------------------------------
        // Miss path: advance to vertical-edge byte and cache it
		//
        //   Y was at entry base; Y+2 → V-edge field
        // ------------------------------------------------------------
        iny
        iny
        lda     (box_ptr),y                 // A := vertical edge value
        sta     last_box_vedge              // cache for next-iteration abort check

        lda     #CONTINUE_SCAN               // A := $FF → continue main scan loop

loop_control:
		// ------------------------------------------------------------
		// Loop control and finalize selection
		//
		//   - If A != $00 → continue scanning
		//   - On break: copy attribute from selected box
		//   - If ATTR_INVALID skip box_cur write; else set actor_box_cur
		// ------------------------------------------------------------
        bne     try_next_box              // A != $00 → keep scanning (continue)

        // Chosen box path: copy attribute (logic/depth) from table
        lda     (box_ptr),y               // A := attribute of selected box
        sta     actor_box_attr,x          // commit attribute for this actor
        cmp     #ATTR_INVALID             // $FF = sentinel → skip box_cur write
        beq     eval_attr_effects

        lda     scan_box_idx              // A := selected box index
        sta     actor_box_cur,x           // set current box for this actor

eval_attr_effects:
		// ------------------------------------------------------------
		// Evaluate selected box attribute for handler updates
		//
		//   - If bit7 clear → skip handler path
		//   - If attr == ATTR_INVALID with bit7 set → exit unchanged
		//   - Else call select_actor_box_handler and report BOX_RESULT_CHANGED
		// ------------------------------------------------------------
        lda     actor_box_attr,x              // A := selected box attribute
        bpl     check_handler_update          // bit7=0 → no special semantics
        cmp     #ATTR_INVALID                 // $FF sentinel with bit7=1?
        beq     finalize_attr_update         // yes → skip updates and exit unchanged

        // Bit7 set and attr != $FF → apply box-specific handler update
        jsr     select_actor_box_handler      // invoke handler for this attribute
        ldx     actor                         // restore actor index after JSR
        lda     #BOX_RESULT_CHANGED           // report: state changed
finalize_attr_update:
        jmp     exit_update_box_for_actor     // unified exit

check_handler_update:
        // ------------------------------------------------------------
        // Handler-based update path
		//
        //   If a previous handler index was latched (nonzero), clear current
        //   walkbox attrs to force a refresh and report “changed”.
        // ------------------------------------------------------------
        lda     selected_handler_idx          // A := prior handler index
        beq     exit_update_box_for_actor     // zero → nothing to do → unchanged

        jsr     clear_actor_walkbox_attrs     // reset per-actor walkbox attributes
        ldx     actor                         // restore X=actor after JSR
        lda     #BOX_RESULT_CHANGED           // indicate state change

exit_update_box_for_actor:
        rts                                    // return with A already set
/*
================================================================================
  select_actor_box_handler
================================================================================

Summary
	Selects and applies the per-actor box-attribute handler based on the
	incoming attribute code. If the handler index is unchanged, refreshes
	the committed attribute/depth and returns. On change, clears per-actor
	planes, updates the handler index while preserving depth bits, patches the
	JSR target via a handler table, and invokes it.

Arguments
	A                       	attribute code
	X                       	actor index

Global Inputs
	actor_box_handler_idx[X]   	current handler index for this actor
	actor_box_attr[X]       	current attribute byte (low 2 bits = depth)
	selected_box_attr       	scratch for computed handler index
	box_attr_handler_lo[]   	handler target table (low bytes)
	box_attr_handler_hi[]   	handler target table (high bytes)

Global Outputs
	actor_box_handler_idx[X]   updated to new handler index on change
	actor_box_attr[X]       	masked to preserve depth (low 2 bits) on change
	selected_box_attr       	set to computed handler index
	box_attr_handler_jsr_op 	patched with handler address (lo/hi)
	(side effect)           	calls clear_actor_walkbox_attrs on handler change

Returns
	None

Description
	- Compute handler index: (A & BOX_CODE_INDEX_MASK) >> 2.
	- If index equals actor_box_handler_idx[X]:
		• Refresh actor_box_attr[X] from selected_box_attr and return.
	- Else:
		• Save index in selected_box_attr.
		• Clear per-actor walkbox attribute planes.
		• Set actor_box_handler_idx[X] := selected index.
		• Preserve only depth bits in actor_box_attr[X] using BOX_ATTR_DEPTH_MASK.
		• Patch JSR operand from handler tables using the new index.
		• Call the patched handler (self-modifying JSR).

Notes
	Uses self-modifying code to branch via a patched JSR operand. Depth
	(bits 0..1) is retained across handler switches; other attribute bits are
	reset. Index range after shift is 0..31 (but only 0-4 is actually implemented).
================================================================================
*/
* = $2DD7
select_actor_box_handler:
        // ------------------------------------------------------------
		// Derive handler index from attribute value
		// Keep bits 2–6 and shift right twice → index range 0–31  
        // ------------------------------------------------------------
		and     #BOX_CODE_INDEX_MASK         // isolate bits 2–6 of input code  
		lsr                                  // >>1  
		lsr                                  // >>2 → normalized handler index

        // ------------------------------------------------------------
		// If computed index == current handler index → same handler active  
		// Refresh actor_box_attr with current selection (updates depth) and exit  
        // ------------------------------------------------------------
		cmp     actor_box_handler_idx,x           // compare new index vs stored handler index  
		bne     update_box_handler_on_prog_change // mismatch → need handler switch  
		lda     selected_box_attr                 // reload current attribute selection  
		sta     actor_box_attr,x                  // refresh committed box attribute/depth  
		rts                                       // no handler change → done

update_box_handler_on_prog_change:
        // ------------------------------------------------------------
		// Handler change: cache new index and reset per-actor attribute planes
        // ------------------------------------------------------------
		sta     selected_box_attr              // save computed handler index
		jsr     clear_actor_walkbox_attrs      // reset attrs for this actor
		
		// ------------------------------------------------------------
		// Commit new handler index and preserve existing depth bits
		//
		// - Update actor_box_handler_idx[X] with selected handler index.
		// - Keep only the low 2 bits (depth) of actor_box_attr[X].
		// - Clear other bits to reset attribute state while preserving depth.
		// ------------------------------------------------------------
		lda     selected_box_attr              // X-prog index := new handler index
		sta     actor_box_handler_idx,x        // commit handler index for this actor
		
		lda     actor_box_attr,x               // fetch current attr/depth byte
		and     #BOX_ATTR_DEPTH_MASK           // preserve depth only (keep low 2 bits)
		sta     actor_box_attr,x               // write back depth, clear other bits

        // ------------------------------------------------------------
		// Patch JSR operand using handler table entry for this actor’s handler
        // ------------------------------------------------------------
		ldy     actor_box_handler_idx,x          // Y := handler index (0..31)
		tya                                      // A := Y
		tax                                      // X := handler index for table lookup
		lda     box_attr_handler_lo,x            // load handler low byte
		sta     box_attr_handler_jsr_op          // write JSR operand low
		lda     box_attr_handler_hi,x            // load handler high byte
		sta     box_attr_handler_jsr_op + 1      // write JSR operand high

        // ------------------------------------------------------------
		// Call through self-modified JSR operand (target set above)
        // ------------------------------------------------------------
		jsr     $0000

* = $2E0F
attr_update_return_stub:               			// common return stub for box-attr handlers
		rts                             		// tail return target patched handlers jump to

/*
================================================================================
  clear_actor_walkbox_attrs
================================================================================

Summary
	Zero three per-actor walkbox attribute slots for the actor in X. Clears the
	attributes at base+{0,4,8} for that actor.

Arguments
	X                       actor index

Global Inputs
	actor_box_handler_idx      pointer to attribute plane 0 (base of per-actor tables)

Global Outputs
	(via pointer walk)
	actor_box_handler_idx[actor]  := 0
	actor_box_facing_override[actor]  := 0
	actor_box_fg_masking[actor]  := 0
	box_attr_ptr            used as scratch pointer during iteration

Returns
	None

Description
	- Initialize box_attr_ptr from actor_box_handler_idx.
	- Use Y=actor for (ptr),Y addressing; reuse X as a 0..2 loop counter.
	- For each of three planes:
		• store 0 at (box_attr_ptr),Y for this actor
		• advance box_attr_ptr by +4 bytes (with page carry)
	- Restore X to the actor index and exit.
================================================================================
*/
* = $2E10
clear_actor_walkbox_attrs:
        // ------------------------------------------------------------
		// Initialize attribute pointer: box_attr_ptr := actor_box_handler_idx (base of attr table)
        // ------------------------------------------------------------
		lda     #<actor_box_handler_idx             
		sta     box_attr_ptr                   
		lda     #>actor_box_handler_idx             
		sta     box_attr_ptr + 1

        // ------------------------------------------------------------
		// Prepare loop registers  
		//
		// Y := actor index (copied from X for indirect addressing)  
		// X := loop counter (0..2) for three attribute clears  
        // ------------------------------------------------------------
		txa  
		tay  
		ldx     #$00

clear_next_attr:
        // ------------------------------------------------------------
		// Clear current attribute slot for this actor
        // ------------------------------------------------------------
		lda     #$00
		sta     (box_attr_ptr),y

        // ------------------------------------------------------------
		// Advance to next attribute plane (+4 bytes)
        // ------------------------------------------------------------
		lda     box_attr_ptr                // lo += 4
		clc
		adc     #$04
		sta     box_attr_ptr
		bcc     advance_to_next_attr_plane         // no page cross
		inc     box_attr_ptr + 1                // carry → bump hi byte

advance_to_next_attr_plane:
        // ------------------------------------------------------------
		// Loop: run three clears at offsets +0,+4,+8 (X = 0,1,2)
        // ------------------------------------------------------------
		inx                                     
		cpx     #$02                            
		bcc     clear_next_attr                 
		beq     clear_next_attr                 

        // ------------------------------------------------------------
		// Epilogue: restore X := actor index and return
        // ------------------------------------------------------------
		tya                                     // A := actor index copy
		tax                                     // X := actor index
		rts                                     // done
/*
================================================================================
  set_facing_override_to_up
================================================================================

Summary
	Force the actor’s box-facing override to “up”. This override is applied 
	exclusively during ladder traversal, ensuring the actor always faces toward 
	the ladder while climbing.

Arguments
	X                       actor index (loaded from actor)

Global Outputs
	actor_box_facing_override[X]   set to FACING_OVERRIDE_UP (override = up)

Description
	Loads the current actor index into X, writes the facing-override code $C1
	to that actor’s override slot, and returns.

Notes
	FACING_OVERRIDE_UP is the encoded value for “face up” in the box-facing override scheme.
================================================================================
*/
* = $2E35
set_facing_override_to_up:
        ldx     actor
        lda     #FACING_OVERRIDE_UP
        sta     actor_box_facing_override,x
        rts
/*
================================================================================
 clear_box_handler_idx
================================================================================
Summary
	Reset the per-actor box handler/program index to zero.

Global Inputs
	actor                          current actor index

Global Outputs
	actor_box_handler_idx[]        entry for actor set to $00

Description
	- Load X from actor.
	- Write $00 to actor_box_handler_idx[X].
	- Return.
================================================================================
*/
* = $2E3D
clear_box_handler_idx:
        ldx     actor
        lda     #$00
        sta     actor_box_handler_idx,x
        rts
/*
================================================================================
  clear_box_handler_idx_b
================================================================================

Summary
	Identical in function to clear_box_prog_idx: clears the actor’s box handler
	index by writing $00 into actor_box_handler_idx[X]. Exists as a duplicate
	entry point with a distinct label.
================================================================================
*/
* = $2E45
clear_box_handler_idx_b:
        ldx     actor
        lda     #$00
        sta     actor_box_handler_idx,x
        rts
/*
================================================================================
  set_box_masked_by_fg
================================================================================
Summary
	Enable foreground masking for the current actor. Optionally enter a blocking
	debug wait that maps I/O in, colors the screen border, and resumes when dbg_gate≠0.

Arguments
	None

Global Inputs
	actor                          current actor index
	dbg_gate                       bit7=1 → skip wait; ≠0 releases wait

Global Outputs
	actor_box_fg_masking[X]        set to FG_OCCLUDED

Returns
	None

Description
	- X := actor; actor_box_fg_masking[X] := FG_OCCLUDED.
	- If dbg_gate.bit7=1 → skip debug section and return.
	- Else: dbg_gate := $00; cpu_port := MAP_IO_IN.
	Loop: set vic_border_color_reg := BORDER_WAIT_FG_MASK_COLOR; poll dbg_gate until ≠0.
	After release: cpu_port := MAP_IO_OUT; return.

Notes
	Blocking loop by design; intended as an interactive breakpoint.
	I/O mapping changes are temporary and restored before exit.
================================================================================
*/
* = $2E4D
set_box_masked_by_fg:
        // ------------------------------------------------------------
		// Enable foreground masking for this actor
		// Mark actor_box_fg_masking[X] := 3 → actor rendered behind FG layer
        // ------------------------------------------------------------
		ldx     actor                         // X := actor index
		lda     #FG_OCCLUDED              	  // mask flag value (FG-occluded)
		sta     actor_box_fg_masking,x        // set FG masking state for actor

        // ------------------------------------------------------------
		// Check debug gate: if bit7 set, skip interactive debug wait  
        // ------------------------------------------------------------
		lda     dbg_gate                      // load debug control byte  
		bmi     debug_exit                    // bit7=1 → bypass wait section

        // ------------------------------------------------------------
		// Reset debug gate to 0 before entering wait loop  
        // ------------------------------------------------------------
		lda     #$00  
		sta     dbg_gate  

        // ------------------------------------------------------------
		// Map I/O into memory space ($01 := $25) so hardware registers are visible  
        // ------------------------------------------------------------
		ldy     #MAP_IO_IN  
		sty     cpu_port

debug_wait_loop:
		//Set border color
		lda     #BORDER_WAIT_FG_MASK_COLOR
		sta     vic_border_color_reg              // visual cue while paused

        // ------------------------------------------------------------
		// Spin until external agent sets dbg_gate != 0
        // ------------------------------------------------------------
		lda     dbg_gate                      // poll gate
		beq     debug_wait_loop               // 0 → keep waiting

        // ------------------------------------------------------------
		// Map I/O out of memory space ($01 := $24) after debug wait
        // ------------------------------------------------------------
		ldy     #MAP_IO_OUT
		sty     cpu_port

debug_exit:
		rts                                     // exit; FG masking already applied

// 0 → $2E0F  attr_update_return_stub
// 1 → $2E35  set_facing_override_to_up
// 2 → $2E3D  clear_box_handler_idx
// 3 → $2E45  clear_box_handler_idx_b
// 4 → $2E4D  set_box_masked_by_fg
* = $2e6e
box_attr_handler_lo:
        .byte   <attr_update_return_stub
		.byte	<set_facing_override_to_up
		.byte	<clear_box_handler_idx
		.byte	<clear_box_handler_idx_b
		.byte	<set_box_masked_by_fg

* = $2e73
box_attr_handler_hi:
        .byte   >attr_update_return_stub
		.byte	>set_facing_override_to_up
		.byte	>clear_box_handler_idx
		.byte	>clear_box_handler_idx_b
		.byte	>set_box_masked_by_fg

/*
================================================================================
  step_actor_x_dda
================================================================================

Summary
	One-pixel X stepping using DDA. Adds |ΔX| into an accumulator; when the
	sum reaches the dominant magnitude, emits a ±1 X move, then validates the
	tentative position against walkboxes and returns a status.

Arguments
	X                       actor index

Global Inputs
	actor_dx_abs[X]         |ΔX| to active waypoint
	actor_dda_dom_abs[X]    dominant magnitude max(|ΔX|,|ΔY|)
	actor_dir_x_bit[X]      bit0: 0=left, 1=right
	position_to_waypoint_x_for_actor[X]
	actor_active_wpt_x[X]

Global Outputs
	actor_dx_accum[X]                       updated X accumulator
	position_to_waypoint_x_for_actor[X]     tentative X after step
	(via snapshot/restore)
		actor_x_pos[]
		actor_y_pos[]
		actor_box_cur[]
		actor_box_attr[]
		actor_path_update_flag[]

Returns
	A = #$00    XAXIS_RESULT_CONTINUE        not at waypoint and no box change
	A = #$01    XAXIS_RESULT_REACHED         X reached waypoint
	A = #$04    XAXIS_RESULT_WBOX_CHANGED    box unresolved after tentative move

Description
	- Accumulate: accumX += |ΔX|; if accumX < dominant → store and continue.
	- Otherwise step one pixel on X:
		• dir_x_bit=0 → X := X−1 (left)
		• dir_x_bit=1 → X := X+1 (right)
	Then accumX := accumX − dominant and store.
	- Snapshot path state → run walkbox resolution:
		• If unresolved (#$FF) → restore snapshot, return WBOX_CHANGED.
	- Compare tentative X to active waypoint X:
		• Equal → REACHED; else CONTINUE.

Notes
	Uses waypoint-relative X for stepping; world state is confirmed by the
	walkbox update. At most one pixel is emitted per call.

================================================================================

Moves the actor horizontally toward its waypoint using precise DDA stepping.

- Each frame adds the X delta (|ΔX|) to a running accumulator that tracks
  fractional progress between pixels. When the total equals or exceeds the
  dominant motion magnitude (the larger of |ΔX| and |ΔY|), the actor moves
  exactly one pixel left or right and keeps the leftover fraction as residual.

- After each tentative move, the routine snapshots the actor’s position and
  calls update_actor_walkbox_state to verify that the new position still lies
  within a valid walkbox. If it doesn’t, the previous state is restored and the
  function reports a “walkbox changed” result.

- If the new X coordinate matches the waypoint’s X, the actor has reached its
  horizontal target and the routine returns “reached.” Otherwise, it returns
  “continue” so the motion can proceed next frame.

In short:
    • Handles per-pixel horizontal stepping with sub-pixel precision.
    • Prevents invalid movement across walkbox boundaries.
    • Returns one of three possible results:
          XAXIS_RESULT_CONTINUE  → continue stepping
          XAXIS_RESULT_REACHED  → reached waypoint
          XAXIS_RESULT_WBOX_CHANGED  → walkbox changed or invalid position
================================================================================
*/
* = $2E8C
step_actor_x_dda:
        // ------------------------------------------------------------
		// Accumulate X progression: add |ΔX| to current accumulator (A = accumX + |ΔX|)
        // ------------------------------------------------------------
		ldx     actor                        // X := actor index
		lda     actor_dx_accum,x             // load current X accumulator
		clc                                   
		adc     actor_dx_abs,x               // add absolute X delta toward waypoint

        // ------------------------------------------------------------
		// If accumX < dominant(|ΔX|,|ΔY|) → no pixel step; commit accumulator
        // ------------------------------------------------------------
		cmp     actor_dda_dom_abs,x           // compare accumulated vs dominant magnitude
		bcc     commit_x_accumulator          // branch if below threshold

        // ------------------------------------------------------------
		// accumX ≥ dominant → emit one X pixel based on dir_x_bit (0=left, 1=right)
        // ------------------------------------------------------------
		ldy     actor_dir_x_bit,x            // Y := direction flag
		bne     step_right_1px               // Y!=0 → right path (INC); else fall through to left (DEC)

		// ------------------------------------------------------------
		// Apply one-pixel X movement based on direction
		//
		// - dir_x_bit = 0 → step left (DEC)
		// - dir_x_bit = 1 → step right (INC)
		//
		// The update affects only the waypoint X copy; world position is
		// validated later through walkbox resolution.
		// ------------------------------------------------------------
		dec     position_to_waypoint_x_for_actor,x   // step 1 px left (X−=1)
		jmp     adjust_accumulated_after_motion_x     // join residual update

step_right_1px:
		inc     position_to_waypoint_x_for_actor,x   // step 1 px right (X+=1)

adjust_accumulated_after_motion_x:
		// ------------------------------------------------------------
		// Reduce accumulator by dominant for 1-px emission
		//
		// A := accumulated − dominant; keep fractional residual
		// ------------------------------------------------------------
		sec                                     // prepare for subtraction
		sbc     actor_dda_dom_abs,x             // A := accumulated − dominant

commit_x_accumulator:
		sta     actor_dx_accum,x                // commit updated X accumulator (residual)

		// ------------------------------------------------------------
		// Snapshot and validate tentative walkbox on X
		//
		// Save path state, recompute current box after tentative X,
		// and roll back on unresolved result.
		// ------------------------------------------------------------		
		jsr     snapshot_actor_path_state       // save pos/box/path for rollback
		jsr     update_actor_walkbox_state    // recompute box after tentative X
		cmp     #BOX_RESULT_UNDETERMINED        // unresolved (#$FF)? Z=0 → BNE taken
		bne     waypoint_x_compare              // resolved → continue to waypoint test		
		jsr     restore_actor_path_snapshot     // rollback on unresolved box result
		lda     #XAXIS_RESULT_WBOX_CHANGED      // status: walkbox changed/unresolved on X
		rts                                     // return with failure status in A

		// ------------------------------------------------------------
		// Waypoint X compare and status selection
		//
		// - If tentative X == waypoint X → return REACHED
		// - Else → return CONTINUE
		// ------------------------------------------------------------
waypoint_x_compare:
		lda     position_to_waypoint_x_for_actor,x   // A := tentative X after possible 1-px step
		cmp     actor_active_wpt_x,x                 // reached? Z=1 if tentative X == waypoint X
		bne     continue_x_traversal                 // not equal → keep traversing X

		lda     #XAXIS_RESULT_REACHED            // A := status → waypoint X reached
		rts                                      // return reached

continue_x_traversal:
		lda     #XAXIS_RESULT_CONTINUE           // A := status → keep moving toward waypoint
		rts                                      // return continue status
/*
================================================================================
  step_actor_y_dda
================================================================================
Summary
	One-pixel Y stepping using DDA. Applies ±1 Y when the Y accumulator reaches
	the dominant magnitude, then validates the tentative position against
	walkboxes and returns a status code.

Arguments
	X = actor                          current actor index

Global Inputs
	actor_dy_abs[X]                    |ΔY| to active waypoint
	actor_dda_dom_abs[X]               dominant magnitude max(|ΔX|,|ΔY|)
	actor_dir_y_bit[X]                 bit0: 0=up, 1=down
	position_to_waypoint_y_for_actor[X]
	actor_active_wpt_y[X]

Global Outputs
	actor_dy_accum[X]                  	 updated Y accumulator
	position_to_waypoint_y_for_actor[X]  tentative Y after step
	(via snapshot/restore) 
		actor_x_pos[]
		actor_y_pos[]
		actor_box_cur[]
		actor_box_attr[]
		actor_path_update_flag[]

Returns
	A = #$00  	YAXIS_RESULT_CONTINUE    	not at waypoint and no box change
	A = #$01  	YAXIS_RESULT_REACHED     	Y reached waypoint
	A = #$04  	YAXIS_RESULT_WBOX_CHANGED  	box unresolved or vertical-only backout

Description
	- Accumulate: accumY += |ΔY|; if accumY < dominant → store and continue.
	- Otherwise step one pixel on Y:
		• dir_y_bit=0 → Y := Y−1 (up)
		• dir_y_bit=1 → Y := Y+1 (down)
	Then accumY := accumY − dominant.
	- Snapshot path state → run walkbox resolution:
		• If unresolved (#$FF) → restore snapshot, return YAXIS_RESULT_WBOX_CHANGED.
	- If |ΔY| == #$FF (sentinel for vertical-only backout) → restore, return YAXIS_RESULT_WBOX_CHANGED.
	- If position_to_waypoint_y == active_wpt_y → return YAXIS_RESULT_REACHED; else YAXIS_RESULT_CONTINUE.

Notes
	Y grows downward on screen; “step_down” uses INC on the waypoint Y copy.
================================================================================

Performs one-pixel Y movement for an actor using DDA stepping.

- Each call accumulates |ΔY| toward the dominant magnitude (max(|ΔX|,|ΔY|)).
  When the accumulated value reaches or exceeds the dominant magnitude, the
  actor moves by ±1 pixel on Y (up or down depending on dir_y_bit) and the
  residual is stored back into the accumulator.

- After any tentative move, the routine snapshots actor path state and calls
  update_actor_walkbox_state to validate the new position against walkboxes.
  If the position cannot be resolved (returns BOX_RESULT_UNDETERMINED), it
  restores the snapshot and returns YAXIS_RESULT_WBOX_CHANGED.

- A special sentinel (|ΔY| == $FF) triggers a vertical-only backout, restoring
  the previous state and returning YAXIS_RESULT_WBOX_CHANGED.

- Finally, the updated Y position is compared against the active waypoint Y:
      equal   	→ YAXIS_RESULT_REACHED
      not equal → YAXIS_RESULT_CONTINUE

The routine ensures per-pixel vertical motion with sub-pixel precision,
while preventing illegal walkbox transitions and signaling the result status.
================================================================================
*/
* = $2ECD
step_actor_y_dda:
        // ------------------------------------------------------------
		// Accumulate Y progression: add |ΔY| to current accumulator (A = accumY + |ΔY|)
        // ------------------------------------------------------------
		lda     actor_dy_accum,x            // load current Y accumulator
		clc                                 
		adc     actor_dy_abs,x              // add absolute Y delta toward waypoint

        // ------------------------------------------------------------
		// If accumY < dominant(|ΔX|,|ΔY|) → no pixel step; just update accumulator and exit  
        // ------------------------------------------------------------
		cmp     actor_dda_dom_abs,x          // compare accumulated Y against dominant magnitude  
		bcc     commit_y_accumulator         // branch if below → insufficient to trigger 1-px step

        // ------------------------------------------------------------
		// accumY ≥ dominant → emit one Y pixel based on dir_y_bit (0=up, 1=down)
        // ------------------------------------------------------------
		ldy     actor_dir_y_bit,x            // Y := dir flag; Z=1 if up(0), Z=0 if down(1)
		bne     step_down                    // Y!=0 → down path (INC); else fall through to up path (DEC)

		dec     position_to_waypoint_y_for_actor,x   // step 1 px up (Y−=1; screen Y increases downward)
		jmp     adjust_accumulated_after_motion       // skip down-path; join to accumulator reduction

step_down:
		inc     position_to_waypoint_y_for_actor,x   // step 1 px down (Y+=1; screen Y increases downward)

adjust_accumulated_after_motion:
        // ------------------------------------------------------------
		// Residual = accumulated − dominant; A still holds accumulated
        // ------------------------------------------------------------
		sec                                     
		sbc     actor_dda_dom_abs,x             // A := accumulated − dominant

commit_y_accumulator:
		sta     actor_dy_accum,x                // commit updated Y accumulator (residual)

		// ------------------------------------------------------------
		// Snapshot and validate tentative walkbox after Y step
		//
		// Save pos/box/path state, recompute current box for the
		//         tentative Y, and roll back on unresolved (#$FF) result.
		// ------------------------------------------------------------
		jsr     snapshot_actor_path_state       // save pos/box/path flags for rollback
		jsr     update_actor_walkbox_state    // recompute box after tentative Y change
		cmp     #BOX_RESULT_UNDETERMINED        // unresolved (#$FF) → must back out
		bne     validate_y_depth                // resolved → continue checks		
		jsr     restore_actor_path_snapshot     // rollback pos/box/path state after failed box resolve
		lda     #YAXIS_RESULT_WBOX_CHANGED      // A := status → walkbox changed/unresolved on Y
		rts                                     // return with failure status kept in A


		// ------------------------------------------------------------
		// Validate Y sentinel and gate to waypoint test
		//
		// - If |ΔY|==0: no backout requested → skip to waypoint compare
		// - If |ΔY|==$FF: vertical-only backout → restore snapshot and return
		// - Else: proceed to waypoint compare
		// ------------------------------------------------------------
validate_y_depth:
		lda     actor_dy_abs,x                 	// fetch |ΔY| for sentinel check
		beq     check_waypoint_y_reached       	// |ΔY|==0 → normal flow, skip backout path
		cmp     #$ff                           	// test vertical-only backout sentinel
		bne     check_waypoint_y_reached       	// |ΔY|≠$FF → proceed to waypoint test (no backout)
		jsr     restore_actor_path_snapshot     // revert tentative pos/box after vertical-only backout
		lda     #YAXIS_RESULT_WBOX_CHANGED      // A := status → Y-axis backout requested (|ΔY| == $FF)
		rts                                     // return with backout status

		// ------------------------------------------------------------
		// Waypoint Y compare and status selection
		//
		// - If tentative Y == waypoint Y → return REACHED
		// - Else → return CONTINUE
		// ------------------------------------------------------------		
check_waypoint_y_reached:
		lda     position_to_waypoint_y_for_actor,x   // A := tentative Y after possible 1-px step
		cmp     actor_active_wpt_y,x                 // reached? Z=1 if tentative Y == waypoint Y
		bne     continue_y_traversal                 // not equal → keep traversing Y
		
		lda     #YAXIS_RESULT_REACHED            // A := status → waypoint Y reached
		rts                                      // return reached

continue_y_traversal:
		lda     #YAXIS_RESULT_CONTINUE           // A := status → keep stepping toward waypoint
		rts                                      // return continue status
/*
================================================================================
  snapshot_actor_path_state
================================================================================
Summary
	Create a reversible checkpoint of the actor’s path-related state, then
	swap live X/Y to the “position→waypoint” copies for speculative path tests.

Arguments
	None

Global Inputs
	actor                           current actor index
	actor_x/y_pos[]					live screen-space position
	position_to_waypoint_x/y_for_actor[]
	actor_box_attr[]                current walkbox attribute tag
	actor_box_cur[]                 current walkbox index
	actor_path_update_flag[]        path change flag

Global Outputs
	saved_actor_x_pos               snapshot: X position
	saved_actor_y_pos               snapshot: Y position
	saved_actor_box_attr            snapshot: walkbox attribute tag
	saved_actor_box_cur             snapshot: walkbox index
	saved_actor_path_update_flag    snapshot: path change flag
	actor_x/y_pos[]					overwritten with waypoint-copy coordinates

Returns
	None

Description
	- Save live X/Y so tentative movement can be reverted.
	- Save the current walkbox attribute tag for exact context restoration.
	- Overwrite live X/Y with the waypoint-copy coordinates used by DDA and
	box-resolution probes.
	- Save current walkbox id and the path-update flag to preserve traversal
	semantics across speculative checks.

Notes
	Pair with the complementary restore routine to roll back if box resolution
	fails or traversal is rejected.
================================================================================
*/
* = $2F1B
snapshot_actor_path_state:
        ldx     actor                        // X := current actor index

        // Checkpoint live position 
        lda     actor_x_pos,x                // save X position
        sta     saved_actor_x_pos
        lda     actor_y_pos,x                // save Y position
        sta     saved_actor_y_pos

        // Save the actor’s current walkbox attribute (depth/behavior tag)
        lda     actor_box_attr,x
        sta     saved_actor_box_attr

        // Replace live actor position with the waypoint copy used for path testing.
        lda     position_to_waypoint_x_for_actor,x   // load temporary X
        sta     actor_x_pos,x                        // overwrite live X position
        lda     position_to_waypoint_y_for_actor,x   // load temporary Y
        sta     actor_y_pos,x                        // overwrite live Y position

        // Save active walkbox index and path-update flag.
        lda     actor_box_cur,x                 // current walkbox index
        sta     saved_actor_box_cur
        lda     actor_path_update_flag,x        // path update flag (set if box changed)
        sta     saved_actor_path_update_flag

        rts                                     
/*
================================================================================
  restore_actor_path_snapshot
================================================================================
Summary
	Restore the actor’s previously snapshotted path and box state after a failed
	traversal or test. Reverts all temporary coordinate and flag changes made
	since snapshot_actor_path_state.

Arguments
	None

Global Inputs
	actor                                current actor index
	saved_actor_x_pos                    saved X coordinate
	saved_actor_y_pos                    saved Y coordinate
	saved_actor_box_attr                 saved walkbox attribute tag
	saved_actor_box_cur                  saved walkbox index
	saved_actor_path_update_flag         saved “box changed” flag

Global Outputs
	actor_x/y_pos[]                      			 restored live coordinates
	position_to_waypoint_x/y_for_actor[]			 synchronized waypoint copies
	actor_box_attr[]                                 restored walkbox attribute
	actor_box_cur[]                                  restored walkbox index
	actor_path_update_flag[]                         restored “box changed” flag

Returns
	None

Description
	- Reloads saved X/Y and mirrors them into waypoint copies to keep position
	and DDA reference coordinates consistent.
	- Restores the actor’s previous walkbox attribute (depth/behavior tag).
	- Restores the current walkbox index and path-update flag to reestablish
	the same traversal context that existed before the snapshot.
	- Used in conjunction with snapshot_actor_path_state to safely undo
	speculative movement or walkbox transitions.
================================================================================
*/
* = $2F48
restore_actor_path_snapshot:
        ldx     actor                        // X := current actor index

        // Restore X coordinate and mirror into the waypoint-working copy
        lda     saved_actor_x_pos
        sta     actor_x_pos,x
        sta     position_to_waypoint_x_for_actor,x

        // Restore Y coordinate and mirror into the waypoint-working copy
        lda     saved_actor_y_pos
        sta     actor_y_pos,x
        sta     position_to_waypoint_y_for_actor,x

        // Restore the actor’s saved walkbox attribute tag (depth/behavior byte)
        lda     saved_actor_box_attr
        sta     actor_box_attr,x

        // Restore the actor’s active walkbox index
        lda     saved_actor_box_cur
        sta     actor_box_cur,x

        // Restore the saved path-update flag
        lda     saved_actor_path_update_flag
        sta     actor_path_update_flag,x

        rts                                     
/*
================================================================================
  init_dda_for_path
================================================================================

Summary
	Compute per-actor path deltas and stepping parameters between the actor’s
	current “position→waypoint” copy and the active destination waypoint. Produce
	absolute |ΔX|/|ΔY|, direction bits, dominant axis and magnitude, and seed the
	DDA accumulators.

Arguments
	actor_active_wpt_x[X]
	actor_active_wpt_y[X]
	position_to_waypoint_x_for_actor[X]
	position_to_waypoint_y_for_actor[X]

Global Inputs
	actor                                  current actor index in X (ZP or global)

Global Outputs
	actor_dir_x_bit[X]                     bit0: 0=left, 1=right
	actor_dir_y_bit[X]                     bit0: 0=up,   1=down
	actor_dda_dom_abs[X]                   max(|ΔX|, |ΔY|)
	actor_dx_abs[X], actor_dy_abs[X]       |ΔX|, |ΔY|
	actor_dx_accum[X], actor_dy_accum[X]   DDA accumulators (seeded)

Returns
	A  		#$00  movement needed (at least one delta nonzero)
			#$01  no movement needed (both deltas zero)

Description
	* Initialize direction bits and dominant axis to horizontal defaults.	
	* For X:
	  * If path ≥ pos: 	A := (path − pos); set dir_x_bit=1 (right) via carry→ROL.
	  * Else:        	A := (pos − path); dir_x_bit remains 0 (left).
	  * Store as |ΔX|.
	* Repeat for Y with down/up semantics into dir_y_bit and |ΔY|.	
	* If both |ΔX| and |ΔY| are zero, return PATH_RET_NO_MOVE immediately.
	* Choose dominant axis and magnitude: dom_abs := max(|ΔX|, |ΔY|).
	* Seed DDA accumulators with |ΔX| and |ΔY|.
	* Publish direction bits, dom_abs, |ΔX|/|ΔY|, and accumulators to per-actor
	  state, then return PATH_RET_MOVE_NEEDED.
================================================================================
*/
* = $2F6F
init_dda_for_path:
        // ------------------------------------------------------------
        // Seed directions and dominant axis: dirs=0 (X:Left, Y:Up), dom=Horizontal
        // ------------------------------------------------------------
        ldy     #$00                
        sty     dir_x_bit                  // X dir bit := 0 (Left)
        sty     dir_y_bit                  // Y dir bit := 0 (Up)
        sty     dda_dominant_axis          // Dominant axis := Horizontal

        // ------------------------------------------------------------
        // Compare actor’s current X coordinate against the target waypoint X.
		//
        // If actor_active_wpt_x < position_to_waypoint_x → actor is to the right of the waypoint,
        // so we compute (pos − path) for a leftward delta; otherwise (path − pos) for rightward.
        // ------------------------------------------------------------
        ldx     actor
        lda     actor_active_wpt_x,x           		// Load target X
        cmp     position_to_waypoint_x_for_actor,x 	// Compare to current position X
        bcc     dx_from_pos_minus_path         		// Branch if path < pos → moving left

        // ------------------------------------------------------------
        // path >= pos: 
		//
		//		A := (path − pos); set dir_x_bit=1 (right) via carry from SBC
        // ------------------------------------------------------------
        sbc     position_to_waypoint_x_for_actor,x   // CMP left C=1 → subtract pos
        rol     dir_x_bit                            // carry→bit0 => 1 (right)
        jmp     commit_dx_abs

dx_from_pos_minus_path:
        // ------------------------------------------------------------
        // path < pos: 
		//
		//		A := (pos − path); keep dir_x_bit=0 (left)
        // ------------------------------------------------------------
        lda     position_to_waypoint_x_for_actor,x   	// load current X (pos)
        sec                                         	// ensure borrow logic correct
        sbc     actor_active_wpt_x,x                 	// subtract target X (path)

commit_dx_abs:
        // ------------------------------------------------------------
        // Commit |ΔX| (absolute horizontal distance) for subsequent DDA setup
        // ------------------------------------------------------------
        sta     dda_dx_abs

        // ------------------------------------------------------------
        // Repeat the same comparison for Y coordinates.
		//
        // If path < pos → moving upward; otherwise downward.
        // ------------------------------------------------------------
        lda     actor_active_wpt_y,x              	// Load target Y
        cmp     position_to_waypoint_y_for_actor,x 	// Compare to current position Y
        bcc     dy_from_pos_minus_path            	// Branch if path < pos → move up

        // ------------------------------------------------------------
        // path ≥ pos: 
		//
		//		A := (path − pos); set dir_y_bit=1 (down) via carry from SBC
        // ------------------------------------------------------------
        sbc     position_to_waypoint_y_for_actor,x   // subtract current Y
        rol     dir_y_bit                            // carry→bit0 => 1 (down)
        jmp     commit_dy_abs

dy_from_pos_minus_path:
        // ------------------------------------------------------------
        // path < pos: 
		//
		//		A := (pos − path); keep dir_y_bit=0 (up)
        // ------------------------------------------------------------
        lda     position_to_waypoint_y_for_actor,x   // load current Y (pos)
        sec                                          // enable borrow for subtract
        sbc     actor_active_wpt_y,x                 // subtract target Y (path)

commit_dy_abs:
        // ------------------------------------------------------------
        // Commit |ΔY| (absolute vertical distance) for DDA calculations
        // ------------------------------------------------------------
        sta     dda_dy_abs

        // ------------------------------------------------------------
        // Test if both horizontal and vertical deltas are zero.
		//
        // If either axis has nonzero delta, continue to dominant-axis selection.
        // ------------------------------------------------------------
        lda     dda_dx_abs                     // check |ΔX|
        bne     select_dominant_axis           // nonzero X → motion present
        ldy     dda_dy_abs                     // check |ΔY|
        bne     select_dominant_axis           // nonzero Y → motion present

        // ------------------------------------------------------------
        // Both |ΔX| and |ΔY| are zero → actor already at waypoint
        // ------------------------------------------------------------
        lda     #PATH_RET_NO_MOVE              // return “no movement needed”
        rts

select_dominant_axis:
        // ------------------------------------------------------------
        // Choose dominant axis: if |ΔX| >= |ΔY| keep Horizontal; else set Vertical
        // ------------------------------------------------------------
        lda     dda_dx_abs                   // A := |ΔX|
        cmp     dda_dy_abs                   // compare |ΔX| vs |ΔY|
        bcs     select_dominant_value        // |ΔX| >= |ΔY| → stay Horizontal
        lda     #DOM_AXIS_V                  // otherwise, Vertical is dominant
        sta     dda_dominant_axis

select_dominant_value:
        // ------------------------------------------------------------
        // Select dominant magnitude: dom := max(|ΔX|, |ΔY|)
        // ------------------------------------------------------------
        lda     dda_dx_abs                   // A := |ΔX|
        cmp     dda_dy_abs                   // compare to |ΔY|
        bcs     store_dominant_value         // if |ΔX| >= |ΔY| keep A
        lda     dda_dy_abs                   // else A := |ΔY|

store_dominant_value:
        sta     dda_dominant_abs             // commit dominant magnitude

        // ------------------------------------------------------------
        // Seed DDA accumulators with absolute deltas (first-step thresholds)
        // ------------------------------------------------------------
        lda     dda_dx_abs                   // X accumulator := |ΔX|
        sta     dda_x_accum_seed
        lda     dda_dy_abs                   // Y accumulator := |ΔY|
        sta     dda_y_accum_seed

        // ------------------------------------------------------------
        // Publish direction bits to per-actor state
        // ------------------------------------------------------------
        ldx     actor                        // X := actor index
        lda     dir_x_bit                    // bit0: 0=left, 1=right
        sta     actor_dir_x_bit,x
        lda     dir_y_bit                    // bit0: 0=up,   1=down
        sta     actor_dir_y_bit,x

        // ------------------------------------------------------------
        // Commit dominant DDA magnitude (max of |ΔX| and |ΔY|) to per-actor state
        // ------------------------------------------------------------
        lda     dda_dominant_abs
        sta     actor_dda_dom_abs,x

        // ------------------------------------------------------------
        // Store absolute deltas for each axis into actor state
        // ------------------------------------------------------------
        lda     dda_dy_abs                   // |ΔY| → actor’s vertical delta
        sta     actor_dy_abs,x
        lda     dda_dx_abs                   // |ΔX| → actor’s horizontal delta
        sta     actor_dx_abs,x

        // ------------------------------------------------------------
        // Initialize actor’s DDA accumulators with starting seed values
        // ------------------------------------------------------------
        lda     dda_x_accum_seed              // seed X accumulator with |ΔX|
        sta     actor_dx_accum,x
        lda     dda_y_accum_seed              // seed Y accumulator with |ΔY|
        sta     actor_dy_accum,x

        // ------------------------------------------------------------
        // Return #$00 → indicates at least one nonzero delta (movement required)
        // ------------------------------------------------------------
        lda     #PATH_RET_MOVE_NEEDED
        rts
