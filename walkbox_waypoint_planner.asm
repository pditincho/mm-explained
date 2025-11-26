/*
================================================================================
Waypoint pathing and corner-routing overview
================================================================================

The waypoint logic ensures that an actor walks naturally between “walkbox” zones
without cutting through walls or getting stuck on edges.

1. Finding where the actor can go
   * When the actor gets a new target, the code checks which walkbox
     contains that spot.
   * If the spot is outside all boxes, it finds the nearest box and snaps
     the target there.
   * If no valid box exists, the operation stops.

2. Building a path
   * The engine keeps a list of connected boxes from a prior search.
   * The last box in that list becomes the *target box*.
   * The actor’s current box is treated as the *source box*.
   * Each box has four edges: left, right, top, and bottom.

3. Figuring out their relationship
   * The code compares the two boxes and generates a coordinate relationship result:
     REL_ABOVE, REL_BELOW, REL_LEFT, REL_RIGHT, or REL_OVERLAP.
   * This defines whether the target box is above, below, left, right, or
     overlapping the current box.

4. Checking if the move is straight or needs a turn
   * Depending on how the boxes line up, the actor might walk straight or need
     to turn a corner.
   * It checks if the actor’s coordinate (X or Y, depending on direction) is
     inside the target box’s range.
   * If it’s inside, it can move straight; otherwise, a corner turn is required.

5. Building a “narrow hallway”
   * When boxes don’t align perfectly, the shared overlap acts as a hallway.
   * The code calculates that by taking:
     • The higher of the two top/left edges.
     • The lower of the two bottom/right edges.
   * If the target isn’t strictly inside this hallway, a corner is needed.

6. Checking along the movement direction
   * It then checks the other coordinate to ensure the target won’t cross
     the current box edge too early.
   * This ensures the actor stays inside valid areas during movement.

7. Picking the right waypoint
   * If the path is clear, the waypoint is simply the target.
   * If not, it picks a *corner point* where the actor should turn before
     continuing toward the goal.
     • Moving up/down → adjust Y, possibly keep X.
     • Moving left/right → adjust X, possibly keep Y.

8. Result
   * Valid waypoint → stored in the actor’s table.
   * Failed computation (overlap/invalid) → use target directly.

In short:
	* Snap the target to the map.
	* See how the boxes connect.
	* If a straight line is safe, go straight.
	* If not, pick the best corner.
	* Store that point as the waypoint so the actor walks smoothly and stays within
	  valid walkboxes.

================================================================================

Key algorithms and techniques
    • Walkbox-backed spatial graph:
        - Navigation runs over a graph whose nodes are walkboxes (AABBs) and
          whose edges come from a separate walkbox adjacency search.
        - This file assumes the search layer has already populated
          actor_discovered_boxes with a valid path.

    • AABB relation classification:
        - classify_box_relation performs oriented AABB separation tests to
          decide if the current box is above, below, left, right, or overlapping
          the target box.
        - The result packs axis and “near-origin” info into REL_* codes that
          later drive axis selection and edge choice.

    • Orthogonal-axis actor containment:
        - classify_actor_axis_vs_box tests the actor against the target box on
          the axis orthogonal to the relation (vertical relation → test X,
          horizontal relation → test Y).
        - The actor is categorized as OUTSIDE_LOW, OUTSIDE_HIGH, or INSIDE
          relative to the target’s span on that axis.

    • “Narrowest hall” corridor intersection:
        - check_path_requires_corner_adjustment fuses current and target box
          edges on the orthogonal axis to create the tightest shared corridor:
              hall_lower_bound = max(first edges)
              hall_upper_bound = min(second edges)
        - The target must lie strictly inside this corridor:
              hall_lower_bound < dest < hall_upper_bound
          or a corner is required.

    • Along-relation edge-crossing checks:
        - After passing the hall test, the routine inspects the target on
          the relation axis (the direction of motion) against the current box
          edge nearest the origin.
        - Depending on whether the current box is nearer or farther from the
          origin, it enforces:
              dest_other < current_edge   or   dest_other ≥ current_edge
          to ensure the straight path does not cross the current box boundary
          before reaching the shared corridor.

    • Corner waypoint generation:
        - If any test fails, compute_waypoint_from_path constructs a corner
          waypoint at a valid box junction:
            - For vertical relations (above/below), it fixes Y from the target
              box’s top/bottom and chooses X from either the actor or the
              target box’s left/right edge.
            - For horizontal relations (left/right), it fixes X from the target
              box’s left/right and chooses Y from either the actor or the
              target box’s top/bottom edge.
        - This guarantees that the waypoint itself lies on an intersection
          compatible with the walkbox layout.

================================================================================

Update flow
    1) stage_actor_path_to_target
        - Snap the requested target into the walkbox system.
        - Optionally rebuild the box path (via find_path_between_walkboxes).
        - Mark actor_path_update_flag and snapshot actor_box_cur.

    2) apply_pending_waypoint_update
        - If actor_path_update_flag is set, clear it and call
          compute_waypoint_from_path.
        - On failure/overlap, fall back to using (actor_target_x, actor_target_y)
          directly as the waypoint.

    3) compute_waypoint_from_path
        - Ensure walkbox geometry is resident.
        - Step actor_search_depth one level toward the origin and fetch the
          corresponding box id from actor_discovered_boxes.
        - Rebuild current_box_ptr/target_box_ptr for this step.
        - If boxes overlap: mark motion about to stop, invalidate depth, and
          signal “no computed path”.
        - Otherwise classify relation, test orthogonal containment and
          “narrowest hall” conditions, and either:
            • emit a straight-line waypoint at the snapped target, or
            • emit a corner waypoint computed from box edges and actor position.

    This module sits between the high-level “actor wants to move here” request
    and the low-level walkbox graph. It converts a precomputed box path plus
    actor positions into safe, O(1)-per-update waypoints that either follow the
    straight corridor between boxes or route via a corner when necessary, so
    actors never cut through walls or leave valid walkable regions.
	
================================================================================

 Box overlap

 To check for a vertical overlap between two boxes, we compare their top and bottom edges.
 The comparison determines whether the boxes share any vertical space on screen.

 Remember: on screen, Y increases as you move downward.
 Therefore, a higher position visually corresponds to a smaller Y value.

 ------------------------------------------------------------
 Scenario 1 — Vertical overlap
 ------------------------------------------------------------
 Condition:
     B_top < A_bottom
 Meaning:
     Box B starts higher on the screen than the bottom of box A,
     so their vertical ranges overlap.

 Diagram (Y increases ↓):

         |-------|  ← A_top
         |   A   |
         |       |       |-------| ← B_top
 A_bottom|-------|       |   B   |
                         |-------| ← B_bottom

 Result:
     Boxes A and B vertically overlap.


 ------------------------------------------------------------
 Scenario 2 — No vertical overlap
 ------------------------------------------------------------
 Condition:
     B_top > A_bottom
 Meaning:
     Box B begins *below* the bottom of box A,
     so there is an empty gap between them.

 Diagram (Y increases ↓):

 A_top   |-------|
         |   A   |
 A_bottom|-------|
                 |-------| ← B_top
                 |   B   |
                 |-------| ← B_bottom

 Result:
     Boxes A and B do not overlap vertically.
     B is relatively *below* A.


 ------------------------------------------------------------
 For horizontal comparisons:
 ------------------------------------------------------------
 Apply the same logic, but use the left/right edges instead of top/bottom.
 Example: if B_left < A_right → horizontal overlap.
 Otherwise, if B_left > A_right → B is to the right of A.
 
================================================================================
 
 Corner waypoints
 
 When determining whether the actor must “turn a corner,” the axis being tested
 depends on the relative position between the current and target boxes.

 If the relation is horizontal (left/right), the routine checks the vertical
 axis (top/bottom).
 
 If the relation is vertical (above/below), the routine checks the horizontal
 axis (left/right).

 This cross-axis test allows the engine to detect whether a straight-line path
 between the actor’s position and the target box would hit a wall, requiring a
 detour via a box corner.

 ------------------------------------------------------------
 Scenario 1 — Perfectly aligned boxes
 ------------------------------------------------------------
 Both boxes are side by side and vertically aligned.  
 The actor can move horizontally with no obstruction.

 Diagram (horizontal relation → test vertical axis):

        |--------|=========|
        |   X    | current |		X: target
        | target |    A    |		A: actor
        |--------|=========|

 Result:
     The actor’s vertical coordinate lies fully inside the overlap range.
     No corner turn needed — the actor can move straight left or right.


 ------------------------------------------------------------
 Scenario 2 — Mismatched box heights
 ------------------------------------------------------------
 The current and target boxes do not share identical vertical spans.
 The actor may need to “turn a corner” if the target row lies
 outside the vertical overlap between both boxes.

 Diagram (Y increases ↓):

 Row 0   |--------|---------|
 Row 1   | target | current |
 Row 2   |        |    A    | ← actor inside current box
 Row 3   |        |---------|
 Row 4   |--------|          ← target box extends lower

 If the target is near Row 4, the actor cannot move left in a straight line,
 since the current box ends at Row 3. It must first travel down to the corner
 before entering the target box.

 Path visualization:

 Row 0   |--------|---------|
 Row 1   | target | current |
 Row 2   |        |         |
 Row 3   |     ...C....A----| ← C = corner where path turns
 Row 4   |---X..--|          ← X = target

 A = actor’s position  
 X = target point  
 C = corner point (path intersection)  
 . = trajectory

 Result:
     The actor’s Y coordinate is below the target box’s bottom edge.
     The movement requires hitting the corner C before continuing
     into the target box.

 Another example:

 			|--------|          	
 			| X      |          	
 			| target |---------|	
 			|--------| current |	
				     |    A    |	
				     |---------|	
		 	          				
	Here, the actor’s vertical position is lower than the bottom edge of the target box.
	As a result, a direct path would intersect the box boundary, so the actor must first 
	reach the current box’s top-left corner before continuing into the target box.


 ------------------------------------------------------------
 Scenario 3 — Vertical relation
 ------------------------------------------------------------
 For vertical relations (target above or below), the logic is inverted:
 the routine checks the horizontal axis instead.

 Example: target box lies below current box,
 but the actor’s X position is outside the overlap horizontally.
 The routine detects that a straight-down move would cross a wall
 and sets the corner waypoint instead.

 -----------------------------------------------------------------------------
 "Narrowest hall"
 -----------------------------------------------------------------------------
 
 The scene may include overlapping boxes forming a narrow passage between
 them. The figure below illustrates one example:

     Origin
     0----------------------> +X axis
     |        current   target
     |        |--------|
     |        |        |---------| <--- farthest top edge
     |        |   A    |    X    |
     |        |        |---------| <--- closest bottom edge
     |        |--------|
     V
     +Y axis

     A = actor’s current position
     X = target

 In this situation, the target box constrains the available corridor
 vertically. To find the most restrictive (“narrowest”) passage,
 choose:
     - the bottom edge nearest to the origin, and
     - the top edge farthest from the origin.

 This defines the limiting region used to test for a clear trajectory.

================================================================================
*/

#importonce
#import "globals.inc"
#import "constants.inc"
#import "actor_motion_constants.inc"
#import "walkbox_dfs.asm"
#import "walkbox_snap.asm"
#import "actor_targeting.asm"


.label target_box_ptr           = $19  // zp ptr → target  walkbox edge table {left,right,top,bottom}
.label hall_lower_bound         = $1E04  // tight corridor lower bound on orthogonal axis (max of first edges)
.label hall_upper_bound         = $1E05  // tight corridor upper bound on orthogonal axis (min of second edges)
.label saved_ofs_y              = $1E8F  // scratch: Y-offset into discovered_boxes_tbl for this actor+depth
.label prev_box_dbg             = $CAD4  // debug: penultimate discovered box id (for inspection/logging)
.label rel_axis_coord           = $CAD5  // scratch: actor coordinate on the axis being evaluated (X or Y)
.label box_relpos_code          = $CAD6  // REL_* code from classify_box_relation (bit1=axis, bit0=near-origin)
.label tgt_box_idx              = $FC3B  // walkbox index selected for target at final BFS depth
.label actor_axis_class         = $FC3B  // axis test vs target: $00 below/left, $01 above/right, $02 inside
.label dest_other_axis          = $FC3F  // temp: target coordinate on the axis opposite to the hall axis

.const DISCOVERED_LIST_STRIDE    = $10    // bytes reserved per actor in discovered_boxes_tbl
.const REL_BELOW                 = $00    // box relation: current below target
.const REL_ABOVE                 = $01    // box relation: current above target
.const REL_RIGHT                 = $02    // box relation: current to the right of target
.const REL_LEFT                  = $03    // box relation: current to the left of target
.const REL_OVERLAP_TOUCH         = $FF    // boxes overlap or share an edge
.const REL_AXIS_HORIZONTAL_BIT   = $02    // bit1 set   → horizontal comparison (left/right)
.const REL_NEAR_ORIGIN_BIT       = $01    // bit0 set   → current box nearer origin (up/left)
.const AXIS_OUTSIDE_LOW          = $00    // actor coordinate below lower edge → corner down/right
.const AXIS_OUTSIDE_HIGH         = $01    // actor coordinate above upper edge → corner up/left
.const AXIS_INSIDE               = $02    // actor coordinate inside target range
.const PATH_ADJUST_REQUIRED      = $FF    // path recalculation needed (return code)
.const PATH_NO_ADJUST            = $00    // straight path valid (return code)
.const NO_COMPUTED_PATH  		 = $FF

/*
================================================================================
  stage_actor_path_to_target
================================================================================

Summary
	Update an actor’s navigation target: snap the requested target to the
	nearest walkbox, decide whether a path rebuild is needed, optionally build
	the path, and record bookkeeping flags and snapshots.

Arguments
	None

Vars/State
	actor                         current actor index (0..N), read
	nearest_box_idx               box index selected by snap step, read
	actor_box_cur             table [X] → actor’s current walkbox, read
	actor_target_box         table [X] → actor’s target walkbox, write
	actor_discovered_boxes        per-actor BFS scratch (stride = 16), write
	actor_path_update_flag        table [X] → path-needs-update flag, write
	actor_box_prev                table [X] → last-known box snapshot, write

Global Outputs
	actor_target_box[X]             updated to snapped target box (if any)
	actor_discovered_boxes[base+0]       cleared to NO_BOX_FOUND when skipping build
	actor_path_update_flag[X]            set to PATH_UPDATE_REQUIRED
	actor_box_prev[X]                    snapshot of actor_box_cur[X]

Returns
	None

Description

	* Calls snap routine to map the desired target to a valid walkbox.
	* If no box is found, exits early and leaves state unchanged.
	* If snapped box equals current box:
	  • Skips path construction.
	  • Clears this actor’s discovered-box slot to NO_BOX_FOUND.
	* Otherwise:
	  • Invokes full path build from current to target box.
	* In all non-early-exit cases:
	  • Sets PATH_UPDATE_REQUIRED for the actor.
	  • Snapshots actor_box_cur into actor_box_prev.

Notes
	* Calls: snap_coords_to_walkbox, find_path_between_walkboxes.
	* Clobbers A, X, Y; flags per last writes (no carry/overflow guarantees).
	* Assumes actor holds a valid index before invocation.
================================================================================
*/
* = $1C4A
stage_actor_path_to_target:
        // Snap target to nearest walkbox
        jsr     snap_actor_target
        ldx     actor
		
		// No box found? If so, exit
        lda     nearest_box_idx
        cmp     #NO_BOX_FOUND
        bne     save_target_box
        rts

save_target_box:
        // Publish target box for this actor
        sta     actor_target_box,x

        // Current and target boxes are equal? If so, skip path building.
        cmp     actor_box_cur,x
        beq     clear_path_buffer

        // Boxes are different, build full path
        jsr     find_path_between_walkboxes
        ldx     actor
        jmp     mark_path_update_and_snapshot

clear_path_buffer:
        // Boxes are equal
        // Clear actor’s discovered-box list (stride = 16 bytes).
        txa
        asl
        asl
        asl
        asl
        clc
        adc     #DISCOVERED_LIST_STRIDE
        tay
        lda     #NO_BOX_FOUND
        sta     actor_discovered_boxes,y

mark_path_update_and_snapshot:
        // Flag path update
        lda     #TRUE
        sta     actor_path_update_flag,x
		
        // Save current box as "prev" box
        lda     actor_box_cur,x
        sta     actor_box_prev,x
        rts
/*
================================================================================
  apply_pending_waypoint_update
================================================================================

Summary
	If a waypoint update is pending for the actor, rebuild the next waypoint
	via walkboxes; otherwise return immediately. On overlap/fail, fall back to
	the raw target coordinates.

Arguments
	X  actor index

Global Inputs
	actor                          current actor index (mirrored in X)
	actor_path_update_flag[]       nonzero → waypoint must be recomputed
	actor_x/y_dest[]			   actor’s target target

Global Outputs
	actor_tgt_waypoint_x/y[]       waypoint X/Y written on success or fallback
	actor_path_update_flag[]       cleared on entry when work is performed

Returns
	None

Description

	* Read actor_path_update_flag[X]; if zero, no work.
	* Clear the flag and call compute_waypoint_from_path.
	* If compute_waypoint_from_path returned $FF (overlap/no path), copy
	  (actor_target_x, actor_target_y) to (actor_tgt_waypoint_x, actor_tgt_waypoint_y).
	* Otherwise the callee has already written the waypoint.

Notes

	* Clobbers A, X. Y is preserved.
	* compute_waypoint_from_path defines whether a corner-adjustment is needed; this
	  routine only orchestrates and applies a safe fallback on failure.
================================================================================
*/
* = $1C81
apply_pending_waypoint_update:
		// Is there a pending path update for this actor? If not, exit
		ldx     actor
		lda     actor_path_update_flag,x
		beq     return_done_ok
  
		// Clear path update flag
		lda     #FALSE
		sta     actor_path_update_flag,x
		
		// Build path through walkboxes
		jsr     compute_waypoint_from_path

		// Is there a computed path? If not, exit
		cmp     #NO_COMPUTED_PATH
		bne     return_done_ok

		// Publish the target coordinates as waypoint coordinates
		ldx     actor
		lda     actor_target_x,x
		sta     actor_tgt_waypoint_x,x
		lda     actor_target_y,x
		sta     actor_tgt_waypoint_y,x
  
return_done_ok:
		rts
/*
================================================================================
  compute_waypoint_from_path
================================================================================
Summary
	Build the next waypoint toward the actor’s snapped target using the
	walkbox path. If the current and target boxes overlap, stop early. Otherwise
	either keep the straight-line target or route via the nearest corner.

Arguments
	X  actor index

Global Inputs
	actor                          current actor index (mirrored in X)
	actor_search_depth[]           final BFS depth for actor (or $FF if none)
	actor_discovered_boxes[]       per-actor discovered boxes (stride = 16)
	actor_box_cur[]            actor’s current walkbox index
	actor_x/y_dest[]			   desired target in pixels
	actor_x/y_pos[]				   actor’s current position in pixels
	current_box_ptr                base pointer to walkbox table {L,R,T,B}

Global Outputs
	actor_tgt_waypoint_x/y[] 		waypoint X/Y emitted for this step
	actor_box_prev[]               snapshot of last box used
	actor_motion_state[]           set to 1 on overlap early-exit
	actor_search_depth[]           reset to $FF on overlap early-exit

Returns
	A = 	NO_COMPUTED_PATH  overlap early-exit, no waypoint change beyond bookkeeping
			$00  waypoint set (either direct target or corner-adjusted)

Description
	* Ensure walkboxes are loaded; if not resident, return.
	* Read BFS depth; if $FF, fail. Else use (depth-1) to pick the final box.
	* Compute discovered-list offset for this actor and fetch the box id.
	* Point target_box_ptr to that box; point current_box_ptr to actor’s box.
	* Compare boxes:
	  • If REL_OVERLAP_TOUCH: stop motion, reset depth, return $FF.
	  • Else cache relation and classify actor vs target on the orthogonal axis.
	* If no adjustment needed: copy target to waypoint.
	* If adjustment needed:
	  • For vertical relation: choose X from actor or target {left/right};
	  set Y from relation {top/bottom}.
	  • For horizontal relation: choose Y from actor or target {top/bottom};
	  set X from relation {left/right}.
	* Return $00.

Notes
	* Box edge order is {left, right, top, bottom}. Each walkbox record is 5 bytes.
	* Bit1 of RELPOS_* encodes axis (0=vertical, 1=horizontal).
	* Equality with edges counts as “inside range” during axis checks.
	* Clobbers A, X, Y; flags reflect last LDA immediate on return.
================================================================================
*/
* = $1CA3
compute_waypoint_from_path:
        // Resolve actor index for all subsequent table lookups
        ldx     actor

        // Clear DFS debug state
        lda     #$FF
        sta     prev_box_dbg

        // ------------------------------------------------------------
        // Ensure walkbox geometry for the current room/costume is resident
        // ------------------------------------------------------------
        jsr     get_walkboxes_for_costume

        // Walkboxes not resident? Exit
        cmp     #WALKBOX_NOT_RESIDENT
        bne     load_bfs_depth_or_fail
        rts


load_bfs_depth_or_fail:
        // ------------------------------------------------------------
        // Validate stored search depth for this actor
        //
        // actor_search_depth,x:
        //   $FF → no active path / invalid depth (nothing to build)
        //    0+ → depth index into actor_discovered_boxes
        // ------------------------------------------------------------
        lda     actor_search_depth,x
        cmp     #$FF
        bne     index_from_depth
        lda     #NO_COMPUTED_PATH
        rts

index_from_depth:
        // ------------------------------------------------------------
        // Use DFS depth to select the next target box from the
        // discovered-box list and update actor_box_prev.
        //
        //   - DEC actor_search_depth,x moves one “step” closer to origin
        //     for the next invocation.
        //   - .A still contains the pre-decrement depth value and is used
        //     as an index into actor_discovered_boxes.
        // ------------------------------------------------------------
        // Move one depth level toward origin in the stored depth
        dec     actor_search_depth,x

        // Save the (pre-decrement) depth index as the discovered-box slot
        sta     tgt_box_idx

        // Compute per-actor base offset: ((actor_index + 1) * 16)
        //   DISCOVERED_LIST_STRIDE = 16; the +1 bias skips a sentinel slot.
        txa
        asl
        asl
        asl
        asl
        clc
        adc     #DISCOVERED_LIST_STRIDE
        tay

        // Add depth index to reach this step's discovered-box entry
        clc
        adc     tgt_box_idx
        tay
        sty     saved_ofs_y

        // Fetch box id at the chosen depth
        lda     actor_discovered_boxes,y

        // Stash as "previous box" and as the current target box index
        sta     actor_box_prev,x
        sta     tgt_box_idx

        // Restore offset for possible penultimate-box debug read
        ldy     saved_ofs_y

        // If depth is 0 or $FF after decrement, there is no penultimate box
        lda     actor_search_depth,x
        beq     point_to_target_box
        cmp     #$FF
        beq     point_to_target_box

        // Debug: copy penultimate box (one level closer to origin) into prev_box_dbg
        dey
        lda     actor_discovered_boxes,y
        sta     prev_box_dbg
        iny

point_to_target_box:
        // ------------------------------------------------------------
        // Derive target_box_ptr from current_box_ptr and tgt_box_idx
        //
        //   target_box_ptr = current_box_ptr + (tgt_box_idx * WALKBOX_STRIDE)
        //   where WALKBOX_STRIDE = 5 bytes per walkbox.
        // ------------------------------------------------------------
        // Preserve original current_box_ptr in temporary scratch (unused vars)
        lda     current_box_ptr
        sta     $1B
        lda     current_box_ptr + 1
        sta     $1C

        // Initialize target_box_ptr high byte from current_box_ptr
        lda     current_box_ptr + 1
        sta     target_box_ptr + 1

        // Compute tgt_box_idx * 5 ( * 4, then + 1) and add to current_box_ptr
        lda     tgt_box_idx
        asl
        asl
        clc
        adc     tgt_box_idx
        adc     current_box_ptr
        sta     target_box_ptr
        bcc     point_to_current_box                    // Carry adjust for high byte
        inc     target_box_ptr + 1

point_to_current_box:
        // ------------------------------------------------------------
        // Re-point current_box_ptr to the actor's current box:
        //
        //   current_box_ptr = base + (actor_box_cur * WALKBOX_STRIDE)
        // ------------------------------------------------------------
        lda     actor_box_cur,x
        asl
        asl
        clc
        adc     actor_box_cur,x
        adc     current_box_ptr
        sta     current_box_ptr
        bcc     classify_box_relation_2
        inc     current_box_ptr + 1

classify_box_relation_2:
        // ------------------------------------------------------------
        // Classify spatial relationship between current and target boxes.
        //
        //   A ← REL_ABOVE / REL_BELOW / REL_LEFT / REL_RIGHT / REL_OVERLAP_TOUCH
        // ------------------------------------------------------------
        ldy     #$00
        jsr     classify_box_relation
        cmp     #REL_OVERLAP_TOUCH
        bne     relation_non_overlap

        // ------------------------------------------------------------
        // Boxes overlap/touch:
        //   - Stop motion on the next frame.
        //   - Invalidate search depth so no further path steps are used.
        //   - Signal "done" to caller via A=NO_COMPUTED_PATH.
        // ------------------------------------------------------------
        ldx     actor
        lda     #MOTION_ABOUT_TO_STOP
        sta     actor_motion_state,x

        // Invalidate search depth
        lda     #$FF
        sta     actor_search_depth,x

        lda     #NO_COMPUTED_PATH
        rts

relation_non_overlap:
        // ------------------------------------------------------------
        // Boxes do not overlap. Cache relation and classify the actor
        // along the orthogonal axis (inside / below / above, etc.).
        // ------------------------------------------------------------
        // Cache relative position (encoded REL_* bits) for later tests
        sta     box_relpos_code

        // Classify actor coordinate vs target-box axis → actor_axis_class
        jsr     classify_actor_axis_vs_box
        ldx     actor
        sta     actor_axis_class

        // ------------------------------------------------------------
        // Decide whether the path segment can be taken straight, or if a
        // corner waypoint is required based on hall width and origin side.
        //
        //   A=$00 → no corner needed (straight segment)
        //   A=$FF → corner adjustment required
        // ------------------------------------------------------------
        jsr     check_path_requires_corner_adjustment
        bne     route_via_corner

        // ------------------------------------------------------------
        // No adjustment:
        //   - Use the actor's snapped target as the waypoint.
        //   - Return A=$00 to signal "straight path" to caller.
        // ------------------------------------------------------------
        lda     actor_target_x,x
        sta     actor_tgt_waypoint_x,x
        lda     actor_target_y,x
        sta     actor_tgt_waypoint_y,x
        lda     #$00
        rts

route_via_corner:
        // ------------------------------------------------------------
        // Corner adjustment required:
        //
        // box_relpos_code bit1:
        //   0 → vertical relation (current above/below target)
        //       → fix X first, then derive Y from box edges.
        //   1 → horizontal relation (current left/right of target)
        //       → fix Y first, then derive X from box edges.
        //
        // actor_axis_class:
        //   AXIS_INSIDE → keep actor's coord on the orthogonal axis.
        //   AXIS_OUTSIDE_LOW/AXIS_OUTSIDE_HIGH → pick nearest edge (left/right or top/bottom).
        // ------------------------------------------------------------
        lda     box_relpos_code
        and     #REL_AXIS_HORIZONTAL_BIT
        bne     corner_fix_for_horizontal_relation

        // ------------------------------------------------------------
        // Vertical relation:
        //   1) Fix X (orthogonal axis).
        //      - If INSIDE, keep actor_pos_x.
        //      - Else, pick left/right edge from target_box_ptr.
        //   2) Derive Y from target box's top/bottom using relation+2.
        // ------------------------------------------------------------
        lda     actor_axis_class
        cmp     #AXIS_INSIDE 
        bne     pick_tgt_left_right

        // Actor X inside corridor → keep actor X for corner waypoint
        lda     actor_pos_x,x
        jmp     write_waypoint_x_then_y

pick_tgt_left_right:
        // Use axis class AXIS_OUTSIDE_LOW/AXIS_OUTSIDE_HIGH ($00/$01) 
		// as index 0/1 into {left,right} box edges
        tay
        lda     (target_box_ptr),y

write_waypoint_x_then_y:
        // Write X for corner waypoint
        sta     actor_tgt_waypoint_x,x

        // Now derive Y (top/bottom) from relation+2
        ldy     box_relpos_code
        iny
        iny                                             // offset += 2 → top/bottom edges
        lda     (target_box_ptr),y
        sta     actor_tgt_waypoint_y,x
        jmp     return_ok

corner_fix_for_horizontal_relation:
        // ------------------------------------------------------------
        // Horizontal relation:
        //   1) Fix Y (orthogonal axis).
        //      - If INSIDE, keep actor_pos_y.
        //      - Else, pick top/bottom from target_box_ptr.
        //   2) Derive X from target box's left/right using relation-2.
        // ------------------------------------------------------------
        lda     actor_axis_class
        cmp     #AXIS_INSIDE 
        bne     pick_tgt_top_bottom

        // Actor Y inside corridor → keep actor Y for corner waypoint
        lda     actor_pos_y,x
        jmp     write_waypoint_y_then_x

pick_tgt_top_bottom:
        // Use axis class AXIS_OUTSIDE_LOW/AXIS_OUTSIDE_HIGH ($00/$01) 
		// as index 0/1 into {top,bottom} box edges
        tay
        iny
        iny                                             // +2 → box top/bottom edges
        lda     (target_box_ptr),y

write_waypoint_y_then_x:
        // Write Y for corner waypoint
        sta     actor_tgt_waypoint_y,x

        // Now derive X (left/right) from relation-2
        ldy     box_relpos_code
        dey
        dey                                             // -2 → box left/right edges
        lda     (target_box_ptr),y
        sta     actor_tgt_waypoint_x,x

return_ok:
        // Corner route successfully computed; signal "path adjusted"
        lda     #$00
        rts

/*
================================================================================
  classify_box_relation
================================================================================
Summary
	Determine the relative position between two axis-aligned boxes (AABBs).

Arguments
	current_box_ptr  	pointer to {left, right, top, bottom} edges of the current box
	target_box_ptr   	pointer to {left, right, top, bottom} edges of the current box

Returns
	A   	REL_BELOW      		current box below target box
			REL_ABOVE      		current box above target box
			REL_RIGHT      		current box right of target box
			REL_LEFT       		current box left of target box
			REL_OVERLAP_TOUCH	boxes overlap or touch

Description
	* Equality counts as overlap: edge or corner contact → OVERLAP.
	* Coordinate convention: larger Y is lower on screen.
	* Offsets: 0=left, 1=right, 2=top, 3=bottom.
	* Axis encoding: bit1 of result set → horizontal (02/03), clear → vertical (00/01).
================================================================================
*/
* = $1D9E
classify_box_relation:
        // ------------------------------------------------------------
		// Vertical test: current.top vs. target.bottom		
        // ------------------------------------------------------------
		// Set offset to top edge (+2)
		iny
		iny
		
		// A := current.top
		lda     (current_box_ptr),y         
		
		// Set offset to bottom edge (+3)
		iny
		
		// Compare vs. target.bottom
		cmp     (target_box_ptr),y   		
		
		// Equal? Continue testing
		beq     test_vertical_above
		
		// current.top > target.bottom → current is below
		// (vertical coordinates grow downwards on the screen)
		bcs     box_below_target            

test_vertical_above:
        // ------------------------------------------------------------
		// Vertical test: current.bottom vs. target.top
        // ------------------------------------------------------------
		// Offset already at bottom edge (+3)
		// A := current.bottom
		lda     (current_box_ptr),y           
		
		// Set offset to top edge (+2)
		dey
		
		// Compare vs. target.top
		cmp     (target_box_ptr),y            
		
		// current.bottom < target.top → current is above
		bcc     box_above_target              

        // ------------------------------------------------------------
		// Horizontal test: current.left vs. target.right
        // ------------------------------------------------------------
		// Set offset to left edge (+0)
		dey
		dey
		
		// A := current.left
		lda     (current_box_ptr),y           
		
		// Set offset to right edge (+1)
		iny
		
		// Compare vs. target.right
		cmp     (target_box_ptr),y            
		
		// Equal? Continue testing
		beq     test_horizontal_left
		
		// current.left > target.right → current is right
		bcs     box_right_of_target                     

test_horizontal_left:
        // ------------------------------------------------------------
		// Horizontal test: current.right vs. target.left
        // ------------------------------------------------------------
		// Offset already at right edge (+1)
		// A := current.right
		lda     (current_box_ptr),y           
		
		// Set offset to left edge (+0)
		dey
		
		// Compare vs. target.left
		cmp     (target_box_ptr),y           
		
		// current.right < target.left → current is left
		bcc     box_left_of_target                  
		
		// Otherwise, boxes are touching or overlapping
		lda     #REL_OVERLAP_TOUCH                     
		jmp     return_relative_position

box_left_of_target:
		lda     #REL_LEFT                          
		jmp     return_relative_position

box_right_of_target:
		lda     #REL_RIGHT                         
		jmp     return_relative_position

box_above_target:
		lda     #REL_ABOVE                         
		jmp     return_relative_position

box_below_target:
		lda     #REL_BELOW                         
		
return_relative_position:
		rts
/*
================================================================================
  classify_actor_axis_vs_box
================================================================================
Summary
	Classify an actor’s coordinate against the target box on the appropriate
	axis to detect if a “corner turn” will be required.

Arguments
	X  actor index

Vars/State
	rel_axis_coord        		scratch: selected actor axis coordinate

Global Inputs
	box_relpos_code       		relation code (bit1: 0=vertical,1=horizontal)
	target_box_ptr        		zp → {left, right, top, bottom}
	actor_x/y_pos[]         	per-actor X/Y position table

Returns
	A = 	AXIS_OUTSIDE_LOW   	actor < lower edge (left/top)
			AXIS_OUTSIDE_HIGH  	actor > upper edge (right/bottom)
			AXIS_INSIDE  		actor within [lower..upper]

Description

	* If relation is horizontal (bit1=1), test actor’s Y vs target top/bottom.
	* If relation is vertical   (bit1=0), test actor’s X vs target left/right.
	* Uses Y as edge offset base: 0 for X-axis (left/right), 2 for Y-axis (top/bottom).
	* Equality with either edge counts as inside.

Notes

	* Clobbers A, Y. Preserves X.
	* Assumes target_box_ptr layout: {left, right, top, bottom}.
	* O(1) early-exit comparisons; no carry/overflow guarantees on return.
================================================================================
*/
* = $1DD9
classify_actor_axis_vs_box:
        // ------------------------------------------------------------
        // Select actor coordinate on the axis orthogonal to box relation.
        //
        // box_relpos_code bit1:
        //   0 → vertical relation (current above/below target)
        //        → orthogonal axis is X (use actor_pos_x).
        //   1 → horizontal relation (current left/right of target)
        //        → orthogonal axis is Y (use actor_pos_y).
        //
        // Y is set to the corresponding target-box offset:
        //   Y = $00 → left/top
        //   Y = $02 → top/bottom
        // ------------------------------------------------------------
        lda     box_relpos_code
        and     #REL_AXIS_HORIZONTAL_BIT
        tay                                 // Y := $00 (vertical) or $02 (horizontal)
        bne     select_actor_y              // A≠0 → horizontal relation → use Y

        // Vertical relation → classify actor X vs target’s horizontal span
        lda     actor_pos_x,x
        jmp     store_axis_coord

select_actor_y:
        // Horizontal relation → classify actor Y vs target’s vertical span
        lda     actor_pos_y,x

store_axis_coord:
        // Cache the actor coordinate on the selected axis
        sta     rel_axis_coord

        // ------------------------------------------------------------
        // Range test vs target box on the selected axis.
        //
        //   Compare against lower edge (left/top):
        //     actor < lower  → AXIS_OUTSIDE_LOW
        //
        //   If actor ≥ lower, compare against upper edge (right/bottom):
        //     actor ≤ upper  → AXIS_INSIDE
        //     actor > upper  → AXIS_OUTSIDE_HIGH
        //
        // This classification drives whether we keep the actor’s coord
        // on the orthogonal axis or snap it to an edge when routing.
        // ------------------------------------------------------------
        lda     rel_axis_coord
        cmp     (target_box_ptr),y          // vs lower (left/top)
        bcs     check_against_upper

        lda     #AXIS_OUTSIDE_LOW           // actor < lower → needs corner down/right
        rts

check_against_upper:
        iny                                 // move to upper (right/bottom)
        cmp     (target_box_ptr),y
        bcc     inside_range
        beq     inside_range

        lda     #AXIS_OUTSIDE_HIGH          // actor > upper → needs corner up/left
        rts

inside_range:
        lda     #AXIS_INSIDE                // actor inside target axis range
        rts
/*
================================================================================
  check_path_requires_corner_adjustment
================================================================================
Summary
	Decide if the actor’s straight-line target requires a corner adjustment.

Arguments
	X  actor index

Returns
	A = 	PATH_ADJUST_REQUIRED 	target cannot be taken straight
			PATH_NO_ADJUST     		straight path is valid

Global Inputs
	relpos_code                   		RELPOS_* from classify_box_relation
	actor_axis_class              		$00/$01 outside, $02 inside (orthogonal axis)
	cur_box_ptr_zp, tgt_box_ptr_zp 		{left,right,top,bottom}
	actor_target_x/y_tbl[]
	
Description
	* First gate: orthogonal-axis containment
	  • If actor_axis_class != AXIS_INSIDE ($02) → PATH_ADJUST_REQUIRED.
	* Build “narrowest hall” on the orthogonal axis
	  • Use relpos_code.bit1 to choose axis (0: horizontal, 1: vertical).
	  • hall_lower_bound := max(current.first, target.first).
	  • hall_upper_bound := min(current.second, target.second).
	  • Require strict interior: hall_lower_bound < dest < hall_upper_bound,
	  else PATH_ADJUST_REQUIRED.
	* Second gate: along-relation coordinate vs current box
	  • Use relpos_code.bit0 to pick which current edge is nearer the origin.
	  • If current is farther (bit0=0): require other < current.first.
	  • If current is closer (bit0=1): require other ≥ current.first.
	  • Pass → PATH_NO_ADJUST, fail → PATH_ADJUST_REQUIRED.


Notes
	* Edge equality on the “hall” test counts as adjustment (strict inequality).
	* Clobbers A, Y; preserves X.
	* Known issue in provided code: the vertical path under switch_to_other_axis is
	  unreachable due to a DEY/BNE sequence; branch should test Y without changing it.
================================================================================
*/
* = $1E07
check_path_requires_corner_adjustment:
        // ------------------------------------------------------------
        // Orthogonal-axis containment: actor must be inside target’s span
        //
        //   A := actor_axis_class ($02 = inside, $00/$01 = outside)
        // ------------------------------------------------------------
        lda     actor_axis_class
        cmp     #AXIS_INSIDE
        beq     build_narrowest_hall

        // Outside orthogonal range → corner adjustment required
        lda     #PATH_ADJUST_REQUIRED
        rts


build_narrowest_hall:
        // ------------------------------------------------------------
        // Build the “narrowest hall” on the orthogonal axis.
        //
        // Axis select from box_relpos_code.bit1:
        //   0 → horizontal edges (left/right), Y := $00
        //   1 → vertical   edges (top/bottom), Y := $02
        //
        // hall_lower_bound  := max(current.first , target.first)   (farther from origin)
        // hall_upper_bound  := min(current.second, target.second)  (closer  to origin)
        //
        // Later we require:
        //   hall_lower_bound < dest < hall_upper_bound
        // ------------------------------------------------------------
        lda     box_relpos_code
        and     #REL_AXIS_HORIZONTAL_BIT
        tay

        // Pick hall_lower_bound = max(firsts) on orthogonal axis
        lda     (current_box_ptr),y
        cmp     (target_box_ptr),y
        beq     pick_target_as_first_edge
        bcs     set_hall_lower_bound

pick_target_as_first_edge:
        // Target’s first edge is nearer the origin → use it as lower bound
        lda     (target_box_ptr),y

set_hall_lower_bound:
        sta     hall_lower_bound

        // Pick hall_upper_bound = min(seconds) on orthogonal axis
        iny
        lda     (current_box_ptr),y
        cmp     (target_box_ptr),y
        bcc     set_hall_upper_bound
        lda     (target_box_ptr),y

set_hall_upper_bound:
        sta     hall_upper_bound

        // ------------------------------------------------------------
        // Choose target coordinate on this orthogonal axis:
        //   bit1=0 → use actor_target_x   (horizontal edges)
        //   bit1=1 → use actor_target_y   (vertical edges)
        // ------------------------------------------------------------
        lda     box_relpos_code
        and     #REL_AXIS_HORIZONTAL_BIT
        bne     load_dest_vertical

        // Horizontal relation on orthogonal axis → test X
        lda     actor_target_x,x
        jmp     test_hall_lower_bound

load_dest_vertical:
        // Vertical relation on orthogonal axis → test Y
        lda     actor_target_y,x

test_hall_lower_bound:
        // Require strict inside: hall_lower_bound < dest < hall_upper_bound
        cmp     hall_lower_bound
        beq     return_PATH_ADJUST_REQUIRED               // touching lower edge → adjust
        bcs     test_hall_upper_bound

return_PATH_ADJUST_REQUIRED:
        // dest ≤ hall_lower_bound → corner required
        lda     #PATH_ADJUST_REQUIRED
        rts

test_hall_upper_bound:
        cmp     hall_upper_bound
        bcc     switch_to_other_axis
        // dest ≥ hall_upper_bound → corner required
        lda     #PATH_ADJUST_REQUIRED
        rts


switch_to_other_axis:
        // ------------------------------------------------------------
        // Now test the OTHER target coordinate against the current
        // box edge along the relation axis.
        //
        // Intended behavior:
        //   - If orthogonal axis was X, test dest Y against a top/bottom
        //     edge from current_box_ptr.
        //   - If orthogonal axis was Y, test dest X against a left/right
        //     edge from current_box_ptr.
        // ------------------------------------------------------------
        dey
        bne     load_other_horizontal_coord

        // Vertical-other case: test Y against top/bottom edge
        lda     actor_target_y,x
        ldy     box_relpos_code
        iny
        iny
        jmp     store_other_axis_coord

load_other_horizontal_coord:
        // Horizontal-other case: test X against left/right edge
        lda     actor_target_x,x
        ldy     box_relpos_code
        dey
        dey

store_other_axis_coord:
        // Cache the target coordinate on the relation axis
        sta     dest_other_axis

        // ------------------------------------------------------------
        // Compare against current box edge nearest the origin:
        //
        //   bit0=1 → current box is closer (up/left)
        //   bit0=0 → current box is farther (down/right)
        //
        // If farther: require dest_other_axis < current.first
        // If closer : require dest_other_axis ≥ current.first
        // ------------------------------------------------------------
        lda     box_relpos_code
        and     #REL_NEAR_ORIGIN_BIT
        bne     check_closest_box_edge

        // Current box is farther from origin: dest must be before its edge
        lda     dest_other_axis
        cmp     (current_box_ptr),y
        bcc     return_PATH_NO_ADJUST_alt                 // OK: dest < current.first
        // Past edge → corner adjustment required
        lda     #PATH_ADJUST_REQUIRED
        rts

return_PATH_NO_ADJUST_alt:
        // Fall through to common "no adjust" exit
        jmp     return_PATH_NO_ADJUST

check_closest_box_edge:
        // Current box is nearer the origin: dest must be at/after its edge
        lda     dest_other_axis
        cmp     (current_box_ptr),y
        beq     return_PATH_ADJUST                        // touching edge → adjust
        bcs     return_PATH_NO_ADJUST                     // OK: dest ≥ current.first

return_PATH_ADJUST:
        // Below/behind edge → corner adjustment required
        lda     #PATH_ADJUST_REQUIRED
        rts

return_PATH_NO_ADJUST:
        // All tests passed → straight path is acceptable
        lda     #PATH_NO_ADJUST
        rts

/*
Pseudo-code

function stage_actor_path_to_target():
    // Snap the actor’s desired target to the walkbox graph
    snap_actor_target()

    x = actor                     // current actor index

    // If no walkbox could be found for the target, bail out
    if nearest_box_idx == NO_BOX_FOUND:
        return

    // Store snapped target box for this actor
    actor_target_box[x] = nearest_box_idx

    // If current box and target box are the same, we don't need a path
    if actor_target_box[x] == actor_box_cur[x]:
        // Compute per-actor base offset into discovered_boxes
        base_offset = (x + 1) * DISCOVERED_LIST_STRIDE

        // Clear this actor's "slot 0" so downstream code knows no path was built
        actor_discovered_boxes[base_offset + 0] = NO_BOX_FOUND
    else:
        // Current box differs from target box: build full path between them
        find_path_between_walkboxes()

    // Mark that the waypoint/path for this actor must be recomputed
    actor_path_update_flag[x] = TRUE

    // Snapshot the actor’s current box for later comparison/diagnostics
    actor_box_prev[x] = actor_box_cur[x]


function apply_pending_waypoint_update():
    x = actor

    // If no path/waypoint update is pending, do nothing
    if actor_path_update_flag[x] == FALSE:
        return

    // Clear the pending flag
    actor_path_update_flag[x] = FALSE

    // Try to compute the next waypoint from the path
    result = compute_waypoint_from_path()

    // If no waypoint was produced (overlap / no path / geometry failure),
    // fall back to "go directly to target coordinates"
    if result == NO_COMPUTED_PATH:
        x = actor
        actor_tgt_waypoint_x[x] = actor_target_x[x]
        actor_tgt_waypoint_y[x] = actor_target_y[x]

    // Otherwise, compute_waypoint_from_path has already written the waypoint
    return


function compute_waypoint_from_path() -> byte:
    x = actor

    // Debug: reset previous-box debug slot
    prev_box_dbg = 0xFF

    // Ensure walkbox geometry is resident for current room/costume
    status = get_walkboxes_for_costume()
    if status == WALKBOX_NOT_RESIDENT:
        // Cannot compute waypoint if geometry is missing
        return NO_COMPUTED_PATH   // effectively “no result”; caller will fall back

    // ----------------------------------------------------------------
    // Validate and use the stored search depth for this actor
    // ----------------------------------------------------------------
    depth = actor_search_depth[x]

    if depth == 0xFF:
        // No valid path recorded for this actor
        return NO_COMPUTED_PATH

    // Decrement depth so that future calls walk back toward the origin
    actor_search_depth[x] = depth - 1

    // Use the pre-decrement depth to pick a box from the discovered list
    depth_index = depth

    // Per-actor base offset into discovered-box list:
    base_offset = (x + 1) * DISCOVERED_LIST_STRIDE

    entry_offset = base_offset + depth_index
    saved_ofs_y = entry_offset

    // Fetch box id at this depth
    box_id = actor_discovered_boxes[entry_offset]

    // Store as the “previous box” and current target box index for this step
    actor_box_prev[x] = box_id
    tgt_box_idx = box_id

    // Debug: if there is still a deeper level after decrement,
    // copy the penultimate box id into prev_box_dbg
    if actor_search_depth[x] != 0 and actor_search_depth[x] != 0xFF:
        penultimate_offset = entry_offset - 1
        prev_box_dbg = actor_discovered_boxes[penultimate_offset]

    // ----------------------------------------------------------------
    // Derive pointers to target and current boxes in the walkbox table
    // ----------------------------------------------------------------

    // current_box_ptr was initialized by get_walkboxes_for_costume()
    base_ptr = current_box_ptr   // base address of box table

    target_box_ptr = base_ptr + (tgt_box_idx * WALKBOX_STRIDE)

    // current_box_ptr = base + (actor_box_cur[x] * WALKBOX_STRIDE)
    cur_idx = actor_box_cur[x]
    current_box_ptr = base_ptr + (cur_idx * WALKBOX_STRIDE)

    // ----------------------------------------------------------------
    // Classify relationship between current and target boxes
    // ----------------------------------------------------------------
    relation = classify_box_relation(current_box_ptr, target_box_ptr)

    if relation == REL_OVERLAP_TOUCH:
        // Boxes overlap/touch: this means we are effectively “at” the target
        x = actor
        actor_motion_state[x] = MOTION_ABOUT_TO_STOP

        // Invalidate further path usage
        actor_search_depth[x] = 0xFF

        return NO_COMPUTED_PATH

    // Cache relation and classify actor vs target along orthogonal axis
    box_relpos_code = relation
    axis_class = classify_actor_axis_vs_box(x)
    actor_axis_class = axis_class

    // ----------------------------------------------------------------
    // Decide straight vs corner
    // ----------------------------------------------------------------
    adjust = check_path_requires_corner_adjustment(x)

    if adjust == PATH_NO_ADJUST:
        // Straight line is safe: waypoint is the snapped target
        x = actor
        actor_tgt_waypoint_x[x] = actor_target_x[x]
        actor_tgt_waypoint_y[x] = actor_target_y[x]
        return 0x00    // success, straight segment

    // ----------------------------------------------------------------
    // Corner adjustment required
    // ----------------------------------------------------------------
    // bit1 of relation selects vertical vs horizontal relation:
    //   0 = vertical (above/below)
    //   1 = horizontal (left/right)
    if (box_relpos_code & REL_AXIS_HORIZONTAL_BIT) == 0:
        // ---------------------------
        // Vertical relation (current above/below target):
        //   1) Fix X (orthogonal axis).
        //   2) Derive Y from target top/bottom.
        // ---------------------------
        if actor_axis_class == AXIS_INSIDE:
            corner_x = actor_pos_x[x]
        else:
            // actor is left or right of the narrowed corridor;
            // use left/right edge from target box
            edge_index = (actor_axis_class == AXIS_OUTSIDE_LOW) ? 0 : 1  // 0=left,1=right
            corner_x = target_box_ptr[edge_index]

        // For vertical relation, Y edge is chosen from {top,bottom} via relation+2
        edge_index_y = box_relpos_code + 2      // 2=top, 3=bottom
        corner_y = target_box_ptr[edge_index_y]

        actor_tgt_waypoint_x[x] = corner_x
        actor_tgt_waypoint_y[x] = corner_y
    else:
        // ---------------------------
        // Horizontal relation (current left/right of target):
        //   1) Fix Y (orthogonal axis).
        //   2) Derive X from target left/right.
        // ---------------------------
        if actor_axis_class == AXIS_INSIDE:
            corner_y = actor_pos_y[x]
        else:
            // actor is above or below the narrowed corridor;
            // use top/bottom edge from target box
            edge_index = (actor_axis_class == AXIS_OUTSIDE_LOW) ? 2 : 3  // 2=top,3=bottom
            corner_y = target_box_ptr[edge_index]

        // For horizontal relation, X edge is chosen from {left,right} via relation-2
        edge_index_x = box_relpos_code - 2      // 0=left, 1=right
        corner_x = target_box_ptr[edge_index_x]

        actor_tgt_waypoint_y[x] = corner_y
        actor_tgt_waypoint_x[x] = corner_x

    return 0x00      // success, corner waypoint emitted


function classify_box_relation(current_box_ptr, target_box_ptr) -> byte:
    // Boxes are AABBs with edges:
    //   [0]=left, [1]=right, [2]=top, [3]=bottom
    cur_left   = current_box_ptr[0]
    cur_right  = current_box_ptr[1]
    cur_top    = current_box_ptr[2]
    cur_bottom = current_box_ptr[3]

    tgt_left   = target_box_ptr[0]
    tgt_right  = target_box_ptr[1]
    tgt_top    = target_box_ptr[2]
    tgt_bottom = target_box_ptr[3]

    // -----------------------------
    // Vertical separation tests
    // -----------------------------
    // If current.top strictly below target.bottom → current below target
    if cur_top > tgt_bottom:
        return REL_BELOW

    // If current.bottom strictly above target.top → current above target
    if cur_bottom < tgt_top:
        return REL_ABOVE

    // -----------------------------
    // Horizontal separation tests
    // -----------------------------
    // If current.left strictly right of target.right → current right of target
    if cur_left > tgt_right:
        return REL_RIGHT

    // If current.right strictly left of target.left → current left of target
    if cur_right < tgt_left:
        return REL_LEFT

    // Otherwise, boxes overlap or touch on at least one axis
    return REL_OVERLAP_TOUCH


function classify_actor_axis_vs_box(actorIndex) -> byte:
    x = actorIndex

    // The relation that was previously computed:
    relation = box_relpos_code

    // bit1: 0 = vertical relation, 1 = horizontal relation
    is_horizontal_relation = (relation & REL_AXIS_HORIZONTAL_BIT) != 0

    // Decide which actor coordinate to test and which box edges to use
    if not is_horizontal_relation:
        // Vertical relation (above/below):
        //   → orthogonal axis is X
        actor_coord = actor_pos_x[x]

        // On this axis, compare against {left,right} at offsets 0,1
        lower_edge_index = 0
        upper_edge_index = 1
    else:
        // Horizontal relation (left/right):
        //   → orthogonal axis is Y
        actor_coord = actor_pos_y[x]

        // On this axis, compare against {top,bottom} at offsets 2,3
        lower_edge_index = 2
        upper_edge_index = 3

    lower = target_box_ptr[lower_edge_index]
    upper = target_box_ptr[upper_edge_index]

    // actor_coord < lower → outside (toward origin-bottom/right depending on axes)
    if actor_coord < lower:
        return AXIS_OUTSIDE_LOW

    // actor_coord > upper → outside (toward origin-top/left)
    if actor_coord > upper:
        return AXIS_OUTSIDE_HIGH

    // Otherwise inside [lower .. upper] inclusive
    return AXIS_INSIDE


function check_path_requires_corner_adjustment(actorIndex) -> byte:
    x = actorIndex

    // ------------------------------------------------
    // 1) Orthogonal-axis containment gate
    // ------------------------------------------------
    if actor_axis_class != AXIS_INSIDE:
        // Actor is outside target's span on orthogonal axis → must use corner
        return PATH_ADJUST_REQUIRED

    // ------------------------------------------------
    // 2) Build "narrowest hall" on the orthogonal axis
    // ------------------------------------------------

    relation = box_relpos_code
    is_horizontal_rel = (relation & REL_AXIS_HORIZONTAL_BIT) != 0

    // Choose which edges to combine for the hall
    if not is_horizontal_rel:
        // Relation vertical → orthogonal axis is horizontal:
        // use left/right edges (indices 0,1)
        first_idx  = 0   // left
        second_idx = 1   // right
        dest_coord = actor_target_x[x]
    else:
        // Relation horizontal → orthogonal axis is vertical:
        // use top/bottom edges (indices 2,3)
        first_idx  = 2   // top
        second_idx = 3   // bottom
        dest_coord = actor_target_y[x]

    // hall_lower_bound = max(current.first, target.first)
    cur_first = current_box_ptr[first_idx]
    tgt_first = target_box_ptr[first_idx]
    if cur_first >= tgt_first:
        hall_lower_bound = cur_first
    else:
        hall_lower_bound = tgt_first

    // hall_upper_bound = min(current.second, target.second)
    cur_second = current_box_ptr[second_idx]
    tgt_second = target_box_ptr[second_idx]
    if cur_second <= tgt_second:
        hall_upper_bound = cur_second
    else:
        hall_upper_bound = tgt_second

    // Strict interior test on orthogonal axis: hall_lower < dest < hall_upper
    if dest_coord <= hall_lower_bound:
        return PATH_ADJUST_REQUIRED
    if dest_coord >= hall_upper_bound:
        return PATH_ADJUST_REQUIRED

    // ------------------------------------------------
    // 3) Test the OTHER coordinate along the relation axis
    // ------------------------------------------------
    // We now consider the target coordinate on the "along movement" axis
    // and compare against the current box edge nearest the origin.

    if not is_horizontal_rel:
        // Relation is vertical: other axis is Y
        dest_other_axis = actor_target_y[x]

        // For vertical relation, relation codes are:
        //   REL_BELOW (0) or REL_ABOVE (1).
        // Using relation+2 selects top/bottom index 2 or 3 on current box.
        edge_index = relation + 2       // 2=top, 3=bottom
    else:
        // Relation is horizontal: other axis is X
        dest_other_axis = actor_target_x[x]

        // For horizontal relation, relation codes are:
        //   REL_RIGHT (2) or REL_LEFT (3).
        // Subtract 2 to get left/right index 0 or 1.
        edge_index = relation - 2       // 0=left, 1=right

    current_edge = current_box_ptr[edge_index]

    // bit0 of relation: 0 = current box farther from origin (down/right),
    //                   1 = current box nearer the origin (up/left)
    is_current_near_origin = (relation & REL_NEAR_ORIGIN_BIT) != 0

	if not is_current_near_origin:
        // Current box is farther away from origin:
        //   require dest_other_axis < current_edge
        if dest_other_axis < current_edge:
            return PATH_NO_ADJUST
        else:
            return PATH_ADJUST_REQUIRED
    else:
        // Current box is nearer origin:
        //   require dest_other_axis >= current_edge (but equality is treated as adjust)
        if dest_other_axis > current_edge:
            return PATH_NO_ADJUST
        else:
            // dest_other_axis <= current_edge → corner required
            return PATH_ADJUST_REQUIRED
			
*/			