/*
================================================================================
Walkbox and Pathfinding System Overview
================================================================================

    Provides all logic for finding, classifying, and connecting “walkboxes”, 
    rectangular walkable zones used by actors for navigation. Handles geometric
    queries (inside checks, nearest box, distance estimation) and graph search
    (building routes between boxes).

Concepts
    • A room contains a list of walkboxes. Each entry defines {left, right, top,
      bottom} edges in pixels. Lists are terminated by $FF.
    • Each box also has an adjacency list describing which boxes connect to it.
    • Coordinates grow downward (larger Y = lower on screen).

Main Components
    1.  Table access
        - get_walkboxes_for_room / get_walkboxes_for_costume / get_walkboxes_ptr
          compute the pointer to the room's walkbox table.
        - Walkbox records have a stride of 5 bytes, plus a global offset at $15.

    2.  Geometry and proximity
        - get_distance_to_box clamps a point to a box, computes the nearest point
          and a fast “octagonal” distance (0 if inside).
        - get_nearest_box scans all boxes to find the one closest to a given
          coordinate pair.
        - is_actor_pos_inside_box tests if an actor's position lies inside a box.

    3.  Path search between boxes
        - build_walkbox_path performs a bounded depth-first search through the
          adjacency graph to connect an actor's current box to a destination box.
        - increment_search_depth / decrement_search_depth manage recursive depth
          with upper/lower caps.
        - adjacency_contains_destination checks if the destination appears in
          the current box's neighbor list.
        - all_adjacent_boxes_known detects when every neighbor has been visited.
        - write_actor_path_buffer serializes the successful path for an actor.

    4.  Packed list utilities
        - get_list_element_offset finds the byte offset of the N-th element in a
          $FF-terminated list.
        - restore_adjacency_list restores scan position when resuming exploration.

Operation summary
    • When a path is requested, the system resolves the room's walkbox table,
      then begins a controlled DFS search.
    • Each depth represents one box in the route. The system tracks which boxes
      were already seen and where to resume in each adjacency list.
    • It stops if:
          - the destination is found  → RESULT_PATH_OK
          - the graph is fully explored → RESULT_PATH_GRAPH_EXHAUSTED
          - the maximum search depth is hit → RESULT_PATH_MAX_DEPTH
    • The final path buffer contains the destination followed by the reverse
      chain of boxes leading back to the start.

Why it works efficiently
    • Entirely integer-based math; no multiplies or square roots.
    • Compact state per actor: current depth, discovered boxes, and resume
      indices.
    • Each actor has a fixed-size (16-byte) segment for its computed route.

================================================================================

Hypothetical DFS example with 4 boxes

Setup
  Boxes: 0,1,2,3
  Start (current_box) = 0
  Goal  (destination_box_id) = 3

Adjacency lists (packed, $FF-terminated)
  adj(0): [1, 2, $FF]
  adj(1): [0, $FF]
  adj(2): [0, 3, $FF]
  adj(3): [2, $FF]

State vars (per algorithm)
  active_depth_level       1-based depth
  actor_discovered_boxes   stack of chosen nodes by depth (index 1..N)
  resume_index_tbl         last scanned neighbor index per depth (starts $FF)

Walkthrough

1) Initialize
   active_depth_level = 0
   actor_discovered_boxes[*] = ?
   resume_index_tbl[*] = ?

2) increment_search_depth
   depth := 1
   actor_discovered_boxes[1] := current_box = 0
   resume_index_tbl[1] := $FF

   Check adj(0) for destination 3 → not found.
   all_adjacent_boxes_known?
     - resume_index_tbl[1] := $00  (first INC)
     - Next candidate = adj(0)[0] = 1 → is_box_seen_by_actor? (scan 1..1) → not seen.
     - Return ONE_UNKNOWN → continue exploration at depth 1 (caller loop).

3) Choose neighbor 1 (conceptually “descend” to 1)
   current_box := 1

   increment_search_depth
   depth := 2
   actor_discovered_boxes[2] := 1
   resume_index_tbl[2] := $FF

   Check adj(1) for destination 3 → not found.
   all_adjacent_boxes_known?
     - resume_index_tbl[2] := $00
     - Next candidate = adj(1)[0] = 0 → is_box_seen_by_actor? (scan 1..2 = {0,1})
       → 0 is seen → keep scanning
     - resume_index_tbl[2] := $01
     - Next byte = $FF → ALL_KNOWN

   decrement_search_depth
   depth := 1
   (backtrack to node 0 context)

4) Resume scanning adj(0) after neighbor index 0
   all_adjacent_boxes_known at depth 1?
     - resume_index_tbl[1] := $01
     - Next candidate = adj(0)[1] = 2 → is_box_seen_by_actor? (scan 1..1 = {0})
       → 2 not seen → ONE_UNKNOWN

5) Choose neighbor 2
   current_box := 2

   increment_search_depth
   depth := 2
   actor_discovered_boxes[2] := 2
   resume_index_tbl[2] := $FF

   Check adj(2) for destination 3 → FOUND (adj(2)[1] = 3)
   write_actor_path_buffer:
     - active_depth_level = 2 → path length = 1 step
     - Output segment:
         [0] = destination_box_id = 3
         [1] = actor_discovered_boxes[2] = 2
     Path = 2 → 3   (from current chain 0 → 2, then 2 → 3)

Result
  RESULT_PATH_OK
  actor_search_depth = 1
  actor_discovered_boxes segment = { 3, 2, ... }

Notes
  - The DFS explored 0→1, backtracked, then 0→2 and hit the destination.
  - resume_index_tbl ensured scanning resumed where it left off per depth.
  - Depth never exceeded 2, far below MAX_SEARCH_DEPTH.
  
================================================================================
Walkbox system — techniques and algorithms used
================================================================================

• Graph search / pathfinding:
    - Bounded depth-first search with explicit depth stack, resumable adjacency
      cursors, and cycle-avoidance “seen box” checks.
    - Adjacency lists stored as FF-terminated sublists; DFS resumes mid-list
      using per-depth resume_index_tbl[] for proper backtracking.
    - Path reconstruction by reversing the depth-indexed discovered-box stack.

• Packed list / table structures:
    - Walkboxes stored as variable-length FF-terminated lists packed back-to-back.
    - get_list_element_offset scans terminators to map a list index to its byte
      offset.
    - Fixed-stride per-actor path buffers for output paths.

• Geometry and distance:
    - Axis-aligned bounding-box (AABB) inclusion checks for “actor inside box?”
    - Nearest-box selection via distance minimization over all walkboxes.
    - Nearest-point-on-rectangle via clamping X/Y to [left..right], [top..bottom].
    - Fast octagonal distance approximation (shift/add metric) to avoid multipliers
      and square roots.

================================================================================
C64/6502-specific techniques and optimizations
================================================================================

• Arithmetic / control-flow techniques:
    - Multiplication by 5 via shift-and-add (index*5 = (index<<2)+index).
    - Iterative DFS loop (no recursion), manual depth management, sentinel-based
      termination.
    - Standard 6502 multi-byte counter and comparison idioms.

================================================================================
*/

#importonce
#import "globals.inc"
#import "constants.inc"

// -----------------------------------------------------------------------------
// Walkbox data pointers and indices
// -----------------------------------------------------------------------------
.label box_ptr                   = $17  // ZP: walkbox table base (lo at $17, hi at $18)
.label walkbox_room              = $FE74  // Active room id for walkbox tables
.label wbox_idx                  = $FC3F  // Scratch: original walkbox index under test

// -----------------------------------------------------------------------------
// Box geometry (current candidate)
// -----------------------------------------------------------------------------
.label box_left_edge             = $1A64  // Box left edge (pixels)
.label box_right_edge            = $1A65  // Box right edge (pixels)
.label box_top_edge              = $1A66  // Box top edge (pixels)
.label box_bottom_edge           = $1A67  // Box bottom edge (pixels)

// -----------------------------------------------------------------------------
// Nearest-point computation (to test point)
// -----------------------------------------------------------------------------
.label nearest_x                 = $1A60  // Nearest X on or inside box
.label nearest_y                 = $1A61  // Nearest Y on or inside box
.label scaled_x_dist             = $1A5C  // Scaled |dx| = 2*|x - nearest_x|
.label scaled_y_dist             = $1A5D  // Scaled |dy| = |y - nearest_y|/4

// -----------------------------------------------------------------------------
// Test point and selection state
// -----------------------------------------------------------------------------
.label test_x                   = $FE72  // Input test X (pixels)
.label test_y                   = $FE73  // Input test Y (pixels)
.label box_index                = $1A5B  // Running box index (starts at -1)
.label min_distance             = $1A5E  // Best scaled distance so far
.label min_nearest_x            = $1A62  // Nearest X for best candidate
.label min_nearest_y            = $1A63  // Nearest Y for best candidate
.label nearest_box_idx          = $FDAC  // Output: best box index

// -----------------------------------------------------------------------------
// Packed list iteration (generic element list)
// -----------------------------------------------------------------------------
.label elem_list_ptr             = $7C  // ZP: packed list base pointer
.label list_count             	 = $FC3D  // Running count of lists visited
.label target_list_idx           = $CAD3  // Requested list index (0-based)
.label target_ofs                = $CAD7  // Output: byte offset to list start

// -----------------------------------------------------------------------------
// Adjacency list traversal
// -----------------------------------------------------------------------------
.label adj_list_ptr              = $7C  // ZP: adjacency list base pointer
.label dest_box_index            = $CAD1  // Target box index to find
.label list_start_ofs            = $CAD7  // Start offset into adjacency sublist

// -----------------------------------------------------------------------------
// Current/active box id (aliases share $CAD3)
// -----------------------------------------------------------------------------
.label current_box               = $CAD3  // Box id under test

// -----------------------------------------------------------------------------
// Depth-first search state
// -----------------------------------------------------------------------------
.label active_depth_level        = $CAD8  // DFS/BFS depth (1-based when active)
.label actor_discovered_boxes    = $CAD9  // Discovered boxes by depth (index 1..N)
.label scan_limit_index          = $FC3D  // Exclusive upper bound for scans (depth+1)

// -----------------------------------------------------------------------------
// Adjacency scan offsets and resume indices
// -----------------------------------------------------------------------------
.label target_offset             = $CAD7  // Base index for selected adjacency sublist
.label list_offset               = $FC3B  // Working Y-offset into (adj_list_ptr),Y
.label resume_index              = $CAD2  // Last-consumed index for this sublist (resume anchor)
.label resume_index_tbl          = $CB29  // Per-depth resume index table, indexed by X

// -----------------------------------------------------------------------------
// Destination and routine return
// -----------------------------------------------------------------------------
.label destination_box_id        = $CAD1  // Destination walkbox id
.label return_value              = $CAD0  // Routine result code

// -----------------------------------------------------------------------------
// Search depth and advance/backtrack results
// -----------------------------------------------------------------------------
.const MAX_SEARCH_DEPTH                = $10    // Hard cap for DFS depth (1..$10)
.const MIN_SEARCH_DEPTH                = $01    // Minimum valid depth
.const ADVANCE_OK                      = $00    // increment_search_depth: advanced
.const ADVANCE_BLOCKED                 = $FF    // increment_search_depth: at cap
.const BACKTRACK_BLOCKED               = $FF    // decrement_search_depth: at min

// -----------------------------------------------------------------------------
// Per-depth scan bounds and list conventions
// -----------------------------------------------------------------------------
.const BOX_LIST_FIRST                  = $01    // First valid depth index (0 unused)
.const ELEM_TERM                       = $FF    // Terminator byte in adjacency sublists
.const ADJ_PTR_OFFSET_BIAS             = $02    // Bias: box_ptr-2 holds relative ofs

// -----------------------------------------------------------------------------
// Walkbox table layout and markers
// -----------------------------------------------------------------------------
.const OFS_WALKBOX                     = $15    // Room header byte: 8-bit ofs to walkboxes
.const WALKBOX_STRIDE                  = $05    // Bytes per walkbox record
.const WALKBOX_SENTINEL                = $FF    // End-of-table marker in walkbox lists
.const WALKBOX_NOT_RESIDENT            = $FF    // Flag: walkbox data not in memory

// -----------------------------------------------------------------------------
// Geometry tests and distance scaling
// -----------------------------------------------------------------------------
.const INSIDE_BOX                      = $00    // Point classified inside/on box
.const OUTSIDE_BOX                     = $FF    // Point classified outside box
.const INIT_MAX_DIST                   = $FF    // Initial “worst” scaled distance

// -----------------------------------------------------------------------------
// Destination adjacency checks
// -----------------------------------------------------------------------------
.const DEST_IS_CONTAINED               = $00    // Destination found in adjacency set
.const DEST_IS_NOT_CONTAINED           = $FF    // Destination absent from adjacency set

// -----------------------------------------------------------------------------
// Adjacency exploration results (per neighbor)
// -----------------------------------------------------------------------------
.const ONE_UNKNOWN              	   = $00    // At least one neighbor unseen
.const ALL_KNOWN                	   = $FF    // All neighbors already seen

// -----------------------------------------------------------------------------
// Seen/Not-seen test results for a single box
// -----------------------------------------------------------------------------
.const BOX_SEEN                        = $00    // Box present in discovered set
.const BOX_NOT_SEEN                    = $FF    // Box not present in discovered set

// -----------------------------------------------------------------------------
// Path building outcomes and return sentinel
// -----------------------------------------------------------------------------
.const RESULT_PATH_OK                  = $00    // Path found and copied
.const RESULT_PATH_GRAPH_EXHAUSTED     = $01    // Search exhausted via backtrack block
.const RESULT_PATH_MAX_DEPTH           = $02    // Search stopped at depth cap
.const INIT_RETURN_SENTINEL            = $FF    // Initial value for return_value

// -----------------------------------------------------------------------------
// Per-actor path buffer sizing
// -----------------------------------------------------------------------------
.const ACTOR_PATH_BUF_SIZE             = $10    // Bytes per actor path segment


/*
================================================================================
  build_walkbox_path
================================================================================
Summary
    Constructs a path of connected walkboxes between an actor's current and
    destination boxes using a bounded depth-first search with backtracking.

Arguments
    actor                           current actor index

Global Inputs
    box_ptr                     	pointer to actor's walkboxes list
    actor_box_cur					current walkbox for actor
    actor_destination_box			destination walkbox for actor

State
    active_depth_level              1-based recursion depth
    current_box                    	current walkbox being processed
    destination_box_id              goal walkbox id
    adj_list_ptr                    pointer to adjacency list

Returns
    .A  	RESULT_PATH_OK					path found and copied
			RESULT_PATH_GRAPH_EXHAUSTED 	full graph explored, no path  
			RESULT_PATH_MAX_DEPTH 		    search capped by max depth

Description
    - Resolves the actor's walkbox list base address
    - Resets search depth and state variables, then loads the actor's current
      and destination box ids.
    - Executes iterative depth-first traversal:
        • Calls increment_search_depth (bounded to MAX_SEARCH_DEPTH).
        • If destination box is adjacent → write_actor_path_buffer and return PATH_OK.
        • Otherwise checks adjacency lists:
              - If all adjacents known → attempt decrement_search_depth.
                    • If backtrack fails → PATH_GRAPH_EXHAUSTED.
                    • Else continue at previous depth.
              - If not all known → continue scanning.
        • Terminates with PATH_MAX_DEPTH if depth limit reached.
    - The loop continues while .A == $00; each nonzero result exits.
================================================================================
*/
* = $18EB
build_walkbox_path:
        // ------------------------------------------------------------
        // Resolve walkboxes for this costume
        // ------------------------------------------------------------
        ldx     actor
        jsr     get_walkboxes_for_costume

        // Point adj_list_ptr two bytes before walkbox list start
        lda     box_ptr + 1
        sta     adj_list_ptr + 1
        lda     box_ptr
        sec
        sbc     #ADJ_PTR_OFFSET_BIAS
        sta     adj_list_ptr
        bcs     adjust_adj_list_ptr
        dec     adj_list_ptr + 1

adjust_adj_list_ptr:
        // ------------------------------------------------------------
        // Read relative offset and apply it to adj_list_ptr.
        // ------------------------------------------------------------
        ldy     #$00
        clc
        adc     (adj_list_ptr),y
        sta     adj_list_ptr
        bcc     init_search_state
        inc     adj_list_ptr + 1

init_search_state:
        // ------------------------------------------------------------
        // Init search state
        // ------------------------------------------------------------
		// Initial depth := 0
        lda     #$00
        sta     active_depth_level
		
		// Initial return value
        lda     #INIT_RETURN_SENTINEL
        sta     return_value

		// current_box := actor's current box
        lda     actor_box_cur,x
        sta     current_box

		// destination_box_id := actor's destination box
        lda     actor_destination_box,x
        sta     destination_box_id

search_loop:
        // ------------------------------------------------------------
        // Advance search depth; exit if max depth reached.
        // ------------------------------------------------------------
        jsr     increment_search_depth
        cmp     #ADVANCE_OK
        bne     hit_max_depth_exit

        // ------------------------------------------------------------
        // Check if destination box is adjacent
        // ------------------------------------------------------------
        jsr     adjacency_contains_destination
        cmp     #DEST_IS_CONTAINED
        bne     explore_or_backtrack		// If not adjacent, continue search
		
        // ------------------------------------------------------------
		// Destination box is adjacent
		// We have a path to it, so generate it and return success
        // ------------------------------------------------------------
        jsr     write_actor_path_buffer
        lda     #RESULT_PATH_OK
        sta     return_value
        rts
		jmp		dummy_label

explore_or_backtrack:
        // ------------------------------------------------------------
        // Process adjacency exploration or backtrack if exhausted.
		//
		// If all adjacent boxes have not been explored yet, continue exploring
        // ------------------------------------------------------------
        jsr     all_adjacent_boxes_known
        cmp     #ALL_KNOWN
        bne     loop_if_continue

        // All adjacent boxes have been explored → attempt backtrack.
        jsr     decrement_search_depth
        cmp     #BACKTRACK_BLOCKED
        bne     backtrack_continue

        // Backtrack failed → search exhausted.
        lda     #RESULT_PATH_GRAPH_EXHAUSTED
        sta     return_value
        rts

backtrack_continue:
        lda     #$00

loop_if_continue:
        // ------------------------------------------------------------
        // Force re-evaluation of adjacents or proceed scanning.
        // ------------------------------------------------------------
        beq     explore_or_backtrack

dummy_label:
        lda     #$00
        jmp     test_continue_and_dispatch

hit_max_depth_exit:
        // ------------------------------------------------------------
        // Reached max depth limit → terminate search.
        // ------------------------------------------------------------
        lda     #RESULT_PATH_MAX_DEPTH
        sta     return_value

test_continue_and_dispatch:
        // ------------------------------------------------------------
        // Continue loop if A == $00, else return.
        // ------------------------------------------------------------
        beq     search_loop
        rts
/*
================================================================================
  increment_search_depth
================================================================================
Summary
	Advance the search depth by one step up to the maximum allowed level.

Global Inputs
	active_depth_level     		1-based current search depth,
								incremented on success
	current_box                 box id to record at the new depth

Global Outputs
	actor_discovered_boxes[...]   	updated at index = active_depth_level
	resume_index_tbl[...]         	primed to $FF at index = active_depth_level

Returns
	.A  		ADVANCE_BLOCKED			if depth already at MAX_SEARCH_DEPTH
				ADVANCE_OK		        if depth incremented and state updated

Description
	- If active_depth_level equals MAX_SEARCH_DEPTH, do not modify state and
	return ADVANCE_BLOCKED.
	- Otherwise:
		• Increment active_depth_level.
		• Store current_box to actor_discovered_boxes[active_depth_level].
		• Set resume_index_tbl[active_depth_level] := $FF so the next scan
		starts at index $00.
		• Return ADVANCE_OK.
================================================================================
*/
* = $195F
increment_search_depth:
        // ------------------------------------------------------------
        // Guard: if depth at maximum → return ADVANCE_BLOCKED.
        // ------------------------------------------------------------
        lda     active_depth_level
        cmp     #MAX_SEARCH_DEPTH
        bne     perform_depth_increment
        lda     #ADVANCE_BLOCKED
        rts

perform_depth_increment:
        // Increase depth
        inc     active_depth_level

        // Record the box discovered at this depth.
        ldy     active_depth_level
        lda     current_box
        sta     actor_discovered_boxes,y

        // Prime resume index to $FF so first INC yields $00 on next scan.
        lda     #$FF
        sta     resume_index_tbl,y

        // Success.
        lda     #ADVANCE_OK
        rts
/*
================================================================================
  decrement_search_depth
================================================================================
Summary
    Perform one backtrack step on the search depth with underflow guard.

Global Inputs
    active_depth_level          1-based current search depth,
								decremented on success
Returns
    .A  BACKTRACK_BLOCKED		if depth == MIN_SEARCH_DEPTH

Description
    - If active_depth_level equals MIN_SEARCH_DEPTH, do not modify state and
      return BACKTRACK_BLOCKED.
    - Otherwise decrement active_depth_level by one.
================================================================================
*/
* = $197D
decrement_search_depth:
        // ------------------------------------------------------------
        // Prevent underflow: if depth == 1 → return BACKTRACK_BLOCKED.
        // ------------------------------------------------------------
        lda     active_depth_level
        cmp     #MIN_SEARCH_DEPTH
        bne     perform_depth_decrement
        lda     #BACKTRACK_BLOCKED
        rts

perform_depth_decrement:
        // ------------------------------------------------------------
        // Decrement active_depth_level and return new depth.
        // ------------------------------------------------------------
        dec     active_depth_level
        rts
/*
================================================================================
  restore_adjacency_list
================================================================================
Summary
    Restore adjacency scanning so the graph walk can resume exactly where 
	it left off. Selects the target neighbot list for this depth and 
	computes its start offset, honoring prior progress.

Arguments
    active_depth_level    		Depth index into per-depth state.
    actor_discovered_boxes[]   	Per-depth selected list.
    resume_index_tbl[]  		Per-depth resume index within that list.

Returns
    current_box           		List index
    resume_index   				Resume index within the selected list.
    list_start_ofs          	Byte offset to that lists's start.

Description
    • Look up the list index chosen for this depth and store it as
      current_box.
    • Load the last processed index for this depth and use it as the starting
      position when resuming inside that list (supports backtracking).
    • Call get_list_element_offset to translate current_box list index into a byte
      offset and store it in list_start_ofs.
================================================================================
*/
* = $198B
restore_adjacency_list:
        // Resolve discovered box index for the current depth
        ldy     active_depth_level                
        lda     actor_discovered_boxes,y
		// current_box is a list index referencing the list with the neighbors of current_box
        sta     current_box           

		// Resolve resume index for current depth
        lda     resume_index_tbl,y
        sta     resume_index        

        // Translate current_box list index to list offset
        jsr     get_list_element_offset
        rts
/*
================================================================================
  adjacency_contains_destination
================================================================================
Summary
	Check whether the destination box index appears in the current box's
	adjacency sublist. Linear scan to sentinel.

Arguments
	(via restore_adjacency_list)
	list_start_ofs      Start offset within the adjacency sublist.

Returns
	A                   DEST_IS_CONTAINED 			if found
						DEST_IS_NOT_CONTAINED 		if not found (hit terminator)
	Y                   Offset advanced to the byte after the last examined one.

Global Inputs
	adj_list_ptr        ZP pointer to adjacency list base.
	dest_box_index      Target box index to search.
	list_start_ofs      Sublist start offset (produced earlier).

Description
	• Initialize list context, then set Y := list_start_ofs.
	• Loop: read candidate = (adj_list_ptr),Y.
		– If candidate == dest_box_index → return #DEST_IS_CONTAINED.
		– Else increment Y; if candidate == ELEM_TERM → return #DEST_IS_NOT_CONTAINED.
		– Else continue.
================================================================================
*/
* = $199E
adjacency_contains_destination:
        // Initialize adjacency context and load actor (X unused here)
        jsr     restore_adjacency_list
        ldx     actor

        // Y := start of this box's adjacency list
        ldy     list_start_ofs

scan_candidate:
        // Read candidate adjacent box index; compare to destination
        lda     (adj_list_ptr),y
        cmp     dest_box_index
        bne     advance_and_check_eol

        // Match → success
        lda     #DEST_IS_CONTAINED
        rts

advance_and_check_eol:
        iny                                 // advance to next candidate
        cmp     #ELEM_TERM                  // was previous byte the terminator?
        bne     scan_candidate              // no → keep scanning

        // Reached end-of-list without a match
		// A is #$FF from last CMP, A = DEST_IS_NOT_CONTAINED
        rts
/*
================================================================================
  all_adjacent_boxes_known
================================================================================
Summary
    Determines whether every adjacent box for the current list has already been 
	discovered by the actor.

Arguments
    element_index                  selects which adjacency sublist to scan

Returns
    .A  		ONE_UNKNOWN   	at least one adjacent box not yet known
				ALL_KNOWN	    all adjacent boxes already known

Description
    - Calls restore_adjacency_list with X = element_index to load adj_list_ptr
      and sublist_base_ofs.
    - Resumes scanning at iter_ofs := sublist_base_ofs + resume_index.
    - For each box id:
        * Reads (adj_list_ptr),Y.
        * If byte == ELEM_TERM → return ALL_KNOWN.
        * Otherwise sets current_box and calls is_box_seen_by_actor.
          Returns ONE_UNKNOWN on first unseen box, else continues.
    - Updates resume_index_tbl,X before each fetch to maintain continuation
      state across calls.
================================================================================
*/
* = $19B6
all_adjacent_boxes_known:
        // ------------------------------------------------------------
        // Prepare adjacency context for X = active_depth_level.
        // Loads adj_list_ptr and target_offset for element X.
        // ------------------------------------------------------------
        ldx     active_depth_level
        jsr     restore_adjacency_list

        // ------------------------------------------------------------
        // Compute list_offset := target_offset + resume_index; then Y := list_offset.
        // This resumes scanning at the element's previously consumed position.
        // ------------------------------------------------------------
        lda     resume_index
        clc
        adc     target_offset
        tay
        sty     list_offset

scan_next_adjacent:
        // ------------------------------------------------------------
        // Advance resume index for this element, then step list_offset.
        // ------------------------------------------------------------
        inc     resume_index_tbl,x
        inc     list_offset

        // ------------------------------------------------------------
        // Read next adjacent box id from (adj_list_ptr),Y.
        // ------------------------------------------------------------
        ldy     list_offset
        lda     (adj_list_ptr),y

        // ------------------------------------------------------------
        // End-of-list sentinel? If yes → all known.
        // ------------------------------------------------------------
        cmp     #ELEM_TERM
        bne     check_adjacent_seen_state
        lda     #ALL_KNOWN
        rts
		jmp		loop_if_box_seen		//Unreachable code
		
check_adjacent_seen_state:
        // ------------------------------------------------------------
        // Test this adjacent: if unseen → return ONE_UNKNOWN.
        // Otherwise continue scanning.
        // ------------------------------------------------------------
        sta     current_box
        jsr     is_box_seen_by_actor          // returns A=$00 if seen, A=$FF if not
        cmp     #BOX_NOT_SEEN                 // Z=1 when unseen
        bne     loop_if_box_seen           // A=$00 (seen) → keep scanning
        lda     #ONE_UNKNOWN

loop_if_box_seen:
        // ------------------------------------------------------------
        // Seen → loop.
        // ------------------------------------------------------------
        bne     scan_next_adjacent            // always taken here (Z=0 after CMP)
        rts                                   // safety (not reached)
/*
================================================================================
  is_box_seen_by_actor
================================================================================
Summary
    Linear membership test for a box id within the actor's discovered boxes list.

Arguments
    current_box					box id under test
    active_depth_level			current DFS depth for this actor

Returns
    .A  		BOX_SEEN 		if found
				BOX_NOT_SEEN 	if not found

Description
    - Sets scan limit to active_depth_level + 1 (exclusive upper bound).
    - Scans actor_discovered_boxes from index 1 up to and including depth.
    - Early exit on first match; otherwise returns BOX_NOT_SEEN.
================================================================================
*/
* = $19EB
is_box_seen_by_actor:
        // ------------------------------------------------------------
        // Initialize scan limit: scan_limit_index := active_depth_level + 1
        // Y will serve as the loop counter (1..active_depth_level).
        // ------------------------------------------------------------
        ldy     active_depth_level
        iny
        sty     scan_limit_index

        // ------------------------------------------------------------
        // Start linear scan at index 1; index 0 is intentionally skipped.
        // ------------------------------------------------------------
        ldy     #BOX_LIST_FIRST

scan_discovered_boxes:
        // ------------------------------------------------------------
        // Compare list[Y] with current_box; match → return BOX_SEEN.
        // ------------------------------------------------------------
        lda     actor_discovered_boxes,y
        cmp     current_box
        bne     step_and_check_limit
        lda     #BOX_SEEN
        rts

step_and_check_limit:
        // ------------------------------------------------------------
        // Advance Y; if Y != scan_limit_index keep scanning. Otherwise not found.
        // ------------------------------------------------------------
        iny
        cpy     scan_limit_index
        bne     scan_discovered_boxes

        // ------------------------------------------------------------
        // No match in 1..active_depth_level → return BOX_NOT_SEEN.
        // ------------------------------------------------------------
        lda     #BOX_NOT_SEEN
        rts
/*
================================================================================
  write_actor_path_buffer
================================================================================
Summary
    Serialize the found walkbox route for the given actor into the per-actor
    path buffer, starting with the destination box and then the reverse chain
    from the current depth down to depth 1.

Arguments
    actor							current actor index

Global Inputs
    active_depth_level             	1-based depth of the discovered route
    destination_box_id             	goal walkbox id
    actor_discovered_boxes[...]    	source stack indexed by depth (1..N)

Global Outputs
    actor_search_depth[...]        	per-actor path length in steps (depth-1)
    actor_discovered_boxes[...]    	per-actor output segment filled with
									{destination, depth..2 entries}

Description
    - Compute Y := 16 * (actor + 1) to address the actor's output segment.
    - Store path length as (active_depth_level - 1) into actor_search_depth[X].
    - Write destination_box_id at the start of the actor's segment.
    - If depth == MIN_SEARCH_DEPTH, exit.
    - Otherwise copy entries from actor_discovered_boxes[depth..2] into the
      actor's segment in reverse order to reconstruct the path.
================================================================================
*/
* = $1A08
write_actor_path_buffer:
        // ------------------------------------------------------------
        // Y := 16 * (actor + 1) → select actor's output segment base.
        // ------------------------------------------------------------
        ldx     actor
        txa
        asl     
        asl     
        asl     
        asl     
        clc
        adc     #ACTOR_PATH_BUF_SIZE
        tay

        // ------------------------------------------------------------
        // Save path length in steps: depth - 1.
        // ------------------------------------------------------------
        lda     active_depth_level
        sta     actor_search_depth,x
        dec     actor_search_depth,x

        // ------------------------------------------------------------
        // Write destination box as first element of the actor's path.
        // ------------------------------------------------------------
        lda     destination_box_id
        sta     actor_discovered_boxes,y
        iny

        // ------------------------------------------------------------
        // If depth == 1, we are already at destination → done.
        // ------------------------------------------------------------
        ldx     active_depth_level
        cpx     #MIN_SEARCH_DEPTH
        beq     end_copy_path

copy_path_reverse_loop:
        // ------------------------------------------------------------
        // Copy source stack in reverse (depth..2) into actor buffer.
        // ------------------------------------------------------------
        lda     actor_discovered_boxes,x
        sta     actor_discovered_boxes,y
        iny
        dex
        cpx     #MIN_SEARCH_DEPTH
        bne     copy_path_reverse_loop

end_copy_path:
        rts

/*
================================================================================
  get_list_element_offset
================================================================================
Summary
	Return the byte offset (from list start) of the target list in a list-of-lists
	where each list is terminated by ELEM_TERM.

Arguments
	target_list_idx        	0-based index of the desired list.

Returns
	target_ofs          	Offset (0..255) to the first byte of the target list.

Description
	• If target_list_idx == 0, the offset is 0.
	• Otherwise scan bytes starting at (elem_list_ptr)+Y. Each ELEM_TERM ends one
	list and increments list_count. When list_count equals
	target_list_idx, Y points at the start of the target list; store Y.
	• No bounds check is performed for indexes past the end of the list.
================================================================================
*/
* = $1A37
get_list_element_offset:
        // ------------------------------------------------------------
        // Initialize: Y=0 scan offset; list_count=0
        // ------------------------------------------------------------
        ldy     #$00
        sty     list_count

        // ------------------------------------------------------------
        // Fast path: index 0 → offset 0
        // ------------------------------------------------------------
        lda     target_list_idx
        bne     scan_next_byte
        sta     target_ofs                 // A holds 0 here
        rts

scan_next_byte:
        // ------------------------------------------------------------
        // Read next byte; advance Y
        // ------------------------------------------------------------
        lda     (elem_list_ptr),y
        iny

        // ------------------------------------------------------------
        // If byte != terminator, still inside current list → keep scanning
        // If byte == terminator, we just ended one list → bump count
        // ------------------------------------------------------------
        cmp     #ELEM_TERM
        bne     scan_next_byte

        inc     list_count                 // ended one list

        // ------------------------------------------------------------
        // Reached the requested list count?
        //   When equal: Y now points to start of target list
        // ------------------------------------------------------------
        lda     list_count
        cmp     target_list_idx
        bne     scan_next_byte

        sty     target_ofs                 // commit offset of target list
        rts
/*
================================================================================
  get_nearest_box
================================================================================
Summary
	Iterate through all walkboxes of a room and determine which one is closest
	to a given coordinate pair (box_check_x, box_check_y). 
	
	Updates global variables with the index of the nearest box, the nearest point 
	on its edges, and the minimum computed distance. 
	
	Returns failure if no boxes are present.

Arguments
	walkbox_room        Room index whose walkboxes are being scanned.
	box_check_x         X coordinate of the test point.
	box_check_y         Y coordinate of the test point.

Returns
	On success:
		nearest_box_idx     → index of nearest walkbox
		min_nearest_x       → nearest X on or inside that box
		min_nearest_y       → nearest Y on or inside that box
		min_distance        → anisotropic distance value
		
	On failure (no room data loaded):
		nearest_box_idx   = #$FF
		A                 = #$FF

Description
	• Calls get_walkboxes_for_room to retrieve the base pointer to the room's
	walkbox list; aborts if the room is not resident.
	• Each walkbox entry consists of 6 bytes:
		[left, right, top, bottom, byte4, byte5]
	and the list terminates with #$FF at the next entry's start.
	• For every box:
		– Loads its edges into box_left_edge..box_bottom_edge.
		– Calls get_distance_to_box to compute both the distance (A) and
		the nearest point (nearest_x, nearest_y).
		– If the distance is less than min_distance, records this box as
	the new closest.
	• Continues until a WALKBOX_SENTINEL sentinel is encountered.
================================================================================
*/
* = $1AC1
get_nearest_box:
        // ------------------------------------------------------------
        // Acquire walkbox table pointer for the requested room
        //   A = #$FF on failure (room not resident)
        // ------------------------------------------------------------
        jsr     get_walkboxes_for_room
		
		//Walkboxes present? If so, continue
        cmp     #WALKBOX_SENTINEL		
        bne     init_scan
		
		//Walkboxes not present, exit with error code
        sta     nearest_box_idx            	// propagate #$FF to output index
        rts

init_scan:
        // ------------------------------------------------------------
        // Initialize search state
        //   min_distance := $FF (worst), box_index := $FF then pre-increment
        // ------------------------------------------------------------
        lda     #INIT_MAX_DIST
        sta     min_distance
        lda     #$ff
        sta     box_index

        ldy     #$00                     	// Y := table offset
scan_next_box:
        inc     box_index                   // next logical record index

        // ------------------------------------------------------------
        // End-of-list check: leading byte == WALKBOX_SENTINEL means no more records
        // ------------------------------------------------------------
        lda     (box_ptr),y
        cmp     #WALKBOX_SENTINEL
        bne     load_current_box
        rts

load_current_box:
        // ------------------------------------------------------------
        // Load current box edges: [left, right, top, bottom]
        // Then skip the next two to land at next record start
        // ------------------------------------------------------------
        lda     (box_ptr),y
        sta     box_left_edge
        iny
        lda     (box_ptr),y
        sta     box_right_edge
        iny
        lda     (box_ptr),y
        sta     box_top_edge
        iny
        lda     (box_ptr),y
        sta     box_bottom_edge
        iny
        iny                                 // skip two extra bytes (stride = 6)

        // ------------------------------------------------------------
        // Compute distance to this box and the nearest point on it
        //   On return: A = distance, nearest_x/nearest_y filled
        // ------------------------------------------------------------
        jsr     get_distance_to_box

        // ------------------------------------------------------------
        // If this distance is strictly smaller, capture as new minimum
        // ------------------------------------------------------------
        cmp     min_distance
        bcs     continue_scan               // A >= min_distance → keep current best

		// New minimum, save box index and nearest coordinates
        sta     min_distance
        lda     box_index
        sta     nearest_box_idx
        lda     nearest_x
        sta     min_nearest_x
        lda     nearest_y
        sta     min_nearest_y

continue_scan:
        jmp     scan_next_box
/*
================================================================================
  get_distance_to_box
================================================================================
Summary
	Compute an anisotropic distance from a test point to an axis-aligned box and
	record the nearest point on or in that box. Inside the box returns zero.

Arguments
	test_x                  Test X coordinate
	test_y                  Test Y coordinate
	box_left_edge           Box left edge
	box_right_edge          Box right edge
	box_top_edge            Box top edge
	box_bottom_edge         Box bottom edge

Returns
	A                       Distance byte
	nearest_x               Nearest X on/inside the box
	nearest_y               Nearest Y on/inside the box

Vars/State
	scaled_x_dist           2*|test_x − nearest_x|
	scaled_y_dist           |test_y − nearest_y|/4

Description
	• Clamp test_x to [box_left_edge, box_right_edge] → nearest_x.
	• Clamp test_y to [box_top_edge, box_bottom_edge] → nearest_y.
	• Compute |dx| and |dy|, then scale: X by ×2, Y by ÷4.
	• If scaled_x_dist > scaled_y_dist: return scaled_x_dist/2 + scaled_y_dist;
	otherwise return scaled_y_dist/2 + scaled_x_dist.
================================================================================

Distance approximation formula

This distance formulation is a fast integer approximation of a Euclidean metric.

A true Euclidean distance requires a square root and multiplications:
	d = sqrt(dx² + dy²) which are costly on 6502 hardware.

Instead, this routine blends the absolute deltas with a fixed ratio:


if |dx| > |dy|:
    d ≈ |dx|/2 + |dy|
else:
    d ≈ |dy|/2 + |dx|


After axis scaling (×2 for X, ÷4 for Y) this produces:
	d ≈ 0.5*max(|dx|,|dy|) + min(|dx|,|dy|)

This hybrid between the Manhattan (L1) and Chebyshev (L∞) metrics forms an
'octagonal' distance contour, closely approximating circular Euclidean ranges.

The blend 0.5*max + min is a midpoint between L1 and L∞ and an inexpensive way 
to get contours roughly circular without using square roots or multiplications.
==================================================================
*/
* = $1B1B
get_distance_to_box:
        // ------------------------------------------------------------
        // Resolve nearest X coordinate relative to box edges
        //   • If test_x > right  → clamp to right edge.
        //   • If test_x < left   → clamp to left edge.
        //   • Otherwise          → point is inside horizontally.
        // ------------------------------------------------------------
        lda     test_x                        // load X coordinate to test
        cmp     box_right_edge                // compare to right edge
        bcc     x_within_or_left_of_right     // if ≤ right, continue testing
        beq     x_within_or_left_of_right
		
        lda     box_right_edge                // test_x > right → clamp to right
        jmp     commit_nearest_x              // done with X side

x_within_or_left_of_right:
        cmp     box_left_edge                 // compare to left edge
        bcs     commit_nearest_x              // if ≥ left → inside; keep original
		
        lda     box_left_edge                 // test_x < left → clamp to left
commit_nearest_x:
        sta     nearest_x                     // store resolved nearest X

        // ------------------------------------------------------------
        // Resolve nearest Y relative to box edges
        //   If test_y > bottom → clamp to bottom
        //   If test_y < top    → clamp to top
        //   Else                → inside vertically, keep test_y
        // ------------------------------------------------------------
        lda     test_y                        // A := test_y
        cmp     box_bottom_edge               // A ? bottom
        bcc     y_within_or_above_bottom      // A <  bottom → maybe above; check top
        beq     y_within_or_above_bottom      // A == bottom → inside so far
		
        lda     box_bottom_edge               // A > bottom → clamp to bottom
        jmp     commit_nearest_y

y_within_or_above_bottom:
        cmp     box_top_edge                  // A ? top
        bcs     commit_nearest_y              // A >= top → inside vertically
		
        lda     box_top_edge                  // A <  top → clamp to top
commit_nearest_y:
        sta     nearest_y                     // nearest_y := resolved Y

		// ------------------------------------------------------------
        // Compute |dx| then store scaled_x_dist = 2*|dx|
		// ------------------------------------------------------------
        lda     test_x                        // A := test_x
        sec                                   // prepare subtraction
        sbc     nearest_x                     // A := test_x - nearest_x
        bcs     set_x_distance                // if A ≥ 0 keep magnitude
        eor     #$ff                          // two's complement negate
        clc
        adc     #$01

set_x_distance:
        asl                                   // A := 2*|dx|
        sta     scaled_x_dist                 // save scaled X distance

		// ------------------------------------------------------------
		// Compute |dy| then scale to scaled_y_dist = |dy|/4
		// ------------------------------------------------------------
        lda     test_y                        // A := test_y
        sec                                   // prepare subtraction
        sbc     nearest_y                     // A := test_y - nearest_y
        bcs     set_y_distance                // if A ≥ 0 keep magnitude
        eor     #$ff                          // two's complement negate
        clc
        adc     #$01

set_y_distance:
        lsr                                   // ÷2
        lsr                                   // ÷4 total
        sta     scaled_y_dist                 // save scaled Y distance

		// ------------------------------------------------------------
		// Combine scaled components
		//   If scaled_x_dist > scaled_y_dist:
		//       return scaled_x_dist/2 + scaled_y_dist
		//   else:
		//       return scaled_y_dist/2 + scaled_x_dist
		// ------------------------------------------------------------
        cmp     scaled_x_dist                 // compare scaled_y_dist (A) vs scaled_x_dist
        bcc     y_dist_le_x_dist              // if y < x  → use y/2 + x
        beq     y_dist_le_x_dist              // if y == x → same path (y/2 + x)

        lda     scaled_x_dist                 // y > x → use x/2 + y
        lsr                                   // x/2
        clc
        adc     scaled_y_dist				  // + y
        jmp     return_distance

y_dist_le_x_dist:
        lda     scaled_y_dist                 // y ≤ x → use y/2 + x
        lsr                                   // y/2
        clc
        adc     scaled_x_dist				  // + x

return_distance:
		rts
/*
==============================================================================
  get_walkboxes_for_room
==============================================================================
Summary
	Return a pointer to the room's walkbox table for a given room index. If the
	room resource is not resident, return a failure code.

Arguments
	walkbox_room       		Room index to query.
	room_ptr_lo_tbl[]      	Base pointer LO bytes for rooms.
	room_ptr_hi_tbl[]      	Base pointer HI bytes for rooms.

Returns
	On success: 
		A = #$00
		box_ptr → walkbox table
		
	On failure: 
		A = WALKBOX_NOT_RESIDENT

Description
	• Test room residency by checking room_ptr_hi_tbl[walkbox_room].
	• If not resident, return failure immediately.
	• If resident, tail-call get_walkboxes_ptr to compute box_ptr from the
	room base plus the walkbox offset stored in the room block.
================================================================================
*/
* = $2DA3
get_walkboxes_for_room:
		//Resolve room's high byte
        ldy     walkbox_room                
        lda     room_ptr_hi_tbl,y           

		//Room resident? If so, continue to get_walkboxes_ptr routine
        bne     ptr_present                 

		//Room not resident - return failure
        lda     #WALKBOX_NOT_RESIDENT       
        rts                                 
ptr_present:
        jmp     get_walkboxes_ptr           
/*
================================================================================
  get_walkboxes_for_costume
================================================================================
Summary
	Return a pointer to the walkbox table for the room currently occupied by the
	active costume's actor. If that room's resource is not resident, return a
	failure code.

Arguments
	active_costume      		Costume index for the actor in focus.
	costume_room_idx[]       	Map: costume index → current room index.
	room_ptr_lo_tbl[]        	Base pointer LO bytes for rooms.
	room_ptr_hi_tbl[]        	Base pointer HI bytes for rooms.

Returns
	On success: 
		A = #$00
		box_ptr → walkbox table
		
	On failure: 
		A = WALKBOX_NOT_RESIDENT

Description
	• Retrieve the actor's current room index from costume_room_idx[active_costume].
	• Check if the room resource is resident via its high-byte entry.
	• If not resident, return failure immediately.
	• If resident, call get_walkboxes_ptr to compute box_ptr for that room.
================================================================================
*/
* = $2DB1
get_walkboxes_for_costume:
		// Resolve active costume's room
        ldy     active_costume              
        lda     costume_room_idx,y          
		
		// Resolve room's base hi
        tay                                 
        lda     room_ptr_hi_tbl,y           
		
		//Room resident? If so, continue to get_walkboxes_ptr routine
        bne     get_walkboxes_ptr           
		
		//Room not resident - return failure
        lda     #WALKBOX_NOT_RESIDENT       
        rts                                 
/*
==============================================================================
  get_walkboxes_ptr
==============================================================================
Summary
	Compute an absolute pointer to the room's walkbox table.

Arguments
	Y    			Room index.
	A    			Room base high byte (room_ptr_hi_tbl[Y]).

Returns
	A 				#$00
	box_ptr         ZP pointer to walkbox table (lo/hi).

Global Inputs
	room_ptr_lo_tbl[]    Low bytes of room base pointers.

Description
	• Seed box_ptr with the room base (HI from A, LO from room_ptr_lo_tbl[Y]).
	• Read the 8-bit walkbox offset at base + OFS_WALKBOX.
	• Add the offset to the base pointer; on carry, increment the high byte.
==============================================================================
*/
* = $2DC0
get_walkboxes_ptr:
        // ------------------------------------------------------------
		// Resolve pointer to room base
        // ------------------------------------------------------------
        sta     box_ptr + 1         // Set HI of box_ptr from A (room base HI; Y=room index)
        lda     room_ptr_lo_tbl,y   // Load LO byte of room base from table[Y]
        sta     box_ptr             // Set LO of box_ptr to complete base pointer

        // ------------------------------------------------------------
		// Fetch 8-bit walkbox offset from room metadata
        // ------------------------------------------------------------
        ldy     #OFS_WALKBOX        // Y := offset of "walkbox offset" within block
        lda     (box_ptr),y         // A := 8-bit walkbox offset
		
        // ------------------------------------------------------------
		// Add walkbox offset to room base to compute absolute walkbox address
        // ------------------------------------------------------------
        clc                         
        adc     box_ptr             // add offset to LO; C=1 if wrap occurred
        sta     box_ptr             // commit updated LO
        bcc     exit_gwp            // no carry → HI unchanged
        inc     box_ptr + 1         // carry → increment HI to complete pointer
		
exit_gwp:
        lda     #$00                
        rts                    		
/*
================================================================================
  is_actor_pos_inside_box
================================================================================
Summary
	Determine whether an actor's position lies within a rectangular box. 
	The box is described by four consecutive bytes: [left, right, top, bottom]. 
	All boundaries are inclusive.

Arguments
	X   			Actor index.
	Y   			Offset to box data (relative to box_ptr).
	box_ptr  		pointer to the start of the box table.
	actor_x_pos[]  Actor horizontal positions.
	actor_y_pos[]  Actor vertical positions.

Returns
	A   			INSIDE_BOX 		if the actor is inside the box.
					OUTSIDE_BOX 	if the actor is outside the box.

Description
	• Compares the actor's X coordinate against the left and right box edges.
	Fails immediately if outside those limits.
	• Compares the actor's Y coordinate against the top and bottom edges.
	Fails if outside the vertical bounds.
	• If all comparisons pass, returns INSIDE_BOX. Otherwise, OUTSIDE_BOX.
================================================================================
*/
* = $30AD
is_actor_pos_inside_box:
        // ------------------------------------------------------------
        // X-range check: ensure left ≤ pos_x ≤ right
        //
		// Reads left edge at (box_ptr)+Y, then right edge at (box_ptr)+Y+1.
        // Early-out if outside on either edge test.
        // ------------------------------------------------------------
        lda     actor_x_pos,x
        cmp     (box_ptr),y                  // pos_x ? left
        bcc     return_outside               // pos_x < left → outside
		
        iny
        cmp     (box_ptr),y                  // pos_x ? right
        beq     check_y                      // equal is inside so far
        bcs     return_outside               // pos_x > right → outside

check_y:
        // ------------------------------------------------------------
        // Y-range check: ensure top ≤ pos_y ≤ bottom
		//
        // Reads top edge at (box_ptr)+Y+2, then bottom edge at (box_ptr)+Y+3.
        // Early-out if outside
        // ------------------------------------------------------------
        iny
        lda     actor_y_pos,x
        cmp     (box_ptr),y                  // pos_y ? top
        bcc     return_outside               // pos_y < top → outside
		
        iny
        cmp     (box_ptr),y                  // pos_y ? bottom
        beq     return_inside                // equal counts as inside
        bcs     return_outside               // pos_y > bottom → outside

return_inside:
        lda     #INSIDE_BOX
        rts

return_outside:
        lda     #OUTSIDE_BOX
        rts
/*
================================================================================
  get_walkbox_offset
================================================================================
Summary
	Compute the byte offset into the walkbox table for a given walkbox index.
	Each walkbox entry occupies 5 bytes, so the offset = index * 5.

Arguments
	A   			Walkbox index

Returns
	Y   			Offset for walkbox (index * 5)

Description
	• Saves the input index to wbox_idx for reuse.
	• Multiplies A by 4 using two left shifts.
	• Adds the saved original index to form (index*4 + index).
	• Transfers the low byte of the result to Y for table addressing.
	• Handles only 8-bit arithmetic; high byte is discarded.
================================================================================
*/
* = $30D0
get_walkbox_offset:
		// Save original walkbox index for later
        sta     wbox_idx                        
		
		// Multiply by 4 
        asl                                     
        asl                                     
		
		// Add original index again: 4 * index + 1 * index = 5 * index
        clc                                     
        adc     wbox_idx                        
		
		// Copy result to Y for table offset
        tay                                     
        rts                                     


/*
Pseudo-code

function build_walkbox_path(actorId) -> ResultCode:
    // 1. Resolve walkbox data and adjacency base for this actor
    status = get_walkboxes_for_costume()
    // (If status == WALKBOX_NOT_RESIDENT, caller may treat as failure.)

    // Compute the base pointer/index into the adjacency blob for this room.
    // Conceptually: adjacencyBase = (walkboxTableBase - 2) + relativeOffset
    adjacencyBase = computeAdjacencyBaseFromWalkboxTable()

    // 2. Initialize DFS state
    activeDepthLevel  = 0
    returnValue       = null
    currentBox        = actorCurrentBox[actorId]
    destinationBoxId  = actorDestinationBox[actorId]

    // 3. Depth-first search loop
    loop forever:
        // Try to descend one level in DFS tree
        if increment_search_depth() == ADVANCE_BLOCKED:
            // Cannot go deeper: hit configured depth cap
            return RESULT_PATH_MAX_DEPTH

        // Check if the destination appears in currentBox’s adjacency list
        if adjacency_contains_destination() == true:
            write_actor_path_buffer(actorId)
            return RESULT_PATH_OK

        // Otherwise, explore neighbors or backtrack
        if all_adjacent_boxes_known() == true:
            // All neighbors of the current node have been processed
            if decrement_search_depth() == BACKTRACK_BLOCKED:
                // No nodes left to backtrack to: graph fully explored
                return RESULT_PATH_GRAPH_EXHAUSTED
            // Backtracked; outer logic (caller/system) may update currentBox before next iteration
        else:
            // There exists at least one unseen neighbor; outer logic will
            // update currentBox and re-enter this loop
            pass

        // Loop again and continue DFS


function increment_search_depth() -> Status:
    if activeDepthLevel == MAX_SEARCH_DEPTH:
        return ADVANCE_BLOCKED

    activeDepthLevel += 1
    depth = activeDepthLevel

    // Record which box is at this depth in the DFS path
    actorDiscoveredBoxes[depth] = currentBox

    // Reset resume index so that, at this depth, neighbor scanning starts at 0
    resumeIndexTable[depth] = -1   // “none processed yet”; scanning logic will increment first

    return ADVANCE_OK


function decrement_search_depth() -> StatusOrDepth:
    if activeDepthLevel == MIN_SEARCH_DEPTH:
        return BACKTRACK_BLOCKED

    activeDepthLevel -= 1
    return activeDepthLevel   // Only used to differentiate from BACKTRACK_BLOCKED


function restore_adjacency_list():
    // Use current depth to recover which box we are exploring
    depth = activeDepthLevel

    // Which box is associated with this depth?
    listIndex = actorDiscoveredBoxes[depth]
    currentBox = listIndex

    // Where did we stop scanning this box’s adjacency last time?
    resumeIndex = resumeIndexTable[depth]

    // Translate “box index” -> “byte/element offset” into packed adjacency blob
    // Conceptually: find Nth sublist in a list-of-lists structure
    listStartOffset = get_list_element_offset(listIndex)

    // Now we know:
    //   - currentBox: which node we’re exploring
    //   - resumeIndex: last neighbor index processed
    //   - listStartOffset: where its adjacency sublist starts in adjacencyBase


function adjacency_contains_destination() -> bool:
    // Prepare context for current depth
    restore_adjacency_list()

    // Scan the adjacency sublist of currentBox from the beginning
    neighborIndex = 0

    loop:
        candidate = readAdjacencyEntry(currentBox, neighborIndex)

        if candidate == TERMINATOR:
            // No more neighbors
            return false

        if candidate == destinationBoxId:
            return true

        neighborIndex += 1
        continue loop


function all_adjacent_boxes_known() -> bool:
    // Prepare adjacency context for the current depth
    depth = activeDepthLevel
    restore_adjacency_list()

    // Start scanning from neighbor after the one we last processed
    neighborIndex = resumeIndex + 1

    loop:
        // Fetch this neighbor
        neighbor = readAdjacencyEntry(currentBox, neighborIndex)

        // If we hit terminator, there are no more neighbors
        if neighbor == TERMINATOR:
            // All neighbors have been examined; nothing new to discover
            return true

        // Update resume index for this depth so future calls know where to resume
        resumeIndexTable[depth] = neighborIndex

        // Check if this neighbor has already been discovered along current DFS path
        currentBox = neighbor
        if is_box_seen_by_actor() == BOX_NOT_SEEN:
            // Found at least one yet-unseen neighbor
            return false

        // This neighbor is already seen; move to the next
        neighborIndex += 1
        continue loop


function is_box_seen_by_actor() -> SeenFlag:
    // Scan the discovered boxes for current DFS path from depth 1..activeDepthLevel
    target = currentBox
    depthLimit = activeDepthLevel

    for depth from 1 to depthLimit:
        if actorDiscoveredBoxes[depth] == target:
            return BOX_SEEN

    return BOX_NOT_SEEN


function write_actor_path_buffer(actorId):
    depth = activeDepthLevel

    // Path length is (number of boxes along chain minus 1)
    pathLength = depth - 1
    actorSearchDepth[actorId] = pathLength

    // Each actor has a dedicated contiguous path segment
    baseIndex = pathSegmentBaseForActor(actorId)  // e.g., ACTOR_PATH_BUF_SIZE * (actorId + 1)
    writeIndex = baseIndex

    // First element is always the destination box
    actorDiscoveredBoxes[writeIndex] = destinationBoxId
    writeIndex += 1

    // If depth == 1, we started in the destination box; nothing more to write
    if depth == MIN_SEARCH_DEPTH:
        return

    // Otherwise, copy the chain of boxes from deepest depth down to 2
    // This encodes the path in reverse order after the destination
    for d from depth down to 2:
        actorDiscoveredBoxes[writeIndex] = actorDiscoveredBoxes[d]
        writeIndex += 1


function get_list_element_offset(targetListIndex) -> int:
    // adjacencyBase represents a packed list-of-lists, each sublist terminated by TERMINATOR.
    // We want the starting offset of sublist #targetListIndex (0-based).

    // Sublist 0 starts at offset 0.
    if targetListIndex == 0:
        return 0

    offset = 0
    listCount = 0

    loop:
        value = adjacencyBase[offset]
        offset += 1

        if value == TERMINATOR:
            // End of one sublist
            listCount += 1

            if listCount == targetListIndex:
                // We just finished sublist (targetListIndex-1);
                // offset now points to the first byte of sublist targetListIndex
                return offset

        // Else still iterating inside the current sublist; keep going
        continue loop


function get_nearest_box(roomId, pointX, pointY) -> (status, nearestBoxIndex, nearestPointX, nearestPointY, bestDistance):
    // 1. Resolve walkbox table for room
    walkboxStatus = get_walkboxes_for_room(roomId)
    if walkboxStatus == WALKBOX_NOT_RESIDENT:
        return (WALKBOX_NOT_RESIDENT, -1, null, null, null)

    // 2. Initialize search
    bestDistance     = +∞
    nearestBoxIndex  = -1
    nearestPointX    = null
    nearestPointY    = null

    boxIndex = 0

    // Iterate all boxes until a sentinel marks the end
    while walkboxExists(roomId, boxIndex):
        box = getWalkbox(roomId, boxIndex)   // { left, right, top, bottom, ... }

        // Compute distance and nearest point on/in this box
        distance, px, py = get_distance_to_box(pointX, pointY, box)

        if distance < bestDistance:
            bestDistance    = distance
            nearestBoxIndex = boxIndex
            nearestPointX   = px
            nearestPointY   = py

        boxIndex += 1

    // On success, bestDistance will be finite and nearestBoxIndex >= 0
    return (0, nearestBoxIndex, nearestPointX, nearestPointY, bestDistance)


function get_distance_to_box(pointX, pointY, box) -> (distance, nearestX, nearestY):
    left   = box.left
    right  = box.right
    top    = box.top
    bottom = box.bottom

    // 1. Clamp point to box horizontally
    if pointX < left:
        nearestX = left
    else if pointX > right:
        nearestX = right
    else:
        nearestX = pointX

    // 2. Clamp point to box vertically
    if pointY < top:
        nearestY = top
    else if pointY > bottom:
        nearestY = bottom
    else:
        nearestY = pointY

    // 3. Compute absolute deltas
    dx = abs(pointX - nearestX)
    dy = abs(pointY - nearestY)

    // 4. Apply anisotropic scaling (horizontal emphasized, vertical de-emphasized)
    scaledX = 2 * dx       // horizontal cost is doubled
    scaledY = dy / 4       // vertical cost is quartered

    // 5. Blend into an “octagonal” distance approximation
    //    d ≈ 0.5 * max(scaledX, scaledY) + min(scaledX, scaledY)
    if scaledY > scaledX:
        distance = (scaledX / 2) + scaledY
    else:
        distance = (scaledY / 2) + scaledX

    return (distance, nearestX, nearestY)


function get_walkboxes_for_room(roomId) -> Status:
    // Check if the room’s walkbox data is loaded
    if roomWalkboxBase[roomId] is not loaded:
        return WALKBOX_NOT_RESIDENT

    // If loaded, compute the address/index of the walkbox table for this room
    get_walkboxes_ptr(roomId)

    return 0   // success


function get_walkboxes_for_costume() -> Status:
    // Map active costume → room
    costumeId = activeCostume
    roomId    = costume_room_idx[costumeId]

    // Check if that room’s walkbox data is loaded
    if roomWalkboxBase[roomId] is not loaded:
        return WALKBOX_NOT_RESIDENT

    // If loaded, compute the address/index of the walkbox table
    get_walkboxes_ptr(roomId)

    return 0


function get_walkboxes_ptr(roomId):
    // Conceptually: roomWalkboxBase[roomId] is the base of the room block.
    // We read an 8-bit offset that tells us where the walkbox table begins
    // relative to that base.

    baseAddress   = roomWalkboxBase[roomId]
    relativeOfs   = readByte(baseAddress + OFS_WALKBOX)  // walkbox offset byte
    walkboxPtr    = baseAddress + relativeOfs

    // Store result for later calls
    box_ptr = walkboxPtr


function is_actor_pos_inside_box(actorId, box) -> bool:
    posX = actor_x_pos[actorId]
    posY = actor_y_pos[actorId]

    left   = box.left
    right  = box.right
    top    = box.top
    bottom = box.bottom

    // Inclusive rectangle test
    if posX < left:   return false
    if posX > right:  return false
    if posY < top:    return false
    if posY > bottom: return false

    return true


function get_walkbox_offset(index) -> int:
    // Each walkbox record occupies a fixed stride of 5 units in some table.
    // Return the starting offset for walkbox 'index'.
    return index * 5
