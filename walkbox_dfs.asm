/*
================================================================================
  Walkbox Pathfinding Core
================================================================================

Overview
	Implements the depth-first search (DFS) layer that walks a room’s walkbox
	adjacency graph to build per-actor routes. Uses a compact packed-list
	representation for adjacency and a small per-depth state block to support
	bounded search with backtracking and cycle avoidance.
	
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
	
Responsibilities
	- Resolve adjacency list base for the current room from the walkbox table.
	- Maintain DFS state:
		- active_depth_level: current depth (1-based when active).
		- actor_discovered_boxes[depth]: which box is at each depth in the path.
		- resume_index_tbl[depth]: where scanning last stopped in each adjacency
		list.
	- Drive the DFS loop:
		- Try to advance one level (increment_search_depth).
		- Check whether the destination box is directly adjacent.
		- If not, decide whether to keep exploring neighbors or backtrack.
		- Terminate on success, depth cap, or graph exhaustion.
	- Serialize the found walkbox path into a fixed-size per-actor segment.

Data layout
	- Adjacency lists:
		- Stored as a “list of lists” blob, addressed via a single base pointer.
		- Each sublist corresponds to one walkbox and is terminated by $FF.
		- Sublist 0 starts at offset 0; later lists are located by counting
		terminators.
	- Per-depth DFS state:
		- active_depth_level: depth of current node.
		- actor_discovered_boxes[1..N]: stack of boxes along the search path.
		- resume_index_tbl[1..N]: last-consumed neighbor index for each box.
	- Per-actor path buffer:
		- Fixed-size segment per actor (same base array as the DFS stack).
		- On success, stores {destination box, then reverse chain of boxes} and
		a separate length field (number of steps).

Key routines
	find_path_between_walkboxes
		- Entry point: orchestrates the bounded DFS for a single actor.
		- Resolves adjacency base, sets up state, and runs the search loop.
		- Returns a result code for success, depth cap, or full graph exhaustion.

	increment_search_depth / decrement_search_depth
		- Adjust active_depth_level with guards for minimum and maximum depth.
		- On increment, record current_box at the new depth and reset the
		per-depth resume index.

	restore_adjacency_list
		- For the current depth, recovers which box is being explored, the
		last resume index, and the start offset of its adjacency sublist.

	is_box_in_list
		- Linear scan of a box’s adjacency sublist to see whether it contains
		the destination box id.

	are_all_adjacents_discovered
		- Resumable scanner over one adjacency sublist using resume_index_tbl.
		- Returns whether all neighbors have already been seen, or exposes the
		next unseen neighbor by setting current_box.

	is_box_discovered
		- Membership test for current_box within the discovered stack
		(depth range 1..active_depth_level).

	serialize_box_path
		- Converts the DFS depth stack into a compact per-actor path segment
		(destination + reverse chain), and stores the path length.

	get_list_start_offset
		- Generic helper that maps a 0-based list index to its starting offset
		inside a packed, terminator-delimited list-of-lists blob.

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
   are_all_adjacents_discovered?
     - resume_index_tbl[1] := $00  (first INC)
     - Next candidate = adj(0)[0] = 1 → is_box_discovered? (scan 1..1) → not seen.
     - Return ONE_UNKNOWN → continue exploration at depth 1 (caller loop).

3) Choose neighbor 1 (conceptually “descend” to 1)
   current_box := 1

   increment_search_depth
   depth := 2
   actor_discovered_boxes[2] := 1
   resume_index_tbl[2] := $FF

   Check adj(1) for destination 3 → not found.
   are_all_adjacents_discovered?
     - resume_index_tbl[2] := $00
     - Next candidate = adj(1)[0] = 0 → is_box_discovered? (scan 1..2 = {0,1})
       → 0 is seen → keep scanning
     - resume_index_tbl[2] := $01
     - Next byte = $FF → ALL_KNOWN

   decrement_search_depth
   depth := 1
   (backtrack to node 0 context)

4) Resume scanning adj(0) after neighbor index 0
   are_all_adjacents_discovered at depth 1?
     - resume_index_tbl[1] := $01
     - Next candidate = adj(0)[1] = 2 → is_box_discovered? (scan 1..1 = {0})
       → 2 not seen → ONE_UNKNOWN

5) Choose neighbor 2
   current_box := 2

   increment_search_depth
   depth := 2
   actor_discovered_boxes[2] := 2
   resume_index_tbl[2] := $FF

   Check adj(2) for destination 3 → FOUND (adj(2)[1] = 3)
   serialize_box_path:
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
    - get_list_start_offset scans terminators to map a list index to its byte
      offset.
    - Fixed-stride per-actor path buffers for output paths.

================================================================================
*/

#importonce
#import "globals.inc"
#import "constants.inc"
#import "walkbox_helpers.asm"


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
  find_path_between_walkboxes
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
        • If destination box is adjacent → serialize_box_path and return PATH_OK.
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
find_path_between_walkboxes:
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
        jsr     is_box_in_list
        cmp     #DEST_IS_CONTAINED
        bne     explore_or_backtrack		// If not adjacent, continue search
		
        // ------------------------------------------------------------
		// Destination box is adjacent
		// We have a path to it, so generate it and return success
        // ------------------------------------------------------------
        jsr     serialize_box_path
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
        jsr     are_all_adjacents_discovered
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
	it left off. Selects the target neighbor list for this depth and 
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
    • Call get_list_start_offset to translate current_box list index into a byte
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
        jsr     get_list_start_offset
        rts
/*
================================================================================
  is_box_in_list
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
is_box_in_list:
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
  are_all_adjacents_discovered
================================================================================
Summary
    Determines whether every adjacent box for the current list has already been 
	discovered by the actor.

Arguments
    active_depth_level          selects which adjacency sublist to scan

Returns
    .A  		ONE_UNKNOWN   	at least one adjacent box not yet known
				ALL_KNOWN	    all adjacent boxes already known

Description
    - Calls restore_adjacency_list with X = active_depth_level to load adj_list_ptr
      and sublist_base_ofs.
    - Resumes scanning at iter_ofs := sublist_base_ofs + resume_index.
    - For each box id:
        * Reads (adj_list_ptr),Y.
        * If byte == ELEM_TERM → return ALL_KNOWN.
        * Otherwise sets current_box and calls is_box_discovered.
          Returns ONE_UNKNOWN on first unseen box, else continues.
    - Updates resume_index_tbl,X before each fetch to maintain continuation
      state across calls.
================================================================================
*/
* = $19B6
are_all_adjacents_discovered:
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
        jsr     is_box_discovered          // returns A=$00 if seen, A=$FF if not
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
  is_box_discovered
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
is_box_discovered:
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
  serialize_box_path
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
serialize_box_path:
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
  get_list_start_offset
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
get_list_start_offset:
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
Pseudo-code

function find_path_between_walkboxes(actorId) -> ResultCode:
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
        if is_box_in_list() == true:
            serialize_box_path(actorId)
            return RESULT_PATH_OK

        // Otherwise, explore neighbors or backtrack
        if are_all_adjacents_discovered() == true:
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
    listStartOffset = get_list_start_offset(listIndex)

    // Now we know:
    //   - currentBox: which node we’re exploring
    //   - resumeIndex: last neighbor index processed
    //   - listStartOffset: where its adjacency sublist starts in adjacencyBase


function is_box_in_list() -> bool:
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


function are_all_adjacents_discovered() -> bool:
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
        if is_box_discovered() == BOX_NOT_SEEN:
            // Found at least one yet-unseen neighbor
            return false

        // This neighbor is already seen; move to the next
        neighborIndex += 1
        continue loop


function is_box_discovered() -> SeenFlag:
    // Scan the discovered boxes for current DFS path from depth 1..activeDepthLevel
    target = currentBox
    depthLimit = activeDepthLevel

    for depth from 1 to depthLimit:
        if actorDiscoveredBoxes[depth] == target:
            return BOX_SEEN

    return BOX_NOT_SEEN


function serialize_box_path(actorId):
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


function get_list_start_offset(targetListIndex) -> int:
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

*/