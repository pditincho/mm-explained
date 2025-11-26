/*
================================================================================
   Memory Manager - Technical Overview
================================================================================

This module implements a full dynamic allocator for variable-sized blocks in a
flat C64 address space. It uses a singly-linked free-list of arbitrarily sized
FREE nodes, supports splitting and consuming blocks, maintains address order, 
coalesces contiguous spans, performs physical compaction of USED blocks, and
finally integrates with the resource subsystem to evict low-priority assets when
memory pressure is high.

The design is tuned for:
    • deterministic pointer semantics (no relocatable handles)
    • fast incremental operations on small lists
    • aggressive recovery from fragmentation
    • minimal RAM footprint
    • compatibility with SCUMM-style resource blocks (sound, costume, etc.).

--------------------------------------------------------------------------------
  BLOCK FORMATS
--------------------------------------------------------------------------------
Two block types exist: FREE and USED. Both begin with a 4-byte header:

    FREE block header:
        +0/+1 : size in bytes (lo = tail bytes on last page; hi = full pages)
        +2/+3 : next pointer in the free list (NULL = end)

    USED block header:
        +0/+1 : size of the block (header + payload)
        +2    : resource_type   (sound, costume, script, etc.)
        +3    : resource_index  (instance ID for fix-ups)

Only FREE blocks participate in the linked list. USED blocks live in memory but
are not linked; they are only traversed implicitly during compaction.

Null pointers are detected via the invariant:
    “No block header ever resides in page $00”.  
Thus any pointer with hi == $00 is treated as NULL.

--------------------------------------------------------------------------------
  GLOBAL FREE-LIST STRUCTURE
--------------------------------------------------------------------------------
The FREE list is singly-linked and anchored by a synthetic header (“stub”):

        mem_free_stub:
            +2..+3 = mem_free_head pointer

Because the stub stores the logical head pointer, operations may uniformly
manipulate “anchor_block.next” whether the anchor is a real FREE node or the
stub. This simplifies sorting and best-fit predecessor tracking.

The system always tracks:
        mem_free_head = first real FREE node (possibly NULL)
        mem_free_tail = last FREE node (never NULL unless list empty)

--------------------------------------------------------------------------------
  ALLOCATION PIPELINE
--------------------------------------------------------------------------------
mem_alloc(X=lo, Y=hi) begins the allocation flow:

1. Primary Best-Fit Attempt
   mem_alloc_bestfit computes:
        mem_req_payload  = requested payload
        mem_req_total    = payload + 4-byte header
   If the free list is non-empty, mem_bestfit_select scans all FREE nodes and
   chooses the smallest block with size ≥ mem_req_total. Ties prefer the later
   block. The chosen block is carved by mem_alloc_from_free.

2. Recovery Step 1 - Physical Compaction
   If no FREE block can satisfy the request, mem_compact_then_release calls:
        mem_compact_leading_free:
            • Takes the FREE head block (“the hole”).
            • Identifies the run of USED blocks immediately to its right.
            • Uses mem_bubble_used_left to physically slide those USED blocks
              into the hole, thereby shifting the hole rightward.
            • Rebuilds the FREE header at the new hole position.
            • Calls mem_coalesce_right to merge adjacent FREE regions.
            • Repeats until no FREE block lies to the right.

   This creates much larger contiguous spans by eliminating interleaved USED
   islands-an effect not possible with pure list surgery.

   Then it retries mem_alloc_bestfit using the saved request size.

3. Recovery Step 2 - Resource Eviction
   If compaction still cannot satisfy the request, the allocator calls
   rsrc_evict_one_by_priority, which frees the least-important asset. External
   pointers are refreshed via rsrc_update_ptr. The allocator then retries
   compaction+best-fit until either:
       • A block is obtained (success), or
       • Nothing else can be evicted (failure).

--------------------------------------------------------------------------------
  FREE-LIST MAINTENANCE ALGORITHMS
--------------------------------------------------------------------------------

1. Best-Fit Scan (mem_bestfit_select)
   Linear walk:
       - Skip candidates < required size.
       - On candidates ≥ required size, choose the smallest.
       - Records the candidate, its predecessor, and its size.
       - Returns the winner for carving.

2. Block Carving (mem_alloc_from_free)
   Given a chosen FREE block:
       - Computes remainder = block_size − mem_req_total.
       - If remainder ≥ 4 bytes, split:
             • allocated block occupies the front portion,
             • a trailing FREE block is created with size=remainder.
       - Else consume the entire block.
       - Predecessor.next and mem_free_tail updated accordingly.

3. Address-Order Sort (mem_sort_free_ao)
   An O(n²) in-place insertion-style sort. For each “anchor” block, it finds any
   node whose address is < anchor_next and swaps it into place via pointer
   rewiring. Sorting the list greatly increases opportunities for coalescing.

4. Coalescing (mem_coalesce_right)
   Walks the FREE list and merges contiguous neighbors:
       if next == current + current.size:
           • absorb next.size into current.size,
           • link out next,
           • update mem_free_tail if needed.

   This may cascade multiple times at the same node.

--------------------------------------------------------------------------------
  COMPACTION ENGINE
--------------------------------------------------------------------------------

1. Leading-FREE Sliding Model
   The allocator treats the FIRST free node as a movable “hole” in memory.
   If USED blocks directly follow it, they can be relocated downward (toward the
   hole) to accumulate a larger free span.

   mem_compact_leading_free:
       - Reads the size of the hole.
       - Sets mem_src = hole_start + hole_size (first USED block).
       - Finds the next FREE block after the USED run.
       - Calls mem_bubble_used_left.

2. Bubble-Left Relocation (mem_bubble_used_left)
   While mem_src < mem_next_free:
       - Reads the USED block’s size and metadata (type/index).
       - Invokes mem_copy_pages to copy entire USED block from mem_src → mem_dst.
       - Updates external references via rsrc_update_ptr.
       - Moves mem_dst and mem_src forward by the block size.

   When mem_src reaches mem_next_free:
       - The hole has slid to mem_dst.
       - A new FREE header is written at mem_dst with size=original_hole_size.
       - mem_free_head := mem_dst.

   The invariants:
       • mem_dst < mem_src always,
       • distance (mem_src − mem_dst) stays equal to the hole size,
       • copy direction is forward and safe for overlap,
       • pointer fix-ups maintain resource validity.

This produces a defragmented region of increasing size, enabling a best-fit
allocation that would otherwise fail.

--------------------------------------------------------------------------------
  MEMORY COPYING (mem_copy_pages)
--------------------------------------------------------------------------------
Copies any block of N bytes, represented as:
        page_cnt = N / 256
        tail     = N % 256
For full pages, mem_copy_tail=0 triggers a 256-byte copy through Y-wrap. The
routine increments read/write page pointers and handles a final partial page.
Used only for USED-block relocation inside mem_bubble_used_left.

--------------------------------------------------------------------------------
  FREE-LIST INITIALIZATION
--------------------------------------------------------------------------------
init_heap_free_list builds a single FREE node at HEAP_FREE_HDR_ADDR covering the
entire heap region and terminates it with next=NULL. Both mem_free_head and
mem_free_tail point to this node.

--------------------------------------------------------------------------------
  SUMMARY OF KEY INVARIANTS
--------------------------------------------------------------------------------
    • No block header ever resides in page $00 → hi==0 means NULL.
    • FREE node size is in bytes (header+payload): lo=tail, hi=pages.
    • USED node size is also in bytes (header+payload).
    • mem_free_stub.next mirrors mem_free_head.
    • mem_free_tail always points to the last FREE node.
    • In compaction: (mem_src − mem_dst) == original_free_size.
    • rsrc_update_ptr is called after every USED relocation.

This system combines classical best-fit allocation with deterministic “bubble-left”
compaction and AO-sorted coalescing, giving a robust allocator capable of
reclaiming large spans even under severe fragmentation.

================================================================================
 
  Public routines:
  	mem_alloc:
  		- Resilient allocator wrapper.
 		Given payload size in X:Y, tries best-fit; on failure compacts/coalesces
  		the FREE list and, as a last resort, releases one low-priority asset,
  		then retries with the preserved request size (mem_req_size).
   	Success: Z=0 and X:Y = allocated block header (lo/hi).
  		Failure: Z=1 (no fit). Side effects: FREE list may be reordered/compacted;
  		a low-priority asset may be released by policy.
 
  	mem_release:
  		- Release a memory block.
  		Append freed block to FREE list, normalize, coalesce.
  		Input:   X:Y = header pointer of the block being released.
  		Actions: link old tail->next to X:Y; set mem_free_tail := X:Y; set new tail->next := NULL;
       normalize FREE list order by address; coalesce right-adjacent blocks.
  		Output:  mem_released_flag := 1. FREE list may be reordered/compacted; registers clobbered.
 
  Private routines:
  
    mem_alloc_bestfit (X/Y = payload size):
      - Computes mem_req_total = payload + 4 (free header size), seeds the scan,
        and dispatches to best-fit. Returns X/Y = allocated header on success (Z=0).
        On failure (free list empty or no fit), returns Z=1 (X/Y unspecified).
  		Used externally by alloc_data.
 
    mem_bestfit_select:
      - Best-fit scan over free nodes (ties prefer later). On success sets
        mem_free_cur to winner and calls mem_alloc_from_free.
 
    mem_alloc_from_free:
      - Split-or-consume the chosen free node. Split if remainder ≥ 4 bytes;
        otherwise splice out the node. Maintains mem_free_tail.
 
  	mem_copy_pages
      - Copies a block as whole pages plus a tail: first moves total_pages
        full 256-byte pages from mem_src→mem_dst, then copies mem_copy_tail
        remaining bytes. Preserves intra-page order; clobbers A/X/Y.
		
  	mem_compact_leading_free
      - Assumes the list starts with a FREE block. While a USED block directly
        follows it, copies that USED (header+payload) down into the FREE region,
        applies pointer fix-ups, advances mem_dst, and rebuilds the FREE header.
        Stops at the first non-adjacent or FREE block. Updates mem_free_head/tail.
 
    mem_sort_free_ao (AO pass):
      - For each anchor, finds the lowest-address node < anchor_next and swaps
        it into anchor_next by pointer rewiring. In-place, O(n^2), non-stable.
        Refresh mem_free_head from the stub on exit; update mem_free_tail.
 
    mem_coalesce_right:
      - Merge right-adjacent free nodes in-place: if next == left+left.size,
        link-out right and grow left by right.size. Cascades at same left node.
 
    mem_compact_then_release:
      - Recovery path when mem_alloc_bestfit fails. Runs address-order sort
        (mem_sort_free_ao), coalesces right-adjacent free nodes (mem_coalesce_right),
        and compacts by bubbling USED blocks left into the leading FREE region
        (mem_bubble_used_left). If still no fit, calls external
        rsrc_evict_one_by_priority and refreshes pointers via rsrc_update_ptr,
        then retries best-fit. Loops until allocation succeeds or nothing remains
        to release. Returns X/Y = allocated header, Z=0 on success; Z=1 on failure.
  
  	mem_read_next_ptr
      - Utility: reads header +2..+3 (little-endian “next” pointer) from the
        block whose header pointer is in mem_hdr_ptr. Returns X/Y = next (hi/lo)
        and sets Z=1 if the next pointer is NULL.
 
    mem_bubble_used_left:
      - While USED blocks immediately follow the leading free node, copy each
        USED (header+payload) into the FREE region (mem_dst<header), fix
        external references, and advance pointers. Rebuild free header at end.
 
  Conventions:
    - Registers: A/X/Y clobbered by most routines; branch docs specify Z/C meaning.
    - Copy helper (mem_copy_pages):
        mem_copy_page_cnt = full pages, mem_copy_tail = tail bytes on last page,
        mem_read_ptr/mem_write_ptr = header addresses; direction safe for overlap.
 
================================================================================
*/

#importonce
#import "globals.inc"
#import "constants.inc"
#import "rsrc_mgmt.asm"

// mem_alloc vars
.label mem_req_size_lo = $5501  // 16-bit requested payload size (bytes, little-endian).
.label mem_req_size_hi = $5502  
								// Contract: allocator treats this as payload bytes; it accounts
								// for header bytes internally. Access with <mem_req_size/>mem_req_size.
// mem_release vars
.label mem_edit_tail_lo 	= $61    // 16-bit ZP “edit tail” pointer (lo/hi) used while appending a
.label mem_edit_tail_hi 	= $62    // freed block to the FREE list. Points at the current tail node
									 // when writing tail->next and terminating the list (next := NULL).
                                                              
// mem_coalesce_right vars
.label left_size_lo        = $56C5    	// 16-bit size of current free block on the left side; shared temp reused elsewhere
.label left_size_hi        = $56C6    	
.label mem_left_lo         = $4F      	// zp pointer to current free block header (lo @ $4F, hi @ $50)
.label mem_left_hi         = $50      	
.label mem_right_lo        = $59      	// pointer to block at (mem_left + left_size): immediate right neighbor
.label mem_right_hi        = $5A      	

// mem_sort_free_ao vars
.label curr_block_lo       = $4F      // node under comparison during scan (alias of mem_free_cur storage)
.label curr_block_hi       = $50      
.label anchor_block_lo     = $51      // pass anchor / predecessor for this sweep (often the head stub initially)
.label anchor_block_hi     = $52      
.label anchor_next_lo      = $53      // first candidate after anchor_block for this pass
.label anchor_next_hi      = $54      
.label min_offender_lo     = $55      // lowest-address node found with addr < anchor_next; $FFFF sentinel = none
.label min_offender_hi     = $56      
.label min_prev_lo         = $57      // predecessor of min_offender (for relinking); shares ZP with `mem_src`
.label min_prev_hi         = $58      
.label curr_prev_lo        = $5746    // ABS temp: rolling predecessor while scanning
.label curr_prev_hi        = $5747    
.label curr_next_lo        = $5748    // ABS temp: cached anchor_next->next
.label curr_next_hi        = $5749    
.label min_next_lo         = $574A    // ABS temp: cached min_offender->next
.label min_next_hi         = $574B    

// mem_alloc_bestfit / mem_bestfit_select vars
.label mem_free_cur_lo     = $4F      // zp pointer to current free block header
.label mem_free_cur_hi     = $50      
.label mem_req_payload_lo  = $FD9C    // requested payload size in bytes (header NOT included)
.label mem_req_payload_hi  = $FD9D    
.label mem_best_blk_lo     = $5813    // pointer to best-fit free block found
.label mem_best_blk_hi     = $5814    
.label mem_best_size_lo    = $5815    // 16-bit size of best-fit block (lo=tail, hi=pages)
.label mem_best_size_hi    = $5816    
.label mem_prev_lo         = $5817    // rolling predecessor of the current candidate during the scan
.label mem_prev_hi         = $5818    
.label mem_req_total_lo    = $5819    // total bytes to carve = mem_req_payload + 4 (header + payload)
.label mem_req_total_hi    = $581A    
.label candidate_size_lo   = $56C5    // size of the candidate under inspection (reuses shared temp @ $56C5)
.label candidate_size_hi   = $56C6    

// mem_compact_then_release vars
.label mem_req_saved_lo    = $58D3    // preserved copy of mem_req_payload across compaction/retry attempts
.label mem_req_saved_hi    = $58D4    

// mem_read_next_ptr vars
.label mem_hdr_ptr         = $4F      // input: pointer to a block header; routine returns next in X=lo, Y=hi

// mem_alloc_from_free vars
.label mem_split_rem_lo    = $56C5    // remainder after carve: mem_best_size − mem_req_total (shared temp)
.label mem_split_rem_hi    = $56C6    
.label temp                = $56C7    // 8-bit scratch used during pointer transfers
.label mem_split_tail_lo   = $55      // trailing free block created on split; in consume path holds predecessor for tail update
.label mem_split_tail_hi   = $56      
.label predecessor_lo      = $51      // predecessor pointer; may be the head stub
.label predecessor_hi      = $52      

// mem_bubble_used_left vars
.label mem_src_lo          = $57      // pointer to USED block being moved (initially right of the leading FREE block); shares ZP with min_prev
.label mem_src_hi          = $58      
.label mem_dst_lo          = $4F      // pointer to mem_dst (start of the leading FREE span)
.label mem_dst_hi          = $50      
.label mem_free_sz_lo      = $59A2    // size (bytes) of the current leading FREE block
.label mem_free_sz_hi      = $59A3    
.label mem_used_sz_lo      = $59A4    // size (bytes) of the USED block being relocated (header + payload)
.label mem_used_sz_hi      = $59A5    
.label mem_next_free_lo    = $59A6    // pointer to the next FREE block after the leading one
.label mem_next_free_hi    = $59A7    
.label resource_type       = $59A8    // resource type tag for pointer fix-ups
.label resource_index      = $59A9    // resource index/id for pointer fix-ups

// mem_copy_pages vars
.label mem_copy_tail       = $5B      // final-page byte count (1..255); set to 0 on non-final pages to copy a full 256 via Y-wrap
.label mem_copy_page_cnt   = $5C      // number of full 256-byte pages to copy; decremented once per completed page
.label mem_read_ptr_lo     = $5D      // mem_src pointer
.label mem_read_ptr_hi     = $5E      
.label mem_write_ptr_lo    = $5F      // mem_dst pointer
.label mem_write_ptr_hi    = $60      

// Head and Tail of free block list
.label mem_free_stub       = $FF61    // synthetic head: its header[+2..+3] holds mem_free_head
.label mem_free_head_lo    = $FF63    // free-list head pointer
.label mem_free_head_hi    = $FF64    
.label mem_free_tail_lo    = $FF65    // free-list tail pointer
.label mem_free_tail_hi    = $FF66    


/*
================================================================================
  mem_alloc
================================================================================
Summary:
    Attempts to allocate a block for a payload.
    Tries best-fit first; if that fails, compacts/coalesces the FREE list,
    and as a last resort releases one low-priority asset, then retries.
    The original requested size is preserved across attempts.
 
Arguments:
    X/Y                    Requested payload size in bytes (Y:hi/X:lo).
  
State:
    mem_req_size		   	Scratch to preserve the request size for retries.
    FREE list         		Memory manager state used by allocator/compactor.
 
Returns:
    Z                          0 on success, 1 on failure (after all attempts).
    X/Y                        On success: pointer to allocated block header (lo/hi).
	
Globals                    
	FREE list may be reordered/compacted
	low-prio asset may be released by policy.
 
Description:
    1) Save the caller’s requested size in mem_req_size
       (so we can restore it for any retry path).
    2) mem_alloc_bestfit:
         - If it finds a block: return with block
         - Else: try recovery.
    3) mem_compact_then_release:
         - Compacts and coalesces the FREE list. 
 			If allocation now fits: return with block.
    4) release_one_costume:
         - Frees one low-priority “costume” and restores X:Y from
           mem_req_size, then loops back to step (2).
    5) On return, X:Y holds the allocated block header pointer
================================================================================
*/
* = $54E4	
mem_alloc:
		// Preserve caller’s requested size:
		// X/Y = payload size (bytes) - needed for retries below.
        stx mem_req_size_lo
        sty mem_req_size_hi

		// First attempt: allocate by best-fit over current free list.
		// On return: Z=0 success (header ptr in X/Y), Z=1 failure → try recovery.
        jsr mem_alloc_bestfit
        bne mem_alloc_ok          

        // Recovery step 1: compact + coalesce (and optionally release resources).
        // On return: Z=0 success (header ptr in X/Y), Z=1 still no fit.
        jsr mem_compact_then_release
        bne mem_alloc_ok          

        // Recovery step 2: unlock or unassign a costume and retry original request.
        jsr rsrc_unlock_or_detach_costume

        // Restore original request size into X/Y and loop back to first attempt.
        ldx mem_req_size_lo
        ldy mem_req_size_hi
        jmp mem_alloc

mem_alloc_ok:
        // Success path: X/Y = allocated block header (little-endian).
        rts
/*
================================================================================
  mem_release
================================================================================ 
Summary:
    Given a block header pointer in X:Y, splice the block as the new
    tail of the FREE list, terminate it, normalize list order by
    address, and coalesce adjacent nodes. Signals that at least one
    resource was released.
 
Arguments:
    X:Y 	              	16-bit pointer to the block header being released.
    mem_free_tail        	16-bit pointer to current FREE-list tail (lo/hi).
 
Description:
    - Snapshot current tail: rsrc_tail_old := mem_free_tail.
    - Make released block the new tail: mem_free_tail := X:Y.
    - Link old tail → new tail:
        rsrc_tail_old->next := mem_free_tail
        (header: +0..+1 size, +2..+3 next).
    - Set edit pointer and terminate list:
        mem_edit_tail := mem_free_tail
        mem_edit_tail->next := NULL
    - Normalize order by address: jsr mem_sort_free_ao  (O(n^2), non-stable).
    - Coalesce right-adjacent blocks: jsr mem_coalesce_right.
================================================================================
*/
* = $5503
mem_release:
		// ------------------------------------------------------------
		// Snapshot current tail of the FREE list so we can splice after it.
		//
		//   mem_edit_tail := mem_free_tail
		// ------------------------------------------------------------
        lda mem_free_tail_lo
        sta mem_edit_tail_lo
        lda mem_free_tail_hi
        sta mem_edit_tail_hi

		// ------------------------------------------------------------
		// Make the block being released (X=lo, Y=hi) the new FREE tail.
		//
		//   mem_free_tail := (X,Y)
		// ------------------------------------------------------------
        stx mem_free_tail_lo
        sty mem_free_tail_hi

		// ------------------------------------------------------------
		// Link old tail → new tail:
		//
		//    mem_edit_tail->next := mem_free_tail
		// ------------------------------------------------------------
        ldy #MEM_HDR_OFS_NEXT_LO        
        lda mem_free_tail_lo
        sta (mem_edit_tail_lo),Y
        iny                         
        lda mem_free_tail_hi
        sta (mem_edit_tail_lo),Y

		// ------------------------------------------------------------
		// Advance our edit pointer to the newly appended FREE tail:
		//
		//   mem_edit_tail := mem_free_tail
		// ------------------------------------------------------------
        stx mem_edit_tail_lo    	// lo = X (from above)
        sta mem_edit_tail_hi        // hi = A (still = mem_free_tail_hi)

		// ------------------------------------------------------------
		// Terminate the list at the new tail:
		//
		//   mem_edit_tail->next := NULL
		// ------------------------------------------------------------
        lda #PTR_NULL
        ldy #MEM_HDR_OFS_NEXT_LO
        sta (mem_edit_tail_lo),Y          
        iny
        sta (mem_edit_tail_lo),Y          

        // Normalize FREE list order by address
        jsr mem_sort_free_ao

		// ------------------------------------------------------------
		// Coalesce right-adjacent FREE nodes:
		//
		// 	 while (next == this + this.size) { this.size += next.size; unlink(next); }
		// ------------------------------------------------------------
        jsr mem_coalesce_right

        // Signal to callers/policies that a block has been released.
        lda #TRUE
        sta rsrc_released_flag
        rts
/*
================================================================================
  mem_coalesce_right
================================================================================
Summary:
    Walks the free list, merging pairs of free nodes that are laid out
    back-to-back in memory (right node starts at left + left.size). Merges can
    cascade at the same left node until its next is non-adjacent. Maintains
    mem_free_tail if the right node was the tail.
 
Arguments:
    mem_free_head    	   	Head pointer of the free list. Seeds the walk.
    mem_free_tail  	    	Tail pointer of the free list. May be updated
								when the rightmost participant of a merge was the tail.
 
Effects:
    In-place merges of adjacent nodes (sizes and next links updated).
    mem_free_tail updated if the old tail got merged into its left neighbor.
 
Description: 
    For each node:
      1) Read its size into mem_used_sz.
 
      2) Compute mem_right = mem_left + mem_used_sz (the address that would
         be immediately to the right if a neighbor is touching).
 
      3) Read mem_next_free (current->next). If mem_next_free == mem_right, the two
         blocks are contiguous:
            • Link-out the right block: first_block.next ← second_block.next.
            • Grow the left block: size(left) ← size(left) + size(right).
            • If the right block was mem_free_tail, move the tail to the coalesced left.
 
      4) If not contiguous, advance to mem_next_free and continue.
 
    This pass eliminates internal gaps between free regions, creating larger
    contiguous spans that improve the likelihood of satisfying big allocations.
 
================================================================================
*/
* = $56C8
mem_coalesce_right:
		// Load the head of the free list into ‘mem_left’.
		lda mem_free_head_lo
		sta mem_left_lo
		lda mem_free_head_hi
		sta mem_left_hi

		// Empty list? If mem_free_head == $0000, nothing to merge → return.
		beq mem_exit_1

compute_adjacent:
		// ------------------------------------------------------------
		// Read the current free block’s size (on the left side):
		// ------------------------------------------------------------
		ldy #MEM_HDR_OFS_SIZE_LO
		lda (mem_left_lo),Y
		sta left_size_lo
		iny
		lda (mem_left_lo),Y
		sta left_size_hi

		// ------------------------------------------------------------
		// Compute the address of the block that would be immediately to the right:
		// 
		// mem_right = mem_left + left_size
		// ------------------------------------------------------------
		clc
		lda left_size_lo
		adc mem_left_lo
		sta mem_right_lo
		lda left_size_hi
		adc mem_left_hi
		sta mem_right_hi

		// Fetch the pointer to the NEXT free block (into X/Y).
		jsr mem_read_next_ptr

		// If there is no next block (X:Y == $0000), we’re at the tail → done.
		bne adjacency_check
		rts

adjacency_check:
		// ------------------------------------------------------------
		// Test right-adjacency:
		// 
		// Are we exactly at mem_left + mem_used_sz ? (i.e., mem_right)
		// Compare mem_next_free (X/Y) against mem_right (lo/hi).
		// ------------------------------------------------------------
		cpx mem_right_lo
		bne adjacency_check_hi
		cpy mem_right_hi
adjacency_check_hi:
		// Branch if NOT adjacent (X:Y ≠ mem_right).
		bne next_block_is_not_adjacent

		// ------------------------------------------------------------
		// Adjacent detected
		// ------------------------------------------------------------
		
		// Unnecessary code, as X/Y already have the desired values
		// Keeping here for consistency with original
		stx mem_right_lo
		sty mem_right_hi

		// ------------------------------------------------------------
		// Splice out the second block from the free list:
		//  mem_left.next ← mem_right.next
		// 
		// Copy header +2..+3 (next pointer) from mem_right → mem_left.
		// ------------------------------------------------------------
		ldy #MEM_HDR_OFS_NEXT_LO
		lda (mem_right_lo),Y
		sta (mem_left_lo),Y
		iny
		lda (mem_right_lo),Y
		sta (mem_left_lo),Y

		// ------------------------------------------------------------
		// Add the two block sizes to form the new, coalesced size in mem_left.
		// 
		// Perform 16-bit addition: mem_left.size = left_size + mem_right.size
		// ------------------------------------------------------------
		ldy #MEM_HDR_OFS_SIZE_LO
		clc
		lda left_size_lo
		adc (mem_right_lo),Y          
		sta (mem_left_lo),Y           
		iny
		lda left_size_hi
		adc (mem_right_lo),Y          
		sta (mem_left_lo),Y           

		// ------------------------------------------------------------
		// If we just removed the TAIL node (mem_right == mem_free_tail),
		// then the newly coalesced first block becomes the new tail.
		// ------------------------------------------------------------
		lda mem_right_lo
		cmp mem_free_tail_lo
		bne continue
		lda mem_right_hi
		cmp mem_free_tail_hi
		bne continue
	   
	    // Set the new tail = mem_left
		lda mem_left_lo
		sta mem_free_tail_lo
		lda mem_left_hi
		sta mem_free_tail_hi

continue:
		// Continue scanning: fall through to null-check using hi byte of mem_left.
		lda mem_left_hi
		jmp is_block_null

next_block_is_not_adjacent:
		// Not adjacent → advance to the next free block and keep checking.
		jsr mem_read_next_ptr			//Redundant call - kept here to match the original
		stx mem_left_lo
		sty mem_left_hi

		// ------------------------------------------------------------
		// Loop guard: if mem_left != $0000, process next candidate; else exit.
		// ------------------------------------------------------------
is_block_null:
		bne compute_adjacent
mem_exit_1:
		rts
/*
================================================================================
  mem_sort_free_ao
================================================================================ 
Summary:
    Reorders the singly linked free-list into ascending address order (AO).
    For each anchor (anchor_block), find the lowest-address node < anchor_next
    within the remainder of the list and swap it into anchor_next’s position
    by pointer rewiring. Repeat until the end. O(n^2), in-place, non-stable.
 
Arguments / Inputs:
    mem_free_stub   	Synthetic head; +2..+3 hold mem_free_head.
    mem_free_head      	Current head pointer (variable).
    mem_free_tail       Tail pointer (variable).
 
Effects / Outputs:
    free list 			becomes address-ordered. 
	mem_free_tail 		updated to final node.
    mem_free_head 		refreshed from the stub on exit.
 
Description:
    The routine treats the free-list stub as a synthetic head whose +2..+3
    mirror a normal block’s ‘next’ field. For each pass:
 
      1) Set anchor_block to the current anchor (initially the stub).
 
      2) Initialize min_offender ← $FFFF (sentinel meaning “none found”).
 
      3) Walk the list window starting at anchor_next = anchor_block->next:
           • For each curr_block, read mem_next_free = curr_block->next.
           • Check Address Order (AO) against anchor_next:
                 (curr_block - anchor_next)  → C=0 means out of order.
           • Track the lowest-address offender (strictly less than any prior).
 
      4) If an offender was found, swap it with anchor_next by re-linking four
         ‘next’ pointers:
             anchor_block.next     	= min_offender
             min_prev.next   		= anchor_next
             min_offender.next 		= anchor_next.next
             anchor_next.next      	= min_next
         (min_prev/min_next snapshot offender’s neighbors.)
 
      5) Advance the anchor:
             anchor_block ← anchor_block->next
         and repeat until anchor_block->next is null.
 
    The effect is akin to performing an insertion step per anchor, moving
    the lowest offending address up to its correct spot after anchor_block. After
    all anchors advance, the free-list is sorted by address, which increases the
    likelihood that adjacent free blocks become coalescible and can be merged
    efficiently by mem_coalesce_right.

Notes: 
    - Null pointer detected via HI byte == $00 (no headers in page $00).
    - mem_read_next_ptr: X=next.lo, Y=next.hi, Z from Y; STX/STY do not change flags.
    - curr_block is the input pointer alias for mem_read_next_ptr.
    - Tie-breaks don’t matter (addresses unique); algorithm is non-stable, but 
	it doesn't matter as sorting keys (addresses) are always unique.
================================================================================
*/
* = $574C
mem_sort_free_ao:
		// ------------------------------------------------------------
		// Initialize sorting by setting ‘anchor_block’ to point at the
		// free block stub. This structure mimics a normal block header,
		// holding the mem_free_head pointer at offsets +2..+3.
		// 
		// The routine will use anchor_block as the list head during traversal,
		// allowing consistent pointer logic for block headers and the stub.
		// ------------------------------------------------------------
		lda #<mem_free_stub
		sta anchor_block_lo
		lda #>mem_free_stub
		sta anchor_block_hi

setup_comparison:
		// ------------------------------------------------------------
		// Prepare pass state:
		//
		//  min_offender ← $FFFF
		//
		// Sentinel meaning “no offender found yet”; any real block address
		// will compare lower and replace it.
		// ------------------------------------------------------------
		lda #$FF
		sta min_offender_lo
		sta min_offender_hi

		// ------------------------------------------------------------
		// Initialize the sliding window at the list head:
		//
		//  curr_prev ← anchor_block  (the stub)
		//
		// We’ll advance to the first real entry next; keeping curr_prev aligned
		// with the node before the current candidate simplifies pointer swaps.
		// ------------------------------------------------------------
		lda anchor_block_lo
		sta curr_prev_lo
		lda anchor_block_hi
		sta curr_prev_hi

		// ------------------------------------------------------------
		// Prime the first candidate pair:
		//
		//   anchor_next ← anchor_block->next    (read stub’s next)
		//   curr_block ← anchor_next            (curr_block mirrored in X/Y for speed)
		//
		// If anchor_next == $0000, the list is empty/singleton → nothing to sort.
		// ------------------------------------------------------------
		ldy #MEM_HDR_OFS_NEXT_LO
		lda (anchor_block_lo),Y
		sta anchor_next_lo
		tax
		iny
		lda (anchor_block_lo),Y
		sta anchor_next_hi
		tay
		bne compare_order            	// nonzero: have a first candidate
		// No next block → nothing to sort
		jmp no_more_blocks

compare_order:
		// ------------------------------------------------------------
		// Establish the comparison window:
		//
		//   curr_block ← X/Y     			(candidate under test)
		//   curr_next  ← curr_block->next
		//
		// We'll verify whether curr_block is in Address Order (AO)
		// relative to anchor_next and track any out-of-order offender.
		// ------------------------------------------------------------
		//   curr_block ← X/Y     			
		stx curr_block_lo
		sty curr_block_hi

		//   curr_next  ← curr_block->next
		jsr mem_read_next_ptr
		stx curr_next_lo
		sty curr_next_hi

		// ------------------------------------------------------------
		// Address Order (AO) check between curr_block and anchor_next.
		// 
		// Compute (curr_block - anchor_next) as a 16-bit subtract
		//
		// Interpretation:
		//   C=1 (no borrow)  → curr_block ≥ anchor_next  → AO OK → skip to next_comparison
		//   C=0 (borrow)     → curr_block <  anchor_next → AO violated → handle offender
		// ------------------------------------------------------------
		sec
		lda curr_block_lo
		sbc anchor_next_lo
		lda curr_block_hi
		sbc anchor_next_hi
		bcs next_comparison            	// C=1 → in order; continue

		// ------------------------------------------------------------
		// AO violated → record the lowest-address offender seen so far.
		// 
		// Compare (curr_block - min_offender):
		//   C=1 (no borrow)  → curr_block ≥ min_offender → not lower → keep existing
		//   C=0 (borrow)     → curr_block <  min_offender → NEW lowest offender
		// ------------------------------------------------------------
		sec
		lda curr_block_lo
		sbc min_offender_lo
		lda curr_block_hi
		sbc min_offender_hi
		bcs next_comparison				// C=1 → not lower than current lowest → skip

		// ------------------------------------------------------------
		// New lowest-address offender detected - snapshot offender and links:
		// 
		//    min_offender ← curr_block       	(the out-of-order node)
		//    min_prev     ← curr_prev          (node before offender)
		//    min_next     ← curr_next          (node after offender)
		//
		// These will be used later to perform the pointer swap with anchor_next.
		// ------------------------------------------------------------
		lda curr_block_lo
		sta min_offender_lo
		lda curr_block_hi
		sta min_offender_hi

		lda curr_prev_lo
		sta min_prev_lo
		lda curr_prev_hi
		sta min_prev_hi

		lda curr_next_lo
		sta min_next_lo
		lda curr_next_hi
		sta min_next_hi

next_comparison:
		// ------------------------------------------------------------
		// Slide the comparison window forward:
		//   curr_prev    ← curr_block
		//   curr_block (X/Y) ← curr_next
		// Then continue while curr_block != $0000.
		// ------------------------------------------------------------
		lda curr_block_lo
		sta curr_prev_lo
		lda curr_block_hi
		sta curr_prev_hi

		ldx curr_next_lo
		ldy curr_next_hi

		// If curr_block (X/Y) is null, we reached the end - proceed to swap check.
		bne compare_order

		// ------------------------------------------------------------
		// End of this sweep: if an out-of-order (min_offender) was found,
		// perform a localized re-link to move it directly after anchor_block
		// (i.e., swap it into the position of anchor_next).
		// 
		//  min_offender == $FFFF → none found → skip swap.
		// ------------------------------------------------------------
		lda min_offender_hi
		cmp #$FF
		beq next_start_block

		// ------------------------------------------------------------
		// Swap min_offender with anchor_next
		//
		// Pointer surgery overview (all writes address header “next” fields):
		//   anchor_block.next     	= min_offender
		//   min_prev.next   		= anchor_next
		//   min_offender.next 		= anchor_next.next
		//   anchor_next.next      	= min_next
		// ------------------------------------------------------------
		ldy #MEM_HDR_OFS_NEXT_LO

		// anchor_block->next = min_offender
		lda min_offender_lo
		sta (anchor_block_lo),Y

		// min_prev->next = anchor_next
		lda anchor_next_lo
		sta (min_prev_lo),Y

		// min_offender->next = anchor_next->next
		lda (anchor_next_lo),Y
		sta (min_offender_lo),Y

		// anchor_next->next = min_next
		lda min_next_lo
		sta (anchor_next_lo),Y

		// Repeat for the high bytes of the pointers
		iny
		lda min_offender_hi
		sta (anchor_block_lo),Y
		lda anchor_next_hi
		sta (min_prev_lo),Y
		lda (anchor_next_lo),Y
		sta (min_offender_lo),Y
		lda min_next_hi
		sta (anchor_next_lo),Y

next_start_block:
		// ------------------------------------------------------------
		// Advance the outer pass anchor:
		//
		//   anchor_block ← anchor_block->next
		//
		// If no further nodes exist, finish by updating the tail pointer.
		// ------------------------------------------------------------
		lda anchor_block_lo
		sta curr_block_lo
		lda anchor_block_hi
		sta curr_block_hi
	   
		jsr mem_read_next_ptr           // X/Y = anchor_block->next
		beq no_more_blocks              // null → list exhausted
	   
		stx anchor_block_lo             // step anchor forward
		sty anchor_block_hi
		jmp setup_comparison            // begin next sweep from new anchor

no_more_blocks:
		// ------------------------------------------------------------
		// Sorting complete - refresh the recorded tail pointer to the final node.
		// (At this point anchor_block holds the last node in the list.)
		// ------------------------------------------------------------
		lda anchor_block_lo
		sta mem_free_tail_lo
		lda anchor_block_hi
		sta mem_free_tail_hi
		rts
/*
================================================================================
  mem_alloc_bestfit
================================================================================ 
Summary:
    Entry point for the allocator.  
	
    Given a requested payload size (in X/Y), this routine searches the free
    list for a suitable memory block, calling the best-fit selector to
    locate and allocate it.
 
Arguments:
    X / Y				Requested payload size (low/high).  
        				Represents only the data portion; 
  						the routine adds 4 bytes for the internal block header.
 
Vars/State:
    mem_req_payload		Stores requested payload size from X/Y.  
    mem_req_total		Computed as mem_req_payload + 4 (header included).  
    mem_free_head		Head pointer of the free list.  
    mem_free_cur        Updated to point to the current candidate free block.
 
Returns:
    X / Y               Pointer to allocated block header if successful.  
    Z Flag              Z=0 on success (block allocated),  
        				Z=1 if no free block exists (free list empty).           
  
Description:
    The allocator begins by recording the caller’s requested size and
    calculating the total number of bytes needed, including the 4-byte
    block header.  
 
    It then initializes the search pointer to the start of the free list.
 
    If no free blocks exist (list is empty), it returns immediately with Z=1.  
 
    Otherwise, it chains into the mem_bestfit_select routine, which
    performs a full best-fit scan to locate the optimal block and split
    or consume it as needed.
	
Notes:
    - Null free pointer is detected via HI byte == $00 (no headers in page $00).
    - BNE uses Z from the last LDA of mem_free_head+1; STA does not affect flags.
    - Best-fit routine consumes mem_req_total and mem_req_payload and performs the split/consume.
================================================================================
*/
* = $581B	
mem_alloc_bestfit:
		// Capture payload size for allocator (alias of raw_data_size used by mem_alloc_from_free).
		stx mem_req_payload_lo
		sty mem_req_payload_hi

		// ------------------------------------------------------------
		// Compute total bytes required including the 4-byte block header:
		// Total requested bytes (HEADER + payload): mem_req_total = raw_data_size + MEM_HDR_LEN
		// ------------------------------------------------------------
		clc
		lda mem_req_payload_lo
		adc #<MEM_HDR_LEN
		sta mem_req_total_lo
		lda mem_req_payload_hi
		adc #>MEM_HDR_LEN
		sta mem_req_total_hi

		// Initialize the scan at the head of the free list.
		lda mem_free_head_lo
		sta mem_free_cur_lo
		// LDA sets Z, BNE below uses Z from LDA.
		lda mem_free_head_hi
		sta mem_free_cur_hi

		// ------------------------------------------------------------
		// If the free list is empty (mem_free_cur == $0000), return with Z=1.
		// Otherwise, tail-call into the best-fit selector.
		// ------------------------------------------------------------
		bne mem_bestfit_select
		rts
/*
================================================================================
  mem_bestfit_select
================================================================================
Summary:
    Walk the free list and select the smallest block whose size ≥ mem_req_total.
    On success, set mem_free_cur to the winner and invoke mem_alloc_from_free to carve it.
 
Arguments:
    mem_req_total           Requested allocation size in bytes (16-bit)
 
Vars/State:
    mem_free_cur     		Current free node under scan. 
  							On entry, points to the first REAL free node (not the stub).
    mem_free_stub     		Synthetic predecessor for the list head (used to seed prev tracking).
    mem_best_blk     		Address of the best candidate found so far (NULL when none).
    mem_best_size     		Size of the best candidate. 
    mem_prev     			Rolling predecessor of mem_free_cur during the scan.
 
Returns: 
    Success: Z = 0, X = lo(mem_best_blk), Y = hi(mem_best_blk).
    Failure: Z = 1 (no fit found). X/Y are unspecified.
 
 
Description:
    Initialize mem_best_size = $FFFF and mem_best_blk = $0000; set mem_prev = head stub.
	
    For each node:
      1) Read candidate size. If candidate < mem_req_total → skip.
      2) If candidate ≤ mem_best_size → record candidate as the new best (size, block, predecessor).
      3) Advance: mem_prev ← mem_free_cur; mem_free_cur ← mem_free_cur->next (via mem_read_next_ptr).
    After the scan:
      - If mem_best_blk == $0000 → return with Z = 1 (no fit).
      - Else: mem_free_cur ← mem_best_blk; JSR mem_alloc_from_free; return X/Y = mem_best_blk.
	  
Notes:
	- Tie-break: equal-size candidates prefer the later one encountered (non-stable).
================================================================================
*/
* = $583F
mem_bestfit_select:
		// ------------------------------------------------------------
		// Initialize “current best” to: size = $FFFF (max) and block = $0000 (none).
		// Any real fitting candidate will be smaller than $FFFF and replace this.
		// ------------------------------------------------------------
		lda #$FF
		sta mem_best_size_lo
		sta mem_best_size_hi
		lda #$00
		sta mem_best_blk_lo
		lda #$00
		sta mem_best_blk_hi

		// ------------------------------------------------------------
		// Seed mem_prev with the free-list stub head (acts as the predecessor
		// of the first real free block). As we walk the list, mem_prev will
		// track the node before ‘mem_free_cur’ for bookkeeping/surgery later.
		// ------------------------------------------------------------
		lda #<mem_free_stub
		sta mem_prev_lo
		lda #>mem_free_stub
		sta mem_prev_hi

check_candidate:
		// ------------------------------------------------------------
		// Read the candidate’s size from its header
		// ------------------------------------------------------------
		ldy #MEM_HDR_OFS_SIZE_LO
		lda (mem_free_cur_lo),Y
		sta candidate_size_lo
		iny
		lda (mem_free_cur_lo),Y
		sta candidate_size_hi

		// ------------------------------------------------------------
		// Fit test: does candidate >= mem_req_total ?
		// 
		// Compute (candidate - mem_req_total). 
		// If a borrow occurs (BCC), then candidate < mem_req_total → it cannot fit → skip to next block.
		// ------------------------------------------------------------
		sec
		lda candidate_size_lo
		sbc mem_req_total_lo
		lda candidate_size_hi
		sbc mem_req_total_hi
		bcc move_to_next_block            // too small → examine next candidate

		// ------------------------------------------------------------
		// DATA FITS - check whether this candidate improves the current best-fit.
		// 
		// Compare (mem_best_size - candidate_size):
		// 
		// If borrow (BCC) → candidate is LARGER than current best → not better.
		// If no borrow    → candidate is <= current best         → accept later.
		// ------------------------------------------------------------
		sec
		lda mem_best_size_lo
		sbc candidate_size_lo
		lda mem_best_size_hi
		sbc candidate_size_hi
		bcc move_to_next_block            // candidate > best → skip, keep current best

		// ------------------------------------------------------------
		// NEW BEST-FIT FOUND
		// 
		// This call to "mem_read_next_ptr" is not really needed, as X/Y will get overwritten
		// again by the following "mem_read_next_ptr" call later on - it doesn't harm either
		// Keeping it here as it's present in the original code
		// ------------------------------------------------------------
		jsr mem_read_next_ptr		

		// Record the new minimum (best) size.
		lda candidate_size_lo
		sta mem_best_size_lo
		lda candidate_size_hi
		sta mem_best_size_hi

		// Record the address of the new best-fit block.
		lda mem_free_cur_lo
		sta mem_best_blk_lo
		lda mem_free_cur_hi
		sta mem_best_blk_hi

		// Record the address of the predecessor
		lda mem_prev_lo
		sta predecessor_lo
		lda mem_prev_hi
		sta predecessor_hi

move_to_next_block:
		// ------------------------------------------------------------
		// Remember the current candidate as 'mem_prev' (predecessor tracking).
		// Useful for later list surgery, though not directly used in this pass.
		// ------------------------------------------------------------
		lda mem_free_cur_lo
		sta mem_prev_lo
		lda mem_free_cur_hi
		sta mem_prev_hi

		// Advance the scan to the next free block in the list.
		jsr mem_read_next_ptr
		stx mem_free_cur_lo
		sty mem_free_cur_hi

		// Continue scanning while mem_free_cur != $0000.
		bne check_candidate

		// ------------------------------------------------------------
		// END OF SCAN - commit best-fit (if any) and allocate
		// Load the winner back into mem_free_cur so mem_alloc_from_free operates on it.
		// ------------------------------------------------------------
		lda mem_best_blk_lo
		sta mem_free_cur_lo
		lda mem_best_blk_hi
		sta mem_free_cur_hi

		// If mem_best_blk == $0000 → no fitting block was found → return with Z=1.
		bne free_block_found
		rts

free_block_found:
		// Consume/split the chosen free block to satisfy the request.
		jsr mem_alloc_from_free

		// ------------------------------------------------------------
		// Return the allocated block pointer to caller:
		//  X = lo(mem_best_blk), Y = hi(mem_best_blk), Z=0 (success).
		// ------------------------------------------------------------
		ldx mem_best_blk_lo
		ldy mem_best_blk_hi
		rts
/*
================================================================================
  mem_compact_then_release
================================================================================
Summary:
    Recovery path for failed allocations. Compact the free list, retry a
    best-fit allocation using the preserved request size, and if that still
    fails, evict lower-priority resources and try again until either a block
    is obtained or nothing remains to release.

Arguments:
    mem_req_payload					16-bit requested payload size (bytes)

Vars/State:
    mem_req_saved					Preserved copy of mem_req_payload across compaction/retry attempts.
	mem_free_head / mem_free_tail	Free-list head/tail used by compaction and coalescing.
									
Returns:
    On success:
		Z = 0 (BNE taken in caller paths)
		X/Y = pointer to allocated block header (lo/hi).

    On failure:
		Z = 1 (no fit and no more resources to evict); X/Y undefined.

Description:
		1) Save the requested payload size into mem_req_saved and set
			mem_req_payload := $FFFF as an “allocation in progress” sentinel.
		2) Run mem_compact_leading_free to bubble USED blocks left over the
			leading FREE region and coalesce any new right-adjacent FREE blocks.
		3) Restore mem_req_saved into X/Y and call mem_alloc_bestfit:
			- If a block is found, return with Z=0 and X/Y = header pointer.
		4) If there is still no fit, call rsrc_evict_one_by_priority:
			- If it evicts something (Z=0), loop back to step 2 and try again.
			- If it cannot evict anything (Z=1), exit with failure (no block).
================================================================================
*/
* = $58D5
mem_compact_then_release:
		// Save caller’s requested payload size
		lda mem_req_payload_lo
		sta mem_req_saved_lo
		lda mem_req_payload_hi
		sta mem_req_saved_hi
				
		// mem_req_payload ← $FFFF (sentinel to indicate “recompute/use saved size”)
		lda #$FF
		sta mem_req_payload_lo
		lda #$FF
		sta mem_req_payload_hi

        // ------------------------------------------------------------
		// Try to make a large contiguous free region by pushing leading
		// free space toward higher addresses and merging neighbors.
        // ------------------------------------------------------------
		jsr mem_compact_leading_free

        // ------------------------------------------------------------
		// Reattempt allocation with the original payload size
		// On success, mem_alloc_bestfit returns with Z=0 → branch to exit.
        // ------------------------------------------------------------
		ldx mem_req_saved_lo
		ldy mem_req_saved_hi
		jsr mem_alloc_bestfit
		bne mctr_exit

        // ------------------------------------------------------------
		// Still no block - release resources by priority and retry.
		// rsrc_evict_one_by_priority returns Z=0 when something was released; loop if so.
        // ------------------------------------------------------------
		jsr rsrc_evict_one_by_priority
		bne mem_compact_then_release

mctr_exit:
		rts
/*
================================================================================
  mem_read_next_ptr
================================================================================
Summary:
    Reads the 16-bit “next” pointer from a block header and returns it in X/Y.
 
Arguments:
    mem_hdr_ptr				Pointer to current block header
 
Returns:
    X/Y                     Next block pointer (lo in X, hi in Y). 
  							
Notes:
    Block header layout:
      +0..+1 : size (bytes, includes header)
      +2..+3 : next pointer (16-bit)
================================================================================
*/
* = $58FF
mem_read_next_ptr:
		// Y = 2 → address the “next” field (low byte) in the block header
		ldy #MEM_HDR_OFS_NEXT_LO
		
		// Load low byte of next pointer → X
		lda (mem_hdr_ptr),Y
		tax
		
		// Load high byte of next pointer → Y
		iny
		lda (mem_hdr_ptr),Y
		tay
		rts
/*
================================================================================
  mem_alloc_from_free
================================================================================ 
Summary:
    Carves the requested block (HEADER + payload) from the selected free node.
    If the true remainder is ≥ 4 bytes (enough for a free header), splits and
    creates a trailing free node; otherwise, consumes the whole free node.
    Updates predecessor.next and keeps mem_free_tail correct.
 
Arguments:
    mem_free_cur			Pointer to the chosen free block header.
    mem_best_size       	16-bit size of the chosen free block
    mem_req_payload         16-bit requested payload size (header NOT included).
    predecessor		        Predecessor free block pointer (may be the head stub)
 
Returns / Updates:
    predecessor.next        new trailing free (split) or original next (consume).
    mem_split_tail          trailing free (split) or predecessor (consume) for uniform tail update.
    mem_split_rem    		(mem_best_size − mem_req_total), split path only.
    mem_free_tail         	updated if the chosen block had been the tail.
 
Notes:
    mem_req_total = mem_req_payload + 4 (payload size + header size).
    Using a split threshold of ≥ 4 permits header-only free nodes
 
Description:
	Computes mem_split_rem = mem_best_size - mem_req_payload
 
    If mem_split_rem ≥ 4, the routine SPLITS:
		• mem_split_tail = mem_free_cur + mem_req_payload
		• predecessor.next ← mem_split_tail
		• mem_split_tail.next  ← mem_free_cur.next
		• mem_split_tail.size  ← mem_split_rem
 
    Otherwise it CONSUMES the entire free block:
		• predecessor.next ← mem_free_cur.next
		• mem_req_payload ← mem_free_sz  (caller receives whole block)
		• mem_split_tail ← predecessor (simplifies tail update)
 
    Finally, if the chosen block was the list tail, mem_free_tail is updated to the
    trailing free block (or stub) so the tail pointer remains correct.
================================================================================
 */
* = $5909
mem_alloc_from_free:
		// ------------------------------------------------------------
		// Compute how much of the chosen free block will remain after carving out
		// the requested payload:
		// 
		// mem_split_rem = mem_best_size - mem_req_payload
		// ------------------------------------------------------------
		sec
		lda mem_best_size_lo
		sbc mem_req_payload_lo
		sta mem_split_rem_lo
		lda mem_best_size_hi
		sbc mem_req_payload_hi
		sta mem_split_rem_hi

		// ------------------------------------------------------------
		// Split vs. Consume decision
		// ------------------------------------------------------------		
		// Is there enough space left to create a NEW free block header?
		// 
		// We need at least 4 bytes. 
		// Perform a 16-bit compare: (mem_split_rem - 4). 
		// If borrow occurs (BCC), there is NOT enough space to split → consume the whole block.
		// Note: C=1 & result==0 means remainder==4 ⇒ header-only free node
		// ------------------------------------------------------------
		sec
		lda mem_split_rem_lo
		sbc #<MEM_HDR_LEN           // subtract header size (low)
		lda mem_split_rem_hi
		sbc #>MEM_HDR_LEN          	// subtract header size (high)
		bcc consume_whole_block     // C=0 → mem_split_rem < 4

		// ------------------------------------------------------------
		// Split block
		// ------------------------------------------------------------
		// 
		// Carve payload from the front of the chosen free block and create a NEW trailing 
		// free block with the leftover space. 
		// The new block will start at mem_split_tail.
		// 
		// mem_split_tail = mem_free_cur + mem_req_payload
		// ------------------------------------------------------------
		clc
		lda mem_free_cur_lo
		adc mem_req_payload_lo
		sta mem_split_tail_lo
		lda mem_free_cur_hi
		adc mem_req_payload_hi
		sta mem_split_tail_hi

		// ------------------------------------------------------------
		// Link the free list to the new trailing block
		// 
		// Update the predecessor's 'next' to point at mem_split_tail.
		// ------------------------------------------------------------
		ldy #MEM_HDR_OFS_NEXT_LO
		lda mem_split_tail_lo
		sta (predecessor_lo),Y     // write next.lo
		iny
		lda mem_split_tail_hi
		sta (predecessor_lo),Y     // write next.hi

		// ------------------------------------------------------------
		// Chain the new trailing free block into the list
		// 
		// Read mem_free_cur->next into X/Y, then store into mem_split_tail header (+2..+3).
		// 
		// mem_split_tail.next = mem_free_cur.next
		// ------------------------------------------------------------
		jsr mem_read_next_ptr
		sty temp                 	// stash hi byte temporarily
		ldy #MEM_HDR_OFS_NEXT_LO
		txa
		sta (mem_split_tail_lo),Y   // header[2] = next.lo
		iny
		lda temp
		sta (mem_split_tail_lo),Y   // header[3] = next.hi

		// ------------------------------------------------------------
		// Initialize the size of the new trailing free block
		// 
		// mem_split_tail.size = mem_split_rem  (header +0..+1)
		// ------------------------------------------------------------
		ldy #MEM_HDR_OFS_SIZE_LO
		lda mem_split_rem_lo
		sta (mem_split_tail_lo),Y   // header[0] = size.lo
		iny
		lda mem_split_rem_hi
		sta (mem_split_tail_lo),Y   // header[1] = size.hi
		jmp tail_adjust_check  		// update tail pointer if needed

consume_whole_block:
		// ------------------------------------------------------------
		// Consume whole block
		// ------------------------------------------------------------
		// Not enough room to hold a new free header (mem_split_rem < 4):
		// consume the ENTIRE chosen free block for this allocation.
		// 
		// Splice the chosen block out of the free list:
		//  predecessor.next = mem_free_cur.next
		// ------------------------------------------------------------
		jsr mem_read_next_ptr
		sty temp
		ldy #MEM_HDR_OFS_NEXT_LO
		txa
		sta (predecessor_lo),Y      // write next.lo
		iny
		lda temp
		sta (predecessor_lo),Y      // write next.hi

		// ------------------------------------------------------------
		// Caller will use the whole block: set requested size to block size.
		// 
		// (mem_req_payload now equals mem_best_size)
		// ------------------------------------------------------------
		lda mem_best_size_lo
		sta mem_req_payload_lo
		lda mem_best_size_hi
		sta mem_req_payload_hi

		// ------------------------------------------------------------
		// No trailing free block is created in this case.
		// 
		// Set mem_split_tail to the list “table” placeholder so downstream
		// tail-adjustment logic can treat both paths uniformly.
		// ------------------------------------------------------------
		lda predecessor_lo
		sta mem_split_tail_lo
		lda predecessor_hi
		sta mem_split_tail_hi

tail_adjust_check:
		// ------------------------------------------------------------
		// Adjust free list tail if needed
		// ------------------------------------------------------------
		// Was the CHOSEN free block also the TAIL of the free list?
		// 
		// If mem_free_tail == mem_free_cur, the block we just split/consumed was the tail.
		// ------------------------------------------------------------
		lda mem_free_tail_lo
		cmp mem_free_cur_lo
		bne tail_adjust_check_else
		
		lda mem_free_tail_hi
		cmp mem_free_cur_hi
		
tail_adjust_check_else:
		// If it wasn't, nothing to adjust, exit
		bne mem_exit

		// ------------------------------------------------------------
		// Yes: update the tail to the new trailing free block produced by this op.
		// 
		// In the split path, mem_split_tail = (mem_free_cur + mem_req_payload).
		// In the “no split” path, mem_split_tail was set to the table placeholder
		// so downstream logic treats both cases uniformly.
		// ------------------------------------------------------------
		lda mem_split_tail_lo
		sta mem_free_tail_lo                  
		lda mem_split_tail_hi
		sta mem_free_tail_hi
mem_exit:
		rts
/*
================================================================================
  mem_compact_leading_free
================================================================================
Summary:
    Iteratively “bubbles” used blocks to the right over the first free block,
    pushing the free space toward higher addresses. After each bubble, merges
    adjacent free blocks. Repeats until no more right-adjacent used blocks
    remain or the free list is exhausted.
 
Preconditions / Invariants:
    - mem_free_cur points to the header of the current leading free block.
    - ZP aliasing: mem_free_cur and mem_dst share storage (mem_dst=free head).
    - Null pointers are detected via hi byte == 0 (no headers live in page $00).
 
Arguments / Inputs:
    mem_free_head         		 Head pointer of the free list (lo/hi).

Updates:
    mem_free_head          		May be updated by 'mem_bubble_used_left' to new free head.
	
Description: 
	-Loads the head of the free list into ‘mem_free_cur’, reads its size into
	‘mem_free_sz’, and computes ‘adjacent_block = mem_free_cur + mem_free_sz’.

	-It then fetches mem_free_cur->next:
		• If no next free block exists, the routine returns (nothing to push).
		• Otherwise, it:
			1) Calls mem_bubble_used_left to relocate any used blocks that sit immediately
			 to the right of the leading free block, thereby sliding the free
			 region upward in memory (toward higher addresses).
			2) Calls mem_coalesce_right to merge contiguous free neighbors that
			 may have become adjacent after the relocation.
			3) Loops back to re-evaluate the (possibly updated) head until no more
			 movement is needed.

	The end result is a more compacted free region near the top, improving the
	chance of satisfying larger allocations.
================================================================================
*/
* = $59AA
mem_compact_leading_free:
		// ------------------------------------------------------------
		// Load the head of the free list into the working pointer ‘mem_free_cur’.
		// This is the leading free region we try to push “to the back” (higher addrs).
		// ------------------------------------------------------------
		// mem_free_head -> mem_free_cur
		lda mem_free_head_lo
		sta mem_free_cur_lo
		lda mem_free_head_hi
		sta mem_free_cur_hi

		// ------------------------------------------------------------
		// Read the free block’s size from its header -> mem_free_sz
		// ------------------------------------------------------------
		ldy #MEM_HDR_OFS_SIZE_LO
		lda (mem_free_cur_lo),Y
		sta mem_free_sz_lo
		iny
		lda (mem_free_cur_lo),Y
		sta mem_free_sz_hi


		// ------------------------------------------------------------
		// Compute the address of the block immediately to the right:
		//  mem_src = mem_free_cur + mem_free_sz
		// ------------------------------------------------------------
		clc
		lda mem_free_cur_lo
		adc mem_free_sz_lo
		sta mem_src_lo
		lda mem_free_cur_hi
		adc mem_free_sz_hi
		sta mem_src_hi

		// ------------------------------------------------------------
		// Read the singly-linked list pointer:
		// 
		// mem_read_next_ptr → X=next.lo, Y=next.hi, Z set from Y (TAY inside).
		// BNE means Y≠0 → non-null next (relies on “no headers in page $00” invariant).
		// ------------------------------------------------------------
		jsr mem_read_next_ptr
		bne next_block_present
		rts

next_block_present:
		// mem_src->free   -> mem_next_free
		stx mem_next_free_lo
		sty mem_next_free_hi

		// ------------------------------------------------------------
		// Relocate any USED blocks that sit immediately to the right of the
		// leading free block so that the free region moves “back” (to higher
		// addresses). This may repeat internally until the next free block
		// is adjacent to ‘mem_dst’.
		//
		// ------------------------------------------------------------
		//
		// mem_free_cur (alias of mem_dst) already points at the FREE head.
		// mem_src = mem_free_cur + size points at the first USED block to the right.
		// mem_next_free is the next FREE block after the USED run.
		// 
		// mem_bubble_used_left will:
		//  - Copy each USED block (header+payload) from mem_src → mem_dst,
		//  - Advance pointers until mem_src >= mem_next_free,
		//  - Rebuild the free header at the new mem_dst and update mem_free_head.	   
		// ------------------------------------------------------------
		jsr mem_bubble_used_left

		// The relocated free head may now be adjacent to the next free block; merge them.
		jsr mem_coalesce_right

		// Re-evaluate the (possibly changed) head; repeat until there is no next free block.		   
		jmp mem_compact_leading_free
/*
================================================================================
  mem_bubble_used_left
================================================================================ 
Summary:
    Copies each USED block immediately to the right of the leading FREE block
    into the FREE region at 'mem_dst' (which is the leading FREE block),
    thereby shifting USED blocks toward lower addresses and pushing FREE space
    toward higher addresses.
 
    Repeats while mem_src < mem_next_free. 
	
	When mem_src reaches mem_next_free, rebuilds the free header at the 
	new 'mem_dst' and updates the list head.
 
    After each move, external pointers are updated; a sound reload is requested
    only if the moved block is an in-use sound.
 
Preconditions:
    mem_dst 		= address of the leading FREE block (start of the hole)
    mem_src      	= mem_dst + mem_free_sz (first USED block to the right)
    mem_next_free  	= pointer to the next FREE block after the current run of USED blocks
 
Arguments:
    mem_src			USED block being moved (points to its HEADER)
    mem_dst       	FREE region to receive the move (points to where HEADER will be written)
    mem_next_free   NEXT FREE block after the current run of USED blocks
 
State:
    mem_free_sz   	original size (bytes) of the leading FREE block
    mem_used_sz     size of the used block being moved.
 
Updates:
    mem_src                     Advanced past the moved used block(s).
    mem_dst                   	Advanced to the end of the moved region.
    mem_free_head              	Updated to the rebuilt free block at the new mem_dst.
    reload_sound_ptrs_flag      Cleared to #$00 before exit.
 
Details:
    This routine “bubbles” used blocks to the right into the leading free space, thereby
    accumulating a larger contiguous free region. 
 
  	For each adjacent used block:
 
      1) Read its size and metadata.
      2) Optionally set reload_sound_ptrs_flag if a sound block is in-use.
      3) Call mem_copy_pages to copy the entire block from ‘mem_src’ → ‘mem_dst’.
      4) Call rsrc_update_ptr(metadata) so external references follow the move.
      5) Advance both pointers (mem_src, mem_dst) by the moved size and loop while more used blocks remain
         before ‘mem_next_free’.
 
    Once the next free block is adjacent, the routine updates mem_free_head and writes
    the free block header (size + next pointer) at ‘mem_dst’, completing the compaction step.
 
Notes:
    - Copy direction is forward and safe for overlap because mem_dst < mem_src.
    - Sound resources: if in use, set 'reload_sound_ptrs_flag' during the move; it is consumed
      by 'rsrc_update_ptr' and then cleared before the next iteration.
================================================================================
*/
* = $59E5
mem_bubble_used_left:
		// Conventions: mem_src=USED block (header addr), mem_dst=FREE hole (header addr); mem_next_free=next FREE block
		
		// ------------------------------------------------------------
		// Read size of the USED block from its header at 'mem_src' -> mem_used_sz
		// Size is of the full block: header + payload.
		// ------------------------------------------------------------
		ldy #MEM_HDR_OFS_SIZE_LO
		lda (mem_src_lo),Y
		sta mem_used_sz_lo             
		iny
		lda (mem_src_lo),Y
		sta mem_used_sz_hi         
		iny                   		// Y now points at resource metadata (type/index)

		// ------------------------------------------------------------
		// Read resource metadata used for pointer fix-ups after relocation:
		// 
		// +2 = resource_type  (e.g., sound, graphics, etc.)
		// +3 = resource_index (which specific instance)
		// ------------------------------------------------------------
		lda (mem_src_lo),Y
		sta resource_type
		pha                        	// save resource_type
		iny
		lda (mem_src_lo),Y
		sta resource_index
		tay                        	// Y = resource_index (for table lookups)
		pla                        	// A = resource_type (restored)

		// ------------------------------------------------------------
		// If the moved block is a SOUND resource and marked “in use”,
		// set a flag so dependent pointers will be refreshed after relocation.
		// ------------------------------------------------------------
		// Resource is a sound?
		cmp #RSRC_TYPE_SOUND        // resource_type == sound?
		bne copy_block_data         // no → continue

		// Is it in use?
		lda sound_liveness_tbl,Y    
		beq copy_block_data         // 0 → not in use → continue

		// Reload sound resource pointers later
		lda #TRUE
		sta reload_sound_ptrs_flag    

copy_block_data:
		// ------------------------------------------------------------
		// Prepare argument block for mem_copy_pages:
		// 
		// mem_copy_tail 		= tail bytes on the last page      	(low  byte of mem_used_sz)
		// mem_copy_page_cnt 	= full 256-byte pages to copy      	(high byte of mem_used_sz)
		// mem_read_ptr 		= mem_src pointer (lo/hi)       	(address of the USED block HEADER)
		// mem_write_ptr 		= mem_dst pointer (lo/hi)  			(address of FREE space to fill)
		// 
		// Copy length equals mem_used_sz (header + payload).
		// ------------------------------------------------------------
		lda mem_used_sz_lo
		sta mem_copy_tail
		lda mem_used_sz_hi
		sta mem_copy_page_cnt

		lda mem_src_lo
		sta mem_read_ptr_lo
		lda mem_src_hi
		sta mem_read_ptr_hi

		lda mem_dst_lo
		sta mem_write_ptr_lo
		lda mem_dst_hi
		sta mem_write_ptr_hi

		// ------------------------------------------------------------
		// Perform the relocation copy (mem_src → mem_dst).
		// ------------------------------------------------------------
		jsr mem_copy_pages

		// ------------------------------------------------------------
		// Fix external references to the moved block.
		// X = resource_index, Y = resource_type.
		// ------------------------------------------------------------
		ldx resource_index
		ldy resource_type
		jsr rsrc_update_ptr

		// Clear the “reload sound pointers” request flag
		lda #FALSE
		sta reload_sound_ptrs_flag

		// ------------------------------------------------------------
		// Advance src and dest pointers by the size of the moved USED block:
		// 
		// mem_dst 		← mem_dst + mem_used_sz
		// mem_src      ← mem_src + mem_used_sz
		// 
		// Invariant preserved: mem_dst < mem_src.
		//
		// mem_dst now points to:	 the start of the remaining FREE space (immediately after the moved USED block) 
		// mem_src now points to:	 the block that originally followed the moved USED block.
		// ------------------------------------------------------------
		// mem_dst += mem_used_sz
		clc                               
		lda mem_dst_lo
		adc mem_used_sz_lo                   
		sta mem_dst_lo
		lda mem_dst_hi
		adc mem_used_sz_hi                
		sta mem_dst_hi

		// mem_src += mem_used_sz
		clc
		lda mem_src_lo
		adc mem_used_sz_lo                   
		sta mem_src_lo
		lda mem_src_hi
		adc mem_used_sz_hi
		sta mem_src_hi


		// ------------------------------------------------------------
		// The next free block can either:
		//
		// 	-reside immediately after the moved free block
		// 	-reside higher, with at least one block in between
		// 
		// If it resides immediately after, we now have two consecutive free blocks, which can be coalesced.
		// If it resides higher, then there's at least one used block in the middle and we can move it to the front.
		// ------------------------------------------------------------
		//
		// Determine if the next free block follows immediately or not
		//
		// Check if there are still USED blocks between our current 'mem_src'
		// and the next FREE block ('mem_next_free'). 
		// 
		// We perform a 16-bit compare:
		// 
		// 		Compute (mem_src - mem_next_free) with SEC/SBC across low, then high.
		//
		// If result < 0 → borrow → C=0 → mem_next_free > mem_src → keep bubbling left.
		// If result ≥ 0 → C=1 → we've reached/passed mem_next_free → stop bubbling.
		// ------------------------------------------------------------
		sec
		lda mem_src_lo
		sbc mem_next_free_lo        // low-byte: mem_src - mem_next_free
		lda mem_src_hi
		sbc mem_next_free_hi        // high-byte with borrow propagation
		
		// If C clear → mem_next_free > mem_src → more used blocks remain to bubble, loop this whole routine
		bcc mem_bubble_used_left

		// ------------------------------------------------------------
		// An adjacent FREE block now follows the relocated region,
		// and resides at mem_dst.
		// ------------------------------------------------------------
		
		// Update the free-list HEAD to point at 'mem_dst' (the newest free block)
		lda mem_dst_lo
		sta mem_free_head_lo
		lda mem_dst_hi
		sta mem_free_head_hi

		// ------------------------------------------------------------
		// Rebuild the FREE block header at 'mem_dst':
		// 		size = mem_free_sz
		// 		next = mem_next_free
		// ------------------------------------------------------------
		ldy #MEM_HDR_OFS_SIZE_LO
		lda mem_free_sz_lo
		sta (mem_dst_lo),Y              
		iny
		lda mem_free_sz_hi
		sta (mem_dst_lo),Y              
		iny
		lda mem_next_free_lo
		sta (mem_dst_lo),Y              
		iny
		lda mem_next_free_hi
		sta (mem_dst_lo),Y              
		rts
/*
================================================================================
  mem_copy_pages
================================================================================
 
Arguments:
    mem_copy_tail        	Bytes to copy on the final (last) page (0..255)
    mem_copy_page_cnt       Number of full 256-byte pages to copy
    mem_read_ptr            Source pointer (lo/hi)
    mem_write_ptr           Destination pointer (lo/hi)
 
Returns:
    Copies (mem_copy_page_cnt * 256 + mem_copy_tail) bytes from mem_read_ptr to mem_write_ptr.
    On exit, mem_read_ptr/mem_write_ptr have been advanced by the total copied size.
 
Notes:
    - If mem_copy_page_cnt == 0 and mem_copy_tail == 0 → nothing to do.
    - The routine copies page-by-page; for non-final pages it copies 256 bytes
      by temporarily setting mem_copy_tail = $00 (meaning 256 via CPY logic).
================================================================================
*/
* = $5C13
mem_copy_pages:
		// Save tail (final-page byte count) on the stack
		lda mem_copy_tail
		pha
		
		// Use .X as page counter
		ldx mem_copy_page_cnt
next_page:
		beq last_page               // X==0 → process the final partial page
		
		// Not the last page: copy a full 256-byte page
		// Clear the tail count
		lda #$00
		sta mem_copy_tail
		//Copy a page
		jmp copy_page

		// It's the last page: restore the tail count
last_page:
		pla
		sta mem_copy_tail
		bne copy_page               // 0 means nothing left to copy → RTS
		rts

		// Copy from mem_read_ptr to mem_write_ptr until Y reaches mem_copy_tail
copy_page:
		ldy #$00
copy_loop:
		lda (mem_read_ptr_lo),Y
		sta (mem_write_ptr_lo),Y
		iny
		cpy mem_copy_tail
		bne copy_loop

		// Finished this page: advance mem_src/dest to next page, decrement page count
		inc mem_read_ptr_hi
		inc mem_write_ptr_hi
		dex
		
		//Do next page
		bpl next_page
		rts
/*
================================================================================
  init_heap_free_list
================================================================================
Summary
	Initialize the allocator with a single free-list node. Writes the initial
	free block header at HEAP_FREE_HDR_ADDR with size HEAP_INIT_SIZE_BYTES and
	null next pointer, then points mem_free_head and mem_free_tail to it.

Global Outputs
	[HEAP_FREE_HDR_ADDR+0]      size_lo := <HEAP_INIT_SIZE_BYTES
	[HEAP_FREE_HDR_ADDR+1]      size_hi := >HEAP_INIT_SIZE_BYTES
	[HEAP_FREE_HDR_ADDR+2]      next_lo := $00
	[HEAP_FREE_HDR_ADDR+3]      next_hi := $00
	mem_free_head               := HEAP_FREE_HDR_ADDR
	mem_free_tail               := HEAP_FREE_HDR_ADDR

Description
	- Store the size of the initial free span into the header (lo, then hi).
	- Null-terminate the “next” link to mark a single-node list.
	- Seed both head and tail pointers with the header address.
	- Result: a 1-node free list covering [HEAP_FREE_HDR_ADDR,
	HEAP_END_ADDR_EXCL), ready for first-fit/split logic elsewhere.
================================================================================
*/
* = $5D4C		
init_heap_free_list:
		// ------------------------------------------------------------
		// Write initial block size into free-block header
		// Sets header[+0] = size_lo and header[+1] = size_hi from
		// HEAP_INIT_SIZE_BYTES, defining the span up to HEAP_END_ADDR_EXCL.
		// ------------------------------------------------------------
        lda     #<HEAP_INIT_SIZE_BYTES           
        sta     HEAP_FREE_HDR_ADDR               // header[+0] := size_lo

        lda     #>HEAP_INIT_SIZE_BYTES           
        sta     HEAP_FREE_HDR_ADDR + 1           // header[+1] := size_hi

		// ------------------------------------------------------------
		// Null-terminate free-list link
		// Zero header[+2..+3] (next_lo/next_hi) to mark end of list.
		// ------------------------------------------------------------
        lda     #$00                             
        sta     HEAP_FREE_HDR_ADDR + 2           // header[+2] := next_lo = NULL
        lda     #$00                             
        sta     HEAP_FREE_HDR_ADDR + 3           // header[+3] := next_hi = NULL

		// ------------------------------------------------------------
		// Seed free-list head and tail with header address
		// Points both mem_free_head and mem_free_tail to HEAP_FREE_HDR_ADDR,
		// establishing a single-node free list at startup.
		// ------------------------------------------------------------
        lda     #<HEAP_FREE_HDR_ADDR             
        sta     mem_free_head_lo                   
        sta     mem_free_tail_lo                   

        lda     #>HEAP_FREE_HDR_ADDR             
        sta     mem_free_head_hi
        sta     mem_free_tail_hi

        rts                                      // return with a 1-node free list

/*
  ===========================================
  mem_alloc_from_free (graphical explanation)
  ===========================================
  
  Addresses grow → right
 
  Names:
    pred             = predecessor node (may be the head stub); we write pred.+2..+3
    mem_free_cur       = chosen FREE block to satisfy the request
    old_next         = mem_free_cur.next (the next FREE block in the list)
    mem_best_size= size(mem_free_cur)   ; 16-bit
 
  ----------------------------------------------------------------------
  initial state (before allocation)
 
  Free blocks linked list:
 
     pred  ──►  mem_free_cur  ──►  old_next  ──►  ...
 
  Memory:
     [ FREE: mem_best_size @ mem_free_cur ]  [ ... ]
 
  Goal: carve space for the request from mem_free_cur, then fix links and tail.
 
  ----------------------------------------------------------------------
  A) Split path
 
  Free blocks linked list:
 
  Before 
    pred ──► mem_free_cur ──► old_next
             [ FREE: mem_best_size ]
 
  After split (payload carve)
    pred ──► mem_split_tail ──► old_next
             [ ALLOCATED ] [ FREE: mem_split_rem ]
                 ^ at mem_free_cur       ^ at mem_free_cur+mem_req_payload
 
    (Note: trailing FREE header sits after payload bytes, not after header+payload.)
 
  ----------------------------------------------------------------------
  B) Consume path
 
  Free blocks linked list:
 
  Before
    pred ──► mem_free_cur ──► old_next
             [ FREE: mem_best_size ]
 
  After consume
    pred ──► old_next
    [ ALLOCATED takes entire mem_free_cur region ]
 
  ----------------------------------------------------------------------
 */

/*
  ===========================================
  mem_bubble_used_left (graphical explanation)
  ===========================================
  Legend
 
  [FREE:F]     free block of constant size F  (header+payload bytes)
  [U# :S#]     used block # with size S#      (header+payload bytes)
  dest         = mem_dst (start of FREE region / “hole”)
  src          = mem_src      (start of first USED block header to the right)
  next_free    = next FREE block after the run of USED blocks
 
  Invariant: dest < src ≤ next_free, and (src - dest) == F at loop entry.
 */

/*
  addresses → increasing
  ┌──────────────────────────────────────────────────────────────────────────┐
  │  dest = A                                                                │
  │  src  = A + F                                                            │
  │                                                                          │
  │  A: [FREE:F] [U1:S1] [U2:S2] … [Uk:Sk] [FREE: G (next_free)]             │
  │       ^dest     ^src                              ^next_free             │
  └──────────────────────────────────────────────────────────────────────────┘
 
  dest := dest + Si
  src  := src  + Si
 
  Loop condition:
    if src < next_free  → more USED blocks remain → repeat
    if src ≥ next_free  → run finished → finalize
   
  One iteration
  -------------
  BEFORE:
     … [FREE:F] [Ui:Si] [Ui+1:Si+1] … [FREE:G] …
         ^dest    ^src
 
  AFTER COPY + ADVANCE:
     … [Ui:Si] [FREE:F] [Ui+1:Si+1] … [FREE:G] …
                  ^dest     ^src
 
  Note: the hole of size F “slides right” by Si bytes. Distance (src - dest) stays == F.
 
 
  Multiple iterations
  -------------------
  Step 0:
    A: [FREE:F] [U1:S1] [U2:S2] [U3:S3] [FREE:G]
          ^dest    ^src                     ^next_free
 
  Step 1 (move U1):
    A: [U1:S1] [FREE:F] [U2:S2] [U3:S3] [FREE:G]
                  ^dest    ^src
 
  Step 2 (move U2):
    A: [U1:S1] [U2:S2] [FREE:F] [U3:S3] [FREE:G]
                          ^dest    ^src
 
  Step 3 (move U3):
    A: [U1:S1] [U2:S2] [U3:S3] [FREE:F] [FREE:G]
                                  ^dest   ^src == next_free
  Done moving: src >= next_free
 */
  
/*  
Below is unified pseudocode for every routine in the file, with 8-bit details removed. 
All addresses are abstract pointers or integer offsets; all sizes are in bytes.

	Block is a struct:
		size: u16 				// bytes, header + payload
		next: Block* | null 	// for FREE blocks only
		type: ResourceType 		// for USED blocks
		index: ResourceId 		// for USED blocks

“FREE list” is a singly linked list of FREE blocks: mem_free_head, mem_free_tail, mem_free_stub.next.  

// Return the "next" pointer stored in a FREE block header.
function mem_read_next_ptr(block: Block*) -> Block*:
    return block.next


// Allocate a block for a payload of given size (bytes) and return header pointer.
function mem_alloc(request_size: u16) -> Block* | null:
    // Preserve original request for possible retries.
    mem_req_size = request_size

    // First attempt: direct best-fit allocation.
    block = mem_alloc_bestfit(request_size)
    if block != null:
        return block

    // Recovery step 1: compact + coalesce + eviction loop.
    block = mem_compact_then_release(request_size)
    if block != null:
        return block

    // Recovery step 2: release one costume, restore the original size, and retry.
    rsrc_unlock_or_detach_costume()

    // Retry original request using best-fit again.
    return mem_alloc(mem_req_size)


// Free a USED block and insert it into the FREE list.
function mem_release(block: Block*):
    // Snapshot current tail of FREE list.
    old_tail = mem_free_tail

    // Make released block the new tail.
    mem_free_tail = block

    // Link old tail → new tail.
    if old_tail != null:
        old_tail.next = block

    // Terminate the list at the new tail.
    block.next = null

    // Normalize by address: sort and coalesce.
    mem_sort_free_ao()
    mem_coalesce_right()

    // Signal to resource management that something was released.
    rsrc_released_flag = true


// Merge adjacent FREE blocks so that contiguous spans become larger.
function mem_coalesce_right():
    left = mem_free_head
    if left == null:
        return

    while true:
        left_size = left.size                         // in bytes
        right_addr = address(left) + left_size        // where a neighbor would start if contiguous

        next_block = mem_read_next_ptr(left)

        // No next block → we are at tail.
        if next_block == null:
            return

        // If next block is physically adjacent, merge it into left.
        if address(next_block) == right_addr:
            right = next_block

            // Splice right out of the list: left.next = right.next
            left.next = right.next

            // Grow left.size by right.size
            left.size = left.size + right.size

            // If right was the tail, move tail to left.
            if mem_free_tail == right:
                mem_free_tail = left

            // Stay at same left; there may be more right-adjacent blocks.
            continue
        else:
            // Not adjacent → advance to next block.
            left = next_block

            // If the new left is null, done.
            if left == null:
                return


// Sort FREE list by ascending address using an anchor-and-swap sweep.
function mem_sort_free_ao():
    anchor_block = &mem_free_stub   // synthetic header; its "next" is mem_free_head

    loop_pass:
        // Initialize per-pass state.
        min_offender = INVALID_ADDRESS  // sentinel; use something like 0xFFFF
        min_prev     = null
        min_next     = null

        curr_prev  = anchor_block
        anchor_next = mem_read_next_ptr(anchor_block)

        // No next block → nothing left to sort.
        if anchor_next == null:
            goto no_more_blocks

        // Scan from anchor_next to the end to find the lowest-address offender.
        curr_block = anchor_next
        while curr_block != null:
            curr_next = mem_read_next_ptr(curr_block)

            // AO check: an "offender" is any block with address < anchor_next.
            if address(curr_block) < address(anchor_next):
                // Track the lowest-address offender seen so far.
                if min_offender == INVALID_ADDRESS or
                   address(curr_block) < address(min_offender):
                    min_offender = curr_block
                    min_prev     = curr_prev
                    min_next     = curr_next
                end if
            end if

            curr_prev  = curr_block
            curr_block = curr_next
        end while

        // If no offender found, just advance anchor.
        if min_offender == INVALID_ADDRESS:
            goto next_start_block

        // Swap min_offender into anchor_next's slot by pointer rewiring:
        //   anchor_block.next   = min_offender
        //   min_prev.next       = anchor_next
        //   min_offender.next   = anchor_next.next
        //   anchor_next.next    = min_next
        tmp_next = anchor_next.next

        anchor_block.next = min_offender
        min_prev.next     = anchor_next
        min_offender.next = tmp_next
        anchor_next.next  = min_next

    next_start_block:
        // Advance anchor.
        anchor_block = mem_read_next_ptr(anchor_block)
        if anchor_block != null:
            goto loop_pass

    no_more_blocks:
        // After sorting, anchor_block points at the final node (tail).
        mem_free_tail = anchor_block


// Compute total request size and invoke best-fit scan.
function mem_alloc_bestfit(payload_size: u16) -> Block* | null:
    mem_req_payload = payload_size
    mem_req_total   = payload_size + HEADER_SIZE_BYTES   // HEADER_SIZE_BYTES = 4

    // Start at head of FREE list.
    mem_free_cur = mem_free_head
    if mem_free_cur == null:
        return null

    return mem_bestfit_select(mem_req_total)


// Recovery path when best-fit fails: compact, then evict resources if needed.
function mem_compact_then_release(request_payload: u16) -> Block* | null:
    // Save requested payload size for retries.
    mem_req_saved   = request_payload
    mem_req_payload = SENTINEL_IN_PROGRESS  // e.g., 0xFFFF; internal marker

    // Step 1: compact leading FREE region and merge neighbors.
    mem_compact_leading_free()

    // Step 2: retry best-fit with original size.
    block = mem_alloc_bestfit(mem_req_saved)
    if block != null:
        return block

    // Step 3: eviction loop.
    while true:
        evicted = rsrc_evict_one_by_priority()
        if not evicted:
            return null   // nothing left to evict; allocation fails

        // After eviction, compact again and retry allocation.
        mem_compact_leading_free()
        block = mem_alloc_bestfit(mem_req_saved)
        if block != null:
            return block


// Split or consume a chosen FREE block to satisfy an allocation request.
procedure mem_alloc_from_free(
    chosen_free: Block*,    // mem_free_cur
    chosen_size: u16,       // mem_best_size
    mem_req_total: u16,     // requested header+payload size in bytes
    predecessor: Block*     // predecessor or stub
):
    remaining = chosen_size - mem_req_total

    if remaining < HEADER_SIZE_BYTES then
        // CONSUME WHOLE BLOCK — no room for a trailing FREE header.

        // Splice chosen block out of FREE list.
        predecessor.next = chosen_free.next

        // Caller effectively takes the entire chosen block.
        // (Callers may record the actual block size if needed.)
        mem_req_payload = chosen_size   // informational

        // For tail update we will treat predecessor as the "new tail" placeholder.
        split_tail = predecessor
    else
        // SPLIT — carve requested block and leave trailing FREE node.

        // The allocated block occupies [chosen_free .. chosen_free + mem_req_total).
        // Trailing FREE node starts immediately after the allocated block.
        split_tail_addr = address(chosen_free) + mem_req_total
        split_tail = (Block*)split_tail_addr

        // Link predecessor to the new trailing FREE block.
        predecessor.next = split_tail

        // New FREE node's "next" is what chosen_free used to point to.
        split_tail.next = chosen_free.next

        // Trailing FREE node gets "remaining" bytes.
        split_tail.size = remaining
    end if

    // If chosen_free was the tail, move tail to split_tail (either real FREE or stub).
    if mem_free_tail == chosen_free:
        mem_free_tail = split_tail


// Slide the leading FREE block “to the right” by bubbling USED blocks into it.
procedure mem_compact_leading_free():
    loop:
        head = mem_free_head
        if head == null:
            return

        // Size of leading FREE block (bytes).
        free_size = head.size

        // First USED block to the right of the hole.
        used_start = address(head) + free_size

        // Next FREE block somewhere after the USED run (or null if none).
        next_free = head.next
        if next_free == null:
            return   // nothing to push into; only one FREE block

        // Bubble USED blocks into the hole, sliding the hole toward next_free.
        mem_bubble_used_left(
            free_start   = address(head),
            used_start   = used_start,
            next_free    = next_free,
            free_size    = free_size
        )

        // After bubble, free head has moved; consecutive FREEs may now be adjacent.
        mem_coalesce_right()

        // Re-evaluate with new head and repeat.
        goto loop


// Move USED blocks into the leading FREE region until the next FREE block is reached.
procedure mem_bubble_used_left(
    free_start: Address,     // original FREE head address (hole start)
    used_start: Address,     // first USED block to the right of the hole
    next_free: Block*,       // first FREE block after the USED run
    free_size: u16           // size in bytes of the hole
):
    dst = free_start        // where free space currently begins
    src = used_start        // current USED block being inspected

    // Keep sliding USED blocks until src reaches next_free.
    while src < address(next_free):
        // Read USED block header at src.
        used_block = (Block*)src
        used_size   = used_block.size      // header + payload
        rtype       = used_block.type
        rindex      = used_block.index

        // If this is an in-use sound resource, request reload after relocation.
        reload_sound_ptrs_flag = false
        if rtype == RSRC_TYPE_SOUND and sound_block_is_in_use(rtype, rindex):
            reload_sound_ptrs_flag = true
        end if

        // Copy the entire USED block into the FREE region.
        mem_copy_pages(
            src_addr = src,
            dst_addr = dst,
            byte_count = used_size
        )

        // Fix external references to point at the new location.
        // New header location for this block is 'dst'.
        rsrc_update_ptr(rtype, rindex, dst)

        // Clear reload flag for next iteration.
        reload_sound_ptrs_flag = false

        // Advance src and dst by the size of this USED block.
        dst = dst + used_size
        src = src + used_size
    end while

    // At this point dst is immediately before next_free, and the hole still has
    // size = free_size. Rebuild FREE head at dst and keep its "next" link.
    new_free = (Block*)dst
    new_free.size = free_size
    new_free.next = next_free

    // Update FREE list head to point at the new hole.
    mem_free_head = new_free


// Copy an arbitrary number of bytes from one address to another.
procedure mem_copy_pages(src_addr: Address, dst_addr: Address, byte_count: u32):
    // In the 8-bit implementation, this is done page-by-page with special
    // handling for the final partial page. Here we treat it as a simple
    // contiguous copy.
    for i from 0 to byte_count - 1:
        memory[dst_addr + i] = memory[src_addr + i]


// Initialize FREE list with one big FREE span covering the heap.
procedure init_heap_free_list():
    // Create a single FREE block spanning [HEAP_FREE_HDR_ADDR, HEAP_END).
    free = (Block*)HEAP_FREE_HDR_ADDR
    free.size = HEAP_INIT_SIZE_BYTES
    free.next = null

    // Seed FREE list head and tail with this block.
    mem_free_head = free
    mem_free_tail = free
*/