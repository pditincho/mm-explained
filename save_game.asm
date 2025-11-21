/*
================================================================================
  Save-game snapshot I/O
================================================================================

Overview
    Implements a simple snapshot save/load system on top of the generic
    disk_high_level streaming helpers. The game’s RAM is treated as a set of
    fixed ranges (engine state, a small zero-page window, and the heap) that
    are streamed to/from fixed track/sector locations on a dedicated save disk.

Responsibilities
    - Media tagging:
        - Store a single identifying byte on track 1 / sector 0 to label a
          disk as a “save disk” (distinct from side-1/side-2 game disks).
        - Reuse a shared buffer (disk_id) for both game and save IDs.
    - Snapshot layout:
        - Define three fixed ranges in RAM:
            - Engine state and variables.
            - A small zero-page window with critical state bytes.
            - Heap/resource region.
        - Map each range to a fixed track/sector starting point on disk.
    - Save operation:
        - If the currently inserted disk is a game disk, skip the entire
          save routine via a stack-surgery helper.
        - Otherwise, tag the disk as a save disk, then stream all three RAM
          ranges to their assigned track/sector chains.
    - Load operation:
        - If the currently inserted disk is a game disk, skip the entire
          load routine via the same helper.
        - Otherwise, verify that the disk tag byte matches the save-disk ID.
        - On match, stream the three disk regions back into their RAM ranges.
        - On mismatch, return a “wrong disk” error code.
    - Disk probing / call gating:
        - Provide a helper that:
            - Reads the tag byte at track 1 / sector 0 into disk_id.
            - Distinguishes “game disk” (side 1/2 IDs) from “other” media.
            - For game disks, discards the immediate caller’s return address
              and returns directly to the caller’s caller, so the caller
              routine is skipped entirely.

Notes
    - The snapshot format is tightly coupled to the current RAM layout and the
      disk track/sector map; any changes must keep these definitions in sync.
    - Disk I/O error handling is delegated to the lower-level streaming layer;
      this module only reports high-level wrong-disk vs OK status.
================================================================================
*/

#importonce
#import "globals.inc"
#import "constants.inc"
#import "disk_high_level.asm"

//------------------------------------------------------------------------------
// Save-disk tag
//------------------------------------------------------------------------------
.const  SAVED_GAME_DISK_ID   = $33    // Expected tag byte for save media

//------------------------------------------------------------------------------
// Fixed write/read targets (tracks/sectors)
//------------------------------------------------------------------------------
.const  SAVE_ID_TRK          = $01    // T1/S0: location of save-disk ID byte
.const  SAVE_ID_SEC          = $00
.const  SAVE_STATE_TRK       = $05    // T5/S0: engine state + vars + free list
.const  SAVE_STATE_SEC       = $00
.const  SAVE_ZP_TRK          = $06    // T6/S0: 7-byte zero-page snippet
.const  SAVE_ZP_SEC          = $00
.const  SAVE_HEAP_TRK        = $07    // T7/S0: heap/resources/layout tables
.const  SAVE_HEAP_SEC        = $00

//------------------------------------------------------------------------------
// Source/destination ranges and sizes
//------------------------------------------------------------------------------
.const  SAVE_ID_ADDR         = $0015  // disk_id buffer address
.const  SAVE_ID_SIZE         = $0001  // 1 byte (tag only)

.const  SAVE_STATE_ADDR      = $FEA1  // $FEA1..$FFF2 inclusive (338 bytes)
.const  SAVE_STATE_SIZE      = $0152

.const  SAVE_ZP_ADDR         = $00C9  // $00C9..$00CF inclusive (7 bytes)
.const  SAVE_ZP_SIZE         = $0007

.const  SAVE_HEAP_ADDR       = $7031  // $7031..$C7FF inclusive (22,479 bytes)
.const  SAVE_HEAP_SIZE       = $57CF

//------------------------------------------------------------------------------
// Game-disk tag probe (T1/S0)
//------------------------------------------------------------------------------
.const  DISK_ID_TRK          = $01    // Track for disk tag read
.const  DISK_ID_SEC          = $00    // Sector for disk tag read
.const  DISK_NOT_GAME        = $00    // Probe result: not a game disk
.const  DISK_IS_GAME         = $01    // Probe result: valid game disk

//------------------------------------------------------------------------------
// Load return codes
//------------------------------------------------------------------------------
.const  LOAD_ERR_WRONG_DISK  = $02    // Load aborted: media tag mismatch
.const  LOAD_OK              = $03    // Load completed successfully

//------------------------------------------------------------------------------
// Variables
//------------------------------------------------------------------------------
.label  disk_id              = $15  // On-disk tag byte cached from T1/S0
/*
================================================================================
  save_state_to_disk
================================================================================
Summary
	Persist current game state to disk using the engine’s linear disk writer. Writes
	a tag byte (disk ID) and three contiguous memory regions to fixed track/sector
	locations, then returns with A=$00.

Returns
	 .A = $00 on success path (set explicitly before RTS).

Global Outputs
	 disk_id                    set to SAVED_GAME_DISK_ID before first write
	 disk_copy_count_lo         low byte of transfer length for each write
	 disk_copy_count_hi         high byte of transfer length for each write
	 
On-disk locations written  
	T1/S0 (ID)
	T5/S0 (engine/vars/free)
	T6/S0 (ZP snippet)
	T7/S0 (heap/resources/layout)

Description
	* Call probe_game_disk_and_skip_caller to ensure media identity .
	* Write 1 byte from $0015 (disk_id) to track 1, sector 0 to tag the save disk.
	* Write $0152 bytes from $FEA1..$FFF2 to track 5, sector 0
	  (engine state, variables, free-block metadata).
	* Write $0007 bytes from $00C9..$00CF to track 6, sector 0 (ZP snippet).
	* Write $57CF bytes from $7031..$C7FF to track 7, sector 0
	  (heap with resources and layout/pointer tables).
	* For each region:
	  * Initialize a linear sector chain with disk_init_chain using
		X=start sector, Y=track.
	  * Provide source pointer in Y:hi, X:lo.
	  * Set disk_copy_count_{lo,hi} to the transfer size.
	  * Call disk_write_linear to stream bytes across the chain.

Notes
	* Assumes a known raw layout; fixed track/sector targets must be reserved.
	* End addresses are inclusive and computed as start + size − 1.
================================================================================
*/
* = $5C3B
save_state_to_disk:
        // Validate disk identity
        jsr     probe_game_disk_and_skip_caller     

        // Write save-disk ID byte to T1/S0
        lda     #SAVED_GAME_DISK_ID           
        sta     disk_id                       
        ldx     #SAVE_ID_SEC                  
        ldy     #SAVE_ID_TRK                  
        jsr     disk_init_chain               // build linear sector chain
        ldx     #<disk_id                     
        ldy     #>disk_id                     
        lda     #<SAVE_ID_SIZE                
        sta     disk_copy_count_lo            
        lda     #>SAVE_ID_SIZE                
        sta     disk_copy_count_hi            
        jsr     disk_write_linear             

        // Write engine state to T5/S0
        ldx     #SAVE_STATE_SEC               
        ldy     #SAVE_STATE_TRK               
        jsr     disk_init_chain               
        ldx     #<SAVE_STATE_ADDR              
        ldy     #>SAVE_STATE_ADDR              
        lda     #<SAVE_STATE_SIZE             
        sta     disk_copy_count_lo            
        lda     #>SAVE_STATE_SIZE             
        sta     disk_copy_count_hi            
        jsr     disk_write_linear             

        // Write ZP snippet to T6/S0
        ldx     #SAVE_ZP_SEC                  
        ldy     #SAVE_ZP_TRK                  
        jsr     disk_init_chain               
        ldx     #<SAVE_ZP_ADDR                 
        ldy     #>SAVE_ZP_ADDR                 
        lda     #<SAVE_ZP_SIZE                
        sta     disk_copy_count_lo            
        lda     #>SAVE_ZP_SIZE                
        sta     disk_copy_count_hi            
        jsr     disk_write_linear             

        // Write heap to T7/S0
        ldx     #SAVE_HEAP_SEC                
        ldy     #SAVE_HEAP_TRK                
        jsr     disk_init_chain               
        ldx     #<SAVE_HEAP_ADDR               
        ldy     #>SAVE_HEAP_ADDR               
        lda     #<SAVE_HEAP_SIZE              
        sta     disk_copy_count_lo            
        lda     #>SAVE_HEAP_SIZE              
        sta     disk_copy_count_hi            
        jsr     disk_write_linear             

        // Return A=$00 to signal success to caller
        lda     #$00                          
        rts                                   
/*
================================================================================
  load_state_from_disk
================================================================================
Summary
	Load save-game data from disk. Accepts only media tagged with
	SAVED_GAME_DISK_ID. If the tag matches, reads three blocks from fixed
	track/sector locations into RAM using the engine’s linear stream reader, then
	returns LOAD_OK. Otherwise returns LOAD_ERR_WRONG_DISK.

Returns
	 .A  		LOAD_ERR_WRONG_DISK if disk tag ≠ SAVED_GAME_DISK_ID.
				LOAD_OK after successfully streaming all save blocks.

Global Inputs
	 disk_id    populated by probe_game_disk_and_skip_caller

Global Outputs
	 RAM[SAVE_STATE_ADDR .. SAVE_STATE_ADDR+SAVE_STATE_SIZE-1]  engine/vars/free
	 RAM[SAVE_ZP_ADDR    .. SAVE_ZP_ADDR+SAVE_ZP_SIZE-1]        ZP snippet
	 RAM[SAVE_HEAP_ADDR  .. SAVE_HEAP_ADDR+SAVE_HEAP_SIZE-1]    heap/resources
	 disk_copy_count_lo / disk_copy_count_hi                    set per transfer
	 .A as noted above

Description
	1. Probe media and populate disk_id via probe_game_disk_and_skip_caller.
	2. Compare disk_id with SAVED_GAME_DISK_ID. If mismatch, return error.
	3. For each block (state, ZP, heap):
	   * Initialize a linear read chain with disk_init_chain_and_read
		 (X=start sector, Y=track).
	   * Set destination pointer in X=lo, Y=hi.
	   * Set transfer length in disk_copy_count_{lo,hi}.
	   * Call disk_stream_copy to stream bytes into RAM across the chain.

Notes
	* No local error handling for drive faults or geometry mismatches; the disk I/O
	  layer must signal and handle such conditions.
	* End addresses are inclusive: end = base + size − 1.
================================================================================
*/
* = $5CA5
load_state_from_disk:
        // ------------------------------------------------------------
        // Read current disk_id from drive; flags may be set by routine
        // ------------------------------------------------------------
        jsr     probe_game_disk_and_skip_caller     // probe media and populate disk_id

        // ------------------------------------------------------------
        // Accept only save disks tagged with SAVED_GAME_DISK_ID ($33)
        // ------------------------------------------------------------
        lda     disk_id                       		// A := on-disk tag byte
        cmp     #SAVED_GAME_DISK_ID           		// tag == save-disk ID?
        beq     load_state_from_sectors       		// yes → proceed to load blocks

        // ------------------------------------------------------------
        // Not a save disk → return error code in A
        // ------------------------------------------------------------
        lda     #LOAD_ERR_WRONG_DISK          		// A := $02 (wrong disk)
        rts                                   

load_state_from_sectors:
        // Read game state from T5/S0
        ldx     #SAVE_STATE_SEC
        ldy     #SAVE_STATE_TRK
        jsr     disk_init_chain_and_read
        ldx     #<SAVE_STATE_ADDR
        ldy     #>SAVE_STATE_ADDR
        lda     #<SAVE_STATE_SIZE
        sta     disk_copy_count_lo
        lda     #>SAVE_STATE_SIZE
        sta     disk_copy_count_hi
        jsr     disk_stream_copy

        // Read ZP bytes from T6/S0
        ldx     #SAVE_ZP_SEC
        ldy     #SAVE_ZP_TRK
        jsr     disk_init_chain_and_read
        ldx     #<SAVE_ZP_ADDR
        ldy     #>SAVE_ZP_ADDR
        lda     #<SAVE_ZP_SIZE
        sta     disk_copy_count_lo
        lda     #>SAVE_ZP_SIZE
        sta     disk_copy_count_hi
        jsr     disk_stream_copy

        // Read heap bytes from T7/S0
        ldx     #SAVE_HEAP_SEC
        ldy     #SAVE_HEAP_TRK
        jsr     disk_init_chain_and_read
        ldx     #<SAVE_HEAP_ADDR
        ldy     #>SAVE_HEAP_ADDR
        lda     #<SAVE_HEAP_SIZE
        sta     disk_copy_count_lo
        lda     #>SAVE_HEAP_SIZE
        sta     disk_copy_count_hi
        jsr     disk_stream_copy

        lda     #LOAD_OK
        rts
/*
================================================================================
  probe_game_disk_and_skip_caller
================================================================================
Summary
	Read disk tag from track 1, sector 0 into disk_id. If the tag matches a valid
	game side (side 1 or side 2), return with A=$01 and skip the immediate caller
	by popping one return address (PLA×2). Otherwise return with A=$00.

Returns
	A = $00  → not a valid game disk.
	A = $01  → valid game disk (side 1 or 2). Returns to caller’s caller.

Global Outputs
	disk_id  	stores the on-disk tag byte read from T1/S0.

Description
	* Initialize a linear read chain for T1/S0.
	* Read one byte from the chain and store it in disk_id.
	* Compare against valid IDs:
		* $31 (side 1)
		* $32 (side 2)
	* If match:
		* Pop two bytes (PLA×2) to drop the caller’s return address.
		* Set A=$01 and RTS (control resumes at caller’s caller).
	* If no match:
		* Set A=$00 and RTS (normal return to caller).
================================================================================
*/
* = $5CFC
probe_game_disk_and_skip_caller:
        // ------------------------------------------------------------
        // Read first byte of track 1, sector 0 into disk_id ($15)
        // ------------------------------------------------------------
        ldx     #DISK_ID_SEC                  // sector 0
        ldy     #DISK_ID_TRK                  // track 1
        jsr     disk_init_chain_and_read      // prepare chain
        jsr     disk_stream_next_byte         // read next byte from chain
        sta     disk_id                       // store tag

        // ------------------------------------------------------------
        // Check if Disk ID matches valid game sides (#$31 or #$32)
        // ------------------------------------------------------------
        lda     disk_id
        cmp     #GAME_DISK_ID_SIDE1
        beq     valid_disk_id
        cmp     #GAME_DISK_ID_SIDE2
        beq     valid_disk_id

        // ------------------------------------------------------------
        // Not a valid game disk → return A=$00
        // ------------------------------------------------------------
        lda     #DISK_NOT_GAME
        rts

valid_disk_id:
        // ------------------------------------------------------------
        // Remove one return address (PLA×2) and return A=$01
        // ------------------------------------------------------------
        pla                                   // drop low byte of caller return
        pla                                   // drop high byte of caller return
        lda     #DISK_IS_GAME
        rts

/*
Pseudo-code

function save_state_to_disk() -> byte
    // Step 1: Probe the disk and possibly skip this routine
    //
    // If the media in the drive is a game disk (side 1 or side 2),
    // probe_game_disk_and_skip_caller() will:
    //   - read the tag at (track 1, sector 0) into disk_id
    //   - recognize it as a game ID
    //   - pop our return address off the stack
    //   - return DISK_IS_GAME (1) directly to our caller’s caller
    //
    // In that case, this function’s body is never executed.
    probe_game_disk_and_skip_caller()

    // If we reach here, the disk is NOT a game disk (e.g., a save disk or
    // some other media). disk_id now contains the tag byte read from track 1,
    // but we do not care about its prior value; we are about to overwrite it.

    // Step 2: Write the save-disk ID tag to (track 1, sector 0)
    disk_id := SAVED_GAME_DISK_ID        // e.g., 0x33

    // Initialize a write chain starting at SAVE_ID_TRK / SAVE_ID_SEC
    disk_init_chain(track = SAVE_ID_TRK, sector = SAVE_ID_SEC)

    // Configure streaming: source = &disk_id, length = SAVE_ID_SIZE (1 byte)
    set_stream_source_address(&disk_id)
    set_stream_byte_count(SAVE_ID_SIZE)

    // Stream the tag byte onto the disk at the start of the chain
    disk_write_linear()

    // Step 3: Write the main engine state block (T5/S0)
    disk_init_chain(track = SAVE_STATE_TRK, sector = SAVE_STATE_SEC)

    set_stream_source_address(SAVE_STATE_ADDR)
    set_stream_byte_count(SAVE_STATE_SIZE)

    disk_write_linear()

    // Step 4: Write the small zero-page snippet (T6/S0)
    disk_init_chain(track = SAVE_ZP_TRK, sector = SAVE_ZP_SEC)

    set_stream_source_address(SAVE_ZP_ADDR)
    set_stream_byte_count(SAVE_ZP_SIZE)

    disk_write_linear()

    // Step 5: Write the heap / resource area (T7/S0)
    disk_init_chain(track = SAVE_HEAP_TRK, sector = SAVE_HEAP_SEC)

    set_stream_source_address(SAVE_HEAP_ADDR)
    set_stream_byte_count(SAVE_HEAP_SIZE)

    disk_write_linear()

    // On the “normal” path, we assume success. Any detailed I/O errors are
    // handled or signaled inside the disk streaming layer.
    return 0   // e.g., “save completed” sentinel
end function


function load_state_from_disk() -> byte
    // Step 1: Probe the disk and possibly skip this routine
    //
    // If the media is a game disk, probe_game_disk_and_skip_caller() will:
    //   - read the tag at (track 1, sector 0) into disk_id
    //   - recognize it as game side 1 or side 2
    //   - pop our return address
    //   - return DISK_IS_GAME (1) directly to our caller’s caller
    //
    // In that scenario, this function’s body never runs.
    probe_game_disk_and_skip_caller()

    // If we are here, the disk is NOT a game disk. disk_id holds whatever
    // tag byte was read from track 1 / sector 0 during the probe.

    // Step 2: Verify that the disk is a save disk
    if disk_id != SAVED_GAME_DISK_ID then
        // The media is not tagged as a save disk; do not touch RAM.
        return LOAD_ERR_WRONG_DISK
    end if

    // Step 3: Restore engine state block from (SAVE_STATE_TRK, SAVE_STATE_SEC)
    disk_init_chain_and_read(track = SAVE_STATE_TRK, sector = SAVE_STATE_SEC)

    set_stream_destination_address(SAVE_STATE_ADDR)
    set_stream_byte_count(SAVE_STATE_SIZE)

    disk_stream_copy()    // Pull bytes from disk into [SAVE_STATE_ADDR ..)

    // Step 4: Restore zero-page snippet
    disk_init_chain_and_read(track = SAVE_ZP_TRK, sector = SAVE_ZP_SEC)

    set_stream_destination_address(SAVE_ZP_ADDR)
    set_stream_byte_count(SAVE_ZP_SIZE)

    disk_stream_copy()

    // Step 5: Restore heap / resource area
    disk_init_chain_and_read(track = SAVE_HEAP_TRK, sector = SAVE_HEAP_SEC)

    set_stream_destination_address(SAVE_HEAP_ADDR)
    set_stream_byte_count(SAVE_HEAP_SIZE)

    disk_stream_copy()

    // At this point the snapshot is fully restored into RAM.
    return LOAD_OK
end function


function probe_game_disk_and_skip_caller() -> byte
    // Step 1: Read the tag byte from the fixed ID location
    disk_init_chain_and_read(track = DISK_ID_TRK, sector = DISK_ID_SEC)

    // Read the next byte from the stream; this is the disk ID / tag.
    tag_byte := disk_stream_next_byte()

    // Persist it in the shared buffer
    disk_id := tag_byte

    // Step 2: Classify the media
    if tag_byte == GAME_DISK_ID_SIDE1 or tag_byte == GAME_DISK_ID_SIDE2 then
        // This is a recognized game disk (side 1 or side 2).

        // Perform stack surgery:
        //   - Remove the immediate caller’s return address.
        //   - This effectively “skips” the caller routine, and our RTS will
        //     return to the caller’s caller instead.
        discard_top_return_address_from_stack()

        // Report “game disk” status to the upper level.
        return DISK_IS_GAME
    else
        // Not a game disk (could be save media or something else). Just
        // report the fact and let the caller continue normally.
        return DISK_NOT_GAME
    end if
end function

*/