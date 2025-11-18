#importonce
#import "globals.inc"
#import "constants.inc"
#import "rsrc_metadata.inc"
#import "memory_mgmt.asm"
#import "disk_high_level.asm"
#import "pause.asm"
#import "actor.asm"
#import "misc.asm"

.const RSRC_DHDR_BYTES              = $04   // On-disk header size (bytes).
.const RSRC_MHDR_BYTES              = $04   // In-memory header size (bytes).

.const RSRC_DHDR_OFF_SIZE_LO        = $00   // Disk header: +0  size.lo   (total bytes, little-endian).
.const RSRC_DHDR_OFF_SIZE_HI        = $01   // Disk header: +1  size.hi.
.const RSRC_DHDR_OFF_SENTINEL       = $02   // Disk header: +2  sentinel byte (must be 0).
.const RSRC_DHDR_OFF_CHECKSUM       = $03   // Disk header: +3  checksum (expected value).

.const RSRC_MHDR_OFF_SIZE_LO        = $00   // Memory header: +0  size.lo   (total bytes, little-endian).
.const RSRC_MHDR_OFF_SIZE_HI        = $01   // Memory header: +1  size.hi.
.const RSRC_MHDR_OFF_TYPE           = $02   // Memory header: +2  resource type id.
.const RSRC_MHDR_OFF_INDEX          = $03   // Memory header: +3  resource index within type.

.const SOUND_PROTECT_1     = $03    // Reserved/non-evictable sound index #3 (skip during eviction).
.const SOUND_PROTECT_2     = $04    // Reserved/non-evictable sound index #4 (skip during eviction).
.const SOUND_PROTECT_3     = $05    // Reserved/non-evictable sound index #5 (skip during eviction).

.const PRIORITY_ROOM       = $00    // Eviction dispatch code: try releasing one ROOM.
.const PRIORITY_COSTUME    = $01    // Eviction dispatch code: try releasing one COSTUME.
.const PRIORITY_SOUND      = $02    // Eviction dispatch code: try releasing EVICTABLE SOUNDS.
.const PRIORITY_SCRIPT     = $03    // Eviction dispatch code: try releasing EVICTABLE SCRIPTS.
.const PRIORITY_WRAP_AT    = $04    // Threshold: if evict_pri_cur ≥ this, set to $FF so INC → $00 (wrap to ROOM).


.label rsrc_hdr_ptr                 = $4F   // ZP pointer → header base in RAM; used with (rsrc_hdr_ptr),Y indexed accesses.
.label rsrc_payload_ptr             = $E4   // 16-bit pointer → first byte after header (payload start).
.label rsrc_payload_len             = $E6   // 16-bit remaining payload byte count (used during checksum loop).
.label rsrc_chk_accum               = $E8   // Running checksum accumulator (XOR of payload bytes).
.label rsrc_chk_expected            = $E9   // Expected checksum loaded from header (disk header +CHECKSUM).
.label rsrc_retry_rem               = $EA   // Retry counter for load/verify; decremented on failure, reset to 1 before UI prompt.

/* Note: rsrc_hdr_scratch and rsrc_total_bytes alias the same storage ($553B).
 * Lifecycle:
 *   - Immediately after disk read: use as 4-byte disk header scratch (copy target).
 *   - After header parse: reuse lo/hi as 16-bit total size (header + payload).
 */
.label rsrc_hdr_scratch             = $553B // 4-byte scratch buffer for on-disk header copy.
.label rsrc_total_bytes             = $553B // 16-bit total size in bytes (header + payload); aliases rsrc_hdr_scratch.
.label rsrc_data_ptr                = $553F // 16-bit pointer → resource base in RAM (header at +0, payload at +RSRC_*HDR_BYTES).

.label rsrc_raw_size                = $FD9C // 16-bit raw payload size (bytes); written into memory header size fields.

.label rsrc_ptr            = $4F    // Zero-page pointer to the resource base in RAM (lo/hi).
                                    // Used for indexed indirection: (rsrc_ptr),Y and via <rsrc_ptr/>rsrc_ptr.

.label evict_pri_cur       = $FE4B  // Current eviction priority selector (0..3).
                                    // Rotation order: ROOM → COSTUME → SOUND → SCRIPT; values ≥ PRIORITY_WRAP_AT wrap to 0 on INC.

.label evict_pri_start     = $FE4C  // Snapshot of starting priority for this pass.
                                    // Used to detect a full cycle with no successful eviction.

.label best_attr           = $FD9F  // Best-so-far LRU “age”/attr score when scanning rooms.
                                    // Non-zero means evictable; larger value = older/staler candidate.

.label best_room_idx       = $FD9E  // Index of the current best LRU room candidate.
                                    // 0 is used as a “no candidate” sentinel (scan runs MAX..1).

.label chain_sector = $1d     // ZP: sector (lo)
.label chain_track  = $1e     // ZP: track  (hi)
.label saved_room_idx  = $3bdd   // scratch: preserves X across disk_ensure_side

// rsrc_ensure_script_resident
// rsrc_ensure_sound_resident
.label script_idx_saved = $19
.label sound_idx_saved  = $19        
.label saved_y = $3bdc
.label rsrc_ptr_lo_tmp  = $19        
.label rsrc_ptr_hi_tmp  = $1A

// rsrc_ensure_costume_resident
.label tmp_ptr_lo = $17
.label tmp_ptr_hi = $18
.label costume_idx_saved = $15

/*
================================================================================
  rsrc_ensure_costume_resident
================================================================================
Summary:
	Given a costume index in X, returns immediately if the costume is already
	resident (pointer table non-zero). Otherwise, selects the correct disk
	side/chain from the owning room, programs the disk stream position using
	a 2-byte disk-location entry (offset + sector), loads the costume via the
	generic resource loader, and publishes the resulting pointer to the
	costume pointer tables.

Arguments:
	X                        Costume resource index (0..N).

Description:
	1) Fast path: if costume_ptr_hi_tbl[X] ≠ 0 → already loaded → rts.
	2) Miss path:
		- Save costume index; X := costume_room_res_idx[index]; jsr room_disk_chain_prepare.
		- X := 2*index; set rsrc_read_offset (offset-in-sector) and rsrc_sector_step (sector index).
		- resource_type := #$02 (COSTUME); jsr rsrc_load_from_disk → X:Y pointer.
		- Publish pointer into costume tables at rsrc_resource_index; rts.
================================================================================
*/
* = $38A8
rsrc_ensure_costume_resident:
        // Stash the incoming costume index (X) so we can publish the pointer later.
        stx     rsrc_resource_index

        // Fast-path residency check:
        //   costume_ptr_hi_tbl[X] != 0  ⇒ already resident in RAM.
        //   LDA sets Z=1 iff A==0; BEQ branches when not loaded (Z=1).
        lda     costume_ptr_hi_tbl,x
        beq     costume_cache_miss     // miss → go load it
        rts                               // hit  → nothing to do

costume_cache_miss:
        // Preserve the original costume index across room resolution & chain setup
        stx     costume_idx_saved

        // Determine owning room for this costume and prepare the disk chain:
        //   X := room index (selects disk side), seed track/sector chain for that room
        lda     costume_room_res_idx,x
        tax
        jsr     room_disk_chain_prepare

        // Re-select the costume’s disk-location pair (2 bytes per entry):
        //   Each entry = [offset-within-sector, sector-index]
        //   Double the costume index to convert entry index → byte offset
        lda     costume_idx_saved
        asl                               // A := 2 * index (carry ignored)
        tax                               // X := byte offset into pair table

        // Program stream position for the loader:
        //   rsrc_read_offset := byte offset within current sector
        //   rsrc_sector_idx := sector index within the room’s chain
        lda     costume_disk_loc_tbl,x
        sta     rsrc_read_offset
        lda     costume_disk_loc_tbl + 1,x
        sta     rsrc_sector_idx

        // Identify resource class and perform the load:
        //   rsrc_resource_type := COSTUME (#$02)
        //   rsrc_load_from_disk → returns X=ptr.lo, Y=ptr.hi (payload start)
        lda     #RSRC_TYPE_COSTUME                    
        sta     rsrc_resource_type
        jsr     rsrc_load_from_disk

        // Publish the returned pointer into the costume tables at the original index.
        // Stash X/Y temporarily so we can use Y as the table index (resource_index).
        stx     tmp_ptr_lo                  // tmp_ptr_lo := ptr.lo
        sty     tmp_ptr_hi                  // tmp_ptr_hi := ptr.hi
        ldy     rsrc_resource_index          // Y := costume index for table write
        lda     tmp_ptr_lo
        sta     costume_ptr_lo_tbl,y    // table[Y].lo := ptr.lo
        lda     tmp_ptr_hi
        sta     costume_ptr_hi_tbl,y    // table[Y].hi := ptr.hi
        rts		
/*
================================================================================
  rsrc_ensure_sound_resident
================================================================================\
Summary:
	Given a sound index in A, returns immediately if the sound is already
	resident (pointer table non-zero). Otherwise, selects the correct disk
	side/chain from the owning room, programs the disk stream position using
	a 2-byte disk-location entry (offset + sector), loads the sound via the
	generic resource loader, and publishes the resulting pointer to the
	sound pointer tables.

Arguments:
	A                        Sound resource index (0..N).

Globals:
	sound_room_idx_tbl[X]    Owning room index for sound X (selects disk side).
	sound_disk_loc_tbl       2 bytes/entry: +0 offset-in-sector, +1 sector index.
	sound_ptr_lo_tbl[Y]      Published with returned ptr.lo.
	sound_ptr_hi_tbl[Y]      Published with returned ptr.hi.
	resource_type            Set to RSRC_TYPE_SOUND prior to load.

Description:
	1) Fast path: X := A; if sound_ptr_hi_tbl[X] ≠ 0 → already loaded → RTS.
	2) Miss path:
		- Save sound index; X := sound_room_idx_tbl[index]; jsr room_disk_chain_prepare.
		- X := 2*index; read disk-location pair:
			rsrc_read_offset := disk_loc[+0]; rsrc_sector_idx := disk_loc[+1].
		- resource_type := RSRC_TYPE_SOUND; jsr rsrc_load_from_disk → X:Y pointer.
		- Publish to sound_ptr_{lo,hi}_tbl[resource_index]; restore Y; RTS.
================================================================================
*/
* = $39E6
rsrc_ensure_sound_resident:
        // Save caller’s Y only for the slow path (publish step below).
        // NOTE: On the fast path we return without restoring Y by design.
        sty saved_y

        // Resource index arrives in A:
        //   - keep a copy for later publishes ( rsrc_resource_index)
        //   - X := index for table lookups (sound_ptr_*_tbl[])
        sta  rsrc_resource_index
        tax

        // Fast-path residency check:
        //   sound_ptr_hi_tbl[X] != 0  ⇒ already resident (in-memory)
        //   LDA sets Z=1 iff A==0; BEQ takes the miss path when not loaded.
        lda sound_ptr_hi_tbl,x
        beq sound_cache_miss
        rts                               // hit: nothing to do (Y intentionally not restored)

		/*---------------------------------------
		 * Cache miss: prepare disk stream
		 *--------------------------------------*/
sound_cache_miss:
        stx sound_idx_saved                         // save sound index (X) for later table writes
        lda sound_room_idx_tbl,x               // A := owning room index for this sound
        tax                                // X := room index (for disk-side selection)
        jsr room_disk_chain_prepare        // ensure correct disk side; seed track/sector chain

        // Re-select the sound’s disk-location entry:
        //   Each entry is 2 bytes: [offset-within-sector, sector-index].
        //   Double the original sound index to get the byte offset.
        lda sound_idx_saved
        asl                                // A := 2 * sound_index (CARRY unused)
        tax                                // X := table byte offset (points at offset field)

        // Program the disk stream position from the 2-byte location pair:
        //   rsrc_read_offset := byte offset within the current sector
        //   rsrc_sector_idx := sector index within the chain
        lda sound_disk_loc_tbl,x
        sta rsrc_read_offset
        lda sound_disk_loc_tbl + 1,x
        sta rsrc_sector_idx

        // Identify resource class and perform the load:
        //   resource_type := 06 (SOUND)
        //   rsrc_load_from_disk → X=ptr.lo, Y=ptr.hi (payload pointer)
        lda #RSRC_TYPE_SOUND                          
        sta rsrc_resource_type
        jsr rsrc_load_from_disk

        // Publish the returned pointer into the sound tables at the original index.
        // Stash X/Y temporarily because we need Y to address by resource_index.
        stx rsrc_ptr_lo_tmp                         // rsrc_ptr_lo_tmp := ptr.lo
        sty rsrc_ptr_hi_tmp                         // rsrc_ptr_hi_tmp := ptr.hi
        ldy rsrc_resource_index                 // Y := sound index used for tables
        lda rsrc_ptr_lo_tmp
        sta sound_ptr_lo_tbl,y             // table[Y].lo := ptr.lo
        lda rsrc_ptr_hi_tmp
        sta sound_ptr_hi_tbl,y             // table[Y].hi := ptr.hi

        // Slow-path only: restore caller’s Y (fast path returns earlier by design).
        ldy saved_y
        rts
/*
================================================================================
  rsrc_ensure_script_resident
================================================================================
Summary:
	Fast path: if the script at index A is already resident (ptr.hi ≠ 0),
	return immediately. Slow path: find the owning room, prepare the disk
	chain for that room, choose the correct sector/offset tables based on
	the script’s scope bit (index bit7), program the stream (read_offset /
	sector_step), set resource_type=SCRIPT, load from disk, and publish the
	returned data pointer into the script pointer tables.

Arguments:
	A                	Script resource index (bit7 encodes scope: 0=global, 1=room).
	Y                   Caller-provided value (saved/restored only on slow path).

Returns:
	On slow path:        X:Y = data pointer; tables updated for this script.
	On fast path:        RTS as soon as ptr.hi ≠ 0 (Y is NOT restored).

Globals:
	room_for_script[X]     Owning room index for script X.
	global_script_disk_loc_tbl[2X] / global_script_sector_index[2X]
	room_script_disk_loc_tbl[2X] / room_script_sector_index[2X]
	script_ptr_lo_tbl[script_index], script_ptr_hi_tbl[script_index]
	rsrc_read_offset, rsrc_sector_step, resource_type

Notes:
	- The ASL on the script index doubles it for 2-byte tables and also captures
	 the original bit7 into C: C=0 ⇒ global script; C=1 ⇒ room script.
================================================================================
*/
* = $3A29
rsrc_ensure_script_resident:
        /*----------------------------------------
         * Fast path: residency check
         *  - We consider a script "resident" if its pointer hi-byte ≠ 0.
         *  - If resident, return immediately (note: Y is NOT restored on this path).
         *---------------------------------------*/
        sty saved_y                        // Save caller's Y for the slow path; may not be restored on early return
        sta rsrc_resource_index                 // Remember script index for the later publish step after load
        tax                                // X := script index (for table lookups below)
        lda script_ptr_hi_tbl,x            // Load hi byte of script data pointer; Z=1 iff value == 0 (not resident)
        beq script_cache_miss              // Not resident → take slow path
        rts                                // Already resident → early exit (Y intentionally left as-is)

script_cache_miss:
        /*----------------------------------------
         * Resolve owning room and prepare disk chain
         *  - Room needed to choose correct disk side/chain before streaming the script.
         *---------------------------------------*/
        stx script_idx_saved               // Preserve original script index across the room lookup
        lda script_room_idx_tbl,x          // A := owning room index for this script
        tax                                // X := room index (parameter for disk-chain prep)
        jsr room_disk_chain_prepare        // Ensure correct disk side; seed/position sector chain for this room

        /*----------------------------------------
         * Program stream position (offset + sector) by scope
         *  - Each table entry is 2 bytes: [offset-within-sector, sector-index].
         *  - ASL doubles the script index for 2-byte entries and shifts original bit7 into C:
         *      C=0 ⇒ global-scoped script, C=1 ⇒ room-scoped script.
         *---------------------------------------*/
        lda script_idx_saved               // A := script index (bit7 encodes scope)
        asl                                // A := 2 * index; C := original bit7 (scope flag)
        tax                                // X := byte index into pair table (offset/sector)
        bcc scope_global_script            // If C=0 → use GLOBAL tables; else fall through to ROOM tables

        // Room-scoped script — use per-room disk-location table [OFF, SEC]
        lda room_script_disk_loc_tbl,x     // load OFF (byte 0): read offset within sector
        sta rsrc_read_offset               // set stream byte offset
        lda room_script_disk_loc_tbl+1,x   // load SEC (byte 1): sector index in chain
        sta rsrc_sector_idx                // set sector index
        jmp load_script_from_disk

scope_global_script:
        // Global-scoped script — use global disk-location table [OFF, SEC]
        lda global_script_disk_loc_tbl,x   // load OFF (byte 0): read offset within sector
        sta rsrc_read_offset               // set stream byte offset
        lda global_script_disk_loc_tbl+1,x // load SEC (byte 1): sector index in chain
        sta rsrc_sector_idx                // set sector index


load_script_from_disk:
        /*----------------------------------------
         * Load & publish the script payload pointer
         *  - Tell loader the class: resource_type := SCRIPT
         *  - rsrc_load_from_disk returns X=ptr.lo, Y=ptr.hi
         *  - Publish that pointer into script_ptr_*_tbl[resource_index]
         *---------------------------------------*/
        lda #RSRC_TYPE_SCRIPT              // Select resource class for the loader
        sta rsrc_resource_type
        jsr rsrc_load_from_disk            // → X=payload.lo, Y=payload.hi (A/flags clobbered)

        // Stash returned pointer bytes before reusing Y as an index
        stx rsrc_ptr_lo_tmp              // save lo
        sty rsrc_ptr_hi_tmp              // save hi
        ldy rsrc_resource_index                 // Y := original script index (table row)

        // Publish pointer to tables[script_index]
        lda rsrc_ptr_lo_tmp
        sta script_ptr_lo_tbl,y            // write lo byte
        lda rsrc_ptr_hi_tmp
        sta script_ptr_hi_tbl,y            // write hi byte

        // Restore caller’s Y saved on the miss path (fast path never saves/restores Y)
        ldy saved_y
        rts
/*
================================================================================
  room_disk_chain_prepare
================================================================================
Summary:
	Ensures the correct disk side is active for the requested room,
	computes the (track, sector) pair for that room, and initializes
	the disk sector chain so the stream is ready for rsrc_load_from_disk.

Arguments:
	X							Index into per-room disk tables.

Description:
	- If the current disk side doesn’t match active_side_id, call
	 disk_ensure_side (X is preserved across the call via temp_index).
	- Offset into room_sec_trk_tbl by (room_index * 2)
	 to fetch sector (lo) and track (hi).
	- Call disk_init_chain with X=sector, Y=track.
================================================================================
*/
* = $3ABD
room_disk_chain_prepare:
        // Side OK fast-path: if the drive’s active side matches the room’s required side, skip change.
        lda room_disk_side_tbl,x          // A := required side ID for room X
        cmp active_side_id                // compare against currently active side
        beq side_ready                    // equal → side already correct

        // Side mismatch: preserve X, switch/verify the correct side, then restore X.
        stx saved_room_idx                // save room index across helper
        jsr disk_ensure_side              // ensure required side is mounted (may prompt/pause)
        ldx saved_room_idx                // restore room index

side_ready:
        // Index into room_sec_trk_tbl: each entry is a 2-byte pair [SECTOR, TRACK].
        // Double the room index (stride = 2) via TXA/ASL, then TAX to get the byte offset.
        txa
        asl                               // A := room_idx * 2  (carry ignored)
        tax                               // X := byte index into pair table

        // Legacy sanity check: (sector | track) != 0 ?
        // Control flow is effectively unchanged—the next label is fetch_sector_track
        // either way. Retained to mirror the original code.
        lda room_sec_trk_tbl+ROOM_ST_OFF_SECTOR,x   // byte 0: sector
        ora room_sec_trk_tbl+ROOM_ST_OFF_TRACK,x    // byte 1: track
        bne fetch_sector_track

fetch_sector_track:
        // Load the per-room disk location pair [SECTOR, TRACK] from the table.
        lda room_sec_trk_tbl+ROOM_ST_OFF_SECTOR,x   // sector for room X
        sta chain_sector                            // stage for API: X := sector
        lda room_sec_trk_tbl+ROOM_ST_OFF_TRACK,x    // track for room X
        sta chain_track                             // stage for API: Y := track

        // Initialize the disk chain at (track, sector).
        // disk_init_chain expects X=sector, Y=track.
        ldx chain_sector
        ldy chain_track
        jsr disk_init_chain                         // stream := T=Y, S=X
        rts
/*
================================================================================
  rsrc_load_from_disk
================================================================================
Summary:
	Pauses the game loop, seeks the disk stream to this resource,
	copies/validates the on-disk header, allocates memory for the
	full object, copies the payload, computes and verifies the
	checksum, then continues into rsrc_hdr_init on success.

Arguments:
	rsrc_sector_idx 		Which sector in the physical chain.
	rsrc_read_offset		Byte offset into the resource stream.
	rsrc_total_bytes		16-bit total size in bytes.

Flow:      
	BEQ on checksum match → falls through to rsrc_hdr_init.

Globals:   
	rsrc_data_ptr 						set to allocated destination.
	rsrc_payload_ptr, rsrc_payload_len 	prepared.
	rsrc_chk_expected/rsrc_chk_accum 	updated.
	rsrc_retry_rem 						decremented on failures.

Description:
	1) Pause game; seek to header start.
	2) Copy header into rsrc_hdr_scratch; validate header sentinel == 0.
	  If not zero: run disk_ensure_side and retry from the top.
	3) Allocate memory for rsrc_total_bytes; publish result in rsrc_data_ptr.
	4) Seek stream to the payload start and copy rsrc_total_bytes bytes to rsrc_data_ptr.
	5) Prepare checksum:
		rsrc_hdr_ptr := rsrc_data_ptr
		rsrc_payload_len := rsrc_total_bytes − header size (RSRC_DHDR_BYTES)
		rsrc_payload_ptr := rsrc_data_ptr + header size (RSRC_DHDR_BYTES)
		rsrc_chk_expected := rsrc_hdr_ptr[RSRC_DHDR_OFF_CHECKSUM]
	6) Checksum: XOR-accumulate payload bytes into rsrc_chk_accum
		When length is 0, compare with rsrc_chk_expected. 
		If equal → BEQ rsrc_hdr_init; otherwise decrement rsrc_retry_rem.
	  If retries remain → retry_copy payload; else show disk_error_msg and retry after user ack.
================================================================================
*/
* = $5541
rsrc_load_from_disk:
        // Pause the game loop while performing disk I/O to avoid tearing/state races.
        jsr pause_game

        /*
         * Read the first data sector for this resource:
         *   X = rsrc_read_offset            (byte offset into the resource stream)
         *   Y = rsrc_sector_idx     (which sector in the physical chain)
         * On return, the disk stream is positioned at the start of the header.
         */
        ldx rsrc_read_offset
        ldy rsrc_sector_idx
        jsr disk_seek_read

        /*
         * Copy the on-disk resource header into local scratch (rsrc_hdr_scratch).
         *   dest = rsrc_hdr_scratch
         *   count = RSRC_DHDR_BYTES bytes
         * After this, the stream pointer advances by header size.
         */
        ldx #<rsrc_hdr_scratch
        ldy #>rsrc_hdr_scratch
        lda #RSRC_DHDR_BYTES
        sta disk_copy_count_lo
        lda #$00
        sta disk_copy_count_hi
        jsr disk_stream_copy

        /*
         * Validate the header’s sentinel (must be 0).
         *   BEQ allocate_payload → OK (Z=1 because A==0)
         *   else: run a disk ID/sync check and restart the load attempt.
         */
        lda rsrc_hdr_scratch + RSRC_DHDR_OFF_SENTINEL
        beq allocate_payload
        jsr disk_id_check
        jmp rsrc_load_from_disk

		/* ----------------------------------------
		 * Allocate space for the whole resource payload
		 * ----------------------------------------
		 */
allocate_payload:
        /*
         * Prepare allocator arguments:
         *   X/Y = rsrc_total_bytes
         */
        ldx rsrc_total_bytes
        ldy rsrc_total_bytes + 1

        /*
         * Request a block large enough for the payload (+ header handled by allocator path).
         * On success: Z=0 and X/Y = allocated header pointer (lo/hi).
         * On failure: Z=1 (X/Y unspecified) and caller’s recovery path applies.
         */
        jsr mem_alloc

        /*
         * Publish the destination payload pointer for subsequent copies:
         *   rsrc_data_ptr := returned X/Y (lo/hi)
         */
        stx rsrc_data_ptr
        sty rsrc_data_ptr + 1

        // Mark that we have at least one retry opportunity for this load sequence.
        lda #$01
        sta rsrc_retry_rem

copy_payload_from_disk:
        /*
         * Copy the entire payload from the disk stream into the allocated destination.
         *   dest    = rsrc_data_ptr (lo/hi)
         *   count   = rsrc_total_bytes (lo/hi)
         *   effect  = advances both stream position and dest pointer by count bytes
         */
		ldx rsrc_read_offset 
		ldy rsrc_sector_idx
		jsr disk_seek_read		
        ldx rsrc_data_ptr
        ldy rsrc_data_ptr + 1
        lda rsrc_total_bytes
        sta disk_copy_count_lo
        lda rsrc_total_bytes + 1
        sta disk_copy_count_hi
        jsr disk_stream_copy

		/*
		 * -------------------------------------------
		 * Prepare checksum calculation over the payload (exclude header)
		 * -------------------------------------------
		 * Seed rsrc_hdr_ptr with the start of the on-memory resource (header at +0).
		 *   rsrc_hdr_ptr := rsrc_data_ptr
		 */
        lda rsrc_data_ptr
        sta rsrc_hdr_ptr
        lda rsrc_data_ptr + 1
        sta rsrc_hdr_ptr + 1

        /*
         * Compute payload byte count (exclude header bytes):
         *   rsrc_payload_len = rsrc_total_bytes - RSRC_DHDR_BYTES
         * Little-endian subtract; C=1 before SBC means “no borrow” baseline.
         */
        sec
        lda rsrc_total_bytes
        sbc #RSRC_DHDR_BYTES
        sta rsrc_payload_len
        lda rsrc_total_bytes + 1
        sbc #$00
        sta rsrc_payload_len + 1

        /*
         * Point rsrc_payload_ptr at the first byte after the header:
         *   rsrc_payload_ptr = rsrc_data_ptr + RSRC_DHDR_BYTES
         * Little-endian add; C cleared so ADC adds carry-in=0.
         */
        clc
        lda rsrc_data_ptr
        adc #RSRC_DHDR_BYTES
        sta rsrc_payload_ptr
        lda rsrc_data_ptr + 1
        adc #$00
        sta rsrc_payload_ptr + 1

        /*
         * Load the expected checksum from the header:
         *   A = *(rsrc_hdr_ptr + RSRC_HDR_OFF_CHECKSUM)
         * Store for later comparison after summing the payload bytes.
         */
        ldy #RSRC_DHDR_OFF_CHECKSUM
        lda (rsrc_hdr_ptr),Y
        sta rsrc_chk_expected

		/*
		 * ----------------------------------------
		 * Initialize running checksum to 0 (XOR accumulator)
		 * ----------------------------------------
		 */
        lda #$00
        sta rsrc_chk_accum
rsrc_chk_step:
        ldy #$00                // always read current byte at rsrc_payload_ptr + 0
        lda rsrc_chk_accum
        /*
         * XOR current payload byte into the running checksum:
         *   checksum := checksum ⊕ *rsrc_payload_ptr
         */
        eor (rsrc_payload_ptr),Y
        sta rsrc_chk_accum

        // Advance source pointer: ++rsrc_payload_ptr (little-endian)
        inc rsrc_payload_ptr
        bne dec_rem_count        		// no wrap → skip hi-byte increment
        inc rsrc_payload_ptr + 1           // wrapped lo → carry into hi

dec_rem_count:
        /*
         * Decrement remaining byte count (16-bit):
         *   if lo==0 then borrow from hi
         */
        lda rsrc_payload_len
        bne dec_rem_lo
        dec rsrc_payload_len + 1
dec_rem_lo:
        dec rsrc_payload_len

        /*
         * Loop if any bytes remain:
         *   test (lo | hi) != 0  → more data pending
         */
        lda rsrc_payload_len
        ora rsrc_payload_len + 1
        bne rsrc_chk_step       // Z=0 → continue; Z=1 → done

		/*
		 * ----------------------------------------
		 * All bytes consumed — verify payload checksum against header’s expected value.
		 * ----------------------------------------
		 */
        lda rsrc_chk_accum
        cmp rsrc_chk_expected

        /*
         * Match? → proceed to write the resource header metadata.
         *   CMP sets Z=1 when A==expected → BEQ takes the “good” path.
         */
        beq rsrc_hdr_init

        /*
         * Mismatch — attempt a retry path:
         *   rsrc_retry_rem := rsrc_retry_rem - 1
         *   if rsrc_retry_rem > 0 → retry_copy the room data and re-check
         */
        dec rsrc_retry_rem
        bne retry_copy	          // Z=0 (counter not zero) → try again

        // No retries left — reset counter to 1 and prompt the user, then retry.
        lda #$01
        sta rsrc_retry_rem

        /*
         * Show disk error and wait for user to acknowledge (e.g., joystick button).
         */
        lda #<DISK_ERROR_MSG
        sta print_msg_ptr
        lda #>DISK_ERROR_MSG
        sta print_msg_ptr + 1
        jsr print_message_wait_for_button

retry_copy:
        // Re-read the raw data from disk, then recompute and re-validate checksum.
        jmp copy_payload_from_disk
/*
================================================================================
  rsrc_hdr_init
================================================================================
Summary:
	Initializes the resource header at rsrc_hdr_ptr with size (lo/hi),
	type, and index. Returns the payload pointer in X:Y, then resumes
	the game.

Arguments (from previous section):
	rsrc_hdr_ptr				Base address for header
	rsrc_raw_size         	16-bit raw payload size in bytes.
	rsrc_resource_type    	Resource type identifier.
	rsrc_resource_index   	Resource index within the type.
	rsrc_data_ptr	        16-bit pointer to start of payload.

Returns:
	X:Y                        rsrc_data_ptr (lo in X, hi in Y).
	Flags                      Clobbered by loads/stores and by unpause_game.

Description:
	Writes header layout at (rsrc_hdr_ptr):
	 +0..+1  size  (little-endian): <rsrc_raw_size, >rsrc_raw_size
	 +2      type  (rsrc_resource_type)
	 +3      index (rsrc_resource_index)
	Loads X:=<rsrc_data_ptr, Y:=>rsrc_data_ptr and calls unpause_game before RTS.
================================================================================
*/
* = $5604
rsrc_hdr_init:
		// Store raw data size
        ldy #$00
        lda rsrc_raw_size
        sta (rsrc_hdr_ptr),Y
        iny
        lda rsrc_raw_size + 1
        sta (rsrc_hdr_ptr),Y
		
		// Store resource type 
        iny
        lda rsrc_resource_type		
        sta (rsrc_hdr_ptr),Y
		
		// Store resource index
        iny
        lda rsrc_resource_index		
        sta (rsrc_hdr_ptr),Y
		
		// Return the resource data pointer via .X and .Y
        ldx rsrc_data_ptr
        ldy rsrc_data_ptr + 1
		
		// Unpause game
        jsr unpause_game
        rts
/*
================================================================================
  rsrc_unlock_or_unassign_costume
================================================================================
Summary:
	Two-phase policy. 
   
	Phase 1 scans all costumes from highest index down and unlocks at most one by 
	clearing bit7 of its attribute byte. If no locks remain, Phase 2 scans indices 8..1 
	and unassigns an actor for the first eligible costume (not the current actor, loaded, attrs==0). 
	After unassignment, resets default destination coords, and “parks” the costume in a holding room.
	If no candidate exists, the routine raises a diagnostic and hard-halts.
	
Uses / Globals:
	costume_attr_tbl[X]    Attribute byte; bit7=1 ⇒ locked, bit7=0 ⇒ unlocked.
	costume_ptr_hi_tbl[X]  Non-zero ⇒ resource resident in memory.
	current_actor_idx      Index of active/controlled actor; never evicted.
	costume_dest_x_tbl[X]  Set to COSTUME_DFLT_X_DEST on unassignment.
	costume_dest_y_tbl[X]  Set to COSTUME_DFLT_Y_DEST on unassignment.
	actor_room_idx_tbl[X]  Set to COSTUME_HOLDING_ROOM on unassignment.

Description:
	1) Unlock pass:
		LDX := COSTUME_MAX_INDEX; scan downward.
		If costume_attr_tbl[X].bit7 == 1 → clear with AND #$7F; RTS.
	2) Unassignment pass (only if no locks found):
		LDX := COSTUME_SCAN_FIRST8; scan X = 8..1.
		Skip if X == current_actor_idx, or not resident (ptr.hi==0), or attrs != 0.
		On hit: attrs := 0; JSR detach_actor_from_costume;
				(dest_x,dest_y) := (COSTUME_DFLT_X_DEST,COSTUME_DFLT_Y_DEST);
				actor_room_idx_tbl[X] := COSTUME_HOLDING_ROOM; RTS.
	3) Failure:
		diag_code := #$05; cpu_port := CPU_PORT_MAP_IO; loop on vic_border_color_reg.
================================================================================
*/
* = $567B
rsrc_unlock_or_unassign_costume:
        ldx #COSTUME_MAX_INDEX            // start at the highest costume index; scan downward one-by-one

scan_locked_costumes:
        // Read attributes for costume X. Bit7 encodes "locked":
        //   bit7 = 1 → locked (N=1)  |  bit7 = 0 → unlocked (N=0)
        lda costume_mem_attrs,x
        bpl advance_lock_scan                 // BPL (N=0) ⇒ already unlocked → skip this entry

        // Locked path: clear the lock bit (bit7) and exit.
        // AND #$7f masks off bit7 without touching other attribute bits.
        and #$7f                         // bit7 := 0 (unlock)
        sta costume_mem_attrs,x          // commit unlock
        rts                              // early return: unlock exactly one per call

advance_lock_scan:
        dex
        bne scan_locked_costumes                // continue scan while X != 0 (note: index 0 is not checked)

        // No locked costumes found — switch to eviction pass over the first 8 (indices 8..1).
        ldx #$08

scan_for_evict:
        // Do not evict the active character:
        // CPX current_kid_idx sets Z=1 when X == current_kid_idx; BEQ ⇒ skip this entry.
        cpx current_kid_idx
        beq advance_costume_evict_scan

        // Candidate must be resident:
        // costume_ptr_hi_tbl[X] == 0 ⇒ not loaded (Z=1 after LDA) → BEQ skip.
        lda costume_ptr_hi_tbl,x
        beq advance_costume_evict_scan

        /*----------------------------------------
         * Evict this costume (free its resource)
         *  - Mark attrs = 0 (idle/unpinned) before dropping the pointer.
         *  - Preserve X across the helper; it may clobber registers/flags.
         *---------------------------------------*/
        lda #$00
        sta costume_mem_attrs,x          // mark costume X as idle (no special attrs)

        // Preserve loop index while freeing the resource for costume X.
        txa                               // A := X (index snapshot)
        pha                               // push index; will restore after JSR

        jsr detach_actor_from_costume               

        // Restore loop index for subsequent bookkeeping (coords/room updates).
        pla                               // A := saved index
        tax                               // X := index


        // Reset default destination for evicted costume (spawn/parking coords).
        // Units are engine-specific (pixels/tiles); X and Y set to sane defaults.
        lda #COSTUME_DFLT_X_DEST                          // default X destination
        sta costume_dest_x,x
        lda #COSTUME_DFLT_Y_DEST                          // default Y destination
        sta costume_dest_y,x

        // Park the costume in the holding room ($2C) and return.
        // This de-associates it from the active room until explicitly reloaded.
        lda #COSTUME_HOLDING_ROOM                          // holding room id
        sta room_for_character,x
        rts                               // done: one costume evicted and parked

advance_costume_evict_scan:
        dex
        bne scan_for_evict            // more candidates (X != 0) → continue first-8 scan

        /*----------------------------------------
         * No eligible costume among first 8 — fail hard:
         *   - Write diagnostic flag ($DC := $05)
         *   - Map I/O at $D000–$DFFF (processor port $01 := $25)
         *   - Set screen border color for a visible alert
         *---------------------------------------*/
        lda #$05
        sta debug_error_code             // app-specific error/diagnostic code

        ldy #MAP_IO_IN                   // enable I/O region 
        sty cpu_port      

costume_evict_hangup:
        sta vic_border_color_reg      		// set border color
        jmp costume_evict_hangup        // infinite loop by design
/*
================================================================================
  rsrc_update_ptr
================================================================================
Summary:
	Dispatches on resource type in Y and writes the resource pointer
	(lo/hi) to the matching pointer table at index X, then returns.
	Each test label (*test_type_…*) checks a specific type code and
	falls through to the next test on mismatch.

Arguments:
	rsrc_ptr            Zero-page pointer to the resource base (lo/hi).
	Y = resource_type   One of:
						 RSRC_TYPE_OBJECT, RSRC_TYPE_COSTUME,
						 RSRC_TYPE_ROOM, RSRC_TYPE_ROOM_LAYERS,
						 RSRC_TYPE_SCRIPT, RSRC_TYPE_SOUND.
	X = resource_index  Table index to update.

Updates:
	object_ptr_lo/hi_tbl[X]
	costume_ptr_lo/hi_tbl[X]
	room_ptr_lo/hi_tbl[X]
	room_gfx_layers_lo/hi[X]
	script_ptr_lo/hi_tbl[X]
	sound_ptr_lo/hi_tbl[X]

Description:
	- Compare Y against known RSRC_TYPE_* codes in ascending checks.
	- On match, store <rsrc_ptr to the *_lo_tbl[X] and >rsrc_ptr to
	 the *_hi_tbl[X], then RTS.
	- On mismatch, fall through to the next *test_type_* label.
	- NOTE (bug/placeholder): if none match, current code jumps to
	 rsrc_evict_one_by_priority; this should trap/halt instead.
================================================================================
*/
* = $5A89
rsrc_update_ptr:
        // Dispatch by resource_type in Y. Each test label *checks* a value and falls through
        // to the next check when not equal. On match, write table[X] := rsrc_ptr and RTS.
        cpy #RSRC_TYPE_ROOM                      
        bne test_type_room_layers

        // type 3: room — publish pointer for room X
        lda rsrc_ptr             // lo byte (little-endian)
        sta room_ptr_lo_tbl,x
        lda rsrc_ptr + 1             // hi byte
        sta room_ptr_hi_tbl,x
        rts

test_type_room_layers:
        cpy #RSRC_TYPE_ROOM_LAYERS                      
        bne test_type_costume

        // type 4: room scene layers — publish pointer for layer set X
        lda rsrc_ptr
        sta room_gfx_layers_lo,x
        lda rsrc_ptr + 1
        sta room_gfx_layers_hi,x
        rts

test_type_costume:
        cpy #RSRC_TYPE_COSTUME                      
        bne test_type_object

        // type 2: costume — publish pointer for costume X
        lda rsrc_ptr
        sta costume_ptr_lo_tbl,x
        lda rsrc_ptr + 1
        sta costume_ptr_hi_tbl,x
        rts

test_type_object:
        cpy #RSRC_TYPE_OBJECT                      
        bne test_type_script

        // type 1: object — publish pointer for object X
        lda rsrc_ptr
        sta object_ptr_lo_tbl,x
        lda rsrc_ptr + 1
        sta object_ptr_hi_tbl,x
        rts

test_type_script:
        cpy #RSRC_TYPE_SCRIPT                      
        bne test_type_sound

        // type 5: script — publish pointer for script X
        lda rsrc_ptr
        sta script_ptr_lo_tbl,x
        lda rsrc_ptr + 1
        sta script_ptr_hi_tbl,x
        rts

test_type_sound:
        cpy #RSRC_TYPE_SOUND                      
        bne rsrc_evict_one_by_priority // BUG - it shouldn't fall through but hang up instead

        // type 6: sound — publish pointer for sound X
        lda rsrc_ptr
        sta sound_ptr_lo_tbl,x
        lda rsrc_ptr + 1
        sta sound_ptr_hi_tbl,x
        rts
/*
================================================================================
  rsrc_evict_one_by_priority
================================================================================
Summary:
	Attempts to evict exactly one resource per call, using a rotating
	priority: room → costume → sound → script. Starts from evict_pri_cur,
	tries that class’ eviction routine, then advances priority (with wrap).
	If any callee frees something, returns true; otherwise cycles priorities
	until it wraps back to the starting priority and returns false.

Reads:
	evict_pri_cur              Current priority selector (0..3).
	rsrc_released_flag         Set ≠ 0 by callees when they free something.

Updates:
	evict_pri_start            Snapshot of starting priority for wrap detection.
	evict_pri_cur              Incremented each attempt; may wrap ($FF→$00).
	rsrc_released_flag         Cleared on entry; may be set by callees.

Returns:
	A = #$FF                   A resource was released (success).
	A = #$00                   Completed a full cycle with no release.

Description:
	1) Clear rsrc_released_flag; save evict_pri_cur into evict_pri_start.
	2) Dispatch based on evict_pri_cur:
		0 → rsrc_release_evictable_room_lru
		1 → rsrc_release_one_evictable_costume
		2 → rsrc_release_evictable_sounds
		3 → rsrc_release_evictable_scripts
	If evict_pri_cur ≥ PRIORITY_WRAP_AT, set it to $FF so INC → $00.
	3) INC evict_pri_cur; if rsrc_released_flag ≠ 0 → return #$FF.
	4) If evict_pri_cur != evict_pri_start → continue dispatch loop.
	Else return #$00 (no release this cycle).
================================================================================
*/
* = $5AE3
rsrc_evict_one_by_priority:
        // Initialize: assume no release this pass (flag := #$00).
        // Policies below will set rsrc_released_flag ≠ 0 on success.
        lda #$00
        sta rsrc_released_flag

        // Snapshot the starting priority so we can detect a full wrap later.
        lda evict_pri_cur
        sta evict_pri_start

dispatch_by_priority:
        // Dispatch on evict_pri_cur (0..3 are valid, >=4 wraps to 0).
        lda evict_pri_cur
        cmp #PRIORITY_ROOM
        bne test_costume

        // case 0 → try releasing one room (LRU policy).
        jsr rsrc_release_evictable_room_lru
        jmp advance_priority

test_costume:
        cmp #PRIORITY_COSTUME
        bne test_sound

        // case 1 → try releasing one costume (not in current room; attrs==0).
        jsr rsrc_release_one_evictable_costume
        jmp advance_priority

test_sound:
        cmp #PRIORITY_SOUND
        bne test_script

        // case 2 → try releasing evictable sounds (skip protected indices).
        jsr rsrc_release_evictable_sounds
        jmp advance_priority

test_script:
        cmp #PRIORITY_SCRIPT
        bne test_wrap_threshold

        // case 3 → try releasing evictable scripts (attrs==0).
        jsr rsrc_release_evictable_scripts
        jmp advance_priority

test_wrap_threshold:
        // If priority >= 4, force wrap on next INC:
        // set to $FF so INC → $00 (wrap to case 0).
        cmp #PRIORITY_WRAP_AT
        bcc advance_priority
        lda #$FF
        sta evict_pri_cur

advance_priority:
        // Advance to next priority; INC $FF → $00 (wrap).
        inc evict_pri_cur

        // Did any callee release something? (flag set ≠ 0)
        lda rsrc_released_flag
        beq wrap_check_no_release

        // Yes → return True (#FF).
        lda #$FF
        rts

wrap_check_no_release:
        // No release yet. Have we wrapped back to the starting priority?
        // If not, continue dispatch loop.
        lda evict_pri_cur
        cmp evict_pri_start
        bne dispatch_by_priority

        // Completed a full cycle with no releases → return False (#00).
        lda #$00
        rts
/*
================================================================================
 rsrc_release_evictable_room_lru
================================================================================
Summary:
	Scans room slots from ROOM_MAX_INDEX down and selects the
	least-recently-used *evictable* room: it must be loaded
	(ptr.hi != 0), not locked (attr bit7 = 0), and have a
	non-zero “age/attr” score. The candidate with the largest
	attr value is evicted: table pointers are cleared and
	mem_release is called.

Reads:
	room_ptr_lo_tbl[X]       Room resource pointer (lo).
	room_ptr_hi_tbl[X]       Room resource pointer (hi); nonzero ⇒ loaded.
	room_mem_attrs[X]        Per-room attribute/age score; bit7=1 ⇒ locked.

Updates:
	room_ptr_*_tbl[X]        Cleared for the evicted room to avoid dangling refs.
	room_mem_attrs[X]        Cleared for the evicted room.

Description:
	- Iterate X := ROOM_MAX_INDEX … 1 (index 0 is not considered).
	- Skip if not loaded (ptr.hi == 0), or if locked (attr bit7 == 1),
	 or if attr == 0 (not evictable/fresh).
	- Track the best candidate using the largest attr value (strictly greater).
	- If a candidate exists:
	   Y := ptr.hi, X := ptr.lo; clear table refs & attrs; call mem_release.
================================================================================
*/
* = $5B38
rsrc_release_evictable_room_lru:
        lda #$00                        // A := 0 (initializer for fields below)
        sta best_attr                   // clear best LRU “staleness” score (0 ⇒ no candidate yet)
        sta best_room_idx               // clear best room index (0 used as “no candidate” sentinel)
        ldx #ROOM_MAX_INDEX             // start scan at highest room index; loop will DEX down to 1 (index 0 skipped)
		
scan_room:
        // Residency gate — only loaded rooms can be evicted:
        // ptr.hi != 0 ⇒ resident; ptr.hi == 0 ⇒ not loaded → skip.
        // BEQ branches when Z=1 (A==0).
        lda room_ptr_hi_tbl,x
        beq advance_room

        // Lock gate — skip rooms marked locked (bit7 set):
        // BMI branches when N=1 (bit7 of A set).
        lda room_mem_attrs,x              // A := attributes for room X
        bmi advance_room                  // locked (bit7=1) → ineligible

        // Eligibility gate — attribute must be non-zero to consider for eviction.
        // CMP #$00 sets Z=1 when A==0 → BEQ skip (in use / pinned / fresh).
        cmp #$00
        beq advance_room

        // LRU selection — prefer strictly larger attr (older/staler):
        // After CMP best_attr:
        //   A < best_attr → C=0  ⇒ BCC skip
        //   A = best_attr → Z=1  ⇒ BEQ skip
        //   A > best_attr → C=1,Z=0 → falls through (new candidate)
        cmp best_attr
        bcc advance_room
        beq advance_room

        // Promote this room as the current best LRU candidate:
        // save its staleness score (A) and its index (X).
        sta best_attr
        stx best_room_idx

advance_room:
        dex                               // examine next room: X := X-1
        bne scan_room                     // loop while X != 0 (index 0 intentionally skipped)
                                          // fall through when X == 0 → selection phase

        // Selection phase: if no candidate was found, best_room_idx remains 0 → nothing to evict.
        lda best_room_idx
        bne evict_room_candidate          // have a candidate → proceed to eviction
        rts                               // none found → return without changes

evict_room_candidate:
        ldx best_room_idx                 // X := selected (LRU) room index

        // Prepare mem_release arguments (expects X=ptr.lo, Y=ptr.hi):
        //   - Load hi first and move to Y
        //   - Load lo and park it on the stack (we’ll TAX it after clearing refs)
        lda room_ptr_hi_tbl,x
        tay                               // Y := ptr.hi
        lda room_ptr_lo_tbl,x
        pha                               // push ptr.lo (will restore → X later)

        // Proactively drop table references to avoid dangling/stale pointers elsewhere
        lda #$00
        sta room_ptr_hi_tbl,x             // mark not-resident: hi := 0
        sta room_ptr_lo_tbl,x             // mark not-resident: lo := 0

        // Mark attributes cleared for the evicted room
        lda #$00
        sta room_mem_attrs,x

        // Recover saved ptr.lo and place it in X for mem_release (expects X=lo, Y=hi).
        pla                               // A := saved ptr.lo (pushed earlier)
        tax                               // X := ptr.lo

        // Free the block at (X=lo, Y=hi). Callee may clobber A/X/Y and flags.
        jsr mem_release
        rts                               // done: room entry cleared and memory released
/*
================================================================================
  rsrc_release_one_evictable_costume
================================================================================
Summary:
	Scans costume slots from COSTUME_MAX_INDEX down and frees the first
	costume that is both loaded and not in the current room, provided
	its attributes mark it evictable (attrs == 0). Exits immediately
	after freeing one costume.

Reads:
	costume_ptr_lo_tbl[X]     Costume resource pointer (lo).
	costume_ptr_hi_tbl[X]     Costume resource pointer (hi); nonzero ⇒ loaded.
	costume_mem_attrs[X]      Per-costume attributes; 0 ⇒ evictable.
	room_for_character[X]     Owning room index for costume X.
	current_room              Index of the current room (active/in use).

Updates:
	costume_ptr_*_tbl[X]      Cleared for the freed costume to avoid dangling refs.
	costume_mem_attrs[X]      Cleared for the freed costume.

Description:
	- Iterate X := COSTUME_MAX_INDEX … 0.
	- Skip if not loaded (ptr.hi == 0) or if in current_room (in use).
	- Skip if attrs != 0 (not evictable); attrs == 0 ⇒ candidate.
	- Preserve loop index on stack; prepare mem_release calling convention:
	   X := ptr.lo, Y := ptr.hi.
	- Clear table pointers and attributes *before* freeing to prevent stale uses.
	- Call mem_release and return.
================================================================================
*/
* = $5B84
rsrc_release_one_evictable_costume:
        ldx #COSTUME_MAX_INDEX        // start from highest costume index; iterate X downward

scan_costume:
        // Residency gate — only loaded costumes can be freed:
        // ptr.hi != 0 ⇒ resident. LDA sets Z=1 when A==0 ⇒ BEQ → not loaded → skip.
        lda costume_ptr_hi_tbl,x
        beq advance_costume

        // Usage gate — skip costumes currently in the active room:
        // room_for_character[X] == current_room ⇒ in use. CMP sets Z=1 on equal ⇒ BEQ skip.
        lda room_for_character,x
        cmp current_room
        beq advance_costume

        // Policy gate — only idle/unpinned costumes are evictable:
        // costume_mem_attrs[X] == 0 ⇒ eligible. BNE (Z=0) ⇒ non-zero attrs → skip.
        lda costume_mem_attrs,x
        bne advance_costume

        // Set up mem_release(X=ptr.lo, Y=ptr.hi) while preserving the scan index.
        // We push (index, ptr.lo) so we can (1) restore X after the free and
        // (2) TAX ptr.lo after clearing table refs below.
        lda costume_ptr_hi_tbl,x
        tay                               // Y := ptr.hi (mem_release calling convention)
        lda costume_ptr_lo_tbl,x
        pha                               // push ptr.lo; will TAX after clearing refs

        // Clear table references *before* freeing to avoid dangling/stale pointers.
        // Mark "not resident" by zeroing both bytes.
        lda #$00
        sta costume_ptr_lo_tbl,x          // ptr.lo := 0
        sta costume_ptr_hi_tbl,x          // ptr.hi := 0
		// Clear memory attributes
		sta costume_mem_attrs,X
        // Restore saved ptr.lo → X for mem_release (expects X=lo, Y=hi).
        pla                               // A := saved ptr.lo
        tax                               // X := ptr.lo

        // Free the block at (X=lo, Y=hi). Callee may clobber A/X/Y/flags.
        jsr mem_release
        rts                               

advance_costume:
        dex                               // X := X-1 → next candidate
        bne scan_costume                  // Z=0 ⇒ more to scan; Z=1 ⇒ finished
        rts
/*
================================================================================
  rsrc_release_evictable_sounds
================================================================================
Summary:
	Scans sound slots from SOUND_MAX_INDEX down to 0. For each slot,
	if its attributes indicate “evictable” (attrs == 0) and it is
	loaded (pointer hi-byte != 0), clears the table entry and calls
	mem_release to free the block. Protected slots are skipped.


Reads:
	sound_attr_tbl[X]         Per-sound attributes; 0 ⇒ evictable.
	sound_ptr_lo_tbl[X]        Sound resource pointer (lo).
	sound_ptr_hi_tbl[X]        Sound resource pointer (hi); nonzero ⇒ loaded.

Updates:
	sound_ptr_*_tbl[X]         Cleared to zero for each released sound.

Description:
	- Iterate X := SOUND_MAX_INDEX … 0.
	- Skip if attrs != 0 (not evictable) or ptr.hi == 0 (not loaded).
	- Skip protected indices SOUND_PROTECT_1, _2, _3.
	- Preserve X on stack; prepare mem_release calling convention:
	   X := ptr.lo, Y := ptr.hi.
	- Clear table pointers *before* freeing to avoid dangling references.
	- Call mem_release; restore X (loop index) and continue.
================================================================================
*/
* = $5BB5
rsrc_release_evictable_sounds:
        ldx #SOUND_MAX_INDEX          // start from the highest sound index; iterate X downward

scan_sound:
        // Policy gate — only idle/unpinned sounds are evictable:
        // sound_attr_tbl[X] == 0  ⇒ eligible
        // LDA sets Z=1 when A==0; BNE (Z=0) ⇒ non-zero attrs → skip.
        lda sound_attr_tbl,x
        bne advance_sound              // in use/locked → not eligible

        // Residency gate — only loaded sounds can be freed:
        // ptr.hi != 0 ⇒ resident; ptr.hi == 0 ⇒ not loaded.
        // BEQ (Z=1) when hi==0 → skip.
        lda sound_ptr_hi_tbl,x
        beq advance_sound              // not resident → nothing to release

        // Protection gate — reserved sounds must never be evicted.
        // CPX sets Z=1 when X == IMM; BEQ branches on Z=1 → skip eviction on match.
        cpx #SOUND_PROTECT_1        
        beq advance_sound           // yes → keep; do not evict
        cpx #SOUND_PROTECT_2        
        beq advance_sound
        cpx #SOUND_PROTECT_3        
        beq advance_sound

        // Set up call to mem_release(X=ptr.lo, Y=ptr.hi) while preserving the scan index.
        txa                               // A := current sound index (copy of X)
        pha                               // push index so we can restore it after freeing
        lda sound_ptr_hi_tbl,x
        tay                               // Y := ptr.hi (mem_release calling convention)
        lda sound_ptr_lo_tbl,x
        pha                               // stash ptr.lo on stack; will TAX after clearing refs

        // Drop table references *before* freeing to avoid dangling/stale pointers elsewhere.
        // Order matters: clear both lo/hi to mark "not resident" prior to mem_release.
        lda #$00
        sta sound_ptr_lo_tbl,x            // ptr.lo := 0
        sta sound_ptr_hi_tbl,x            // ptr.hi := 0

        // Restore saved ptr.lo → X for mem_release (expects X=lo, Y=hi).
        pla                               // A := saved ptr.lo
        tax                               // X := ptr.lo

        // Free the block at (X=lo, Y=hi). Callee may clobber A/X/Y/flags.
        jsr mem_release

        // Recover the loop index and continue scanning candidates.
        pla                               // A := saved sound index
        tax                               // X := sound index

advance_sound:
        dex                               // X := X-1 → next candidate
        bne scan_sound                    // Z=0 ⇒ more to scan; Z=1 ⇒ finished
        rts
/*
================================================================================
  rsrc_release_evictable_scripts
================================================================================
Summary:
	Scans all script slots from highest index down. For each script,
	if its memory attributes indicate “evictable” (attrs==0) and it is
	loaded (pointer hi-byte != 0), clears the table entry and calls
	mem_release to free the block.

Reads:
	script_memory_attrs[X]  Per-script memory attributes; 0 ⇒ evictable.
	script_ptr_lo_tbl[X]    Script resource pointer (lo).
	script_ptr_hi_tbl[X]    Script resource pointer (hi); nonzero ⇒ loaded.

Updates:
	script_ptr_*_tbl[X]     Cleared to zero for each released script.
	rsrc_released_flag 		Set by mem_release upon any successful free.

Description:
	- Iterate X := SCRIPT_MAX_INDEX .. 0.
	- Skip if attrs != 0 (not evictable) or pointer hi == 0 (not loaded).
	- Preserve X (index) on stack; form (X=ptr.lo, Y=ptr.hi) for mem_release.
	- Clear table pointers *before* freeing to avoid dangling references.
	- Restore index and continue scanning until all candidates processed.
================================================================================
*/
* = $5BEA
rsrc_release_evictable_scripts:
        ldx #SCRIPT_MAX_INDEX        // start from highest script index; iterate X downward

scan_script:
        // Policy gate — must be idle/unpinned to evict:
        // attr == 0 ⇒ evictable; attr != 0 ⇒ keep.
        // LDA sets Z=1 when A==0; BNE (Z=0) → non-zero attrs → skip.
        lda script_mem_attrs,x
        bne advance_script           // in use/locked → not eligible

        // Residency gate — only loaded scripts can be freed:
        // pointer.hi != 0 ⇒ in memory; pointer.hi == 0 ⇒ not loaded.
        // BEQ (Z=1) when hi==0 → skip.
        lda script_ptr_hi_tbl,x
        beq advance_script           // not resident → nothing to release

        // Set up for mem_release(X=ptr.lo, Y=ptr.hi) and keep loop index intact.
        txa                               // A := script index (copy of X)
        pha                               // push index to stack → restore after free
        lda script_ptr_hi_tbl,x
        tay                               // Y := ptr.hi (mem_release calling convention)
        lda script_ptr_lo_tbl,x
        pha                               // stash ptr.lo on stack; will TAX after clearing refs

        // Clear table references *before* freeing to prevent dangling/stale pointers.
        lda #$00
        sta script_ptr_lo_tbl,x           // ptr.lo := 0 (not resident)
        sta script_ptr_hi_tbl,x           // ptr.hi := 0

        // Restore ptr.lo into X for mem_release (expects X=lo, Y=hi).
        pla                               // A := saved ptr.lo
        tax                               // X := ptr.lo

        // Free the block at (X=lo, Y=hi). Callee may clobber A/X/Y/flags.
        jsr mem_release

        // Restore loop index (saved before the free) and continue scan.
        pla                               // A := saved script index
        tax                               // X := script index

advance_script:
        dex                               // X := X-1 → next candidate
        bne scan_script                   // more to scan? (Z=0) → loop; else done
        rts
		