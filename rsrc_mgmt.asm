/*
================================================================================
  Resource management overview
================================================================================

This file implements the full lifecycle for SCUMM-style resources (rooms,
costumes, sounds, scripts, objects) between disk and the heap-like memory
manager. It provides helpers to ensure a resource is resident, a shared loader
that copies and validates resources from disk into RAM, and an eviction
framework that frees no-longer-needed data when memory runs low.

At the front of each resource on disk there is a small header containing the
total size, a sentinel byte, and a checksum. The loader copies this header into
a scratch buffer, validates the sentinel, and computes the total bytes required
(header + payload). It then asks the memory manager for a block of that size
and streams the entire resource into the allocated region. A checksum pass over
the payload verifies integrity; if it fails, the loader retries and shows a
disk error prompt before trying again.

Once the resource is safely in RAM, the loader writes a compact in-memory
header at the base of the block. This in-memory header records the payload
size, the resource type (room, costume, sound, script, etc.), and the resource
index. The module then publishes the block’s base pointer into the appropriate
global pointer table (room_ptr_*_tbl, costume_ptr_*_tbl, sound_ptr_*_tbl,
script_ptr_*_tbl) so the rest of the engine can locate the resource quickly.

Per-type cache helpers (rsrc_cache_costume, rsrc_cache_sound, rsrc_cache_script)
all follow the same pattern: they check the corresponding pointer table; if the
entry is already nonzero, they return immediately. On a miss, they look up the
owning room and the room’s disk side, call room_disk_chain_prepare to ensure
the correct disk is active and the chain is positioned, compute the per-type
disk offset and sector for the resource, and then call the shared disk loader.
When the loader returns, they update the type-specific pointer table with the
new base address and restore any caller-saved registers.

room_disk_chain_prepare is the bridge between resource indices and the disk
driver. Given a room index, it checks whether the room lives on the current
disk side and, if not, asks the high-level disk code to swap sides. It then
reads the room’s starting sector/track pair from a table, seeds the disk-chain
state (chain_sector, chain_track), and initializes the disk chain so subsequent
reads for this room’s resources can be expressed as relative seeks.

For relocation and compaction, rsrc_update_ptr provides a single entry point to
update resource tables after the memory manager moves a block. The caller sets
resource_type, resource_index, and rsrc_ptr_lo/hi to the new base address, and
this routine writes the new pointer into the correct global table based on the
type. This keeps all resource-pointer updates consistent when the heap is
repacked.

When memory is tight, rsrc_evict_one_by_priority coordinates eviction. It keeps
a rotating priority class selector (rooms → costumes → sounds → scripts) and,
on each call, dispatches to a per-type eviction routine. Each routine scans its
pointer table and attribute flags, looking for a block that is resident and not
locked or in active use. Rooms use an age/attribute field to approximate LRU;
costumes cannot be evicted if they belong to the current room; sounds and
scripts skip entries with nonzero attributes and, for sounds, a small set of
“protected” indices. Once a candidate is found, the routine clears the pointer
table entry, calls mem_release on the block, and signals success back to the
priority scheduler.

Taken together, these pieces form a layered resource system:

  - Disk-side tables describe where each resource lives (offset + sector/track).
  - room_disk_chain_prepare syncs disk state for the relevant room/side.
  - rsrc_load_from_disk copies and validates the resource into heap memory.
  - rsrc_hdr_init tags the block with type/index and returns a usable pointer.
  - Cache helpers expose “ensure resident” operations per resource type.
  - rsrc_update_ptr keeps tables correct when the heap relocates blocks.
  - The eviction framework reclaims space according to per-type policies.

The rest of the engine can therefore treat resource access as simple table
lookups, leaving disk I/O, integrity checking, and memory pressure handling to
this module.

================================================================================
  Techniques and algorithms used in this module
================================================================================

This module combines several classic systems-programming techniques to manage
SCUMM-style resources efficiently on a constrained machine:

  • Demand-paged resource caching  
      Each “ensure resident” routine (costume/sound/script) checks a pointer
      table and loads from disk only on a miss. Pointer-table indirection allows
      all other subsystems to treat resources as stable handles.

  • Disk-chain–based streaming I/O  
      room_disk_chain_prepare maps room indices to disk side / sector / track
      and seeds a streaming chain, so all later resource reads use unified
      relative offsets rather than raw track/sector logic.

  • Header + payload format with integrity checks  
      Each on-disk resource carries a compact header (size/sentinel/checksum).
      The loader copies this header, validates the sentinel, then copies the
      full resource and verifies an XOR checksum over the payload.

  • Normalized in-memory headers  
      After loading, the code rewrites a clean in-RAM header (payload size,
      type, index), separating disk format from engine format.

  • Heap-relocation support via pointer updates  
      A centralized rsrc_update_ptr routine rewrites the appropriate pointer
      table entry after the memory manager moves a block, enabling compaction.

  • Priority-rotating eviction scheduler  
      rsrc_evict_one_by_priority cycles fairly through eviction classes
      (rooms → costumes → sounds → scripts) until one frees memory or a full
      rotation completes without success.

  • Per-type eviction policies  
      Rooms use an LRU-like “max age” selection; costumes skip those belonging
      to the active room; sounds and scripts evict entries whose reference/
      attribute flags are zero, with special protected-sound exclusions.

  • Attribute-based locking  
      Nonzero mem-attribute fields for rooms, costumes, sounds, and scripts pin
      resources in memory, preventing the eviction routines from releasing
      blocks still in use.

================================================================================
*/

#importonce
#import "globals.inc"
#import "constants.inc"
#import "rsrc_metadata.inc"
#import "memory_mgmt.asm"
#import "disk_high_level.asm"
#import "pause.asm"
#import "actor.asm"
#import "misc.asm"

// On-disk resource header
.const RSRC_DHDR_BYTES              = $04   // On-disk header size (bytes).
.const RSRC_DHDR_OFF_SIZE_LO        = $00   // Disk header: +0  size.lo   (total bytes, little-endian).
.const RSRC_DHDR_OFF_SIZE_HI        = $01   // Disk header: +1  size.hi.
.const RSRC_DHDR_OFF_SENTINEL       = $02   // Disk header: +2  sentinel byte (must be 0).
.const RSRC_DHDR_OFF_CHECKSUM       = $03   // Disk header: +3  checksum (expected value).

// In-memory resource header
.const RSRC_MHDR_BYTES              = $04   // In-memory header size (bytes).
.const RSRC_MHDR_OFF_SIZE_LO        = $00   // Memory header: +0  size.lo   (total bytes, little-endian).
.const RSRC_MHDR_OFF_SIZE_HI        = $01   // Memory header: +1  size.hi.
.const RSRC_MHDR_OFF_TYPE           = $02   // Memory header: +2  resource type id.
.const RSRC_MHDR_OFF_INDEX          = $03   // Memory header: +3  resource index within type.

.const PROTECTED_SOUND_3     		= $03    // Reserved/non-evictable sound index #3 (skip during eviction).
.const PROTECTED_SOUND_4     		= $04    // Reserved/non-evictable sound index #4 (skip during eviction).
.const PROTECTED_SOUND_5     		= $05    // Reserved/non-evictable sound index #5 (skip during eviction).

.const PRIORITY_ROOM       			= $00    // Eviction dispatch code: try releasing one ROOM.
.const PRIORITY_COSTUME    			= $01    // Eviction dispatch code: try releasing one COSTUME.
.const PRIORITY_SOUND      			= $02    // Eviction dispatch code: try releasing EVICTABLE SOUNDS.
.const PRIORITY_SCRIPT     			= $03    // Eviction dispatch code: try releasing EVICTABLE SCRIPTS.
.const PRIORITY_WRAP_AT    			= $04    // Threshold: if evict_pri_cur ≥ this, set to $FF so INC → $00 (wrap to ROOM).


.label costume_idx_saved            = $15    // Saved costume index across disk/room lookup while caching a costume
.label tmp_ptr_lo                   = $17    // Scratch: low byte of a resource base pointer (for table updates)
.label tmp_ptr_hi                   = $18    // Scratch: high byte of a resource base pointer (for table updates)
.label script_idx_saved             = $19    // Saved script index during script caching (shares ZP with other aliases)
.label sound_idx_saved              = $19    // Saved sound index during sound caching (mutually exclusive with script/ptr use)
.label rsrc_ptr_lo_tmp              = $19    // Scratch: low byte of relocated resource pointer (shares ZP with *_idx_saved)
.label rsrc_ptr_hi_tmp              = $1A    // Scratch: high byte of relocated resource pointer (paired with rsrc_ptr_lo_tmp)
.label chain_sector                 = $1D    // Current disk-chain sector for the active room/resource stream
.label chain_track                  = $1E    // Current disk-chain track for the active room/resource stream
.label rsrc_ptr_lo                  = $4F    // Generic low byte of resource base pointer (used by rsrc_update_ptr)
.label rsrc_ptr_hi                  = $50    // Generic high byte of resource base pointer (used by rsrc_update_ptr)
.label rsrc_hdr_ptr_lo              = $4F    // ZP pointer: low byte of in-memory header base (aliases rsrc_ptr_lo)
.label rsrc_hdr_ptr_hi              = $50    // ZP pointer: high byte of in-memory header base (aliases rsrc_ptr_hi)
.label rsrc_payload_ptr_lo          = $E4    // 16-bit pointer: low byte of payload cursor (first byte after header)
.label rsrc_payload_ptr_hi          = $E5    // 16-bit pointer: high byte of payload cursor (first byte after header)
.label rsrc_payload_len_lo          = $E6    // 16-bit remaining payload length: low byte (checksum loop counter)
.label rsrc_payload_len_hi          = $E7    // 16-bit remaining payload length: high byte (checksum loop counter)
.label rsrc_chk_accum               = $E8    // Running XOR checksum accumulator over payload bytes
.label rsrc_chk_expected            = $E9    // Expected checksum byte loaded from disk header
.label rsrc_retry_rem               = $EA    // Remaining retries for load/verify before showing disk error UI

.label saved_y                      = $3BDC  // Saved Y register across rsrc_cache_* calls that clobber caller’s Y
.label saved_room_idx               = $3BDD  // Saved room index across disk_ensure_side in room_disk_chain_prepare

.label rsrc_hdr_scratch             = $553B  // 4-byte scratch buffer for copied on-disk header (size/sentinel/checksum)
.label rsrc_total_bytes_lo          = $553B  // 16-bit total size (header + payload), low byte; aliases rsrc_hdr_scratch
.label rsrc_total_bytes_hi          = $553C  // 16-bit total size (header + payload), high byte

.label rsrc_data_ptr_lo             = $553F  // 16-bit base pointer to allocated resource block in RAM, low byte
.label rsrc_data_ptr_hi             = $5540  // 16-bit base pointer to allocated resource block in RAM, high byte

.label rsrc_raw_size_lo             = $FD9C  // 16-bit raw payload size (bytes) for in-memory header, low byte
.label rsrc_raw_size_hi             = $FD9D  // 16-bit raw payload size (bytes) for in-memory header, high byte

.label evict_pri_cur                = $FE4B  // Current eviction class (0=room,1=costume,2=sound,3=script; wraps at PRIORITY_WRAP_AT)
.label evict_pri_start              = $FE4C  // Eviction priority snapshot used to detect a full rotation with no release

.label oldest_age                   = $FD9F  // Best-so-far nonzero room age/attr value when scanning for an eviction candidate
.label oldest_room_idx              = $FD9E  // Room index associated with oldest_age; 0 means “no candidate yet”


/*
================================================================================
  rsrc_cache_costume
================================================================================
Summary:
	Cache a costume resource in memory.
	
	Returns immediately if the costume is already resident. 
	Otherwise, selects the correct disk	location from the owning room, 
	programs the disk stream position, 	loads the costume via the generic 
	resource loader, and publishes the resulting pointer to the costume pointer tables.

Arguments:
	X		Costume resource index

Description:
	1) Fast path: if costume already in memory, exit
	2) Miss path:
		- Save costume index
		- Resolve room for costume
		- Prepare disk chain load
		- Resolve disk location
		- Set resource type and load from disk
		- Publish pointer into costume tables
================================================================================
*/
* = $38A8
rsrc_cache_costume:
        // Stash the incoming costume index (X) so we can publish the pointer later.
        stx     rsrc_resource_index

        // ------------------------------------------------------------
        // Residency check
		//
        //   costume_ptr_hi_tbl[X] != 0  ⇒ already resident in RAM.
        // ------------------------------------------------------------
        lda     costume_ptr_hi_tbl,x
        beq     costume_cache_miss     	// miss → go load it
        rts                             // hit  → nothing to do

costume_cache_miss:
        // Preserve the original costume index across room resolution & chain setup
        stx     costume_idx_saved

        // ------------------------------------------------------------
        // Determine owning room for this costume and prepare the disk chain
		//
        //   X := room index (selects disk side), seed track/sector chain for that room
        // ------------------------------------------------------------
        lda     costume_room_res_idx,x
        tax
        jsr     room_disk_chain_prepare

        // ------------------------------------------------------------
        // Re-select the costume’s disk-location pair (2 bytes per entry)
		//
        //   Each entry = [offset-within-sector, sector-index]
        // ------------------------------------------------------------
        lda     costume_idx_saved
        asl                             // A := 2 * index
        tax                             // X := byte offset into pair table
        lda     costume_disk_loc_tbl,x
        sta     rsrc_read_offset
        lda     costume_disk_loc_tbl + 1,x
        sta     rsrc_sector_idx

        // ------------------------------------------------------------
        // Identify resource class and perform the load
		//
        //   rsrc_resource_type := COSTUME 
        //   rsrc_load_from_disk → returns X=ptr.lo, Y=ptr.hi (payload start)
        // ------------------------------------------------------------
        lda     #RSRC_TYPE_COSTUME                    
        sta     rsrc_resource_type
        jsr     rsrc_load_from_disk

        // ------------------------------------------------------------
        // Publish the returned pointer into the costume tables at the original index.
        // Stash X/Y temporarily so we can use Y as the table index (resource_index).
        // ------------------------------------------------------------
        stx     tmp_ptr_lo                  
        sty     tmp_ptr_hi                  
		
        ldy     rsrc_resource_index     // Y := costume index for table write
        lda     tmp_ptr_lo
        sta     costume_ptr_lo_tbl,y    
        lda     tmp_ptr_hi
        sta     costume_ptr_hi_tbl,y    
        rts		
/*
================================================================================
  rsrc_cache_sound
================================================================================
Summary:
	Cache a sound resource in memory.
	
	Returns immediately if the sound is already resident. 
	Otherwise, selects the correct disk	location from the owning room, 
	programs the disk stream position, 	loads the sound via the generic 
	resource loader, and publishes the resulting pointer to the sound pointer tables.
	
Arguments:
	A		Sound resource index

Description:
	1) Fast path: if sound already in memory, exit
	2) Miss path:
		- Save sound index
		- Resolve room for sound
		- Prepare disk chain load
		- Resolve disk location
		- Set resource type and load from disk
		- Publish pointer into sound tables
================================================================================
*/
* = $39E6
rsrc_cache_sound:
        // Save caller’s Y
        sty saved_y

        // Resource index arrives in A, copy to X
        sta  rsrc_resource_index
        tax

        // ------------------------------------------------------------
        // Residency check
		//
        //   sound_ptr_hi_tbl[X] != 0  ⇒ already resident in RAM.
        // ------------------------------------------------------------
        lda sound_ptr_hi_tbl,x
        beq sound_cache_miss			// miss → go load it
        rts                             // hit  → nothing to do  

sound_cache_miss:
        // Save sound index (X) for later table writes
		stx sound_idx_saved             
		
        // ------------------------------------------------------------
        // Determine owning room for this sound and prepare the disk chain
		//
        //   X := room index (selects disk side), seed track/sector chain for that room
        // ------------------------------------------------------------		
        lda sound_room_idx_tbl,x        
        tax                             
        jsr room_disk_chain_prepare     

        // ------------------------------------------------------------
        // Re-select the sound’s disk-location pair (2 bytes per entry)
		//
        //   Each entry = [offset-within-sector, sector-index]
        // ------------------------------------------------------------
        lda sound_idx_saved
        asl                             // A := 2 * sound_index
        tax                             // X := table byte offset
        lda sound_disk_loc_tbl,x
        sta rsrc_read_offset
        lda sound_disk_loc_tbl + 1,x
        sta rsrc_sector_idx

        // ------------------------------------------------------------
        // Identify resource class and perform the load
		//
        //   rsrc_resource_type := SOUND
        //   rsrc_load_from_disk → X=ptr.lo, Y=ptr.hi (payload pointer)
        // ------------------------------------------------------------
        lda #RSRC_TYPE_SOUND                          
        sta rsrc_resource_type
        jsr rsrc_load_from_disk

        // ------------------------------------------------------------
        // Publish the returned pointer into the sound tables
        // Stash X/Y temporarily so we can use Y as the table index (resource_index).
        // ------------------------------------------------------------
        stx rsrc_ptr_lo_tmp                     
        sty rsrc_ptr_hi_tmp                     
		
        ldy rsrc_resource_index			// Y := sound index used for tables
        lda rsrc_ptr_lo_tmp
        sta sound_ptr_lo_tbl,y             
        lda rsrc_ptr_hi_tmp
        sta sound_ptr_hi_tbl,y             

        // Restore caller’s Y
        ldy saved_y
        rts
/*
================================================================================
  rsrc_cache_script
================================================================================
Summary:
	Cache a script resource in memory.
	
	Returns immediately if the script is already resident. 
	Otherwise, selects the correct disk	location from the owning room, 
	programs the disk stream position, 	loads the script via the generic 
	resource loader, and publishes the resulting pointer to the script pointer tables.

Arguments:
	A		Script resource index (bit7 encodes scope: 0=global, 1=room)

Notes:
	- The ASL on the script index doubles it for 2-byte tables and also captures
	 the original bit7 into C: C=0 ⇒ global script; C=1 ⇒ room script.
================================================================================
*/
* = $3A29
rsrc_cache_script:
        // Save caller’s Y		 
        sty saved_y                        
		
        // Resource index arrives in A, copy to X
        sta rsrc_resource_index                 
        tax                                
		
        // ------------------------------------------------------------
        // Residency check
		//
        //   script_ptr_hi_tbl[X] != 0  ⇒ already resident in RAM.
        // ------------------------------------------------------------
        lda script_ptr_hi_tbl,x            
        beq script_cache_miss           // miss → go load it
        rts                             // hit  → nothing to do  

script_cache_miss:
        // Save script index (X) for later table writes
        stx script_idx_saved               
		
        // ------------------------------------------------------------
        // Determine owning room for this script and prepare the disk chain
		//
        //   X := room index (selects disk side), seed track/sector chain for that room
        // ------------------------------------------------------------		
        lda script_room_idx_tbl,x          
        tax                                
        jsr room_disk_chain_prepare        

        // ------------------------------------------------------------
        // Re-select the sound’s disk-location pair (2 bytes per entry)
		//
        //   Each entry = [offset-within-sector, sector-index]
        // ------------------------------------------------------------
        lda script_idx_saved            // A := script index (bit7 encodes scope)
        asl                             // A := 2 * index; C := original bit7 (scope flag)
        tax                             // X := byte index into pair table (offset/sector)
        bcc scope_global_script       	// If C=0 → use GLOBAL tables; else fall through to ROOM tables

        // Room-scoped script - use per-room disk-location table
        lda room_script_disk_loc_tbl,x     
        sta rsrc_read_offset               
        lda room_script_disk_loc_tbl+1,x   
        sta rsrc_sector_idx                
        jmp load_script_from_disk

scope_global_script:
        // Global-scoped script - use global disk-location table
        lda global_script_disk_loc_tbl,x   
        sta rsrc_read_offset               
        lda global_script_disk_loc_tbl+1,x 
        sta rsrc_sector_idx                


load_script_from_disk:
        // ------------------------------------------------------------
        // Identify resource class and perform the load
		//
        //   rsrc_resource_type := SCRIPT
        //   rsrc_load_from_disk → X=ptr.lo, Y=ptr.hi (payload pointer)
        // ------------------------------------------------------------
        lda #RSRC_TYPE_SCRIPT              
        sta rsrc_resource_type
        jsr rsrc_load_from_disk            

        // ------------------------------------------------------------
        // Publish the returned pointer into the script tables
        // Stash X/Y temporarily so we can use Y as the table index (resource_index).
        // ------------------------------------------------------------
        stx rsrc_ptr_lo_tmp              
        sty rsrc_ptr_hi_tmp              

        ldy rsrc_resource_index       	// Y := original script index (table row)
        lda rsrc_ptr_lo_tmp
        sta script_ptr_lo_tbl,y       
        lda rsrc_ptr_hi_tmp
        sta script_ptr_hi_tbl,y       

        // Restore caller’s Y
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
	X		Index into per-room disk tables.

Description:
	- If the current disk side doesn’t match active_side_id, call disk_ensure_side.
	- Resolve room's disk location to fetch sector and track
	- Call disk_init_chain with X=sector, Y=track.
================================================================================
*/
* = $3ABD
room_disk_chain_prepare:
        // ------------------------------------------------------------
		// Active Disk Side matches the room’s required side?
        // If so, continue
        // ------------------------------------------------------------
        lda room_disk_side_tbl,x          // A := required side ID for room X
        cmp active_side_id                // compare against currently active side
        beq side_ready                    // equal → side already correct

        // ------------------------------------------------------------
        // Side mismatch: switch and verify the correct side
        // ------------------------------------------------------------
        stx saved_room_idx                // save room index across helper
        jsr disk_ensure_side              // ensure required side is mounted (may prompt/pause)
        ldx saved_room_idx                // restore room index

side_ready:
        // ------------------------------------------------------------
		// Resolve disk location for room (track and sector)
		//
        // Index into room_sec_trk_tbl: each entry is a 2-byte pair [SECTOR, TRACK].
        // ------------------------------------------------------------
        txa
        asl                               // A := room_idx * 2  (carry ignored)
        tax                               // X := byte index into pair table

        // ------------------------------------------------------------
        // Legacy sanity check: (sector | track) != 0 ?
		//
        // Control flow is effectively unchanged - the next label is fetch_sector_track
        // either way. Retained to mirror the original code.
        // ------------------------------------------------------------
        lda room_sec_trk_tbl+ROOM_ST_OFF_SECTOR,x   // byte 0: sector
        ora room_sec_trk_tbl+ROOM_ST_OFF_TRACK,x    // byte 1: track
        bne fetch_sector_track

fetch_sector_track:
        // ------------------------------------------------------------
        // Load the room disk location [SECTOR, TRACK] from the table.
        // ------------------------------------------------------------
        lda room_sec_trk_tbl+ROOM_ST_OFF_SECTOR,x   
        sta chain_sector                            
        lda room_sec_trk_tbl+ROOM_ST_OFF_TRACK,x    
        sta chain_track                             

        // ------------------------------------------------------------
        // Initialize the disk chain at (track, sector).
		//
        // disk_init_chain expects X=sector, Y=track.
        // ------------------------------------------------------------
        ldx chain_sector
        ldy chain_track
        jsr disk_init_chain                         
        rts
/*
================================================================================
  rsrc_load_from_disk
================================================================================
Summary:
	Loads a resource from disk.
	
	-Pauses the game loop
	-Seeks the disk stream to the resource
	-Copies and validates the on-disk header
	-Allocates memory for the full resource
	-Copies the payload
	-Computes and verifies the checksum
	-Continues into rsrc_hdr_init on success

Arguments:
	rsrc_sector_idx 		Which sector in the physical chain.
	rsrc_read_offset		Byte offset into the resource stream.
	rsrc_total_bytes		16-bit total size in bytes.

Globals:   
	rsrc_data_ptr 						set to allocated destination.
	rsrc_payload_ptr, rsrc_payload_len 	prepared.

Description:
	1) Pause game; seek to header start.
	2) Copy header into rsrc_hdr_scratch; validate header sentinel == 0.
		If not zero: run disk_id_check and retry from the top.
	3) Allocate memory for rsrc_total_bytes; publish result in rsrc_data_ptr.
	4) Seek stream to the payload start and copy rsrc_total_bytes bytes to rsrc_data_ptr.
	5) Prepare checksum:
		rsrc_hdr_ptr := rsrc_data_ptr
		rsrc_payload_len := rsrc_total_bytes − header size
		rsrc_payload_ptr := rsrc_data_ptr + header size
		rsrc_chk_expected := rsrc_hdr_ptr[checksum_offset]
	6) Checksum: XOR-accumulate payload bytes into rsrc_chk_accum
		When length is 0, compare with rsrc_chk_expected. 
		If equal → continue to rsrc_hdr_init; otherwise decrement rsrc_retry_rem.
		If retries remain → retry_copy payload; else show disk_error_msg and retry after user ack.
================================================================================
*/
* = $5541
rsrc_load_from_disk:
        // Pause the game loop while performing disk I/O to avoid problems.
        jsr pause_game

		// ------------------------------------------------------------
		// Read the first data sector for this resource:
		//
		// X = rsrc_read_offset		(byte offset into the resource stream)
		// Y = rsrc_sector_idx     	(which sector in the physical chain)
		//
		// On return, the disk stream is positioned at the start of the resource header.
		// ------------------------------------------------------------
        ldx rsrc_read_offset
        ldy rsrc_sector_idx
        jsr disk_seek_read

		// ------------------------------------------------------------
		// Copy the on-disk resource header into local scratch (rsrc_hdr_scratch).
		//
		// 		dest = rsrc_hdr_scratch
		// 		count = RSRC_DHDR_BYTES bytes
		//
		// After this, the stream pointer will have read past the header
		// ------------------------------------------------------------
        ldx #<rsrc_hdr_scratch
        ldy #>rsrc_hdr_scratch
		
        lda #RSRC_DHDR_BYTES
        sta disk_copy_count_lo
		
        lda #$00
        sta disk_copy_count_hi
		
        jsr disk_stream_copy

		// ------------------------------------------------------------
		// Validate the header’s sentinel (must be 0).
		// ------------------------------------------------------------
        lda rsrc_hdr_scratch + RSRC_DHDR_OFF_SENTINEL
        beq allocate_payload		// Sentinel ok? Continue
		
		// Sentinel validation failed - check the disk side and try again
        jsr disk_id_check
        jmp rsrc_load_from_disk

allocate_payload:
		// ------------------------------------------------------------
		// Prepare block allocation size
		//
		// X/Y = rsrc_total_bytes
		// ------------------------------------------------------------
        ldx rsrc_total_bytes_lo
        ldy rsrc_total_bytes_hi

		// ------------------------------------------------------------
		// Request a block large enough for the payload + header
		// ------------------------------------------------------------
        jsr mem_alloc

		// ------------------------------------------------------------
		// Publish the destination payload pointer for subsequent copies
		//
		// rsrc_data_ptr := returned X/Y (lo/hi)
		// ------------------------------------------------------------
        stx rsrc_data_ptr_lo
        sty rsrc_data_ptr_hi

        // Mark that we have at least one retry opportunity for this load sequence.
        lda #$01
        sta rsrc_retry_rem

copy_payload_from_disk:
		// Seek to resource start (sector index + offset)
		ldx rsrc_read_offset 
		ldy rsrc_sector_idx
		jsr disk_seek_read		
		
		// ------------------------------------------------------------
		// Copy the entire resource from the disk stream into the allocated block
		//
		// dest    = rsrc_data_ptr
		// count   = rsrc_total_bytes 
		// ------------------------------------------------------------
        ldx rsrc_data_ptr_lo
        ldy rsrc_data_ptr_hi
		
        lda rsrc_total_bytes_lo		
        sta disk_copy_count_lo
        lda rsrc_total_bytes_hi
        sta disk_copy_count_hi
		
        jsr disk_stream_copy

		// ------------------------------------------------------------
		// Copy resource pointer to rsrc_hdr_ptr
		// ------------------------------------------------------------
        lda rsrc_data_ptr_lo
        sta rsrc_hdr_ptr_lo
        lda rsrc_data_ptr_hi
        sta rsrc_hdr_ptr_hi

		// ------------------------------------------------------------
		// Compute payload byte count (exclude header bytes):
		//
		// 		rsrc_payload_len = rsrc_total_bytes - RSRC_DHDR_BYTES
		// ------------------------------------------------------------
        sec
        lda rsrc_total_bytes_lo
        sbc #RSRC_DHDR_BYTES
        sta rsrc_payload_len_lo
		
        lda rsrc_total_bytes_hi
        sbc #$00
        sta rsrc_payload_len_hi

		// ------------------------------------------------------------
		// Point rsrc_payload_ptr at the payload start (past the header)
		// rsrc_payload_ptr = rsrc_data_ptr + RSRC_DHDR_BYTES
		// ------------------------------------------------------------
        clc
        lda rsrc_data_ptr_lo
        adc #RSRC_DHDR_BYTES
        sta rsrc_payload_ptr_lo
		
        lda rsrc_data_ptr_hi
        adc #$00
        sta rsrc_payload_ptr_hi

		// ------------------------------------------------------------
		// Load the expected checksum from the header
		// ------------------------------------------------------------
        ldy #RSRC_DHDR_OFF_CHECKSUM
        lda (rsrc_hdr_ptr_lo),Y
        sta rsrc_chk_expected

		// Initialize running checksum to 0 (XOR accumulator)
        lda #$00
        sta rsrc_chk_accum
rsrc_chk_step:
        ldy #$00                // always read current byte at rsrc_payload_ptr + 0
		// ------------------------------------------------------------
		// XOR current payload byte into the running checksum:
		// checksum := checksum ⊕ *rsrc_payload_ptr
		// ------------------------------------------------------------
        lda rsrc_chk_accum
        eor (rsrc_payload_ptr_lo),Y
        sta rsrc_chk_accum

        // Advance rsrc_payload_ptr by 1
        inc rsrc_payload_ptr_lo
        bne dec_rem_count        		
        inc rsrc_payload_ptr_hi        

dec_rem_count:
		// ------------------------------------------------------------
		// Decrement remaining byte count
		// ------------------------------------------------------------
        lda rsrc_payload_len_lo
        bne dec_rem_lo
        dec rsrc_payload_len_hi
dec_rem_lo:
        dec rsrc_payload_len_lo

		// ------------------------------------------------------------
		// Loop if any bytes remain
		// ------------------------------------------------------------
        lda rsrc_payload_len_lo
        ora rsrc_payload_len_hi
        bne rsrc_chk_step       // Z=0 → continue; Z=1 → done

		// All bytes consumed - verify computed checksum against expected value.
        lda rsrc_chk_accum
        cmp rsrc_chk_expected

		// ------------------------------------------------------------
		// Match? → proceed to write the resource header metadata
		// 	by skipping to rsrc_hdr_init section
		// ------------------------------------------------------------
        beq rsrc_hdr_init

		// ------------------------------------------------------------
		// Mismatch - attempt a retry
		//
		// rsrc_retry_rem := rsrc_retry_rem - 1
		// if rsrc_retry_rem > 0 → retry_copy the room data and re-check
		// ------------------------------------------------------------
        dec rsrc_retry_rem
		
		// Retries left? If so, retry the payload copy
        bne retry_copy	          

        // ------------------------------------------------------------
        // No retries left - reset retry counter to 1
		//
        // Show disk error message and wait for user to acknowledge
        // ------------------------------------------------------------
        lda #$01
        sta rsrc_retry_rem

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
	Initialize the in-memory resource header with metadata:	size (lo/hi), type, and index. 
	Returns the resource pointer in X:Y, then resumes the game.

Arguments (from previous section):
	rsrc_hdr_ptr			Base address for header
	rsrc_raw_size         	16-bit raw size in bytes.
	rsrc_resource_type    	Resource type identifier.
	rsrc_resource_index   	Resource index within the type.
	rsrc_data_ptr	        16-bit pointer to start of payload.

Returns:
	X:Y						rsrc_data_ptr (lo in X, hi in Y).

Description:
	Writes header layout at (rsrc_hdr_ptr):
	 +0..+1  size  (little-endian)
	 +2      type  
	 +3      index 
	Loads X:=<rsrc_data_ptr, Y:=>rsrc_data_ptr and calls unpause_game before RTS.
================================================================================
*/
* = $5604
rsrc_hdr_init:
		// Store raw data size
        ldy #$00
        lda rsrc_raw_size_lo
        sta (rsrc_hdr_ptr_lo),Y
		
        iny
        lda rsrc_raw_size_hi
        sta (rsrc_hdr_ptr_lo),Y
		
		// Store resource type 
        iny
        lda rsrc_resource_type		
        sta (rsrc_hdr_ptr_lo),Y
		
		// Store resource index
        iny
        lda rsrc_resource_index		
        sta (rsrc_hdr_ptr_lo),Y
		
		// Return the resource data pointer via .X and .Y
        ldx rsrc_data_ptr_lo
        ldy rsrc_data_ptr_hi
		
		// Unpause game
        jsr unpause_game
        rts
/*
================================================================================
  rsrc_update_ptr
================================================================================
Summary:
	Publish an updated pointer to a resource (after memory relocation).
	
	Dispatches on resource type and writes the updated resource pointer
	to the resource table, then returns.

Arguments:
	X = resource_index  Table index to update
	rsrc_ptr            Zero-page pointer to the resource base
	Y = resource_type   One of:
							 RSRC_TYPE_OBJECT, RSRC_TYPE_COSTUME,
							 RSRC_TYPE_ROOM, RSRC_TYPE_ROOM_LAYERS,
							 RSRC_TYPE_SCRIPT, RSRC_TYPE_SOUND

Description:
	- Compare Y against known RSRC_TYPE_* codes in ascending checks.
	- On match, store updated rsrc_ptr to the resource *_tbl[X], then exit.
	- On mismatch, fall through to the next type check.
================================================================================
*/
* = $5A89
rsrc_update_ptr:
        // Dispatch by resource_type in Y
        cpy #RSRC_TYPE_ROOM                      
        bne test_type_room_layers

        // Type Room - publish pointer for room X
        lda rsrc_ptr_lo             
        sta room_ptr_lo_tbl,x
        lda rsrc_ptr_hi             
        sta room_ptr_hi_tbl,x
        rts

test_type_room_layers:
        cpy #RSRC_TYPE_ROOM_LAYERS                      
        bne test_type_costume

        // Type Room GFX layers - publish pointer for layer set X
        lda rsrc_ptr_lo
        sta room_gfx_layers_lo,x
        lda rsrc_ptr_hi
        sta room_gfx_layers_hi,x
        rts

test_type_costume:
        cpy #RSRC_TYPE_COSTUME                      
        bne test_type_object

        // Type Costume - publish pointer for costume X
        lda rsrc_ptr_lo
        sta costume_ptr_lo_tbl,x
        lda rsrc_ptr_hi
        sta costume_ptr_hi_tbl,x
        rts

test_type_object:
        cpy #RSRC_TYPE_OBJECT                      
        bne test_type_script

        // Type Object - publish pointer for object X
        lda rsrc_ptr_lo
        sta object_ptr_lo_tbl,x
        lda rsrc_ptr_hi
        sta object_ptr_hi_tbl,x
        rts

test_type_script:
        cpy #RSRC_TYPE_SCRIPT                      
        bne test_type_sound

        // Type Script - publish pointer for script X
        lda rsrc_ptr_lo
        sta script_ptr_lo_tbl,x
        lda rsrc_ptr_hi
        sta script_ptr_hi_tbl,x
        rts

test_type_sound:
        cpy #RSRC_TYPE_SOUND                      
        bne rsrc_evict_one_by_priority // BUG - it shouldn't fall through but fail instead

        // Type Sound - publish pointer for sound X
        lda rsrc_ptr_lo
        sta sound_ptr_lo_tbl,x
        lda rsrc_ptr_hi
        sta sound_ptr_hi_tbl,x
        rts
/*
================================================================================
  rsrc_evict_one_by_priority
================================================================================
Summary:
	Attempts to evict at least one resource per call, using a rotating
	priority: room → costume → sound → script. Starts from evict_pri_cur,
	tries that class’ eviction routine, then advances priority (with wrap).
	If any callee frees something, returns true; otherwise cycles priorities
	until it wraps back to the starting priority and returns false.

Reads:
	evict_pri_cur              Current priority selector (0..3).

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
		0 → rsrc_release_one_evictable_room
		1 → rsrc_release_one_evictable_costume
		2 → rsrc_release_evictable_sounds
		3 → rsrc_release_evictable_scripts
	If evict_pri_cur ≥ PRIORITY_WRAP_AT, set it to $FF so INC → $00.
	3) INC evict_pri_cur; if rsrc_released_flag ≠ 0 → return True.
	4) If evict_pri_cur != evict_pri_start → continue dispatch loop.
	Else return False (no release this cycle).
================================================================================
*/
* = $5AE3
rsrc_evict_one_by_priority:
        // Initialize the resource released flag to False
        lda #FALSE
        sta rsrc_released_flag

        // Snapshot the starting eviction priority so we can detect a full wrap later.
        lda evict_pri_cur
        sta evict_pri_start

dispatch_by_priority:
        // Dispatch on evict_pri_cur (0..3 are valid, >=4 wraps to 0).
        lda evict_pri_cur
        cmp #PRIORITY_ROOM
        bne test_costume

        // case 0 → try releasing one room using LRU
        jsr rsrc_release_one_evictable_room
        jmp advance_priority

test_costume:
        cmp #PRIORITY_COSTUME
        bne test_sound

        // case 1 → try releasing one costume
        jsr rsrc_release_one_evictable_costume
        jmp advance_priority

test_sound:
        cmp #PRIORITY_SOUND
        bne test_script

        // case 2 → try releasing evictable sounds
        jsr rsrc_release_evictable_sounds
        jmp advance_priority

test_script:
        cmp #PRIORITY_SCRIPT
        bne test_wrap_threshold

        // case 3 → try releasing evictable scripts
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

        // Did any callee release something? 
        lda rsrc_released_flag
        beq wrap_check_no_release

        // Yes → return True.
        lda #BTRUE
        rts

wrap_check_no_release:
        // No release yet. Have we wrapped back to the starting priority?
        // If not, continue dispatch loop.
        lda evict_pri_cur
        cmp evict_pri_start
        bne dispatch_by_priority

        // Completed a full cycle with no releases → return False
        lda #FALSE
        rts
/*
================================================================================
 rsrc_release_one_evictable_room
================================================================================
Summary:
	Release the Least Recently Used evictable room, if possible.
	
	Scans all room slots from the max index down and selects the
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
	- Track the best candidate using the largest age value (strictly greater).
	- If a candidate exists:
	   Y := ptr.hi, X := ptr.lo; clear table refs & attrs; call mem_release.
================================================================================
*/
* = $5B38
rsrc_release_one_evictable_room:
		// Init oldest LRU “staleness” score (0 ⇒ no candidate yet)
        lda #$00                        
        sta oldest_age                   
		// Init oldest room index (0 used as “no candidate” sentinel)
        sta oldest_room_idx               
		
		// Start scan at highest room index; loop will go down to 1 (index 0 skipped)
        ldx #ROOM_MAX_INDEX             	
scan_room:
		// Room resident? If not, skip
        lda room_ptr_hi_tbl,x
        beq advance_room

		// Room locked? If so, skip
        lda room_mem_attrs,x              
        bmi advance_room                  // locked (bit7=1) → ineligible

        // Room has nonzero age? If not, skip
        cmp #$00
        beq advance_room

        // LRU selection - prefer strictly larger age (older/staler):
        // After CMP oldest_age:
        //   room_age < oldest_age → C=0  ⇒ skip
        //   room_age = oldest_age → Z=1  ⇒ skip
        //   room_age > oldest_age → C=1,Z=0 → falls through (new candidate)
        cmp oldest_age
        bcc advance_room
        beq advance_room

        // Promote this room as the current oldest LRU candidate:
        // save its age (A) and its index (X).
        sta oldest_age
        stx oldest_room_idx

		// Loop until we exhaust all rooms
advance_room:
        dex                               
        bne scan_room                     
                                          
        // Selection phase
		// If no candidate was found, oldest_room_idx remains 0 → nothing to evict.
        lda oldest_room_idx
        bne evict_room_candidate          // have a candidate → proceed to eviction
        rts                               // none found → return without changes

evict_room_candidate:
		// Fetch oldest room index
        ldx oldest_room_idx                 

        // Fetch room resource pointer and prepare mem_release arguments 
		// mem_release expects X=ptr.lo, Y=ptr.hi
        lda room_ptr_hi_tbl,x
        tay                               
        lda room_ptr_lo_tbl,x
        pha                               // save lo

        // Clear table references to avoid dangling/stale pointers elsewhere
        lda #$00
        sta room_ptr_hi_tbl,x             
        sta room_ptr_lo_tbl,x             

        // Clear room age
        lda #$00
        sta room_mem_attrs,x

        // Recover saved ptr.lo and place it in X for mem_release
        pla                               
        tax                               

        // Free the room resource block
        jsr mem_release
        rts                               
/*
================================================================================
  rsrc_release_one_evictable_costume
================================================================================
Summary:
	Find one evictable costume. If found, release it and return.
	
	Scans all costumes from COSTUME_MAX_INDEX down and frees the first
	costume that is both loaded and not in the current room, provided
	its attributes mark it evictable (attrs == 0). Exits immediately
	after freeing one costume.

Reads:
	costume_ptr_lo_tbl[X]     	Costume resource pointer (lo).
	costume_ptr_hi_tbl[X]     	Costume resource pointer (hi); nonzero ⇒ loaded.
	costume_mem_attrs[X]      	Per-costume attributes; 0 ⇒ evictable.
	room_for_costume[X]     	Owning room index for costume X.
	current_room              	Index of the current room (active/in use).

Updates:
	costume_ptr_*_tbl[X]      	Cleared for the freed costume to avoid dangling refs.
	costume_mem_attrs[X]      	Cleared for the freed costume.

Description:
	- Iterate X := COSTUME_MAX_INDEX … 0.
	- Skip if not loaded (ptr.hi == 0) or if in current_room (in use).
	- Skip if attrs != 0 (not evictable); attrs == 0 ⇒ candidate.
	- Preserve loop index on stack; prepare mem_release calling convention:
	   X := ptr.lo, Y := ptr.hi.
	- Clear table pointers and attributes before freeing to prevent stale uses.
	- Call mem_release and return.
================================================================================
*/
* = $5B84
rsrc_release_one_evictable_costume:
		// Start from highest costume index; iterate X downward
        ldx #COSTUME_MAX_INDEX        

scan_costume:
        // Costume resident? If not, skip
        lda costume_ptr_hi_tbl,x
        beq advance_costume

        // Costume present in the current room? If so, skip
        lda room_for_costume,x
        cmp current_room
        beq advance_costume

        // Costume in use (by refcount/locking)? If so, skip
        lda costume_mem_attrs,x
        bne advance_costume

        // Set up mem_release by copying the costume pointer to X/Y
        lda costume_ptr_hi_tbl,x
        tay                               
        lda costume_ptr_lo_tbl,x
		// Save costume index 
        pha                               

        // Clear table references *before* freeing to avoid dangling/stale pointers.
        // Mark "not resident" by zeroing both bytes.
        lda #$00
        sta costume_ptr_lo_tbl,x          
        sta costume_ptr_hi_tbl,x          
		
		// Clear refcount
		sta costume_mem_attrs,X
		
        // Restore costume lo into .X
		// mem_release expects X=lo, Y=hi
        pla                               
        tax                               

        // Free the block at (X=lo, Y=hi)
        jsr mem_release
        rts                               

		// Continue to next costume or exit
advance_costume:
        dex                               
        bne scan_costume                  
        rts
/*
================================================================================
  rsrc_release_evictable_sounds
================================================================================
Summary:
	Release _all_ evictable sounds.
	
	Scans sound slots from SOUND_MAX_INDEX down to 0. For each slot,
	if its attributes indicate “evictable” (attrs == 0) and it is
	resident (pointer hi-byte != 0), clears the table entry and calls
	mem_release to free the block. Protected sounds are skipped.

Reads:
	sound_attr_tbl[X]         	Per-sound attributes; 0 ⇒ evictable.
	sound_ptr_lo_tbl[X]        	Sound resource pointer (lo).
	sound_ptr_hi_tbl[X]        	Sound resource pointer (hi); nonzero ⇒ loaded.

Updates:
	sound_ptr_*_tbl[X]         	Cleared to zero for each released sound.

Description:
	- Iterate X := SOUND_MAX_INDEX … 0.
	- Skip if attrs != 0 (not evictable) or ptr.hi == 0 (not loaded).
	- Skip protected sounds PROTECTED_SOUND_3, _4, _5.
	- Preserve X on stack; prepare mem_release calling convention:
	   X := ptr.lo, Y := ptr.hi.
	- Clear table pointers before freeing to avoid dangling references.
	- Call mem_release; restore X (loop index) and continue.
================================================================================
*/
* = $5BB5
rsrc_release_evictable_sounds:
		// Start from the highest sound index; iterate X downward
        ldx #SOUND_MAX_INDEX          

scan_sound:
        // Sound in use (by refcount/locking)? If so, skip
        lda sound_attr_tbl,x
        bne advance_sound              

        // Sound resident? If not, skip
        lda sound_ptr_hi_tbl,x
        beq advance_sound              

		// Sound is protected? If so, skip
        cpx #PROTECTED_SOUND_3        
        beq advance_sound           
        cpx #PROTECTED_SOUND_4        
        beq advance_sound
        cpx #PROTECTED_SOUND_5        
        beq advance_sound

        // Set up mem_release by copying the sound pointer to X/Y
        txa                               
        pha                               
        lda sound_ptr_hi_tbl,x
        tay                               
        lda sound_ptr_lo_tbl,x
        pha                               // Save lo ptr

        // Clear table references *before* freeing to avoid dangling/stale pointers.
        // Mark "not resident" by zeroing both bytes.
        lda #$00
        sta sound_ptr_lo_tbl,x            
        sta sound_ptr_hi_tbl,x            

        // Restore sound lo into .X
		// mem_release expects X=lo, Y=hi
        pla                               
        tax                               

        // Free the block at (X=lo, Y=hi)
        jsr mem_release

        // Recover the loop index and continue scanning candidates.
        pla                               
        tax                               

		// Continue to next sound or exit
advance_sound:
        dex                       
        bne scan_sound            
        rts
/*
================================================================================
  rsrc_release_evictable_scripts
================================================================================
Summary:
	Release _all_ evictable scripts.
	
	Scans all script slots from highest index down. For each script,
	if its memory attributes indicate “evictable” (attrs==0) and it is
	loaded (pointer hi-byte != 0), clears the table entry and calls
	mem_release to free the block.

Reads:
	script_mem_attrs[X]  	Per-script memory attributes; 0 ⇒ evictable.
	script_ptr_lo_tbl[X]    Script resource pointer (lo).
	script_ptr_hi_tbl[X]    Script resource pointer (hi); nonzero ⇒ loaded.

Updates:
	script_ptr_*_tbl[X]     Cleared to zero for each released script.

Description:
	- Iterate X := SCRIPT_MAX_INDEX .. 0.
	- Skip if attrs != 0 (not evictable) or pointer hi == 0 (not loaded).
	- Preserve X (index) on stack; form (X=ptr.lo, Y=ptr.hi) for mem_release.
	- Clear table pointers before freeing to avoid dangling references.
	- Restore index and continue scanning until all candidates processed.
================================================================================
*/
* = $5BEA
rsrc_release_evictable_scripts:
		// Start from highest script index; iterate X downward
        ldx #SCRIPT_MAX_INDEX        

scan_script:
        // Script in use (by refcount/locking)? If so, skip
        lda script_mem_attrs,x
        bne advance_script           

        // Script resident? If not, skip
        lda script_ptr_hi_tbl,x
        beq advance_script           

        // Set up mem_release by copying the script pointer to X/Y
        txa                               
        pha                               // save index on stack
        lda script_ptr_hi_tbl,x
        tay                               
        lda script_ptr_lo_tbl,x
        pha                               // save lo on stack

        // Clear table references *before* freeing to avoid dangling/stale pointers.
        // Mark "not resident" by zeroing both bytes.
        lda #$00
        sta script_ptr_lo_tbl,x           
        sta script_ptr_hi_tbl,x           

        // Restore lo into X for mem_release (expects X=lo, Y=hi).
        pla                               
        tax                               

        // Free the block at (X=lo, Y=hi)
        jsr mem_release

        // Restore loop index and continue scan
        pla                               
        tax                               

		// Continue to next script or exit
advance_script:
        dex                               
        bne scan_script                   
        rts

/* 
Pseudo-code		

function rsrc_cache_costume(costumeIndex):
    // Fast path: already loaded?
    if costume_ptr[costumeIndex] != NULL:
        return

    // Miss: remember index for later table writes
    costume_idx_saved = costumeIndex
    rsrc_resource_index = costumeIndex

    // Resolve owning room and prepare disk chain
    roomIndex = costume_room_res_idx[costumeIndex]
    room_disk_chain_prepare(roomIndex)

    // Lookup disk location: 2-byte entry [offset, sectorIndex]
    (offsetWithinStream, sectorIndex) = costume_disk_loc_tbl[costumeIndex]
    rsrc_read_offset = offsetWithinStream
    rsrc_sector_idx  = sectorIndex

    // Identify resource type and perform load
    rsrc_resource_type = RSRC_TYPE_COSTUME
    ptr = rsrc_load_from_disk()        // returns base pointer to resource in RAM

    // Publish pointer into costume pointer tables
    costume_ptr[costume_idx_saved] = ptr

function rsrc_cache_sound(soundIndex, callerY):
    // Save caller's Y-equivalent if needed by caller
    saved_y = callerY

    rsrc_resource_index = soundIndex

    // Fast path: already resident?
    if sound_ptr[soundIndex] != NULL:
        // Restore Y and return
        callerY = saved_y
        return

    // Miss: remember index for later table writes
    sound_idx_saved = soundIndex

    // Resolve owning room and prepare disk chain
    roomIndex = sound_room_idx_tbl[soundIndex]
    room_disk_chain_prepare(roomIndex)

    // Lookup disk location: 2-byte entry [offset, sectorIndex]
    (offsetWithinStream, sectorIndex) = sound_disk_loc_tbl[soundIndex]
    rsrc_read_offset = offsetWithinStream
    rsrc_sector_idx  = sectorIndex

    // Identify resource class and perform the load
    rsrc_resource_type = RSRC_TYPE_SOUND
    ptr = rsrc_load_from_disk()

    // Publish the returned pointer into the sound tables
    sound_ptr[sound_idx_saved] = ptr

    // Restore caller Y-equivalent and return
    callerY = saved_y

// scriptIndexWithScopeBit: bit7=1 → room script, bit7=0 → global script
function rsrc_cache_script(scriptIndexWithScopeBit, callerY):
    saved_y = callerY

    rsrc_resource_index = scriptIndexWithScopeBit
    script_idx_saved    = scriptIndexWithScopeBit

    // Fast path: already resident?
    if script_ptr[scriptIndexWithScopeBit] != NULL:
        callerY = saved_y
        return

    // Determine owning room
    roomIndex = script_room_idx_tbl[scriptIndexWithScopeBit]
    room_disk_chain_prepare(roomIndex)

    // Decode scope and lookup disk location
    indexWithoutScopeBit = scriptIndexWithScopeBit & 0x7F
    isRoomScoped = (scriptIndexWithScopeBit & 0x80) != 0

    if isRoomScoped:
        (offsetWithinStream, sectorIndex) = room_script_disk_loc_tbl[indexWithoutScopeBit]
    else:
        (offsetWithinStream, sectorIndex) = global_script_disk_loc_tbl[indexWithoutScopeBit]

    rsrc_read_offset = offsetWithinStream
    rsrc_sector_idx  = sectorIndex

    // Identify resource class and perform load
    rsrc_resource_type = RSRC_TYPE_SCRIPT
    ptr = rsrc_load_from_disk()

    // Publish the returned pointer into the script tables
    script_ptr[rsrc_resource_index] = ptr

    callerY = saved_y

function room_disk_chain_prepare(roomIndex):
    // Check if this room is on the currently active disk side
    requiredSide = room_disk_side_tbl[roomIndex]
    if requiredSide != active_side_id:
        // Switch disk sides if needed (may prompt / wait)
        saved_room_idx = roomIndex
        disk_ensure_side(requiredSide)
        roomIndex = saved_room_idx

    // Resolve disk location: each room has [SECTOR, TRACK]
    entryIndex = roomIndex * 2     // 2 bytes per entry
    sector = room_sec_trk_tbl[entryIndex + ROOM_ST_OFF_SECTOR]
    track  = room_sec_trk_tbl[entryIndex + ROOM_ST_OFF_TRACK]

    chain_sector = sector
    chain_track  = track

    // Initialize chain for further sequential reads
    disk_init_chain(sector, track)

function rsrc_load_from_disk() -> pointer:
    pause_game()

    while true:
        // 1) Read header into scratch buffer
        disk_seek_read(offset = rsrc_read_offset,
                       sectorIndex = rsrc_sector_idx)

        disk_stream_copy(
            dest  = rsrc_hdr_scratch,      // 4-byte buffer
            count = RSRC_DHDR_BYTES
        )

        // Interpret on-disk header
        totalBytes   = read16(rsrc_hdr_scratch + RSRC_DHDR_OFF_SIZE_LO)
        sentinel     = read8 (rsrc_hdr_scratch + RSRC_DHDR_OFF_SENTINEL)
        expectedChk  = read8 (rsrc_hdr_scratch + RSRC_DHDR_OFF_CHECKSUM)

        // Sentinel must be 0, otherwise check disk ID and retry header read
        if sentinel != 0:
            disk_id_check()                // verify correct disk/ID
            continue                       // restart header read

        // 2) Prepare sizes
        rsrc_total_bytes = totalBytes              // header + payload
        rsrc_raw_size    = totalBytes - RSRC_DHDR_BYTES

        // 3) Allocate a block large enough for header + payload
        ptr = mem_alloc(size = rsrc_total_bytes)
        rsrc_data_ptr = ptr
        rsrc_hdr_ptr  = ptr       // header lives at start of block

        // We allow one “silent” retry before we show the UI error message
        rsrc_retry_rem = 1

        // 4) Copy full resource (header + payload) from disk into RAM
        //    This block is factored in assembly as copy_payload_from_disk
        disk_seek_read(offset = rsrc_read_offset,
                       sectorIndex = rsrc_sector_idx)

        disk_stream_copy(
            dest  = rsrc_data_ptr,
            count = rsrc_total_bytes
        )

        // 5) Set up checksum inputs
        payloadLen = rsrc_total_bytes - RSRC_DHDR_BYTES
        payloadPtr = rsrc_data_ptr + RSRC_DHDR_BYTES

        rsrc_payload_len  = payloadLen
        rsrc_payload_ptr  = payloadPtr
        rsrc_chk_expected = expectedChk
        rsrc_chk_accum    = 0

        // 6) Checksum loop (rsrc_chk_step in assembly)
        while rsrc_payload_len > 0:
            rsrc_chk_accum ^= *rsrc_payload_ptr
            rsrc_payload_ptr += 1
            rsrc_payload_len -= 1

        // 7) Compare checksum
        if rsrc_chk_accum == rsrc_chk_expected:
            // Success: finalize header and return pointer
            return rsrc_hdr_init()

        // 8) Checksum mismatch: handle retry / user prompt
        rsrc_retry_rem -= 1

        if rsrc_retry_rem >= 0:
            // At least one retry remaining before we show UI
            // (in practice the assembly sets retry_rem to 1 once
            //  and then falls through to the UI path afterwards,
            //  but high-level behavior is: “keep retrying until OK”)
            // retry: loop top
            continue

        // No retries left: prompt user, then retry again
        rsrc_retry_rem = 1
        print_msg_ptr = DISK_ERROR_MSG
        print_message_wait_for_button()
        // After user acknowledges, loop back and retry

function rsrc_hdr_init() -> pointer:
    // Write in-memory header at base of allocated block
    header = rsrc_hdr_ptr  // same as rsrc_data_ptr

    header.size  = rsrc_raw_size        // payload size only
    header.type  = rsrc_resource_type
    header.index = rsrc_resource_index

    // Return start-of-payload pointer to caller
    resultPtr = rsrc_data_ptr

    unpause_game()
    return resultPtr

// Update pointer tables after compaction/relocation.
// Inputs are conceptually:
//   resourceIndex   = X
//   resourceType    = Y
//   newPtr          = rsrc_ptr (global)
function rsrc_update_ptr(resourceIndex, resourceType, newPtr):
    switch resourceType:
        case RSRC_TYPE_ROOM:
            room_ptr[resourceIndex] = newPtr
            return

        case RSRC_TYPE_ROOM_LAYERS:
            room_layers_ptr[resourceIndex] = newPtr
            return

        case RSRC_TYPE_COSTUME:
            costume_ptr[resourceIndex] = newPtr
            return

        case RSRC_TYPE_OBJECT:
            object_ptr[resourceIndex] = newPtr
            return

        case RSRC_TYPE_SCRIPT:
            script_ptr[resourceIndex] = newPtr
            return

        case RSRC_TYPE_SOUND:
            sound_ptr[resourceIndex] = newPtr
            return

        default:
            // BUG in current assembly: falls into eviction routine instead
            // Correct high-level behavior should be “ignore” or “signal error”.
            return

// Try to free at least one resource according to a rotating priority:
//   0 = rooms, 1 = costumes, 2 = sounds, 3 = scripts
// Returns true if anything was released, false otherwise.
function rsrc_evict_one_by_priority() -> bool:
    rsrc_released_flag = false

    startPriority = evict_pri_cur

    while true:
        // Normalize priority if caller left it at or past wrap sentinel
        if evict_pri_cur >= PRIORITY_WRAP_AT:
            evict_pri_cur = 0

        switch evict_pri_cur:
            case PRIORITY_ROOM:
                rsrc_release_one_evictable_room()
            case PRIORITY_COSTUME:
                rsrc_release_one_evictable_costume()
            case PRIORITY_SOUND:
                rsrc_release_evictable_sounds()
            case PRIORITY_SCRIPT:
                rsrc_release_evictable_scripts()
            default:
                // Should not occur; treat as no-op
                break

        // Bump priority for next attempt
        evict_pri_cur += 1

        if rsrc_released_flag:
            return true

        // If we have come full circle without freeing anything, give up
        if evict_pri_cur == startPriority:
            return false

        // Otherwise, try next priority class

// Try to evict exactly one room, picking the “oldest” eligible room.
// Uses room_mem_attrs as an age/lock field:
//   - bit7 set   ⇒ locked, never evict
//   - 0 value    ⇒ not evictable
//   - larger val ⇒ older/staler candidate
function rsrc_release_one_evictable_room():
    bestRoomIndex = 0
    bestAge       = 0

    // Scan from highest to lowest index; index 0 is ignored
    for roomIndex from ROOM_MAX_INDEX down to 1:
        ptr = room_ptr[roomIndex]
        attrs = room_mem_attrs[roomIndex]

        // Not resident or not eligible
        if ptr == NULL:
            continue
        if (attrs & 0x80) != 0:      // locked
            continue
        if attrs == 0:               // age = 0 → not evictable
            continue

        // Pick the room with the largest age value
        if attrs > bestAge:
            bestAge       = attrs
            bestRoomIndex = roomIndex

    if bestRoomIndex == 0:
        // No candidate found
        return

    // Evict chosen room
    ptr = room_ptr[bestRoomIndex]
    room_ptr[bestRoomIndex]      = NULL
    room_mem_attrs[bestRoomIndex] = 0

    mem_release(ptr)
    // mem_release should set rsrc_released_flag = true

// Try to evict exactly one costume. A costume is evictable if:
//   - It is resident
//   - It does NOT belong to the current room
//   - Its costume_mem_attrs value is 0 (no locks, no references)
function rsrc_release_one_evictable_costume():
    // Scan from highest to lowest index
    for costumeIndex from COSTUME_MAX_INDEX down to 0:
        ptr   = costume_ptr[costumeIndex]
        attrs = costume_mem_attrs[costumeIndex]

        if ptr == NULL:
            continue

        // Never evict costume belonging to the current room
        if room_for_costume[costumeIndex] == current_room:
            continue

        // Only evict costumes with attrs == 0
        if attrs != 0:
            continue

        // Found a candidate: release it and stop
        costume_ptr[costumeIndex]      = NULL
        costume_mem_attrs[costumeIndex] = 0

        mem_release(ptr)
        // mem_release should set rsrc_released_flag = true
        return

    // No evictable costume found; do nothing

// Try to evict all sounds that are:
//   - Resident
//   - Have sound_attr_tbl[index] == 0 (no locks/references)
//   - Are NOT in the “protected” set (PROTECTED_SOUND_3/4/5)
function rsrc_release_evictable_sounds():
    for soundIndex from SOUND_MAX_INDEX down to 0:
        attrs = sound_attr_tbl[soundIndex]
        ptr   = sound_ptr[soundIndex]

        if attrs != 0:
            continue           // in use / locked
        if ptr == NULL:
            continue           // not resident

        if soundIndex == PROTECTED_SOUND_3 or
           soundIndex == PROTECTED_SOUND_4 or
           soundIndex == PROTECTED_SOUND_5:
            continue           // skip protected sounds

        // Evict this sound
        sound_ptr[soundIndex] = NULL
        mem_release(ptr)
        // mem_release should set rsrc_released_flag = true
        // Note: routine continues scanning and may free several sounds

// Try to evict all scripts that are:
//   - Resident
//   - Have script_mem_attrs[index] == 0 (no locks/references)
function rsrc_release_evictable_scripts():
    for scriptIndex from SCRIPT_MAX_INDEX down to 0:
        attrs = script_mem_attrs[scriptIndex]
        ptr   = script_ptr[scriptIndex]

        if attrs != 0:
            continue           // in use / locked
        if ptr == NULL:
            continue           // not resident

        // Evict this script
        script_ptr[scriptIndex] = NULL
        mem_release(ptr)
        // mem_release should set rsrc_released_flag = true
        // Routine continues scanning and may free multiple scripts
*/		