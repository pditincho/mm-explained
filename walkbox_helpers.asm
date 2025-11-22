/*
================================================================================
  Walkbox table access helpers
================================================================================

Overview
	Centralized helpers for locating and indexing a room’s walkbox table.
	
	Higher-level pathing code uses this module to:
		- Verify that a room resource (and its walkboxes) is resident in memory.
		- Compute an absolute pointer to the room’s walkbox table
		- Derive the byte offset within the walkbox table for a given box index.

Responsibilities
	- Track the “current” walkbox room via walkbox_room and expose a shared
	pointer (box_ptr) into its walkbox table.
	
	- Provide room- and costume-based entry points to resolve box_ptr:
		• get_walkboxes_for_room:   uses walkbox_room directly.
		• get_walkboxes_for_costume: follows costume_room_idx[active_costume].
		
	- Encapsulate the room-header layout detail that the walkbox table is
	stored at (room_base + OFS_WALKBOX).
	
	- Compute per-box byte offsets under the invariant that each walkbox
	record has a fixed stride of 5 bytes (WALKBOX_STRIDE).

Routines
	- get_walkboxes_for_room
		Given walkbox_room, checks if the room resource	is resident. 
		If not, returns WALKBOX_NOT_RESIDENT. On success, tail-calls 
		get_walkboxes_ptr to seed box_ptr.

	- get_walkboxes_for_costume
		Maps active_costume → room, then performs the same residency check 
		as get_walkboxes_for_room. 	On success, calls get_walkboxes_ptr to 
		initialize box_ptr for that room; 	on failure,	returns 
		WALKBOX_NOT_RESIDENT.

	- get_walkboxes_ptr
		Core pointer resolver. 
		Treats (room_ptr_lo_tbl[Y], A) as the base address of the room block, 
		reads the 8-bit walkbox offset stored at base+OFS_WALKBOX, 
		adds it to the base, and stores the result into	box_ptr (lo/hi). 
		Returns A=0 on success.

	- get_walkbox_offset
		Computes the byte offset for a given walkbox index into the current
		table: offset = index * WALKBOX_STRIDE. Implemented as
		(index*4 + index) using 8-bit arithmetic and returns the low byte in Y,
		which is used as an index when addressing through box_ptr.

How it is used
	- Pathfinding and walkbox geometry routines first call one of the
	“get_walkboxes_*” entry points to ensure the underlying room is
	resident and to populate box_ptr.
	- Once box_ptr is valid, callers use get_walkbox_offset to seek to a
	specific walkbox record (index * 5) and then perform AABB tests,
	adjacency scans, or path building against those records.
================================================================================
*/

#importonce
#import "globals.inc"
#import "constants.inc"

// -----------------------------------------------------------------------------
// Walkbox data pointers and indices
// -----------------------------------------------------------------------------
.label box_ptr                   = $17  // ZP: walkbox table base (lo at $17, hi at $18)
.label wbox_idx                  = $FC3F  // Scratch: original walkbox index under test
.label walkbox_room              = $FE74  // Active room id for walkbox tables

// -----------------------------------------------------------------------------
// Walkbox table layout and markers
// -----------------------------------------------------------------------------
.const OFS_WALKBOX                     = $15    // Room header byte: 8-bit ofs to walkboxes
.const WALKBOX_STRIDE                  = $05    // Bytes per walkbox record
.const WALKBOX_SENTINEL                = $FF    // End-of-table marker in walkbox lists
.const WALKBOX_NOT_RESIDENT            = $FF    // Flag: walkbox data not in memory

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


function get_walkbox_offset(index) -> int:
    // Each walkbox record occupies a fixed stride of 5 units in some table.
    // Return the starting offset for walkbox 'index'.
    return index * 5
*/