/*
================================================================================
  Room Loader / Scene Switcher
================================================================================

Overview
	Orchestrates the full lifecycle of a room (scene) in memory, from
	disk-backed resource loading to per-frame-ready in-RAM structures.
	It bridges the low-level resource manager and the higher-level script,
	actor, and rendering systems.

Responsibilities:
	- Room switching:
		- Tear down the current room (UI, actors, sprites, EXIT script).
		- Switch current_room to a new index or to “no room”.
		- Bring up the new room (ENTRY script, sounds, scripts, costumes).
	- Residency and caching:
		- Ensure a room resource is resident in RAM on demand.
		- Maintain room pointers (room_ptr_*_tbl) and a combined lock/age
		attribute (room_liveness_tbl) for LRU-style management.
		- Age the previous room, mark the active room as most-recently used,
		and increment ages for other resident rooms.

Data layout:
	- Per-room metadata block:
		- Fixed 8-bit fields (width, height, video flag, background colors).
		- Fixed 16-bit offsets to compressed graphics layers (tile matrix,
		color, mask, mask indexes).
		- Object_count, sounds_count, scripts_count, and script entry/exit
		pointers.
	- Per-object tables:
		- N×16-bit table of per-object compressed gfx offsets.
		- N×16-bit table of per-object record offsets.
		- Object records with packed fields for IDs, geometry, parent links,
		destinations, and flags; scattered into per-field arrays.

	Major routines:
	- switch_to_room:
		- High-level entry point for switching scenes; handles teardown of
		current room, calls load_room_gfx_and_objects, then runs ENTRY
		script, loads sounds/scripts, spawns actors for present costumes,
		and requests the open-shutter transition.
	- load_room_gfx_and_objects:
		- Switches current_room to X, ages the old room in
		room_liveness_tbl, releases room-layer resources, handles the
		“no room” sentinel case, and, for a non-zero room, ensures
		residency, reads metadata/objects/graphics, and performs an LRU
		age pass across all resident rooms.
	- cache_room:
		- Fast path skips disk if room_ptr_hi_tbl[X] is non-zero and only
		updates liveness; cold path primes the disk chain, loads the room
		via the resource manager, publishes its base pointer, and then
		sets its age to 1 while preserving the lock bit.
	- load_room_metadata:
		- Uses current_room to locate room_base, skips the 4-byte header,
		copies the 8-bit and 16-bit metadata fields into a local buffer,
		installs room EXIT/ENTRY script pointers, and reads the 4-byte
		symbol dictionaries for tile, color, and mask compressed streams.
	- load_room_objects:
		- Reads object_count from metadata, builds the per-object gfx-offset
		and record-offset tables, then walks each object record to scatter
		IDs, geometry, parent indices, destinations, and destination flags
		into the engine’s attribute arrays.
	- read_room_graphics:
		- Programs VIC background colors for the active room, then decodes
		the tile-definition stream into the tile bitmap page and loads the
		mask-pattern block: size-prefixed, heap-allocated, tagged as a
		ROOM_LAYERS resource, and filled via the shared dict-4 decoder.
	- load_room_sounds_and_scripts:
		- Reads sound/script counts from metadata and walks a single
		subresource index table (sounds first, scripts second) to ensure
		each referenced sound and script resource is resident.
	- get_room_sounds_base_address:
		- Computes the base pointer to the shared subresource index table
		for the active room, located after the two N×2 per-object offset
		tables (gfx offsets and record offsets).
	- spawn_actors_for_room_costumes:
		- Scans all costume slots; for any slot whose actor_for_costume
		sentinel indicates “unassigned” and whose costume_room_idx matches
		current_room, ensures its costume resource is resident and binds a
		free actor slot to it.

Invariants and conventions:
	- Index 0 is used as a “no room” sentinel in the liveness table and
	room switching logic.
	- room_liveness_tbl encodes lock and age in a single byte: bit7 is the
	lock bit, bits0..6 form an age counter (0 = most recent).
	- Per-object offset tables are effectively 1-based: table index 0..1
	are reserved and the first actual object entry starts at pair index 2.
	- Compressed graphic layers share a small dict-4 stream format with a
	4-byte symbol dictionary stored at the beginning of each stream.
================================================================================
*/

#importonce

#import "constants.inc"
#import "globals.inc"
#import "rsrc_mgmt.asm"
#import "decompressor.asm"
#import "room_gfx_rsrc.asm"
#import "sentence_action.asm"
#import "irq_handlers.asm"
#import "init_engine.asm"



// ---------------------------------------------------------
// Room metadata layout (offsets from start of metadata, or from room_base + 4)
// ---------------------------------------------------------
// - 16-bit fields are little-endian: lo at base+ofs, hi at base+ofs+1.
// - All offsets are byte offsets from (room_base + 4), i.e., past the 4-byte header.
.const ROOM_META_WIDTH_OFS          = $00  // room width in tiles
.const ROOM_META_HEIGHT_OFS         = $01  // room height in tiles
.const ROOM_META_VIDEO_FLAG_OFS     = $02  // video/render flag
.const ROOM_META_BG0_OFS            = $03  // background color 0
.const ROOM_META_BG1_OFS            = $04  // background color 1
.const ROOM_META_BG2_OFS            = $05  // background color 2
.const ROOM_META_TILEDEF_OFS     	= $06  // 16-bit offset to tile definitions (compressed), decompress to D800–E000 (~$0800 bytes)
.const ROOM_META_TILEMATRIX_OFS  	= $08  // 16-bit offset to tile matrix (compressed), size = width * height
.const ROOM_META_COLOR_OFS       	= $0A  // 16-bit offset to color layer (compressed), size = width * height
.const ROOM_META_MASK_OFS        	= $0C  // 16-bit offset to mask layer (compressed), size = width * height
.const ROOM_META_MASKIDX_OFS     	= $0E  // 16-bit offset to mask indexes (compressed)
.const ROOM_META_OBJ_COUNT_OFS      = $10   // total number of objects
.const ROOM_META_BBOX_START_OFS     = $11   // offset to boundary boxes start
.const ROOM_META_SOUND_COUNT_OFS    = $12   // total number of sounds
.const ROOM_META_SCRIPT_COUNT_OFS   = $13   // total number of scripts
.const ROOM_EXIT_SCRIPT_OFS         = $14   // +$14..+$15 = exit script offset (16-bit)
.const ROOM_ENTRY_SCRIPT_OFS        = $16   // +$16..+$17 = entry script offset (16-bit)
.const ROOM_META_OBJ_GFX_OFS        = $18   // +$18..+$19 = offsets for objects' compressed gfx layers (16-bit)

/* ROOM_OBJ_ABS_START
 * Absolute byte offset (from room_base) to the start of the per-object
 * compressed-graphics offsets table.
 
 * Defined as: ROOM_META_OBJ_GFX_OFS (relative to room_base+MEM_HDR_LEN) + MEM_HDR_LEN,
 * so typically $18 + $04 = $1C.
 
 */
.const ROOM_OBJ_ABS_START  = ROOM_META_OBJ_GFX_OFS + MEM_HDR_LEN

/* ---------------------------------------------------------
 * Object metadata field offsets (relative to object record base), bytes
 * Notes:
 *   - These offsets are read by load_room_objects when scattering fields.
 *   - Packed fields:
 *       +$07: bit7 = parent_possible, bits0..6 = y_start
 *       +$0B: bits7..5 = prep_index (unused here), bits4..0 = y_destination
 *       +$0C: bits7..3 = height, bits2..0 = destination_active
 * --------------------------------------------------------- */
.const OBJ_META_IDX_LO_OFS       = $04  // object identifier (lo)
.const OBJ_META_IDX_HI_OFS       = $05  // object identifier (hi)
.const OBJ_META_X_START_OFS      = $06  // initial X position
.const OBJ_META_Y_START_OFS      = $07  // packed: bit7=parent_possible, bits0..6=y_start (0..127)
.const OBJ_META_WIDTH_OFS        = $08  // width (game units)
.const OBJ_META_PARENT_IDX_OFS   = $09  // parent object index (if any)
.const OBJ_META_X_DEST_OFS       = $0A  // target X position
.const OBJ_META_Y_DEST_OFS       = $0B  // packed: bits7..5=prep_index (“Use”), bits4..0=y_destination (0..31)
.const OBJ_META_HEIGHT_OFS       = $0C  // packed: bits7..3=height, bits2..0=destination_active (nonzero ⇒ active)
.const OBJ_META_NAME_PTR_OFS     = $0D  // offset to name string
.const OBJ_META_VERB_HDLR_OFS    = $0E  // start of verb-handler offset table


// ---------------------------------------------------------
// Room attribute byte (bitfields): bit7 = lock (active-high), bits0..6 = age.
// Used by LRU/locking logic for resident rooms.
// ---------------------------------------------------------
.const ROOM_ATTR_LOCK           = $80  // bit7 mask: 1 = locked (active-high), 0 = unlocked
.const ROOM_ATTR_AGE_MASK       = $7F  // bits0..6 mask: age counter range 0..$7F (preserves lock bit)

.const ROOM_ATTR_UNLOCKED_AGE_0 = $00  // preset: unlocked, age = 0
.const ROOM_ATTR_UNLOCKED_AGE_1 = $01  // preset: unlocked, age = 1
.const ROOM_ATTR_LOCKED_AGE_1   = $81  // preset: locked,   age = 1 (lock bit + age bits)

// ---------------------------------------------------------
// Metadata slice bounds (offsets from room_base + 4), units: bytes
// Intended for loop bounds: copy while Y < *_END_EXCL.
// ---------------------------------------------------------
.const ROOM_META_8_START_OFS   = $00  // start of 8-bit fields block (+$00..+$05)
.const ROOM_META_8_END_EXCL    = $06  // end-exclusive bound for 8-bit block (6 bytes total)

.const ROOM_META_16_START_OFS  = $06  // start of 16-bit fields block (+$06..+$0F), step in 2-byte pairs
.const ROOM_META_16_END_EXCL   = $10  // end-exclusive bound for 16-bit block (5 pairs, lo at ofs, hi at ofs+1)


// symbol dictionaries for decompression (copied before stream)
.label tile_dict              = $714A    // 4-byte symbol dictionary for the tile stream
.label color_dict             = $714E    // 4-byte symbol dictionary for the color stream
.label mask_dict              = $7152    // 4-byte symbol dictionary for the mask-index stream

// load_room_metadata vars
.label room_base_lo           = $C3      // low byte of current room resource base
.label room_base_hi           = $C4      // high byte of current room resource base
.label read_ptr_lo            = $15      // low byte of metadata/record read pointer
.label read_ptr_hi            = $16      // high byte of metadata/record read pointer

// cache_room vars
.label room_data_ptr_local_lo = $387B    // low byte of freshly loaded room base
.label room_data_ptr_local_hi = $387C    // high byte of freshly loaded room base
.label room_index             = $387D    // room index used when publishing pointer/tables

// load_room_objects vars
.label obj_count_remaining    = $1D      // remaining objects to process in current room
.label object_idx             = $1B      // 1-based object index for table/record access

// load_room_sounds_and_scripts vars
.label room_sounds_base       = $15      // base pointer (lo/hi) to sounds+scripts index table
.label sound_count_left       = $17      // remaining sounds to ensure resident
.label script_count_left      = $18      // remaining scripts to ensure resident
.label meta_ptr               = $19      // pointer (lo/hi) to room metadata block

// spawn_actors_for_room_costumes vars
.label unused                 = $3880    // legacy scratch byte written on entry (no semantics)

// read_room_graphics vars
.label bytes_left_lo          = $15      // remaining bytes to decode (low byte)
.label bytes_left_hi          = $16      // remaining bytes to decode (high byte)
.label payload_len_lo         = $1D      // payload size prefix for mask block (low byte)
.label payload_len_hi         = $1E      // payload size prefix for mask block (high byte)
.label gfx_read_ptr_lo        = $27      // compressed-stream read pointer (low byte)
.label gfx_read_ptr_hi        = $28      // compressed-stream read pointer (high byte)
.label gfx_write_ptr_lo       = $19      // decode destination write pointer (low byte)
.label gfx_write_ptr_hi       = $1A      // decode destination write pointer (high byte)
.label gfx_alloc_base_lo      = $4F      // base of allocated mask-pattern block (low byte)
.label gfx_alloc_base_hi      = $50      // base of allocated mask-pattern block (high byte)
.label dbg_room_width         = $FD91    // debug mirror of room_width for diagnostics
.label dbg_room_height        = $FD92    // debug mirror of room_height for diagnostics


/*
================================================================================
  switch_to_room
================================================================================
Summary
	Switch to another room and hydrate assets.
	
	Tear down the current room cleanly, switch to a new room given by X,
	then bring up its scripts, sounds, actors, and raster state, finishing
	with an “open shutter” transition.

Arguments
	X                       Incoming target room index to activate.

Global Inputs
	actor_for_costume[]     Per-costume assignment sentinel (bit7=1 ⇒ free).
	current_room            Index of the room being exited (used in subcalls).

Global Outputs
	actor_for_costume[]     Cleared for any slot that was bound to an actor,
	via detach_actor_from_costume.
	var_current_room        Updated to the new active room index (mirror of X).
	cam_target_pos          Seeded with CAM_DEFAULT_POS for the new room.
	open_shutter_flag       Set to 1 to request an “open shutter” transition.	
	(room / script / sound / actor state)	Updated indirectly							
	(UI / raster / sprite state)			Reinitialized indirectly

Description
	- Prologue:
		• Save X (target room index) on the stack so teardown logic can
		clobber registers freely.
	- Teardown of current room:
		• init_sentence_ui_and_stack to clear pending verb/noun input so
		old commands cannot leak into the new scene.
		• execute_room_exit_script to run the current room’s EXIT script
		while its data is still valid.
		• hide_all_actors_and_release_sprites to remove all on-screen
		actors/sprites and avoid one-frame carryover.
		• Walk costume slots from $18 down to $01; for any slot whose
		actor_for_costume[x].bit7==0 (assigned), call
		detach_actor_from_costume to unbind the actor and free the slot.
	- Switch to new room:
		• Restore X from the stack and publish it to var_current_room as
		the new room index.
		• Seed cam_target_pos with CAM_DEFAULT_POS so camera logic has a
		sane starting goal.
		• Call init_raster_and_sound_state to reinitialize raster IRQ and
		sound envelopes before loading new assets.
		• Call load_room_gfx_and_objects to:
			– Age and release resources for the old room.
			– Ensure the new room is resident and load its metadata,
			objects, and graphics.
	- Bring-up for new room:
		• execute_room_entry_script to run the new room’s ENTRY script.
		• load_room_sounds_and_scripts to ensure all room-linked sounds
		and scripts are resident.
		• spawn_actors_for_room_costumes to spawn actors for costumes whose
		dynamic room index matches current_room.
	- Visual transition:
		• Set open_shutter_flag := 1 to request an open-shutter effect on
		subsequent frames.
		• Call init_raster_irq_env to reprogram the raster IRQ chain for
		the new scene before returning.

Notes
	Costume slot $00 is intentionally skipped by the release loop (reserved
	by convention). EXIT scripts always run before the room switch; ENTRY
	scripts always run after the new room’s assets are loaded.
================================================================================
*/
* = $3466
switch_to_room:
		// Save target room index
		txa
		pha

		// Teardown (in order) to leave no stale UI/actors before switching rooms
		jsr     init_sentence_ui_and_stack      	// flush pending verb/noun input so scripts don’t consume old commands
		jsr     execute_room_exit_script         	// run the CURRENT room’s exit logic while its data is still valid
		jsr     hide_all_actors_and_release_sprites // hide all actors to avoid flicker during reload

		// ------------------------------------------------------------
		// Release any actor → costume bindings
		// ------------------------------------------------------------
		ldx     #COSTUME_MAX_INDEX
release_costume_in_use:
		lda     actor_for_costume,x
		bmi     next_costume                   		// N=1 (bit7 set) → empty slot → skip unassign

		txa                                     	// save X across subcall
		pha
		jsr     detach_actor_from_costume       	// free this costume slot’s actor assignment
		pla
		tax

next_costume:
		dex                                     
		bne     release_costume_in_use          

		// Recover target room index saved earlier and publish it for debugging
		pla
		tax
		stx     var_current_room              

		// Seed camera with a default target position 
		lda     #CAM_DEFAULT_POS
		sta     cam_target_pos       

		// Re-init sprites/sound engine before loading room assets
		jsr     init_raster_and_sound_state

		// Switch assets to the new room (updates current_room, loads base data, etc.)
		jsr     load_room_gfx_and_objects

		// Run the new room's entry script
		jsr     execute_room_entry_script

		// Cache room’s sounds and scripts
		jsr     load_room_sounds_and_scripts

		// Spawn all actors for the costumes in the room
		jsr     spawn_actors_for_room_costumes

		// Request “open shutter” transition effect on next frame(s)
		lda     #SHUTTER_OPEN
		sta     open_shutter_flag

		// Reset raster IRQ handlers for the new scene
		jsr     init_raster_irq_env
		rts
/*
================================================================================
  load_room_gfx_and_objects
================================================================================
Summary
		Switch to a new room, load its data and assets accordingly.
		
        -Switch the active room to X (with X=0 meaning “no active room”)
		-Age the previously active room in the liveness table
		-Release per-room visual resources
		-Optionally clear room script entry points
		-Ensure room data is cached
		-Load its metadata/objects/graphics
		-Run an LRU age pass over all resident rooms

Arguments
        X                       New room index to activate.
                                X = 0 ⇒ select “no active room” and clear
                                entry/exit script pointers.

        A                       Only semantically required when current_room is
                                0 at entry: must already hold the attribute
                                value to store into room_liveness_tbl[0].

Global Inputs
        current_room            Index of the active room at entry (old room).
        room_liveness_tbl[]     Per-room attribute byte (bit7=lock, bits0..6=age).
        room_ptr_lo_tbl[]       Per-room base pointer (lo); hi!=0 ⇒ resident.
        room_ptr_hi_tbl[]       Per-room base pointer (hi); hi!=0 ⇒ resident.
        room_gfx_layers_lo/hi   Per-room graphics-layer buffer pointer.
        mask_bit_patterns_lo/hi Pointer to heap-allocated mask-patterns block.

Global Outputs
        current_room            Updated to X (new active room index).
        var_current_room        Mirror of current_room for subsystems/scripts.
        room_liveness_tbl[]     Updated:
                                  - Old room: age set to 1 (locked/unlocked path).
                                  - New room: age set to 0 while preserving lock bit.
        room_gfx_layers_hi      Cleared to 0 when old room-layer buffer freed.
        mask_bit_patterns_hi    Cleared to 0 when mask-patterns block freed.
        room_exit_script_ptr    Cleared to 0 when X=0 (no active room).
        room_entry_script_ptr   Cleared to 0 when X=0 (no active room).

Description
        - Age outgoing room:
             • Y := current_room.
             • If Y != 0:
                  – Read room_liveness_tbl[Y]; if lock bit set, write
                    ROOM_ATTR_LOCKED_AGE_1, otherwise ROOM_ATTR_UNLOCKED_AGE_1.
             • If Y == 0:
                  – Skip lock test and directly store A to room_liveness_tbl[0].
        - Publish new active room:
             • current_room := X.
             • var_current_room := current_room.
        - Release prior per-room visual resources:
             • If room_gfx_layers_hi != 0, call mem_release on the room-gfx
               buffer and then clear room_gfx_layers_hi to 0 (lo left stale).
             • If mask_bit_patterns_hi != 0, call mem_release on the mask-
               patterns block and then clear mask_bit_patterns_hi to 0 (lo left
               stale).
        - Branch on new room index:
             • If current_room == 0:
                  – Clear room_exit_script_ptr and room_entry_script_ptr to 0.
                  – Discard the caller’s return address from the stack (used
                    when interrupting a running script).
                  – RTS with “no room active”.
             • Else (current_room != 0):
                  – Call cache_room to ensure the new room is loaded
                    and to set its age to 1.
                  – Read room_liveness_tbl[current_room]; if locked, write
                    ROOM_ATTR_LOCK (lock + age=0), else write
                    ROOM_ATTR_UNLOCKED_AGE_0 (unlocked + age=0).
                  – Load room content in order:
                       · load_room_metadata
                       · load_room_objects
                       · read_room_graphics
                       · setup_room_columns_with_decode_snapshots
        - LRU upkeep:
             • For X from ROOM_MAX_INDEX down to 1:
                  – If room_ptr_hi_tbl[X] == 0, skip (room not resident).
                  – Else mask age = room_liveness_tbl[X] & ROOM_ATTR_AGE_MASK.
                  – If age != 0, increment room_liveness_tbl[X] (lock bit
                    preserved).
        - Epilogue:
             • X := current_room

Notes
        Index 0 is treated specially as a “no room” sentinel: this routine
        still updates room_liveness_tbl[0] on entry and, when X=0, clears the
        room’s entry/exit script pointers and discards the caller’s return
        address to abort script execution.
================================================================================
*/
* = $34A4
load_room_gfx_and_objects:
        // Y := current room index. If Y==0 (“no room”), skip the lock test and
        // jump to the write; A must already hold the desired attribute value.
        ldy     current_room
        beq     write_room_attr_age

		// ------------------------------------------------------------
		// Set current room's age to 1 while preserving the locking state (bit7)
		//
		// room_liveness_tbl[Y].bit7  = lock (unchanged)
		// room_liveness_tbl[Y].bits0..6 = age (set to 1)
		// ------------------------------------------------------------
        lda     room_liveness_tbl,y
        bpl     room_attr_unlocked            	
        lda     #ROOM_ATTR_LOCKED_AGE_1         // locked + age=1
        jmp     write_room_attr_age
room_attr_unlocked:
        lda     #ROOM_ATTR_UNLOCKED_AGE_1      	// unlocked + age=1
write_room_attr_age:
        // Commit new liveness
        sta     room_liveness_tbl,y

		// ------------------------------------------------------------
		// Publish the new current room index
		// - var_current_room mirrors it for scripts
		// ------------------------------------------------------------
        stx     current_room
        lda     current_room
        sta     var_current_room

		// ------------------------------------------------------------
		// Release gfx resource if present
		// ------------------------------------------------------------
        ldx     room_gfx_layers_lo           
        ldy     room_gfx_layers_hi           
        beq     clear_room_gfx_layers_hi 	// hi==0 → nothing to free
		
		// mem_release(X=lo, Y=hi)
        jsr     mem_release

clear_room_gfx_layers_hi:
		// Mark gfx resource as not resident
        lda     #$00
        sta     room_gfx_layers_hi          

		// ------------------------------------------------------------
		// Release mask resource if present
		// ------------------------------------------------------------
        ldx     mask_bit_patterns_lo        
        ldy     mask_bit_patterns_hi        
        beq     clear_mask_patterns_hi  	// hi==0 → nothing to free
		
		// mem_release(X=lo, Y=hi)
        jsr     mem_release

clear_mask_patterns_hi:
		// Mark mask resource as not resident
        lda     #$00
        sta     mask_bit_patterns_hi        

		// ------------------------------------------------------------
		// Branch by room validity
		//
		// current_room != 0 → load this room’s graphics & objects
		// current_room == 0 → no active room: clear entry/exit script pointers and return
		// ------------------------------------------------------------
        ldx     current_room
        bne     room_load_assets   // non-zero → proceed with loading

        // No active room: clear out room script entry/exit pointers.
        lda     #$00
        sta     room_exit_script_ptr       
        sta     room_exit_script_ptr + 1   
        sta     room_entry_script_ptr      
        sta     room_entry_script_ptr + 1  

        // Balance caller’s stack usage
		// We come from a running script so discarding the return address
		// is necessary to interrupt its execution
        pla                                   
        pla                                   
        rts                                   

room_load_assets:
		// ------------------------------------------------------------
		// Cache new room
		// ------------------------------------------------------------
        jsr     cache_room

		// ------------------------------------------------------------
		// Mark active room as MRU (age = 0), keep lock state
		//
		// - room_liveness_tbl: bit7 = lock, bits0..6 = age
		// - If locked (bit7=1) → write LOCK|0
		// - If unlocked       → write 0
		// ------------------------------------------------------------
        ldx     current_room               
        lda     room_liveness_tbl,x
        bpl     room_unlocked                  		// bit7=0 → unlocked path
        lda     #ROOM_ATTR_LOCK                		// locked + age=0 (bit7 set; age bits 0)
        jmp     commit_room_age
room_unlocked:
        lda     #ROOM_ATTR_UNLOCKED_AGE_0           // unlocked + age=0 (all bits clear)
commit_room_age:
        // Commit new liveness
        sta     room_liveness_tbl,x               	

		// ------------------------------------------------------------
		// Load remaining room content, in order:
		//
		// - load_room_metadata   	: room metadata
		// - load_room_objects     	: object table (IDs, states, positions)
		// - read_room_graphics    	: graphics layers
		// - setup_room_columns_with_decode_snapshots : setup room columns to accelerate rendering
		// ------------------------------------------------------------
        jsr     load_room_metadata
        jsr     load_room_objects
        jsr     read_room_graphics
        jsr     setup_room_columns_with_decode_snapshots

		// ------------------------------------------------------------
		// LRU upkeep
		//
		// For every resident room, increment its age by 1.
		//
		// - A room is considered resident if room_ptr_hi_tbl[x] != 0
		// - Age field is bits0..6; bit7 is lock and must be preserved
		// - If age==0, leave at 0 (current/MRU or freshly touched)
		// - Otherwise INC the full byte (lock bit unchanged)
		// ------------------------------------------------------------
        ldx     #ROOM_MAX_INDEX
lru_age_scan:
        lda     room_ptr_hi_tbl,x
        beq     age_scan_next                 	// not resident → skip

        lda     room_liveness_tbl,x
        and     #ROOM_ATTR_AGE_MASK          	// extract age
		cmp		#$00
        beq     age_scan_next                 	// age==0 → do not age

        inc     room_liveness_tbl,x             // ++age; lock bit (bit7) remains as-is

age_scan_next:
        dex
        bne     lru_age_scan

		// Restore X to the active room index and return
        ldx     current_room          
        rts
/*
================================================================================
  load_room_metadata
================================================================================
Summary
	Read and setup room metadata, including:
		-room width
		-room height
		-background colors
		-entry and exit scripts
		-tile/color/mask compressed streams
		
	Copy the active room’s core metadata into a local buffer, install room
	entry/exit script pointers, and read the 4-byte symbol dictionaries for
	the tile, color, and mask compressed streams.

Global Inputs
	current_room            Index of the active room.
	room_ptr_lo_tbl[]       Per-room resource base pointer (lo).
	room_ptr_hi_tbl[]       Per-room resource base pointer (hi); hi!=0 ⇒ resident.

Global Outputs
	current_room_rsrc       Published base pointer for the active room resource.
	room_metadata_base[]    Buffer receiving width/height, flags, and all 16-bit
							layer offsets copied from metadata.
	room_exit_script_ptr    Installed 16-bit pointer to room exit script.
	room_entry_script_ptr   Installed 16-bit pointer to room entry script.
	tile_dict[0..3]         4-byte symbol dictionary for tile-matrix stream.
	color_dict[0..3]        4-byte symbol dictionary for color-layer stream.
	mask_dict[0..3]         4-byte symbol dictionary for mask-layer stream.

Description
	- Select the active room’s base:
		• Use current_room to index room_ptr_*_tbl and publish the resulting
		pointer both to current_room_rsrc and to room_base for local use.
	- Initialize read_ptr to the metadata block:
		• Compute read_ptr := room_base + MEM_HDR_LEN, skipping the 4-byte
		in-memory resource header so reads start at metadata offset +$00.
	- Copy 8-bit metadata fields:
		• For Y from ROOM_META_8_START_OFS to ROOM_META_8_END_EXCL−1, copy
		bytes from (read_ptr),Y into room_metadata_base,Y
		(e.g., width, height, video flag, bg0..bg2).
	- Copy 16-bit offset fields:
		• Starting at ROOM_META_16_START_OFS, copy five (lo,hi) pairs from
		(read_ptr),Y into room_metadata_base,Y up to ROOM_META_16_END_EXCL.
		• These offsets define the tile definitions, tile matrix, color layer,
		mask layer, and mask-index streams relative to (room_base + 4).
	- Install script entry points:
		• Read the 16-bit exit script pointer at ROOM_EXIT_SCRIPT_OFS and
		store it into room_exit_script_ptr.
		• Read the 16-bit entry script pointer at ROOM_ENTRY_SCRIPT_OFS and
		store it into room_entry_script_ptr.
	- Read per-layer symbol dictionaries:
		• For the tile-matrix layer:
			– Compute read_ptr := room_base + tile_matrix_ofs and copy the
			4-byte dictionary at that location into tile_dict[0..3].
		• For the color layer:
			– Compute read_ptr := room_base + color_layer_ofs and copy the
			4-byte dictionary into color_dict[0..3].
		• For the mask layer:
			– Compute read_ptr := room_base + mask_layer_ofs and copy the
			4-byte dictionary into mask_dict[0..3].

Notes
	Assumes the active room is already resident
	(room_ptr_hi_tbl[current_room] != 0) and that the metadata offsets and
	layer pointers refer to valid regions within the loaded room resource.
================================================================================
*/
* = $3532
load_room_metadata:
		// ------------------------------------------------------------
		// Resolve room's base address
		// Mirror it into current_room_rsrc as well
		// ------------------------------------------------------------
		ldx     current_room
		lda     room_ptr_hi_tbl,x   
		sta     current_room_rsrc + 1  
		sta     room_base_hi  
        
		lda     room_ptr_lo_tbl,x
		sta     current_room_rsrc  
		sta     room_base_lo          

		// ------------------------------------------------------------
		// Set read cursor past the resource header,
		// so subsequent reads start at the first metadata field. 
		// ------------------------------------------------------------
		clc
		lda     room_base_lo
		adc     #<MEM_HDR_LEN
		sta     read_ptr_lo
		
		lda     room_base_hi
		adc     #>MEM_HDR_LEN
		sta     read_ptr_hi

		// ------------------------------------------------------------
		// Copy the six 8-bit fields following the header
		// ------------------------------------------------------------
		ldy     #ROOM_META_8_START_OFS
copy_meta_8bit_loop:
		lda     (read_ptr_lo),y
		sta     room_metadata_base,y
		iny
		cpy     #ROOM_META_8_END_EXCL            
		bne     copy_meta_8bit_loop

		// ------------------------------------------------------------
		// Copy the five 16-bit fields next
		// ------------------------------------------------------------
		ldy     #ROOM_META_16_START_OFS
copy_meta_16bit_loop:
		lda     (read_ptr_lo),y
		sta     room_metadata_base,y
		iny
		
		lda     (read_ptr_lo),y
		sta     room_metadata_base,y
		iny
		cpy     #ROOM_META_16_END_EXCL
		bne     copy_meta_16bit_loop

		// Copy room exit script pointer
		ldy     #ROOM_EXIT_SCRIPT_OFS
		lda     (read_ptr_lo),y
		sta     room_exit_script_ptr
		
		iny
		lda     (read_ptr_lo),y
		sta     room_exit_script_ptr + 1
		
		// Copy room entry script pointer
		iny
		lda     (read_ptr_lo),y
		sta     room_entry_script_ptr
		
		iny
		lda     (read_ptr_lo),y
		sta     room_entry_script_ptr + 1

		// ------------------------------------------------------------
		// Set read cursor at the start of the tile matrix
		//
		// read_ptr = room_base + tile_matrix_ofs
		//
		// The first bytes at this section form the symbol dictionary
		// required by the decompressor for a compressed stream.
		// ------------------------------------------------------------
		clc
		lda     room_base_lo
		adc     tile_matrix_ofs_lo
		sta     read_ptr_lo
		lda     room_base_hi
		adc     tile_matrix_ofs_hi
		sta     read_ptr_hi

		// ------------------------------------------------------------
		// Copy the symbol dictionary for the tile matrix.
		// ------------------------------------------------------------
		ldy     #COMP_DICT_MAX_OFS
copy_tile_dict_loop:
		lda     (read_ptr_lo),y
		sta     tile_dict,y
		dey
		bpl     copy_tile_dict_loop


		// ------------------------------------------------------------
		// Repeat the logic for the color layer
		//
		// read_ptr = room_base + color_layer_ofs
		// ------------------------------------------------------------
		clc
		lda     room_base_lo
		adc     color_layer_ofs_lo
		sta     read_ptr_lo
		lda     room_base_hi
		adc     color_layer_ofs_hi
		sta     read_ptr_hi

		ldy     #COMP_DICT_MAX_OFS
copy_color_dict_loop:
		lda     (read_ptr_lo),y
		sta     color_dict,y
		dey
		bpl     copy_color_dict_loop

		// ------------------------------------------------------------
		// Repeat the logic for the mask layer
		//
		// read_ptr = room_base + mask_layer_ofs
		// ------------------------------------------------------------
		clc
		lda     room_base_lo
		adc     mask_layer_ofs_lo
		sta     read_ptr_lo
		lda     room_base_hi
		adc     mask_layer_ofs_hi
		sta     read_ptr_hi

		ldy     #COMP_DICT_MAX_OFS
copy_mask_dict_loop:
		lda     (read_ptr_lo),y
		sta     mask_dict,y
		dey
		bpl     copy_mask_dict_loop

		rts
/*
================================================================================
  load_room_sounds_and_scripts
================================================================================
Summary
		Cache all room's sounds and scripts.
		
        Ensure all sounds and scripts referenced by the active room’s metadata
        are resident by walking a shared subresource index table (sounds first,
        scripts immediately after).

Global Inputs
        current_room            Active room index.
        room_ptr_lo_tbl[]       Per-room resource base pointer (lo).
        room_ptr_hi_tbl[]       Per-room resource base pointer (hi); hi!=0 ⇒ resident.

Global Outputs
        (sound/script resources)	Marked resident via rsrc_cache_sound/script.

Description
        - Compute meta_ptr := room_ptr[current_room] + MEM_HDR_LEN to reach the
          room’s metadata block.
        - Read sound_count_left from ROOM_META_SOUND_COUNT_OFS and
          script_count_left from ROOM_META_SCRIPT_COUNT_OFS.
        - Sounds phase:
             • Initialize Y := 0 as the running index into the subresource table.
             • While sound_count_left > 0:
                  – Call get_room_sounds_base_address to obtain room_sounds_base.
                  – Load the sound index from (room_sounds_base),Y into A.
                  – Call rsrc_cache_sound to ensure that sound is resident
                    (callees must not clobber Y).
                  – Increment Y and decrement sound_count_left.
        - Scripts phase:
             • Continue with the same Y; script indices directly follow the
               sound indices in the same table.
             • While script_count_left > 0:
                  – Call get_room_sounds_base_address again to re-acquire the same base.
                  – Load the script index from (room_sounds_base),Y into A.
                  – Call rsrc_cache_script to ensure that script is resident
                    (again, Y must be preserved).
                  – Increment Y and decrement script_count_left.

Notes
	Assumes the active room is already resident (room_ptr_hi_tbl[current_room] != 0),
	and that the subresource index table is laid out as:
            [sound_count entries][script_count entries]
			
	with counts stored separately in metadata. 
	Callers rely on Y being preserved by rsrc_cache_sound/script and get_room_sounds_base_address.
================================================================================
*/
* = $35D5
load_room_sounds_and_scripts:
		// ------------------------------------------------------------
		// Position read cursor at the room’s metadata: meta_ptr = room_ptr[x] + MEM_HDR_LEN.
		// ------------------------------------------------------------
		ldx     current_room
		lda     room_ptr_lo_tbl,x
		clc
		adc     #<MEM_HDR_LEN
		sta     meta_ptr
		lda     room_ptr_hi_tbl,x
		adc     #>MEM_HDR_LEN
		sta     meta_ptr + 1

		// ------------------------------------------------------------
		// Fetch subresource counts from metadata
		//
		//   +ROOM_META_SOUND_COUNT_OFS → sound_count (drives first loop)
		//   +ROOM_META_SCRIPT_COUNT_OFS → script_count (drives second loop; Y will continue past sounds)
		// ------------------------------------------------------------
		ldy     #ROOM_META_SOUND_COUNT_OFS
		lda     (meta_ptr),y
		sta     sound_count_left

		ldy     #ROOM_META_SCRIPT_COUNT_OFS
		lda     (meta_ptr),y
		sta     script_count_left

		// ------------------------------------------------------------
		// Ensure all sounds are resident
		// ------------------------------------------------------------
		ldy     #$00                    
		lda     sound_count_left
		beq     scripts_ensure_resident 	// no sounds left → skip straight to scripts

sound_next:
		jsr     get_room_sounds_base_address   		// get base address of {sounds, then scripts} index table
		lda     (room_sounds_base),y     	// fetch sound index at current Y
		jsr     rsrc_cache_sound 			// cache sound by index
		iny                              	// advance to next sound index
		dec     sound_count_left         	// loop until count reaches 0
		bne     sound_next

scripts_ensure_resident:
		// ------------------------------------------------------------
		// Ensure all scripts are resident
		// Y intentionally continues from the sounds loop, so scripts follow sounds contiguously
		// ------------------------------------------------------------
		lda     script_count_left
		beq     load_room_sounds_and_scripts_exit   

script_next:
		jsr     get_room_sounds_base_address    	// base of {sounds, then scripts} index list
		lda     (room_sounds_base),y        // fetch script index at current Y
		jsr     rsrc_cache_script         	// cache script by index
		iny                                 // advance to next script index
		dec     script_count_left           // loop until count reaches 0
		bne     script_next

load_room_sounds_and_scripts_exit:
		rts
/*
================================================================================
  get_room_sounds_base_address
================================================================================
Summary
	Get the base address of the sound section start in a room resource.
	
Global Inputs
	current_room            Index of the active room.
	room_obj_count          Number of objects in the active room (N).
	room_ptr_lo_tbl[]       Per-room resource base pointer (lo).
	room_ptr_hi_tbl[]       Per-room resource base pointer (hi).

Global Outputs
	room_sounds_base        ZP pointer to the base of the {sounds+scripts}
							index table for the active room.

Description
	- Interpret room_obj_count as N, the number of objects in the room.
	- Compute the byte size of the two per-object offset tables:
		• gfx_offset[N] (N×2 bytes) and record_offset[N] (N×2 bytes)
		together occupy 4*N bytes.
	- Add ROOM_OBJ_ABS_START (fixed base of the first per-object table) to
	this total, then add the result to room_ptr_*_tbl[current_room] to
	form:
		• room_sounds_base = room_base + ROOM_OBJ_ABS_START + 4*N
	- Store the resulting pointer (lo,hi) into room_sounds_base for callers.
	- Callers then index into this table with Y:
		• First sound_count entries are sound indices.
		• Next script_count entries are script indices.

Notes
	The implementation only uses the low byte of (4*N + ROOM_OBJ_ABS_START)
	and relies on carry from the final add into the high byte. 
	If (4*N + ROOM_OBJ_ABS_START) ≥ $0100, the high byte of this offset is
	effectively truncated, which can misaddress the table for large N
	(e.g., many objects in a room).
================================================================================
*/
* = $3618
get_room_sounds_base_address:
		// ------------------------------------------------------------
		// Compute size of room object's offset tables 
		// 2 bytes for object graphics offset
		// 2 bytes for object record offset
		// Total size = 4 * object count
		// ------------------------------------------------------------
		ldx     current_room
		lda     room_obj_count
		asl
		asl

		// Add offset of the first offset table to get an offset past the 2 tables
		clc
		adc     #ROOM_OBJ_ABS_START

		// ------------------------------------------------------------
		// Add offset to room base address
		//
		// The result is an absolute address to the start of the room's sounds section
		// BUG: if (4*N + $1C) ≥ $0100, its high byte is ignored unless this add carries,
		//      yielding an error of multiples of $0100 (e.g., N ≥ 57 can mispoint).
		// ------------------------------------------------------------
		clc
		adc     room_ptr_lo_tbl,x
		sta     room_sounds_base

		lda     room_ptr_hi_tbl,x
		adc     #$00
		sta     room_sounds_base + 1
		rts
/*
================================================================================
  load_room_objects
================================================================================
Summary
		Set up pointers and metadata for all objects in a room resource.
		
        Read the object table for the active room, build two per-object offset
        tables (compressed-gfx offsets and object-record offsets), then walk each
        object record to scatter its fields into the engine’s per-object
        attribute arrays.

Global Inputs
        current_room                    Index of the active room.
        room_ptr_lo_tbl[]               Per-room base pointer (lo) for resident rooms.
        room_ptr_hi_tbl[]               Per-room base pointer (hi); hi!=0 ⇒ resident.

Global Outputs
        room_obj_count                  Total object count for this room.
        obj_gfx_ptr_tbl[]               N×16-bit offsets to compressed gfx streams.
        room_obj_ofs_tbl[]              N×16-bit offsets to object-record starts.
        room_obj_id_{lo,hi}_tbl[]       Object IDs.
        obj_width_tbl[]                 Width values.
        obj_height_tbl[]                Height values.
        obj_left_col_tbl[]              Initial X positions.
        obj_top_row_tbl[]               Initial Y positions.
        parent_idx_tbl[]                Parent object indices.
        ancestor_overlay_req_tbl[]      Parent-possible flag (bit7 encoded).
        object_x_destination[]          Target X coordinate.
        object_y_destination[]          Target Y coordinate (lower 5 bits).
        object_destination_active[]     Destination-active flag (lower 3 bits).

Description
        - Load room_base from room_ptr_*_tbl[current_room].
        - Compute read_ptr := room_base + MEM_HDR_LEN to reach metadata.
        - Read object_count from metadata, mirror to room_obj_count and
          obj_count_remaining; if zero, return immediately.
        - Build compressed-gfx offset table:
             • Starting at ROOM_META_OBJ_GFX_OFS, copy N 16-bit (lo,hi) pairs
               into obj_gfx_ptr_tbl[], beginning at table index 2 (index 0..1
               reserved by convention).
        - Build object-record offset table:
             • Immediately following the gfx-offset block, copy the next N
               16-bit (lo,hi) pairs into room_obj_ofs_tbl[], again starting at
               table index 2.
        - For each object 1..N:
             • Compute idx2 = object_idx * 2 (pair index).
             • Build read_ptr := room_base + room_obj_ofs_tbl[idx2].
             • Extract fields from the object record:
                 – ID (lo,hi)
                 – width
                 – start X, start Y (parent-flag bit7 and Y-start bits0..6)
                 – parent index
                 – destination X and destination Y (lower 5 bits)
                 – destination_active (bits0..2)
                 – height (bits7..3)
               Write each to the corresponding attribute table.
             • Advance object_idx and decrement obj_count_remaining until all
               records are processed.

Notes
        Tables obj_gfx_ptr_tbl[] and room_obj_ofs_tbl[] use 1-based indexing in
        practice: entry 0..1 are reserved, and the first real object lives at
        pair index 2. This mirrors the original engine’s table layout.
================================================================================
*/
* = $3631
load_room_objects:
		// Resolve current room's base address
		ldx     current_room		
		lda     room_ptr_hi_tbl,x
		sta     room_base_hi            
		lda     room_ptr_lo_tbl,x
		sta     room_base_lo            

		// Skip header to set the read pointer
		// read_ptr = room_base + header
		clc
		lda     room_base_lo
		adc     #<MEM_HDR_LEN
		sta     read_ptr_lo
		lda     room_base_hi
		adc     #>MEM_HDR_LEN
		sta     read_ptr_hi

		// Fetch object_count
		ldy     #ROOM_META_OBJ_COUNT_OFS
		lda     (read_ptr_lo),y
		
		// Mirror it: obj_count_remaining drives loops; room_obj_count is for callers/UI.
		sta     obj_count_remaining
		sta     room_obj_count

		// Object count == 0? If so, exit
		bne     copy_obj_comp_ofs
		jmp     load_room_objects_exit

		// ------------------------------------------------------------
		// Build the per-object gfx offset table.
		// ------------------------------------------------------------
copy_obj_comp_ofs:
		sta     object_idx              	// object_idx := object_count (pair counter)
		ldy     #ROOM_META_OBJ_GFX_OFS   	// source cursor: start of OBJ GFX offsets block
		ldx     #$02                 		// dest cursor: table starts at index 2 (slot 0 reserved)

copy_obj_comp_ofs_loop:
		// Copy a 16-bit offset (lo/hi) and advance
		lda     (read_ptr_lo),y         	// copy lo byte
		sta     obj_gfx_ptr_tbl,x
		iny
		inx
		lda     (read_ptr_lo),y         	// copy hi byte
		sta     obj_gfx_ptr_tbl,x
		iny
		inx
		dec     object_idx              	// exit when all object pairs consumed (Z=1)
		bne     copy_obj_comp_ofs_loop

		// ------------------------------------------------------------
		// Stage copy of per-object record starts (offsets within the room blob).
		// object_idx := remaining objects; Y already points past previous table.
		// ------------------------------------------------------------
		lda     obj_count_remaining
		sta     object_idx
		lda     $1E                 		// Unknown purpose of this copy
		sta     $1C
		
		// Addresses pairs start at index 2 (slot 0 skipped)
		ldx     #$02                		
copy_obj_record_ofs_loop:
		// Copy a 16-bit start offset from metadata.
		lda     (read_ptr_lo),y
		sta     room_obj_ofs_tbl,x       	// lo byte
		iny
		inx
		lda     (read_ptr_lo),y
		sta     room_obj_ofs_tbl,x       	// hi byte
		iny
		inx
		dec     object_idx
		bne     copy_obj_record_ofs_loop

		// Walk objects; for each, fetch its record and fan out fields.
		lda     #$01
		sta     object_idx
load_obj_records:
		// Compute pair index into the offset table: X = object_idx * 2.
		lda     object_idx
		asl     
		tax

		// Convert object offset (relative to room base) to an absolute address
		// read_ptr = room_base + room_obj_ofs[X]
		lda     room_obj_ofs_tbl,x
		sta     read_ptr_lo
		lda     room_obj_ofs_tbl+1,x
		sta     read_ptr_hi
		
		clc
		lda     read_ptr_lo
		adc     room_base_lo          
		sta     read_ptr_lo
		
		lda     read_ptr_hi
		adc     room_base_hi          
		sta     read_ptr_hi

		// Switch X to the object index for attribute table writes
		ldx     object_idx

		// Fetch the object’s index from the record header
		ldy     #OBJ_META_IDX_HI_OFS
		lda     (read_ptr_lo),y
		sta     room_obj_id_hi_tbl,x
		ldy     #OBJ_META_IDX_LO_OFS
		lda     (read_ptr_lo),y
		sta     room_obj_id_lo_tbl,x

		// Fetch object width (in columns)
		ldy     #OBJ_META_WIDTH_OFS
		lda     (read_ptr_lo),y
		sta     obj_width_tbl,x

		// Fetch object leftmost column
		ldy     #OBJ_META_X_START_OFS
		lda     (read_ptr_lo),y
		sta     obj_left_col_tbl,x

		// Fetch packed ancestor overlay requirement/top row byte
		// Bit 7 is ancestor requirement
		// Bits 6-0 are top object row
		ldy     #OBJ_META_Y_START_OFS
		lda     (read_ptr_lo),y
		bpl     set_00            		
		lda     #%1000_0000             
		jmp     set_result
set_00:
		lda     #%0000_0000
set_result:
		sta     ancestor_overlay_req_tbl,x

		lda     (read_ptr_lo),y
		and     #%0111_1111
		sta     obj_top_row_tbl,x

		// Fetch parent object index: room object index (NOT Object ID) of this object’s parent.
		ldy     #OBJ_META_PARENT_IDX_OFS
		lda     (read_ptr_lo),y
		sta     parent_idx_tbl,x

		// Fetch horizontal "destination" coordinate.
		ldy     #OBJ_META_X_DEST_OFS
		lda     (read_ptr_lo),y
		sta     object_x_destination,x

		// Fetch vertical "destination" coordinate (bits 4-0).
		ldy     #OBJ_META_Y_DEST_OFS
		lda     (read_ptr_lo),y
		and     #%0001_1111
		sta     object_y_destination,x

		// Fetch packed height/destination active byte
		//   - bits2..0 → destination_active (3-bit flag)
		//   - bits7..3 → height (5-bit value)
		ldy     #OBJ_META_HEIGHT_OFS
		lda     (read_ptr_lo),y
		pha
		and     #%0000_0111              
		sta     object_destination_active,x
		pla
		lsr     
		lsr     
		lsr     
		sta     obj_height_tbl,x

		// Advance to next object
		inc     object_idx
		dec     obj_count_remaining
		bne     load_obj_records

load_room_objects_exit:
		rts
/*
================================================================================
  read_room_graphics
================================================================================
Summary
		Loads and sets up a room's graphics data: 
			-background colors
			-tile definitions
			-mask bit-patterns
			
Global Inputs
        current_room            Active room index.
        room_ptr_lo_tbl[]       Base address (lo) of each resident room resource.
        room_ptr_hi_tbl[]       Base address (hi) of each resident room resource.
        room_bg_colors[0..3]    Room background color bytes (bg3..bg0).
        room_tile_definitions_offset
                                16-bit offset to compressed tile-definitions stream.
        mask_indexes_ofs        16-bit offset to size-prefixed mask-index stream.

Global Outputs
        mask_bit_patterns_lo/hi Pointer to heap-allocated mask-pattern block.

Description
        - Select the active room’s resource base from room_ptr_*_tbl.
        - Program VIC background colors:
             • Map I/O in, write bg3..bg0, mirror bg0 to room_bg0_shadow,
               map I/O out.
        - Tile-definitions load:
             • Set bytes_left := TILE_DEFS_SIZE.
             • Point decomp_src_ptr to (room_base + tile_definitions_offset).
             • Point gfx_write_ptr to TILE_DEFS_ADDR ($D800).
             • Initialize dict-4 decoder and stream exactly TILE_DEFS_SIZE
               decoded bytes into $D800..$DFFF.
        - Mask-pattern block:
             • If mask_bit_patterns_hi != 0, free previous block via mem_release.
             • Point gfx_read_ptr to (room_base + mask_indexes_ofs).
             • Read 16-bit payload_len from start of block.
             • Allocate (payload_len + MEM_HDR_LEN) bytes via mem_alloc.
             • Initialize 4-byte resource header and publish pointer to
               mask_bit_patterns_lo/hi.
             • Set gfx_write_ptr to (allocated_base + MEM_HDR_LEN).
             • Point decomp_src_ptr to (gfx_read_ptr + 2) to skip size prefix.
             • Initialize dict-4 decoder and stream exactly payload_len decoded
               bytes into the allocated buffer.

Notes
        Assumes the room is already resident (room_ptr_hi_tbl[current_room] != 0)
        and that the tile-definitions stream always decodes to TILE_DEFS_SIZE
        bytes with no end-marker.
================================================================================
*/
* = $3710
read_room_graphics:
        // Snapshot room dimensions into debug vars
        lda     room_width
        sta     dbg_room_width
        lda     room_height
        sta     dbg_room_height

		// ------------------------------------------------------------
		// Resolve current room's base pointer
		// ------------------------------------------------------------
        ldx     current_room
        lda     room_ptr_lo_tbl,x     
        sta     room_base_lo
        lda     room_ptr_hi_tbl,x     
        sta     room_base_hi

		// ------------------------------------------------------------
		// Setup room's background colors via VIC registers
		//
		// - Enable IO mapping so VIC registers are visible
		// - Write bg3..bg0 in a short X-descending loop
		// - Mirror bg0 to a shadow copy for later reads
		// ------------------------------------------------------------
        ldy     #MAP_IO_IN
        sty     cpu_port                    

        ldx     #$03                        	// X := 3 → write bg3, bg2, bg1, bg0
bkg_colors_set_loop:
        lda     room_bg_colors,x            
        sta     vic_bg0_reg,x               
        dex
        bne     bkg_colors_set_loop         

		// Keep a shadow of background color 0
        lda     room_bg0                    
        sta     room_bg0_shadow

        ldy     #MAP_IO_OUT
        sty     cpu_port

		// ------------------------------------------------------------
		// Decompress tile definitions
		// ------------------------------------------------------------
        // Set decompression byte count
        lda     #<TILE_DEFS_SIZE
        sta     bytes_left_lo
        lda     #>TILE_DEFS_SIZE
        sta     bytes_left_hi 

		// ------------------------------------------------------------
		// Point decompression source at the tile-definitions stream:
		// decomp_src_ptr = room_base + tile_defs_ofs
		// ------------------------------------------------------------
        clc
        lda     room_base_lo
        adc     room_tile_definitions_offset
        sta     decomp_src_ptr_lo
		
        lda     room_base_hi
        adc     room_tile_definitions_offset + 1
        sta     decomp_src_ptr_hi

        // Set destination pointer for decoded tile definitions
        lda     #<TILE_DEFS_ADDR
        sta     gfx_write_ptr_lo
        lda     #>TILE_DEFS_ADDR
        sta     gfx_write_ptr_hi

		// ------------------------------------------------------------
		// Initialize the 4-symbol dictionary decoder for this stream
		// (uses decomp_src_ptr as the input stream; prepares the symbol table/state)
		// ------------------------------------------------------------
        jsr     decomp_dict4_init

		// ------------------------------------------------------------
        // Decompress tile data
		//
        // Loop intent: keep Y=0 and advance dst pointer manually; stop when remain_bytes == 0.
		// ------------------------------------------------------------
        ldy     #$00
decomp_tiles_loop:
        jsr     decomp_stream_next            	// produce next decoded byte in A
        sta     (gfx_write_ptr_lo),y            // store at *dst_ptr
		
		// Move write pointer
        inc     gfx_write_ptr_lo                
        bne     tiles_update_count
        inc     gfx_write_ptr_hi               

		// Update count and loop
tiles_update_count:
        lda     bytes_left_lo                   
        bne     tiles_dec_lo_byte
        dec     bytes_left_hi
tiles_dec_lo_byte:
        dec     bytes_left_lo                   
        lda     bytes_left_lo
        ora     bytes_left_hi                   
        bne     decomp_tiles_loop             

		// ------------------------------------------------------------
		// Mask bit-patterns loading
		// ------------------------------------------------------------
        // Is there a resident mask bit-patterns block? If so, release it
        ldx     mask_bit_patterns_lo          
        ldy     mask_bit_patterns_hi          
        beq     setup_mask_patterns_load      	// hi==0 ⇒ no prior block → skip free
		
		// Block resident, release it (block pointer at X/Y)
        jsr     mem_release                   

setup_mask_patterns_load:
		// ------------------------------------------------------------
		// Resolve mask pointer
		// gfx_read_ptr = room_base + mask_idx_ofs  (start of size-prefixed block)
		// ------------------------------------------------------------
        clc
        lda     room_base_lo
        adc     mask_indexes_ofs
        sta     gfx_read_ptr_lo
        lda     room_base_hi
        adc     mask_indexes_ofs + 1
        sta     gfx_read_ptr_hi

        // Read mask payload size
        ldy     #$00
        lda     (gfx_read_ptr_lo),y
        sta     payload_len_lo
        iny
        lda     (gfx_read_ptr_lo),y
        sta     payload_len_hi

		// ------------------------------------------------------------
		// Compute total block size = payload_len + header (4 bytes)
		// Store into bytes_left for use as the request to mem_alloc
		// ------------------------------------------------------------
        clc
        lda     payload_len_lo
        adc     #<MEM_HDR_LEN
        sta     bytes_left_lo
		
        lda     payload_len_hi
        adc     #>MEM_HDR_LEN
        sta     bytes_left_hi

		// ------------------------------------------------------------
		// Alloc a block for the mask resource
		// mem_alloc expects (X=lo, Y=hi) byte count; returns block base in (X,Y)
		// ------------------------------------------------------------
        ldx     bytes_left_lo
        ldy     bytes_left_hi
        jsr     mem_alloc

        // Stash the block base for later writes and publishing
        stx     gfx_alloc_base_lo
        sty     gfx_alloc_base_hi

		// ------------------------------------------------------------
		// Initialize the resource header for this block
		// ------------------------------------------------------------
        lda     #RSRC_TYPE_ROOM_LAYERS
        sta     rsrc_resource_type
        lda     #RSRC_INDEX_MASK_LAYER
        sta     rsrc_resource_index
        jsr     rsrc_hdr_init

        // ------------------------------------------------------------
        // Publish the newly allocated block as the current mask bit-patterns
        // ------------------------------------------------------------
        ldx     gfx_alloc_base_lo
        ldy     gfx_alloc_base_hi
        stx     mask_bit_patterns_lo          
        sty     mask_bit_patterns_hi          
        stx     gfx_write_ptr_lo
        sty     gfx_write_ptr_hi

        // ------------------------------------------------------------
        // Point destination at start of payload within the new block:
		//
        // gfx_write_ptr_lo := gfx_write_ptr_lo + MEM_HDR_LEN  (skip 4-byte resource header)
        // ------------------------------------------------------------
        clc
        lda     gfx_write_ptr_lo
        adc     #<MEM_HDR_LEN          
        sta     gfx_write_ptr_lo
		
        lda     gfx_write_ptr_hi
        adc     #>MEM_HDR_LEN          
        sta     gfx_write_ptr_hi

        // ------------------------------------------------------------
        // Set decompression byte count (as the payload size)
        // ------------------------------------------------------------
        lda     payload_len_lo
        sta     bytes_left_lo
        lda     payload_len_hi
        sta     bytes_left_hi

        // ------------------------------------------------------------
        // Reacquire compressed mask stream
		//
        // gfx_read_ptr = room_base + mask_indexes_ofs
        // ------------------------------------------------------------
		ldx 	current_room
		lda 	room_ptr_lo_tbl,X
		sta 	room_base_lo
		lda 	room_ptr_hi_tbl,X
		sta 	room_base_hi
		
        clc
        lda     room_base_lo
        adc     mask_indexes_ofs
        sta     gfx_read_ptr_lo
        lda     room_base_hi
        adc     mask_indexes_ofs + 1
        sta     gfx_read_ptr_hi

        // ------------------------------------------------------------
        // Set decompression source pointer past the 2-byte size prefix:
		//
        // decomp_src_ptr = gfx_read_ptr + 2
        // ------------------------------------------------------------
        clc
        lda     gfx_read_ptr_lo
        adc     #$02
        sta     decomp_src_ptr_lo
        lda     gfx_read_ptr_hi
        adc     #$00
        sta     decomp_src_ptr_hi

		// ------------------------------------------------------------
		// Initialize the 4-symbol dictionary decoder for this stream
		// (uses decomp_src_ptr as the input stream; prepares the symbol table/state)
		// ------------------------------------------------------------
        jsr     decomp_dict4_init
		
		// ------------------------------------------------------------
        // Decompress mask data
		//
        // Loop intent: keep Y=0 and advance dst pointer manually; stop when remain_bytes == 0.
		// ------------------------------------------------------------
        ldy     #$00
decomp_mask_patterns_loop:
        jsr     decomp_stream_next            	// produce next decoded byte in A
        sta     (gfx_write_ptr_lo),y            // store at *dst_ptr
		
		// Move write pointer
        inc     gfx_write_ptr_lo                
        bne     mask_update_count
        inc     gfx_write_ptr_hi                

		// Update count and loop
mask_update_count:
        lda     bytes_left_lo                   
        bne     mask_dec_lo_byte
        dec     bytes_left_hi
mask_dec_lo_byte:
        dec     bytes_left_lo                   
        lda     bytes_left_lo
        ora     bytes_left_hi                   
        bne     decomp_mask_patterns_loop     

        rts
/*
=======================================================================
  cache_room
=======================================================================
Summary
	Cache a room into memory.
	
	Ensure the room identified by X is resident in memory. If already
	resident, skip disk I/O and only update its liveness age while
	preserving the lock bit.

Arguments
	X                       Target room index to ensure resident and age.

Global Inputs
	room_ptr_hi_tbl[]       Per-room hi-byte pointer; hi!=0 ⇒ room already resident.
	room_liveness_tbl[]     Per-room attribute byte (lock bit and age field).

Global Outputs
	room_ptr_lo_tbl[]       Updated with newly loaded room base (cold path only).
	room_ptr_hi_tbl[]       Updated with newly loaded room base (cold path only).
	room_liveness_tbl[]     Updated to AGE_1 while preserving lock bit.

Description
	- Fast path:
		- Check room_ptr_hi_tbl[X]; if non-zero, room is already resident.
		- Skip disk I/O and jump directly to the age update for that room.
	- Cold path:
		- Mirror X into rsrc_resource_index and room_index for downstream use.
		- Call room_disk_chain_prepare to configure the disk chain for this room.
		- Clear rsrc_read_offset and rsrc_sector_idx so the load starts at the header.
		- Set rsrc_resource_type to the room resource type.
		- Call rsrc_load_from_disk to fetch the room resource; capture its pointer in
		room_data_ptr_local and publish it into room_ptr_lo_tbl/room_ptr_hi_tbl at
		index room_index.
		- Reload Y from rsrc_resource_index so the subsequent age update targets
		this room.
		- Age update:
			- Read room_liveness_tbl[Y]; if the lock bit is set, write LOCKED_AGE_1,
			otherwise write UNLOCKED_AGE_1.
			- Leave with the room’s lock bit preserved and its age field set to 1.

Notes
	This routine assumes that, on the fast path, Y already refers to the same
	room index as X when room_ptr_hi_tbl[X] is non-zero; the age update uses Y
	as the index into room_liveness_tbl.
=======================================================================
*/
* = $3833
cache_room:
		// Room already resident? If so, skip load and set age to 1
		lda     room_ptr_hi_tbl,x
		bne     room_set_age_to_1

        // ------------------------------------------------------------
		// Room not resident 
		// 
		// Prime the loader’s parameter block for a room fetch.
		// Mirror the room index into both slots so subroutines can use it
		// without relying on X surviving; room_disk_chain_prepare initializes
		// the device/chain state prior to the read.
        // ------------------------------------------------------------
		stx     rsrc_resource_index
		stx     room_index
		jsr     room_disk_chain_prepare

        // ------------------------------------------------------------
		// Load room from disk and stash pointer
        // ------------------------------------------------------------
		// Room resources always reside at the start of a disk chain (sector 0, offset 0)
		lda     #$00
		sta     rsrc_read_offset
		lda     #$00
		sta     rsrc_sector_idx

		// Set resource type
		lda     #RSRC_TYPE_ROOM
		sta     rsrc_resource_type

		// Load room from disk
		jsr     rsrc_load_from_disk
		
		// Stash room pointer
		stx     room_data_ptr_local_lo              
		sty     room_data_ptr_local_hi          

        // ------------------------------------------------------------
		// Publish the pointer into the residency tables
        // ------------------------------------------------------------
		ldy     room_index
		lda     room_data_ptr_local_lo
		sta     room_ptr_lo_tbl,y
		lda     room_data_ptr_local_hi
		sta     room_ptr_hi_tbl,y

        // ------------------------------------------------------------
		// Set age = 1 while preserving lock (bit7 of room liveness).
        // ------------------------------------------------------------
		ldy     rsrc_resource_index
room_set_age_to_1:
		lda     room_liveness_tbl,y
		bpl     attr_unlocked_path

		// Locked case
		lda     #ROOM_ATTR_LOCKED_AGE_1
		jmp     commit_room_attr
attr_unlocked_path:
		// Unlocked case
		lda     #ROOM_ATTR_UNLOCKED_AGE_1
commit_room_attr:
		sta     room_liveness_tbl,y
		rts
/*
=======================================================================
  spawn_actors_for_room_costumes
=======================================================================
Summary
	Assign actors for all costumes present in the current room.

Global Inputs
	actor_for_costume[]     Per-costume assignment (bit7=1 ⇒ unassigned).
	costume_room_idx[]      Per-costume current room index.
	current_room            Active room index to match against.

Global Outputs
	actor_for_costume[]     Updated indirectly via assign_costume_to_free_actor.

Description
	- Start with X=0 and scan each costume slot up to COSTUME_MAX_INDEX.
	- Skip if actor_for_costume[x].bit7==0 (already assigned).
	- Skip if costume_room_idx[x] != current_room.
	- For a matching slot, call rsrc_cache_costume to ensure the costume
	resource is resident and to publish its resource index.
	- Load X from rsrc_resource_index and call assign_costume_to_free_actor
	so a free actor is allocated and bound to this costume.
	- Restore X from rsrc_resource_index and continue scanning until all
	costume slots have been considered.

Notes
	Assumes the costume slot index and costume resource index share the
	same index space (rsrc_resource_index corresponds to the scanned slot).
=======================================================================
*/
* = $3881
spawn_actors_for_room_costumes:
        // Legacy/harmless side effect: write A to 'unused'
        sta     unused

        // Iterate over all costume slots
        ldx     #$00
check_costume_presence:
		// Costume already has an actor assigned? If so, skip
        // bit7=1 ⇒ no actor bound; bit7=0 ⇒ already bound → skip
        lda     actor_for_costume,x
        bpl     next_costume_2

        // Costume is present in the current room? If not, skip
        lda     costume_room_idx,x
        cmp     current_room
        beq     costume_in_current_room      
        jmp     next_costume_2

costume_in_current_room:
		// Cache costume in memory
        jsr     rsrc_cache_costume

        // Bind a free actor slot to this costume.
        ldx     rsrc_resource_index
        jsr     assign_costume_to_free_actor

        // Restore costume index into X
        ldx     rsrc_resource_index

next_costume_2:
        // Advance to next costume
        inx
        cpx     #COSTUME_MAX_INDEX + 1
        bne     check_costume_presence
        rts
