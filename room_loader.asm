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
// Room meta layout (offsets from start of metadata, or from room_base + 4)
// ---------------------------------------------------------
.const ROOM_META_WIDTH_OFS          = $00  // room width in tiles
.const ROOM_META_HEIGHT_OFS         = $01  // room height in tiles (always $11)
.const ROOM_META_VIDEO_FLAG_OFS     = $02  // video/render flag (observed $00)
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
 * Usage: the subresource index table begins at
 *   room_base + ROOM_OBJ_ABS_START + (4 * object_count)
 * because two N×2 tables (gfx_ofs, record_ofs) precede it.
 */
.const ROOM_OBJ_ABS_START  = ROOM_META_OBJ_GFX_OFS + MEM_HDR_LEN

// Notes:
// - 16-bit fields above are little-endian: lo at base+ofs, hi at base+ofs+1.
// - All offsets are byte offsets from (room_base + 4), i.e., past the 4-byte header.

/* ---------------------------------------------------------
 * Object metadata field offsets (relative to object record base), bytes
 * Notes:
 *   - These offsets are read by room_read_objects when scattering fields.
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
.label tile_dict         = $714A
.label color_dict        = $714E
.label mask_dict         = $7152

// room_read_metadata vars
.label room_base = $c3
.label read_ptr = $15

// room_ensure_resident vars
.label room_data_ptr_local = $387b
.label room_index = $387d

// room_read_objects vars
.label obj_count_remaining = $1d
.label object_idx = $1b

// room_load_sounds_and_scripts vars
.label room_sounds_base = $15
.label sound_count_left = $17
.label script_count_left = $18
.label meta_ptr = $19

//room_spawn_present_costumes vars
.label unused = $3880


//read_room_graphics vars
.label bytes_left	      	= $15   // byte counter  (16-bit)
.label payload_len		    = $1d   // payload size read from stream (16-bit)
.label gfx_read_ptr 		= $27 	// generic read pointer / decomp source (16-bit pointer)
.label gfx_write_ptr 		= $19 	// generic write pointer (16-bit pointer)
.label gfx_alloc_base      		= $4f   // mem_alloc return (16-bit pointer)
.label dbg_room_width    	= $fd91 // debug mirror: room width
.label dbg_room_height   	= $fd92 // debug mirror: room height


/*===========================================
 * load_room: switch to a new room and (re)hydrate scene state
 *
 * Summary:
 *   Tears down the current room cleanly, switches assets to the target room,
 *   then brings up logic/content for the new scene (entry script, sounds,
 *   scripts, actors), and finalizes display state (camera, raster, shutter FX).
 *
 * Arguments:
 *   X (incoming)              New room index to activate.
 *
 * Returns:
 *   (none)                    Control returns to caller after the new room is ready.
 *   Registers                 A/X/Y clobbered.
 *   Flags                     Modified by subcalls; no guarantees to callers.
 *
 * Description:
 *   - Prologue: save X on stack to survive teardown subcalls.
 *   - Teardown (old room): clear sentence queue, run EXIT script, hide all actors.
 *   - Release per-actor visuals: walk costume slots $18..$01; if actor_for_costume[x]
 *     has bit7=0, unbind via detach_actor_from_costume (bit7=1 = empty slot).
 *   - Switch: restore X; mirror to var_current_room; seed camera with CAM_DEFAULT_POS;
 *     re-init sprites/sound; call room_switch_load_assets to set current_room, load
 *     base room data, and wire scene pointers/LRU.
 *   - Bring-up (new room): run room ENTRY script; ensure room sounds & scripts are
 *     resident; instantiate/load actors present in the room.
 *   - Visual system: set open_shutter_flag=1 for transition; reprogram raster IRQs.
 *   - Notes/edge cases:
 *       • Costume slot $00 is not processed (reserved by convention).
 *       • EXIT script runs before the switch; ENTRY script runs after assets load.
 *       • Callers must supply a valid room index in X.
 *===========================================*/
* = $3466
load_room:
		// Preserve the incoming target room index across teardown work
		txa
		pha

		// Teardown (in order) to leave no stale UI/actors before switching rooms
		jsr     init_sentence_ui_and_stack      // flush pending verb/noun input so scripts don’t consume old commands
		jsr     execute_room_exit_script         // run the CURRENT room’s exit logic while its data is still valid
		jsr     hide_all_actors_and_release_sprites // hide all actors to avoid one-frame carryover/flicker during reload

		/*---------------------------------------
		 * Release any actor → costume bindings
		 *  -X scans costume slots $18..$01; exit when X reaches $00 (BNE falls through).
		 *  -actor_for_costume[x] has bit7=1 ⇒ no actor assigned (BMI taken → skip).
		 *  -detach_actor_from_costume clears the mapping for the current slot.
		 *--------------------------------------*/
		ldx     #$18
release_costume_in_use:
		lda     actor_for_costume,x
		bmi     next_costume                     // N=1 (bit7 set) → empty slot → skip unassign

		txa                                     // preserve X across subcall
		pha
		jsr     detach_actor_from_costume       // free this costume slot’s actor assignment
		pla
		tax

next_costume:
		dex                                     // loop toward $00
		bne     release_costume_in_use           // Z=0 while X≠$00 → continue

		// Recover target room index saved earlier and publish it for debugging.
		pla
		tax
		stx     var_current_room              

		// Seed camera with a sane starting goal so the first frame has a stable scroll target.
		lda     #CAM_DEFAULT_POS
		sta     cam_target_pos       

		// Re-init sprites/sound engine before loading room assets
		jsr     init_raster_and_sound_state

		// Switch assets to the new room (updates current_room, loads base data, etc.)
		jsr     room_switch_load_assets

		// Run the new room's entry script
		jsr     execute_room_entry_script

		// Ensure room’s sounds and scripts are resident
		jsr     room_load_sounds_and_scripts

		// Instantiate/load actors present in this room
		jsr     room_spawn_present_costumes

		// Request “open shutter” transition effect on next frame(s)
		lda     #$01
		sta     open_shutter_flag

		// Reset raster IRQ handlers for the new scene
		jsr     init_raster_irq_env
		rts
 /*===========================================
 * room_switch_load_assets: switch active room, load assets, and age LRU
 *
 * Summary:
 *   Switches the active room to X (0 = “no room”). Ages the previously active
 *   room, publishes the new current_room, frees old room-layer resources, and
 *   either loads all assets for the new room or clears script hooks if X=0.
 *
 * Arguments:
 *   X         			New room to activate. X=0 means “no active room”.
 *   Y         			Implicitly read from current_room to age the OLD room.
 *   A 	       			Used only when current_room==0 at entry: A must already
 *                         contain the attribute value to store for room #0/sentinel.
 *
 * Returns:
 *   X (room index)        On load path: X = current_room (echoed). On X=0 path:
 *                         unspecified.
 *   A, Y                  Clobbered.
 *   Globals               current_room, var_current_room,
 *                         room_attr_tbl[old], room_attr_tbl[new],
 *                         room_gfx_layers_hi := 0, mask_bit_patterns_hi := 0,
 *                         (if X=0) room_entry_script_ptr := 0, room_exit_script_ptr := 0.
 *   Flags                 Not preserved / undefined for callers.
 *
 * Description:
 *   1) Read Y := current_room. If Y≠0, set old room’s age to 1 while preserving its
 *      lock state (bit7): if locked → write $81, else → write $01. If Y==0, skip
 *      the lock test and store whatever is already in A to room_attr_tbl[0].
 *   2) Publish the new active room: current_room := X and mirror to var_current_room.
 *   3) Free prior per-room resources if present:
 *        - mem_release(room_layer_ptr); then clear only hi part of the ptr.
 *        - mem_release(mask_bit_patterns); then clear only hi part.
 *   4) If current_room==0: null out room entry/exit script pointers, discard the
 *      caller’s return address (interrupt running script), and RTS.
 *   5) Else load assets for the active room:
 *        - room_ensure_resident
 *        - Mark active room MRU (age=0) while preserving lock (write $80 if locked,
 *          else $00).
 *        - room_read_metadata, room_read_objects, read_room_graphics,
 *          setup_room_columns_with_decode_snapshots.
 *   6) LRU upkeep: for all resident rooms (room_ptr_hi_tbl[x]≠0), if age!=0 then
 *      INC room_attr_tbl[x]. 
 *   7) Restore X := current_room and RTS.
 *
 *   Notes/edge cases:
 *     - Index 0 denotes “no room”; routine still writes room_attr_tbl[0] on entry.
 *     - A is only semantically required when there was no previous room (Y==0) to
 *       supply the attribute for index 0/sentinel.
 *===========================================*/
* = $34A4
room_switch_load_assets:
        // Y := current room index. If Y==0 (“no room”), skip the lock test and
        // jump to the write; A must already hold the desired attribute value.
        ldy     current_room
        beq     write_room_attr_age

        /*---------------------------------------
         * Set age=1 while preserving the lock bit (bit7)
         *   room_attr_tbl[Y].bit7  = lock (unchanged)
         *   room_attr_tbl[Y].bits0..6 = age (set to 1)
         *--------------------------------------*/
        lda     room_attr_tbl,y
        bpl     room_attr_unlocked            // bit7 clear → unlocked → use AGE_1 only
        lda     #ROOM_ATTR_LOCKED_AGE_1         // locked + age=1
        jmp     write_room_attr_age
room_attr_unlocked:
        lda     #ROOM_ATTR_UNLOCKED_AGE_1              // unlocked + age=1
write_room_attr_age:
        // Commit attribute byte for room Y (age=1; lock preserved per path above).
        sta     room_attr_tbl,y

        /*---------------------------------------
         * Publish the active room index
         *  - current_room := X
         *  - var_current_room mirrors it for subsystems/scripts
         *--------------------------------------*/
        stx     current_room
        lda     current_room
        sta     var_current_room

        /*---------------------------------------
         * Release room-layer resource if present
         *  - mem_release(X=lo, Y=hi)
         *  - Clear only the hi byte afterward to mark “not loaded”
         *--------------------------------------*/
        ldx     room_gfx_layers_lo           // X := ptr.lo (call arg)
        ldy     room_gfx_layers_hi           // Y := ptr.hi (call arg)
        beq     clear_room_gfx_layers_hi     // hi==0 → nothing to free
        jsr     mem_release

clear_room_gfx_layers_hi:
        lda     #$00
        sta     room_gfx_layers_hi           // mark unloaded (hi=0; lo left stale by convention)

        /*---------------------------------------
         * Release mask-bit-patterns resource, if present
         *  - mem_release calling convention: X=ptr.lo, Y=ptr.hi
         *  - If hi==0, pointer is considered null → nothing to free
         *  - After freeing, clear only the hi byte to mark “not loaded”
         *--------------------------------------*/
        ldx     mask_bit_patterns_lo        // X := ptr.lo (arg for mem_release)
        ldy     mask_bit_patterns_hi        // Y := ptr.hi (arg for mem_release)
        beq     clear_mask_patterns_hi  	// not loaded → skip free
        jsr     mem_release

clear_mask_patterns_hi:
        lda     #$00
        sta     mask_bit_patterns_hi        // mark unloaded (hi=0; lo left stale by convention)

        /*---------------------------------------
         * Branch by room validity:
         *   - current_room != 0 → load this room’s graphics & objects
         *   - current_room == 0 → no active room: clear entry/exit script pointers and return
         *--------------------------------------*/
        ldx     current_room
        bne     room_load_assets   // non-zero → proceed with loading

        // No active room: null out room script entry/exit pointers.
        lda     #$00
        sta     room_exit_script_ptr       // lo := 0
        sta     room_exit_script_ptr + 1       // hi := 0
        sta     room_entry_script_ptr      // lo := 0
        sta     room_entry_script_ptr + 1      // hi := 0

        // Balance caller’s stack usage
		// We come from a running script? So discarding the return address
		// is necessary to interrupt its execution
        pla                                   
        pla                                   
        rts                                   // return with no room active

room_load_assets:
        /*---------------------------------------
         * Ensure active room is resident
         *  - Uses current_room to fetch the room’s main data
         *  - On return, pointer tables reflect the resident room
         *  - Keep X = current_room for subsequent steps (ageing, subloads)
         *--------------------------------------*/
        jsr     room_ensure_resident
        ldx     current_room               // X := active room index

        /*---------------------------------------
         * Mark active room as MRU (age = 0), keep lock state
         *   - room_attr_tbl: bit7 = lock, bits0..6 = age
         *   - If locked (bit7=1) → write LOCK|0
         *   - If unlocked       → write 0
         *--------------------------------------*/
        lda     room_attr_tbl,x
        bpl     room_unlocked                  // bit7=0 → unlocked path
        lda     #ROOM_ATTR_LOCK                // locked + age=0 (bit7 set; age bits 0)
        jmp     commit_room_age
room_unlocked:
        lda     #ROOM_ATTR_UNLOCKED_AGE_0               // unlocked + age=0 (all bits clear)
commit_room_age:
        sta     room_attr_tbl,x               // commit attribute byte for room X

        /*---------------------------------------
         * Load remaining room content, in order:
         *  - room_read_metadata   : room metadata/params (scripts, flags, bounds…)
         *  - room_read_objects     : object table (IDs, states, positions)
         *  - read_room_graphics    : visual assets (tiles/bitmaps/sprites)
         *  - setup_room_columns_with_decode_snapshots : scene-layer/mask pointers & final wiring
         *--------------------------------------*/
        jsr     room_read_metadata
        jsr     room_read_objects
        jsr     read_room_graphics
        jsr     setup_room_columns_with_decode_snapshots

        /*---------------------------------------
         * LRU upkeep: increment age for resident, non-zero-age rooms
         *   - A room is considered resident if room_ptr_hi_tbl[x] != 0
         *   - Age field is bits0..6; bit7 is lock and must be preserved
         *   - If age==0, leave at 0 (current/MRU or freshly touched)
         *   - Otherwise INC the full byte (lock bit unchanged)
         *--------------------------------------*/
        ldx     #ROOM_MAX_INDEX
lru_age_scan:
        lda     room_ptr_hi_tbl,x
        beq     age_scan_next                 // not loaded → skip

        lda     room_attr_tbl,x
        and     #ROOM_ATTR_AGE_MASK          // extract age (0..$7F)
		cmp		#$00
        beq     age_scan_next                 // age==0 → do not age

        inc     room_attr_tbl,x              // ++age; lock bit (bit7) remains as-is

age_scan_next:
        dex
        bne     lru_age_scan

        /*---------------------------------------
         * Epilogue: restore X to the active room index and return
         *--------------------------------------*/
        ldx     current_room          // X := current room (for callers that rely on it)
        rts
/*===========================================
 * room_read_metadata: copy per-room meta, script pointers, and layer dictionaries
 *
 * Summary:
 *   Uses current_room to find the loaded room’s base, skips the 4-byte header
 *   (header +0..+3), copies the fixed 8-bit meta block (+$00..+$05) and the
 *   16-bit offset block (+$06..+$0F) into room_data_ptr_local, installs exit/entry
 *   script pointers (+$14..+$17), then reads the 4-byte symbol dictionaries
 *   for the tile, color, and mask streams by seeking to
 *   (room_base + tile_matrix_ofs/color_layer_ofs/mask_layer_ofs).
 *
 * Arguments:
 *   current_room          Index of the active room (selects room_ptr_*_tbl[x]).
 *   room_ptr_lo_tbl[x]    Per-room data pointer (lo) for resident rooms.
 *   room_ptr_hi_tbl[x]    Per-room data pointer (hi); hi != 0 ⇒ resident.
 *
 * Returns:
 *   Registers            A,X,Y clobbered; no flags preserved.
 *   Globals              
 *                        room_exit_script_ptr, room_entry_script_ptr
 *                        tile_dict[0..3], color_dict[0..3], mask_dict[0..3]
 *
 * Description:
 *   1) Load room_base from room_ptr_*_tbl[current_room] and mirror to
 *      current_room_rsrc for other subsystems.
 *   2) Set read_ptr := room_base + MEM_HDR_LEN, so reads begin at meta +$00.
 *   3) Copy the six 8-bit meta fields (+$00..+$05) to room_data_ptr_local[0..5]
 *      (e.g., width, height, video flag, bg0..bg2).
 *   4) Copy the five 16-bit offsets (+$06..+$0F) to room_data_ptr_local[6..$0F]
 *      (tile definitions, tile matrix, color layer, mask layer, mask indexes).
 *   5) Read script pointers at +$14..+$17 and store to
 *      room_exit_script_ptr and room_entry_script_ptr.
 *   6) For each layer (tile/color/mask): compute
 *      read_ptr := room_base + <layer_ofs>, then copy the 4-byte
 *      symbol dictionary at that location into the corresponding dict buffer.
 *
 * Notes / invariants:
 *   - Assumes the room is resident (room_ptr_hi_tbl[current_room] != 0).
 *   - Expects tile_matrix_ofs, color_layer_ofs, mask_layer_ofs, mask_indexes_ofs
 *     to contain valid byte offsets relative to (room_base + 4).
 *   - Header +0..+3 is skipped; all documented offsets are relative to meta +$00.
 *===========================================*/
* = $3532
room_read_metadata:
		// Use current_room to select this room’s loaded base; mirror it for shared
		// consumers (current_room_rsrc) and keep a local zp pointer (room_base)
		// for subsequent pointer arithmetic and indexed reads.
		ldx     current_room

		lda     room_ptr_hi_tbl,x   
		sta     current_room_rsrc + 1  // publish base for other subsystems
		sta     room_base + 1          // local working base (hi)
		lda     room_ptr_lo_tbl,x
		sta     current_room_rsrc  // publish base (lo)
		sta     room_base          // local working base (lo)


		// Advance read cursor past the 4-byte resource header (header +0..+3),
		// so subsequent reads start at the first metadata field. 
		clc
		lda     room_base
		adc     #<MEM_HDR_LEN
		sta     read_ptr
		lda     room_base + 1
		adc     #>MEM_HDR_LEN
		sta     read_ptr + 1

		// Copy the six 8-bit fields following the header
		ldy     #ROOM_META_8_START_OFS
copy_meta_loop:
		lda     (read_ptr),y
		sta     room_metadata_base,y
		iny
		cpy     #ROOM_META_8_END_EXCL            
		bne     copy_meta_loop

		// Copy the five 16-bit fields
		//   +$06..+$07 → tile_definitions_ofs
		//   +$08..+$09 → tile_matrix_ofs
		//   +$0A..+$0B → color_layer_ofs
		//   +$0C..+$0D → mask_layer_ofs
		//   +$0E..+$0F → mask_indexes_ofs
		ldy     #ROOM_META_16_START_OFS
copy_gfx_ofs_loop:
		lda     (read_ptr),y
		sta     room_metadata_base,y
		iny
		lda     (read_ptr),y
		sta     room_metadata_base,y
		iny
		cpy     #ROOM_META_16_END_EXCL
		bne     copy_gfx_ofs_loop

		// Install script entry points from the metadata block:
		//   +$14..+$15 → room_exit_script_ptr
		//   +$16..+$17 → room_entry_script_ptr
		ldy     #ROOM_EXIT_SCRIPT_OFS
		lda     (read_ptr),y
		sta     room_exit_script_ptr
		iny
		lda     (read_ptr),y
		sta     room_exit_script_ptr + 1
		iny
		lda     (read_ptr),y
		sta     room_entry_script_ptr
		iny
		lda     (read_ptr),y
		sta     room_entry_script_ptr + 1

		// Position read_ptr at the tile-matrix section:
		// read_ptr = room_base + tile_matrix_ofs
		// The first bytes at this section form the symbol dictionary
		// required by the decompressor for the tile matrix stream.
		clc
		lda     room_base
		adc     tile_matrix_ofs
		sta     read_ptr
		lda     room_base + 1
		adc     tile_matrix_ofs + 1
		sta     read_ptr + 1

		// Copy the symbol dictionary for the tile matrix.
		ldy     #COMP_DICT_MAX_OFS
copy_tile_dict_loop:
		lda     (read_ptr),y
		sta     tile_dict,y
		dey
		bpl     copy_tile_dict_loop


		// Position read_ptr at the color-layer section:
		// read_ptr = room_base + color_layer_ofs
		// The first bytes at this section are the symbol dictionary 
		// for the color stream.
		clc
		lda     room_base
		adc     color_layer_ofs
		sta     read_ptr
		lda     room_base + 1
		adc     color_layer_ofs + 1
		sta     read_ptr + 1

		// Copy the symbol dictionary for the color layer.
		ldy     #COMP_DICT_MAX_OFS
copy_color_dict_loop:
		lda     (read_ptr),y
		sta     color_dict,y
		dey
		bpl     copy_color_dict_loop

		// Position read_ptr at the mask-layer section:
		// read_ptr = room_base + mask_layer_ofs.
		// The first bytes at this section are the symbol dictionary 
		// for the mask stream.
		clc
		lda     room_base
		adc     mask_layer_ofs
		sta     read_ptr
		lda     room_base + 1
		adc     mask_layer_ofs + 1
		sta     read_ptr + 1

		// Copy the symbol dictionary for the mask layer.
		ldy     #COMP_DICT_MAX_OFS
copy_mask_dict_loop:
		lda     (read_ptr),y
		sta     mask_dict,y
		dey
		bpl     copy_mask_dict_loop

		rts
/*===========================================
 * room_load_sounds_and_scripts: ensure all room sounds & scripts are resident
 *
 * Summary:
 *   Reads sound/script counts from the room’s metadata, then walks a shared
 *   subresource index table to load sounds first and scripts second. The table
 *   is contiguous: script indices immediately follow the sound indices.
 *
 * Arguments:
 *   current_room                 		Active room index selecting room_ptr_*.
 *   room_ptr_*_tbl						Base pointers for each room resource.
 * Uses: 
 *   room_get_sounds_base        		Returns base of subresource index table for this room.
 *
 * Returns:
 *   Registers                    A,X,Y clobbered.
 *   Flags                        Modified per loads/loops; no promises to callers.
 *   Globals updated              Sounds and scripts ensured resident.
 *                                
 *
 * Description:
 *   1) meta_ptr := room_ptr[current_room] + MEM_HDR_LEN to point at the metadata block.
 *   2) Read counts: sound_count and script_count
 *   3) Sounds phase:
 *        - Y := 0; for each sound: base := room_get_sounds_base(); idx := base[Y]; load sound; Y++.
 *   4) Scripts phase:
 *        - Continue with the same Y (scripts follow sounds contiguously):
 *          for each script: base := room_get_sounds_base(); idx := base[Y]; ensure script; Y++.
 *   Notes / invariants:
 *     • Callees (subres pointer getter, sound/script loaders) must preserve Y.
 *     • Caller must ensure the room is resident (room_ptr_hi_tbl[current_room] ≠ 0).
 *     • The subresource index table base must correspond to (room_base + $1C + 4*object_count).
 *===========================================*/
 * = $35D5
room_load_sounds_and_scripts:
		// Position cursor at the room’s metadata: meta_ptr = room_ptr[x] + MEM_HDR_LEN.
		// (skip the fixed header so subsequent reads index fields directly)
		ldx     current_room
		lda     room_ptr_lo_tbl,x
		clc
		adc     #<MEM_HDR_LEN
		sta     meta_ptr
		lda     room_ptr_hi_tbl,x
		adc     #>MEM_HDR_LEN
		sta     meta_ptr + 1

		// Fetch subresource counts from metadata:
		//   +$12 → sound_count (drives first loop)
		//   +$13 → script_count (drives second loop; Y will continue past sounds)
		ldy     #ROOM_META_SOUND_COUNT_OFS
		lda     (meta_ptr),y
		sta     sound_count_left

		ldy     #ROOM_META_SCRIPT_COUNT_OFS
		lda     (meta_ptr),y
		sta     script_count_left

		/*---------------------------------------
		 * Phase: ensure all sounds are resident
		 *  - Intent: iterate sound indices stored in the subresource table.
		 *  - Y is the running index into that table; callees must not clobber Y.
		 *--------------------------------------*/
		ldy     #$00                    // start at first entry in the subresource index table
		lda     sound_count_left
		beq     scripts_ensure_resident // no sounds → skip straight to scripts

sound_next:
		jsr     room_get_sounds_base   // compute/get base of {sounds, then scripts} index table
		lda     (room_sounds_base),y     // fetch sound index at current Y
		jsr     rsrc_cache_sound // load/ensure sound by index (must preserve Y)
		iny                              // advance to next subresource index
		dec     sound_count_left         // loop exit when count reaches 0 (Z=1)
		bne     sound_next

scripts_ensure_resident:
		// Ensure all scripts referenced by this room are resident.
		// Y intentionally continues from the sounds loop, so scripts follow sounds contiguously.
		lda     script_count_left
		beq     room_load_sounds_and_scripts_exit   // nothing to do

script_next:
		// Re-acquire the subresource index table base; callees must preserve Y.
		jsr     room_get_sounds_base               // base of {sounds, then scripts} index list
		lda     (room_sounds_base),y                 // fetch script index at current Y
		jsr     rsrc_cache_script         // ensure script is loaded/resident (must not clobber Y)
		iny                                         // advance to next subresource index
		dec     script_count_left                   // loop exit when count reaches 0 (Z=1)
		bne     script_next

room_load_sounds_and_scripts_exit:
		rts
/*===========================================
 * room_get_sounds_base: compute base pointer to sounds/scripts index table
 *
 * Summary:
 *   Produces a pointer to the contiguous subresource index table for the active
 *   room, where sound indices come first and script indices follow immediately.
 *   The table begins after the two per-object offset tables in metadata.
 *
 * Arguments:
 *   current_room                Active room index selecting room_ptr_*.
 *   room_ptr_lo_tbl, room_ptr_hi_tbl
 *                                Base pointer to the loaded room resource.
 *   room_obj_count       N = number of objects in this room.
 *
 * Returns:
 *   room_sounds_base             ZP pointer set to:
 *                                 room_base + ROOM_OBJ_ABS_START + (4 * N)
 *                               (Because two N×2 tables precede: gfx_ofs[N], rec_ofs[N].)
 *   Registers                   A,X clobbered; Y preserved (callers rely on Y as the running index).
 *   Flags                       Modified; no guarantees to callers.
 *
 * Description:
 *   Given room_base and N = room_obj_count, the subresource index table
 *   (sounds then scripts) starts at:
 *     room_base + ROOM_OBJ_ABS_START + 2*N (gfx_ofs) + 2*N (rec_ofs)
 *   = room_base + ROOM_OBJ_ABS_START + 4*N.
 *   Callers then iterate with Y over the table: first sound_count entries, then
 *   script_count entries. Ensure callees preserve Y.
 *
 * Notes:
 *   • Precondition: room is resident (room_ptr_hi_tbl[current_room] ≠ 0).
 *===========================================*/
* = $3618
room_get_sounds_base:
		// Compute pointer to the sounds and scripts index table for this room:
		//   room_sounds_base = room_ptr[current_room] + ($1C + 4*N) where N=room_obj_count.
		ldx     current_room

		// Form low-byte of 4*N (tables total size): A = (N << 2). High byte is discarded here.
		lda     room_obj_count
		asl
		asl

		// Add fixed preface (object gfx + header skip).
		clc
		adc     #ROOM_OBJ_ABS_START

		// Add offset.low to room base; carry only bumps the base.high.
		// BUG: if (4*N + $1C) ≥ $0100, its high byte is ignored unless this add carries,
		//      yielding an error of multiples of $0100 (e.g., N ≥ 57 can mispoint).
		clc
		adc     room_ptr_lo_tbl,x
		sta     room_sounds_base

		lda     room_ptr_hi_tbl,x
		adc     #$00
		sta     room_sounds_base + 1
		rts
/*===========================================
 * room_read_objects: gather per-object tables and scatter object fields
 *
 * Summary:
 *   Reads object_count from the room metadata, builds two variable-length
 *   per-object tables (compressed-gfx offsets and object-record offsets),
 *   then walks each object record to populate attribute arrays
 *   (ids, geometry, parent links, destinations, flags).
 *
 * Arguments:
 *   current_room                 Index selecting the active room.
 *   room_ptr_lo_tbl, room_ptr_hi_tbl
 *                                Per-room base pointer (must be resident; hi≠0 elsewhere).
 *
 * Returns:
 *   Registers                    A,X,Y clobbered; no flags preserved.
 *   Globals updated              
 *                                room_obj_count            (mirrored count)
 *                                obj_gfx_ptr_tbl[*]               (N×16-bit gfx stream offsets)
 *                                room_obj_ofs_tbl[*]              (N×16-bit record start offsets)
 *                                room_object_index_{lo,hi}[*]     (IDs)
 *                                obj_width_tbl[*], obj_height_tbl[*]
 *                                obj_left_col_tbl[*], obj_top_row_tbl[*]
 *                                object_x_destination[*], object_y_destination[*]
 *                                ancestor_overlay_req_tbl[*], parent_idx_tbl[*]
 *
 * Description:
 *   - Compute read_ptr = room_base + MEM_HDR_LEN to the metadata block.
 *   - Read object_count (N); if zero, return.
 *   - Copy N 16-bit entries into obj_gfx_ptr_tbl.
 *   - Immediately following, copy N 16-bit entries into room_obj_ofs_tbl.
 *     (Tables are written starting at byte index 2: arrays are effectively 1-based,
 *      with bytes 0..1 reserved.)
 *   - For object_idx = 1..N:
 *       • Form record pointer: room_base + room_obj_ofs_tbl[object_idx].
 *       • Extract fields
 *   - No residency guard is performed in this routine; callers must ensure
 *     room_ptr_hi_tbl[current_room] ≠ 0.
 *===========================================*/
* = $3631
room_read_objects:
		// Select the active room’s base address for subsequent indexed reads.
		// Precondition: room is resident (room_ptr_hi_tbl[x] ≠ 0 elsewhere).
		ldx     current_room
		
		lda     room_ptr_hi_tbl,x
		sta     room_base + 1            // working base (hi)
		lda     room_ptr_lo_tbl,x
		sta     room_base            // working base (lo)

		// Start parsing at the metadata region: read_ptr = room_base + header.
		clc
		lda     room_base
		adc     #<MEM_HDR_LEN
		sta     read_ptr
		lda     room_base + 1
		adc     #>MEM_HDR_LEN
		sta     read_ptr + 1

		// Fetch object_count from metadata.
		// Mirror it: obj_count_remaining drives loops; room_obj_count is for callers/UI.
		// Branch: BNE continues when count ≠ 0 (Z=0); otherwise early-return.
		ldy     #ROOM_META_OBJ_COUNT_OFS
		lda     (read_ptr),y
		sta     obj_count_remaining
		sta     room_obj_count
		bne     copy_obj_comp_ofs
		jmp     room_read_objects_exit

		// Build the per-object gfx compressed-stream offset table.
		// Later code can jump straight to each object’s compressed gfx data.
		// Loop: for each object (object_idx times), copy a 16-bit offset (lo/hi) and advance.
copy_obj_comp_ofs:
		sta     object_idx              // object_idx := object_count (pair counter)
		ldy     #ROOM_META_OBJ_GFX_OFS   // source cursor: start of offsets block
		ldx     #$02                 // dest cursor: table starts at index 2 (slot 0 reserved)

copy_obj_comp_ofs_loop:
		lda     (read_ptr),y         // copy lo byte
		sta     obj_gfx_ptr_tbl,x
		iny
		inx
		lda     (read_ptr),y         // copy hi byte
		sta     obj_gfx_ptr_tbl,x
		iny
		inx
		dec     object_idx              // exit when all object pairs consumed (Z=1)
		bne     copy_obj_comp_ofs_loop

		// Stage copy of per-object record starts (offsets within the room blob).
		// object_idx := remaining objects; Y already points past previous table.
		lda     obj_count_remaining
		sta     object_idx
		lda     $1e                 // Unknown purpose of this copy
		sta     $1c
		ldx     #$02                // destination pairs start at index 2 (slot 0 reserved)

copy_obj_record_ofs_loop:
		// For each object: copy a 16-bit start offset (lo,hi) from metadata.
		// Loop exits when object_idx reaches 0.
		lda     (read_ptr),y
		sta     room_obj_ofs_tbl,x       // lo byte
		iny
		inx
		lda     (read_ptr),y
		sta     room_obj_ofs_tbl,x       // hi byte
		iny
		inx
		dec     object_idx
		bne     copy_obj_record_ofs_loop

		// Walk objects; for each, fetch its record and fan out fields.
		lda     #$01
		sta     object_idx

load_obj_records:
		// Compute pair index into the offset table: idx2 = object_idx * 2.
		lda     object_idx
		asl     
		tax

		// Build a direct pointer to this object’s record within the room blob:
		// read_ptr := room_base + room_obj_ofs_tbl[x]  (bytes from room_base).
		lda     room_obj_ofs_tbl,x
		sta     read_ptr
		lda     room_obj_ofs_tbl+1,x
		sta     read_ptr + 1
		clc
		lda     read_ptr
		adc     room_base          // add base (lo)
		sta     read_ptr
		lda     read_ptr + 1
		adc     room_base + 1          // add base (hi) + carry
		sta     read_ptr + 1

		// Switch X to the object index for attribute table writes.
		ldx     object_idx

		// Capture the object’s identifier from the record header:
		ldy     #OBJ_META_IDX_HI_OFS
		lda     (read_ptr),y
		sta     room_obj_id_hi_tbl,x
		ldy     #OBJ_META_IDX_LO_OFS
		lda     (read_ptr),y
		sta     room_obj_id_lo_tbl,x

		// Record-specified geometry: width byte
		ldy     #OBJ_META_WIDTH_OFS
		lda     (read_ptr),y
		sta     obj_width_tbl,x

		// Horizontal start coordinate
		ldy     #OBJ_META_X_START_OFS
		lda     (read_ptr),y
		sta     obj_left_col_tbl,x

		// Derive parent-linkability flag from Y-start bit7.
		// Branch meaning: BPL when bit7=0 (N=0) → not linkable; otherwise linkable.
		ldy     #OBJ_META_Y_START_OFS
		lda     (read_ptr),y
		bpl     set_00            		// bit7 clear → store $00
		lda     #%1000_0000             // bit7 set   → store $80
		jmp     set_result
set_00:
		lda     #%0000_0000
set_result:
		sta     ancestor_overlay_req_tbl,x

		// From the same byte, keep bits0..6 as the vertical start coordinate.
		// Mask clears the parent flag (bit7) set/checked above.
		lda     (read_ptr),y
		and     #%0111_1111
		sta     obj_top_row_tbl,x

		// Parent chain: room object index (NOT Object ID) of this object’s parent.
		ldy     #OBJ_META_PARENT_IDX_OFS
		lda     (read_ptr),y
		sta     parent_idx_tbl,x

		// Target horizontal coordinate.
		ldy     #OBJ_META_X_DEST_OFS
		lda     (read_ptr),y
		sta     object_x_destination,x

		// From the same byte: keep bits4..0 as target vertical coordinate
		// Upper bits (7..5) encode a “preposition index” that is ignored by this routine.
		ldy     #OBJ_META_Y_DEST_OFS
		lda     (read_ptr),y
		and     #%0001_1111
		sta     object_y_destination,x

		// Height byte packs two fields:
		//   - bits2..0 → destination_active (3-bit flag)
		//   - bits7..3 → height (5-bit value)
		// Extract in-place: first store low 3 bits, then shift right 3 to expose height.
		ldy     #OBJ_META_HEIGHT_OFS
		lda     (read_ptr),y
		pha
		and     #%0000_0111              
		sta     object_destination_active,x
		pla
		lsr     
		lsr     
		lsr     
		sta     obj_height_tbl,x

		// Advance to next object: object_idx is 1-based; obj_count_remaining tracks loop bound.
		// Exit when obj_count_remaining reaches 0 (Z=1 after DEC); otherwise process next.
		inc     object_idx
		dec     obj_count_remaining
		bne     load_obj_records

room_read_objects_exit:
		// Done: no registers/flags preserved by contract.
		rts
/*===========================================
  read_room_graphics: decode tiles & mask-indexes, program BKG colors

  Summary:
    Programs VIC background colors for the active room, then loads two
    graphics subresources from the room blob:
      (1) Tile definitions stream → decompressed to $D800..$DFFF (size $0800).
      (2) Mask bit-patterns block → size-prefixed, heap-allocated, headered, and filled.

  Arguments:
    current_room                   Index of the active room; selects room_ptr_*_tbl.
	
  Reads:
    room_ptr_*_tbl				   Per-room base pointers to the room blob in RAM.
    room_bg_colors[0..3]           Background color bytes for VIC (bg0..bg3).
    room_bg0                       Primary background color (mirrored to shadow).
    room_tile_definitions_offset   Offset (from room_base) to the tile-defs stream.
    mask_indexes_ofs               Offset (from room_base) to mask-index block.

  Returns:
    (none)  Side effects only.
	
 Side effects:
    VIC background registers      bg0..bg3 written.
    room_bg0_shadow               Updated to match room_bg0.
    $D800..$DFFF                  Filled with decompressed tile definitions ($0800 bytes).
    mask_bit_patterns_lo/hi       Updated to newly allocated block base (or left zero if none).
    rsrc_resource_type/index      Set to tag the allocated mask-index block as ROOM_LAYERS/1.
    CPU/ZP temporaries            bytes_left, payload_len, src_ptr/dst_ptr, gfx_alloc_base updated.

  Description:
	-assumes the active room is already resident
	-reads room_base from room_ptr_*_tbl[current_room]
    -temporarily maps I/O to poke VIC background registers (bg3..bg0) and mirrors bg0 to a shadow
    -decompression #1: tile-defs stream at (room_base + room_tile_definitions_offset) uses a 4-byte
    dictionary header and a dictionary-4 decoder; exactly $0800 decoded bytes are written linearly
    to $D800..$DFFF.
    -if a previous mask bit-patterns block exists (mask_bit_patterns_hi != 0), it is freed. 
	-the mask block lives at (room_base + mask_indexes_ofs) and begins with a 16-bit little-endian payload
    size. 
	-the routine allocates (payload_size + 4) bytes
	-initializes a standard 4-byte resource header
	-records the block in mask_bit_patterns_{lo,hi}
	-decompresses exactly payload_size bytes into (block_base + 4) using the same dictionary-4 scheme starting after the
    size field. 
	-on exit, A/X/Y and flags are not preserved.
===========================================*/
* = $3710
read_room_graphics:
        // Snapshot dimensions for debugging
        lda     room_width
        sta     dbg_room_width
        lda     room_height
        sta     dbg_room_height

        /*---------------------------------------
         * Select active room's resource base
         *  - X holds current_room; use it to index room_ptr_*_tbl
         *  - Cache in room_base for subsequent offset math/reads
         *--------------------------------------*/
        ldx     current_room

        lda     room_ptr_lo_tbl,x     // base.lo := room_ptr_lo_tbl[current_room]
        sta     room_base
        lda     room_ptr_hi_tbl,x     // base.hi := room_ptr_hi_tbl[current_room]
        sta     room_base + 1

        /*---------------------------------------
         * Program background colors via VIC registers
         *  - Enable IO mapping so VIC registers are visible
         *  - Write bg3..bg0 in a short X-descending loop
         *  - Mirror bg0 to a shadow copy for later reads
         *--------------------------------------*/
        ldy     #MAP_IO_IN
        sty     cpu_port                    // map IO in (enable VIC register access)

        ldx     #$03                        // X := 3 → write bg3, bg2, bg1, bg0
bkg_colors_set_loop:
        lda     room_bg_colors,x            // fetch configured background color X
        sta     vic_bg0_reg,x               // poke VIC background color register X
        dex
        bne     bkg_colors_set_loop         // loop until X wraps past 0

        lda     room_bg0                    // keep a CPU-side shadow of bg0
        sta     room_bg0_shadow

        ldy     #MAP_IO_OUT
        sty     cpu_port

        /*---------------------------------------
         Set decode byte budget for tile definitions (exactly $0800 bytes to write)
         *--------------------------------------*/
        lda     #<TILE_DEFS_SIZE
        sta     bytes_left
        lda     #>TILE_DEFS_SIZE
        sta     bytes_left + 1 

        /*---------------------------------------
         Point decoder source at the tile-definitions stream:
         decomp_src_ptr = room_base + tile_defs_ofs  (units: bytes)
         *--------------------------------------*/
        clc
        lda     room_base
        adc     room_tile_definitions_offset
        sta     decomp_src_ptr
        lda     room_base + 1
        adc     room_tile_definitions_offset + 1
        sta     decomp_src_ptr + 1

        /*---------------------------------------
         Set destination for decoded tile definitions (tile bitmap page)
         *--------------------------------------*/
        lda     #<TILE_DEFS_ADDR
        sta     gfx_write_ptr
        lda     #>TILE_DEFS_ADDR
        sta     gfx_write_ptr + 1

        /*---------------------------------------
		 Decompress tile definitions
         *--------------------------------------*/
        // Initialize the 4-symbol dictionary decoder for this stream
        // (uses decomp_src_ptr as the input stream; prepares the symbol table/state)
        jsr     decomp_dict4_init

        // Decode tiles: stream → [gfx_write_ptr .. +$0800)
        // Loop intent: keep Y=0 and advance dst pointer manually; stop when remain_bytes == 0.
        ldy     #$00
decomp_tiles_loop:
        jsr     decomp_stream_next            // produce next decoded byte in A
        sta     (gfx_write_ptr),y             // store at *dst_ptr
        inc     gfx_write_ptr                // dst_ptr++ (lo)
        bne     tiles_update_count
        inc     gfx_write_ptr + 1                // carry into hi on wrap

tiles_update_count:
        lda     bytes_left                   // 16-bit countdown: if lo==0 then dec hi
        bne     tiles_dec_lo_byte
        dec     bytes_left + 1
tiles_dec_lo_byte:
        dec     bytes_left                   // --remain_bytes.lo
        lda     bytes_left
        ora     bytes_left + 1                   // zero when both lo and hi are zero
        bne     decomp_tiles_loop             // keep decoding while bytes remain

        /*---------------------------------------
         If a previous mask bit-patterns block is resident, release it first
         *--------------------------------------*/
        ldx     mask_bit_patterns_lo          // X:= existing block base.lo
        ldy     mask_bit_patterns_hi          // Y:= existing block base.hi
        beq     setup_mask_patterns_load      // hi==0 ⇒ no prior block → skip free
        jsr     mem_release                   // free block at (X,Y)

setup_mask_patterns_load:
        /*---------------------------------------
         Position stream cursor at the mask-indexes section:
           gfx_read_ptr = room_base + mask_idx_ofs  (start of size-prefixed block)
         *--------------------------------------*/
        clc
        lda     room_base
        adc     mask_indexes_ofs
        sta     gfx_read_ptr
        lda     room_base + 1
        adc     mask_indexes_ofs + 1
        sta     gfx_read_ptr + 1

        /*---------------------------------------
         Read payload size prefix at start of mask-index block (little-endian: lo,hi)
         *--------------------------------------*/
        ldy     #$00
        lda     (gfx_read_ptr),y
        sta     payload_len
        iny
        lda     (gfx_read_ptr),y
        sta     payload_len + 1

        /*---------------------------------------
         Compute total allocation size = payload_size + resource header (4 bytes)
         Store into remain_bytes for use as the request to mem_alloc
         *--------------------------------------*/
        clc
        lda     payload_len
        adc     #<MEM_HDR_LEN
        sta     bytes_left
        lda     payload_len + 1
        adc     #>MEM_HDR_LEN
        sta     bytes_left + 1

        /*---------------------------------------
         Request a heap block large enough to hold [4-byte header + payload]
         mem_alloc expects (X=lo, Y=hi) byte count; returns block base in (X,Y)
         *--------------------------------------*/
        ldx     bytes_left
        ldy     bytes_left + 1
        jsr     mem_alloc

        // Record the allocated block base for later writes and publishing
        stx     gfx_alloc_base
        sty     gfx_alloc_base + 1

        /*---------------------------------------
         Initialize the resource header for this block:
		 
           rsrc_type := ROOM_LAYERS, rsrc_index := 1
           rsrc_hdr_init writes the standard 4-byte header at gfx_alloc_base
         *--------------------------------------*/
        lda     #RSRC_TYPE_ROOM_LAYERS
        sta     rsrc_resource_type
        lda     #$01
        sta     rsrc_resource_index
        jsr     rsrc_hdr_init

        /*---------------------------------------
         Publish the newly allocated block as the room’s mask bit-patterns buffer
         *--------------------------------------*/
        ldx     gfx_alloc_base
        ldy     gfx_alloc_base + 1
        stx     mask_bit_patterns_lo          // record base.lo
        sty     mask_bit_patterns_hi          // record base.hi (hi==0 would mean “none”)

        /*---------------------------------------
         Point destination at start of payload within the new block:
         dst_ptr := gfx_alloc_base + MEM_HDR_LEN  (skip 4-byte resource header)
         *--------------------------------------*/
        stx     gfx_write_ptr
        sty     gfx_write_ptr + 1
        clc
        lda     gfx_write_ptr
        adc     #<MEM_HDR_LEN          // add header size (bytes) to lo
        sta     gfx_write_ptr
        lda     gfx_write_ptr + 1
        adc     #>MEM_HDR_LEN          // propagate carry into hi
        sta     gfx_write_ptr + 1

        /*---------------------------------------
         Set remaining byte budget to the payload size (we’ll decode exactly this many bytes)
         *--------------------------------------*/
        lda     payload_len
        sta     bytes_left
        lda     payload_len + 1
        sta     bytes_left + 1

		ldx 	current_room
		lda 	room_ptr_lo_tbl,X
		sta 	room_base
		lda 	room_ptr_hi_tbl,X
		sta 	room_base + 1

        /*---------------------------------------
         Reacquire stream base for mask-index section:
           gfx_read_ptr = room_base + mask_idx_ofs  (we’ll skip the size next)
         *--------------------------------------*/
        clc
        lda     room_base
        adc     mask_indexes_ofs
        sta     gfx_read_ptr
        lda     room_base + 1
        adc     mask_indexes_ofs + 1
        sta     gfx_read_ptr + 1

        /*---------------------------------------
         Set decoder source just past the 2-byte size prefix:
           decomp_src_ptr = gfx_read_ptr + 2  (units: bytes)
         *--------------------------------------*/
        clc
        lda     gfx_read_ptr
        adc     #$02
        sta     decomp_src_ptr
        lda     gfx_read_ptr + 1
        adc     #$00
        sta     decomp_src_ptr + 1

        /*---------------------------------------
		 Decompress mask patterns
         *--------------------------------------*/
        // Initialize the 4-symbol decoder for the mask-index stream:
        // reads the 4-byte dictionary at decomp_src_ptr (+0..+3) and primes state
        jsr     decomp_dict4_init
		
        // Decode mask bit-patterns payload into dst_ptr
        // Loop intent: keep Y fixed at 0, advance dst_ptr manually; stop when remain_bytes == 0
        ldy     #$00
decomp_mask_patterns_loop:
        jsr     decomp_stream_next            // A ← next decoded byte from comp_src_ptr
        sta     (gfx_write_ptr),y             // store to *dst_ptr
        inc     gfx_write_ptr                // dst_ptr++
        bne     mask_update_count
        inc     gfx_write_ptr + 1                // carry into hi on wrap

mask_update_count:
        lda     bytes_left                   // 16-bit countdown: if lo==0 then dec hi
        bne     mask_dec_lo_byte
        dec     bytes_left + 1
mask_dec_lo_byte:
        dec     bytes_left                   // --remain_bytes.lo
        lda     bytes_left
        ora     bytes_left + 1                   // Z=1 when both lo and hi are zero 
        bne     decomp_mask_patterns_loop     // continue until no bytes remain

        rts
/*===========================================
 * room_ensure_resident: ensure room is resident; set age=1 (preserve lock)
 *
 * Summary:
 *   Ensures the room indexed by X is resident in memory. If already resident
 *   (room_ptr_hi_tbl[x] != 0), skips I/O and just updates its LRU age to 1
 *   while preserving the lock bit. Otherwise, primes the loader, reads the
 *   room resource, publishes its pointer, then updates age=1.
 *
 * Arguments:
 *   X 				       Target room index to ensure resident and age.
 * 
 * State/Vars:
 *   room_index            Scratch mirror of X for publishing pointer.
 *   rsrc_resource_index        Scratch mirror of X for loader/age selection.
 *
 * Returns:
 *   (cold path)           Pointer tables updated for room Y=room_index.
 *   X, Y                  Clobbered by loader; on cold path Y ends as rsrc_resource_index,
 *                         on fast path Y is unspecified (not initialized here).
 *   A                     Clobbered.
 *   Flags                 Undefined on return.
 * 
 * Globals:
 *   		               room_ptr_lo_tbl[room_index], room_ptr_hi_tbl[room_index],
 *                         room_attr_tbl[rsrc_resource_index],
 *                         rsrc_read_offset := 0, rsrc_sector_idx := 0,
 *                         rsrc_resource_type := RSRC_TYPE_ROOM.
 *
 * Description:
 *   Fast path: 
 *				-test room_ptr_hi_tbl[x]
 *				-if nonzero, branch directly to age update
 *   Cold path: 
 * 				-copy X into rsrc_resource_index and room_index
 * 				-call room_disk_chain_prepare
 *   			-initialize rsrc_read_offset=0 (bytes) and rsrc_sector_idx=0 (sectors/pages)
 *   			-set rsrc_resource_type=RSRC_TYPE_ROOM
 *				-call rsrc_load_from_disk which returns a pointer in X/Y and save it into room_data_ptr_local
 *  			-publish to room_ptr_lo_tbl/hi_tbl[y=room_index]
 *
 *   Finally, select Y := rsrc_resource_index and set room_attr_tbl[y] to:
 *     - ROOM_ATTR_LOCKED_AGE_1 if bit7 was set
 *     - ROOM_ATTR_UNLOCKED_AGE_1 if bit7 was clear
 *===========================================*/
* = $3833
room_ensure_resident:
		// Fast path: if the room’s data pointer is already non-null (hi != $00),
		// skip storage access and go update its age. LDA sets Z based on the hi byte;
		// BNE (Z=0) takes the branch when resident.
		lda     room_ptr_hi_tbl,x
		bne     room_set_age_to_1

		// Cold path: prime the loader’s parameter block for a room fetch.
		// Mirror the room index into both slots so subroutines can use it
		// without relying on X surviving; room_disk_chain_prepare initializes
		// the device/chain state prior to the read.
		stx     rsrc_resource_index
		stx     room_index
		jsr     room_disk_chain_prepare

		// Start reads at the resource header base: read_offset = $00 (bytes).
		lda     #$00
		sta     rsrc_read_offset

		// Begin at the first sector
		lda     #$00
		sta     rsrc_sector_idx

		// Select resource kind for the loader/dispatcher: type = RSRC_TYPE_ROOM (=3).
		lda     #RSRC_TYPE_ROOM
		sta     rsrc_resource_type

		// Fetch room from disk
		jsr     rsrc_load_from_disk
		stx     room_data_ptr_local              // save ptr.lo
		sty     room_data_ptr_local + 1              // save ptr.hi

		// Publish the pointer into the residency tables (Y = room_index):
		//   lo -> room_ptr_lo_tbl[y], hi -> room_ptr_hi_tbl[y].
		ldy     room_index
		lda     room_data_ptr_local
		sta     room_ptr_lo_tbl,y
		lda     room_data_ptr_local + 1
		sta     room_ptr_hi_tbl,y

		// Choose which room’s attribute to update (index lives in rsrc_resource_index here).
		ldy     rsrc_resource_index

room_set_age_to_1:
		// Set age = 1 while preserving lock (bit7).
		// Branch logic: LDA sets N from bit7; BPL (N=0) → unlocked path; otherwise locked.
		lda     room_attr_tbl,y
		bpl     attr_unlocked_path

		// Locked case
		lda     #ROOM_ATTR_LOCKED_AGE_1
		jmp     commit_room_attr

attr_unlocked_path:
		// Unlocked case
		lda     #ROOM_ATTR_UNLOCKED_AGE_1

commit_room_attr:
		// Post-condition: room_attr_tbl[y] updated; lock preserved, age reset to 1.
		sta     room_attr_tbl,y
		rts

/*===========================================
 * room_spawn_present_costumes: spawn actors for unassigned costumes present in current_room
 *
 * Summary:
 *   Walks all costume slots and, for any slot that is currently unassigned and
 *   whose costume is located in the active room, ensures its costume resource
 *   is resident and binds a free actor slot to it.
 *
 * Arguments:
 *   X (entry)                  Ignored; routine initializes X=0 and iterates 0..COSTUME_MAX_INDEX.
 *   actor_for_costume[]        Per-costume assignment sentinel (bit7=1 ⇒ unassigned, bit7=0 ⇒ assigned).
 *   costume_room_idx[]         Per-costume current room index (dynamic location).
 *   current_room               Active room index to match against.
 *   rsrc_cache_costume (subroutine)
 *                              Ensures the costume asset is loaded; sets rsrc_resource_index.
 *   assign_costume_to_free_actor     Binds a free actor slot; consumes X as costume/resource index.
 *
 * Returns:
 *   A, X, Y                    Clobbered.
 *   Flags                      Modified by loop/subcalls; no guarantees to callers.
 *   Globals updated            actor tables via assign_costume_to_free_actor;
 *                              rsrc_resource_index set by rsrc_cache_costume;
 *                              'unused' receives A (legacy write, no semantic effect).
 *
 * Description:
 *   Iterates X from 0 to COSTUME_MAX_INDEX. For each X:
 *     - If actor_for_costume[X].bit7 == 0, skip (already assigned).
 *     - If costume_room_idx[X] != current_room, skip (not present here).
 *     - Otherwise:
 *         • rsrc_cache_costume → guarantees asset availability and writes rsrc_resource_index.
 *         • X := rsrc_resource_index; assign_costume_to_free_actor → allocate/init actor for this costume.
 *   Loop exits once all costume slots have been considered.
 *   Notes:
 *     • Assignment sentinel is active-high for “free”: bit7=1 means no actor bound yet.
 *     • The routine relies on the convention that assign_costume_to_free_actor consumes X as the
 *       costume/resource index; X is (re)loaded from rsrc_resource_index before the call.
 *===========================================*/		
* = $3881
room_spawn_present_costumes:
        // Legacy/harmless side effect: write A to 'unused' (A is caller-clobber here)
        sta     unused

        // Iterate over all costume slots (X := 0..$18); exit when X == $19
        ldx     #$00
check_costume_presence:
        // Only act on costumes that are currently UNASSIGNED:
        // sentinel: actor_for_costume[x] bit7=1 ⇒ no actor bound; bit7=0 ⇒ already bound → skip
        lda     actor_for_costume,x
        bpl     next_costume_2

        // Further filter: only spawn if this costume’s character is in the ACTIVE room
        // Compare dynamic location (costume_room_idx[x]) to current_room
        lda     costume_room_idx,x
        cmp     current_room
        beq     costume_in_current_room      // equal → handle; else fall through to skip
        jmp     next_costume_2

costume_in_current_room:
        // Make sure this costume’s asset is loaded before spawning an actor.
        // Contract: rsrc_cache_costume sets rsrc_resource_index to the
        //           resource index corresponding to the costume we’re handling.
        jsr     rsrc_cache_costume

        // Bind a free actor slot to this costume.
        // Convention: assign_costume_to_free_actor consumes X as the costume/resource index,
        // so load X from rsrc_resource_index produced by the loader above.
        ldx     rsrc_resource_index
        jsr     assign_costume_to_free_actor

        // Restore costume index into X
        ldx     rsrc_resource_index

next_costume_2:
        // Loop control: advance to next costume; exit once X == COSTUME_MAX_INDEX+1.
        inx
        cpx     #(COSTUME_MAX_INDEX + 1)    // processed 0..COSTUME_MAX_INDEX
        bne     check_costume_presence
        rts
