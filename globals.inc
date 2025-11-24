#importonce

//===========================================================
// Interrupt handlers
//===========================================================
.label nmi_handler_lo = $FFFA
.label nmi_handler_hi = $FFFB
.label irq_handler = $FFFE

// Arguments for mem_fill_x routines (declared here so ASM modules don't use absolute addresses for these vars instead of ZP ones)
.label fill_dest_ptr            = $19   // 16-bit destination pointer (lo at $19, hi at $1A); advanced after each store
.label fill_byte_cnt            = $1B   // 16-bit remaining byte count (lo at $1B, hi at $1C); decremented to zero

// ===========================================================
// Video
// ===========================================================
/*
Video memory mode values
	#00		Do nothing
	#01		Character base: D800	Video matrix: C800	Sprite shape: CBF8
	#02		Character base: D800	Video matrix: CC00	Sprite shape: CFF8
	#03		Copy color data only
*/
.label video_setup_mode = $22

//This variable (frame_buffer) can only take values of #01 or #02, it's just the index
//#01 - frame_buffer_base = CC28, #02 = C828 (yes, they are not in numerical order)
.label frame_buffer = $23
.label frame_buffer_base = $24		//Base address of frame buffer
.label vic_color_ram      = $D800
.label vic_memory_layout_shadow = $be


// ===========================================================
// Decompressor
// ===========================================================
.label decomp_src_ptr_lo 	= $27    // 16-bit pointer to compressed input stream 
.label decomp_src_ptr_hi 	= $28    // 16-bit pointer to compressed input stream 
.label decomp_emit_mode    	= $29    // current mode flag ($00 = direct, $FF = run). Only bit 7 is tested
.label decomp_emit_rem     	= $2A    // remaining outputs for the current operation (stores L, meaning L+1 total outputs)
.label decomp_run_symbol   	= $2B    // byte value to output while in run mode

// ===========================================================
// Input controls
// ===========================================================
.label kbd_keycode         = $30    // ZP: published keycode when ready
.label joy_state     	   = $33    // ZP: snapshot of joystick mask (bits 0–4)

.label vertical_offset 	  = $35
.label current_sprite_y_pos = $3f

/*
Values observed
#00	Scroll right
#FF	Scroll left
#C8	No scroll
#01	Room scene rendering starts (by opening the "lens" outwards)
*/
.label screen_scroll_flag = $4e

.label cel_seq_tbl           = $84    // cel sequence table for current actor
.label mask_row_ptr                = $86    // ZP: pointer to current 8×8-tile row within foreground mask layer
.label destination_variable = $d0
.label room_bg0_shadow = $8f
.label room_bg1_shadow = $90
.label sprite_shape_data_ptr = $8c
.label actor = $D8
.label irq_entry_count = $d9
.label print_msg_ptr	   = $DA	// ZP: 16-bit pointer to message to be printed (print_message_wait_for_button)
.label debug_error_code = $DC

// ===========================================================
// PRNG
// ===========================================================
.label random_1 = $dd
.label random_2 = $de
.label random_3 = $df


// ===========================================================
// Flags
// ===========================================================
.label vic_color_ram_copy_done = $1677
.label color_ram_copy_done = $1677 //Boolean
.label game_paused_flag = $1679	//#01 if game is paused, #00 otherwise
.label needs_sentence_rebuild = $95
.label remove_obj_from_inv_flag = $0670
.label open_shutter_flag = $066E
.label raster_irq_init_pending_flag = $96
.label kbd_key_ready       = $2061    // flag: 1 when kbd_keycode is valid
.label reload_sound_ptrs_flag     = $FE70    // flag: non-zero => refresh sound resource pointers after relocation
.label rsrc_released_flag = $FE4A
.label button_presses = $cb8a
.label joystick_button_pressed = $cb89
.label sentence_bar_needs_refresh = $fe49	//#00 = don't refresh the sentence bar, #01 = refresh it
									//By "refresh", we mean to update its contents
.label init_sentence_ui_flag = $fe8f
.label new_sound_instructions_allowed = $fe71	//00 = not allowed, nonzero = allowed
.label room_scene_clicked_flag = $fd77
.label forced_sentence_trigger = $fe8e
.label arpeggio_ongoing = $5160	// #00 = disabled, #FF = enabled
.label arpeggio_secondary_active_flag = $5161		// #00 = disabled, #FF = enabled
.label arpeggio_primary_active_flag = $5162		// #00 = disabled, #FF = enabled
.label filter_arpeggio_enabled_flag = $5164			// #00 = filter voice, #FF = stop filtering voice
.label alt_voice_slots_cleared_flag = $5167	// #00 = false, #FF = true
.label music_playback_in_progress = $4816
.label sound_processing_disabled_flag = $4819	//#00 = false, #FF = true
.label lowest_priority_sound_starting_flag = $4cf2 //#01 = True, #00 = false
.label hide_cursor_flag               = $166C
.label voice3_in_use = $4CF5				//#00 = false, #FF = true
.label actor_path_update_flag = $fde1

// ===========================================================
// Semaphores and signals
// ===========================================================
.label ui_section_semaphore = $1678 //Semaphore used to control access to a section in irq_handler1
.label video_processed_signal = $166b
.label irq_sync_lock = $e3
.label video_update_signal = $8e			// synchronization flag - NOT to be confused with vsync

// ===========================================================
// Sprites
// ===========================================================
// Double-buffer selector for sprite attribute buffers. #$00 or #$01.
.label sprite_bank_sel                 = $FD14

// X low byte (bits 0–7) per actor-assigned sprite slot.
.label sprite_0_x_lo                   = $0FCA
.label sprite_1_x_lo                   = $0FCB
.label sprite_2_x_lo                   = $0FCC
.label sprite_3_x_lo                   = $0FCD
.label sprite_4_x_lo                   = $0FCE
.label sprite_5_x_lo                   = $0FCF
.label sprite_6_x_lo                   = $0FD0

// Cursor (sprite 7) X low byte.
.label cursor_sprite_x_lo              = $0FD1

// Per-sprite color nibbles (0–15). Written to $D027..$D02D.
.label sprite_0_color                  = $0FD2
.label sprite_1_color                  = $0FD3
.label sprite_2_color                  = $0FD4
.label sprite_3_color                  = $0FD5
.label sprite_4_color                  = $0FD6
.label sprite_5_color                  = $0FD7
.label sprite_6_color                  = $0FD8

// Cursor (sprite 7) color nibble (0–15). Written to $D02E.
.label cursor_sprite_color             = $0FD9

// Hardware sprite slot indices (0–7). Map logical actors → VIC sprite slots.
.label vic_sprite_idx_0                = $0FDA
.label vic_sprite_idx_1                = $0FDB
.label vic_sprite_idx_2                = $0FDC
.label vic_sprite_idx_3                = $0FDD
.label vic_sprite_idx_4                = $0FDE
.label vic_sprite_idx_5                = $0FDF
.label vic_sprite_idx_6                = $0FE0

// Shadow of $D01D horizontal expansion control. Bit n expands sprite n (2× width).
.label sprite_horizontal_expansion     = $0FEA

// X high bit (bit 8) per sprite as 0/1 bytes. Mirrors packed version below.
.label sprite_0_x_hi                   = $0FE1
.label sprite_1_x_hi                   = $0FE2
.label sprite_2_x_hi                   = $0FE3
.label sprite_3_x_hi                   = $0FE4
.label sprite_4_x_hi                   = $0FE5
.label sprite_5_x_hi                   = $0FE6
.label sprite_6_x_hi                   = $0FE7

// Cursor (sprite 7) X high bit (bit 8) as 0/1 byte.
.label cursor_sprite_x_hi              = $0FE8

// Packed X high bits mirror of $D010. Bit n = MSB of sprite n X.
.label sprites_x_hi_combined           = $0FE9

// Cursor (sprite 7) Y position.
.label cursor_sprite_y                 = $0FEB


// -----------------------------------------------------------------------------
// Shape indices for successive raster IRQ handlers (per-scanline strip control)
// Each byte is a shape index written into the sprite shape table for that pass.
// -----------------------------------------------------------------------------
.label sprite_shape_1                  = $1661
.label sprite_shape_2                  = $1662
.label sprite_shape_3                  = $1663
.label sprite_shape_4                  = $1664
.label sprite_shape_5                  = $1665
.label sprite_shape_6                  = $1666
.label sprite_shape_7                  = $1667
.label sprite_shape_8                  = $1668

// Cursor shape index used across screen banks.
.label sprite_shape_cursor             = $1669

// Pending shapeset switch flag. 0 = no change; 1 or 2 = switch to that set.
.label target_sprite_shapeset          = $166A

//===========================================================
// Cursor state
//===========================================================
// Hide cursor flag. #$01 = hidden, #$00 = visible.

// Down-counter for cursor color update cadence.
// Decremented by irq_handler1 each run. When it reaches 0, color advances and
// the counter is reloaded from cursor_color_period.
.label cursor_color_wait_ticks        = $166D

// Current cursor color table index (0..N-1). Advanced when wait counter expires.
.label cursor_color_idx               = $166E

//===========================================================
// Cursor physics
//===========================================================
.label cursor_x_pos                   = $CB7F   // cursor X position (pixels, 0..$9F)
.label cursor_y_pos                   = $CB81   // cursor Y position (pixels, $08..$BF)
.label cursor_x_vel_frac              = $CB82   // X velocity, fractional byte (8.8 fixed-point)
.label cursor_x_vel_int               = $CB83   // X velocity, integer byte   (adds to X position)
.label cursor_y_vel_frac              = $CB84   // Y velocity, fractional byte
.label cursor_y_vel_int               = $CB85   // Y velocity, integer byte
.label drag_shift_count        = $CB7D   // expected 7 (build drag via 7× ASL/ROL)
.label accel_h_mask            = $CB7B   // horizontal acceleration mask (expects $FA)
.label accel_v_mask            = $CB7C   // vertical   acceleration mask (expects $FF)
.label cursor_x_cell = $cb86		
.label cursor_y_cell = $cb87		//Range: 01-17 (01-23 decimal)
.label cursor_y_pos_half = $fe7d				//Vertical range #08-63 (08-99)
.label cursor_x_pos_quarter_relative = $fe7e			//Horizontal range #00-27 (00-39)			
.label cursor_y_pos_half_off_by_8 = $fe7f	//Vertical range #00-5B (00-91)
.label cursor_x_pos_quarter_absolute = $fe80	//Horizontal range #07-2E (07-46)

// ===========================================================
// Camera and room viewport
// ===========================================================
.label viewport_left_col = $0664	 //Left column for the visible room scene
.label viewport_right_col = $0665 //Right column for the visible room scene
.label cam_target_pos = $0666
.label cam_current_pos = $0667
.label cam_mode = $066a				//02 = follow costume, #03 = pan to position
.label cam_follow_costume_id = $066b	//Index of costume to follow
.label cam_pan_goal = $066c				//Position to pan camera to
.label actor_follow_needs_pan = $066d		
//Flashlight beam coordinates 
.label flashlight_beam_x = $fe79
.label flashlight_beam_y = $fe7a
/*Lights_status values
#00	environment lights off, flashlights off
#01	environment lights off, flashlight on
#02	environment lights on*/
.label global_lights_state = $FEAB

// ===========================================================
// Disk
// ===========================================================
.label active_side_id = $fe9c //This represents the desired Side ID (the ID that identifies each of the game's disk sides)
						//Once disk_ensure_correct_side is called, this parameter is set and execution will not resume until the proper disk side is inserted
									  
// ===========================================================
// Room subresources metadata
// ===========================================================
.label mask_base_ptr = $fd6d
.label room_metadata_base = $7139
.label room_width = $7139
.label room_height = $713a
.label room_bg0 = $713b
.label room_bg_colors = $713b
.label room_tile_definitions_offset = $713f
.label current_room = $7149
.label room_gfx_layers_active_ptr_lo = $FE98
.label room_gfx_layers_active_ptr_hi = $FE99
.label room_obj_count = $715c	
.label obj_gfx_ptr_tbl = $7291
.label room_obj_ofs_tbl = $72c9		//Offsets of objects in a room resource
.label room_obj_id_hi_tbl = $7221	//Per-room objects ID table (mapping room object index to global Object ID)
.label room_obj_id_lo_tbl = $723d
.label tile_matrix_ofs_lo   = $7141	// 16-bit offset to tile matrix section
.label tile_matrix_ofs_hi   = $7142	// 16-bit offset to tile matrix section
.label color_layer_ofs_lo  	= $7143	// 16-bit offset to color layer section
.label color_layer_ofs_hi	= $7144	// 16-bit offset to color layer section
.label mask_layer_ofs_lo    = $7145	// 16-bit offset to mask layer section 
.label mask_layer_ofs_hi    = $7146	// 16-bit offset to mask layer section 
.label mask_indexes_ofs  = $7147	// 16-bit offset to mask indexes section
.label room_exit_script_ptr   = $7156	// 16-bit offset to exit script within room block
.label room_entry_script_ptr  = $7158	// 16-bit offset to entry script within room block

// ===========================================================
// Objects
// ===========================================================
.label obj_left_col_tbl = $715D		//Per-room object left column table (for rendering)
.label obj_top_row_tbl = $7179		//Per-room object top row table (for rendering)
.label obj_width_tbl = $7195		//Per-room object width in columns table (for rendering)
.label obj_height_tbl = $71b1		//Per-room object height in rows table (for rendering)
.label object_x_destination = $71cd
.label object_y_destination = $71e9
.label object_destination_active = $7205
.label parent_idx_tbl = $7259			//Room object index of the parent object - WARNING: it's the object index within a room's objects, NOT the object ID
										//For instance, Room 1 (Mansion entrance) has 19 objects. Among them:
										// Object index 3: key (Object ID 0-22)
										// Object index 4: doormat (Object ID 0-21)
										// The key has parent object INDEX = 4 (doormat)

.label ancestor_overlay_req    = $15     // zp: required ancestor bit7 ($00 clear / $80 set)
.label ancestor_overlay_req_tbl = $7275	//Requirement for ancestor objects overlay state (boolean: $80 = True, $00 = False)
.label object_attributes = $7301		//Global attributes for objects (indexed by Object ID) - Note: these are NOT memory attributes like used for other resources	

// ===========================================================
// Resource pointer tables
// ===========================================================
.label object_ptr_lo_tbl = $705E
.label object_ptr_hi_tbl = $708B
.label room_ptr_hi_tbl = $77F3		
.label room_ptr_lo_tbl = $782A
.label costume_ptr_hi_tbl = $7861		
.label costume_ptr_lo_tbl = $787A
.label sound_ptr_hi_tbl = $78C5			
.label sound_ptr_lo_tbl = $790B
.label script_ptr_hi_tbl = $7997		
.label script_ptr_lo_tbl = $7A37
.label room_gfx_layers_hi = $7B77		
.label room_gfx_layers_lo = $7B79
.label mask_bit_patterns_hi = $7B78		
.label mask_bit_patterns_lo = $7B7A

// ===========================================================
// Resource liveness (locking, refcount, age)
// ===========================================================
// Per-resource liveness state tables; one byte per resource.
.label room_liveness_tbl 			= $77BC
.label costume_liveness_tbl 		= $77A3
.label sound_liveness_tbl       	= $7951    
.label script_liveness_tbl 			= $7AD7

// ===========================================================
// Costumes
// ===========================================================
.label room_for_costume = $FEFC
.label costume_room_idx = $fefc	 //The CURRENT room where a costume is present
.label current_room_rsrc = $715A	//Used by draw_room_scene to ensure residence of the room resources
.label costume_dest_y = $ff15
.label costume_dest_x = $ff2e
.label dest_x = $fde6
.label dest_y = $fde7

// ===========================================================
// Actor-Costume bindings
// ===========================================================
.label costume_for_actor = $fe53		//#FF = actor not in use
.label actor_for_costume = $fe57		//Bit 7 set - actor moving, clear - not moving
.label active_costume = $fc30

// ===========================================================
// Actor clips
// ===========================================================
.label actor_current_clip = $fd02		//The current animation set for the actor
.label actor_target_clip = $fd06		//The desired animation set for the actor
.label actor_anim_counter = $fd93
.label costume_clip_set = $ff47		/*The animation set for a costume, limited to the standing range of sets (#04-#07)
										This is used by scripts to determine the direction a costume is facing 
										See the details for "animation sets"*/
.label active_costume_rsrc_hi = $7893
.label active_costume_rsrc_lo = $78ac
.label actor_gfx_base_lo = $cb42				// Base address of the graphics section, for each actor (low byte)
.label actor_gfx_base_hi = $cb46				// Base address of the graphics section, for each actor (hi byte)
.label actor_ofs_cel_hi_tbl = $cb4a				// 8-bit offset (relative to *gfx base*) to the high-byte table of cel addresses, for each actor
.label actor_clip_tbl_lo = $fe90				// Base address of clip table, for each actor (low byte)
.label actor_clip_tbl_hi = $fe94				// Base address of clip table, for each actor (hi byte)
.label actor_cel_seq_tbl_lo = $cb3a				// Base address of cel sequence table, for each actor (low byte)
.label actor_cel_seq_tbl_hi = $cb3e				// Base address of cel sequence table, for each actor (hi byte)
.label target_clip_id            = $FDE9  // Target clip id (per-actor intent)
.label clip_loop_cnt = $fde8 			// Current clip loop counter
.label actor_clip_loop_cnt = $fd0a		// Clip loop counter, for actor

// ===========================================================
// Actor limbs
// ===========================================================
.label current_limb = $FC41

.label actor_limb_slot_base = $FC36		//Base slot index into per-limb tables (listed below), for the current actor
//Per-limb tables
//Each table has 8 limb slots for each of the 4 actors, for a total of 32 limb slots
.label limb_frame_idx = $FC42			// Which animation cel to use as _current_ for the actor limb
.label limb_current_cel = $FC62			// Current cel sequence _offset_ for each limb - read the detail in the data structures file
.label limb_current_cel_seq = $FC82		// Current cel sequence
.label limb_target_cel_seq = $FCA2		// Target cel sequence
// Loop count:
// #00 = stop animation when frames are exhausted
// #FF = loop animation from the first frame when frames are exhausted
// any other value - loop once, decrement counter
.label limb_cur_loop_cnt = $FCC2		// Current loop count
.label limb_tgt_loop_cnt = $FCE2		// Target loop count

.label limb_flip_tbl = $FC05			// Horizontal flipping: #00 = unflipped, #80 = flipped

// ===========================================================
// Costume and actor rendering states and flags
// ===========================================================
/*
------------------------------------------------
 Costume animation settings

Bit 7 represents whether a costume animates its mouth when speaking (0 = it animates)
Bit 6 represents the same thing, but it's used in a different routine (prepare_message_for_topbar)
Bits 5-0 represent the sound to be played when walking (footsteps)
Note that Jeff has #00 as he's barefoot

00 - Has speaking animation, no sound played when walking
06 - Has speaking animation, sound 6 played when walking (footsteps)
46 - No speaking animation, sound 6 played when walking (footsteps)
C0 - No speaking animation, no sound played when walking
FF - No speaking animation, sound #3F played when walking
------------------------------------------------
*/
.label costume_anim_attrs = $f0d9
.label actor_motion_state        = $FD68  // per-actor motion state
.label actor_visible = $fd6f

/*
===========================================================
Animation state for actor
===========================================================
Only 2 bits are used, bit 5 (#20) and bit 0 (#01).

	Bit 5 		set - draw the actor in the main loop
				clear - ignore actor in main loop
	Bit 0 		set - refresh actor's visibility and graphics

 A refresh is triggered when:
	-the actor gets behind a foreground layer
	-an animation frame of a limb has changed
	-a limb base cell has changed
	-the animation is reset
	-a camera is fixed on a new actor
	-the animation counter has not expired

 An actor being updated in the main loop is triggered when:
	-the animation is reset
	-the actor is stopped
	-the animation counter has not expired
*/
.label actor_render_flags = $FD97

/*
===========================================================
Mouth state for actor
===========================================================
When an actor is speaking, the main_loop will automatically switch between
 the speaking states (01 and FF), thus opening and closing the mouth.

Possible mouth state values
---------------------------
	#00		closed	(default)
	#01		speaking and closed	
	#FF		speaking and open
	#80		permanently open	(will stay open until explicitly closed)
*/
.label actor_mouth_state = $fe9d	


// ===========================================================
// Actor sprites
// ===========================================================
.label sprite_buffer = $fd14		//Sprite buffer index (#00 or #01)
.label actor_tile_x_coordinate            = $FD20  // Input: actor tile X column
.label actor_tgt_sprite_x_lo = $fd1c
.label actor_tgt_sprite_x_hi = $fd39
.label actor_sprite_x_hi = $fd58
.label actor_sprite_x_lo = $fd60
.label character_sprite_bkg_colors = $f0c0	  //Background colors for each character
.label character_sprite_y = $fc37
.label sprite_vertical_offset = $cb55
.label actor_sprite_y_extent = $FD4F
.label relative_sprite_index = $fd78
.label relative_sprite_for_actor = $fd7c
.label fifth_byte = $fc03
.label intercel_vertical_displacement = $fc04
.label actor_sprite_base = $fc2e						//Base address of the current sprite being drawn in memory
.label actor_sprite_index = $fc32

// ===========================================================
// Actor waypoints and positions
// ===========================================================
.label actor_active_wpt_x = $fd25
.label actor_active_wpt_y = $fd2d
.label actor_tgt_waypoint_x = $fd29
.label actor_tgt_waypoint_y = $fd31
.label actor_y_pos = $fd3d
.label actor_x_pos = $fd41
.label actor_x_dest = $fdd9
.label actor_y_dest = $fddd
.label position_to_waypoint_x_for_actor = $fd45
.label position_to_waypoint_y_for_actor = $fd49

// ===========================================================
// Actor walkbox search and traversal
// ===========================================================
.label actor_box_attr = $fd18
.label actor_box_handler_idx = $fda0
.label actor_box_facing_override = $fda4
.label actor_box_fg_masking = $fda8
.label actor_destination_box = $fdd5
.label actor_box_cur = $fdcd
.label actor_box_prev = $fdd1
.label actor_search_depth       = $FE75

// ===========================================================
// Actor path
// ===========================================================
.label dir_x_bit = $cb58	//#00 = left, #01 = right
.label dir_y_bit = $cb59	//#00 = up, #01 = down	
.label dda_dominant_axis = $cb66	//Which axis of movement is dominant, #00 = horizontal, #01 = vertical

//Possible rename: actor_facing_mask
.label facing_direction_for_actor = $fdeb		//#00 = right, #01 = left, #80 = down, #81 = up
.label path_delta_directions_for_actor = $fdef	//#00 = right, #01 = left, #80 = down, #81 = up
.label actor_dda_dom_abs = $fdf3	//The dominant delta value (the largest between the deltas for the 2 axis)
.label actor_dx_accum = $fdf7		//Value of delta x step (how much to move along this axis on each step to match the rate of motion on the other axis)
.label actor_dy_accum = $fdfb		//Value of delta y step
.label actor_dy_abs = $fdff			//Absolute value of delta between current position and path destination (Y axis)
.label actor_dx_abs = $fe03			//Absolute value of delta between current position and path destination (X axis)
.label actor_dir_x_bit = $fe07 //#00 = left, #01 = right
.label actor_dir_y_bit = $fe0b //#00 = up, #01 = down	
.label saved_actor_x_pos = $cb5a
.label saved_actor_y_pos = $cb5b
.label saved_actor_box_attr = $cb5d
.label saved_actor_box_cur = $cb5e
.label saved_actor_path_update_flag = $cb5f

// ===========================================================
// Game script vars
// ===========================================================
.label current_kid_idx = $ff67
.label game_vars = $ff67
/*
These vars are copies of some game engine variables.
I did not find any usage of them in the final scripts.
They were definitely intended as debugging aids.
*/
.label var_camera_position = $ff69			//This would be var[2]
.label var_msg_flag = $ff6a				//This would be var[3]
.label var_current_room = $ff6b			//This would be var[4]
.label var_active_io_id_lo = $ff6c	//This would be var[5]
.label var_destination_x = $ff6d			//This would be var[6]
.label var_destination_y = $ff6e			//This would be var[7]
.label var_music_playback_in_progress = $ff6f	//This would be var[8]
.label var_active_verb_id = $ff70				//This would be var[9]
.label var_message_length = $ff71			//This would be var[10]
//FF72 and onwards are  no longer used as debugging variables
.label kid_ids = $ffc8
.label kid1_costume_id = $FFC8
.label kid2_costume_id = $FFC9
.label kid3_costume_id = $FFCA
.label engine_vars = $FEAC
.label dest_var = $5f76         // Destination game-var index

/*
 Actor_vars bit meanings when bit is set
 Bit 7 - actor invisible (dead, game won or in the radiation suit)
 Bit 6 - actor frozen (for instance, when it's captured by Edna, Ed or the tentacle)
 Bit 3 - is Weird Ed's enemy 
 Bit 2 - has watched the TV commercial (and thus knows where to send the envelope)
 Bit 1 - is Green Tentacle's friend (after giving him a recording contract)
 Bit 0 - is strong (after doing exercise with the Hunk-o-matic)
*/
.label actor_vars = $fee3


//===========================================================
// UI 
//===========================================================
//Control mode values
//#00 - cutscene
//#01 - push only, verb forcefully set to "push" (no walking possible) - used for keypad interaction and start screen
//#02 - normal play, disable kid change via Function keys
//#03 - normal play, enable kid change via Function keys
.label control_mode = $fea8

.label talking_costume = $fe9a
.label msg_owner_costume = $fe9b	//#FF means nobody's talking (and thus the msg bar must be cleared)
						//Otherwise, it has the same value as talking_costume
//Base address for message bar source buffers
.label source_msg_base = $0200

//===========================================================
// Sentence engine
//===========================================================
.label sentstk_top_idx = $fe17
.label sentstk_free_slots = $fe18	
.label current_verb_id = $fe1f
.label direct_object_idx_lo = $fe20
.label direct_object_idx_hi = $fe21
.label current_preposition = $fe22
.label indirect_object_idx_lo = $fe23
.label indirect_object_idx_hi = $fe24
.label sentence_parts = $fe1f
.label inventory_objects = $7031
.label room_obj_idx_hi = $7221
.label room_obj_idx_lo = $723d

//===================================================
// Script engine
//===================================================
.label saved_task_idx  = $FEA6    	
.label task_cur_idx = $70B8	//Script slot of script being executed
.label task_pc_ofs_hi_tbl = $70B9
.label task_pc_ofs_lo_tbl = $70C9
//Script states
//Bit 7 set = frozen for cutscene
//#00	Inactive (no script)
//#01	Paused
//#02	Running
.label task_state_tbl = $70D9
.label task_pause_counter_1 = $70e9
.label task_pause_counter_2 = $70f9
.label task_pause_counter_3 = $7109
//Script types
//#00 = object script
//#01 = room script
//#02 = global script
.label task_type_tbl = $7119
.label task_script_idx_tbl = $7129
.label script_2_mem_attr = $7ad9
.label opcode = $fe8c
.label cur_script_slot = $fe89			//Script engine's slot index for script being executed
.label script_base_lo = $fe8a			//Resource base address of script being executed
.label script_base_hi = $fe8b
//Read pointer of the current script
.label task_pc_lo = $5e15
.label task_pc_hi = $5e16
.label interrupted_pc_lo = $fea4
.label interrupted_pc_hi = $fea5
.label interrupted_script_index = $fea7

//===========================================================
// Sound engine
//===========================================================
.label sid_volfilt_reg_shadow = $476A
.label voice_adsr_attack_decay = $476c
.label voice_adsr_sustain_release = $476f
.label sound_id_to_start_on_voice = $4772
.label voice_data_base_lo = $4779
.label voice_data_base_hi = $4780
.label voice_data_offsets_lo = $4787
.label voice_data_offsets_hi = $478e
.label voice_priority_0 = $4795
.label voice_priority_1 = $4796
.label voice_priority_2 = $4797
.label voice_priority_3 = $4798
.label voice_priority_4 = $4799
.label voice_priority_5 = $479A
.label voice_priority_6 = $479B
.label voice_sound_id_tbl = $479c
.label rsrc_for_voice_1 = $479d
.label rsrc_for_voice_2 = $479e
.label rsrc_for_voice_3 = $479f
.label rsrc_for_voice_4 = $47a0
.label rsrc_for_voice_5 = $47a1
.label rsrc_for_voice_6 = $47a2
.label voice_instr_header = $47a3
.label voice_instr_repcount = $47aa
.label voice_ctrl_shadow = $47b1
.label voice_instr_pc_lo = $47b8
.label voice_instr_pc_hi = $47bf
.label voice_freq_lo = $47c6
.label filter_cutoff_freq_lo = $47c9
.label voice_freq_hi = $47cd
.label filter_cutoff_freq_hi = $47d0
.label voice_duration_lo = $47d4
.label voice_duration_hi = $47db
.label voice_gliss_lo = $47e2
.label voice_gliss_hi = $47e9
.label voice_base_addr_lo = $47f0
.label voice_base_addr_hi = $47f7
.label voice_instr_loop_ofs_lo = $47fe
.label voice_instr_loop_ofs_hi = $4805
.label voice_instr_active_mask = $480c	//Bitmask that holds which voices are currently executing sound instructions
.label voice_sid_range_flags = $4811	//#00 if X < 3, #FF otherwise
.label music_voices_in_use = $4817
.label music_voices_in_use_2 = $4818		//A bitmask used to represent which voices are being used by music
.label stop_sound_cleanup_mode = $481A				//#01 = stop, dereference sound, stop arpeggio, #FF = just stop
.label tracked_lowest_priority_sound = $4cf1
.label filter_control_snapshot = $4cfa
.label total_real_voices_available = $4cff
//Values observed: #01, #02, #07, #0A, #64, #6E, #73, #78
// 00000001		#01		14 times	Sink water, Shower water, Radio untuned, Dial tone, tentacle noise, 
//									Reactor alarm 1, Scratched record (x2), Typewriter, Glass-breaking noise,
//									Zom-b-matic, Meteor angry, Meteor happy, Meteor panicked
// 00000010		#02		2 times		Hunk-o-matic up/down
// 00000111		#07		6 times		Piano music (x2), Main theme - short version (x2), Intro music (x2)
// 00001010		#0A		2 times		Crickets, Comet burn
// 01100100		#64		35 times - Door open, close, Piggy bank break, Coin inserted, Light switch, Lab door open
//								Microwave bell, Microwave rotation, Hamster explosion, Keypad beep, Busy tone, Edna phone ring, 
//								Library phone ring, Window glass crack, Alien, Pendulum clock, Radio being tuned, Reactor alarm 2,
//								Water flow, Water valve, Unused melody, Tape being stopped, Select kid, Comet crash, Meltdown countdown, Unknown, 
//								Shower curtain open, Videogame, Tentacle blocking, Meteor police, Door bell, Garage door open, 
//								Car rocket ignition, Car rocket liftoff, Reactor explosion

// 01101110		#6E		1 time		Radiation
// 01110011		#73		1 time		Close steel door
// 01111000		#78		1 time		Zom-b-matic shutdown

.label sound_priority = $4d01		
.label pending_sound_idx = $4d05
.label voices_allocated = $4d07	//Bitmask that holds which voices are currently allocated (not necessarily actively executing sound instructions)
.label sound_to_stop = $4d08

//Combine multiple notes in one voice by arpeggio
.label filter_mode_mask = $5166

.label alt_voice_adsr_attack_decay = $5168
.label alt_voice_adsr_sustain_release = $516a
.label alt_voice_data_ptrs_lo = $516c
.label alt_voice_data_ptrs_hi = $516f
.label alt_voice_ctrl_shadow = $517e
.label alt_voice_freq_lo = $5187
.label alt_voice_freq_hi = $518a
.label alt_voice_duration_lo = $518d
.label alt_voice_duration_hi = $5190
.label alt_voice_gliss_lo = $5193
.label alt_voice_gliss_hi = $5196
.label alt_voice_priority_0         = $5172  // Alternate priority for voice slots 0–2 (indexed by Y)
.label alt_voice_sound_id_tbl       = $5175  // Alternate sound ID for voice slots 0–2 (indexed by Y)
.label alt_voice_instr_header       = $5178  // Alternate instruction header byte for voice slots 0–2
.label alt_voice_instr_repcount     = $517B  // Alternate instruction repetition count for voice slots 0–2
.label alt_voice_instr_pc_lo        = $5181  // Alternate instruction PC low byte for voice slots 0–2
.label alt_voice_instr_pc_hi        = $5184  // Alternate instruction PC high byte for voice slots 0–2
.label alt_voice_base_addr_lo       = $5199  // Alternate base address low byte for voice slots 0–2
.label alt_voice_base_addr_hi       = $519C  // Alternate base address high byte for voice slots 0–2
.label alt_voice_instr_loop_ofs_lo  = $519F  // Alternate loop offset low byte for voice instruction for slots 0–2
.label alt_voice_instr_loop_ofs_hi  = $51A2  // Alternate loop offset high byte for voice instruction for slots 0–2
.label inlined_music_address = $48d5
.label inlined_music_ptr = $53cb          

.label music_to_start_ptr_lo = $FEA2
.label music_to_start_ptr_hi = $FEA1
.label music_in_progress_ptr_lo = $547C
.label music_in_progress_ptr_hi = $547D
.label music_track_offsets_lo = $547E
.label music_track_offsets_hi = $5487
.label music_track_ptrs_lo = $5490
.label music_track_ptrs_hi = $5499
.label selected_music_idx = $fea3
.label voice_read_ptr            = $BA      // ZP pointer to current voice script/parameter stream
.label filter_control_reg_copy   = $476B    // Shadow copy of SID filter control register (software mirror)
.label x_saved                   = $480D    // Scratch storage for last logical voice index processed
.label voice_active_mask_snapshot = $3465

