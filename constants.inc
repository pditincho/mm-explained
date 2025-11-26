#importonce

// Generic booleans
// There are two boolean conventions used in this codebase:
//   • $01 represents “true” for standard logic flags
//   • $FF represents “true” for 'other' logic flags
// To make this distinction explicit, all $FF-true flags are given a “_b” suffix.
.const FALSE                       = $00    // Logical false value for _all_ boolean flags
.const TRUE                        = $01    // Logical true value for unsuffixed boolean flags
.const BTRUE       		           = $FF    // Logical true value for "_b"-suffixed boolean flags

// Generic signal set/clear
.const SIGNAL_SET                  = $01    // Generic “signal asserted” value
.const SIGNAL_CLEAR                = $00    // Generic “signal cleared” value
.const LOCK_SET                    = $01    // Lock engaged / acquired
.const LOCK_CLEAR                  = $00    // Lock released / available

// Generic pointers and masks
.const PTR_NULL                    = $00    // Null pointer sentinel
.const MSK_LOW_NIBBLE              = $0F    // Mask bits 0–3, clear bits 4–7
.const MSK_HIGH_NIBBLE             = $F0    // Mask bits 4–7, clear bits 0–3
.const MSK_LOW7BITS                = $7F    // Mask bits 0–6, clear bit7
.const MASK_BIT7				   	= %1000_0000
.const MASK_BIT6				   	= %0100_0000
.const MASK_BIT5				   	= %0010_0000
.const MASK_CLEAR_BIT7				= %0111_1111
.const MASK_CLEAR_BIT6				= %1011_1111
.const MASK_CLEAR_BIT5				= %1101_1111

// Block header layout (4-byte header + payload)
.const MEM_HDR_OFS_SIZE_LO 			= $00   // Low byte of block size (total, including header)
.const MEM_HDR_OFS_SIZE_HI 			= $01   // High byte of block size
.const MEM_HDR_OFS_NEXT_LO 			= $02   // Low byte of pointer to next block
.const MEM_HDR_OFS_NEXT_HI 			= $03   // High byte of pointer to next block
.const MEM_HDR_LEN     				= $04   // Total header length

// I/O mapping presets for $01 (cpu_port) 
.const MAP_IO_OUT                 	= $24   // Map RAM at $D000–$DFFF
.const MAP_IO_IN                   	= $25   // Map I/O registers at $D000–$DFFF

// Printable message strings
.const DISK_ERROR_MSG 			    = $3B81
.const SIDE_ID_MSG        			= $3B59 // “Put in side # and push joystick button.” (in RAM)

// Resource types
.const RSRC_TYPE_OBJECT          	= $01   // Object resource (clickable world or inventory entity)
.const RSRC_TYPE_COSTUME         	= $02   // Costume resource (actor graphics/animation data)
.const RSRC_TYPE_ROOM            	= $03   // Room resource (geometry, object list, scripts)
.const RSRC_TYPE_ROOM_LAYERS    	= $04   // Room layers resource (color/mask/composite data)
.const RSRC_TYPE_SCRIPT          	= $05   // Script resource (bytecode instructions)
.const RSRC_TYPE_SOUND           	= $06   // Sound resource (SFX or music data)

// Resource locking
.const RSRC_LOCK_MASK				= MASK_BIT7
.const RSRC_CLEAR_LOCK_MASK			= MASK_CLEAR_BIT7

// Subresource types
.const RSRC_INDEX_GFX_LAYERS     	= $00   // Subresource index → graphics layer table
.const RSRC_INDEX_MASK_LAYER     	= $01   // Subresource index → mask/collision layer

// Per-resource totals
.const ROOM_MAX_INDEX      		 	= $36   // Highest valid room resource index (0–$36 inclusive)
.const COSTUME_MAX_INDEX   			= $18   // Highest valid costume resource index (0–$18 inclusive)
.const SCRIPT_MAX_INDEX    			= $9F   // Highest valid script resource index (0–$9F inclusive)
.const SOUND_MAX_INDEX     			= $45   // Highest valid sound resource index (0–$45 inclusive)
.const FIRST_NON_KID_INDEX          = $08   // Recipient index ≥ $08 → not a kid
.const COSTUME_ID_PLANT          	= $13   // costume id for plant (skip mirroring)

// Viewport dimensions
.const VIEW_ROWS 					= $11   // Total number of visible rows in the viewport (17 rows)
.const VIEW_COLS 					= $28   // Total number of visible columns in the viewport (40 columns)
.const VIEW_COL_MAX_IDX  			= $27   // Viewport width minus one (for rightmost inclusive bound)
.const VIEW_HALF_SPAN         		= $14   // Half-width of visible viewport in world coordinates

// Scroll render flag
.const SCROLL_RIGHT_FLAG    		= $00   // right scroll
.const SCROLL_LEFT_FLAG     		= $FF   // left scroll
.const SCROLL_INIT_FLAG     		= $01   // initial full draw

// Costume defaults
.const COSTUME_DFLT_X_DEST  		= $14   // Default X destination for actor when costume is initialized
.const COSTUME_DFLT_Y_DEST  		= $3C   // Default Y destination for actor when costume is initialized
.const COSTUME_HOLDING_ROOM 		= $2C   // Room index used as holding area for inactive or hidden costumes

// Object ID hi
.const OBJ_HI_MOVABLE           = $00    // Object hi-id type tag: movable (ownership applies)
.const OBJ_HI_IMMOVABLE         = $01    // Object hi-id type tag: immovable (in a room)
.const OBJ_HI_COSTUME           = $02    // Object hi-id type tag: costume/character

// Compression
.const COMP_DICT_MAX_OFS 			= $03	// Max offset of a compression dictionary

// Camera 
.const CAM_DEFAULT_POS        		= $C8   // Default horizontal camera position at room entry
.const CAM_PAN_THRESHOLD      		= $14   // Threshold before triggering camera panning
.const CAM_MODE_FOLLOW_ACTOR 		= $02
.const CAM_MODE_PAN 				= $03

// Screen clamp
.const SCREEN_Y_MAX                	= $91   // Last visible Y coordinate in pixels (bottom of viewport)

.const TILE_DEFS_ADDR 				= $D800
.const TILE_DEFS_SIZE 				= $0800
.const MASK_LAYER_COL_0         	= $6AE1 // MASK  layer column 0 base
.const COLOR_LAYER_COL_0        	= $6D89 // COLOR layer column 0 base

// Actor state bitmasks
.const ACTOR_IS_INVISIBLE 			= $80
.const ACTOR_IS_FROZEN 				= $40
.const MOTION_STOPPED_CODE          = $02   // motion state value: actor stopped
.const ACTOR_STATE_STOPPED        	= $02   // Actor not moving

// Room index
.const ROOM_IDX_START 				= $00	// Kid Select screen

// Costume index
.const COSTUME_INDEX_DAVE            = $03    

/*
Actor render flags
===========================================================
Only 2 bits are used, bit 5 (#20) and bit 0 (#01).

	Bit 5 		set - VISIBLE, draw the actor in the main loop
				clear - NOT VISIBLE, ignore actor in main loop
	Bit 0 		set - REFRESH, refresh actor's cels
*/
.const ACTOR_RENDER_VISIBLE			= $20   
.const ACTOR_RENDER_REFRESH         = $01   
.const ACTOR_RENDER_VISIBLE_AND_REFRESH = ACTOR_RENDER_VISIBLE | ACTOR_RENDER_REFRESH   
.const ACTOR_RENDER_CLEAR_REFRESH   = $FE    
.const ACTOR_RENDER_CLEAR_VISIBLE   = $DF


// Actor mouth states
.const MOUTH_STATE_CLOSED          	= $00   // idle: mouth closed (default)
.const MOUTH_STATE_SPEAK_CLOSED    	= $01   // speaking cycle: frame with mouth closed
.const MOUTH_STATE_SPEAK_OPEN      	= $FF   // speaking cycle: frame with mouth open
.const MOUTH_STATE_HELD_OPEN       	= $80   // forced open until explicitly closed
.const MOUTH_TOGGLE_MASK          = $FE    // EOR mask to toggle mouth states #$01 <-> #$FF

.const OBJ_NAME_OFS              	= $0D   // Byte offset to name pointer within object record
.const OBJ_TYPE_COSTUME            	= $02   // target_obj_hi value for “actor”
.const NO_PARENT_IDX                = $00   // parent index value meaning “no parent”

.const OWNER_NIBBLE_MASK         	= MSK_LOW_NIBBLE  // Mask to extract owner nibble from attributes

.const ENTITY_NONE                	= $00   // No destination entity selected/active

// Costume attributes
.const ATTR_REMOVED_FROM_ROOM_MASK  = $20   // object_attributes bit5: removed from room
.const ATTR_INSIDE_PARENT_MASK      = $80   // object_attributes bit7: inside parent flag
.const COSTUME_ATTR_NO_MOUTH_B7   	= $80   // bit7: 1 = do NOT animate mouth (main loop); 0 = animate
.const COSTUME_ATTR_NO_MOUTH_B6   	= $40   // bit6: 1 = do NOT animate mouth (prepare_message_for_topbar); 0 = animate
.const COSTUME_ATTR_STEP_SFX_MASK 	= $3F   // bits5..0: footsteps sound ID

// Movement direction bitmasks
.const DIRECTION_LEFT  				= $01   // left movement direction code
.const DIRECTION_RIGHT 				= $00   // right movement direction code
.const DIRECTION_DOWN  				= $80   // down movement direction code
.const DIRECTION_UP    				= $81   // up movement direction code

// Actor rendering engine totals
.const MAX_LIMB_IDX        			= $07   // Highest limb index (0–7, 8 limbs total per actor)
.const ACTOR_COUNT_TOTAL   			= $04   // Total number of actor slots (0–3)
.const ACTOR_MAX_INDEX     			= ACTOR_COUNT_TOTAL - 1    // Highest valid actor index (#$03)
.const SPRITE_COUNT_TOTAL  			= $04   // Total number of hardware sprites available (0–3)
.const SPRITE_MAX_INDEX    			= SPRITE_COUNT_TOTAL - 1   // Highest valid sprite index (#$03)

.const NO_SPRITE                 	= $FF   // Sentinel: no sprite assigned


// Animation clip IDs
.const CLIP_WALK_LEFT              	= $00   // Walking clip id: left
.const CLIP_WALK_RIGHT             	= $01   // Walking clip id: right
.const CLIP_WALK_DOWN              	= $02   // Walking clip id: down
.const CLIP_WALK_UP                	= $03   // Walking clip id: up
.const CLIP_STAND_LEFT             	= $04   // Standing clip id: facing left
.const CLIP_STAND_RIGHT            	= $05   // Standing clip id: facing right
.const CLIP_STAND_DOWN             	= $06   // Standing clip id: facing down
.const CLIP_STAND_UP               	= $07   // Standing clip id: facing up

// Colors
.const COLOR_BLACK                 	= $00   // Background color 0 = black

//CC00-CECF - graphics area (including parts not visible)
.const SENTENCE_BAR_BASE 			= $ced0

// Verbs
.const VERB_NONE					= $00
.const VERB_OPEN 					= $01
.const VERB_CLOSE 					= $02
.const VERB_GIVE 					= $03
.const VERB_TURN_ON 				= $04
.const VERB_TURN_OFF 				= $05
.const VERB_FIX 					= $06
.const VERB_NEW_KID 				= $07
.const VERB_UNLOCK 					= $08
.const VERB_PUSH 					= $09
.const VERB_PULL 					= $0A
.const VERB_USE 					= $0B
.const READ_VERB 					= $0C
.const VERB_WALK_TO 				= $0D
.const VERB_PICK_UP 				= $0E
.const VERB_WHAT_IS 				= $0F

// Prepositions
.const PREPOSITION_NONE_NEEDED 		= $00
.const PREPOSITION_WITH 			= $02
.const PREPOSITION_TO 				= $04

// Control modes
.const CONTROL_MODE_CUTSCENE      = $00    // Cutscene / restricted UI mode
.const CONTROL_MODE_KEYPAD        = $01    // Keypad entry mode
.const CONTROL_MODE_DISABLE_KID   = $02    // Normal play but kid switching disabled
.const CONTROL_MODE_NORMAL        = $03    // Normal play mode

// Lights_status values
.const LIGHTS_DARKNESS 				= $00
.const LIGHTS_FLASHLIGHT_ONLY 		= $01
.const LIGHTS_ENVIRONMENT_ON 		= $02

// Game Disk Identifiers
.const GAME_DISK_ID_SAVEGAME   		= $30   // Disk ID for saved games
.const GAME_DISK_ID_SIDE1   		= $31   // Valid game tag: side 1
.const GAME_DISK_ID_SIDE2   		= $32   // Valid game tag: side 2
.const DISK_SIDE_ID_BASE        	= $30    // Base offset: side_id ($31/$32 → 1/2)

// Sentence stack
.const SENT_STACK_EMPTY_IDX        	= $FF   // Token stack sentinel “empty”
.const SENT_STACK_MAX_TOKENS       	= $06   // Max tokens in sentence stack
.const SENT_STACK_LAST_INDEX       	= SENT_STACK_MAX_TOKENS - 1   

// Text printing
.const SPACE_CHAR              		= $20   // Space character
.const WORD_HARD_STOP           	= $40   // In-word hard stop token; forces early termination
.const WORD_TERMINATOR          	= $00   // End-of-string marker for word/text data

// Joystick bitmasks
.const JOY1_FIRE_MASK              	= $10   // Joystick fire button bit mask (AND result nonzero → pressed)

// VIC-II control register presets
//----------------------------------------------------------------
//Values used throughout the game for the vic_screen_control_reg_1
//----------------------------------------------------------------
// #00	game code entry
// #0B	after IRQ handler has exited
// #1B	irq_handler1, init_raster_irq_env
// #1C	irq_handler3
// #1D	irq_handler17
// Blank screen bit cleared/set in print_message_wait_for_button
.const VIC_CTRL_BLANK_0       		= $00   // blank screen, vertical scroll = 0
.const VIC_CTRL_BLANK_3       		= $0B   // blank screen, vertical scroll = 3
.const VIC_CTRL_DEFAULT_1       	= $1B   // 25 rows, vertical scroll = 3
.const VIC_CTRL_DEFAULT_2       	= $1C   // 25 rows, vertical scroll = 4
.const VIC_CTRL_DEFAULT_3       	= $1D   // 25 rows, vertical scroll = 5
.const CTRL1_ROOM_REGION_PRESET  	= $1C   // DEN=1, 25 rows, VSCROLL=4, bitmap/ECM=0
.const CTRL1_INT_REGION_PRESET   	= $1D   // DEN=1, 25 rows, VSCROLL=5 (UI band)
.const CTRL1_BLANK_PRESET 		 	= $0B	// blank, 25 rows, vscroll 3
.const VIC_CTRL1_UNBLANK_MASK      	= $10   // OR-mask to set bit 4 (unblank screen per this routine)
.const VIC_CTRL1_BLANK_MASK    		= $EF   // AND-mask to clear bit 4 (blank screen per this routine)

// vic_screen_control_reg_2
.const CTRL2_ROOM_REGION_PRESET  	= $18   // multicolor ON, 40 cols, HSCROLL=0
.const CTRL2_INT_REGION_PRESET   	= $09   // multicolor OFF, 40 cols, HSCROLL=1

// VIC memory layout ($D018) presets
.const VIC_LAYOUT_C800          	= $26   // Screen base $C800, char base $D800
.const VIC_LAYOUT_CC00          	= $36   // Screen base $CC00, char base $D800
.const VIC_LAYOUT_CC00_F800     	= $3E   // Screen at $CC00, character set at $F800
.const TEXT_CHARSET_LAYOUT      	= $3E   // Screen=$CC00, charset=$F800

// ------------------------------------------------------------
// VIC-II interrupt constants
// ------------------------------------------------------------
.const VIC_IRQ_RASTER_ACK        = $81    // $D019 raster-IRQ ack mask (write 1s; bit7 mirrors)

// Frame buffers base addresses
.const FRAMEBUF1_BASE     			= $C800    
.const FRAMEBUF2_BASE      			= $CC00   
.const FRAMEBUFFER_INIT            = $01      // Initial framebuffer index

// VIC sprites bitmasks
.const SPRITES_ENABLE_ALL          	= $FF   // $D015: enable sprites 0–7
.const SPRITES_DISABLE_ALL         	= $00   // $D015: disable sprites 0–7

// VIC raster lines for IRQs
.const RASTER_IRQ_LINE_INIT        	= $FA   // $D012: raster line 250

// Video setup modes (video_setup_mode)
.const VID_SETUP_NONE           	= $00   // No change; keep current VIC layout
.const VID_SETUP_C800           	= $01   // Char @ $D800, screen @ $C800, sprites @ $CBF8 → copy color RAM
.const VID_SETUP_CC00           	= $02   // Char @ $D800, screen @ $CC00, sprites @ $CFF8 → copy color RAM
.const VID_SETUP_COPY_COLORS    	= $03   // Copy color RAM only, no layout change

//Frame buffers for room viewport
.const VIEW_FRAME_BUF_0        = $C828  // framebuffer 0 base (tile layer matrix)
.const VIEW_FRAME_BUF_1        = $CC28  // framebuffer 1 base (tile layer matrix)
.const VIEW_FRAME_BUF_SIZE     = $02A8  // framebuffer span in bytes

// Shutter modes
.const SHUTTER_CLOSE          = $00     // Mode selector value for closing shutter
.const SHUTTER_OPEN           = $01     // Mode selector value for opening shutter

// Sprite shape base addresses for each layout
.label SPRITE_SHAPE_SET1_ADDR   	= $CBF8 // Sprite shape data base for layout C800
.label SPRITE_SHAPE_SET2_ADDR   	= $CFF8 // Sprite shape data base for layout CC00
.const SPRITE_BASE_0      			= $E000 // VRAM base for sprite bank 0
.const SPRITE_BASE_1      			= $E800 // VRAM base for sprite bank 1

// Script types
.const SCRIPT_TYPE_OBJECT 			= $00
.const SCRIPT_TYPE_ROOM 			= $01
.const SCRIPT_TYPE_GLOBAL 			= $02

// Tasks
.const TASK_TOTAL 					= $10
.const TASK_MAX_INDEX 				= TASK_TOTAL - 1 
.const TASK_IDX_NONE           		= $FF    // No script running (idle slot sentinel)
.const TASK_STATE_INACTIVE    		= $00
.const TASK_STATE_PAUSED    		= $01
.const TASK_STATE_RUNNING    		= $02    // resume state for script slot
.const TASK_MASK_SUSPENDED        = MASK_BIT7
.const TASK_MASK_CLEAR_SUSPENDED  = MASK_CLEAR_BIT7

* = $3BA6
game_paused_flag_msg:
        .byte $47,$61,$6D,$65,$20,$70,$61,$75,$73,$65
        .byte $64,$2C,$20,$70,$72,$65,$73,$73,$20,$53
        .byte $50,$41,$43,$45,$20,$74,$6F,$20,$63,$6F
        .byte $6E,$74,$69,$6E,$75,$65,$2E,$20,$20,$00
        // "Game paused, press SPACE to continue.  "

// ----------------------------------------
// Screen row byte offsets (40-column tile mode)
// ----------------------------------------
// Precomputed 16-bit offsets for the start of each tile row
// within the screen buffer. Row stride = 40 bytes = $28.
//
// Rows: 25 total → indices 0..24.
// offset(row) = row * $28
//   e.g., row 0 → $0000, row 7 → $0118, row 24 → $03C0.
//
// Use: screen_base + screen_row_offsets_{hi,lo}[row]
// to address the first cell of a given row.
* = $3EA1
screen_row_offsets_hi:
.byte $00, $00, $00, $00, $00, $00, $00
.byte $01, $01, $01, $01, $01, $01, $02
.byte $02, $02, $02, $02, $02, $02, $03
.byte $03, $03, $03, $03

* = $3EBA
screen_row_offsets_lo:
.byte $00, $28, $50, $78, $A0, $C8, $F0
.byte $18, $40, $68, $90, $B8, $E0, $08
.byte $30, $58, $80, $A8, $D0, $F8, $20
.byte $48, $70, $98, $C0


.const CPU_PORT_DDR_INIT           = $07      // 6510 DDR: banking bits as outputs


.const  LOGICAL_VOICE_LIMIT       = $03      // Logical voices 0–2 map to real SID voices; X >= 3 is special/filter
.const SOUND_REFCOUNT_BIT        = %0000_0001   // Bit 0 indicates non-zero refcount for a sound
.const MAX_SND_PRIORITY = $7F
.const SOUND_IDX_NONE = $FF
.const MIN_SOUND_PRIORITY = $01
.const STOP_SOUND_MODE_VOICES_ONLY   = $01  // Soft stop: stop voices only, no refcount/arpeggio/alt-state cleanup
.const STOP_SOUND_MODE_FULL_CLEANUP  = $FF  // Hard stop: when stopping the starting sound, also do full cleanup

.const START_SOUND_ERR_NOT_LOADED          = $01  // Sound resource not resident in memory
.const START_SOUND_ERR_MUSIC_ALL_VOICES    = $FE  // Music uses all three real voices; cannot allocate any
.const START_SOUND_ERR_INSUFFICIENT_VOICES = $FF  // Cannot obtain enough voices (availability/priority/voice-3 conflict)
.const MUSIC_IDX_1              = $01    // First preferred music slot
.const MUSIC_IDX_2              = $02    // Second preferred music slot

.const HEAP_FREE_HDR_ADDR       = $7B7B    // Address of initial free block header
.const HEAP_INIT_SIZE_BYTES     = $4C85    // Size of initial free span

.const ROOM_FRONT_OF_MANSION          = $01    // Room where walking sounds are disabled
.const ROOM_MANSION_ROAD_ENTRANCE     = $2C    // Second exterior room with no walking sounds
.const WALK_SND_THROTTLE_RESET        = $02    // Number of calls between walking-sound checks

// Inventory dataset and parsing
.const  INVENTORY_SLOTS           = $2D    // Total logical item slots (45)
.const OWNER_REMOVE_SENTINEL      = $0D    // Special value meaning “remove from inventory”
.const CURSOR_Y_BIAS   = $08          // Bias before halving Y

.const NO_ACTOR                    = $FF      // Sentinel: no actor mapped
.const NO_COSTUME                  = $FF      // Sentinel: actor has no costume
