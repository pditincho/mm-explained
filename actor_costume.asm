#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "actor_sprites.asm"
#import "walkbox_waypoint_planner.asm"
#import "rsrc_mgmt.asm"

/*
================================================================================
  detach_actor_from_costume
================================================================================
Summary
	Releases the link between an actor and its assigned costume.
	
	The actor’s last known position is stored in the costume’s destination
	fields, its visibility and mouth states are cleared, the mutual mapping
	tables are reset, and the costume’s standing animation is updated to match
	the actor’s facing direction. Finally, sprite assignments are refreshed to 
	reflect the change.

Arguments
	X   										costume index to unassign

Global Inputs
	actor_for_costume[]           				actor index mapped to each costume
	actor_x/y_pos[]				  				actor position tables
	actor_cur_facing_direction[]    			current facing direction bitmask

Global Outputs
	costume_target_x/y[]					 		snapshot of actor’s last position
	actor_visible[]								set to OFFSCREEN
	actor_mouth_state[]  						set to MOUTH_CLOSED
	actor_for_costume[]							unlinked
	costume_for_actor[]  						unlinked
	costume_clip_set[]  						updated standing clip ID
	sprite assignments  						recalculated by sync_sprites_with_actors

Description
	1. Look up the actor currently assigned to costume X.
	2. Copy the actor’s X/Y position into the costume’s destination slots.
	3. Clear visibility and mouth animation (actor no longer shown).
	4. Break the mapping between actor and costume.
	5. Convert the actor’s direction bitmask into a standing clip ID and
	store it as the costume’s current animation set.
	6. Call sync_sprites_with_actors to reconcile sprite ownership.

Notes
	The routine does not free memory or modify actor resources, only logical
	mappings and runtime state. It effectively “parks” the costume for later
	reuse or teleportation.
================================================================================
*/
* = $38E4
detach_actor_from_costume:
		// Resolve actor for costume
		lda     actor_for_costume,x              
		tay                                     

		// ------------------------------------------------------------  
		// Snapshot actor’s current position into the costume record  
		//  
		// Used so when the costume gets re-assigned later, it reappears  
		// where the actor was last located.  
		// ------------------------------------------------------------  
		lda     actor_pos_x,y                    
		sta     costume_target_x,x                 
		lda     actor_pos_y,y                    
		sta     costume_target_y,x                 

		// Set actor offscreen
		lda     #ACTOR_OFFSCREEN                 
		sta     actor_visible,y     
		
		// Set mouth state to CLOSED
		sta     actor_mouth_state,y              // mouth state := closed  

		// ------------------------------------------------------------  
		// Break costume↔actor mapping  
		// ------------------------------------------------------------  
		lda     #NO_ACTOR                        
		sta     actor_for_costume,x              // unassign actor from costume  
		sta     costume_for_actor,y              // unassign costume from actor  

		// ------------------------------------------------------------  
		// Store standing clip set based on actor's facing direction  
		//  
		// Converts the actor’s facing direction into its equivalent  
		// standing clip ID and stores it in the costume entry.  
		// ------------------------------------------------------------  
		lda     actor_cur_facing_direction,y       
		jsr     map_facing_direction_to_standing_clip  
		sta     costume_clip_set,x               

		// Sync sprites to reflect the change in the actor list
		jsr     sync_sprites_with_actors  
		rts                                     
/*
================================================================================
  assign_costume_to_free_actor
================================================================================
Summary
	Searches for an unassigned actor slot and initializes it for the currently
	active costume. If no actor slot is free, the routine falls through without
	performing any assignment. This is an implementation bug.

Global Inputs
	current_room               current active room ID
	costume_for_actor[]        table mapping actors to assigned costumes

Global Outputs
	actor_for_costume[]        updated when a free actor is found
	costume_for_actor[]        updated with the new costume–actor mapping
	actor state tables         initialized by initialize_actor_for_costume

Description
	- If no valid room context exists, exit.
	- Otherwise, scan all actors:
		• If costume_for_actor[Y] == FF, the slot is free.
		• Call initialize_actor_for_costume to configure it for the
		currently active costume, then exit.
		• If all actor slots are occupied, it _should_ return with an error,
		but in reality it falls through (which is a bug).
================================================================================
*/
* = $3911
assign_costume_to_free_actor:
		// Is there a valid current room? If not, exit
		lda     current_room                    
		bne     begin_scan_free_actor           
		rts                                     

begin_scan_free_actor:
		// Use Y as actor index
		ldy     #$00                            // Y := 0 (start with actor #0)

find_free_actor_slot:
		// Is there a costume assigned to this actor? If so, skip it
		lda     costume_for_actor,y             
		bpl     advance_actor_scan_or_exit      

		// ------------------------------------------------------------  
		// Found a free actor slot: initialize and assign it  
		//  
		// Calls initialize_actor_for_costume to set up actor state,  
		// coordinates, and links to the active costume.  
		// ------------------------------------------------------------  
		jsr     initialize_actor_for_costume    
		rts                                     

advance_actor_scan_or_exit:
		// Next actor
		iny                                     
		cpy     #ACTOR_COUNT_TOTAL              
		bne     find_free_actor_slot            

		// ------------------------------------------------------------  
		// Fallback: no available actor found - BUG (it shouldn't fall through)
		// ------------------------------------------------------------  
/*
================================================================================
  initialize_actor_for_costume
================================================================================
Summary
	Link an actor to a costume, snap its spawn position into a valid walkbox,
	ensure the costume resource is ready, and initialize all motion, path, and
	animation state before applying the initial clip.

Arguments
	X       						Costume index to bind
	Y       						Actor index to bind

Global Inputs
	costume_target_x/y[]             	Desired spawn X for each costume (room-space)
	current_room                 	Active room id for walkbox context
	nearest_box_index            	Walkbox index chosen by prior proximity search
	costume_clip_set[]           	Default clip set id per costume

Global Outputs
	actor                        	Updated to bound actor index
	active_costume               	Set to the bound costume index
	costume_for_actor[]          	Actor → costume mapping established
	actor_for_costume[]          	Costume → actor mapping established
	costume_target_x/y[]				Updated to snapped spawn coordinates
	actor_x/y_pos[] 				Actor current position set to snapped coords
	actor_x/y_dest[]				Destination position initialized to snapped coords
	actor_cur_waypoint_x/y[]			Active waypoint seeded to snapped coords
	actor_mouth_state[]          	Mouth state reset (closed)
	active_costume_rsrc_lo/hi[]		Cleared stale costume resource pointers
	actor_current_clip[]			Initialized to inactive clip set
	actor_target_clip[]				Initialized to inactive clip set	
	limb_current_cel_seq[]			All limbs initialized to inactive cel sequences
	limb_target_cel_seq[]			All limbs initialized to inactive cel sequences
	limb_tgt_loop_cnt[]          	Limb loop counters cleared
	actor_motion_state[]         	Motion set to "stopped"
	actor_anim_counter[]         	Animation counter seeded to 1
	actor_search_depth[]         	Search depth reset to sentinel
	actor_path_update_flag[]     	Flag set to request path update
	actor_box_cur[]              	Current walkbox index seeded from nearest_box_index
	actor_box_prev[]             	Previous walkbox index seeded from nearest_box_index
	actor_target_box[]      	Destination walkbox index seeded from nearest_box_index
	target_clip_id               	Initial target clip id set from costume_clip_set
	clip_loop_cnt                	Initial clip loop count set to 1

Description
	• Establish bidirectional links between the specified actor and costume and
	cache the costume index as active_costume.
	• Snap costume’s desired destination coordinates to the nearest valid walkbox 
	and write the snapped values back into both costume and actor position/waypoint 
	tables.
	• Reset the actor’s mouth state, ensure the costume resource is cached,
	clear stale resource pointers, and run costume setup to parse graphics
	and animation metadata.
	• Initialize actor clips and all limb cel sequences to inactive values,
	then seed motion, loop counters, and animation timer for a clean start.
	• Initialize pathfinding-related state (search depth, update flag, and
	current/previous/destination walkboxes) based on nearest_box_index and
	freshly loaded walkbox data.
	• Apply the costume’s default clip as the initial target clip with a
	single loop, so the actor appears in a valid pose immediately after
	binding.
================================================================================
*/

* = $3927
initialize_actor_for_costume:
		// ------------------------------------------------------------
		// Bind actor and costume together
		//
		// Establish bidirectional mapping between the actor and costume.
		// X = costume index, Y = actor index.
		// ------------------------------------------------------------
		txa                                      // A := costume index
		sta     costume_for_actor,y              // link costume → actor
		sty     actor                            // actor := Y (cache for later)
		tya                                      // A := actor index
		sta     actor_for_costume,x              // link actor → costume
		stx     active_costume                   // active_costume := X

		// ------------------------------------------------------------  
		// Snap coordinates to nearest valid walkbox  
		//  
		// - Copy the costume’s destination coordinates and current room  
		// into the walkbox snapping inputs (test_x/test_y).  
		// - Aligns input coordinates to a valid walkable area.  
		// - Restores X and Y afterward to resume proper table indexing.  
		// ------------------------------------------------------------  
		lda     costume_target_x,x                 // A := destination X  
		sta     test_x                           // store as snap X input  
		lda     costume_target_y,x                 // A := destination Y  
		sta     test_y                           // store as snap Y input  
		lda     current_room                     // A := current room index  
		sta     walkbox_room                     // define walkbox context  
		jsr     snap_coords_to_walkbox           // clamp to walkbox limits  
		ldx     active_costume                   // restore costume index  
		ldy     actor                            // restore actor index  

		// ------------------------------------------------------------  
		// Write snapped coordinates to costume and actor tables  
		//  
		// Synchronizes costume and actor position, destination,  
		// and active waypoint data.  
		// ------------------------------------------------------------  
		lda     test_x                           
		sta     costume_target_x,x                 
		sta     actor_pos_x,y                    
		sta     actor_target_x,y                   
		sta     actor_cur_waypoint_x,y             
		
		lda     test_y                           
		sta     costume_target_y,x                 
		sta     actor_pos_y,y                    
		sta     actor_target_y,y                   
		sta     actor_cur_waypoint_y,y             

		// ------------------------------------------------------------  
		// Reset mouth animation state  
		// ------------------------------------------------------------  
		lda     #MOUTH_STATE_CLOSED              
		sta     actor_mouth_state,y              

		// ------------------------------------------------------------  
		// Ensure costume resource is cached 
		// ------------------------------------------------------------  
		jsr     rsrc_cache_costume     
		
		// Clears any stale resource pointer afterward
		ldx     active_costume                   
		lda     #$00  
		sta     active_costume_rsrc_lo,x         
		lda     #$00  
		sta     active_costume_rsrc_hi,x         

		// ------------------------------------------------------------  
		// Setup costume rendering and animation metadata  
		// ------------------------------------------------------------  
		jsr     setup_costume_for_actor          

		// ------------------------------------------------------------  
		// Initialize clips and limb cel sequences
		// ------------------------------------------------------------
		// Set current and target clips to inactive
		ldy     actor                            
		lda     #INACTIVE_CLIP_SET               
		sta     actor_current_clip,y             
		sta     actor_target_clip,y              

		// Compute Y := limb base offset = actor index * 8
		lda     actor                            
		asl                                      
		asl                                      
		asl                                      
		tay                                      
		
		// X := limb slot index
		ldx     #MAX_LIMB_IDX                    
init_each_limb_base_cel:
		// Set current and target cel sequences to inactive
		lda     #INACTIVE_CEL_SEQ                
		sta     limb_current_cel_seq,y           
		sta     limb_target_cel_seq,y            
		
		// Next limb slot
		iny                                      
		dex                                      
		bpl     init_each_limb_base_cel          

		// Restore actor/costume indices  
		ldy     actor                            
		ldx     active_costume                   

		// ------------------------------------------------------------  
		// Initialize motion state and animation loop/counter
		// ------------------------------------------------------------  
		// Set actor's motion to "stopped"
		lda     #MOTION_STOPPED_CODE      
		sta     actor_motion_state,y      
		
		// Init target loop count and animation counter
		lda     #$00  
		sta     limb_tgt_loop_cnt,y              
		lda     #$01  
		sta     actor_anim_counter,y             

		// ------------------------------------------------------------  
		// Initialize pathfinding 
		// ------------------------------------------------------------  
		// Init walkbox search depth
		lda     #$FF  
		sta     actor_search_depth,y             

		// Mark pathfinding update needed
		lda     #TRUE  
		sta     actor_path_update_flag,y 
		

		// ------------------------------------------------------------  
		// Load walkbox data for this costume  
		//  
		// Retrieves collision/movement boundaries and determines  
		// the actor’s box placement.  
		// ------------------------------------------------------------  
		lda     nearest_box_index                // current valid walkbox index  
		sta     actor_box_cur,y                  // set current box  
		sta     actor_box_prev,y                 // set previous box  
		sta     actor_target_box,y          // set destination box  
		ldx     actor                            
		jsr     get_walkboxes_for_costume        // load walkbox tables  
		jsr     begin_box_scan                   // resolve current box  

		// ------------------------------------------------------------  
		// Apply initial animation clip  
		// ------------------------------------------------------------  
		ldx     active_costume                
		
		// Set current clip set as target clip
		lda     costume_clip_set,x               
		sta     target_clip_id                   
		
		// Set one loop for this clip
		lda     #$01                             
		sta     clip_loop_cnt                    
		
		// Assign animation clip
		jsr     assign_clip_to_costume           
		rts                                      
