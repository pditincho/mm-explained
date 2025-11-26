/*
================================================================================
Sentence/action system
================================================================================

Summary
	This module powers the point-and-click “sentence” UX: pick a verb, then
	zero or more complements (direct object, optional preposition, optional
	indirect object). It keeps a small stack of pending sentences, decides when
	to walk first vs act immediately, and dispatches either per-object verb
	handlers or global defaults.
	
Terminology
		DO		Direct object
		IO		Indirect object

Core data

	* Current picks (UI state): current_verb_id, direct_object_idx_{lo,hi},
	  preposition, indirect_object_idx_{lo,hi}.
	* Stack (up to 6): parallel arrays of verb/prep/DO/IO plus sentstk_top_idx
	  (top) and sentstk_free_slots (capacity).
	* “Active” copy: once a stacked sentence is selected to run, its tokens are
	  mirrored into active_* so execution code has a stable snapshot.
	* Destination: target_entity plus dest_{x,y} are used to route walking
	  targets and stage paths. var_destination_{x,y} mirror the clamped target
	  for scripts/debug.

Main loop
	process_sentence:

		1. Optional reset of DO/IO/preposition if init_sentence_ui_flag is set.
		2. If the current kid is incapacitated, restrict verbs to “New kid”.
		3. Control-mode branch:

		   * Cutscene: only try to run completed sentences.
		   * Keypad: force PUSH verb and bypass walking later.
		   * Normal: if verb == “What is?”, set a “forced trigger”.
		4. Forced trigger path:

		   * If verb = GIVE and a preposition is set, pick an actor under cursor
			 as IO; else pick an object under cursor.
		   * Update DO or IO, guarding DO == IO and setting UI-rebuild flags when
			 selections repeat. Always request a sentence-bar refresh.
		5. If verb is Walk to, clear target_entity and mark a rebuild (so UI
		   reflects the bare walk). Then fall into run_sentence_if_complete.

Completion gate
	run_sentence_if_complete:

		* Always refresh the bar first. If a “rebuild needed” flag was set, clear
		  it and continue.
		* If no verb, just refresh again and exit.
		* NEW KID → handle_new_kid_verb.
		* WALK TO → can execute without objects → dispatch_or_push_action.
		* Other verbs:

		  * Require a DO. If missing, refresh and exit.
		  * If a preposition is required and missing, call select_preposition_for_verb.
			If it returns none, we can run. Otherwise store it, mark refresh, and
			exit until the user picks an IO.
		  * If we have a preposition but no IO yet, refresh and exit.
		  * With all parts present → dispatch_or_push_action.

“New kid”
	handle_new_kid_verb:

		* Only in CONTROL_MODE_NORMAL. Cursor X chooses kid0/1/2 by name columns.
		* Refresh UI, call switch_active_kid_if_different, then normalize back to
		  WALK TO and request refresh.

Dispatch vs stack
	dispatch_or_push_action:

		* Reset stack bookkeeping (free slots to max, head to “empty” sentinel).
		* If verb == WHAT IS → do nothing and return.
		* If verb != WALK TO or DO is present → push_sentence:

		  * Advance sentstk_top_idx and store current tokens to stacked_* arrays.
		  * If verb wasn’t WALK TO, reset the UI to WALK TO and clear DO+prep.
		  * Clear target_entity and return.
		* Bare WALK TO (no DO):

		  * Resolve acting entity from current kid and copy cursor coords to dest.
		  * Clamp to walkable space, publish var_destination_{x,y}.
		  * If kid is frozen, stop here.
		  * Otherwise write actor_target_x/y_dest and call stage_actor_path_to_target.

Queued execution
	process_sentence_stack_entry:

		* If a destination is already active, exit. If stack empty, exit.
		* Drop sentences whose DO == IO.
		* Inventory check in priority order:

		  * If either DO or IO is already in current kid’s inventory, proceed.
		  * Else try to push a “Pick up <obj>” sentence for DO, else IO, but
			only if the object has a custom Pick Up handler. If neither can be
			picked up, drop the sentence.
		* Activate the current sentence by copying stacked_* → active_* and pop the
		  stack (with underflow reset protection).
		* Decide walk vs execute:

		  * If DO is in inventory and there is no preposition → execute now.
		  * If a preposition exists, ensure IO is also in inventory; if not, walk
			to IO first.
		  * If DO is not in inventory:

			* In KEYPAD mode: execute now, no walking.
			* Otherwise walk to DO.

Verb execution
	execute_verb_handler_for_object:

		* Refresh bar, resolve the active DO’s resource, and look up a custom
		  handler for active_verb_id via find_object_verb_handler_offset.
		* If none:

		  * GIVE: if recipient is a kid, transfer ownership (owner nibble) and
			refresh inventory; then return.
		  * WALK TO: return.
		  * Else run the global defaults script (#3).
		* Guard: READ requires lights; if dark, fall back to global defaults.
		* If a custom handler exists: compute its script offset from the object’s
		  base, set script read address, and dispatch_script_ops_loop.

Helper scans and guards
	find_object_verb_handler_offset:

		* Scans the object’s handler table starting at +$0E: {verb_id, offset}
		  pairs, terminated by 0. A special “default” id (#$0F) matches any verb.
		  Returns offset on match. Returns #$00 if absent and verb ≠ WALK TO,
		  or #$0D if absent and verb == WALK TO.

	has_pickup_script_for_sentence_part:

		* Select DO or IO from the stacked entry at sentstk_top_idx. Reject actors.
		  Resolve the object and check for a VERB_PICK_UP handler. Returns 1 if
		  present.

	is_sentence_object_owned_by_current_kid:

		* Given DO vs IO selector, read the stacked object. If its hi byte says
		  “in some inventory”, compare the owner nibble against current_kid_idx
		  and return 1 if it matches, else 0.

	push_pickup_for_sentence_part:

		* Append a “Pick Up <obj>” sentence for the selected complement. Validates
		  stack bounds and enters a visible debug hang if overflow occurs.

Kid switching and UI reset
	switch_active_kid_if_different:

		* Change current_kid_idx if needed, stop any running script, recenter the
		  camera, refresh inventory, then fall through to init_sentence_ui_and_stack.

	init_sentence_ui_and_stack:

		* Clear destination, force a bar refresh, reset stack capacity and head,
		  set default verb to WALK TO, and clear DO/prep/IO.

Typical traces

	* “Walk to” click on floor: bare WALK TO path clamps cursor position into a
	  walkable box and stages a path unless frozen.
	* “Use key on door”: ensure DO “key” is in inventory or push a “Pick up
	  key” first. If a preposition selects “with/door” and IO is not in
	  inventory, walk to door first. When ready, execute custom handler or
	  default.
	* “Give coin to kid”: if IO is a kid, ownership is updated and inventory
	  refreshes without running a script.
================================================================================
┌─────────────────────┐
│ process_sentence    │
└──┬──────────────────┘
   │  reset flags, incapacity limits, mode dispatch,
   │  forced “What is?” hit-testing, UI refresh
   │
   ├───────────────▶ (jmp) run_sentence_if_complete
   │
   └─(cursor hit paths)───▶ finalize_and_maybe_execute ... ▶ run_sentence_if_complete

┌──────────────────────────────┐
│ run_sentence_if_complete     │  refreshes bar, clears rebuild flag
└─┬────────────────────────────┘
  │
  ├─· no verb ·───────────────▶ refresh_sentence_bar_trampoline
  │
  ├─· VERB_NEW_KID ·──────────▶ handle_new_kid_verb
  │
  ├─· VERB_WALK_TO ·──────────▶ dispatch_or_push_action
  │
  └─· other verbs ·
        │
        ├─· DO missing ·──────▶ refresh_sentence_bar_trampoline
        │
        └─· DO present ·
              │
              ├─· preposition missing ·──▶ select_preposition_for_verb
              │      ├─ none needed ─────▶ dispatch_or_push_action
              │      └─ inferred prep ───▶ save prep + UI refresh (return)
              │
              └─· preposition present ·
                     ├─· IO missing ·────▶ refresh_sentence_bar_trampoline
                     └─· IO present ·────▶ dispatch_or_push_action

┌──────────────────────────────┐
│ handle_new_kid_verb          │
└─┬────────────────────────────┘
  │ normal mode only: map cursor X to kid0/1/2
  │ refresh UI, switch_active_kid_if_different
  └──────────────▶ set WALK_TO + UI refresh (return)

┌──────────────────────────────┐
│ dispatch_or_push_action      │
└─┬────────────────────────────┘
  │ reset stack bookkeeping
  │
  ├─· VERB_WHAT_IS ·───────────▶ rts
  │
  ├─· WALK_TO + no DO ·────────▶ (bare walk)
  │       set active_costume from current_kid
  │       dest := clamped cursor coords
  │       publish var_destination_{x,y}
  │       if not frozen: set actor_x/y_dest + stage_actor_path_to_target
  │       rts
  │
  └─ otherwise (verb + DO [+prep + IO]) ──▶ push_sentence
          write stacked_* at ++top
          if verb ≠ WALK_TO: reset UI to WALK_TO, clear DO + prep
          clear target_entity, rts

┌──────────────────────────────────┐
│ process_sentence_stack_entry     │
└─┬────────────────────────────────┘
  │ if destination active → rts
  │ if stack empty → rts
  │
  │ (redundancy) if prep and DO==IO → drop entry, rts
  │
  │ (inventory gate)
  │   check DO owned? if not → proceed
  │   check IO owned? if not → proceed
  │   else try scripted pickups in order:
  │        DO: has_pickup? yes→ push_pickup_for_sentence_part → rts
  │        IO: has_pickup? yes→ push_pickup_for_sentence_part → rts
  │        none → drop entry, rts
  │
  │ activate: stacked_* → active_* ; pop; underflow guard
  │
  └─ decide walk vs execute
      ├─ DO in inventory:
      │     ├─ no prep → execute_active_verb
      │     └─ prep present:
      │            IO in inventory? yes → execute_verb_handler_for_object
      │                                  no  → init_walk_to_indirect_object
      │
      └─ DO not in inventory:
            ├─ KEYPAD mode → execute_verb_handler_for_object, clear target_entity, rts
            └─ otherwise    → init_walk_to_direct_object

┌──────────────────────────────┐
│ init_walk_to_indirect_object │
└─┬────────────────────────────┘
  │ destination_obj := IO
  │ set_approach_point → target_x/target_y, target_entity
  │ var_destination_{x,y} := dest_{x,y}
  │ active_costume := current_kid
  │ if not frozen: set_costume_target
  └──────────────▶ exit_process_sentence_stack_entry → rts

┌──────────────────────────────┐
│ init_walk_to_direct_object   │
└─┬────────────────────────────┘
  │ destination_obj := DO
  │ set_approach_point → target_x/target_y, target_entity
  │ var_destination_{x,y} := dest_{x,y}
  │ active_costume := current_kid
  │ if not frozen: set_costume_target
  └──────────────▶ exit_process_sentence_stack_entry → rts

┌────────────────────────────────────┐
│ execute_verb_handler_for_object    │
└─┬──────────────────────────────────┘
  │ mark UI refresh
  │ bind DO’s resource to slot (resolve_object_resource)
  │ A := find_object_verb_handler_offset(active_verb_id)
  │
  ├─ A == 0  (no handler):
  │     ├─ GIVE:
  │     │    if IO is kid → set object owner nibble, refresh_inventory, rts
  │     ├─ WALK_TO: rts
  │     └─ otherwise → launch_global_defaults_script(#3) with var_active_verb_id, rts
  │
  └─ A != 0 (custom handler found):
        if READ and lights off → launch_global_defaults_script
        else:
          compute absolute script offset from room_obj_ofs + A
          seed script state (var_active_io_id_lo, script_offsets_*,
                        set_script_base_from_type,
                        set_current_task_pc)
          dispatch_script_ops_loop, rts

┌────────────────────────────────────┐
│ find_object_verb_handler_offset    │
└────────────────────────────────────┘
  Scan pairs {verb_id, handler_ofs} at object + $0E.
  Terminator verb_id=0 → if requested=WALK_TO return $0D else $00.
  DEFAULT_VERB ($0F) matches any verb.

┌─────────────────────────────────────────────┐
│ has_pickup_script_for_sentence_part         │
└─────────────────────────────────────────────┘
  Select DO or IO from stack top.
  Reject if object type is ACTOR.
  resolve_object_resource → find handler for VERB_PICK_UP.
  Return TRUE/FALSE.

┌─────────────────────────────────────────────┐
│ is_sentence_object_owned_by_current_kid     │
└─────────────────────────────────────────────┘
  Select DO or IO from stack top.
  If hi byte flags “in some inventory,” compare owner nibble to current_kid_idx.
  Return FOUND / NOT_FOUND.

*/
#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "sentence_text.asm"
#import "actor_targeting.asm"
#import "ui_interaction.asm"
#import "misc.asm"
#import "script_engine.asm"
#import "camera.asm"

// Queued sentence parts (parallel LIFO stacks, max depth = 6)
.label stacked_verb_ids        = $fe25    // Stack of verb IDs (index 0..5; top tracked separately)
.label stacked_do_id_lo        = $fe2b    // Stack of direct-object IDs: low byte per entry
.label stacked_do_id_hi        = $fe31    // Stack of direct-object IDs: high byte per entry
.label stacked_prep_ids        = $fe37    // Stack of preposition IDs (one byte per entry)
.label stacked_io_id_lo        = $fe3d    // Stack of indirect-object IDs: low byte per entry
.label stacked_io_id_hi        = $fe43    // Stack of indirect-object IDs: high byte per entry

// Active (dequeued) sentence snapshot
.label active_verb_id          = $fe19    // Active verb ID being processed
.label active_do_id_lo         = $fe1a    // Active direct-object ID: low byte
.label active_do_id_hi         = $fe1b    // Active direct-object ID: high byte
.label active_prep_id          = $fe1c    // Active preposition ID (0 = none)
.label active_io_id_lo         = $fe1d    // Active indirect-object ID: low byte
.label active_io_id_hi         = $fe1e    // Active indirect-object ID: high byte

// Scratch / helpers
.label verb_index              = $0e6f    // Latched verb ID for comparisons/table scans
.label object_rsrc_ptr         = $15      // ZP pointer to object resource base (lo at $15, hi at $16)
.label temp_x_2                = $0e70    // Saves X across calls where X must be preserved
.label temp_y_2                = $0e71    // Saves Y across calls where Y must be preserved
.label object_ptr              = $15      // ZP pointer used for target object index lookups (alias of object_rsrc_ptr)


.const VERB_TABLE_START_OFS       = $0E    // Byte offset in object resource where {verb,ofs} pairs begin
.const VERB_SCAN_SEED_Y           = VERB_TABLE_START_OFS - 2   // Y init so first INY,INY lands at +$0E entry
.const DEFAULT_VERB               = $0F    // Wildcard verb id; matches any verb in handler scan
.const NO_HANDLER_RET             = $00    // find_object_verb_handler_offset: no handler and verb ≠ WALK_TO

.const ARG_IS_DO                  = $01    // Selector A value: operate on direct object
.const ARG_IS_IO                  = $02    // Selector A value: operate on indirect object

.const FORCED_TRIGGER_SET          = $01    // forced_sentence_trigger: set → run sentence immediately
.const FORCED_TRIGGER_CLEARED      = $FF    // forced_sentence_trigger: cleared after handling

.const REBUILD_SENTENCE_ON        = $01    // needs_sentence_rebuild flag value: rebuild now

.const OBJ_IDX_NONE               = $00    // Sentinel lo byte for “no object selected”
.const PREPOSITION_NONE           = $00    // No preposition selected

.const GLOBAL_DEFAULTS_SCRIPT_ID  = $03    // Global “verb defaults” script identifier

.const DEFAULT_VERB_WALK_TO       = VERB_WALK_TO    // Verb id for “Walk to” default case

.const SECOND_KID_NAME_COLUMN     = $0B    // Column threshold for kid #2 selection in UI
.const THIRD_KID_NAME_COLUMN      = $18    // Column threshold for kid #3 selection in UI

/*
================================================================================
  process_sentence
================================================================================

Summary
	Process the current verb sentence for this frame. Optionally reset parts,
	apply incapacity limits, branch by control mode, handle forced-trigger
	selection via cursor hits, then run the completeness gate.

Global Inputs
	init_sentence_ui_flag             One-shot reset request
	current_kid_idx                   Active kid index
	actor_vars[]                      Kid state flags (sign bit used for incapacitated)
	control_mode                      Current control mode
	current_verb_id                   Active verb token
	current_preposition               Current preposition token
	direct_object_idx_lo/hi           Current direct object id
	indirect_object_idx_lo/hi         Current indirect object id
	cursor_x_pos_quarter_relative     Cursor X on sentence bar (used in subroutines)

Global Outputs
	current_verb_id                   May be clamped to NEW_KID or reset to WALK_TO
	current_preposition, direct_object_idx_lo/hi,
	indirect_object_idx_lo/hi         Cleared on resets or updated from cursor hits
	forced_sentence_trigger           Set/cleared when “What is?” is active
	sentence_bar_needs_refresh        Set when UI must redraw the bar
	needs_sentence_rebuild            Set for one-shot rebuild after certain updates
	target_entity                Cleared when normalizing WALK_TO without object

Description
	* Reset path:
		  • If init_sentence_ui_flag is set:
			  – If no preposition: clear the flag only.
			  – If a preposition exists: clear prep, IO, and DO; then clear the flag.
	* Incapacity limits:
		  • If the current kid is “incapacitated” (sign bit in actor_vars), force
		  NEW_KID behavior by zeroing current_verb_id unless it is NEW_KID.
	* Control-mode dispatch:
		  • CUTSCENE → jump to run_sentence_if_complete.
		  • KEYPAD   → force current_verb_id = VERB_PUSH, then fall through.
		  • NORMAL   → if VERB_WHAT_IS, set forced_sentence_trigger.
	* Forced trigger:
		  • If forced_sentence_trigger == SET:
			  – If verb == GIVE and a preposition exists, select an actor under the
			  cursor as IO; else select an object under the cursor (DO or IO).
			  – Handle “no hit” (including WALK_TO special case) or commit DO/IO.
			  – Normalize UI flags and fall to the completeness gate.
		  • Otherwise, fall straight to run_sentence_if_complete.
	* Cursor-hit handlers:
		  • DO path: confirm or set DO; request UI refresh; in KEYPAD set rebuild.
		  • IO path: reject DO==IO, confirm or set IO; in KEYPAD set rebuild.
	* Finalization:
		  • Always request a UI refresh before the completeness gate.
		  • If verb is WALK_TO, also clear target_entity and set rebuild.
		  • Tail-fall to run_sentence_if_complete for execution/stacking.

Notes
	* This routine never executes scripts or stages walking directly; it only
	  mutates sentence parts, UI flags, and routes to the completeness logic.
	* DO/IO equality is explicitly rejected to prevent redundant sentences.
================================================================================
*/
* = $077C
process_sentence:
		//Reset sentence needed?
        lda     init_sentence_ui_flag            	 // Load one-shot UI reset flag; nonzero → run reset path
        beq     enforce_verb_limits_if_incapacitated // Flag clear → skip reset and continue to incapacity gate

        // ------------------------------------------------------------
        // Reset sentence parts if required
        // ------------------------------------------------------------
        lda     current_preposition              // Load current preposition token for reset logic
        beq     apply_pending_sentence_reset     // If none set → perform partial reset only

		//Clear sentence parts
        lda     #$00                            
        sta     current_preposition             
        sta     indirect_object_idx_lo          
        sta     indirect_object_idx_hi          
        sta     direct_object_idx_lo            
        sta     direct_object_idx_hi            

apply_pending_sentence_reset:                 
        sta     init_sentence_ui_flag         // Clear one-shot sentence UI reset flag

        // ------------------------------------------------------------
        // Limit verbs if the current kid is dead or in a radiation suit
        // ------------------------------------------------------------
enforce_verb_limits_if_incapacitated:      
        ldx     current_kid_idx            	// X := active kid index
        lda     actor_vars,x               	// A := status flags for kid X; N set if incapacitated
        bpl     dispatch_by_control_mode    // N=0 (positive) → alive/normal → continue
		
		// Kid incapacitated - force-disable all verbs except “New Kid”
		// New kid verb?
        lda     current_verb_id             
        cmp     #VERB_NEW_KID               
        beq     dispatch_by_control_mode    // If equal → allow verb, continue normal flow
		
		//All other verbs disallowed
        lda     #$00                        
        sta     current_verb_id             

        // ------------------------------------------------------------
        // Control mode dispatch
        // ------------------------------------------------------------
dispatch_by_control_mode:      
		//Cutscene mode?
        lda     control_mode               			// A := current control mode
        cmp     #CONTROL_MODE_CUTSCENE     			// Cutscene mode?
        bne     handle_keypad_mode_or_fallthrough 	// If not, check keypad/normal next
		
		//Cutscene mode
        jmp     run_sentence_if_complete   			// Cutscene: only evaluate/run completed sentences

handle_keypad_mode_or_fallthrough:          
		//Keypad mode?
        cmp     #CONTROL_MODE_KEYPAD        		// Compare current control mode against keypad
        bne     handle_normal_mode          		// If not keypad mode → handle normal mode next

        // Keypad control mode forces the “Push” verb regardless of cursor or UI state
        lda     #VERB_PUSH                  		
        sta     current_verb_id             		

        // ------------------------------------------------------------
        // Normal control mode
        // ------------------------------------------------------------
handle_normal_mode:                        
		// Verb is 'What is?'
        lda     current_verb_id             	
        cmp     #VERB_WHAT_IS               	
        bne     handle_click_trigger_or_defer 	// If not, skip forced-trigger setup

		//What-is verb - force a sentence trigger
        lda     #FORCED_TRIGGER_SET          	// Load flag value indicating forced trigger
        sta     forced_sentence_trigger      	

handle_click_trigger_or_defer:             		
		// Forced trigger pending?
        lda     forced_sentence_trigger     	
        cmp     #FORCED_TRIGGER_SET         	
        beq     on_forced_trigger           	// Yes → handle immediate cursor-based selection
		
        jmp     run_sentence_if_complete        // no forced trigger → check normally

on_forced_trigger:                          
        // ------------------------------------------------------------
        // Forced-trigger path
        // ------------------------------------------------------------
		// Clear forced-trigger so it fires only once
        lda     #FORCED_TRIGGER_CLEARED     
        sta     forced_sentence_trigger     

		//Verb is "Give"?
        lda     current_verb_id             
        cmp     #VERB_GIVE                  
        bne     pick_object_under_cursor

        // GIVE path: IO must be an actor when a preposition is present
        lda     current_preposition         // Check if a preposition has been chosen
        beq     pick_object_under_cursor    // None yet → select DO under cursor
		
        jsr     find_actor_under_cursor_excl_kid // Preposition set → pick actor under cursor as IO
        jmp     branch_on_cursor_hit        // Continue with common cursor-hit handling

pick_object_under_cursor:                 	
		// Select object under cursor (DO if no prep, else IO)
        jsr     find_object_at_cursor // Returns: X=object id lo, A=object id hi; X=OBJ_IDX_NONE if no hit

branch_on_cursor_hit:                   
		// Object found under cursor?
        cpx     #OBJ_IDX_NONE          		
        bne     cursor_hit_object_found 	// If hit present → handle object-found path

        // ------------------------------------------------------------
        // No object found under cursor
        // ------------------------------------------------------------
		// Verb is "Walk to"?
        lda     current_verb_id             
        cmp     #VERB_WALK_TO               
        bne     finalize_after_walkto_nohit // If not, proceed to general finalization

        // Bare “Walk to” with no hit → clear complements
        lda     #OBJ_IDX_NONE              // Prepare sentinel for “no object selected”
        sta     direct_object_idx_lo       // Clear DO presence (lo byte acts as presence flag)
        sta     current_preposition        // Clear preposition token
        sta     indirect_object_idx_lo     // Clear IO presence (lo byte as presence flag)

finalize_after_walkto_nohit:            	// No object hit path complete; continue to UI/update gate
        jmp     finalize_and_maybe_execute 	// Jump to finalization and completeness check

        // ------------------------------------------------------------
        // Object found under cursor
        // ------------------------------------------------------------
cursor_hit_object_found:                 		// Object detected under cursor → determine role
        ldy     current_preposition      		// Load preposition token to see if IO context is active
        beq     update_or_confirm_direct_object // If none set → treat as DO selection path

        // ------------------------------------------------------------
        // Handle indirect object selection
        // ------------------------------------------------------------
        cpx     indirect_object_idx_lo      	// Compare hit object’s lo byte with current IO lo
        bne     guard_reject_do_eq_io       	// If different → objects differ; continue
        cmp     indirect_object_idx_hi      	// Compare A (hi byte) against current IO hi
        bne     guard_reject_do_eq_io       	// If different → objects differ; continue
		
		//Force rebuild sentence
        ldy     #REBUILD_SENTENCE_ON          	
        sty     needs_sentence_rebuild        	

guard_reject_do_eq_io:
        cpx     direct_object_idx_lo          	// Compare hit object’s lo byte with current DO lo
        bne     commit_new_indirect_object    	// If different → not the same object; accept as new IO
        cmp     direct_object_idx_hi          	// Compare A (hi byte) with current DO hi
        beq     finalize_after_io_update      	// DO == IO → reject selection and finalize

commit_new_indirect_object:             		
		// Commit newly selected indirect object (IO)
        stx     indirect_object_idx_lo   		
        sta     indirect_object_idx_hi   		

		//Keypad control mode?
        lda     control_mode               		
        cmp     #CONTROL_MODE_KEYPAD       		
        bne     finalize_after_io_update   		// If not keypad mode → skip manual rebuild handling

		//Force rebuild sentence
        lda     #REBUILD_SENTENCE_ON          	
        sta     needs_sentence_rebuild        	

		// IO path complete; proceed to UI refresh and execution gate
finalize_after_io_update:               		
        jmp     finalize_and_maybe_execute

        // ------------------------------------------------------------
        // Handle direct object selection
        // ------------------------------------------------------------
update_or_confirm_direct_object:         		
        cpx     direct_object_idx_lo      		// Compare hit object’s lo byte with current DO lo
        bne     commit_new_direct_object  		// Different object → commit as new DO
        cmp     direct_object_idx_hi      		// Compare A (hi byte) with current DO hi
        bne     commit_new_direct_object  		// Different high byte → commit new DO
        ldy     #REBUILD_SENTENCE_ON      		// Same DO reselected → trigger UI rebuild
        sty     needs_sentence_rebuild

		// Commit newly selected direct object (DO)
commit_new_direct_object:               
        stx     direct_object_idx_lo     		// DO id lo := hit object lo
        sta     direct_object_idx_hi     		// DO id hi := hit object hi
        lda     #TRUE      		// Flag a sentence-bar redraw
        sta     sentence_bar_needs_refresh

		//Keypad control mode?
        lda     control_mode               		// Load current control mode for rebuild decision
        cmp     #CONTROL_MODE_KEYPAD       		// Is this keypad (manual input) mode?
        bne     finalize_and_maybe_execute 		// If not keypad mode → continue to finalization
		
		//Force rebuild sentence
        lda     #REBUILD_SENTENCE_ON       		// Keypad mode → force sentence rebuild
        sta     needs_sentence_rebuild     		// Mark UI to reconstruct sentence bar manually

        // ------------------------------------------------------------
        // Finalize sentence processing and run if complete
        // ------------------------------------------------------------
finalize_and_maybe_execute:           			// Finalize selections, then run completeness gate
		// Request a sentence-bar redraw this frame
        lda     #TRUE    			
        sta     sentence_bar_needs_refresh

		// Verb is "Walk to"?
        lda     current_verb_id        			
        cmp     #VERB_WALK_TO
        bne     run_sentence_if_complete 		// Not “Walk to” → proceed to completeness checks

		// Clear any active destination entity marker
        lda     #ENTITY_NONE               		
        sta     target_entity
		
		//Force rebuild sentence
        lda     #REBUILD_SENTENCE_ON       		
        sta     needs_sentence_rebuild     		
        // Fall through to run_sentence_if_complete

// ------------------------------------------------------------
// Trampoline for sentence bar refresh.
// Exists only to provide a callable JMP target for other routines
// that need to refresh the UI without pushing a full JSR/RTS frame.
// ------------------------------------------------------------
* = $0AF0
refresh_sentence_bar_trampoline:
		jmp refresh_sentence_bar

/*
================================================================================
  run_sentence_if_complete
================================================================================

Summary
	Run the current verb sentence if all required parts are present. Handles
	special verbs, requests UI refreshes when parts are missing, and dispatches
	either immediate execution or stacking via dispatch_or_push_action.

Vars/State
	needs_sentence_rebuild          Cleared here if set; triggers a one-shot UI rebuild

Global Inputs
	current_verb_id                 Active verb token (0 if none)
	direct_object_idx_lo            Presence check for direct object (lo byte)
	current_preposition             Current preposition token (0 if none)
	indirect_object_idx_lo          Presence check for indirect object (lo byte)

Global Outputs
	needs_sentence_rebuild          Cleared when observed
	sentence_bar_needs_refresh      Set when preposition was just chosen
	(UI) refresh via refresh_sentence_bar_trampoline
	
	(control flow) tail-calls:
		- handle_new_kid_verb
		- dispatch_or_push_action

Description
	* Always refresh the sentence bar first so the UI stays in sync.
	* If a one-shot sentence rebuild was requested, clear the flag and continue.
	* If no verb is selected, refresh again and exit.
	* If verb = VERB_NEW_KID, delegate to handle_new_kid_verb.
	* If verb = VERB_WALK_TO, execute immediately (no DO required) via
	  dispatch_or_push_action.
	* For all other verbs:
		  • Require a direct object; if missing, refresh and exit.
		  • If preposition is needed but missing, call select_preposition_for_verb:
			  – If it returns 0 → no preposition required; execute now.
			  – If it returns nonzero → save it, request a UI refresh, and exit so
			  the user can choose an indirect object.
		  • If preposition is present but no indirect object yet, refresh and exit.
		  • When all parts are present, execute via dispatch_or_push_action.

Notes
	* This routine performs no movement or script execution itself; it only
	  validates sentence completeness and routes control.
	* UI is refreshed at entry and whenever a missing part blocks execution.
================================================================================
*/
* = $0874
run_sentence_if_complete:
        // ------------------------------------------------------------
        // Execute the sentence only if all mandatory parts are present.
		//
        // - Always sync the UI first.
        // - If a rebuild was requested, clear the flag and continue.
        // ------------------------------------------------------------
        jsr     refresh_sentence_bar             // sync sentence bar with current parser state

		//Clear the sentence rebuild flag if it's still set
        lda     needs_sentence_rebuild           
        beq     refresh_sentence_bar   //FIX           		
        lda     #FALSE                           
        sta     needs_sentence_rebuild           

check_verb_validity:
        // ------------------------------------------------------------
        // Validate verb presence.
		//
        // If current_verb_id == 0 → no verb selected → refresh and exit.
        // Otherwise continue to special-verb dispatch.
        // ------------------------------------------------------------
        lda     current_verb_id                  // load verb id
        bne     dispatch_new_kid_or_next         // Z=0 → id≠0 → handle NEW_KID/WALK_TO cases
        jmp     refresh_sentence_bar_trampoline  // id==0 → no verb → refresh UI and return

        // ------------------------------------------------------------
        // Handle "New Kid" verb
		//
        // New kid → jump to dedicated handler. Otherwise continue flow.
        // ------------------------------------------------------------
dispatch_new_kid_or_next:
        cmp     #VERB_NEW_KID                    
        bne     dispatch_walkto_or_need_do       // not NEW_KID → check WALK_TO / DO path
        jmp     handle_new_kid_verb              // NEW_KID → delegate to handler

dispatch_walkto_or_need_do:
        // ------------------------------------------------------------
        // Handle "Walk to" verb
		//
        // Walk to? Execute immediately.
        // ------------------------------------------------------------
        cmp     #VERB_WALK_TO                    
        bne     require_direct_object_or_refresh // not WALK_TO → must require a DO or refresh
        jmp     dispatch_or_push_action          // WALK_TO → execute without objects

require_direct_object_or_refresh:
        // ------------------------------------------------------------
        // Require a direct object for verbs that need one.
		//
        // If direct_object_idx_lo == 0 → none selected → refresh and exit.
        // Otherwise continue to preposition/IO resolution.
        // ------------------------------------------------------------
        lda     direct_object_idx_lo            // load DO low index
        bne     resolve_preposition_and_io      // DO present → proceed
        jmp     refresh_sentence_bar_trampoline // no DO → update UI and return
		
		//Unreachable code - replicated from original
		jmp		refresh_sentence_bar

resolve_preposition_and_io:
        // ------------------------------------------------------------
        // Preposition/IO gate.
		//
        // - If preposition is absent → derive it for the current verb.
        // - If preposition exists → require an indirect object.
        // ------------------------------------------------------------
        lda     current_preposition                  	
        beq     select_preposition_for_current_verb_id 	// none → select one for this verb

        lda     indirect_object_idx_lo 
		beq		ref_trampoline
		
        // ------------------------------------------------------------
        // All parts validated → tail-jump to execution.
		//
        // Avoids extra stack depth by jumping, not calling.
        // ------------------------------------------------------------
		jmp     dispatch_or_push_action
		
		//Unreachable code - replicated from original
		jmp 	select_preposition_for_current_verb_id
		
ref_trampoline:		
        jmp     refresh_sentence_bar_trampoline      	// IO missing → refresh and exit

        
        // ------------------------------------------------------------
        // Preposition determination path
        // ------------------------------------------------------------
select_preposition_for_current_verb_id:
        jsr     select_preposition_for_verb      		// A := selected preposition
        bne     save_preposition_and_request_ui_refresh // have preposition → persist and refresh UI
        jmp     dispatch_or_push_action          		// no preposition needed → execute now

		//Unreachable code - replicated from original
		jmp		refresh_sentence_bar

save_preposition_and_request_ui_refresh:
        // ------------------------------------------------------------
        // Persist inferred preposition and trigger a UI refresh.
		//
        // A holds the preposition returned by select_preposition_for_verb.
        // ------------------------------------------------------------
        sta     current_preposition              // commit selected preposition

        lda     #TRUE              // flag the sentence bar for redraw
        sta     sentence_bar_needs_refresh       // UI will update on next refresh path

        // Fall through to refresh_sentence_bar

/*
================================================================================
  handle_new_kid_verb
================================================================================
Summary
	Handle the “New kid” verb. In normal control mode, map the cursor’s X position
	to kid 0/1/2, refresh the UI, and attempt a kid switch. Outside normal control,
	just normalize the verb/UI and exit.

Global Inputs
	control_mode                     Current control mode (must be CONTROL_MODE_NORMAL)
	cursor_x_pos_quarter_relative    Cursor X on the sentence bar (quarter-pixel units)

Global Outputs
	current_verb_id                  Set to VERB_WALK_TO
	sentence_bar_needs_refresh       Set to TRUE
	(current_kid_idx)                May change via switch_active_kid_if_different

Description
	* If control_mode ≠ CONTROL_MODE_NORMAL:
		  • Normalize UI: set current_verb_id = VERB_WALK_TO and request a bar refresh.
		  • Return.
	* Otherwise:
		  • Determine kid by cursor X:
			  – X < SECOND_KID_NAME_COLUMN  → kid 0
			  – SECOND_KID_NAME_COLUMN ≤ X ≤ THIRD_KID_NAME_COLUMN → kid 1
			  – X > THIRD_KID_NAME_COLUMN   → kid 2
		  • Push kid index on stack (PHA), reset UI to WALK_TO and refresh the bar,
		  then POP and call switch_active_kid_if_different.
		  • Normalize UI again to WALK_TO and request refresh before returning.

Notes
	* The routine always leaves the UI in the neutral “Walk to” state.
	* If the selected kid is already active, switch_active_kid_if_different exits
	  early and no camera/inventory updates occur.
================================================================================
*/
* = $099B
handle_new_kid_verb:
        // ------------------------------------------------------------
        // Normal control mode?
        // ------------------------------------------------------------
        lda     control_mode                 
        cmp     #CONTROL_MODE_NORMAL         // gate: only allow kid change in normal mode
        bne     set_walk_and_exit            // not normal → skip selection; normalize verb/UI and exit

        // ------------------------------------------------------------
        // Normal mode: determine which kid was clicked
        // ------------------------------------------------------------
select_by_cursor:
        lda     cursor_x_pos_quarter_relative    // get cursor X in sentence-bar quarters
        cmp     #SECOND_KID_NAME_COLUMN          // compare with first threshold
        bcs     third_kid_check                  // X ≥ second col → candidate kid1 or kid2

        // X < SECOND_KID_NAME_COLUMN → select kid 0
        lda     #$00                             // kid index := 0
        jmp     select_kid                       // commit selection

third_kid_check:
        // SECOND_KID_NAME_COLUMN ≤ X ? compare against THIRD_KID_NAME_COLUMN
        cmp     #THIRD_KID_NAME_COLUMN           // decide between kid1 and kid2
        bcc     second_kid_selected              // X < third threshold → kid 1
        beq     second_kid_selected              // X == third threshold → kid 1
        lda     #$02                             // X > third threshold → kid 2
        jmp     select_kid                       // commit selection

second_kid_selected:
        lda     #$01                             // kid index := 1

        // ------------------------------------------------------------
        // Commit kid change
        // ------------------------------------------------------------
select_kid:
        pha                                      // preserve selected kid index across UI refresh
		
        lda     #VERB_WALK_TO
        sta     current_verb_id                  // normalize verb to "Walk to" before switching
        lda     #TRUE
        sta     sentence_bar_needs_refresh       // request sentence-bar redraw
        jsr     refresh_sentence_bar             // sync UI to reflect pending change
		
        pla                                      // restore kid index into A
        jsr     switch_active_kid_if_different   // commit switch only if target ≠ current

set_walk_and_exit:		
        // ------------------------------------------------------------
        // Normalize and exit.
		//
        // Ensures parser leaves NEW KID flow in a standard "Walk to" state
        // and refreshes the UI to reflect that.
        // ------------------------------------------------------------
        lda     #VERB_WALK_TO                    // restore default verb
        sta     current_verb_id                  // update parser verb state
        lda     #TRUE
        sta     sentence_bar_needs_refresh       // flag UI for redraw
        rts                                      // return to caller
/*
================================================================================
  process_sentence_stack_entry
================================================================================

Summary
	Process one stacked sentence at the top of the sentence stack. If all parts
	are ready, execute immediately. Otherwise stage walking toward the needed
	object. Skips invalid or redundant entries and advances the stack pointer.

Global Inputs
	sentstk_top_idx                   Current top index (–1 when empty)
	stacked_verb_ids[]                Stacked verb ids
	stacked_do_id_lo/hi[]             Stacked direct-object ids
	stacked_prep_ids[]                Stacked preposition ids
	stacked_io_id_lo/hi[]             Stacked indirect-object ids
	target_entity                Nonzero if a walk is already in progress
	control_mode
	current_kid_idx, actor_vars[]     Actor state; includes ACTOR_IS_FROZEN
	object_attributes[]               For inventory/owner checks

Global Outputs
	sentstk_top_idx                   Decremented on consume or drop
	sentstk_free_slots                Decremented on consume; reset on underflow
	active_verb_id                    Latched verb for execution
	active_do_id_lo/hi                Latched DO id
	active_prep_id                    Latched preposition
	active_io_id_lo/hi                Latched IO id
	target_entity                Set when staging a walk
	active_costume                    Acting costume for pathing

Description
	* Early exits:
		  • If a destination is already active, do nothing.
		  • If the stack is empty, do nothing.
	* Redundancy filter:
		• If a preposition is present and DO == IO, drop the entry.
	* Inventory gate:
		  • Check DO and IO ownership for the current kid. If neither is owned,
		  attempt to push a scripted “Pick Up <obj>” for DO, else IO. If neither
		  supports Pick Up, drop the entry.
	* Activation:
		  • Copy the top entry into active_* and pop the stack. Protect against
		  underflow by restoring empty-state invariants and capacity.
	* Dispatch:
		  • If DO is in inventory and no preposition → execute now.
		  • If a preposition exists, ensure IO readiness; if IO not ready → walk to IO.
		  • If DO not in inventory:
			  – In keypad mode → execute directly (no walking).
			  – Otherwise → walk to DO.

Notes
	* Ownership checks use the object’s “in inventory” flag plus owner nibble.
	* Walking resolves the destination entity and respects the ACTOR_IS_FROZEN gate.
================================================================================
*/
* = $0B9C
process_sentence_stack_entry:
        // ------------------------------------------------------------
		// Check if there's an active destination entity; if so, exit
        // ------------------------------------------------------------
        lda     target_entity            // load current destination entity
        beq     check_stack_nonempty          // no destination active → process next stacked sentence
        rts                                   // destination active → do nothing this frame

        // ------------------------------------------------------------
		// Check if there's a sentence in the stack; if not, exit
        // ------------------------------------------------------------
check_stack_nonempty:
        ldx     sentstk_top_idx               // X := top index
        bpl     check_same_complements        // top ≥ 0 → there is a sentence to process
        rts                                   // top < 0 → stack empty → exit

        // ------------------------------------------------------------
        // Skip identical complement sentences (DO == IO)
        // ------------------------------------------------------------
check_same_complements:
        lda     stacked_prep_ids,x              // preposition present?
        beq     set_active_sentence_tokens      // no prep → identical DO/IO test not applicable

        lda     stacked_do_id_lo,x              // load DO lo
        beq     verify_inventory_status         // no DO → skip identical-complements check

        cmp     stacked_io_id_lo,x              // compare DO lo vs IO lo
        bne     verify_inventory_status         // differ → complements not identical

        lda     stacked_do_id_hi,x              // load DO hi
        cmp     stacked_io_id_hi,x              // compare DO hi vs IO hi
        bne     verify_inventory_status         // differ → complements not identical

        dec     sentstk_top_idx                // pop entry: discard sentence with DO==IO
        rts                                    // return without processing this stack item

        // ------------------------------------------------------------
        // Check if complements are in inventory, else try scripted pickup
        // ------------------------------------------------------------
verify_inventory_status:
        lda     #ARG_IS_DO                     			// A := check direct object (DO)
        jsr     is_sentence_object_owned_by_current_kid 
        cmp     #FALSE          						// test ownership of DO
        bne     set_active_sentence_tokens     			// A≠0 → DO not owned → proceed to activate sentence

        lda     #ARG_IS_IO                     			// A := check indirect object (IO)
        jsr     is_sentence_object_owned_by_current_kid 
        cmp     #FALSE          						// test ownership of IO
        bne     set_active_sentence_tokens     			// A≠0 → IO not owned → activate sentence (may walk)

        // Attempt scripted pickup for the direct object
        lda     #ARG_IS_DO                     			// select DO
        jsr     has_pickup_script_for_sentence_part 	
        cmp     #TRUE                         			// script available?
        bne     try_indirect_object_pickup     			// no → try IO pickup instead

        lda     #ARG_IS_DO                     			// select DO
        jsr     push_pickup_for_sentence_part  			// push pickup action for DO
        rts                                    			// defer sentence; pickup will run first
		
		//Unreachable code - replicated from original
		jmp		set_active_sentence_tokens

try_indirect_object_pickup:
        // Attempt scripted pickup for the indirect object
        lda     #ARG_IS_IO                     			// select IO
        jsr     has_pickup_script_for_sentence_part 	
        cmp     #TRUE                          			// script available?
        bne     drop_sentence_no_pickup_available 		// no → drop this sentence
		
        lda     #ARG_IS_IO                     			// select IO
        jsr     push_pickup_for_sentence_part  			// push pickup action for IO
        rts                                    			// defer sentence; pickup runs first

		//Unreachable code - replicated from original
		jmp		set_active_sentence_tokens

drop_sentence_no_pickup_available:
        // discard entry: neither DO nor IO can be auto-picked
		dec     sentstk_top_idx                
        rts                                    			

        // ------------------------------------------------------------
        // Activate current sentence: copy into active_* vars
        // ------------------------------------------------------------
set_active_sentence_tokens:
        ldy     sentstk_top_idx                 // Y := stack top index
		
        lda     stacked_verb_ids,y              
        sta     active_verb_id                  // commit active verb

        lda     stacked_do_id_lo,y              
        sta     active_do_id_lo                 // commit DO lo
        lda     stacked_do_id_hi,y              
        sta     active_do_id_hi                 // commit DO hi

        lda     stacked_prep_ids,y              
        sta     active_prep_id                  // commit preposition

        lda     stacked_io_id_lo,y              
        sta     active_io_id_lo                 // commit IO lo
        lda     stacked_io_id_hi,y              
        sta     active_io_id_hi                 // commit IO hi

        dec     sentstk_top_idx               	// pop: advance past consumed entry
        dec     sentstk_free_slots            	// track capacity used
        bpl     stack_capacity_ok             	// top ≥ 0 → stack still valid; continue

        // Stack underflow guard: reset indices and exit
        lda     #SENT_STACK_EMPTY_IDX          	// restore empty-top sentinel
        sta     sentstk_top_idx
        lda     #SENT_STACK_MAX_TOKENS         	// restore full capacity counter
        sta     sentstk_free_slots
        rts                                     // nothing more to process

        // ------------------------------------------------------------
        // Determine if we must walk or execute
        // ------------------------------------------------------------
stack_capacity_ok:
        ldx     active_do_id_lo                	// X := DO lo
        lda     active_do_id_hi                	// A := DO hi
        jsr     resolve_object_if_not_costume  	// returns A==0 if DO is in inventory, ≠0 if in world
        bne     check_walk_to_direct_object    	// DO in world → plan walk toward DO

        lda     active_prep_id                 	// load preposition
        beq     execute_active_verb            	// no preposition → execute now

        // Preposition present → ensure IO availability
        ldx     active_io_id_lo               	// X := IO lo
        lda     active_io_id_hi               	// A := IO hi
        jsr     resolve_object_if_not_costume 	// A==0 if IO in inventory, ≠0 if in world
        bne     init_walk_to_indirect_object  	// IO in world → initiate walk to IO
        jmp     execute_verb_handler_for_object // IO ready → execute verb now

		//Unreachable code - replicated from original
		jmp		exit_process_sentence_stack_entry
		
        // ------------------------------------------------------------
        // Walk toward indirect object
        // ------------------------------------------------------------
init_walk_to_indirect_object:
        ldx     active_io_id_lo               	// X := IO lo
        lda     active_io_id_hi               	// A := IO hi
        stx     target_obj_lo            	// set destination object (lo)
        sta     target_obj_hi            	// set destination object (hi)
        jsr     set_approach_point// compute world target_x/target_y and entity type
        sty     target_entity            	// record destination entity kind

		//Copy coordinates to debug vars
        lda     target_x                         
        sta     var_target_x              
        lda     target_y                         
        sta     var_target_y              
		
        lda     current_kid_idx                	// select actor for movement
        sta     active_costume                 	// set active costume = current kid

		//Check if actor is incapacitated
        tax                                     // X := active_costume index for per-actor arrays
        lda     actor_vars,x                    // load actor flags
        and     #ACTOR_IS_FROZEN                 // isolate frozen bit; Z=1 iff not frozen
        bne     exit_hsq                        // frozen → skip movement setup
		
		//Actor free to move, set destination
        jsr     set_costume_target           // issue destination to pathing system
exit_hsq:
        jmp     exit_process_sentence_stack_entry // tail-jump to common exit

        // ------------------------------------------------------------
        // Execute verb directly
        // ------------------------------------------------------------
execute_active_verb:
        // Execute the active verb immediately (tail-jump).
        jmp     execute_verb_handler_for_object
		
		//Unreachable code - replicated from original
		jmp		exit_process_sentence_stack_entry

        // ------------------------------------------------------------
        // Walk toward direct object if not in inventory
        // ------------------------------------------------------------
check_walk_to_direct_object:
        lda     control_mode                    // read input mode
        cmp     #CONTROL_MODE_KEYPAD             // keypad mode disables auto-walk
        bne     init_walk_to_direct_object      // not keypad → walk toward DO if needed

        jsr     execute_verb_handler_for_object // keypad mode → execute immediately, no walking
        lda     #$00                            // clear any pending destination
        sta     target_entity              // ensure pathing is idle
        rts                                     // done

init_walk_to_direct_object:
        ldx     active_do_id_lo               	// X := DO lo
        lda     active_do_id_hi               	// A := DO hi
        stx     target_obj_lo            	// stage destination object (lo)
        sta     target_obj_hi            	// stage destination object (hi)
        jsr     set_approach_point// compute target_x/target_y and entity kind
        sty     target_entity            	// cache routed destination entity

		//Copy coordinates to debug vars
        lda     target_x                         
        sta     var_target_x              
        lda     target_y                         
        sta     var_target_y              

        lda     current_kid_idx                	// select actor to move (kid index)
        sta     active_costume                 	// set active costume = current kid
		
		//Check if actor is incapacitated
        tax                                     // X := actor index for per-actor arrays
        lda     actor_vars,x                    // load actor state flags
        and     #ACTOR_IS_FROZEN                 // test frozen bit
        bne     exit_process_sentence_stack_entry // frozen → skip issuing a destination
		
		//Actor free to move, set destination
        jsr     set_costume_target           // start pathing toward var_destination_{x,y}

exit_process_sentence_stack_entry:
        rts                                     // done processing this stacked sentence
/*
================================================================================
  dispatch_or_push_action
================================================================================
Summary
	Prepare the sentence stack for a new command, then either:
		* Walk immediately to the cursor location for a bare “Walk to”, or
		* Push a complete sentence (verb + DO [+ prep + IO]) onto the stacks and
		  normalize the UI back to “Walk to”.

Global Inputs
	current_verb_id
	direct_object_idx_lo, direct_object_idx_hi
	preposition
	indirect_object_idx_lo, indirect_object_idx_hi
	cursor_x_pos_quarter_absolute, cursor_y_pos_half_off_by_8
	current_kid_idx
	actor_for_costume[]               costume → actor index map
	actor_vars[]                      includes ACTOR_IS_FROZEN flag

Global Outputs
	sentstk_free_slots                reset to SENT_STACK_MAX_TOKENS
	sentstk_top_idx                   reset to SENT_STACK_EMPTY_IDX; incremented on push
	stacked_verb_ids[x]               written on push
	stacked_do_id_lo/hi[x]            written on push
	stacked_prep_ids[x]               written on push
	stacked_io_id_lo/hi[x]            written on push
	current_verb_id                   reset to VERB_WALK_TO after pushing non-walk verbs
	direct_object_idx_lo              cleared after pushing non-walk verbs
	preposition                       cleared after pushing non-walk verbs
	target_entity                cleared before exit
	active_costume                    set for bare “Walk to”
	actor                             latched actor for movement path
	actor_target_x[], actor_target_y[]    written for immediate walk
	(via call) stage_actor_path_to_target()

Description
	* Reset sentence-stack bookkeeping: set sentstk_free_slots to the maximum
	  and sentstk_top_idx to “empty”. If the verb is VERB_WHAT_IS, return.

	* If the verb is WALK_TO and no direct object is selected, treat it as a
	  bare walk:
		  • Resolve the acting entity from current_kid_idx.
		  • Copy cursor coordinates to target_x/target_y and clamp to walkable space.
		  • Publish var_target_x/y for debugging.
		  • If the actor is not frozen, write actor_target_x/y_dest and call
		  stage_actor_path_to_target. Then return.

	* Otherwise push a full sentence:
		  • Increment sentstk_top_idx and write verb, DO, prep, IO into the
		  stacked_* arrays at index X.
		  • If the verb was not WALK_TO, reset UI defaults by setting
		  current_verb_id = VERB_WALK_TO and clearing DO and preposition.
		  • Clear target_entity and return.

Notes
	* This routine does not execute verbs. It only stages immediate walking
	  for the bare “Walk to” case or records a sentence for later handling.
	* Indices and capacity are managed as a LIFO stack; range checks occur in
	  the push helpers.
================================================================================
*/
* = $0AF3
dispatch_or_push_action:
        // ------------------------------------------------------------
        // Reset sentence-stack bookkeeping
		//
        // Seeds free-capacity to max and resets head index to empty
        // sentinel. Prepares stack for a new sentence; actual
        // capacity decrementing occurs elsewhere.
        // ------------------------------------------------------------		
        lda     #SENT_STACK_MAX_TOKENS          
        sta     sentstk_free_slots     		// Reset free-slot counter; decremented elsewhere

        lda     #SENT_STACK_EMPTY_IDX            
        sta     sentstk_top_idx             // Reset head to “no entries” for pre-increment use

        // ------------------------------------------------------------
        // Early exit for “What is” verb
		//
        // If verb is VERB_WHAT_IS, no action is performed and the routine returns immediately. 
		// Otherwise, control falls through to walk-to handling.
        // ------------------------------------------------------------
		// Is verb "What is"?
        lda     current_verb_id
        cmp     #VERB_WHAT_IS                   
        bne     handle_walk_to                  // Not "What is" → continue handling
		
		// "What is" has no action → return
        rts                                     

        // ------------------------------------------------------------
        // Walk-to verb dispatch
		//
        // Distinguishes between a bare “Walk to” (no object selected)
        // and a full “Walk to <object>” action. Non–walk-to verbs or
        // walk-to with object are stacked for later execution; only a
        // bare walk-to proceeds to immediate movement.
        // ------------------------------------------------------------
handle_walk_to:
        cmp     #VERB_WALK_TO                   // Z=1 if current verb is "Walk to"
        bne     push_sentence                	// Different verb → push full sentence

        lda     direct_object_idx_lo            // Test DO presence using low byte
        bne     push_sentence                	// DO present → treat as full action and push

        // ------------------------------------------------------------
        // Bare “Walk to” → walk to cursor location
		//
        // Resolve acting entity from current kid
        // Sets active_costume := current_kid_idx, maps costume → actor,
        // and stores actor index for subsequent movement/path logic.
        // ------------------------------------------------------------
        lda     current_kid_idx                 // Select current kid as the acting entity
        sta     active_costume                  // Latch kid’s costume as active

        ldx     active_costume                 	// X := active costume index for table lookup
        lda     actor_for_costume,x            	// A := actor index mapped from costume
        tax                                    	// X := actor index 
        stx     actor                          	// Persist actor index for subsequent movement/path ops

        // ------------------------------------------------------------
        // Capture and normalize destination coordinates
		//
        // Loads cursor position (quarter X, half Y) into target_x/target_y
        // and calls snap_coords_to_walkbox to constrain them to valid
        // walkable terrain.
        // ------------------------------------------------------------
        lda     cursor_x_pos_quarter_absolute   // Get raw cursor X (quarter-pixel units)
        sta     target_x                          // Seed destination X before clamping

        lda     cursor_y_pos_half_off_by_8      // Get raw cursor Y (half-pixel, offset-by-8)
        sta     target_y                          // Seed destination Y before clamping

        jsr     snap_coords_to_walkbox          // Clamp target_x/target_y to nearest walkable box

        // ------------------------------------------------------------
        // Publish normalized destination for debug
        // ------------------------------------------------------------
        ldx     actor                          	// X := actor index for movement context
        lda     target_x                        	// Load clamped X coordinate
        sta     var_target_x              	// Store global X destination (for debugging)

        lda     target_y                         	// Load clamped Y coordinate
        sta     var_target_y              	// Store global Y destination (for debugging)

        // ------------------------------------------------------------
        // Frozen-state gate
		//
        // If the current kid is frozen, suppress any path setup or
        // movement. Destination mirrors remain updated but unused.
        // ------------------------------------------------------------
        ldx     current_kid_idx                    
        lda     actor_vars,x                   // Load actor’s state flags
        and     #ACTOR_IS_FROZEN                // Mask bit(s) indicating frozen state
        bne     exit_bare_walk				   // If frozen → skip path setup and exit routine

        // ------------------------------------------------------------
        // Stage path to destination
		//
        // Writes actor_target_x/y_dest from clamped target_x/target_y, then
        // calls stage_actor_path_to_target to build the walking path
        // for the resolved actor.
        // ------------------------------------------------------------
        ldx     actor                          // X := actor index for destination update
        lda     target_x                         // Load finalized X target
        sta     actor_target_x,x                 // Commit actor’s horizontal destination

        lda     target_y                         // Load finalized Y target
        sta     actor_target_y,x                 // Commit actor’s vertical destination

        jsr     stage_actor_path_to_target     // Build and stage walking path toward target_x/target_y
exit_bare_walk:
        rts

        // ------------------------------------------------------------
        // Push a complete sentence for later execution
        // ------------------------------------------------------------
push_sentence:
        // ------------------------------------------------------------
        // Advance stack index
		//
        // Increments sentstk_top_idx to point to the next free
        // slot and loads it into X for writing the stacked sentence
        // tokens.
        // ------------------------------------------------------------
        inc     sentstk_top_idx           // Advance stack pointer to next free slot
        ldx     sentstk_top_idx           // X := current stack entry index for storing tokens

        // ------------------------------------------------------------
        // Push current sentence tokens into stack entry X
		//
        // Stores: verb, DO lo/hi, preposition, IO lo/hi into the
        // parallel stacked_sentence_* arrays at index X.
        // ------------------------------------------------------------
        lda     current_verb_id                    
        sta     stacked_verb_ids,x         

        lda     direct_object_idx_lo            	 
        sta     stacked_do_id_lo,x  
        lda     direct_object_idx_hi            	 
        sta     stacked_do_id_hi,x  
		
        lda     current_preposition                    		 
        sta     stacked_prep_ids,x  	 

        lda     indirect_object_idx_lo          		
        sta     stacked_io_id_lo,x  	
        lda     indirect_object_idx_hi          		
        sta     stacked_io_id_hi,x   

        // ------------------------------------------------------------
        // Post-stack verb reset gate
		//
        // If current_verb_id == VERB_WALK_TO, skip UI reset and exit;
        // otherwise fall through to reset defaults.
        // ------------------------------------------------------------
        lda     current_verb_id                 // Reload current verb for post-stack reset test
        cmp     #VERB_WALK_TO                  	// Z=1 if it was already "Walk to"
        beq     finalize_and_exit              	// If so, skip UI verb reset and exit

        // ------------------------------------------------------------
        // Reset UI verb defaults after pushing
		//
        // Revert current_verb_id to VERB_WALK_TO and clear DO/preposition
        // so the input bar idles on a neutral “Walk to” state.
        // ------------------------------------------------------------
        lda     #VERB_WALK_TO                   // Default UI verb after pushing
        sta     current_verb_id                 // Reset current verb to "Walk to"

        lda     #$00                            
        sta     direct_object_idx_lo            // Clear direct object reference
        lda     #$00                            
        sta     current_preposition             // Clear preposition token

        // ------------------------------------------------------------
        // Finalize and exit
		//
        // Clears target_entity to indicate no pending target and
        // returns. Stack state and tokens remain committed.
        // ------------------------------------------------------------
finalize_and_exit:
        lda     #$00                            // Prepare clear value
        sta     target_entity              // Reset destination entity marker (none targeted)
        rts                                     // Return → sentence stacked or action completed
/*
================================================================================
  execute_verb_handler_for_object
================================================================================
Summary
	Run the correct verb logic for the active direct object. 
	Prefer a custom	per-object handler; otherwise fall back to defaults:

		* GIVE with a kid recipient transfers ownership and refreshes inventory.
		* WALK TO does nothing and returns.
		* All other verbs run the global “defaults” script (#3).
		  A guard enforces that READ requires lights; if dark, use defaults.

Global Inputs
	active_do_id_lo/hi				       Object to act on
	active_verb_id                         Verb to execute
	active_io_id_lo                        Recipient for GIVE
	object_attributes[]                    Per-object attribute byte
	global_lights_state                    Nonzero if lights are on
	room_obj_ofs                           Per-room base offset for object scripts

Global Outputs
	sentence_bar_needs_refresh             Set TRUE to redraw the sentence bar
	task_script_idx_tbl         Bound to object’s resource for script VM
	task_type_tbl            Script type associated with resource
	task_cur_idx                    Cleared before launching scripts
	task_pc_ofs_lo_tbl/hi				   Computed handler offset (custom path)
	object_attributes[x]                   Updated owner on GIVE→kid
	(refresh) refresh_inventory      Invoked after ownership change

Description
	* Mark UI for refresh.
	* Resolve active DO’s resource and bind it to the script slot metadata.
	* Look up a custom handler for active_verb_id:
		  • If absent:
			  – If verb = GIVE and IO is a kid: write new owner into object_attributes
			  and refresh inventory; return.
			  – If verb = WALK TO: return.
			  – Else launch the global defaults script (#3) with var_active_verb_id.
		  • If present:
			  – If verb = READ and lights are off: fall back to defaults script.
			  – Else compute absolute script offset (room_obj_ofs + handler_ofs),
			  seed VM state (script_offsets_*), set base and read address, 
			  then dispatch_script_ops_loop.
================================================================================
*/
* = $0DC5
execute_verb_handler_for_object:
        // ------------------------------------------------------------
        // Mark sentence bar for refreshing
        // ------------------------------------------------------------
        lda     #TRUE                           
        sta     sentence_bar_needs_refresh      // redraw on next UI pass

        // ------------------------------------------------------------
        // Get the active DO's resource
        // ------------------------------------------------------------
        ldx     active_do_id_lo                 // X := active object id (low byte)
        lda     active_do_id_hi                 // A := active object id (high byte)
        jsr     resolve_object_resource         // resolve (A:hi, X:lo) → resource; returns Y=index, A=script type
        sty     task_script_idx_tbl  // stash Y: resource index for script slot
        sta     task_type_tbl     // stash A: script type for this slot (room/global)

        // ------------------------------------------------------------
        // Get the object's script handler for the verb, if any
        // ------------------------------------------------------------
        lda     active_verb_id                  // A := current verb id to check
        jsr     find_object_verb_handler_offset // lookup custom handler offset for this verb/object combo → A := offset (0 if none)

        // ------------------------------------------------------------
        // Did we find a handler?
        // ------------------------------------------------------------
        bne     guard_read_requires_light       // if A ≠ 0 → handler found → branch to run or guard-read check path

        // ------------------------------------------------------------
        // No handler found → run default handlers
        // ------------------------------------------------------------
		
		// Check if verb is "Give"
        lda     active_verb_id                  
        cmp     #VERB_GIVE                      
        bne     guard_walk_to_early_exit        // if not "Give" → skip to walk-to check

        // ------------------------------------------------------------
        // "Give to another kid" special case processing
        // ------------------------------------------------------------
        lda     active_io_id_lo                 // A := recipient id
        cmp     #FIRST_NON_KID_INDEX            // set C if A ≥ FIRST_NON_KID_INDEX → recipient is not a kid
        bcs     return_after_give_path          // not a kid → skip fast GIVE transfer and return

        ldx     active_do_id_lo                 // X := index of direct object whose ownership may change
        lda     object_attributes,x             // A := attribute byte for that object (hi nibble = preserved flags, lo nibble = owner)
        and     #MSK_HIGH_NIBBLE                // keep hi nibble only; clear owner nibble
        ora     active_io_id_lo                 // merge new owner id into lo nibble (recipient kid index)
        sta     object_attributes,x             // commit updated owner nibble back to object_attributes[X]
		
        jsr     refresh_inventory         // refresh inventory UI to reflect new ownership
		
return_after_give_path:
        rts

guard_walk_to_early_exit:
        // ------------------------------------------------------------
        // If verb is "Walk to", exit
        // ------------------------------------------------------------
        cmp     #VERB_WALK_TO                   
        beq     return_after_walk_to            

launch_global_defaults_script:
        // ------------------------------------------------------------
        // Run global "verb defaults" script (#3)
        // ------------------------------------------------------------
        lda     active_verb_id                  // A := current verb id
        sta     var_active_verb_id              // copy to debug var
		
        lda     #TASK_IDX_NONE               // A := sentinel slot (no existing script)
        sta     task_cur_idx             // mark no script bound to this slot
		
        lda     #GLOBAL_DEFAULTS_SCRIPT_ID      // A := id of global defaults script
        jsr     launch_global_script             // start script #3 using seeded verb
		
return_after_walk_to:
        rts                                     // return to caller (no further handling needed)

guard_read_requires_light:
        // ------------------------------------------------------------
        // Handle "Read" verb guard (requires light)
        // ------------------------------------------------------------
        pha                                     // save handler offset (A) for later use
		
		// Verb is "Read"?		
        lda     active_verb_id                  
        cmp     #READ_VERB                      
        bne     launch_custom_handler           // not READ → proceed with custom handler

		// Lights are on?
        lda     global_lights_state             
        bne     launch_custom_handler           // lit → allow custom handler

		// Reading with lights off
        pla                                     // darkness: restore A so stack stays balanced
        jmp     launch_global_defaults_script   // skip custom handler; run global defaults instead

launch_custom_handler:
        // ------------------------------------------------------------
        // Execute custom handler
        // ------------------------------------------------------------
        pla                                     // A := handler offset (restore value saved in guard)
		
		// Build pointer to script
        clc                                     
        adc.zp  room_obj_ofs_lo                   // A := low(addr) = handler_ofs + room_obj_ofs.lo
        sta     task_pc_ofs_lo_tbl               
        lda     #$00                            
        adc.zp  room_obj_ofs_hi
        sta     task_pc_ofs_hi_tbl               // save high byte of script address

		//Select script slot 0
        lda     #$00                            
        sta     task_cur_idx             
		
		//Copy IO to debug var
        lda     active_io_id_lo                 
        sta     var_active_io_id_lo             
		
		//Execute script
        jsr     set_script_base_from_type// set resource base for this script context
        jsr     set_current_task_pc // set PC/read pointer to script_offsets_{hi,lo}
        jsr     dispatch_script_ops_loop          // execute first script opcode
        rts                                     
/*
================================================================================
  find_object_verb_handler_offset
================================================================================
Summary
	Scan an object’s verb-handler table and return the handler script offset for a
	requested verb. Supports a wildcard “default” handler and a table terminator.

Arguments
	.A      			Requested verb id to look up

Vars/State
	verb_index      	Latched copy of requested verb id for comparisons

Global Inputs
	object_rsrc_ptr     ZP pointer to start of the object resource

Returns
	.A      Handler script offset (nonzero) if a matching entry is found
			DEFAULT_VERB_WALK_TO (#$0D) if no entry and verb == Walk to
			NO_HANDLER_RET (#$00) if no entry and verb ≠ Walk to

Description
	* Initialize Y so the first INY, INY lands on the first handler entry at
	  VERB_TABLE_START_OFS: each entry is {verb_id, handler_ofs}.
	* Loop over entries:
	  • If verb_id == #$00 → end of table:
		  – If requested verb == VERB_WALK_TO, return #$0D.
		  – Else return NO_HANDLER_RET.
	  • If verb_id == DEFAULT_VERB → match any verb: return its handler_ofs.
	  • If verb_id == requested verb → return its handler_ofs.
	  • Otherwise advance to the next pair and continue scanning.

Notes
	* Table encoding: pairs of one-byte verb_id followed by one-byte offset,
	  terminated by verb_id == #$00.
================================================================================
*/
* = $0E4A
find_object_verb_handler_offset:
        // Store requested verb index
        sta     verb_index                     

        // Initialize scan offset (first entry at +VERB_TABLE_START_OFS)
        ldy     #VERB_SCAN_SEED_Y              // Y := seed so first INY,INY lands on the first {verb_id, offset} pair

scan_next_verb_entry:
        // ------------------------------------------------------------
        // Advance two bytes to next handler entry
        // ------------------------------------------------------------
        iny                                     // Y := Y + 1 → move to next pair byte 0 (verb_id)
        iny                                     // Y := Y + 1 → move to next pair byte 1 (offset)

        // ------------------------------------------------------------
        // Read verb id from handler pair
        // ------------------------------------------------------------
        lda     (object_rsrc_ptr),y             // A := verb_id at table[Y]; checks for $00 terminator next

        // ------------------------------------------------------------
        // End of table marker?
        // ------------------------------------------------------------
        bne     evaluate_default_handler        // if A ≠ $00 → not end; check default/exact match next
                                                // if A = $00 → end-of-table; fall through to no-handler logic

        // ------------------------------------------------------------
        // Reached end of handlers list
		//
        // Return #$0D if VERB_WALK_TO, else #$00
        // ------------------------------------------------------------
        lda     verb_index                  // A := requested verb id
        cmp     #VERB_WALK_TO               // if requested verb is WALK TO, branch with A unchanged (VERB_WALK_TO)
        beq     return_no_handler           // else continue
		
        lda     #NO_HANDLER_RET             // A := $00 (no handler sentinel)
return_no_handler:
        rts                                 // return; if WALK TO path taken, A == VERB_WALK_TO

evaluate_default_handler:
        // ------------------------------------------------------------
        // Default handler?
        // ------------------------------------------------------------
        cmp     #DEFAULT_VERB                 // A (verb_id) == DEFAULT_VERB ? → default entry
        bne     compare_verb_id               // not default → check for exact verb match
        jmp     return_handler_offset         // default matched → return the next byte as handler offset

compare_verb_id:
        // ------------------------------------------------------------
        // Compare handler’s verb id with requested verb
        // ------------------------------------------------------------
        cmp     verb_index                    // compare table verb_id vs requested verb
        bne     scan_next_verb_entry          // not equal → advance to next {verb_id,offset} pair

return_handler_offset:
        // ------------------------------------------------------------
        // Match - return next byte as handler script offset
        // ------------------------------------------------------------
        iny                                     // Y := Y + 1 → move from verb_id to offset byte
        lda     (object_rsrc_ptr),y             // return A := handler script offset (nonzero expected by caller)
        rts                                     
/*
================================================================================
  has_pickup_script_for_sentence_part
================================================================================
Summary
	Checks whether the selected sentence object (direct or indirect) defines a
	custom “Pick Up” verb handler. Actor objects are rejected up front.

Arguments
	.A      Selector for which object to test:
			ARG_IS_DO → check direct object
			ARG_IS_IO → check indirect object

Global Inputs
	sentstk_top_idx         Top index into the sentence stacks
	stacked_do_id_lo/hi     Direct-object ID pair at each stack entry
	stacked_io_id_lo/hi     Indirect-object ID pair at each stack entry

Returns
	.A      TRUE  if a Pick Up handler exists for the chosen object
			FALSE if absent or the target is an actor

Description
	* Read the current stack entry (sentstk_top_idx) and select DO or IO per .A.
	* If the object type equals OBJ_TYPE_COSTUME, return FALSE (actors aren’t pickable).
	* Otherwise resolve the object resource, then scan its handler table for
	  VERB_PICK_UP using find_object_verb_handler_offset.
	* Return TRUE if a nonzero handler offset is found; else return FALSE.
	* X and Y are preserved via temp_x_2 and temp_y_2.
================================================================================
*/
* = $0E73
has_pickup_script_for_sentence_part:
        // Save X and Y
        stx     temp_x_2                         
        sty     temp_y_2                         

        // Load current stack index into Y
        ldy     sentstk_top_idx                // Y := top sentence stack index to select DO/IO ids

        // ------------------------------------------------------------
        // DO vs IO selector: A == #ARG_IS_DO → direct object
        // ------------------------------------------------------------
        cmp     #ARG_IS_DO                    
        bne     load_indirect_object_index    // not DO (Z=0) → jump to Indirect Object load

        // ------------------------------------------------------------
        // Load direct object index (lo→X, hi→A)
        // ------------------------------------------------------------
        ldx     stacked_do_id_lo,y             
        lda     stacked_do_id_hi,y             
        jmp     check_actor_class              

load_indirect_object_index:
        // ------------------------------------------------------------
        // Load indirect object index (lo→X, hi→A)
        // ------------------------------------------------------------
        ldx     stacked_io_id_lo,y             
        lda     stacked_io_id_hi,y             

check_actor_class:
        // ------------------------------------------------------------
        // Object class gate: if it's an actor, it's not pickable
        // ------------------------------------------------------------
        cmp     #OBJ_TYPE_COSTUME                		// compare hi-byte/type with actor code
        bne     resolve_and_check_pickup_handler 	// not an actor → proceed to resource/handler check

        // ------------------------------------------------------------
        // Not pickable → return False
        // ------------------------------------------------------------
        lda     #FALSE                         
        rts                                    

resolve_and_check_pickup_handler:
        // ------------------------------------------------------------
        // Resolve object and query Pick Up handler
        // ------------------------------------------------------------
        jsr     resolve_object_resource         // set object context using A:hi, X:lo id
        lda     #VERB_PICK_UP                    // A := verb id for "Pick Up"
        jsr     find_object_verb_handler_offset // A := handler offset (0 if none)

        // Nonzero → script exists
        bne     pickup_handler_present          // offset ≠ 0 → handler present → return TRUE

        // ------------------------------------------------------------
        // No script → return False
        // ------------------------------------------------------------
        lda     #FALSE                          
        jmp     has_object_pickup_exit          

pickup_handler_present:
        // ------------------------------------------------------------
        // Script present → return True
        // ------------------------------------------------------------
        lda     #TRUE                           

has_object_pickup_exit:
        // Restore X and Y, then return
        ldx     temp_x_2                          
        ldy     temp_y_2                          
        rts                                     
/*
================================================================================
 is_sentence_object_owned_by_current_kid
================================================================================
Summary
	Determine if the selected sentence object (direct or indirect) is owned by the
	current kid. Uses the sentence stack top index to select DO or IO, then checks
	inventory flags and the owner nibble.

Arguments
	.A      Selector for which object to check:
				ARG_IS_DO → check direct object
				ARG_IS_IO → check indirect object

Global Inputs
	sentstk_top_idx         Top index into the sentence stacks
	stacked_do_id_lo/hi     Direct object ID pair for each stack entry
	stacked_io_id_lo/hi     Indirect object ID pair for each stack entry
	object_attributes[]     Per-object attribute byte; low nibble = owner id
	current_kid_idx         Index of the current kid


Returns
	.A      TRUE   	if the object is in current kid’s inventory
			FALSE  	otherwise

Description
	* Select DO or IO from the sentence stacks using sentstk_top_idx.
	* The object’s high-byte ID acts as an “in some inventory” flag:
	  #$00 → the object is in an inventory; nonzero → not in any inventory.
	* If in an inventory, read object_attributes[y], mask the owner nibble, and
	  compare to current_kid_idx. Match → FOUND, else NOT_FOUND.
	* X is preserved via temp_x_2; Y exits holding the object index used for lookup.
================================================================================
*/
* = $0EAE
is_sentence_object_owned_by_current_kid:
        // Save X register to temporary
        stx     temp_x_2                          

        // Fetch index of current sentence part being analyzed
        ldx     sentstk_top_idx                 // X := top-of-stack sentence index

        // Are we checking for a direct object?
        cmp     #ARG_IS_DO                      
        bne     load_indirect_object_id         // not DO → use IO path

        // ------------------------------------------------------------
        // Direct object path
        // ------------------------------------------------------------
		// Load DO id into Y/A
        ldy     stacked_do_id_lo,x              
        lda     stacked_do_id_hi,x              
        jmp     guard_in_some_inventory         

load_indirect_object_id:
        // ------------------------------------------------------------
        // Indirect object path
        // ------------------------------------------------------------
		// Load IO id into Y/A
        ldy     stacked_io_id_lo,x              
        lda     stacked_io_id_hi,x              

guard_in_some_inventory:
        // ------------------------------------------------------------
		// Check if object in somebody's inventory
		//
        // The object's hi byte represents if it's in somebody's
        // inventory (#00 = in inventory, otherwise not)
        // ------------------------------------------------------------
        beq     compare_owner_with_current_kid  // A==0 → object is in some inventory → check owner

        // ------------------------------------------------------------
        // Not in any inventory → return FALSE
        // ------------------------------------------------------------
        lda     #FALSE         
        rts                                     // early exit

compare_owner_with_current_kid:
        // ------------------------------------------------------------
        // Check if the item belongs to the current kid
        // ------------------------------------------------------------
        lda     object_attributes,y             // A := attribute byte for object index Y
        and     #MSK_LOW_NIBBLE                 // keep owner nibble (low 4 bits)
        cmp     current_kid_idx                 // is owner equal to current kid index?
        bne     return_not_owned_by_current_kid // no → not owned by current kid

        // ------------------------------------------------------------
        // In current kid's inventory → return TRUE
        // ------------------------------------------------------------
        lda     #TRUE          
        jmp     exit_inv_check                  // restore and return

return_not_owned_by_current_kid:
        // ------------------------------------------------------------
        // Not in current kid's inventory → return FALSE
        // ------------------------------------------------------------
        lda     #FALSE           

exit_inv_check:
        // Restore X register and return
        ldx     temp_x_2                          
        rts                                     
/*
================================================================================
  push_pickup_for_sentence_part
================================================================================
Summary
	Push a new “Pick Up <object>” sentence onto the sentence stack for either the
	direct object (DO) or the indirect object (IO) selected in the current stack
	entry. Copies the chosen object ID, clears the preposition, and writes the
	VERB_PICK_UP into the stacked verb list.

Arguments
	.A      Complement selector:
				ARG_IS_IO → use indirect object from current entry
				ARG_IS_DO → use direct object from current entry

Vars/State
	object_ptr              ZP scratch: receives selected object id (lo/hi)

Global Inputs
	sentstk_top_idx         Current sentence stack top index
	stacked_do_id_lo/hi     DO id pairs at each stack entry
	stacked_io_id_lo/hi     IO id pairs at each stack entry

Global Outputs
	sentstk_top_idx         Incremented on successful push
	stacked_verb_ids[x]     Written with VERB_PICK_UP
	stacked_prep_ids[x]     Cleared to "no preposition"
	stacked_do_id_lo/hi[x]  Written with selected object id

Description
	* Preserve X in temp_x_2.
	* Read current stack top (sentstk_top_idx). Select DO or IO based on .A.
	* Copy the chosen object id into object_ptr (lo/hi).
	* Increment sentstk_top_idx and range-check against SENT_STACK_MAX_TOKENS.
		  • On overflow: write debug_error_code (#$2D), map I/O (cpu_port ← MAP_IO_IN),
		  set vic_border_color_reg and loop forever.
		  • Otherwise: write VERB_PICK_UP, clear stacked_prep_ids, and store the
		  object id into stacked_do_id_lo/hi at the new top.
	* Restore X and return.

Notes
	* This is a push-only helper. It does not validate whether the object
	  actually supports a Pick Up handler.
	* The push writes the object into the DO fields by design; PICK_UP operates
	  on a single direct object without a preposition or IO.
================================================================================
*/
* = $0EE1
push_pickup_for_sentence_part:
        // Save X
        stx     temp_x_2                          

        // Fetch current stack index
        ldx     sentstk_top_idx                 // X := top sentence stack slot to read/write

        // Is the sentence part the indirect object? 
        cmp     #ARG_IS_IO                      
        bne     fetch_direct_object             // not IO → use DO path

        // ------------------------------------------------------------
        // Indirect object → copy indices into object_ptr
        // ------------------------------------------------------------
        lda     stacked_io_id_lo,x              
        sta     object_ptr                     
        lda     stacked_io_id_hi,x              
        sta     object_ptr + 1
        jmp     next_sentence_index             // join common path to push and advance

fetch_direct_object:
        // ------------------------------------------------------------
        // Direct object → copy indices into object_ptr
        // ------------------------------------------------------------
        lda     stacked_do_id_lo,x              
        sta     object_ptr                     
        lda     stacked_do_id_hi,x              
        sta     object_ptr + 1

next_sentence_index:
        // ------------------------------------------------------------
        // Advance stack index and validate range
        // ------------------------------------------------------------
        inc     sentstk_top_idx                 // bump top index to allocate a new sentence entry
        ldx     sentstk_top_idx                 // X := new top index
        cpx     #SENT_STACK_MAX_TOKENS          
        bne     push_pickup_obj                 // stack index within range → continue to push new entry

        // ------------------------------------------------------------
        // Invalid sentence part index → debug hang
        // ------------------------------------------------------------
        lda     #$2D                            // A := error code $2D
        sta     debug_error_code                // record error for diagnostics
        ldy     #MAP_IO_IN                      
        sty     cpu_port                        // enable I/O mapping
hangup_loop:
        sta     vic_border_color_reg            // change border color
        jmp     hangup_loop                     // halt here for debugging

push_pickup_obj:
        // ------------------------------------------------------------
        // Push a new “Pick Up” sentence for the selected object
        // ------------------------------------------------------------
        // push "Pick up" verb
		lda     #VERB_PICK_UP                    
        sta     stacked_verb_ids,x              
		
		// clear preposition for this entry
        lda     #PREPOSITION_NONE
        sta     stacked_prep_ids,x              
		
		// copy object ID
        lda     object_ptr                     
        sta     stacked_do_id_lo,x              
        lda     object_ptr + 1
        sta     stacked_do_id_hi,x              

        // Restore X and return
        ldx     temp_x_2                          
        rts                                     
		
/*
================================================================================
  switch_active_kid_if_different
================================================================================
Summary
	Switch control to the selected kid if it differs from the current one,
	stop any running script, recenter the camera, refresh inventory, then
	fall through to sentence/UI re-initialization.

Arguments
    X            			   UI slot index of the selected kid (indexes kid_ids[]).

Global Inputs
	kid_ids[]                  lookup table: UI slot → kid id
	current_kid_idx            active kid id
	task_cur_idx        active script slot id

Global Outputs
	current_kid_idx            ← kid_ids[X]        (if different)
	task_cur_idx        ← $FF               (script stopped)

Description
	• Load kid id from kid_ids[X].
	• If it matches current_kid_idx, return with no changes.
	• Otherwise:
		– Write new kid id to current_kid_idx.
		– Stop any running script by setting task_cur_idx to $FF.
		– Recenter camera on the new kid’s actor.
		– Refresh the items/inventory display.
	• Execution then falls through to init_sentence_ui_and_stack to reset
	the verb/sentence UI.
================================================================================
*/
* = $29AE
switch_active_kid_if_different:
        // X := desired index, A := kid id for that index
        tax                                     // X := desired kid list index (preserve A)
        lda     kid_ids,x                       // A := kid id at index X

        // If already current, exit
        cmp     current_kid_idx                 
        bne     commit_kid_change               // different → commit change
        rts                                     // same kid → no work

commit_kid_change:
        // Commit new kid and stop scripts
        sta     current_kid_idx                 // set active kid id
        sta     current_kid_idx                 // redundant write (kept to mirror original binary)
        lda     #$FF                            // A := sentinel "no active script slot"
        sta     task_cur_idx             // stop or clear current script

        // Recenter camera and refresh inventory
        lda     current_kid_idx                 // A := new active kid for camera routine
        jsr     cam_follow_costume             // center viewport on new kid
        jsr     refresh_inventory         // rebuild inventory UI for new kid
		//Fall through to init_sentence_ui_and_stack
/*
================================================================================
  init_sentence_ui_and_stack
================================================================================
Summary
	Reset the verb/sentence UI to a known baseline and clear any pending
	command. Set default verb to VERB_WALK_TO and mark the UI for redraw.

Global Outputs
	target_entity             ← ENTITY_NONE
	sentence_bar_needs_refresh     ← TRUE
	sentstk_free_slots   		   ← SENT_STACK_MAX_TOKENS
	sentstk_top_idx           	   ← SENT_STACK_EMPTY_IDX
	current_verb_id                ← VERB_WALK_TO
	direct_object_idx_lo           ← $00
	direct_object_idx_hi           ← $00
	preposition                    ← $00
	indirect_object_idx_lo         ← $00
	indirect_object_idx_hi         ← $00

Description
	• Clear any active destination so pathing is idle.
	• Request a sentence bar refresh on next UI pass.
	• Restore sentence stack to empty with full capacity.
	• Set default verb to “Walk to”.
	• Clear all sentence complements: DO, preposition, IO.
================================================================================
*/
* = $29CC
init_sentence_ui_and_stack:
		// Clear current destination entity
        lda     #ENTITY_NONE                   
        sta     target_entity             

		// Request sentence bar redraw
        lda     #TRUE                          
        sta     sentence_bar_needs_refresh     

		//Empty the stack
        lda     #SENT_STACK_MAX_TOKENS         // A := max stack size
        sta     sentstk_free_slots             // reset free-slot counter

        lda     #SENT_STACK_EMPTY_IDX          // A := sentinel “empty”
        sta     sentstk_top_idx                // mark stack as empty

		//Set default verb
        lda     #VERB_WALK_TO                  
        sta     current_verb_id                // set current verb to WALK TO

		//Clear DO, IO and preposition
        lda     #$00                           
        sta     direct_object_idx_lo           // clear DO
        sta     direct_object_idx_hi           
        sta     current_preposition            // clear preposition
        sta     indirect_object_idx_lo         // clear IO
        sta     indirect_object_idx_hi         

        rts                                    
