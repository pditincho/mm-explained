/*
================================================================================
render_object.asm
================================================================================

Summary
        Rendering pipeline for room objects on a 40-column tile screen.
        Provides an overlay-only pass with ancestor policy, horizontal
        frustum culling, and a trimmed-window decompressor that writes
        tiles, color, and mask layers.

Public routine
        render_room_objects        Descend room object list, cull, then gate by visibility.
		
Private routines		
        render_object_if_visible   X-axis culling vs viewport and scroll state.
        decode_object_gfx          Resolve source, trim to viewport, emit three layers.
        decode_trimmed_window      Row-wise decode with left/right stream skips.

Dependencies
        decompressor.asm     	   stream decoder

Global Inputs
        current_room                      Active room id.
        room_obj_count             Loop basis (implementation must match count vs last-index).
        room_ptr_lo/hi_tbl				  Per-room base pointer tables.
        room_obj_id_lo/hi_tbl			  Map room slot → object INDEX (lo) and immutability flag (hi).
        object_attributes[]               Per-object attribute byte; bit7 = OVERLAY_MASK.
        parent_idx_tbl[]                  Parent relation as a room object INDEX; 0 = no parent.
        ancestor_overlay_req_tbl[]        Per-object required ancestor overlay state.
        viewport_left_col/right_col       Visible window columns (inclusive).
        screen_scroll_flag                $00=scroll right, $FF=scroll left, other=no scroll.
        obj_*_tbl[]                       Geometry and stream pointers for objects.
        screen_row_offsets_{hi,lo}[]      Precomputed row offsets (stride = 40 bytes).

Global Outputs (selected)
        room_rsrc_base                    Zero-page pointer to current room resource base (lo/hi).
        decomp_src_ptr                    Source pointer for the decompressor (lo/hi).
        destination_ptr                   Per-layer destination pointer, advanced per row.
        src_skip_left/right               Horizontal trims in columns.
        cols_to_draw / rows_to_draw       Visible window dimensions in columns/rows.

Behavioral Notes
        - Overlay pass: objects with bit7=0 are culled before any parent checks.
        - Ancestor policy: each ancestor’s bit7 must equal the child’s requirement
          (ancestor_overlay_req_tbl) or the child is culled.
        - Visibility: render only if object’s [left, right) intersects the viewport.
          During scrolling, collapse to a single incoming column to minimize work.
        - Immutable/background: entries with nonzero room_obj_id_hi_tbl are skipped.

Assumptions and Constraints
        - No parent cycles; parent_idx_tbl either 0 or a valid room object index.
        - id/index tables are consistent; attribute lookups use INDEX space.
================================================================================

Overlay redraw flag (OVERLAY_MASK) and script linkage

Most room art is baked into the background; many objects are initially identical
to that background. Such objects do not require per-frame redraw. When a script
changes an object’s visual state (e.g., the doormat after pickup), it must be
overdrawn on top of the existing background. We encode this need with bit7 of
object_attributes (OVERLAY_MASK).

Game scripts toggle this via opcode setState08:
- set bit7 (1): object must be overdrawn (dynamic overlay present).
- clear bit7 (0): object is visually identical to the background; skip redraw.

The renderer’s overlay-only pass first culls any object with bit7=0. Surviving
candidates may then be constrained by ancestor policy: each parent’s bit7 must
match the per-child requirement (ancestor_overlay_req_tbl). Any mismatch culls
the child, preventing redundant overdraws beneath a container’s overlay state.

This design minimizes draw cost, avoids double-painting static art, and keeps
scripted state changes tightly coupled to rendering through a single bit.

================================================================================

Ancestor overlay requirement (per-child parent bit7 policy)

Each drawable child object carries an ancestor overlay requirement that dictates
the expected overlay state (bit7) of every ancestor up to the root. The engine
enforces this with a climb: at each step it compares the parent’s bit7
(object_attributes[parent] & OVERLAY_MASK) against the child’s requirement:
  - required = $80 → every ancestor must have bit7=1
  - required = $00 → every ancestor must have bit7=0
A mismatch at any ancestor culls the original child.

Example A (outside): Front door key under the doormat
- Key: required = $80 (True). Parent: Doormat.
- Doormat bit7=1 while it’s an active overlay (e.g., moved/revealed state).
- Since parent bit7 (1) matches required (1), the chain passes → the key will
  be overlaid when the doormat is overlaid (moved).

Example B (living room): Old rusty key (hanging) from the glass chandelier
- Key: required = $00 (False). Parent: Chandelier.
- Chandelier bit7=0 while it is part of the baked background (no overlay).
- Since parent bit7 (0) matches required (0), the chain passes → the key (hanging) will be
  be overlaid when the chandelier is NOT overlaid (that is, in its original state). 
  If the chandelier later becomes overlaid (when dropped), the mismatch (1 vs required 0) 
  culls the key. Instead, a script will put a new object on the floor (old rusty key) next to the
  broken chandelier.

================================================================================
*/

#importonce
#import "globals.inc"
#import "constants.inc"
#import "decompressor.asm"

// decode_object_gfx
.label obj_left_col            = $3E9C   // object X start column (world/view coords)
.label obj_top_row             = $3E9D   // object Y start row index
.label obj_width               = $3E9E   // object span in columns
.label decompress_candidate    = $3EA0   // temp: saved object index (X) during decode
.label screen_row_offset       = $1B     // zp: current screen row base offset (lo/hi at $1B/$1C)
.label room_rsrc_base          = $C3     // zp: room resource base pointer (lo/hi at $C3/$C4)

// decode_trimmed_window
.label obj_rows_remaining      = $15     // zp: rows left to output in this object
.label destination_ptr         = $19     // zp: screen dest pointer (lo/hi)
.label src_skip_left           = $3E99   // columns to skip on the left from source
.label src_skip_right          = $3E9A   // columns to skip on the right from source
.label cols_to_draw            = $3E9B   // columns to draw after trimming
.label rows_to_draw            = $3E9F   // total rows to draw in this pass
.const ROW_STRIDE              = VIEW_COLS   // bytes per screen row (visible columns)

// render_room_objects
.label draw_candidate_idx      = $3EA0   // candidate object index saved for render/advance

.const OVERLAY_MASK            = $80     // attributes bit7: needs overlay redraw


/*
================================================================================
render_room_objects
================================================================================

Summary
        Scan room objects in descending order. Render only those flagged for
        overlay redraw and whose ancestor chain satisfies the required overlay
        state. Visibility on viewport is checked before decoding.

Arguments
        None (uses globals and tables; X/A/Y are scratch)

Global Inputs
        current_room                   Active room id used to load room base pointer.
        room_obj_count          Loop basis. 
        room_ptr_lo/hi_tbl             Per-room base pointer tables (lo/hi).
        room_obj_id_lo_tbl             Map from loop slot to object ID used by
                                       object_attributes[] lookups.
        room_obj_id_hi_tbl             High byte associated with the slot; used here
                                       as “immutable/background” test when nonzero.
        object_attributes              Attribute array indexed by object INDEX.
        parent_idx_tbl                 Parent relation as a ROOM OBJECT INDEX; 0 = no parent.
        ancestor_overlay_req_tbl       Per-object required ancestor overlay state
                                       ($00 require clear, $80 require set).
									   
Vars/State
        draw_candidate_idx        		Saved candidate object index for render test.
        ancestor_overlay_req       		Required ancestor overlay state ($00 or $80).
        room_rsrc_base            		Active room base pointer (lo/hi) for resource access.


Description
        - Early exit if room has no objects.
        - Load room_rsrc_base from current_room.
        - For each object (descending):
          - Cull immutable/background objects.
          - Quick gate: require OVERLAY bit (bit7) set on the object.
          - Save candidate index.
          - Parent chain filter:
            - required := ancestor_overlay_req_tbl[candidate]
            - parent   := parent_idx_tbl[candidate]
            - While parent != 0:
              - if (obj_attr[parent] & OVERLAY_MASK) != required → cull
              - candidate := parent; parent := parent_idx_tbl[candidate]
          - If passed, restore saved index and call render_object_if_visible.
================================================================================
*/
* = $3C92
render_room_objects:
        // ----------------------------------------
        // Early out if room has zero objects
        // ----------------------------------------
        ldx     room_obj_count        // Load room object last-index/count; Z=1 if zero
        bne     load_room_base_ptr           // X != 0 → proceed with setup and loop
        rts                                  // No objects → return early

load_room_base_ptr:
        // ----------------------------------------
        // Refresh room_rsrc_base = room_ptr[current_room] 
        // ----------------------------------------
        ldx     current_room                 // X := current room id
        lda     room_ptr_lo_tbl,x            // A := room base lo
        sta     room_rsrc_base               // room_rsrc_base.lo = A
        lda     room_ptr_hi_tbl,x            // A := room base hi
        sta     room_rsrc_base + 1               // room_rsrc_base.hi = A

        // ----------------------------------------
        // Main scan loop: X := last index (descending)
		// Note: X is indexing per-room objects (not global object IDs)
        // ----------------------------------------
        ldx     room_obj_count        // Initialize descending loop: X = last object index (or count-1)

scan_objects_desc:
        // ----------------------------------------
        // Immutable cull: objects with nonzero index_hi are always pre-baked in the background
        // ----------------------------------------
        lda     room_obj_id_hi_tbl,x       		 // Fetch immutability flag/high index byte
        bne     advance_to_prev_object       // ≠0 → immutable → skip

        // ----------------------------------------
        // If the object is not requiring an overlay drawing, cull it
        // - obj_idx := room_obj_id_lo_tbl[X]
        // - if (object_attributes[obj_idx] & OVERLAY_MASK) == 0 → skip
        // ----------------------------------------
        ldy     room_obj_id_lo_tbl,x      		// Y := object index mapped from X
        lda     object_attributes,y         // A := attributes; N reflects bit7 (OVERLAY_MASK)
        bpl     advance_to_prev_object      // N=0 → bit7 clear → skip candidate

        // ----------------------------------------
        // Mark this X as draw candidate
        // ----------------------------------------
        stx     draw_candidate_idx           	  // Save X as candidate to test/draw

climb_parent_chain_check:
        lda     ancestor_overlay_req_tbl,x        // Load required ancestor overlay state
        sta     ancestor_overlay_req              // Save requirement for upcoming compare

        lda     parent_idx_tbl,x                  // Load parent INDEX (0 = no parent)
        bne     fetch_parent_and_test             // Nonzero → check parent’s overlay state against requirement

        // No parent → ancestor test passes; proceed to visibility/render
        jmp     test_and_render_candidate

fetch_parent_and_test:
        tay                                     // Y := parent INDEX (1..N)
        lda     room_obj_id_lo_tbl,y                 // A := parent OBJECT-ID from map (index→id)
        tax                                     // X := parent OBJECT-ID for next lookup

        lda     object_attributes,x             // A := parent attributes (addressed by ID here)
        and     #OVERLAY_MASK                   // check overlay state
        cmp     ancestor_overlay_req            // does it match required state?
        bne     cull_candidate                  // no → cull original candidate

        tya                                     // X := parent INDEX to continue climb
        tax
        jmp     climb_parent_chain_check        // check next ancestor

cull_candidate:
        // ----------------------------------------
        // Skip this candidate and resume the descending loop
        // ----------------------------------------
        ldx     draw_candidate_idx            // restore loop index for step/branch
        jmp     advance_to_prev_object        // continue scan

test_and_render_candidate:
        // ----------------------------------------
        // Visibility gate on X; render only if intersects viewport
        // ----------------------------------------
        ldx     draw_candidate_idx            // restore candidate index for draw
        jmp     render_object_if_visible      // decode/draw if visible this frame

advance_to_prev_object:
        // ----------------------------------------
        // Loop step: X := X-1; continue until X == 0
        // ----------------------------------------
        dex                             // move to previous object index
        bne     scan_objects_desc       // X != 0 → keep scanning
        rts                             // X == 0 → done
/*
================================================================================
render_object_if_visible
================================================================================

Summary
	Frustum-cull on the X axis against the current viewport and decode the
	object’s graphics only if its X span intersects the visible window.

Arguments
	X  = object index

Global Inputs
	screen_scroll_flag   $00=right, $FF=left, other=none
	viewport_left_col    inclusive left column of visible window
	viewport_right_col   inclusive right column of visible window
	obj_left_col_tbl     per-object starting column
	obj_width_tbl        per-object width in columns
	
Description
	- Dispatch by scroll state in screen_scroll_flag:
	  - $00 (scroll right): render only if obj_left <= viewport_right_col AND obj_right > viewport_right_col.
	  - $FF (scroll left):  render only if obj_left <= viewport_left_col  AND obj_right > viewport_left_col.
	  - else (no scroll):   render only if [obj_left,obj_right) overlaps [viewport_left_col,viewport_right_col).
	- obj_left := obj_left_col_tbl[X]
	- obj_right   := obj_left + obj_width_tbl[X]
	- On pass, call decode_object_gfx then jump to next_object.
	- On fail, jump to ns_culled → next_object.
================================================================================
No Scroll: Test overlap with [viewport_left_col, viewport_right_col)

Rule to RENDER:
  obj_left ≤ viewport_right_col  AND  obj_right > viewport_left_col

Legend:
  ┌─viewport──────────────────┐
  │ LEFT          RIGHT       │
  └───────────────────────────┘
  [obj] shows object span from obj_left to obj_right


A) CULLED — entirely left or just touching LEFT
   (obj_right ≤ viewport_left_col)

                          ┌─viewport──────────────────┐
                          │LEFT                  RIGHT│
                          └───────────────────────────┘
  [ obj──────────]
  obj_left     obj_right ≤


B) CULLED — entirely right or just touching RIGHT
   (obj_left > viewport_right_col)

                          ┌─viewport──────────────────┐
                          │LEFT                  RIGHT│
                          └───────────────────────────┘                     [────obj────]
                                                                         obj_left  >


C) RENDER — overlaps window
   (obj_left ≤ viewport_right_col  AND  obj_right > viewport_left_col)

                          ┌─viewport──────────────────┐
                          │LEFT                  RIGHT│
                          └───────────────────────────┘
                [──────────── obj ────────────]
                 obj_left ≤                 obj_right >


================================================================================
Scroll Left: Test against LEFT edge

Rule to RENDER:
  obj_left ≤ viewport_left_col  AND  obj_right > viewport_left_col

Legend:

  ┌─viewport──────────────┐
  │ LEFT                  │
  └───────────────────────┘
  [obj] shows object span from obj_left to obj_right


A) CULLED — object entirely left or just touching LEFT
   (obj_right ≤ viewport_left_col)

                           ┌─viewport──────────────┐
                           │ LEFT                  │
                           └───────────────────────┘
[ obj──────────]
  obj_left     obj_right ≤


B) CULLED — object starts right of LEFT (already inside view)
   (obj_left > viewport_left_col)

                           ┌─viewport──────────────┐
                           │ LEFT                  │
                           └───────────────────────┘
                             [──────obj────────]
                              obj_left  >


C) RENDER — object crosses LEFT
   (obj_left ≤ viewport_left_col  and  obj_right > viewport_left_col)

                           ┌─viewport──────────────┐
                           │ LEFT                  │
                           └───────────────────────┘
         [──────obj──────────────]
         obj_left ≤         obj_right >

================================================================================
Scroll Right: Test against RIGHT edge

Rule to RENDER:
  obj_left ≤ viewport_right_col  AND  obj_right > viewport_right_col

Legend:
  ┌───────viewport────────┐
  │                 RIGHT │  ← viewport_right_col
  └───────────────────────┘
  [obj] shows object span from obj_left to obj_right

A) CULLED — object entirely before or exactly up to RIGHT
   (obj_right ≤ viewport_right_col)

   ┌───────────────────────┐
   │                  RIGHT│
   └───────────────────────┘
   [ obj──────────]
     obj_left      obj_right   viewport_right_col
                  ≤

B) CULLED — object starts beyond RIGHT
   (obj_left > viewport_right_col)

   ┌───────────────────────┐
   │                  RIGHT│
   └───────────────────────┘                 [────────obj────────]
                                             obj_left  >  viewport_right_col

C) RENDER — object crosses RIGHT
   (obj_left ≤ viewport_right_col  and  obj_right > viewport_right_col)

   ┌───────────────────────┐
   │                  RIGHT│
   └───────────────────────┘
                    [──────obj──────────────]
                     obj_left      obj_right
                     ≤            >
================================================================================
*/
* = $3D27
render_object_if_visible:
		lda     screen_scroll_flag        // Load X-scroll flag: $00=right, $FF=left, >$00=none (convention).
										  // Z=1 only if $00 (right-scroll case)
		bne     check_scroll_left         // Z=0 → nonzero: branch to sign check/left handling

		// ------------------------------------------------------------
		// Scroll right (#$00): render if object crosses the right edge
		// ------------------------------------------------------------
		lda     obj_left_col_tbl,x            // obj_left := obj_left_col_tbl[X]
		cmp     viewport_right_col            // C=1 if obj_left >= viewport_right_col; Z=1 if ==
		beq     sr_obj_left_le_viewport_right // obj_left == viewport_right_col → continue
		bcs     sr_culled                     // obj_left  > viewport_right_col → not visible

sr_obj_left_le_viewport_right:
		clc                                   // prepare obj_left + width
		adc     obj_width_tbl,x               // obj_right := obj_left + width
		cmp     viewport_right_col            // C=1 if obj_right >= viewport_right_col; Z=1 if ==
		bcc     sr_culled                     // obj_right  < viewport_right_col → not visible
		beq     sr_culled                     // obj_right == viewport_right_col → culled

		jsr     decode_object_gfx             // visible in right-scroll case → decode
		jmp     advance_to_prev_object        // advance

sr_culled:
		jmp     ns_culled                     // skip render, advance via common exit

check_scroll_left:
		bpl     case_no_scroll        // N=0 → scroll_flag >= $00 (right or none) 
									  // N=0 → use no-scroll path; N=1 ($FF) = left

		// ------------------------------------------------------------
		// Scroll left (#$FF): render if object crosses the left edge
		// ------------------------------------------------------------
		lda     obj_left_col_tbl,x            // obj_left := obj_left_col_tbl[X]
		cmp     viewport_left_col             // C=1 if obj_left >= viewport_left_col; Z=1 if ==
		beq     sl_obj_left_le_viewport_left  // obj_left == viewport_left_col → continue
		bcs     sl_culled                     // obj_left  > viewport_left_col → not visible

sl_obj_left_le_viewport_left:
		clc                                   // prepare obj_left + width
		adc     obj_width_tbl,x               // obj_right := obj_left + width
		cmp     viewport_left_col             // C=1 if obj_right >= viewport_left_col; Z=1 if ==
		bcc     sl_culled                     // obj_right  < viewport_left_col → not visible
		beq     sl_culled                     // obj_right == viewport_left_col → culled (touching only)

		jsr     decode_object_gfx             // visible in left-scroll case → decode
		jmp     advance_to_prev_object        // advance

sl_culled:
		jmp     ns_culled                     // skip render, join common no-scroll cull

		// ------------------------------------------------------------
		// No scroll: render if [start,end) overlaps [left,right]
		// ------------------------------------------------------------
case_no_scroll:
		lda     obj_left_col_tbl,x            // obj_left := obj_left_col_tbl[X]
		cmp     viewport_right_col            // C=1 if obj_left >= viewport_right_col; Z=1 if ==
		beq     ns_obj_left_le_viewport_right // obj_left == viewport_right_col → still eligible
		bcs     ns_culled                     // obj_left  > viewport_right_col → no overlap

ns_obj_left_le_viewport_right:
		clc                                   // prepare obj_left + width
		adc     obj_width_tbl,x               // obj_right := obj_left + width
		cmp     viewport_left_col             // C=1 if obj_right >= viewport_left_col; Z=1 if ==
		bcc     ns_culled                     // obj_right  < viewport_left_col → no overlap
		beq     ns_culled                     // obj_right == viewport_left_col → touching only

		jsr     decode_object_gfx             // overlaps [left,right) → render
		jmp     advance_to_prev_object        // advance to next object

ns_culled:
		jmp     advance_to_prev_object

/*
============================================================================
decode_object_gfx 
============================================================================
Decode one object’s compressed graphics into three layers (tiles, color, mask). 
Trim horizontally to the viewport and optionally collapse to a single column on scroll.

Arguments
	- X		Object index.

Returns
	- None.
	- On return: X restored from decompress_candidate. A,Y undefined.

Global Inputs
	- room_rsrc_base 			Base of room data (lo/hi).
	- obj_left_col_tbl[]		Per-object left column (world/view coords).
	- obj_top_row_tbl[]			Per-object top row index.
	- obj_width_tbl[]			Per-object width in columns.
	- obj_height_tbl[]			Per-object height in rows.
	- obj_gfx_ptr_tbl[]			Per-object compressed stream pointer (lo/hi pairs).
	- viewport_left_col			Leftmost visible column in viewport.
	- viewport_right_col		Rightmost visible column in viewport.
	- screen_scroll_flag		$00=scroll right, $FF=scroll left, other=none.
	- frame_buffer_base			Pixel/tile layer base address (lo/hi).

Global/Module Outputs
	- decomp_src_ptr (lo/hi)	Source stream pointer for decoder.
	- destination_ptr (lo/hi)	Set per layer before decode.
	- src_skip_left				Horizontal left trim in columns.
	- src_skip_right			Horizontal right trim in columns.
	- cols_to_draw				Visible column count (may be 1 during scroll).
	- rows_to_draw				Visible row count.
	- Pixel/Color/Mask layers written via decode_trimmed_window.

Description
	- Load per-object geometry (left, top, width, height) to globals.
	- Compute object gfx source pointer (inside the room resource):
		decomp_src_ptr = room_rsrc_base + obj_gfx_ptr_tbl[X*2].
		
	- Left trim: max(0, viewport_left − object_left) → src_skip_left.
	- Right trim: max(0, object_right − viewport_right) → src_skip_right.
	- Visible width: width − left_trim − right_trim → cols_to_draw.
	
	- If scroll in progress, collapse render to 1 column:
	  • $00 (right): shift to rightmost column, cols_to_draw := 1.
	  • $FF (left):  shift to leftmost column, cols_to_draw := 1.
	  
	- Compute destination offset for layer buffers
		- Compute row base from screen_row_offsets_*[obj_top_row].
		- Add horizontal delta and left trim.
	- For each layer:
	  • Tiles: destination := frame_buffer_base + offset.
	  • Color: destination := COLOR_LAYER_COL_0 + offset.
	  • Mask:  destination := MASK_LAYER_COL_0 + offset.
	  • Call decode_trimmed_window to emit data.

============================================================================
*/

* = $3D87
decode_object_gfx:
        // ------------------------------------------------------------
        // Load object geometry                                       
        // ------------------------------------------------------------
		lda     obj_left_col_tbl,x        // object left column
		sta     obj_left_col            	 
		
		lda     obj_top_row_tbl,x         // object top row
		sta     obj_top_row              	
		
		lda     obj_width_tbl,x           // object width in columns
		sta     obj_width                	
		
		lda     obj_height_tbl,x          // object height in rows
		sta     rows_to_draw            	

        // ------------------------------------------------------------
        // Resolve compressed source pointer for this object:         
        // room_rsrc_base = base											
        // obj_gfx_ptr_tbl = offset										
        // decomp_src_ptr := room_rsrc_base + obj_gfx_ptr_tbl[X*2]     
        // ------------------------------------------------------------
        //Setup Y as index into the object graphics table (Y = obj_index * 2)
		txa                                 
		asl                                 
		tay                                 
	
        //Add base (room_rsrc_base) to offset (obj_gfx_ptr_tbl,Y)
		clc                                 
		lda     room_rsrc_base            // A := room_rsrc_base.lo
		adc     obj_gfx_ptr_tbl,y         // A := lo(base) + lo(offset)
		sta     decomp_src_ptr_lo           // decomp_src_ptr.lo := result
		lda     room_rsrc_base + 1            // A := room_rsrc_base.hi
		adc     obj_gfx_ptr_tbl+1,y       // A := hi(base) + hi(offset) + C
		sta     decomp_src_ptr_hi           // decomp_src_ptr.hi := result

        // ------------------------------------------------------------
        // Compute left trim: max(0, viewport_left_col - obj_left_col)           
        // ------------------------------------------------------------
		lda     viewport_left_col          
		sec                               // viewport_left_col - obj_left_col = delta
		sbc     obj_left_col              // A := delta; C=1 if viewport ≥ object
		bcs     apply_left_trim           // if C set → trim = delta
		lda     #$00                      // else no left trim
apply_left_trim:
		sta     src_skip_left             // commit left-trim column count

        // ------------------------------------------------------------
        // Compute right trim: max(0, obj_right - viewport_right_col)        
        // where obj_right = obj_left + obj_width - 1                 
        // ------------------------------------------------------------
		lda     obj_width                  
		sec                                 
		sbc     #$01                      // A := width - 1
		clc                                 
		adc     obj_left_col              // A := obj_right (= left + width - 1)
		sec                               // A := delta (obj_right - viewport right)
		sbc     viewport_right_col        // C=1 if obj_right ≥ viewport_right
		bcs     apply_right_trim          // if C set → trim = delta
		lda     #$00                      // else no right trim
apply_right_trim:
		sta     src_skip_right            // commit right-trim column count

        // ------------------------------------------------------------
        // Visible width := obj_width - src_skip_left - src_skip_right        
        // ------------------------------------------------------------
		lda     obj_width                  
		sec                                 
		sbc     src_skip_left             // A := width - src_skip_left
		sec                               // visible width = (width - left) - src_skip_right
		sbc     src_skip_right            // A := visible width
		sta     cols_to_draw              // store visible width as cols_to_draw

        // ------------------------------------------------------------
        // Scroll handling: collapse to single column if scrolling     
        // right (flag=$00): move skip_left to rightmost column      
        // left  (flag=$FF): move skip_right to leftmost column      
        // ------------------------------------------------------------
		lda     screen_scroll_flag        // scrolling? Z=1 → right-scroll case ($00)
		bne     r_check_scroll_left       // nonzero → test for left-scroll

        // Screen scrolling right ($00): draw only the rightmost incoming column
		lda     cols_to_draw              // width
		sec                                 
		sbc     #$01					  // (width - 1)
		clc
		adc     src_skip_left             // (width - 1) + left_skip
		sta     src_skip_left             // shift window to its last column
		lda     #$01                      // draw only 1 column
		sta     cols_to_draw               
		jmp     trims_finalized

r_check_scroll_left:
		bpl     trims_finalized           // flag ≥ 0 → not left-scroll → keep full width

        // Screen scrolling left ($FF): draw only the leftmost incoming column
		lda     cols_to_draw              // width
		sec                               // (width - 1) + right_skip
		sbc     #$01					  // (width - 1)
		clc
		adc     src_skip_right            // (width - 1) + right_skip
		sta     src_skip_right            // shift window to its first column
		lda     #$01                      // draw only 1 column
		sta     cols_to_draw              

trims_finalized:
        // ------------------------------------------------------------
		// Compute row base offset for obj_top_row                   
        // ------------------------------------------------------------
		ldy     obj_top_row               // Y := row index to draw at
		lda     screen_row_offsets_hi,y   // A := row offset hi byte
		sta     screen_row_offset + 1        // save hi
		lda     screen_row_offsets_lo,y   // A := row offset lo byte
		sta     screen_row_offset        // save lo

        // ------------------------------------------------------------
        // Compute starting column offset:                            
        // col0 := obj_left - viewport_left_col + src_skip_left            
        // screen_row_offset += col0                                
        // ------------------------------------------------------------
		lda     obj_left_col              // A := object left column
		sec                               // prepare obj_left - viewport_left
		sbc     viewport_left_col         // A := horizontal delta into viewport
		clc                               // add left trim to reach first visible col
		adc     src_skip_left
		clc                               // add column delta to row base offset
		adc     screen_row_offset
		sta     screen_row_offset        // update row base offset (lo)
		bcc     start_decode              // no carry → hi unchanged
		inc     screen_row_offset + 1      // carry → bump row base offset (hi)

start_decode:	

        // ------------------------------------------------------------
		// Layer 1: tiles - dst := frame_buffer_base + row_offset   
        // ------------------------------------------------------------
		jsr     decomp_dict4_init         // init dict4 decoder for this stream
		clc                               // compute dst := base + offset
		lda     screen_row_offset
		adc     frame_buffer_base
		sta     destination_ptr          // dst.lo
		lda     screen_row_offset + 1
		adc     frame_buffer_base + 1
		sta     destination_ptr + 1          // dst.hi
		jsr     decode_trimmed_window     // blit visible window into pixel layer

        // ------------------------------------------------------------
        // Layer 2: color - dst := COLOR_LAYER base + row_offset                  
        // ------------------------------------------------------------
		clc                               
		lda     screen_row_offset        // A := row_offset.lo
		adc     #<COLOR_LAYER_COL_0       // add base lo
		sta     destination_ptr          // dst.lo
		lda     screen_row_offset + 1        // A := row_offset.hi
		adc     #>COLOR_LAYER_COL_0       // add base hi + carry
		sta     destination_ptr + 1          // dst.hi
		jsr     decode_trimmed_window     // blit visible window into color layer

        // ------------------------------------------------------------
        // Layer 3: mask - dst := MASK_LAYER base + row_offset                   
        // ------------------------------------------------------------
		clc                               
		lda     screen_row_offset        // A := row_offset.lo
		adc     #<MASK_LAYER_COL_0        // add base lo
		sta     destination_ptr          // dst.lo
		lda     screen_row_offset + 1        // A := row_offset.hi
		adc     #>MASK_LAYER_COL_0        // add base hi + carry
		sta     destination_ptr + 1          // dst.hi
		jsr     decode_trimmed_window     // blit visible window into mask layer

        // ------------------------------------------------------------
        // Restore X and return                                        
        // ------------------------------------------------------------
		ldx     decompress_candidate      // restore object index (X)
		rts                               // return

/*
================================================================================
decode_trimmed_window
================================================================================
Summary

  Decode a rectangular window from the decompression stream into a destination
  buffer laid out in 40-byte rows. For each output row, skip a left margin,
  decode cols_to_draw bytes, then skip a right margin.

Arguments

  None (uses globals).

Vars/State

  obj_rows_remaining     loop counter for remaining rows
  decomp_skip_rem    byte count consumed by decomp_skip_8bit

Global Inputs

  src_skip_left      bytes to skip before each row in the stream
  src_skip_right     bytes to skip after each row in the stream
  cols_to_draw       number of bytes to decode per row
  rows_to_draw       number of rows to output
  destination_ptr    ZP pointer to start of current destination row

Global Outputs

  destination_ptr  	advanced by ROW_STRIDE after each row (net: height * stride)
  (destination_ptr + k*ROW_STRIDE + 0..cols_to_draw-1) written with bytes

Returns

  Nothing. A and Y clobbered; X preserved.

Description

  • For each row:
    - Skip src_skip_left decoded bytes via decomp_skip_8bit.
    - Decode cols_to_draw bytes with decomp_stream_next into destination_ptr[Y].
    - Advance destination_ptr by ROW_STRIDE (handle carry).
    - Skip src_skip_right decoded bytes via decomp_skip_8bit.

Notes

  • Helper routines required:
    - decomp_stream_next → A = next decoded byte.
    - decomp_skip_8bit   → consumes decomp_skip_rem decoded bytes.
  • cols_to_draw must be nonzero to avoid a 256-byte wrap from the CPY loop.
================================================================================
*/
* = $3E67
decode_trimmed_window:
        // ------------------------------------------------------------
        // Initialize loop: obj_rows_remaining ← rows_to_draw             
        // ------------------------------------------------------------
		lda     rows_to_draw             // A := requested row count
		sta     obj_rows_remaining           // seed row loop counter

skip_left_margin:
        // ------------------------------------------------------------
        // Skip left margin: consume src_skip_left decoded bytes     
        // ------------------------------------------------------------
		lda     src_skip_left            // A := bytes to skip before this row
		sta     decomp_skip_rem          // set skip counter for helper
		jsr     decomp_skip_8bit         // consume that many decoded bytes

        // ------------------------------------------------------------
        // start_decode one row: write cols_to_draw bytes               
        // ------------------------------------------------------------
		ldy     #$00                     // Y := column index (0..cols_to_draw-1)
decode_row_loop:
		jsr     decomp_stream_next       // A := next decoded byte
		sta     (destination_ptr),y      // write byte to current row at dst[Y]
		iny                              // advance column
		cpy     cols_to_draw             // Z=1 when Y == cols_to_draw
		bne     decode_row_loop          // loop until full row written

        // ------------------------------------------------------------
        // Advance destination to next row (add 40, handle carry)      
        // ------------------------------------------------------------
		lda     <destination_ptr         // A := dst_lo
		clc                              // prepare unsigned add
		adc     #ROW_STRIDE              // add row stride (40 bytes)
		sta     destination_ptr         // store new dst_lo
		bcc     skip_right_margin        // no carry → hi byte unchanged
		inc     destination_ptr + 1         // carry → bump dst_hi

skip_right_margin:
        // ------------------------------------------------------------
        // Skip post-row margin: consume src_skip_right decoded bytes 
        // ------------------------------------------------------------
		lda     src_skip_right           // A := bytes to skip after this row
		sta     decomp_skip_rem          // set skip counter for helper
		jsr     decomp_skip_8bit         // consume that many decoded bytes

        // ------------------------------------------------------------
        // Loop over rows                                              
        // ------------------------------------------------------------
		dec     obj_rows_remaining           // one row completed
		bne     skip_left_margin         // more rows pending → next row
		rts                              // done

