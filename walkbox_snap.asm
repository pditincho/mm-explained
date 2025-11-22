#importonce
#import "globals.inc"
#import "constants.inc"
#import "walkbox_helpers.asm"

// -----------------------------------------------------------------------------
// Box geometry (current candidate)
// -----------------------------------------------------------------------------
.label box_left_edge             = $1A64  // Box left edge (pixels)
.label box_right_edge            = $1A65  // Box right edge (pixels)
.label box_top_edge              = $1A66  // Box top edge (pixels)
.label box_bottom_edge           = $1A67  // Box bottom edge (pixels)

// -----------------------------------------------------------------------------
// Nearest-point computation (to test point)
// -----------------------------------------------------------------------------
.label nearest_x                 = $1A60  // Nearest X on or inside box
.label nearest_y                 = $1A61  // Nearest Y on or inside box
.label scaled_x_dist             = $1A5C  // Scaled |dx| = 2*|x - nearest_x|
.label scaled_y_dist             = $1A5D  // Scaled |dy| = |y - nearest_y|/4

// -----------------------------------------------------------------------------
// Test point and selection state
// -----------------------------------------------------------------------------
.label test_x                   = $FE72  // Input test X (pixels)
.label test_y                   = $FE73  // Input test Y (pixels)
.label box_index                = $1A5B  // Running box index (starts at -1)
.label min_distance             = $1A5E  // Best scaled distance so far
.label min_nearest_x            = $1A62  // Nearest X for best candidate
.label min_nearest_y            = $1A63  // Nearest Y for best candidate
.label nearest_box_idx          = $FDAC  // Output: best box index

.label nearest_x_candidate      = $1A62  // snapped X (pixels) nearest to destination within chosen walkbox
.label nearest_y_candidate      = $1A63  // snapped Y (pixels) nearest to destination within chosen walkbox
.label diag_slope_mode          = $FC3D  // decoded slope selector
.label diag_clamp_applied       = $CB39  // 00=no adjust, 01=adjusted
.label nearest_box_index        = $FDAC  // current walkbox index

// -----------------------------------------------------------------------------
// Geometry tests and distance scaling
// -----------------------------------------------------------------------------
.const INSIDE_BOX                      = $00    // Point classified inside/on box
.const OUTSIDE_BOX                     = $FF    // Point classified outside box
.const INIT_MAX_DIST                   = $FF    // Initial “worst” scaled distance

.const NO_BOX_FOUND              = $FF    // sentinel: no valid walkbox located

.const WALKBOX_ATTR_OFS          = $04    // record: attribute byte
.const DIAG_NOT_CLAMPED          = $00    // function return: no clamp applied
.const DIAG_CLAMPED              = $01    // function return: clamp applied
.const DIAG_ENABLE_BIT           = $80    // attribute bit7: diagonal present
.const DIAG_MASK                 = $7C    // attribute bits6..2 mask for slope code
.const DIAG_CODE_UP_LEFT         = $08    // bits3..2=10b → up-left slope
.const DIAG_CODE_DOWN_RIGHT      = $0C    // bits3..2=11b → down-right slope
.const DIAG_MODE_UP_LEFT     	 = $01    // stored in box_attribute for up-left
.const DIAG_MODE_DOWN_RIGHT  	 = $02    // stored in box_attribute for down-right
.const ROOM_X_MAX                = $A0    // upper bound used in up-left path

.label current_box_ptr          = $17  // zp ptr → current walkbox edge table {left,right,top,bottom}
.label candidate_x              = $FE72  // candidate X
.label candidate_y              = $FE73  // candidate Y

/*
================================================================================
Diagonal edge horizontal offset lookup table
================================================================================

Summary
    Provides the per-scanline ΔX (horizontal shift) used when clamping
    coordinates to diagonal walkbox edges.

Description
    Each entry corresponds to one vertical pixel row below the walkbox’s top
    edge. The value gives the number of pixels to shift horizontally from the
    straight boundary to follow the diagonal slope.

    Used by clamp_to_diagonal_edge to determine how far the visible
    wall edge moves horizontally as Y increases.

Notes
    - Indexed by (candidate_y − top_edge).
    - Values plateau briefly to approximate shallower segments of the wall.
    - Table length = 22 entries.

================================================================================
*/
* = $1C09
diag_dx_lut:
        .byte $00,$01,$02,$03,$03
        .byte $04,$05,$06,$06,$07
        .byte $08,$09,$09,$0A,$0B
        .byte $0C,$0C,$0D,$0E,$0F
        .byte $10,$10

/*
================================================================================
snap_coords_to_walkbox
================================================================================
Summary
    Snap coordinates to the nearest valid point of the nearest walkbox.

Global Inputs
    nearest_x/y_candidate      nearest X/Y before slope adjustment

Global Outputs
    test_x                     adjusted X
    test_y                     adjusted Y

Returns
    A  nearest_box_idx        $FF if no walkbox found

Description
    - Preserve X and Y.
    - Call get_nearest_box to compute nearest_box_idx and nearest_{x,y}_candidate.
    - If nearest_box_idx == NO_BOX_FOUND:
        • Restore X and Y, return with A=$FF (current code has a stack bug).
    - Else:
        • Write nearest_{x,y}_candidate to test_{x,y}.
        • Call clamp_to_diagonal_edge to refine along sloped edges.
        • Restore X and Y and return nearest_box_idx in A.
================================================================================
*/
* = $1A9B
snap_coords_to_walkbox:
        // Save X and Y.
        txa
        pha
        tya
        pha

        // Find nearest walkbox (sets nearest_box_idx and nearest_{x,y}_candidate).
        jsr     get_nearest_box

        // If no box found → return NO_BOX_FOUND
        lda     nearest_box_idx
        cmp     #NO_BOX_FOUND
        bne     write_nearest_point
		
		//BUG - original return address not in stack, so it will crash on return
		rts

write_nearest_point:
        // Set nearest point coordinates
        lda     nearest_x_candidate
        sta     test_x
        lda     nearest_y_candidate
        sta     test_y
		
		// Clamp to diagonal edge if needed
        jsr     clamp_to_diagonal_edge

        // Restore Y,X and return nearest_box_idx in A.
        pla
        tay
        pla
        tax
        lda     nearest_box_idx
        rts
/*
================================================================================
  get_nearest_box
================================================================================
Summary
	Iterate through all walkboxes of a room and determine which one is closest
	to a given coordinate pair (box_check_x, box_check_y). 
	
	Updates global variables with the index of the nearest box, the nearest point 
	on its edges, and the minimum computed distance. 
	
	Returns failure if no boxes are present.

Arguments
	walkbox_room        Room index whose walkboxes are being scanned.
	box_check_x         X coordinate of the test point.
	box_check_y         Y coordinate of the test point.

Returns
	On success:
		nearest_box_idx     → index of nearest walkbox
		min_nearest_x       → nearest X on or inside that box
		min_nearest_y       → nearest Y on or inside that box
		min_distance        → anisotropic distance value
		
	On failure (no room data loaded):
		nearest_box_idx   = #$FF
		A                 = #$FF

Description
	• Calls get_walkboxes_for_room to retrieve the base pointer to the room's
	walkbox list; aborts if the room is not resident.
	• Each walkbox entry consists of 6 bytes:
		[left, right, top, bottom, byte4, byte5]
	and the list terminates with #$FF at the next entry's start.
	• For every box:
		– Loads its edges into box_left_edge..box_bottom_edge.
		– Calls get_distance_to_box to compute both the distance (A) and
		the nearest point (nearest_x, nearest_y).
		– If the distance is less than min_distance, records this box as
	the new closest.
	• Continues until a WALKBOX_SENTINEL sentinel is encountered.
================================================================================
*/
* = $1AC1
get_nearest_box:
        // ------------------------------------------------------------
        // Acquire walkbox table pointer for the requested room
        //   A = #$FF on failure (room not resident)
        // ------------------------------------------------------------
        jsr     get_walkboxes_for_room
		
		//Walkboxes present? If so, continue
        cmp     #WALKBOX_SENTINEL		
        bne     init_scan
		
		//Walkboxes not present, exit with error code
        sta     nearest_box_idx            	// propagate #$FF to output index
        rts

init_scan:
        // ------------------------------------------------------------
        // Initialize search state
        //   min_distance := $FF (worst), box_index := $FF then pre-increment
        // ------------------------------------------------------------
        lda     #INIT_MAX_DIST
        sta     min_distance
        lda     #$ff
        sta     box_index

        ldy     #$00                     	// Y := table offset
scan_next_box:
        inc     box_index                   // next logical record index

        // ------------------------------------------------------------
        // End-of-list check: leading byte == WALKBOX_SENTINEL means no more records
        // ------------------------------------------------------------
        lda     (box_ptr),y
        cmp     #WALKBOX_SENTINEL
        bne     load_current_box
        rts

load_current_box:
        // ------------------------------------------------------------
        // Load current box edges: [left, right, top, bottom]
        // Then skip the next two to land at next record start
        // ------------------------------------------------------------
        lda     (box_ptr),y
        sta     box_left_edge
        iny
        lda     (box_ptr),y
        sta     box_right_edge
        iny
        lda     (box_ptr),y
        sta     box_top_edge
        iny
        lda     (box_ptr),y
        sta     box_bottom_edge
        iny
        iny                                 // skip two extra bytes (stride = 6)

        // ------------------------------------------------------------
        // Compute distance to this box and the nearest point on it
        //   On return: A = distance, nearest_x/nearest_y filled
        // ------------------------------------------------------------
        jsr     get_distance_to_box

        // ------------------------------------------------------------
        // If this distance is strictly smaller, capture as new minimum
        // ------------------------------------------------------------
        cmp     min_distance
        bcs     continue_scan               // A >= min_distance → keep current best

		// New minimum, save box index and nearest coordinates
        sta     min_distance
        lda     box_index
        sta     nearest_box_idx
        lda     nearest_x
        sta     min_nearest_x
        lda     nearest_y
        sta     min_nearest_y

continue_scan:
        jmp     scan_next_box
/*
================================================================================
  get_distance_to_box
================================================================================
Summary
	Compute an anisotropic distance from a test point to an axis-aligned box and
	record the nearest point on or in that box. Inside the box returns zero.

Arguments
	test_x                  Test X coordinate
	test_y                  Test Y coordinate
	box_left_edge           Box left edge
	box_right_edge          Box right edge
	box_top_edge            Box top edge
	box_bottom_edge         Box bottom edge

Returns
	A                       Distance byte
	nearest_x               Nearest X on/inside the box
	nearest_y               Nearest Y on/inside the box

Vars/State
	scaled_x_dist           2*|test_x − nearest_x|
	scaled_y_dist           |test_y − nearest_y|/4

Description
	• Clamp test_x to [box_left_edge, box_right_edge] → nearest_x.
	• Clamp test_y to [box_top_edge, box_bottom_edge] → nearest_y.
	• Compute |dx| and |dy|, then scale: X by ×2, Y by ÷4.
	• If scaled_x_dist > scaled_y_dist: return scaled_x_dist/2 + scaled_y_dist;
	otherwise return scaled_y_dist/2 + scaled_x_dist.
================================================================================

Distance approximation formula

This distance formulation is a fast integer approximation of a Euclidean metric.

A true Euclidean distance requires a square root and multiplications:
	d = sqrt(dx² + dy²) which are costly on 6502 hardware.

Instead, this routine blends the absolute deltas with a fixed ratio:


if |dx| > |dy|:
    d ≈ |dx|/2 + |dy|
else:
    d ≈ |dy|/2 + |dx|


After axis scaling (×2 for X, ÷4 for Y) this produces:
	d ≈ 0.5*max(|dx|,|dy|) + min(|dx|,|dy|)

This hybrid between the Manhattan (L1) and Chebyshev (L∞) metrics forms an
'octagonal' distance contour, closely approximating circular Euclidean ranges.

The blend 0.5*max + min is a midpoint between L1 and L∞ and an inexpensive way 
to get contours roughly circular without using square roots or multiplications.
==================================================================
*/
* = $1B1B
get_distance_to_box:
        // ------------------------------------------------------------
        // Resolve nearest X coordinate relative to box edges
        //   • If test_x > right  → clamp to right edge.
        //   • If test_x < left   → clamp to left edge.
        //   • Otherwise          → point is inside horizontally.
        // ------------------------------------------------------------
        lda     test_x                        // load X coordinate to test
        cmp     box_right_edge                // compare to right edge
        bcc     x_within_or_left_of_right     // if ≤ right, continue testing
        beq     x_within_or_left_of_right
		
        lda     box_right_edge                // test_x > right → clamp to right
        jmp     commit_nearest_x              // done with X side

x_within_or_left_of_right:
        cmp     box_left_edge                 // compare to left edge
        bcs     commit_nearest_x              // if ≥ left → inside; keep original
		
        lda     box_left_edge                 // test_x < left → clamp to left
commit_nearest_x:
        sta     nearest_x                     // store resolved nearest X

        // ------------------------------------------------------------
        // Resolve nearest Y relative to box edges
        //   If test_y > bottom → clamp to bottom
        //   If test_y < top    → clamp to top
        //   Else                → inside vertically, keep test_y
        // ------------------------------------------------------------
        lda     test_y                        // A := test_y
        cmp     box_bottom_edge               // A ? bottom
        bcc     y_within_or_above_bottom      // A <  bottom → maybe above; check top
        beq     y_within_or_above_bottom      // A == bottom → inside so far
		
        lda     box_bottom_edge               // A > bottom → clamp to bottom
        jmp     commit_nearest_y

y_within_or_above_bottom:
        cmp     box_top_edge                  // A ? top
        bcs     commit_nearest_y              // A >= top → inside vertically
		
        lda     box_top_edge                  // A <  top → clamp to top
commit_nearest_y:
        sta     nearest_y                     // nearest_y := resolved Y

		// ------------------------------------------------------------
        // Compute |dx| then store scaled_x_dist = 2*|dx|
		// ------------------------------------------------------------
        lda     test_x                        // A := test_x
        sec                                   // prepare subtraction
        sbc     nearest_x                     // A := test_x - nearest_x
        bcs     set_x_distance                // if A ≥ 0 keep magnitude
        eor     #$ff                          // two's complement negate
        clc
        adc     #$01

set_x_distance:
        asl                                   // A := 2*|dx|
        sta     scaled_x_dist                 // save scaled X distance

		// ------------------------------------------------------------
		// Compute |dy| then scale to scaled_y_dist = |dy|/4
		// ------------------------------------------------------------
        lda     test_y                        // A := test_y
        sec                                   // prepare subtraction
        sbc     nearest_y                     // A := test_y - nearest_y
        bcs     set_y_distance                // if A ≥ 0 keep magnitude
        eor     #$ff                          // two's complement negate
        clc
        adc     #$01

set_y_distance:
        lsr                                   // ÷2
        lsr                                   // ÷4 total
        sta     scaled_y_dist                 // save scaled Y distance

		// ------------------------------------------------------------
		// Combine scaled components
		//   If scaled_x_dist > scaled_y_dist:
		//       return scaled_x_dist/2 + scaled_y_dist
		//   else:
		//       return scaled_y_dist/2 + scaled_x_dist
		// ------------------------------------------------------------
        cmp     scaled_x_dist                 // compare scaled_y_dist (A) vs scaled_x_dist
        bcc     y_dist_le_x_dist              // if y < x  → use y/2 + x
        beq     y_dist_le_x_dist              // if y == x → same path (y/2 + x)

        lda     scaled_x_dist                 // y > x → use x/2 + y
        lsr                                   // x/2
        clc
        adc     scaled_y_dist				  // + y
        jmp     return_distance

y_dist_le_x_dist:
        lda     scaled_y_dist                 // y ≤ x → use y/2 + x
        lsr                                   // y/2
        clc
        adc     scaled_x_dist				  // + x

return_distance:
		rts
/*
================================================================================
  clamp_to_diagonal_edge 
================================================================================
Summary
	Adjust an X coordinate to respect a diagonal walkbox boundary.
	This uses a per-row	ΔX lookup indexed by vertical distance from the walkbox top edge.

Returns
	.A  	DIAG_CLAMPED  if the coordinate was adjusted
			DIAG_NOT_CLAMPED if it wasn't
			
Global Inputs
	candidate_x                   X coordinate under test
	candidate_y                   Y coordinate under test
	nearest_box_index             reference walkbox index 
	current_box_ptr               pointer to walkbox record base

Global Outputs
	candidate_x                   clamped if needed

Description
	- Resolve walkbox record
	- Fetch walkbox attribute
	- Require attribute bit7 to enable diagonal handling; otherwise return.
	- Decode slope code:
		* If up-left, use right − dx later on
		* If down-right, use left + dx later on
	- Compute dy = candidate_y − top, then dx = diag_dx_lut[dy].
	- Compare adjusted boundary with candidate_x:
		* up-left: if (right − dx) ≥ candidate_x, clamp candidate_x
		and guard vs ROOM_X_MAX.
		* down-right: if (left + dx) ≤ candidate_x, clamp candidate_x.
	- Set diag_clamp_applied and return through finalize_diag_exit.
================================================================================
*/
* = $1B91
clamp_to_diagonal_edge:
		// Init adjustment return value to "no adjustment"
        lda     #DIAG_NOT_CLAMPED
        sta     diag_clamp_applied             

        // ------------------------------------------------------------
        // Resolve walkbox record of the reference box
        // ------------------------------------------------------------
        lda     nearest_box_index
        jsr     get_walkbox_offset            
		
        // ------------------------------------------------------------
		// Resolve packed attribute offset (relative to walkbox base)
        // ------------------------------------------------------------
        tya									// Y := offset to walkbox record
        clc
        adc     #WALKBOX_ATTR_OFS			// add relative offset to packed attribute byte
        tay									// Y := offset to packed byte
		
		// Read packed attribute
        lda     (current_box_ptr),y
		
        // ------------------------------------------------------------
		// Does it have a diagonal slope? (bit 7 set needed). If not, exit.
        // ------------------------------------------------------------
        bmi     diag_attr_enabled                      
        rts

        // ------------------------------------------------------------
        // Resolve slope kind (bits 6..2)
        // ------------------------------------------------------------
diag_attr_enabled:
        and     #DIAG_MASK
		
		// Branch on the kind of slope (down/right or up/left)
        cmp     #DIAG_CODE_DOWN_RIGHT
        bne     check_slope_up_left

        // Down-right slope (right edge)
        lda     #DIAG_MODE_DOWN_RIGHT
        jmp     store_slope_mode

check_slope_up_left:
        cmp     #DIAG_CODE_UP_LEFT
        bne     no_diagonal_exit

        // Up-left slope (left edge)
        lda     #DIAG_MODE_UP_LEFT
        jmp     store_slope_mode

no_diagonal_exit:
		//Not a diagonal slope - exit
        rts

store_slope_mode:
		// Stash slope kind
        sta     diag_slope_mode                 
		
        // ------------------------------------------------------------
        // Compute dy from top(+2), fetch dx from table, clamp X
        // ------------------------------------------------------------
        lda     candidate_y                 // A := candidate Y
		
		// Resolve offset to top edge of box
        dey                                 // Y := base + 3 (bottom)
        dey                                 // Y := base + 2 (top)
		
		// Compute dy = Y position - top edge of box
        sec
        sbc     (current_box_ptr),y         // A := Y - top
        tax                                 // X := dy

		// Resolve offset to right edge of box
        dey                              	// Y := base + 1 (right)

        // ------------------------------------------------------------
		// Do we have an up/left slope?
        // ------------------------------------------------------------
        lda     diag_slope_mode
        cmp     #DIAG_MODE_UP_LEFT
        bne     handle_down_right_slope

        // ------------------------------------------------------------
        // Up-left slope: boundary = right edge - diag_dx[dy]
        // ------------------------------------------------------------
        lda     (current_box_ptr),y        	// A := right
		// Compute adjusted X coordinate = right edge - diag_dx (from the LUT)
        sec
        sbc     diag_dx_lut,x        		// right - diag_dx[dy]
		
        // ------------------------------------------------------------
		// Adjusted coordinate ≥ candidate X ? 
		// If so, it's to the right of the slope, leave it as it is.
		// Otherwise, it's to the left of the slope, adjust.
        // ------------------------------------------------------------
        cmp     candidate_x                 // adjusted ≥ candidate X ?
        bcs     room_width_guard
		
        // ------------------------------------------------------------
		// No adjustment needed
        // ------------------------------------------------------------
        lda     #DIAG_NOT_CLAMPED                         
        jmp     finalize_diag_exit

room_width_guard:
		//Clamp against the max possible X position (is this feasible in actual game?)
        cmp     #ROOM_X_MAX                          
        bcc     apply_x_clamp_up
        beq     apply_x_clamp_up
		
		// Clamp to #00 (another bug?)
        lda     #$00
apply_x_clamp_up:
        // ------------------------------------------------------------
		// Adjustment needed - clamp to diagonal slope
        // ------------------------------------------------------------
        sta     candidate_x
        jmp     finalize_diag_exit

        // ------------------------------------------------------------
        // Down-right slope: boundary = left edge + diag_dx[dy]
        // ------------------------------------------------------------
handle_down_right_slope:
        dey                                 // Y := base + 0 (left)
        lda     (current_box_ptr),y         // left edge
		// Compute adjusted X coordinate = left edge + diag_dx (from the LUT)
        clc
        adc     diag_dx_lut,x        		// left + diag_dx
		
        // ------------------------------------------------------------
		// Adjusted coordinate ≤ candidate X ? 
		// If so, it's to the left of the slope, leave it as it is.
		// Otherwise, it's to the right of the slope, adjust.
        // ------------------------------------------------------------
        cmp     candidate_x                 
        bcc     apply_x_clamp_down
        beq     apply_x_clamp_down
		
        // ------------------------------------------------------------
		// No adjustment needed
        // ------------------------------------------------------------
        lda     #DIAG_NOT_CLAMPED              
        jmp     finalize_diag_exit

apply_x_clamp_down:
        // ------------------------------------------------------------
		// Adjustment needed - clamp to diagonal slope
        // ------------------------------------------------------------
        sta     candidate_x

finalize_diag_exit:
		// BUG - original code returns DIAG_CLAMPED even if no adjustment happened
		// but it's ignored by the caller anyway
        lda     #DIAG_CLAMPED     			
        sta     diag_clamp_applied			
        rts
		
/*
================================================================================
  is_actor_pos_inside_box
================================================================================
Summary
	Determine whether an actor's position lies within a rectangular box. 
	The box is described by four consecutive bytes: [left, right, top, bottom]. 
	All boundaries are inclusive.

Arguments
	X   			Actor index.
	Y   			Offset to box data (relative to box_ptr).
	box_ptr  		pointer to the start of the box table.
	actor_x_pos[]  Actor horizontal positions.
	actor_y_pos[]  Actor vertical positions.

Returns
	A   			INSIDE_BOX 		if the actor is inside the box.
					OUTSIDE_BOX 	if the actor is outside the box.

Description
	• Compares the actor's X coordinate against the left and right box edges.
	Fails immediately if outside those limits.
	• Compares the actor's Y coordinate against the top and bottom edges.
	Fails if outside the vertical bounds.
	• If all comparisons pass, returns INSIDE_BOX. Otherwise, OUTSIDE_BOX.
================================================================================
*/
* = $30AD
is_actor_pos_inside_box:
        // ------------------------------------------------------------
        // X-range check: ensure left ≤ pos_x ≤ right
        //
		// Reads left edge at (box_ptr)+Y, then right edge at (box_ptr)+Y+1.
        // Early-out if outside on either edge test.
        // ------------------------------------------------------------
        lda     actor_x_pos,x
        cmp     (box_ptr),y                  // pos_x ? left
        bcc     return_outside               // pos_x < left → outside
		
        iny
        cmp     (box_ptr),y                  // pos_x ? right
        beq     check_y                      // equal is inside so far
        bcs     return_outside               // pos_x > right → outside

check_y:
        // ------------------------------------------------------------
        // Y-range check: ensure top ≤ pos_y ≤ bottom
		//
        // Reads top edge at (box_ptr)+Y+2, then bottom edge at (box_ptr)+Y+3.
        // Early-out if outside
        // ------------------------------------------------------------
        iny
        lda     actor_y_pos,x
        cmp     (box_ptr),y                  // pos_y ? top
        bcc     return_outside               // pos_y < top → outside
		
        iny
        cmp     (box_ptr),y                  // pos_y ? bottom
        beq     return_inside                // equal counts as inside
        bcs     return_outside               // pos_y > bottom → outside

return_inside:
        lda     #INSIDE_BOX
        rts

return_outside:
        lda     #OUTSIDE_BOX
        rts

/*
Pseudo-code

function get_nearest_box(roomId, pointX, pointY) -> (status, nearestBoxIndex, nearestPointX, nearestPointY, bestDistance):
    // 1. Resolve walkbox table for room
    walkboxStatus = get_walkboxes_for_room(roomId)
    if walkboxStatus == WALKBOX_NOT_RESIDENT:
        return (WALKBOX_NOT_RESIDENT, -1, null, null, null)

    // 2. Initialize search
    bestDistance     = +∞
    nearestBoxIndex  = -1
    nearestPointX    = null
    nearestPointY    = null

    boxIndex = 0

    // Iterate all boxes until a sentinel marks the end
    while walkboxExists(roomId, boxIndex):
        box = getWalkbox(roomId, boxIndex)   // { left, right, top, bottom, ... }

        // Compute distance and nearest point on/in this box
        distance, px, py = get_distance_to_box(pointX, pointY, box)

        if distance < bestDistance:
            bestDistance    = distance
            nearestBoxIndex = boxIndex
            nearestPointX   = px
            nearestPointY   = py

        boxIndex += 1

    // On success, bestDistance will be finite and nearestBoxIndex >= 0
    return (0, nearestBoxIndex, nearestPointX, nearestPointY, bestDistance)


function get_distance_to_box(pointX, pointY, box) -> (distance, nearestX, nearestY):
    left   = box.left
    right  = box.right
    top    = box.top
    bottom = box.bottom

    // 1. Clamp point to box horizontally
    if pointX < left:
        nearestX = left
    else if pointX > right:
        nearestX = right
    else:
        nearestX = pointX

    // 2. Clamp point to box vertically
    if pointY < top:
        nearestY = top
    else if pointY > bottom:
        nearestY = bottom
    else:
        nearestY = pointY

    // 3. Compute absolute deltas
    dx = abs(pointX - nearestX)
    dy = abs(pointY - nearestY)

    // 4. Apply anisotropic scaling (horizontal emphasized, vertical de-emphasized)
    scaledX = 2 * dx       // horizontal cost is doubled
    scaledY = dy / 4       // vertical cost is quartered

    // 5. Blend into an “octagonal” distance approximation
    //    d ≈ 0.5 * max(scaledX, scaledY) + min(scaledX, scaledY)
    if scaledY > scaledX:
        distance = (scaledX / 2) + scaledY
    else:
        distance = (scaledY / 2) + scaledX

    return (distance, nearestX, nearestY)



function is_actor_pos_inside_box(actorId, box) -> bool:
    posX = actor_x_pos[actorId]
    posY = actor_y_pos[actorId]

    left   = box.left
    right  = box.right
    top    = box.top
    bottom = box.bottom

    // Inclusive rectangle test
    if posX < left:   return false
    if posX > right:  return false
    if posY < top:    return false
    if posY > bottom: return false

    return true

*/