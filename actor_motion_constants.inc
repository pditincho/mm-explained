#importonce
.const INIT_SCAN_IDX               = $FF    // Initial box scan index; pre-increments to 0
.const EDGE_INVALID                = $FF    // Sentinel: last_box_vedge is invalid
.const BREAK_SCAN                  = $00    // Control: stop scanning boxes
.const CONTINUE_SCAN               = $FF    // Control: continue scanning boxes

.const BOX_CODE_INDEX_MASK         = $7C    // Bits 2..6 of attr: handler index
.const BOX_ATTR_DEPTH_MASK         = $03    // Bits 0..1 of attr: depth value

.const PATH_RET_MOVE_NEEDED        = $00    // DDA init: motion required (any delta ≠ 0)
.const PATH_RET_NO_MOVE            = $01    // DDA init: no motion (both deltas = 0)
.const DOM_AXIS_H                  = $00    // Dominant axis flag: horizontal (|ΔX| ≥ |ΔY|)
.const DOM_AXIS_V                  = $01    // Dominant axis flag: vertical   (|ΔY| >  |ΔX|)

.const MOTION_WALKING              = $00    // Low nibble: regular walking
.const MOTION_ABOUT_TO_STOP        = $01    // Low nibble: about to stop
.const MOTION_STOPPED              = $02    // Full state value: fully stopped
.const MOTION_TRAVERSE_HORIZ       = $03    // Low nibble: traverse horizontally
.const MOTION_TRAVERSE_VERT        = $04    // Low nibble: traverse vertically

.const MSK_TURN_STAND              = $80    // Flag: standing/turning (bit7)

// Traversal direction mask byte (DIR_AXIS_BIT|DIR_SIGN_BIT encoding)
.const TRV_DN_MASK                 = $80    // Vertical, down
.const TRV_UP_MASK                 = $81    // Vertical, up
.const TRV_RT_MASK                 = $00    // Horizontal, right
.const TRV_LT_MASK                 = $01    // Horizontal, left

.const FACING_OVERRIDE_BIT         = $40    // actor_box_facing_override: apply override (bit6)
.const FACING_OVERRIDE_CLEAR_MASK  = $BF    // AND mask to clear override bit6
.const FG_OCCLUDED             	   = $03    // actor_box_fg_occlusion: FG-occluded value
.const FG_MASKING_CLEAR            = $00    // actor_box_fg_occlusion: clear FG masking
.const BOX_UNRESOLVED_CODE         = $FF    // resolve_actor_walkbox_and_behavior: box undetermined

.const MOTION_FLAG_TURNING         = $80    // motion_state_for_actor: turning flag (bit7)
.const MOTION_FLAG_CLEAR_TURNING   = $7F    // AND mask: clear turning flag

.const DIR_AXIS_BIT                = $80    // Direction mask: axis bit (0=H, 1=V)
.const DIR_SIGN_BIT                = $01    // Direction mask: sign (0=+ right/down, 1=− left/up)
.const DIR_SIGN_CLEAR_MASK         = $FE    // AND mask: clear sign bit

.const BOX_RESULT_UNCHANGED        = $00    // Return A: current box unchanged
.const BOX_RESULT_CHANGED          = $01    // Return A: current box changed
.const BOX_RESULT_UNDETERMINED     = $FF    // Return A: box could not be determined
.const ATTR_INVALID                = $FF    // Walkbox attribute sentinel: no box

.const XAXIS_RESULT_CONTINUE       = $00    // X step: not reached, no box change
.const XAXIS_RESULT_REACHED        = $01    // X step: waypoint reached
.const XAXIS_RESULT_WBOX_CHANGED   = $03    // X step: walkbox changed
.const YAXIS_RESULT_CONTINUE       = $00    // Y step: not reached, no box change
.const YAXIS_RESULT_REACHED        = $01    // Y step: waypoint reached
.const YAXIS_RESULT_WBOX_CHANGED   = $04    // Y step: walkbox changed

.const FACING_OVERRIDE_UP          = $C1    // actor_box_facing_override encoded “up”
.const BORDER_WAIT_FG_MASK_COLOR   = $05    // VIC border color during debug wait
