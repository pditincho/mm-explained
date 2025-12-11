/*
==============================================================================
  
  Pause/unpause game state and reflect status via UI cursor
 
  Summary:
    Provides two entry points:
      - pause_game:   Enters paused mode, switches cursor to a “snail” shape,
                      and forces cursor color to white for clear visual feedback.
      - unpause_game: Leaves paused mode and restores the normal cursor shape.
  
==============================================================================
*/
#importonce
#import "globals.inc"
#import "constants.inc"

.const CURSOR_SNAIL = $C2
.const CURSOR_CROSS = $C1

/*
==============================================================================
  pause_game: enter paused mode and signal via UI cursor
 
  Summary:
    Sets the global paused flag and swaps the cursor sprite to a “snail”
    as a visual indicator. Also forces the cursor color to white for clarity.
 
  Arguments:
    (none)
 
  Returns:
    (none)                 No register return value.
    Registers              A clobbered; X/Y preserved by convention if callers need.
 
  Description:
    - sprite_shape_cursor := CURSOR_SNAIL  (visual pause indicator)
    - game_paused_flag         := 1             (global paused state)
    - cursor_sprite_color := $01           (white, improves contrast while paused)
==============================================================================
*/
* = $18D1
pause_game:
        // Switch cursor sprite to “snail” to visually indicate pause.
        lda #CURSOR_SNAIL
        sta sprite_shape_cursor

        // Set paused flag (game_paused_flag := 1).
        // NOTE: We intentionally reuse A=$01 below for the cursor color write.
        lda #TRUE
        sta game_paused_flag

        // Force cursor color to white ($01) while paused for clarity/contrast.
        sta cursor_sprite_color
        rts

/*
==============================================================================
  unpause_game: resume gameplay and restore normal cursor
 
  Summary:
    Clears the global paused flag and restores the normal cursor sprite.
    Cursor color is left unchanged.
 
  Arguments:
    (none)
 
  Returns:
    (none)                 No register return value.
    Registers              A clobbered; X/Y preserved by convention if callers need.
 
  Description:
    - sprite_shape_cursor := CURSOR_CROSS  (normal “arrow/cross” cursor)
    - game_paused_flag         := 0             (gameplay resumes)
==============================================================================
*/
* = $18DF
unpause_game:
        // Restore the normal cursor sprite (arrow/crosshair).
        lda #CURSOR_CROSS
        sta sprite_shape_cursor

        // Clear paused flag (game_paused_flag := 0). Cursor color left as-is.
        lda #FALSE
        sta game_paused_flag
        rts

/*
procedure pause_game():
    // Change cursor appearance to indicate paused state
    sprite_shape_cursor := CURSOR_SNAIL

    // Mark the game as paused
    game_paused_flag := TRUE

    // Force the cursor’s color to white so it stands out
    cursor_sprite_color := 1   // white

    return


procedure unpause_game():
    // Restore the normal cursor appearance
    sprite_shape_cursor := CURSOR_CROSS

    // Mark the game as not paused anymore
    game_paused_flag := FALSE

    // Cursor color is not changed here
    return
*/