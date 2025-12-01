#importonce


.label cpu_port_ddr = $00    // 6510 CPU port data direction register (bitmask: 1=output, 0=input; controls memory banking lines)
// ============================================================================
// 6510 CPU I/O port ($0001)
// ============================================================================
// Controls memory configuration and I/O mapping:
//   bit0 = LORAM  → enable BASIC ROM ($A000–$BFFF) when 1
//   bit1 = HIRAM  → enable KERNAL ROM ($E000–$FFFF) when 1
//   bit2 = CHAREN → enable character ROM/I/O at $D000–$DFFF when 0 = char ROM, 1 = I/O
//   bits3–5 = Cassette motor/sense/output lines
.label cpu_port                        = $0001


// ============================================================================
// VIC-II registers
// ============================================================================
.label vic_sprite0_x_lo_reg           = $D000	// Sprite 0 X low byte (bits 0–7). Bit 8 lives in vic_sprite_x_msb_reg bit 0.
.label vic_sprite0_y_reg              = $D001	// Sprite 0 Y position (0–255). Top-left origin, increases downward.
.label vic_sprite1_y_reg              = $D003	// Sprite 1 Y position (0–255).
.label vic_sprite2_y_reg              = $D005	// Sprite 2 Y position (0–255).
.label vic_sprite3_y_reg              = $D007	// Sprite 3 Y position (0–255).
.label vic_sprite4_y_reg              = $D009   // Sprite 4 Y position (0–255).
.label vic_sprite5_y_reg              = $D00B	// Sprite 5 Y position (0–255).
.label vic_sprite6_y_reg              = $D00D	// Sprite 6 Y position (0–255).
.label vic_sprite7_y_reg              = $D00F	// Sprite 7 Y position (0–255). In this codebase, sprite 7 is the cursor.
.label vic_sprite7_x_lo_reg           = $D00E	// Sprite 7 X low byte (bits 0–7) for the cursor. Bit 8 at vic_sprite_x_msb_reg bit 7.
.label vic_sprite_x_msb_reg       	  = $D010	// X high bits for all 8 sprites. Bit n = MSB of sprite n X (adds +256 when set).
												//   bit0→spr0, bit1→spr1, …, bit7→spr7.

// ----------------------------------------------------------------------------
// Screen control register 1 ($D011)  vertical scroll and display configuration
// ----------------------------------------------------------------------------
// Bit layout:
//   bit7 = High bit (bit8) of raster compare register ($D012)
//   bit6 = Extended color mode enable (ECM)         1 = on, 0 = off
//   bit5 = Bitmap graphics mode enable               1 = bitmap, 0 = text
//   bit4 = Display enable                            1 = visible, 0 = blanked
//   bit3 = Row select                                1 = 25 rows, 0 = 24 rows
//   bits0–2 = Vertical fine scroll (0–7 scanlines)
//
// Combined, this register controls vertical timing, display enable, and modes.
// ----------------------------------------------------------------------------
.label vic_screen_control_reg_1        = $D011

// Current raster line (low 8 bits). Range 0–255; combined with $D011 bit7 for 9-bit beam position.
// Used for raster interrupts and beam-synced effects.
.label vic_raster_line_reg             = $D012
.label vic_sprite_enable_reg = $D015    // Sprite enable register (bit set = sprite visible on screen, bits 0–7 correspond to sprites 0–7)

// ============================================================================
// VIC-II screen control register 2 ($D016) — horizontal scroll & mode control
// ----------------------------------------------------------------------------
// Bit layout:
//   bits0–2 = Horizontal fine scroll (0–7 pixels)
//   bit3    = Text display column width (1=40 cols, 0=38 cols)
//   bit4    = Multicolor mode enable (1=on, 0=off)
//   bit5    = Video chip reset (normally 0)
//   bits6–7 = Unused
//
//   Controls horizontal fine scrolling, column width, and text/bitmap
//   multicolor enable. Changing bits 0–2 mid-frame shifts the display left
//   or right by up to 7 pixels. Setting bit4 enables multicolor interpretation
//   for character and bitmap modes.
//
// Common values used in this codebase:
//   #$18 → %00011000 → multicolor on, 40 columns, scroll 0
//   #$09 → %00001001 → multicolor off, 40 columns, scroll 1
//
// References:
//   - Set to #$18 in irq_handler3 and init_memory_sound_and_video.
//   - Set to #$09 in irq_handler17.
// ============================================================================
.label vic_screen_control_reg_2         = $D016

.label vic_sprite_y_expand_reg     = $D017    // Controls Y double-height for sprites (bit per sprite)

// Video memory layout register ($D018).
// Bits 1–3 select character generator base (in 2 KB blocks).
// Bits 4–7 select screen matrix base (in 1 KB blocks).
// Defines which RAM regions VIC reads for screen codes and glyphs.
.label vic_memory_layout_reg           = $D018

// VIC-II interrupt flag register ($D019).
// Read: current interrupt sources. Write: acknowledge (clear) bits by writing 1s.
// Bit layout:
//   bit0 = raster compare hit
//   bit1 = sprite-background collision
//   bit2 = sprite-sprite collision
//   bit3 = lightpen trigger
//   bit7 = master IRQ flag (set if any enabled source triggered)
.label vic_irq_flag_reg                = $D019

.label vic_irq_mask_reg = $D01A    // VIC-II interrupt enable register (bit0=raster, bit1=sprite collision, bit2=background collision, bit3=light pen)
.label vic_sprite_priority_reg     = $D01B    // Sprite priority vs background (1 = behind)
.label vic_sprite_mc_enable_reg    = $D01C    // Enables multicolor mode per sprite (bit set = multicolor)

// Sprite horizontal expansion control. Bit n doubles the width of sprite n (2×).
//   0 = normal width, 1 = expanded. Write-only on VIC-II.
//   bit0→spr0, bit1→spr1, …, bit7→spr7.
.label vic_sprite_x_expand_reg         = $D01D
.label vic_coll_spr_spr_reg        = $D01E    // Sprite–sprite collision latch (write to clear)
.label vic_coll_spr_bg_reg         = $D01F    // Sprite–background collision latch (write to clear)

// Border color register ($D020). Lower 4 bits select color index (0–15).
// Sets the color for screen border and background when display is blanked.
.label vic_border_color_reg            = $D020

// Background color register 0 ($D021).
// Primary background color used in text, bitmap, or multicolor modes.
// In multicolor modes, acts as "color 0" for shared background pixels.
.label vic_bg0_reg                     = $D021

.label vic_sprite_mcolor0_reg = $D025    // Global sprite multicolor 0 (bit pair "01") used by all sprites
.label vic_sprite_mcolor1_reg = $D026    // Global sprite multicolor 1 (bit pair "11") used by all sprites

// Sprite color registers base ($D027–$D02E). Each byte sets color for sprite n.
// Only lower 4 bits are used (0–15). Bit pair “10” in bitmap selects this color.
.label vic_sprite0_color_reg           = $D027
.label vic_sprite1_color_reg           = $D028
.label vic_sprite2_color_reg           = $D029
.label vic_sprite3_color_reg           = $D02A
.label vic_sprite4_color_reg           = $D02B
.label vic_sprite5_color_reg           = $D02C
.label vic_sprite6_color_reg           = $D02D
.label vic_sprite7_color_reg           = $D02E

.label voice1_freq_reg_lo        = $D400    // Base SID register: voice 1 frequency low byte
.label voice1_freq_reg_hi        = $D401    // Base SID register: voice 1 frequency high byte
.label voice1_control_register   = $D404    // Base SID register: voice 1 control (gate/waveform/test/ring/sync)
.label voice1_attack_delay_reg   = $D405    // Base SID register: voice 1 attack/decay envelope
.label voice1_sustain_release_reg = $D406   // Base SID register: voice 1 sustain/release envelope
.label voice2_control_register   = $D40B    
.label voice3_control_register   = $D412
.label filter_cutoff_freq_lo_reg = $D415
.label filter_cutoff_freq_hi_reg = $D416
.label sid_filter_control   = $D417    // SID filter control hardware register (writes affect filter)
.label sid_master_volume			   = $D418
// ============================================================================
// CIA ports
// ============================================================================
// CIA #1 — Complex Interface Adapter 1 ($DC00–$DC0F)
// Handles keyboard matrix, joystick #2, timers, TOD clock, and interrupts.
// ============================================================================

// Port A data register ($DC00).
// Reads/writes data on CIA1 Port A pins depending on DDRA settings.
// - When a bit in DDRA = 1 → output latch (drives keyboard column lines).
// - When a bit in DDRA = 0 → input buffer (reads keyboard row states).
// Used in combination with Port B for keyboard scanning and joystick sensing.
.label cia1_pra                        = $DC00

// Port B data register ($DC01).
// Reads logic levels on CIA1 Port B pins (or drives them if configured as outputs).
// On the C64:
//   bits0–4 = joystick #2 inputs (active low)
//   bits0–7 = also part of keyboard matrix readback
//   bit7    = cassette read line
.label cia1_prb                        = $DC01

// Data Direction Register A ($DC02).
// Controls I/O direction of each Port A line:
//   bit=1 → output, bit=0 → input.
// Typical: DDRA = $FF for keyboard column drive (all outputs).
.label cia1_ddra                       = $DC02

// Data Direction Register B ($DC03).
// Controls I/O direction of Port B lines (same format as DDRA).
// Typical: DDRB = $00 for keyboard/joystick input (all inputs).
.label cia1_ddrb                       = $DC03

// CIA1 timer B high byte ($DC07).
// MSB of timer B reload value. Write $DC06 (low) first, then $DC07 (high).
// Counter loads from the 16-bit latch when LOAD=1 in CRB, or on underflow in one-shot.
.label cia1_timer_b_hi_reg             = $DC07

// CIA1 interrupt status ($DC0D).
// Read: pending sources. Write: clear bits by writing 1 to the asserted sources.
// Bits: b0=TA underflow, b1=TB underflow, b2=TOD alarm, b3=SDR complete,
//       b4=FLAG pin, b7=IRQ summary (set if any enabled source is pending).
.label cia1_irq_status_reg             = $DC0D

// CIA1 timer A control ($DC0E).
// Bits: b0=START, b1=PBON (toggle PB6 on underflow), b2=OUTMODE (toggle/pulse),
//       b3=RUNMODE (1=one-shot), b4=LOAD (force reload), b5=INMODE (0=ϕ2, 1=CNT).
// b6,b7 unused for timer A.
.label cia1_timer_a_ctrl_reg           = $DC0E

// CIA1 timer B control ($DC0F).
// Bits: b0=START, b1=PBON (toggle PB7 on underflow), b2=OUTMODE (toggle/pulse),
//       b3=RUNMODE (1=one-shot), b4=LOAD (force reload),
//       b5–b6=INMODE for timer B:
//           00=ϕ2, 01=CNT pin, 10=Timer A underflow, 11=TA underflow while CNT=1.
// b7 reserved.
.label cia1_timer_b_ctrl_reg           = $DC0F

// ============================================================================
// CIA #2 — Complex Interface Adapter 2 ($DD00–$DD0F)
// Handles serial IEC bus, RS-232, user port, and interrupt timing.
// ============================================================================

// Port A data register ($DD00).
// - On write: drives IEC bus control lines (ATN, CLK, DATA) and user port bits.
// - On read: samples IEC bus state and RS-232 signals.
// Bit mapping varies by mode:
//   bits0–2 = IEC DATA/CLK/ATN (open drain, active low)
//   bits5–7 = RS-232 lines or user port.
// Setting DDRA controls which lines act as outputs.
.label cia2_pra                        = $DD00


// CIA2 Port B data register ($DD01).
// Reads or writes data on Port B pins. Direction set by DDRB ($DD03).
// On the C64, used for RS-232 signals, user port, and some cartridge I/O.
// Bit usage depends on system mode:
//   bits0–7 → user port lines or serial link (TxD, RTS, DTR, etc.)
.label cia2_prb                        = $DD01

// CIA2 Port A Data Direction Register ($DD02).
// Each bit controls Port A line direction: 1=output, 0=input.
// Port A connects primarily to the IEC serial bus (ATN, CLK, DATA).
// Typical config: DDRA = %00111111 ($3F) to drive IEC outputs.
.label cia2_ddra                       = $DD02

// CIA2 Port B Data Direction Register ($DD03).
// Controls direction of Port B pins (same format as DDRA).
// Typically left as inputs for user port or RS-232 reception.
.label cia2_ddrb                       = $DD03

// CIA2 interrupt status register ($DD0D).
// Read: reports which CIA2 interrupt sources are pending.
// Write: clear specific sources by writing 1s to their bits.
// Bits identical to CIA1’s IRQ layout:
//   b0=Timer A underflow, b1=Timer B underflow,
//   b2=TOD alarm, b3=Serial data ready, b4=FLAG pin, b7=master IRQ flag.
.label cia2_irq_status_reg             = $DD0D

