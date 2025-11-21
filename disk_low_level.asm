/*
================================================================================
  IEC bit-banging layer – overview
================================================================================
This module implements the low-level IEC serial protocol purely in software by
driving and sampling CIA2 Port A (PRA). It never uses timers or IRQs; instead,
it relies on carefully chosen instruction sequences and short busy-wait loops
to shape the waveform on the active-low IEC bus (DATA/CLOCK/ATN via inverters).

Core responsibilities
  - Provide byte-level send/receive primitives:
      • iec_send_byte: transmit one byte MSB-first using four two-bit “dibit”
        phases (ODD/EVEN CLOCK levels per dibit).
      • iec_recv_byte: sample one byte MSB-first via four receive dibits.
  - Provide a small synchronization preamble:
      • iec_sync: burn a pre-delay, poll CLK-IN until released, then write
        IEC_MASK_ATN_RELEASE and burn a post-delay so ATN/bus state is stable.
  - Provide a 3-byte command sender:
      • iec_send_cmd: send track → sector → operation using the byte sender
        after presetting PRA and running iec_sync.

Data and masks
  - PRA bit masks (PRA_TXD / PRA_ATN_OUT / PRA_CLK_OUT / PRA_DATA_OUT /
    PRA_CLK_IN) describe the CIA2 Port A wiring to the IEC bus and CLOCK-IN
    sense bit. All writes are full-byte masks that always keep PRA_TXD high
    to avoid disturbing the user-port TXD line.
  - Composite masks (IEC_MASK_ATN_RELEASE, IEC_MASK_PRESET_BEFORE_SYNC,
    IEC_RECV_ODD/EVEN, IEC_SEND_*_D0/D1) encode the exact PRA patterns for:
      • Sync pre/post states.
      • Receive dibits (CLOCK high vs. low while sampling DATA IN).
      • Transmit dibits (CLOCK high vs. low with DATA selected by carry).
  - iec_shiftreg is a software shift register used symmetrically for RX and
    TX: ROL moves bits through carry so that both directions use MSB-first
    ordering. iec_cmd_track/iec_cmd_sector hold the first two command bytes
    for iec_send_cmd.

Bit-level protocol (dibit abstraction)
  - Each “dibit” operation (iec_send_dibit / iec_recv_dibit) handles two bits:
      • ODD phase: CLOCK high, DATA sampled/driven once.
      • EVEN phase: CLOCK low, DATA sampled/driven once.
  - For TX:
      • iec_send_byte loads A into iec_shiftreg and calls iec_send_dibit
        four times.
      • iec_send_dibit ROLs iec_shiftreg twice, using carry to select between
        “DATA=0” vs. “DATA=1” masks for ODD and EVEN, and writes those masks
        to cia2_pra with small timing gaps in between.
  - For RX:
      • iec_recv_byte calls iec_recv_dibit four times, then copies the final
        assembled byte from iec_shiftreg into A.
      • iec_recv_dibit writes receive masks to cia2_pra, reads PRA, ROLs A
        to pull DATA IN (bit7) into carry, then ROLs iec_shiftreg so the new
        bits accumulate MSB-first across successive calls.

Timing model
  - IEC_PRE_SYNC_DELAY / IEC_POST_SYNC_DELAY are simple Y-decrement loops that
    provide coarse pre/post delays around sync.
  - Short register “shuffle” sequences (TXA/TYA/PLA/TAY, etc.) inside the
    dibit routines create fixed-cycle gaps between ODD and EVEN phases and
    before return, shaping the clock period and setup/hold times.
  - No CIA timers are used; all timing is encoded in the instruction stream.

Call structure
  - High level:
      • iec_send_cmd → iec_sync + iec_send_byte
	  • iec_send_byte → iec_send_dibit
      • iec_recv_byte → iec_recv_dibit
  - The dibit routines are the only places that directly manipulate PRA bit
    patterns during data transfer; all higher-level routines work in terms of
    whole bytes and preconfigured masks.

Preconditions and side effects
  - Assumes DDRA is configured so that DATA/CLOCK/ATN bits are outputs and
    CLK-IN is an input. VIC bank selection (PRA bits 0–1) is clobbered by
    full-byte PRA writes and must be managed elsewhere.
	
================================================================================
 Techniques used in this file
================================================================================

MSB-first shift-register design:
	The code treats each byte as a left-rotating MSB-first bitstream. For transmit,
	ROL iec_shiftreg exposes the next MSB in carry; for receive, ROL A moves
	DATA-IN (bit7) into carry and ROL iec_shiftreg appends it at bit0. This creates
	a symmetric TX/RX path without bit-mask arithmetic.

Dibit framing (2-bit phases):
	All IEC traffic is encoded as “dibits”: each dibit performs two phases:
		ODD  = CLOCK high
		EVEN = CLOCK low
	Each phase samples or emits one bit. Four dibits form a full byte. Public
	routines operate on bytes; private dibit routines implement the waveform.

Active-low bus via full-mask PRA writes:
	DATA, CLOCK, and ATN are inverted by external hardware. Instead of toggling
	bits individually, the code uses composite full-byte masks for each phase
	(SEND/RECV ODD/EVEN, ATN release, preset). Masks always keep TXD high so
	user-port TXD never glitches during PRA writes.

Polling synchronization on CLK-IN:
	iec_sync performs a pre-delay, then spins with BIT cia2_pra and BEQ until the
	CLOCK-IN sense bit reads as released. After syncing, ATN is released using a
	composite mask, followed by a post-delay to stabilize the bus.

Timing via instruction-counted gaps:
	No CIA timers or interrupts are used. Precise bit timing is produced via:
		• Y-decrement busy-wait loops (coarse delays)
		• short register-shuffle sequences (fine gaps)
	The IEC waveform is defined entirely by CPU cycle counts.
*/

#importonce
#import "registers.inc"
#import "globals.inc"
#import "constants.inc"

//--- CIA2 Port A bit masks (PRA = $DD00) ---
// NOTE: IEC lines are active-low through inverters (7406): 0 asserts line, 1 releases it.
.const PRA_TXD      = %00000100   // Bit 2 — User-port TXD; always held high in masks to avoid glitches.
.const PRA_ATN_OUT  = %00001000   // Bit 3 — IEC ATN (via inverter): 0=assert ATN, 1=release ATN.
.const PRA_CLK_OUT  = %00010000   // Bit 4 — IEC CLOCK (via inverter): 0=drive clock low (assert), 1=release.
.const PRA_DATA_OUT = %00100000   // Bit 5 — IEC DATA  (via inverter): 0=drive data low (assert), 1=release.
.const PRA_CLK_IN   = %01000000   // Bit 6 — IEC CLOCK input; polled via BIT in the sync loop.

// --- Composite PRA write masks (phase/preset states) ---
//Note: TXD is explicitly kept high in all masks so full-byte PRA writes never
//accidentally drive TXD low and disturb the user-port side.
.const IEC_MASK_ATN_RELEASE           = PRA_ATN_OUT | PRA_TXD   // PRA: ATN+TXD high; releases ATN on active-low IEC bus.

.const IEC_MASK_PRESET_BEFORE_SYNC    = PRA_DATA_OUT | PRA_TXD  // Preset before sync: DATA+TXD high (released), ATN unchanged (“ready to send”).

// Receive phases (two samples per call): odd = CLOCK high, even = CLOCK low
.const IEC_RECV_ODD                   = PRA_CLK_OUT | PRA_ATN_OUT | PRA_TXD   // Receive ODD: drive CLOCK high, ATN/TXD released while sampling first bit.
.const IEC_RECV_EVEN                  = PRA_ATN_OUT | PRA_TXD                 // Receive EVEN: CLOCK low (no PRA_CLK_OUT), ATN/TXD released for second bit.

// Transmit phases (two emitted bits per call), select DATA based on C after ROL iec_shiftreg
.const IEC_SEND_ODD_D1                = PRA_DATA_OUT | PRA_CLK_OUT | PRA_ATN_OUT | PRA_TXD   // Send ODD, DATA=1: CLOCK high, DATA released, ATN/TXD released.
.const IEC_SEND_ODD_D0                = PRA_CLK_OUT | PRA_ATN_OUT | PRA_TXD                  // Send ODD, DATA=0: CLOCK high, DATA asserted low, ATN/TXD released.
.const IEC_SEND_EVEN_D1               = PRA_DATA_OUT | PRA_ATN_OUT | PRA_TXD                 // Send EVEN, DATA=1: CLOCK low, DATA released, ATN/TXD released.
.const IEC_SEND_EVEN_D0               = PRA_ATN_OUT | PRA_TXD                                // Send EVEN, DATA=0: CLOCK low, DATA asserted low, ATN/TXD released.

// --- Timing constants for simple busy-wait loops ---
.const IEC_PRE_SYNC_DELAY  = $28      // Loop count for pre-sync settle delay before polling CLOCK IN.
.const IEC_POST_SYNC_DELAY = $06      // Loop count for post-sync settle delay after releasing ATN.

// --- Working/storage locations ---
.label iec_shiftreg        = $4752    // Software shift register for IEC TX/RX bit pairs (ROL via carry).
.label iec_cmd_track       = $4753    // Latched IEC command parameter: track byte to send.
.label iec_cmd_sector      = $4754    // Latched IEC command parameter: sector byte to send.

/*
================================================================================
  iec_send_cmd
================================================================================
Summary
	Send a command to the disk drive.
	
	Transmit a 3-byte IEC command header (track, sector, operation) using the
	low-level bit-banged sender, after preparing CIA2 PRA and running iec_sync.

Arguments
		A                     Operation code (3rd command byte) to send.

Global Inputs
		iec_cmd_track         Latched track byte (1st command byte).
		iec_cmd_sector        Latched sector byte (2nd command byte).

Description
	* Saves the operation code from A on the stack.
	* Presets CIA2 PRA using IEC_MASK_PRESET_BEFORE_SYNC to put DATA/TXD into
	a known “released” state before bus sync.
	* Calls iec_sync to sync with the drive
	* Loads iec_cmd_track into A and calls iec_send_byte to transmit the first
	command byte.
	* Loads iec_cmd_sector into A and calls iec_send_byte to transmit the
	second command byte.
	* Restores the saved operation code from the stack into A and calls
	iec_send_byte again to transmit the third command byte.
	* Leaves the bus in the post-send state established by the final call to
	iec_send_byte; higher-level code is responsible for any follow-up IEC
	handshakes or turnarounds.
================================================================================
*/
* = $46C2
iec_send_cmd:
		// Save the operation code (A) so we can transmit it last.
		pha

		// ------------------------------------------------------------
		// Preset bus outputs before syncing (mask defined by IEC_MASK_PRESET_BEFORE_SYNC).
		// NOTE: preserves protocol-defined idle levels for ATN/DATA/CLOCK as required.
		// ------------------------------------------------------------
		lda #IEC_MASK_PRESET_BEFORE_SYNC
		sta cia2_pra

		// Synchronize with the drive.
		jsr iec_sync

		// ------------------------------------------------------------
		// Transmit command bytes on the wire in this order:
		// track → sector → operation
		// Each byte is sent MSB-first by iec_send_byte (2 bits per phase).
		// ------------------------------------------------------------
		lda iec_cmd_track
		jsr iec_send_byte

		lda iec_cmd_sector
		jsr iec_send_byte

		// Restore the saved operation code and send it.
		pla
		jsr iec_send_byte

		rts
/*
================================================================================
  iec_sync
================================================================================
Summary
	Synchronize state with the disk drive.
	
	Wait for the IEC CLOCK IN line to be released at CIA2 PRA, then release ATN
	and apply pre/post delays so the bus is in a stable idle state before later
	IEC traffic.

Global Inputs
	cia2_pra                  Polled via BIT to test the CLOCK IN sense bit.

Global Outputs
	cia2_pra                  Written with IEC_MASK_ATN_RELEASE to release ATN
								on the IEC connector.
Description
	* Burns a pre-sync delay using IEC_PRE_SYNC_DELAY so the bus lines can
	  settle before sampling.
	* Loads PRA_CLK_IN into A and uses BIT cia2_pra / BEQ in a loop to
	  wait until the CLOCK IN sense bit reads set (line released after
	  inversion).
	* Writes IEC_MASK_ATN_RELEASE to cia2_pra, releasing ATN and establishing
	  a known PRA output pattern while keeping TXD high.
	* Burns a post-sync delay using IEC_POST_SYNC_DELAY so the peer has time
	  to observe the stable ATN/bus state before the next command or data
	  byte is transmitted.
================================================================================
*/
* = $46DC
iec_sync:
		// Pre-sync settle time: allow bus lines to reach idle before polling.
		ldy #IEC_PRE_SYNC_DELAY
sync_wait_1:
		dey
		bne sync_wait_1

        // ------------------------------------------------------------
		// Poll CLOCK IN until it reads *set* at PRA:
		//
		// A ← PRA_CLK_IN (bit mask to test)
		// BIT cia2_pra sets Z=1 when (A & cia2_pra) == 0  → line reads clear
		// Loop while Z=1 (clear), proceed when Z=0 (bit reads set/released).
        // ------------------------------------------------------------
		lda #PRA_CLK_IN
wait_for_CLOCK_IN_set:
		bit cia2_pra
		beq wait_for_CLOCK_IN_set

		// Release ATN
		lda #IEC_MASK_ATN_RELEASE
		sta cia2_pra

		// Post-sync settle time: give the peer time to observe ATN before traffic.
		ldy #IEC_POST_SYNC_DELAY
sync_wait_2:
		dey
		bne sync_wait_2
		rts
/*
================================================================================
  iec_recv_byte
================================================================================
Summary
	Receive one byte from the serial bus.
	
	Receive one byte from the IEC bus by invoking iec_recv_dibit four times and
	returning the assembled value in A.

Global Inputs
	iec_shiftreg      Working receive accumulator

Global Outputs
	iec_shiftreg      Contains the fully assembled 8-bit value after four
						iec_recv_dibit calls.
Returns
	A                 Received byte copied from iec_shiftreg after all eight
						bits have been sampled.

Description
	* Calls iec_recv_dibit four times in succession; each call samples two bits
	  from the IEC DATA line using odd/even CLOCK phases and merges them into
	  iec_shiftreg in MSB-first order.
	* After the fourth pair, loads the completed byte from iec_shiftreg into A
	  and returns to the caller, leaving X preserved and Y clobbered by the
	  inner receive logic.
================================================================================
*/
* = $46F3
iec_recv_byte:
		jsr iec_recv_dibit
		jsr iec_recv_dibit
		jsr iec_recv_dibit
		jsr iec_recv_dibit
		lda iec_shiftreg
		rts
/*
================================================================================
  iec_recv_dibit
================================================================================
Summary
	Receive a dibit (2 bits) from the serial bus.
	
	Sample two consecutive bits from the IEC bus via CIA2 PRA, using an odd
	(CLOCK high) phase followed by an even (CLOCK low) phase, and merge them
	into iec_shiftreg with MSB-first ordering.

Global Inputs
	iec_shiftreg      Partially assembled receive byte; higher bits already
						captured by prior calls.

Global Outputs
	iec_shiftreg      Rotated left twice; two newly received bits are inserted
						at the low end, advancing the assembled byte.

Description
	* Drives the IEC bus into an “odd” (CLOCK high) receive configuration,
	  then samples the DATA IN bit from CIA2 PRA and ROLs it into the low end
	  of iec_shiftreg.
	* Executes a short, fixed-cycle delay to satisfy IEC timing between odd
	  and even phases.
	* Drives the IEC bus into an “even” (CLOCK low) receive configuration,
	  samples DATA IN again, and ROLs the second bit into iec_shiftreg.
	* After one call, iec_shiftreg has been advanced by two bits, preserving
	  previously received bits and appending the new pair MSB-first so that
	  four calls build a full received byte for iec_recv_byte.
================================================================================
*/
* = $4703
iec_recv_dibit:
        // ------------------------------------------------------------
		// Odd phase (clock high): sample DATA IN and shift into iec_shiftreg.
        // ------------------------------------------------------------
		ldy #IEC_RECV_ODD       // Setup output mask: ATN released, CLOCK high (phase 1)
		lda cia2_pra 			// Read PRA (bit7 carries DATA IN after inversion)
		sty cia2_pra 			// Drive odd-phase output mask on PRA
		rol                     // DATA IN (bit7) → C (carry)
		rol iec_shiftreg        // iec_shiftreg <<= 1; insert C at bit0

        // ------------------------------------------------------------
		// Small timing gap between phases
        // ------------------------------------------------------------
		tya
		pha
		tya
		pla
		tya

        // ------------------------------------------------------------
		// Even phase (clock low): sample next bit and shift it in as well.
        // ------------------------------------------------------------
		ldy #IEC_RECV_EVEN      // Setup output mask: ATN released, CLOCK low (phase 2)
		lda cia2_pra 			// Read PRA again for next bit
		sty cia2_pra 			// Drive even-phase output mask on PRA
		rol                     // DATA IN (bit7) → C
		rol iec_shiftreg        // iec_shiftreg <<= 1; insert C at bit0

        // ------------------------------------------------------------
		// Small timing gap between phases
        // ------------------------------------------------------------
		tya
		tay
		rts
/*
================================================================================
  iec_send_byte
================================================================================
Summary
	Send one byte over the serial bus.
	
	Transmit one byte on the IEC bus by loading it into iec_shiftreg and calling
	iec_send_dibit four times, emitting eight bits MSB-first via CIA2 Port A.

Arguments
	A 					Byte to transmit MSB-first on the IEC DATA line.

Global Outputs
	iec_shiftreg        Loaded with the input byte, then rotated left eight times
						across the four iec_send_dibit calls

Description
	* Copies the caller’s byte from A into iec_shiftreg as a software shift
	register for MSB-first transmission.
	* Invokes iec_send_dibit four times
	* After four pairs, all eight bits of the original byte have been driven
	onto the IEC DATA line, synchronized to CLOCK transitions, satisfying
	the required two-phase (odd/even) bit timing for one full byte.
================================================================================
*/ 
* = $4723
iec_send_byte:
		sta iec_shiftreg
		jsr iec_send_dibit
		jsr iec_send_dibit
		jsr iec_send_dibit
		jsr iec_send_dibit
		rts
/*
================================================================================
  iec_send_dibit
================================================================================
Summary
	Send a dibit (2 bits) over the serial bus.
	
	Emit two consecutive bits from iec_shiftreg onto the IEC bus via CIA2 PRA,
	using an odd (CLOCK high) phase followed by an even (CLOCK low) phase,
	transmitting bits MSB-first and advancing the software shift register.

Global Inputs
	iec_shiftreg    Current MSB-first bit stream to send on the IEC DATA line.

Global Outputs
	iec_shiftreg    Rotated left twice; the two transmitted bits are consumed
	and the remaining bits are advanced for subsequent calls.

Description
	* Performs an odd (CLOCK high) transmit phase:
		* Loads the default odd-phase mask with DATA=0 into Y.
		* Rotates iec_shiftreg left to move the next MSB into carry.
		* If carry=1, selects the odd-phase mask with DATA=1 instead.
		* Writes the chosen odd-phase mask to cia2_pra, driving CLOCK high and
		DATA according to the bit value.
	* Executes a small, fixed-cycle timing gap before the even phase.
	* Performs an even (CLOCK low) transmit phase:
		* Loads the default even-phase mask with DATA=0 into Y.
		* Rotates iec_shiftreg left again to expose the following bit in carry.
		* If carry=1, selects the even-phase mask with DATA=1 instead.
		* Writes the chosen even-phase mask to cia2_pra, driving CLOCK low and
		DATA according to the bit value.
	* Executes a short settle delay and returns.
	* After one call, two bits (former bit7 then bit6) have been emitted
	MSB-first, and iec_shiftreg has been advanced by two bits for reuse
	by the next iec_send_dibit or iec_send_byte call.
================================================================================
*/
* = $4733
iec_send_dibit:
        // ------------------------------------------------------------
		// Emit two bits using two phases: ODD (clock high) then EVEN (clock low).
		// Default to DATA=0 mask for the odd phase; ROL moves next data bit into C.
        // ------------------------------------------------------------
		ldy #IEC_SEND_ODD_D0		// ODD phase mask: CLOCK high, DATA=0 (ATN as required)
		rol iec_shiftreg            // next bit → C
		bcc iec_set_phase_odd      	// if C=0 keep DATA=0; else select DATA=1 mask
		
		ldy #IEC_SEND_ODD_D1        // ODD phase mask: CLOCK high, DATA=1
iec_set_phase_odd:
		// Drive odd-phase outputs on PRA
		sty cia2_pra 	

        // ------------------------------------------------------------
		// Small timing gap between phases
        // ------------------------------------------------------------
		txa
		tya
		txa
		tay

        // ------------------------------------------------------------
		// EVEN phase: repeat with CLOCK low. Default to DATA=0; ROL exposes next bit in C.
        // ------------------------------------------------------------
		ldy #IEC_SEND_EVEN_D0   	// EVEN phase mask: CLOCK low, DATA=0
		rol iec_shiftreg            // next bit → C
		bcc iec_set_phase_even      // if C=0 keep DATA=0; else select DATA=1 mask
		
		ldy #IEC_SEND_EVEN_D1       // EVEN phase mask: CLOCK low, DATA=1
iec_set_phase_even:
		// Drive even-phase outputs on PRA
		sty cia2_pra 	

        // ------------------------------------------------------------
		// Short settle before returning
        // ------------------------------------------------------------
		txa
		tay
		rts


/*		
// ------------------------------------------------------------
// iec_send_cmd
// ------------------------------------------------------------
function iec_send_cmd(op_code):
    // Put DATA and TXD into a “released” idle state before syncing
    cia2_pra := IEC_MASK_PRESET_BEFORE_SYNC

    // Run pre/post delays, wait for CLOCK-IN released, then release ATN
    iec_sync()

    // Send 1st command byte: track
    byte_to_send := iec_cmd_track
    iec_send_byte(byte_to_send)

    // Send 2nd command byte: sector
    byte_to_send := iec_cmd_sector
    iec_send_byte(byte_to_send)

    // Send 3rd command byte: operation
    iec_send_byte(op_code)

    return


// ------------------------------------------------------------
// iec_sync
// ------------------------------------------------------------
function iec_sync():
    // Pre-sync delay (let bus settle)
    y := IEC_PRE_SYNC_DELAY
    while y != 0:
        y := y - 1

    // Wait until CLOCK-IN sense bit reads “released”
    mask := IEC_PRA_CLK_IN
    repeat:
        // BIT-style test: loop while (cia2_pra & mask) == 0
        if (cia2_pra & mask) == 0:
            goto repeat

    // Release ATN and keep TXD high with a composite mask
    cia2_pra := IEC_MASK_ATN_RELEASE

    // Post-sync delay (let peer observe ATN/bus state)
    y := IEC_POST_SYNC_DELAY
    while y != 0:
        y := y - 1

    return


// ------------------------------------------------------------
// iec_recv_byte
// ------------------------------------------------------------
function iec_recv_byte() -> byte:
    // Assumes iec_shiftreg was initialized appropriately by caller
    repeat 4 times:
        iec_recv_pair()   // each call appends 2 bits into iec_shiftreg (MSB-first)

    result := iec_shiftreg
    return result


// ------------------------------------------------------------
// iec_recv_pair
//   (receive 2 bits: odd phase then even phase)
// ------------------------------------------------------------
function iec_recv_pair():
    // -------- Odd phase: CLOCK high, sample first bit --------
    // Configure bus for odd-phase sampling (ATN released, CLOCK high)
    cia2_pra := IEC_RECV_ODD

    // Read current PRA and extract DATA-IN bit (after inversion)
    sample := cia2_pra
    first_bit := extract_data_in_bit(sample)     // logical 0/1

    // Shift new bit into receive shift register (MSB-first stream)
    // Conceptually: (shift left, insert new bit at LSB)
    iec_shiftreg := (iec_shiftreg << 1) | first_bit

    // Tiny timing gap between phases (few fixed-cycle instructions)

    // -------- Even phase: CLOCK low, sample second bit --------
    // Configure bus for even-phase sampling (ATN released, CLOCK low)
    cia2_pra := IEC_RECV_EVEN

    sample := cia2_pra
    second_bit := extract_data_in_bit(sample)

    iec_shiftreg := (iec_shiftreg << 1) | second_bit

    // Short settle delay before returning
    return


// ------------------------------------------------------------
// iec_send_byte
// ------------------------------------------------------------
function iec_send_byte(value):
    // Load software shift register with the byte to send MSB-first
    iec_shiftreg := value

    // Each pair call emits two bits (odd+even phase)
    repeat 4 times:
        iec_send_pair()

    // After 4 pairs, all 8 bits have been sent
    return


// ------------------------------------------------------------
// iec_send_pair
//   (send 2 bits: odd phase then even phase)
// ------------------------------------------------------------
function iec_send_pair():
    // -------- ODD phase: CLOCK high, send first bit --------
    // Default to “DATA=0” mask for odd phase
    mask := IEC_SEND_ODD_D0

    // Rotate MSB of current stream out as the next bit
    // Conceptually:
    //   next_bit := (iec_shiftreg >> 7) & 1
    //   iec_shiftreg := (iec_shiftreg << 1)
    next_bit := msb(iec_shiftreg)
    iec_shiftreg := (iec_shiftreg << 1)

    if next_bit == 1:
        mask := IEC_SEND_ODD_D1     // select “DATA=1” variant

    // Drive odd-phase outputs on PRA (CLOCK high, DATA per mask)
    cia2_pra := mask

    // Small timing gap between phases (fixed-cycle register shuffles)
	//...
	
    // -------- EVEN phase: CLOCK low, send second bit --------
    mask := IEC_SEND_EVEN_D0        // default “DATA=0” mask for even phase

    next_bit := msb(iec_shiftreg)
    iec_shiftreg := (iec_shiftreg << 1) & 0xFF

    if next_bit == 1:
        mask := IEC_SEND_EVEN_D1    // select “DATA=1” variant

    // Drive even-phase outputs on PRA (CLOCK low, DATA per mask)
    cia2_pra := mask

    // Short settle delay before returning
	//...
    return
*/		