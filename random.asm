/*
================================================================================
 Pseudo Random Number Generator
================================================================================

The generator implements a 24-bit Linear Feedback Shift Register (LFSR) whose
state is stored in three bytes: rng_state_hi, rng_state_mid, and rng_state_lo.
Each call advances the register eight times and then combines the result with
a user-supplied 8-bit value in X to produce the output byte.


LFSR core
	• Feedback taps:
		The feedback bit is derived from (rng_state_hi bit6) XOR (rng_state_mid bit7).
		This pair of taps determines the feedback polynomial implicitly.

	• Shift sequence:
		Each iteration performs a left rotation through the three bytes:
		rng_state_hi ← (rng_state_hi << 1) | feedback
		rng_state_mid← (rng_state_mid<< 1) | carry
		rng_state_lo ← (rng_state_lo << 1) | carry
		After eight iterations, the state has been advanced by eight LFSR steps.
		The carry chain simulates a 24-bit shift register that wraps feedback into
		the least-significant bit of the high byte.

	• Entropy source:
		The bit pattern across rng_state_hi..lo encodes a 24-bit repeating sequence.
		A maximal-length tap combination would yield (2^24)-1 distinct non-zero states
		before repeating. The taps used here are non-standard, so period length may
		be shorter.


Mixer stage
	• After the LFSR step, the low byte (rng_state_lo) is copied into mix_gate,
	and the caller’s X is copied into mix_src_x.

	• The mixer uses a gated feedback loop:
		A := 0
		while mix_src_x ≠ 0:
			shift mix_gate left → C
			
			if C = 1: shift mix_src_x right, add mix_src_x to A
			else:     shift mix_src_x right only
			
	The carry from the shifts modulates how A accumulates X bits, injecting
	non-linear coupling between rng_state_lo and X.

• Result:
	The accumulator A contains the mixed byte returned to the caller.
	This post-processing hides LFSR linearity and slightly decorrelates output
	values from sequential state bytes.

Characteristics
	* State size: 24 bits
	* Steps per call: 8
	* Output width: 8 bits
	* Deterministic, repeatable sequence from initial seed.
	* Low computational cost (~80 cycles total).
	* Quality suitable for gameplay randomness, not cryptography.
================================================================================
*/
#importonce

.label rng_state_hi   = $dd    // 24-bit LFSR state: high byte
.label rng_state_mid  = $de    // 24-bit LFSR state: mid byte
.label rng_state_lo   = $df    // 24-bit LFSR state: low  byte

.label mix_src_x      = $e1    // copy of X used by mixer (shift/add source)
.label mix_gate       = $e2    // copy of rng_state_lo; shifted to gate adds via carry

/*
================================================================================
  generate_random_byte
================================================================================
Summary
	Produce an 8-bit pseudo-random value by advancing a 24-bit LFSR eight steps
	and mixing the updated low byte with the caller’s X via a gated shift/add
	accumulator.

Arguments
	.X  Secondary seed and mixer source.

Returns
	.A  Pseudo-random byte.

Global Inputs
	rng_state_hi   24-bit LFSR state, high byte
	rng_state_mid  24-bit LFSR state, mid  byte
	rng_state_lo   24-bit LFSR state, low  byte

Global Outputs
	rng_state_hi   advanced by 8 LFSR steps
	rng_state_mid  advanced by 8 LFSR steps
	rng_state_lo   advanced by 8 LFSR steps
	mix_gate       temp copy of rng_state_lo used by mixer (clobbered)
	mix_src_x      temp copy of X used by mixer (clobbered)

Description
	- LFSR advance (8 iterations):
		• Feedback bit = (rng_state_hi bit6) XOR (rng_state_mid bit7).
		• Rotate left the 24-bit state through carry (hi → mid → lo).
	- Mixer:
		• Initialize A := 0.
		• Use mix_gate (shifted MSB→C) to conditionally add shifted copies of
		mix_src_x into A until mix_src_x becomes zero.
	- Output is the mixed accumulator in A.
================================================================================
*/
* = $D7D4
generate_random_byte:
        // ----------------------------------------------------
        // Step LFSR 8 times (Y counts down from 7)
        // feedback bit := (rng_state_hi bit6) XOR (rng_state_mid bit7)
        // ----------------------------------------------------
        ldy     #$07            // Y := 7 → will run 8 total iterations (counts 7..0)

advance_lfsr_8_steps:
        lda     rng_state_hi    // A := high byte; source for feedback bit extraction
        asl                     // Shift left: bit6→bit7; C := old bit7(rng_state_hi)
                                //   Now A bit7 holds old bit6; C holds old bit7

        eor     rng_state_mid   // A := A XOR mid; mixes old hi.bit6 with mid bits
                                //   Targeting mid.bit7 specifically for feedback parity

        asl                     // Shift left: C := feedback = old(A bit7)
                                //   C now holds (hi.bit6 XOR mid.bit7)

        rol     rng_state_hi    // ROL through C: inject feedback into hi LSB; hi<<=1
                                //   C := old hi.bit7; Z/N per result

        rol     rng_state_mid   // Propagate carry into mid; mid<<=1
                                //   C := old mid.bit7

        rol     rng_state_lo    // Propagate carry into low; low<<=1
                                //   C := old low.bit7 (discarded); completes 24-bit step

        dey                     // Y := Y - 1
        bpl     advance_lfsr_8_steps // Loop until Y >= 0 → total 8 steps (Y:7..0)

        // ----------------------------------------------------
        // Mixer initialization
        // mix_gate := rng_state_lo (updated LFSR low)
        // mix_src_x := X (secondary seed)
        // ----------------------------------------------------
        lda     rng_state_lo        // Load mixer seed from LFSR low byte
        sta     mix_gate            // mix_gate := rng_state_lo (will shift to gate adds)
        stx     mix_src_x           // mix_src_x := X copy used as shift/add source

        // ----------------------------------------------------
        // Mixer loop: nonlinear accumulation using ASL/LSR/ADC
        // Produces final byte in A
        // ----------------------------------------------------
        lda     #$00                // A := 0 (accumulator init for mixer)
        beq     mix_loop_entry      // always branch; prior C is irrelevant (ASL below sets it)

mix_accumulate_with_carry:
        lsr     mix_src_x           // If gate=1: shift X-source once here...
        adc     mix_src_x           // ...then add it into A with carry (nonlinear mix)

mix_loop_entry:
        asl     mix_gate            		// Gate bit := msb(mix_gate); C := gate; mix_gate <<= 1
        bcs     mix_accumulate_with_carry 	// If gate=1 → do gated add path (extra shift+add)
        lsr     mix_src_x           		// Else gate=0 → shift X-source once this iteration
        bne     mix_loop_entry      		// Loop while any bits remain in mix_src_x
        rts                         		// Return with mixed byte in A

/*
Pseudo-code

function generate_random_byte(X : byte) -> byte
    // State: global 24-bit LFSR (rng_state_hi, rng_state_mid, rng_state_lo)

    // 1) Advance the 24-bit LFSR by 8 steps
    repeat 8 times:
        advance_lfsr_once()

    // 2) Initialize mixer inputs
    mix_gate  := rng_state_lo    // gate pattern from updated low byte
    mix_src_x := X               // copy of caller’s X for mixing
    A         := 0               // accumulator for final output

    // 3) Gated shift/add mixing loop
    while mix_src_x != 0:
        // Consume one gate bit from mix_gate (its top bit before shift)
        gate_bit := msb(mix_gate)
        mix_gate := (mix_gate << 1) & $FF

        if gate_bit == 1 then
            // “Gate open”: use this iteration to fold mix_src_x into A
            mix_src_x := mix_src_x >> 1          // shift source first
            A         := (A + mix_src_x) & $FF   // 8-bit add with carry
        else
            // “Gate closed”: just decay mix_src_x without adding
            mix_src_x := mix_src_x >> 1
        end if
    end while

    return A
end function


function advance_lfsr_once()
    // Combine two tap bits to form feedback
    tap_hi  := bit(rng_state_hi, 6)   // bit index 6 of high byte
    tap_mid := bit(rng_state_mid, 7)  // bit index 7 of mid byte
    feedback := tap_hi XOR tap_mid    // 0 or 1

    // Treat rng_state_hi/mid/lo as a single 24-bit value and rotate left:
    //   - Shift left by 1 bit.
    //   - Insert feedback into the new least-significant bit.
    temp := (rng_state_hi << 16) |
            (rng_state_mid << 8)  |
             rng_state_lo

    temp := ( (temp << 1) & $FFFFFF ) | feedback

    rng_state_hi  := (temp >> 16) & $FF
    rng_state_mid := (temp >> 8)  & $FF
    rng_state_lo  :=  temp        & $FF
end function

*/