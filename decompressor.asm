/*
================================================================================
  Hybrid RLE + 4-symbol dictionary decompressor (streaming, 1 byte/call)
================================================================================
 
Summary:
    Implements a compact decompression format that combines Run-Length Encoding
    with a 4-entry symbol dictionary. 
 
  	The decoder exposes a streaming API: you first load the dictionary from the input, 
  	then repeatedly call a routine that returns the next decompressed byte. 
  	Two helper routines can “skip” decompressed bytes without storing them (useful to fast-forward).
 
Arguments / State:
    decomp_src_ptr         	16-bit pointer to the compressed input stream.
    decomp_skip_rem         Requested count for “skip” helpers
                            (8/16-bit, depending on the routine).
 
State:
    decomp_dict4      		4 symbols (bytes) copied from the stream and
                            referenced by index during dictionary runs.
 
    decomp_emit_mode        Current mode flag (#$00 = DIRECT, #$FF = RUN).
                            Only bit 7 is tested to select the emission path.
 
    decomp_emit_rem         Remaining outputs in the current operation;
                            stores L meaning “emit L+1” total bytes.
 
    decomp_run_symbol       Latched byte to output while in RUN mode
                            (both ad-hoc and dictionary runs).
 
    decomp_y_save           Scratch to preserve Y inside the main routine.
 
 
Public entry points: 
    decomp_dict4_init       Loads 4 bytes from decomp_src_ptr into
                            decomp_dict4, advances decomp_src_ptr
                            by 4, and clears the current mode/counter.
 
    decomp_stream_next      Returns 1 decompressed byte per call in A;
                            maintains state across calls.
 
    decomp_skip_16bit          Discards decompressed bytes until decomp_skip_rem
                            reaches zero.
 
    decomp_skip_8bit           Discards decompressed bytes using an 8-bit
                            count loaded from decomp_skip_rem.
 
Private routines:
    decomp_read_src_byte    Fetches one byte from (decomp_src_ptr),
                            advances the pointer.
 
Returns:
    decomp_stream_next      A = next decompressed byte (Y preserved).
    Other routines          None (state/pointers updated as side effects).
 
================================================================================
  Technical refresher
================================================================================

RLE (Run-Length Encoding)
 
    RLE is a simple compression technique for repeating values: instead of storing
    “AAAAAA” as six separate ‘A’ bytes, we store a pair (run-length, symbol) and
    expand it at decode time. 
 
Dictionary
 
    Many data sets have a few very common symbols (e.g., space, zero, newline).
    A dictionary lets us assign a short index (here 2 bits → 4 entries) to those
    frequent symbols. By referencing them via an index instead of storing the
    full byte each time, we save space. 
 
  	The decoder loads them with decomp_dict4_init and can then use
    their indices in control bytes to trigger dictionary runs.
 
RLE variants
 
  	This compression format uses RLE in two flavors:
        AD-HOC RUN — repeats a literal byte read from the stream.
        DICTIONARY RUN — repeats a byte looked up in a small dictionary.
 
Direct mode
  	RLE (runs) only pays off when the next outputs repeat the same symbol.
  	If symbols don’t repeat, a run header would cost extra bytes with no gain.
 
  	Direct mode emits one symbol as-is, with no run header:
    	• for ad-hoc (non-dictionary) symbols,
    	• for dictionary symbols that occur once (no repetition),
    	• to avoid negative compression (run metadata > savings).
 
There are then 3 main operations defined:
 
  	-repeat an ad-hoc value				expressed as a run-length and the ad-hoc repeated symbol
  	-repeat a dictionary value			expressed as a run-length and the dictionary symbol index
  	-direct mode						expressed as a counter, followed by one or more symbols
 
Format overview
 
    -Each new “operation” begins with a control byte. 
  	-We denote its lower bits as an encoded length L. 
  	-Important: L means the decoder will emit L+1 bytes.
    -That convention allows lengths up to 64 for direct/ad-hoc runs and 32 for
    dictionary runs, while keeping the control byte compact.
 
    1) DIRECT mode (bits 7..6 = 00, i.e., control < $40)
       L = control (0..63) → output the next (L+1) bytes literally.
       Layout: [control] [b0] [b1] ... [bL]
  	   Output: [b0] [b1] ... [bL]
 
    2) AD-HOC RUN (bits 7..6 = 01, i.e., $40..$7F)
       L = control & $3F (0..63), next byte is the literal to repeat.
       Layout: [control] [literal]
       Output: (L+1) copies of that literal.
 
    3) DICTIONARY RUN (bit 7 = 1, i.e., ≥ $80)
       index = (control >> 5) & 3 selects one of 4 dictionary symbols.
       L = control & $1F (0..31) → output (L+1) copies of that symbol.
       Layout: [control]   (no extra byte)
  	   Output: (L+1) copies of dict4[index]
 
Streaming state machine:
 
    decomp_emit_rem holds L (“count-1”): 
  		when starting an operation we emit the first byte immediately; 
  		on subsequent calls we decrement first, then emit.
 
    decomp_emit_mode is #$00 for DIRECT and #$FF for RUN; 
  		only bit 7 matters, enabling a quick branch (BIT + BMI) to the proper emission path.
 
    decomp_run_symbol is captured at operation start for both AD-HOC and DICT runs.
 
Typical usage:
 
    1) Point decomp_src_ptr at the compressed buffer.
    2) Setup the dictionary by calling decomp_dict4_init   
    3) Loop calling decomp_stream_next until you’ve produced the desired
       number of decompressed bytes; store or process A each time.
    4) Use the skip helpers to fast-forward without materializing bytes.
 
Limits & notes:
 
    decomp_dict4 resides in page 1 ($0100..$0103), which is the 6502 stack
    page; we're safe as long as the stack never grows down to these addresses.
 
    All routines update the shared decoder state and decomp_src_ptr so that
    decoding can be paused/resumed seamlessly (including after skips).
	
================================================================================
Decompressor — technical summary of core techniques
================================================================================

• Hybrid literal + RLE + static-dictionary compression:
    The format combines DIRECT literals, AD-HOC RUNs of a literal byte, and
    DICT RUNs referencing a 4-symbol dictionary loaded from the stream. This is
    a lightweight RLE variant augmented with a tiny static dictionary to encode
    repeated-symbol regions compactly.

• Streaming state-machine decompression:
    The core routine (decomp_stream_next) operates as a finite-state machine,
    consuming one control byte when needed and producing exactly one output byte
    per call. All state (mode, remaining count, run symbol, source pointer) is
    persistent, enabling incremental decompression.

• Linear-time skip over compressed data:
    decomp_skip_8bit / decomp_skip_16bit repeatedly invoke the same state
    machine while discarding output, providing a correct “seek forward by N
    decompressed bytes” mechanism without reimplementing the decoder.

• Packed control-byte bitfields:
    Bits 7–6 select mode; bits 6–5 (DICT RUN) select dictionary index; low bits
    encode length as L meaning “emit L+1 bytes.” Keeps format compact.

================================================================================
C64/6502-specific techniques and optimizations
================================================================================

• 4-entry dictionary preloaded from stream:
    Four bytes are read into page-1 locations for cheap indexed access. DICT RUN
    reuses these entries as run symbols.

• Dictionary index extraction via shifts:
    DICT RUN parses the 2-bit index with LSR/AND, a compact 6502 bitfield
    extraction pattern.

• Single-bit mode dispatch:
    decomp_emit_mode uses bit7 only. A single BIT/BMI dispatch selects between
    DIRECT output (read literal) and RUN output (emit run symbol).

================================================================================
*/
#importonce
#import "globals.inc"

.label decomp_y_save       = $2D    // temporary storage for Y register within decomp_stream_next
.label decomp_skip_rem     = $2E    // decompressed-byte skip counter (low at $2e, high at $2f) for skip helpers
.label decomp_dict4   	   = $0100	// dictionary of symbols (4 symbols: $0100-0103)

.const DIRECT_MODE         = $00    // constant: direct mode selector
.const RUN_MODE            = $FF    // constant: run mode selector

/*
================================================================================
  decomp_dict4_init
================================================================================
   Initializes the symbol dictionary with 4 entries.
   
Arguments:
  	decomp_src_ptr			16-bit pointer to the start of compressed input data.
 
Description:
  	Reads four bytes from the current compressed data stream and copies them into
  	the symbol dictionary. These four symbols are used as the dictionary entries
  	for dictionary-based runs during decompression.
 
  	After copying, the compressed data pointer is advanced by four bytes so the
  	next read will begin immediately after the dictionary. The routine also resets
  	the decompression state (mode and counter) so that the next decompression call
  	starts cleanly.
================================================================================
*/
* = $0104
decomp_dict4_init:
		// Copy 4 dictionary bytes from input: Y = 3..0
		ldy #$03	   

dict_copy_loop:
		lda (decomp_src_ptr_lo),Y   	// read source byte at ptr+Y
		sta decomp_dict4,Y       	// store to dictionary [$0100..$0103]
		dey                          // next lower index
		bpl dict_copy_loop           // loop until Y = $FF

		// ------------------------------------------------	   
		// Advance input pointer by 4 (past the dictionary)
		// ------------------------------------------------
		clc
		lda decomp_src_ptr_lo
		adc #$04
		sta decomp_src_ptr_lo
		lda decomp_src_ptr_hi
		adc #$00
		sta decomp_src_ptr_hi

		// ------------------------------------------------	   
		// Reset state: no active operation, counter = 0
		// ------------------------------------------------	   
		lda #$00
		sta decomp_emit_rem
		sta decomp_emit_mode
		rts
/*
================================================================================
  decomp_stream_next
================================================================================
  Retrieves the next decompressed byte from the stream
 
Arguments:
  	decomp_src_ptr 				Pointer to current read position in compressed data.
  	decomp_dict4				4-entry symbol lookup table initialized earlier.
  	decomp_emit_rem				Remaining repetitions or direct bytes to output.
  	decomp_emit_mode 			Indicates current mode (#$00 = direct, #$FF = run).
  	decomp_run_symbol 			Holds symbol currently being repeated in run mode.
 
Returns:
  	A 							The next decompressed byte.
 
 
Description:
  	This is the core decompression routine implementing a hybrid RLE and dictionary
  	encoding scheme. 
	
	Each control byte from the input determines one of three modes: 
		• Direct Mode (bits 7–6 = 00)
			The byte value specifies how many literal bytes follow directly in the stream (L+1 bytes total)
 
		• Ad-hoc Run (bits 7–6 = 01)
			Bits 5-0 give a run length (L+1) and the following byte is the literal symbol to repeat
 
		• Dictionary Run (bit 7 = 1)
			Bits 6–5 select one of four dictionary symbols and bits 4–0 give a run length (L+1)
 
	Note that the symbol dictionary has to be set up first, before calling this routine.
	This is done via decomp_dict4_init.
================================================================================
*/
decomp_stream_next:
		// Preserve Y
		sty decomp_y_save
		
		// If an operation is already active (counter > 0), continue it
		lda decomp_emit_rem
		bne repeat_operation

		// ------------------------------------------------	   
		// No active op: fetch a control byte and configure the next op
		// ------------------------------------------------	   
		jsr decomp_read_src_byte
		
		// Classify by top bits
		cmp #$40
		bcs ctrl_ge_40

		// ------------------------------------------------	   
		//  DIRECT: ctrl < $40 → L in A. Set counter=L and output first raw byte.
		//  (Total bytes in this direct block = L+1)
		// ------------------------------------------------	   
		sta decomp_emit_rem
		
		// Set direct mode (bit7 clear)
		lda #DIRECT_MODE
		jmp set_emit_mode

ctrl_ge_40:
		cmp #$80
		bcs ctrl_ge_80

		// ------------------------------------------------	   
		// AD-HOC RUN: $40 ≤ ctrl < $80
		// Low 6 bits = L (repeat count-1), next byte = literal to repeat
		// ------------------------------------------------	   
		and #$3F
		sta decomp_emit_rem	   
		
		// Get the literal to repeat
		jsr decomp_read_src_byte
		jmp latch_run_symbol

		// ------------------------------------------------	   
		// DICTIONARY RUN: ctrl ≥ $80
		//  Bits 4..0 = L, bits 6..5 = dictionary index
		// ------------------------------------------------	   
ctrl_ge_80:
		// Extract L (run length-1) to counter
		tax
		and #$1F		
		sta decomp_emit_rem
		
		// Recover full ctrl in A
		txa
		
		// Compute index = (ctrl >> 5) & 3
		lsr 
		lsr 
		lsr 
		lsr 
		lsr 
		and #$03	
		tax
		
		// Fetch symbol from dictionary
		lda decomp_dict4,X
		
		// ------------------------------------------------	   
		// Initialize run with chosen symbol, then mark mode as RUN
		// ------------------------------------------------	   
latch_run_symbol:
		sta decomp_run_symbol
		
		// Set run mode (bit7 set)
		lda #RUN_MODE
set_emit_mode:
		sta decomp_emit_mode
		
		// New op just set up: skip the pre-decrement path and emit first byte
		jmp emit_by_mode

repeat_operation:
		// Active op: decrement remaining count before emitting this byte
		dec decomp_emit_rem

emit_by_mode:
		// Decide emission path by bit7 of mode: RUN (negative) vs DIRECT (non-negative)
		bit decomp_emit_mode
		bmi emit_run_byte

		// ------------------------------------------------	   
		// DIRECT: output next raw byte from input
		// ------------------------------------------------	   
		jsr decomp_read_src_byte
		jmp return_byte

		// ------------------------------------------------	   
		// RUN: output previously latched decomp_run_symbol
		// ------------------------------------------------	   
emit_run_byte:
		lda decomp_run_symbol

return_byte:
		// Restore Y and return byte in A
		ldy decomp_y_save
		rts
/*
================================================================================
  decomp_read_src_byte
================================================================================
  Reads one byte from the compressed data stream.
 
Arguments:
  	decomp_src_ptr			Pointer to current position in compressed data.
 
Returns:
  	A						Byte read from compressed data.
 
Description:
  	Fetches a single byte from the memory location pointed to by decomp_src_ptr,
  	then automatically increments the pointer. This routine is used by higher-level
  	decompression functions whenever a new byte is required from the input stream.
================================================================================
*/
decomp_read_src_byte:
       ldy #$00                     // Y=0 so (ptr),Y reads at ptr
       lda (decomp_src_ptr_lo),Y   	// fetch byte
       inc decomp_src_ptr_lo       	// bump low byte
       bne return_read              // if not wrapped, done
       inc decomp_src_ptr_hi   	// else bump high byte
return_read:
       rts
/*
================================================================================
  decomp_skip_16bit
================================================================================
  Skips a specified amount of decompressed data (16-bit count).
 
Arguments:
  	decomp_skip_rem			16-bit counter specifying how many decompressed bytes to skip.
 
Description:
  	Runs the decompression routine repeatedly to simulate producing data without
  	storing it. Each decompressed byte decrements decomp_skip_rem until it reaches zero.
  	This version supports skipping up to 64 kb.
 
  	This function is useful for fast-forwarding through portions of compressed
  	data without needing to actually copy or process the output.
================================================================================
*/
decomp_skip_16bit:
       // If decomp_skip_rem == 0, nothing to skip
       lda decomp_skip_rem
       ora decomp_skip_rem + 1
       bne skip16_step
       rts

skip16_step:
       // Consume one decompressed byte (discard result)
       jsr decomp_stream_next
	   
       // Decrement 16-bit decomp_skip_rem (low then high if needed)
       lda decomp_skip_rem
       bne dec_skip_lo
	   
       // decomp_skip_rem low is zero → borrow from high
       dec decomp_skip_rem + 1
	   
dec_skip_lo:
       dec decomp_skip_rem
       jmp decomp_skip_16bit
/*
================================================================================
  decomp_skip_8bit
================================================================================
  Skips a specified amount of decompressed data (8-bit count).
 
Arguments:
  	decomp_skip_rem			8-bit counter specifying how many decompressed bytes to skip.
 
Description:
  	A compact version of decomp_skip_16bit for skipping up to 255 bytes.
  	It repeatedly calls the decompression routine, discarding each byte produced,
  	until the counter reaches zero. Commonly used for small, localized skips in
  	the compressed data stream.
================================================================================
*/
decomp_skip_8bit:
       // Load 8-bit count; if zero, done
       ldy decomp_skip_rem
       bne skip8_step
       rts
	   
skip8_step:
       // Consume one decompressed byte (discard result)
       jsr decomp_stream_next
	   
       dey
       bne skip8_step
       rts
/*
  HYBRID RLE + 4-SYMBOL DICTIONARY DECOMPRESSOR — TEXT FLOW DIAGRAM
  =================================================================
 
  [decomp_dict4_init]
  ------------------------
  ENTRY
    │
    ├─ Y ← #$03                                			// copy 4 bytes (indices 3..0)
    ├─ LOOP:  A ← (decomp_src_ptr),Y
    │         decomp_dict4[Y] ← A
    │         Y ← Y-1
    │         IF Y ≥ 0 THEN LOOP
    │
    ├─ Advance decomp_src_ptr by +4       				// skip past dictionary
    │
    ├─ decomp_emit_rem ← 0
    ├─ decomp_emit_mode ← 0                     		// clear state
    └─ RTS
 
 
  [decomp_stream_next]
  ----------------------------
  ENTRY
    │
    ├─ Save Y into decomp_y_save
    │
    ├─ IF decomp_emit_rem ≠ 0 THEN
    │      ├─ decomp_emit_rem ← decomp_emit_rem - 1  	// continue current op
    │      └─ GOTO EMIT
    │
    ├─ ELSE  (no active op: parse a control byte)
    │      ├─ ctrl ← decomp_read_src_byte()
    │      │
    │      ├─ IF ctrl < $40  (DIRECT) THEN
    │      │     ├─ decomp_emit_rem ← ctrl        		// L (will emit L+1 total)
    │      │     ├─ decomp_emit_mode ← $00      		// DIRECT_MODE (bit7=0)
    │      │     └─ GOTO EMIT
    │      │
    │      ├─ IF $40 ≤ ctrl < $80  (AD-HOC RUN) THEN
    │      │     ├─ decomp_emit_rem ← (ctrl & $3F)       // L
    │      │     ├─ decomp_run_symbol ← decomp_read_src_byte()
    │      │     ├─ decomp_emit_mode ← $FF             	// RUN_MODE (bit7=1)
    │      │     └─ GOTO EMIT
    │      │
    │      └─ ELSE  (ctrl ≥ $80 → DICTIONARY RUN)
    │            ├─ L        ←  ctrl & $1F
    │            ├─ index    ← (ctrl >> 5) & 3
    │            ├─ decomp_emit_rem ← L
    │            ├─ decomp_run_symbol ← decomp_dict4[index]
    │            ├─ decomp_emit_mode ← $FF             	// RUN_MODE
    │            └─ GOTO EMIT
    │
  EMIT:
    │  Decide by decomp_emit_mode bit7:
    │
    ├─ IF RUN_MODE (bit7=1) THEN
    │      A ← decomp_run_symbol
    │
    ├─ ELSE (DIRECT_MODE, bit7=0)
    │      A ← decomp_read_src_byte()
    │
    ├─ Restore Y from decomp_y_save
    └─ RTS   											// A = next decompressed byte
 
 
  [decomp_read_src_byte]
  ---------------------------
  ENTRY
    │
    ├─ A ← (decomp_src_ptr)               				// with Y=0
    ├─ decomp_src_ptr.low  ← +1
    ├─ IF low wrapped to 0 THEN
    │      decomp_src_ptr.high ← +1
    └─ RTS   											// A = fetched byte
 
 
  [decomp_skip_16bit]
  ----------------------------
  ENTRY
    │
    ├─ IF decomp_skip_rem == 0 THEN RTS
    │
    ├─ LOOP:
    │     JSR decomp_stream_next       					// discard A
    │     IF decomp_skip_rem.low == 0 THEN
    │         decomp_skip_rem.high ← decomp_skip_rem.high - 1
    │     decomp_skip_rem.low  ← decomp_skip_rem.low - 1
    │     IF decomp_skip_rem != 0 THEN LOOP
    │
    └─ RTS
 
 
  [decomp_skip_8bit]
  ---------------------------
  ENTRY
    │
    ├─ Y ← decomp_skip_rem.low
    ├─ IF Y == 0 THEN RTS
    │
    ├─ LOOP:
    │     JSR decomp_stream_next       					// discard A
    │     Y ← Y - 1
    │     IF Y ≠ 0 THEN LOOP
    │
    └─ RTS
 
 
  FORMAT & STATE (for reference while reading the flow)
  -----------------------------------------------------
  • Control byte selects operation and encodes length L (which means “emit L+1 bytes”):
      DIRECT           : 00LLLLLL       (ctrl < $40) → then read (L+1) literal bytes
      AD-HOC RUN       : 01LLLLLL val   ($40..$7F)   → repeat ‘val’ (L+1) times
      DICTIONARY RUN   : 1IILLLLL       (≥ $80)      → index=II (0..3), repeat dict[index] (L+1) times
  • Internal state across calls:
      decomp_emit_rem     : remaining outputs for the current op (stores L; the routine emits on entry,
                         and pre-decrements on subsequent calls).
      decomp_emit_mode  : $00 (DIRECT) / $FF (RUN); only bit7 is tested at EMIT time.
      decomp_run_symbol : latched at op start for both AD-HOC and DICT runs.
      decomp_y_save           : saves caller’s Y.
  • Contract: call decomp_dict4_init once (copies 4 bytes, advances input by 4, clears state),
    then call decomp_stream_next repeatedly to stream the output. The skip_* helpers advance
    the same state while discarding data, allowing fast-forwarding within the compressed stream.
 */
