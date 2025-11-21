/*
================================================================================
  Disk I/O high-level helpers – file overview
================================================================================

Summary
        High-level disk I/O layer for a C64 talking to a 1541-style drive over
        the serial bus. It wraps the low-level byte-at-a-time protocol with 
		routines for physical sector chaining, buffered streaming, and robust 
		retry/diagnostic behavior, using a fixed sector buffer at $0300 and 
		several small SMC helpers to keep the tight loops efficient.

Responsibilities
	- Geometry and chaining:
		- Track/sector “chain” seeding via (start_track,start_sector).
		- Physical stepping across sectors and tracks using
		max_sector_index_by_track and disk_next_phys_sector.
		
	- Buffered reads:
		- Read a chosen sector into SECTOR_BUFFER via
		disk_read_sector_into_buffer with retry and UI error reporting.
		- Provide a byte-at-a-time streaming API (disk_stream_next_byte) that
		automatically advances geometry and refills the buffer at sector
		boundaries.
		
	- Bulk transfers:
		- Copy arbitrary-length streams from disk into RAM via
		disk_stream_copy, using a self-modified STA absolute store.
		- Write linear memory buffers out as a sequence of whole sectors via
		disk_write_linear, rounding up the byte count and retrying failed
		writes until they succeed.
		
	- Protocol framing:
		- Implement the 0x01-prefixed escape protocol for reads
		(disk_read_sector), decoding literal-0x01, EOD, error, and sync
		requests, with a visual hang on unknown control codes.
		- Provide a low-level 256-byte write primitive (disk_write_sector)
		that uses a self-modified LDA absolute,X loop for throughput.
		
	- Disk-side management:
		- Offer a simple “ensure correct side inserted” helper
		(disk_ensure_correct_side) that patches a UI message, pauses
		gameplay, and loops until the side-ID sector matches the requested
		digit.
		- Provide a minimal drive-reset wrapper (disk_reset_drive) that
		sends DISK_CMD_RESET over IEC.

Core concepts
	- Physical sector chain:
		The file treats the disk as a purely physical sequence of
		(track,sector) tuples. The chain head (start_track,start_sector) is
		seeded once, and disk_next_phys_sector walks the 1541 geometry:
		increment sector, wrap to 0 and increment track when the per-track
		max index is exceeded. There is no notion of filesystem or linked
		allocation here; callers are responsible for any higher-level mapping.
	- Fixed sector buffer:
		All “high-level” reads land in SECTOR_BUFFER at $0300 via
		disk_read_sector_into_buffer. Streaming routines maintain a
		per-sector byte offset (disk_buf_off) and use it as an index into
		that buffer, refilling and advancing geometry when the offset wraps.
	- Streaming API:
		disk_stream_next_byte hides sector boundaries from callers:
		they see a flat byte stream, while the routine manages
		(current_track,current_sector), refilling the buffer and updating
		disk_buf_off and the geometry as needed. disk_stream_copy builds on
		this to transfer a fixed-length slice into linear RAM.
	- Self-modifying code:
		Several routines patch 16-bit operands in place so hot loops use
		absolute or absolute,X addressing:
			- disk_read_sector patches an STA absolute,X (disk_store_abs) so
			  data bytes store to disk_dest_ptr+X.
			- disk_write_sector patches an LDA absolute,X (disk_load_abs) so it
			  reads from disk_src_ptr+X.
			- disk_stream_copy patches an STA absolute (disk_dest_store_abs),
			  then increments the inlined operand to walk the destination as a
			  16-bit pointer.
		This requires the code to execute from RAM, but keeps inner loops as
		tight as possible.

Error handling and UI
	- Read and write wrappers (disk_read_sector_into_buffer,
	  disk_write_linear) treat the low-level protocol’s flag/escape
	  signals as recoverable events. They:
		- On error, print an error message, wait for a button press
		  and retry the same sector until success.
		- On success, update bookkeeping (e.g., last_track_loaded /
		  last_sector_loaded) and return.
	- A truly unexpected escape discriminator causes disk_read_sector to
	  paint the VIC border green (DISK_FAULT_BORDER) and spin in a hang
	  loop. This is intentionally intrusive for debugging bad drive code
	  or media.

Preconditions and invariants
	- disk_stream_copy requires a non-zero 16-bit byte count; a zero count
	  will underflow the counter and copy 64 KiB before terminating.
	- disk_write_linear assumes the total byte count is non-zero; if both
	  counter bytes are zero it will still attempt to write and then loop
	  forever.
	- disk_next_phys_sector does not clamp at end-of-disk; callers decide
	  when to stop advancing.
	- All protocol interactions assume the drive-side code understands the
	  DISK_CMD_* opcodes and the 0x01-prefixed escape format described in
	  the header above.

Integration points
	- Relies on disk_low_level.asm for the raw IEC transactions:
	  iec_send_cmd, iec_sync, iec_recv_byte, iec_send_byte, and the
	  associated global command parameters.
	- Uses pause.asm and ui_messages.asm to coordinate with the game loop
	  and to display blocking UI prompts (disk errors and disk side
	  instructions).
	- Exposes a small, consistent ABI to higher-level systems:
	  “point me at a track/sector, read into $0300, then stream or copy
	  bytes; or, write N bytes as whole sectors from this linear buffer.”

================================================================================
  Techniques and algorithms used in this module
================================================================================

This module combines several low-level and high-level techniques to provide a
robust, efficient disk-I/O abstraction for a 1541-style drive:

• Physical geometry stepping  
  Uses max_sector_index_by_track to walk real disk layout (advance sector, wrap
  to sector 0 and next track). No filesystem semantics; purely physical order.

• Fixed-buffer sector caching + byte-stream abstraction  
  All reads land in SECTOR_BUFFER ($0300). disk_stream_next_byte presents a
  continuous byte stream by auto-advancing geometry and refilling the buffer
  when the per-sector offset wraps.

• Self-modifying code for tight loops  
  Patches 16-bit operands of STA absolute, STA absolute,X, and LDA absolute,X so
  hot loops read/write directly from caller buffers with minimal overhead.

• Escape-coded read protocol  
  Interprets 0x01-prefixed sequences for literal-0x01, EOD, error, and sync.  
  Unknown sequences trigger a visual fault (green border) and hang for debugging.

• Linear-buffer → multi-sector write  
  Converts a byte count into whole-sector writes, advancing physical geometry
  and the source pointer page-by-page until all sectors are transmitted.

• Streaming copy with inlined pointer walking  
  disk_stream_copy auto-increments a self-modified absolute operand to walk a
  16-bit destination pointer while pulling bytes from the streaming reader.

================================================================================
*/

#importonce
#import "globals.inc"
#import "constants.inc"
#import "disk_low_level.asm"
#import "pause.asm"
#import "ui_messages.asm"

.const DISK_CMD_RESET      = $20    // Drive opcode: reset the disk drive
.const DISK_CMD_READ       = $30    // Drive opcode: request a sector read
.const DISK_CMD_WRITE      = $40    // Drive opcode: request a sector write

// --- Stream control markers (drive → host) ---
.const DISK_RX_ESC         = $01    // Escape prefix: signals that a control discriminator byte follows
.const DISK_RX_ESC_ESC     = $01    // 01 01 → literal 0x01 data byte (escaped)
.const DISK_RX_ERR         = $11    // 01 11 → error marker from drive (C=1 on return)
.const DISK_RX_SYNC        = $21    // 01 21 → host should call iec_sync and continue
.const DISK_RX_EOD         = $81    // 01 81 → end-of-data marker for the current read (C=0)

.const DISK_FAULT_BORDER   = $05    // VIC border color used by hang_loop to signal a fatal protocol fault (green)

.const SECTOR_BUFFER       = $0300  // Fixed 256-byte sector buffer page used by high-level readers

.const SIDE_ID_DIGIT_OFF   = $0C    // Offset of the '#' digit placeholder inside SIDE_ID_MSG
.const SIDE_ID_TRACK       = $01    // Track number that stores the side-ID byte (Track 1)
.const SIDE_ID_SECTOR      = $00    // Sector number that stores the side-ID byte (Sector 0)

/*
===========================================
  max_sector_index_by_track — max sector index per track (0-based)
 
  Summary:
    Lookup table indexed by track number. 
  	Each entry stores the maximum valid sector index for that track (0-based), not the count.
 
    Encodings (1541-style zones):
      $14 = 20 → 21 sectors (0..20)    // tracks 1–17
      $12 = 18 → 19 sectors (0..18)    // tracks 18–24
      $11 = 17 → 18 sectors (0..17)    // tracks 25–30
      $10 = 16 → 17 sectors (0..16)    // tracks 31–35
 
  Notes:
    • Entry 0 is a placeholder for “track 0” (unused on 1541).
    • If the count is needed, add 1 to the table value.
===========================================
*/
* = $45C7
max_sector_index_by_track:
        .byte $00                          //  0: unused placeholder
        .byte $14,$14,$14,$14,$14,$14,$14,$14,$14,$14  //  1–10: 21 sectors/track (max idx 20)
        .byte $14,$14,$14,$14,$14,$14,$14,$12,$12,$12  // 11–20: 21 (11–17), then 19 (18–20)
        .byte $12,$12,$12,$12,$11,$11,$11,$11,$11,$11  // 21–30: 19 (21–24), then 18 (25–30)
        .byte $10,$10,$10,$10,$10                      // 31–35: 17 sectors/track (max idx 16)
		
.label disk_buf_off        = $4639   // Current 0..255 byte offset into SECTOR_BUFFER; wrap triggers next-sector refill
.label disk_start_off      = $463A   // Seed intra-sector byte offset for the next disk_seek_and_read_sector/disk_init_chain_and_read
.label disk_chain_step     = $463B   // Number of physical sectors to step from (start_track,start_sector) in disk_seek_and_read_sector

.label start_sector        = $4633   // Seed 0-based sector index at the head of the physical sector chain
.label start_track         = $4634   // Seed track number at the head of the physical sector chain

.label disk_y_save         = $462F   // Saved copy of Y across disk_read_sector_into_buffer
.label disk_x_save         = $4630   // Saved copy of X across disk_read_sector_into_buffer

.label disk_src_ptr        = $4631   // (lo/hi) shared buffer pointer; 256-byte source page for writes
.label disk_dest_ptr       = $4631   // (lo/hi) shared buffer pointer; base destination for reads (aliases disk_src_ptr)

.label current_sector      = $4635   // Current 0-based sector index for streaming/read/write operations
.label current_track       = $4636   // Current track number for streaming/read/write operations

.label last_sector_loaded  = $4637   // 0-based sector index of the last sector successfully read into SECTOR_BUFFER
.label last_track_loaded   = $4638   // Track number of the last sector successfully read into SECTOR_BUFFER

.label disk_copy_count_lo  = $463C   // Low byte of remaining byte count for stream copy/write
.label disk_copy_count_hi  = $463D   // High byte of remaining byte count; reused as sector countdown in disk_write_linear


/*
================================================================================
  disk_init_chain
================================================================================
Summary
		Seed the start of a physical sector chain from (X=sector,Y=track) and 
		clear chain/bookkeeping state.

Arguments
        X  					Starting sector index (0-based).
        Y  					Starting track number.

Global Outputs
        start_sector        Seeded from X as the first sector in the chain.
        start_track         Seeded from Y as the first track in the chain.
        last_sector_loaded  Cleared to 0 to indicate no sector is currently cached.
        last_track_loaded   Cleared to 0 to indicate no sector is currently cached.
        disk_start_off      Cleared to 0 so reads begin at byte 0 in the sector.
        disk_chain_step     Cleared to 0 so no sector stepping is pending.

Description
        - Capture the caller-provided (sector,track) pair as the physical head
          of a physical sector chain.
        - Reset the “last loaded” bookkeeping so code can detect that no sector
          has yet been read into the fixed buffer.
        - Initialize per-sector offset and chain index so the next seek/read
          naturally targets byte 0 of the head sector.
================================================================================
*/
* = $44D3
disk_init_chain:
        // Initialize chain start from caller registers
        // NOTE: This routine expects X=sector, Y=track
        stx start_sector                  
        sty start_track                   

        // Clear “last T/S loaded” bookkeeping (no sector read yet)
        lda #$00
        sta last_sector_loaded            
        lda #$00
        sta last_track_loaded             

        // Start reading at the beginning of the sector (byte offset = 0)
        lda #$00
        sta disk_start_off                

        // Begin at the first sector in the chain (chain step 0)
        lda #$00
        sta disk_chain_step            	  
		
        rts                               
/*
================================================================================
  disk_init_chain_and_read
================================================================================
Summary
		Initialize the chain from (X,Y) and immediately read that seed sector 
		at offset 0 into SECTOR_BUFFER.
		
Arguments
        X  					Starting sector index (0-based) to seed the chain.
        Y  					Starting track number to seed the chain.

Global Outputs
        last_sector_loaded  Updated to the successfully loaded sector index.
        last_track_loaded   Updated to the successfully loaded track number.
        current_sector      Set to the resolved sector (same as start_* here).
        current_track       Set to the resolved track (same as start_* here).

Description
        - Call disk_init_chain so the seed (start_sector,start_track) comes
          from the caller’s X/Y, with all chain state zeroed.
        - Load X and Y from disk_start_off and disk_chain_step (both 0 after
          initialization).
        - Fall through into disk_seek_and_read_sector, which resolves the physical cursor,
          loads the selected sector into SECTOR_BUFFER, and returns to the
          original caller.
================================================================================
*/
* = $44EE
disk_init_chain_and_read:
         // Initialize chain defaults
        jsr disk_init_chain

        // Prepare arguments for the next routine:
		//
        //   X = intra-sector byte offset (0..255)
        //   Y = sector-chain step (how many sectors to step forward from start)
        ldx disk_start_off
        ldy disk_chain_step

        // Fall through intentionally to disk_seek_and_read_sector
        //   which expects X=start offset and Y=chain step
/*
================================================================================
  disk_seek_and_read_sector
================================================================================
Summary
		Step Y physical sectors from the start geometry, apply intra-sector 
		offset X, and read the resolved sector into SECTOR_BUFFER.
		
Arguments
        X  Intra-sector byte offset (0..255) to start reading from.
        Y  Sector-chain index: how many physical sectors to step forward from
           (start_sector,start_track).

Global Inputs
        start_sector        Base sector index (0-based) of the chain.
        start_track         Base track number of the chain.

Global Outputs
        disk_start_off      Updated to X (remembered intra-sector offset).
        disk_chain_step     Updated to Y (remembered physical step count).
        current_sector      Set to the resolved sector after applying Y steps.
        current_track       Set to the resolved track after applying Y steps.
        last_sector_loaded  Updated to current_sector on successful read.
        last_track_loaded   Updated to current_track on successful read.

Description
        - Copy the caller’s requested offset and chain index into persistent
          globals for later use or inspection.
        - Initialize the geometry cursor from (start_sector,start_track), then
          advance it by disk_chain_step sectors using disk_next_phys_sector
          with wrap across tracks.
        - Program disk_buf_off with the desired starting byte offset and invoke
          disk_read_sector_into_buffer to pull that sector into SECTOR_BUFFER
          with retry-on-error semantics.
================================================================================
*/
* = $44F7
disk_seek_and_read_sector:
		// Stash arguments X/Y into local vars
        stx disk_start_off                  // stash intra-sector byte offset
        sty disk_chain_step             	// stash physical sector step count

        // Initialize physical cursor to the start of the chain
        lda start_sector
        sta current_sector
        lda start_track
        sta current_track

        // If step == 0, we want the first sector: skip advancement
        lda disk_chain_step
        beq sector_chain_index_reached

        // Advance forward 'disk_chain_step' sectors in physical order
        tax                                 // X ← steps remaining
next_sector_in_chain:
        jsr disk_next_phys_sector       	
        dex
        bne next_sector_in_chain

        // Program the intra-sector read offset (starting byte within the sector)
sector_chain_index_reached:
        lda disk_start_off
        sta disk_buf_off

        // Load the selected sector into SECTOR_BUFFER; may retry on I/O errors
        jsr disk_read_sector_into_buffer
        rts
/*
================================================================================
  disk_stream_copy
================================================================================
Summary
		Copy a disk-backed byte stream from disk_stream_next_byte into a 
		linear RAM destination.
		
Arguments
        X  					Destination base address low byte.
        Y  					Destination base address high byte.
        disk_copy_count_lo  Low byte of total byte count to copy; must not be 0
							together with disk_copy_count_hi.
        disk_copy_count_hi  High byte of total byte count to copy.

Global Outputs
        current_sector      Advanced as needed when wrapping beyond the last
                            byte of a sector.
        current_track       Advanced when sectors wrap across tracks according
                            to disk_next_phys_sector.
        Destination memory  The range [X:Y] .. [X:Y] + (byte_count - 1) is
                            filled from the disk-backed stream.

Description
        - Patch the operand of an STA absolute instruction so that each store
          lands at the caller’s requested destination address.
        - In a loop, fetch bytes via disk_stream_next_byte so that crossing a
          sector boundary automatically advances geometry and refills
          SECTOR_BUFFER.
        - Store each byte through the SMC-patched STA, bump the inlined
          low/high operand to advance the destination by 1, and decrement the
          16-bit copy counter until it reaches zero.
        - Rely on the precondition that the counter is non-zero; otherwise the
          underflow behavior will cause a full 64 KiB copy before terminating.
================================================================================
*/
* = $451F
disk_stream_copy:
        // Destination address comes in X/Y
        // Patch the absolute STA operand with destination address:
        //   STA $FFFF  ← ($FFFF is self-modified below via disk_dest_store_abs)
        stx disk_dest_store_abs              
        sty disk_dest_store_abs + 1          

        // NOTE: This routine assumes the 16-bit byte counter > 0.
        // If both bytes are 0, the first iteration would underflow and loop forever.
read_loop:
        // Get next byte from the disk-backed stream.
        jsr disk_stream_next_byte

        // Store to the current destination (absolute operand is SMC-patched above).
        sta $FFFF                           // write A → [disk_dest_ptr]
.label disk_dest_store_abs = * - 2      	// points at the STA operand (lo/hi)

        // Advance destination pointer
        inc disk_dest_store_abs             
        bne decrement_counters              
        inc disk_dest_store_abs + 1         
		
        // Decrement remaining byte count
decrement_counters:
        lda disk_copy_count_lo   			
        bne decrement_counter_lo            
        dec disk_copy_count_hi   					
decrement_counter_lo:
        dec disk_copy_count_lo   			

        // Loop while (hi|lo) != 0 (i.e., bytes remain to copy).
        lda disk_copy_count_lo
        ora disk_copy_count_hi
        bne read_loop

        rts
/*
================================================================================
  disk_write_linear
================================================================================
Summary
		Write a rounded-up sequence of sectors from a linear 256-byte–paged buffer 
		starting at (start_track,start_sector), retrying on errors.
		
Arguments
        X                     Low byte of disk_src_ptr, the base of the first
                              256-byte page to write.
        Y                     High byte of disk_src_ptr.
        disk_copy_count_lo    Low byte of total byte count to write.
        disk_copy_count_hi    High byte of total byte count to write.
        start_sector          Starting sector index (0-based) for the write
                              chain.
        start_track           Starting track number for the write chain.

Global Outputs
        current_sector        Advanced for each successful sector, ending at
                              the sector immediately after the last one written.
        current_track         Advanced across tracks as sectors roll over.
        disk_src_ptr          Updated to point one page beyond the last sector
                              written.
        disk_copy_count_hi    Used as a countdown of sectors remaining; reaches
                              0 when the loop terminates.

Description
        - Initialize disk_src_ptr from X/Y and seed the working geometry
          cursors (current_sector,current_track) from start_sector/start_track.
        - Round the total byte count up to an integral number of sectors by
          adding 1 to disk_copy_count_hi if any low-byte remainder exists.
        - For each sector:
          - Map I/O in, call disk_write_sector for the current geometry and
            source page, and loop on error while showing DISK_ERROR_MSG via
            print_message_wait_for_button.
          - On success, map I/O out, advance to the next physical sector via
            disk_next_phys_sector, bump disk_src_ptr by +256 bytes, and
            decrement the sector countdown.
        - Stop once all counted sectors have been written; callers must avoid
          passing a zero total byte count, which would otherwise produce an
          infinite loop.
================================================================================
*/
* = $4547
disk_write_linear:
		// Source pointer comes in X/Y
        // Initialize source base pointer for this batch (256-byte pages)
        stx disk_src_ptr                         
        sty disk_src_ptr + 1                     

        // Seed write cursor with starting geometry
        lda start_sector
        sta current_sector
        lda start_track
        sta current_track

        // Round total byte count up to whole sectors:
        // sectors_to_write = hi + (lo != 0 ? 1 : 0)
        // NOTE: disk_copy_count_hi is used as the loop counter after this.
        lda disk_copy_count_lo
        beq try_write
        inc disk_copy_count_hi

try_write:
        // Map I/O in
        ldy #MAP_IO_IN
        sty cpu_port

        // Write one full sector from disk_src_ptr .. disk_src_ptr+$00FF to (current_track,current_sector)
        // disk_write_sector returns C=0 on success, C=1 on error
        ldx current_track
        ldy current_sector
        jsr disk_write_sector
        bcc write_succeeded                // success → advance to next sector

        // Error path: show error message and retry the same sector
        lda #<DISK_ERROR_MSG
        sta print_msg_ptr
        lda #>DISK_ERROR_MSG
        sta print_msg_ptr + 1
        jsr print_message_wait_for_button
        jmp try_write

write_succeeded:
        // Map I/O out
        ldy #MAP_IO_OUT
        sty cpu_port

        // Advance geometry cursor to the next physical sector
        jsr disk_next_phys_sector

        // Advance source pointer by +256 bytes (next page of the buffer)
        inc disk_src_ptr + 1

        // Loop until all pages have been written
        dec disk_copy_count_hi
        bne try_write

        rts
/*
================================================================================
  disk_stream_next_byte
================================================================================
Summary
		Return the next byte from SECTOR_BUFFER, advancing disk_buf_off and 
		auto-refilling on sector boundaries.

Returns
        A  				Next byte from SECTOR_BUFFER at the current stream position.
        disk_buf_off	Incremented by 1; wraps to 0 after $FF and triggers a sector
						advance/refill.

Global Inputs
        disk_buf_off          Current byte offset into SECTOR_BUFFER.
        current_sector        Current sector index driving the stream.
        current_track         Current track index driving the stream.

Global Outputs
        disk_buf_off          Updated to the next byte position, or reset to 0
                              after loading the next sector.
        current_sector        Advanced when crossing the end of a sector.
        current_track         Advanced by disk_next_phys_sector as sectors wrap
                              across tracks.

Description
        - Use disk_buf_off as an index into SECTOR_BUFFER to fetch the next
          byte, and push that byte on the stack so it can be returned in A
          regardless of refills.
        - Increment disk_buf_off; if it does not wrap, restore Y, pull A, and
          return immediately.
        - On wrap from $FF to $00, advance the geometry via
          disk_next_phys_sector and load the next sector into SECTOR_BUFFER
          with disk_read_sector_into_buffer, then reassert disk_buf_off = 0.
        - Restore Y and pull the previously fetched byte into A so that Z/N
          reflect the returned data value.
================================================================================
*/
* = $458E
disk_stream_next_byte:
        // Save caller's Y
        sty disk_y_saved_2

        // Use current per-sector offset as index into the sector buffer
        ldy disk_buf_off

        // Fetch the byte at SECTOR_BUFFER + offset
        // (We’ll return this byte in A even if we need to refill the buffer.)
        lda SECTOR_BUFFER,Y

        // Preserve the fetched byte while we potentially advance/refill
        // PLA later will restore A and set N/Z to the returned value.
        pha

        // Post-increment the offset; wrap to $00 after $FF
        inc disk_buf_off

        // If no wrap (offset != 0), we’re still within the same sector → done
        bne publish_byte

        // Offset wrapped to 0 → we just consumed the last byte of this sector
        // Advance to the next valid (track,sector) and load it into the read buffer
        jsr disk_next_phys_sector     			
        jsr disk_read_sector_into_buffer        

        // Start at the beginning of the freshly loaded sector
        lda #$00
        sta disk_buf_off

publish_byte:
        // Restore caller’s Y, then restore A = fetched byte
        ldy disk_y_saved_2
        pla
        rts

disk_y_saved_2:
        .byte $00                         // one-byte save area for Y
/*
================================================================================
  disk_next_phys_sector
================================================================================
Summary
		Increment current_sector. Wrap to (next track, sector 0) when the 
		per-track maximum index is exceeded.

Global Inputs
        current_sector        		0-based sector index prior to increment.
        current_track         		Track number prior to increment.
        max_sector_index_by_track	Lookup table of per-track maximum sector index
									(0-based) for the medium.

Global Outputs
        current_sector        Incremented by one if still within the track’s
                              maximum index; otherwise reset to 0 on wrap.
        current_track         Unchanged if the new sector index is in range;
                              incremented by one when a wrap occurs.

Description
        - Increment the 0-based sector index and fetch the track’s maximum
          sector index from max_sector_index_by_track.
        - If the new index is less than or equal to the maximum, keep the
          current track and use the updated sector.
        - If the new index exceeded the maximum, wrap the sector back to 0 and
          advance the track, leaving end-of-disk policy to the caller.
================================================================================
*/
* = $45AE
disk_next_phys_sector:
        // Advance to the next sector index (0-based)
        inc current_sector

        // Geometry check: compare the new sector index (A) against the
        // track’s MAX valid sector index from the table (0-based):
        //   A = current_sector
        //   Y = current_track (table index)
        lda current_sector
        ldy current_track
        cmp max_sector_index_by_track,Y          

        // In range if A ≤ max:
        //   BCC → A < max  → ok
        //   BEQ → A == max → ok (exactly the last valid sector)
        bcc exit_dnsp
        beq exit_dnsp

        // Overflowed past last sector on this track
		// Advance to next track, sector 0
        lda #$00
        sta current_sector
        inc current_track

exit_dnsp:
        rts
/*
================================================================================
  disk_read_sector_into_buffer
================================================================================
Summary
		Read (current_track,current_sector) into SECTOR_BUFFER 
		with retry and UI error reporting until successful.

Global Inputs
        current_sector        Sector index (0-based) to read.
        current_track         Track number to read.

Global Outputs
        disk_dest_ptr         Set to SECTOR_BUFFER so disk_read_sector stores
                              into it.
        last_sector_loaded    Updated to current_sector after a successful read.
        last_track_loaded     Updated to current_track after a successful read.

Description
        - Save caller X/Y into dedicated scratch bytes so they can be
          restored later.
        - Initialize disk_dest_ptr so disk_read_sector’s self-modifying store
          targets SECTOR_BUFFER.
        - In a loop, map I/O in, invoke disk_read_sector for the current
          (track,sector), and map I/O out afterward.
        - On error (C=1), point print_msg_ptr at DISK_ERROR_MSG, call
          print_message_wait_for_button, and retry the same sector.
        - On success (C=0), record the loaded geometry in last_*_loaded,
          restore caller X/Y, and return.
================================================================================
*/
* = $45EB
disk_read_sector_into_buffer:
        // Preserve caller registers used as temporary scratch by this routine
        stx disk_x_save
        sty disk_y_save

        // Configure destination buffer: disk_dest_ptr ← $0300 (SECTOR_BUFFER)
        lda #<SECTOR_BUFFER
        sta disk_dest_ptr
        lda #>SECTOR_BUFFER
        sta disk_dest_ptr + 1

attempt_read_sector:
        // Map I/O space in (make CIA/VIC etc. visible at $D000–$DFFF)
        ldy #MAP_IO_IN
        sty cpu_port

        // Kick off a sector read with current track/sector
        // Contract: disk_read_sector returns C=0 on success, C=1 on error/special-fail
        ldx current_track
        ldy current_sector
        jsr disk_read_sector

        // Hide I/O space again (restore RAM under I/O)
        ldy #MAP_IO_OUT
        sty cpu_port

        // Success (carry clear)?
        bcc read_succeeded

        // Failure path: set message pointer → print → retry indefinitely
        lda #<DISK_ERROR_MSG
        sta print_msg_ptr
        lda #>DISK_ERROR_MSG
        sta print_msg_ptr + 1
        jsr print_message_wait_for_button

        // Retry the entire read (same track/sector, same destination)
        jmp attempt_read_sector

read_succeeded:
        // Record which sector/track ended up in the buffer (status bookkeeping)
        lda current_sector
        sta last_sector_loaded
        lda current_track
        sta last_track_loaded

        // Restore caller’s X/Y and return (A/flags reflect last ops)
        ldx disk_x_save
        ldy disk_y_save
        rts
/*
================================================================================
  disk_write_sector
================================================================================
Summary
		Stream exactly 256 bytes from disk_src_ptr to the drive at (X=track,Y=sector).

Arguments
        X  					Track number of the target sector.
        Y  					Sector number of the target sector.
        disk_src_ptr		Base address (lo/hi) of a 256-byte buffer to transmit.

Returns
        Carry  				0 to signal successful transmission of 256 bytes.

Global Inputs
        disk_src_ptr        Base address of the sector payload in RAM.

Description
        - Latch the caller’s track and sector into the IEC command parameter
          globals for later use by the drive code.
        - Patch the operand of an LDA absolute,X instruction so it reads from
          disk_src_ptr, enabling a tight 256-byte streaming loop over the
          caller’s buffer.
        - Send the DISK_CMD_WRITE opcode with iec_send_cmd and align bus timing
          with iec_sync.
        - Loop X from 0 to $FF, loading from the patched absolute,X source and
          sending each byte via iec_send_byte.
        - When X wraps after 256 bytes, clear carry and return to the caller,
          leaving protocol-level completion and error detection to the higher
          layers.
================================================================================
*/
* = $4641
disk_write_sector:
        // Stash target location for the drive to write to
		// X = track, Y = sector
        stx iec_cmd_track       
        sty iec_cmd_sector      

        // Self-modify the absolute LDA used in the data loop so it reads from disk_src_ptr
        //   LDA $4000,X  ; operand ($4000) is patched below with disk_src_ptr (lo/hi)
        lda disk_src_ptr
        sta disk_load_abs                
        lda disk_src_ptr + 1
        sta disk_load_abs + 1            

        // Issue the “write sector” command, then synchronize bus state before data phase
        lda #DISK_CMD_WRITE
        jsr iec_send_cmd            		
        jsr iec_sync                  		

        // Stream exactly 256 bytes: X is the byte index (0..255). When X wraps to 0, we’re done.
        ldx #$00
send_next_byte:
        // Read next data byte from the caller’s buffer (absolute operand patched above)
		// Put data byte in .A
        lda $4000,X                         
.label disk_load_abs = * - 2

        // Send the byte over the serial bus to the drive
        jsr iec_send_byte

        // Advance to next byte; continue until X wraps ($FF→$00) after 256 sends
        inx									
        bne send_next_byte					

        // Success path (no error signalling from the inner loop): return C=0
        clc
        rts
/*
================================================================================
  disk_reset_drive
================================================================================
Summary
		Send DISK_CMD_RESET over the serial bus to request that the drive 
		perform its own reset sequence.

Description
        - Load the drive-reset opcode into A and call iec_send_cmd so that the
          command is framed and transmitted over IEC.
        - Return immediately after queuing the command; any required bus
          resynchronization or drive-state reinitialization is the caller’s
          responsibility.
================================================================================
*/
* = $4668
disk_reset_drive:
		// Load the protocol opcode for a drive reset.
		// (This command ignores track/sector; only the operation byte matters.)
        lda #DISK_CMD_RESET              

		// Send the reset command to the drive
        jsr iec_send_cmd
        rts
/*
================================================================================
  disk_read_sector
================================================================================
Summary
		Read a sector into disk_dest_ptr using an escape-coded stream that 
		handles EOD, errors, sync requests, and fatal protocol faults.

Arguments
        X  				Track number to read.
        Y  				Sector number to read.
        disk_dest_ptr	Base address (lo/hi) of the destination buffer

Returns
        Carry  			0 on end-of-data (success)
						1 if an error marker was seen.

Global Inputs
        disk_dest_ptr   Base destination address for received bytes.

Global Outputs
        disk_dest_ptr 	Destination memory filled with the received
                        sector payload until EOD or error.

Description
        - Latch the caller’s track and sector into disk command globals and
          patch the operand of a STA absolute,X instruction so that each data
          byte is stored at disk_dest_ptr+X, spilling into subsequent pages
          when X wraps.
        - Send DISK_CMD_READ and synchronize the serial bus before starting the
          data phase.
        - In a loop, receive bytes with iec_recv_byte and interpret a leading
          DISK_RX_ESC (0x01) as the start of a control sequence:
			  - ESC_ESC → treat as literal 0x01 data and store it.
			  - EOD     → set C=0 and return (successful end-of-data).
			  - ERR     → set C=1 and return (error reported by drive).
			  - SYNC    → call iec_sync and continue receiving.
			  - Any other discriminator → paint the border with DISK_FAULT_BORDER
				and enter an infinite hang for debugging.
        - For ordinary data bytes, store via the SMC-patched STA absolute,X,
          increment X, and on wrap increment the high byte of the store
          operand to continue into the next page.
================================================================================
*/
* = $466E
disk_read_sector:
        // Track and sector come in X/Y
		// Stash them into command parameters
        stx iec_cmd_track                  // track ← X
        sty iec_cmd_sector                 // sector ← Y

        // Patch the absolute store used below (self-modifying)
        //   STA $FFFF,X  ; operand ($FFFF) is replaced with disk_dest_ptr
        lda disk_dest_ptr
        sta disk_store_abs            		
        lda disk_dest_ptr + 1
        sta disk_store_abs + 1        		

        // Issue "read sector" command to the drive, then synchronize bus state
        lda #DISK_CMD_READ
        jsr iec_send_cmd
        jsr iec_sync

        // Use X as the running offset into the destination page
        // (X will wrap after $FF → $00; we detect that to bump the high byte)
        ldx #$00
next_byte:
        // Receive one byte from the serial bus
        jsr iec_recv_byte

        // Is this the start of a special sequence?
        cmp #DISK_RX_ESC
        bne store_in_buffer                	// no → ordinary data byte, store it

        // Special sequence: read the discriminator byte
        jsr iec_recv_byte

        // 01 01 → literal 0x01 (escaped), treat as normal data
        cmp #DISK_RX_ESC_ESC
        beq store_in_buffer

        // 01 81 → end-of-data: signal success (C=0) and return
        cmp #DISK_RX_EOD
        beq command_success

        // 01 11 → error: signal failure (C=1) and return
        cmp #DISK_RX_ERR
        beq command_error

        // 01 21 → sync request: re-sync and continue receiving
        cmp #DISK_RX_SYNC
        bne hang_loop                      	// unknown 01 xx ⇒ fatal: hang for debugging

        // Perform sync then resume the stream
        jsr iec_sync
        jmp next_byte

store_in_buffer:
        // Store byte at inlined destination + X
        // (absolute address below is patched at entry via disk_store_abs)
        sta $FFFF,X                        	// write to disk_dest_ptr + X
.label disk_store_abs = * - 2

        // Advance offset; if X wrapped ($FF→$00), bump the high byte of dest
        inx
        bne next_byte_2                    	
        inc disk_store_abs + 1        		
next_byte_2:
        jmp next_byte

command_error:
        sec                                 // C=1 ⇒ failure
        rts

command_success:
        clc                                 // C=0 ⇒ success
        rts

hang_loop:
        // Fatal/unknown sequence: mark screen border, loop forever
        lda #DISK_FAULT_BORDER
        sta vic_border_color_reg
        jmp hang_loop
/*
================================================================================
  disk_ensure_correct_side
================================================================================
Summary
		Block gameplay and repeatedly read the disk side-ID until it matches 
		the requested side digit, prompting the user as needed.
		
        The desired ID is a digit character code ('1' or '2') that is also 
		displayed in an on-screen prompt.

Arguments
        A  		Desired side ID as an ASCII digit code (e.g., $31 for '1').

Returns
        A  		Holds the matching side ID read from media when the routine returns.

Global Outputs
        active_side_id        Updated to the requested digit and used for
                              subsequent comparisons.
							  
Description
        - Patch the SIDE_ID_MSG string so that its placeholder digit displays
          the desired side ID, and remember that digit in active_side_id.
        - Call pause_game to suspend normal gameplay while the disk side is
          being verified.
        - In a loop:
          - Use disk_init_chain_and_read with (sector=SIDE_ID_SECTOR,
            track=SIDE_ID_TRACK) to read the side-ID sector into
            SECTOR_BUFFER.
          - Call disk_stream_next_byte to fetch the first byte from that
            sector, which is treated as the side ID on media.
          - If it matches active_side_id, call unpause_game and return with A
            holding the matching ID.
          - If it does not match, update print_msg_ptr to SIDE_ID_MSG, show the
            prompt via print_message_wait_for_button, and retry until the user
            inserts the correct disk side.
================================================================================
*/
* = $3AEB
disk_ensure_correct_side:
        // Patch the prompt with the requested digit and remember it for compare.
        sta SIDE_ID_MSG + SIDE_ID_DIGIT_OFF   	// display the desired side in the message
        sta active_side_id            			// keep for CMP below
		
        // Suspend gameplay while waiting for user to insert the correct side.
        jsr pause_game

read_side_id:
        // Seek and read the very first byte on disk (T=1,S=0) - this is the side ID.
        ldx #SIDE_ID_SECTOR
        ldy #SIDE_ID_TRACK
        jsr disk_init_chain_and_read          	
        jsr disk_stream_next_byte             	

        // Matches the desired digit? If yes, resume gameplay and return.
        cmp active_side_id
        bne side_id_mismatch
        jsr unpause_game
        rts

side_id_mismatch:
        // Show the prompt (now patched with the digit) and wait for acknowledgment.
        lda #<SIDE_ID_MSG
        sta print_msg_ptr
        lda #>SIDE_ID_MSG
        sta print_msg_ptr + 1
        jsr print_message_wait_for_button     // user swaps disk side, then presses button

        // Retry reading the side ID until it matches the desired digit.
        jmp read_side_id

/*
Pseudo-code

function disk_init_chain(sector, track):
    // Called with X=sector, Y=track in the real code
    start_sector      = sector
    start_track       = track

    last_sector_loaded = 0   // mark “no cached sector yet”
    last_track_loaded  = 0

    disk_start_off    = 0    // start at byte 0 of sector
    disk_chain_step   = 0    // first sector in chain


function disk_init_chain_and_read(sector, track):
    // ABI: X=sector, Y=track
    disk_init_chain(sector, track)

    offset    = disk_start_off   // always 0 after init
    step      = disk_chain_step  // always 0 after init

    // Fall through to “seek and read” using offset/step
    disk_seek_and_read_sector(offset, step)
    // Returns when disk_seek_and_read_sector completes


function disk_seek_and_read_sector(offset, step):
    // Persist caller’s parameters
    disk_start_off  = offset
    disk_chain_step = step

    // Initialize cursor from seed
    current_sector = start_sector
    current_track  = start_track

    // Advance “step” sectors in physical order
    for i in 1 .. step:
        disk_next_phys_sector()

    // Set initial byte offset inside the selected sector
    disk_buf_off = disk_start_off

    // Load (current_track,current_sector) into SECTOR_BUFFER with retry
    disk_read_sector_into_buffer()


function disk_stream_copy(dest_ptr, byte_count):
    // Precondition: byte_count > 0 (16-bit non-zero)
    // SMC: internally patches an inlined destination pointer to dest_ptr

    dst = dest_ptr
    remaining = byte_count

    while remaining > 0:
        byte = disk_stream_next_byte()   // abstract stream source
        *dst = byte                      // store to current destination
        dst = dst + 1                    // advance destination pointer

        remaining = remaining - 1


function disk_write_linear(src_ptr, byte_count):
    // src_ptr: base of linear buffer
    // byte_count: total bytes (16-bit)
    // Precondition: byte_count > 0 (file warns: zero ⇒ bad behavior)

    disk_src_ptr = src_ptr

    current_sector = start_sector
    current_track  = start_track

    // Convert bytes → sectors: ceil(byte_count / 256)
    sectors_to_write = byte_count / 256
    if (byte_count % 256) != 0:
        sectors_to_write += 1

    while sectors_to_write > 0:
        // Try to write one 256-byte page to (current_track,current_sector)
        success = disk_write_sector(current_track, current_sector, disk_src_ptr)

        if not success:
            // Show UI error and retry same sector
            print_msg_ptr = &DISK_ERROR_MSG
            print_message_wait_for_button()
            // Do not advance geometry or buffer; loop repeats
            continue

        // Successful sector write:
        //  - advance physical geometry
        //  - advance source pointer by 256 bytes
        disk_next_phys_sector()
        disk_src_ptr = disk_src_ptr + 256

        sectors_to_write -= 1


function disk_stream_next_byte() -> byte:
    // Fetch byte from current sector buffer
    index = disk_buf_off
    byte  = SECTOR_BUFFER[index]

    // Increment offset within sector
    disk_buf_off = (disk_buf_off + 1) & 0xFF

    if disk_buf_off != 0:
        // Still within same sector; no refill needed
        return byte

    // Wrapped past last byte in sector: advance geometry + refill buffer
    disk_next_phys_sector()
    disk_read_sector_into_buffer()
    disk_buf_off = 0   // start at beginning of new sector

    return byte


function disk_next_phys_sector():
    current_sector += 1

    max_index = max_sector_index_by_track[current_track]

    if current_sector <= max_index:
        // Still within this track
        return

    // Wrapped beyond last sector of this track
    current_sector = 0
    current_track  += 1
    // No global clamp: caller must enforce end-of-disk policy


function disk_read_sector_into_buffer():
    // Configure low-level destination
    disk_dest_ptr = &SECTOR_BUFFER[0]

    loop:
        // Low-level read of (current_track,current_sector) into disk_dest_ptr
        success = disk_read_sector(current_track, current_sector, disk_dest_ptr)

        if success:
            break

        // On error: show message and retry indefinitely
        print_msg_ptr = &DISK_ERROR_MSG
        print_message_wait_for_button()
        // Loop retries same (track,sector)

    // Bookkeeping: remember which sector is cached
    last_sector_loaded = current_sector
    last_track_loaded  = current_track



function disk_write_sector(track, sector, src_ptr) -> bool:
    // Latch command parameters for drive-side code
    iec_cmd_track  = track
    iec_cmd_sector = sector

    // Send “write sector” opcode and sync bus
    iec_send_cmd(DISK_CMD_WRITE)
    iec_sync()

    // Stream exactly 256 bytes: X = 0..255
    for i in 0 .. 255:
        byte = *(src_ptr + i)
        iec_send_byte(byte)

    // No error path signaled from here; carry=0 in real code
    return true


function disk_reset_drive():
    // Send single reset opcode; drive performs internal reset
    iec_send_cmd(DISK_CMD_RESET)


function disk_read_sector(track, sector, dest_ptr) -> bool:
    // Latch command parameters
    iec_cmd_track  = track
    iec_cmd_sector = sector

    // Send “read sector” opcode and sync
    iec_send_cmd(DISK_CMD_READ)
    iec_sync()

    offset = 0    // X in real code
    high_page_adjust = 0   // modeled by incrementing patched hi-byte

    loop:
        byte = iec_recv_byte()

        if byte != DISK_RX_ESC:
            // Ordinary data byte: store and advance offset
            *(dest_ptr + high_page_adjust + offset) = byte
            offset = (offset + 1) & 0xFF
            if offset == 0:
                // Crossed a page boundary; adjust destination high byte
                high_page_adjust += 256
            goto loop

        // Escape prefix: read discriminator
        code = iec_recv_byte()

        if code == DISK_RX_ESC_ESC:
            // Literal 0x01 data
            literal = 0x01
            *(dest_ptr + high_page_adjust + offset) = literal
            offset = (offset + 1) & 0xFF
            if offset == 0:
                high_page_adjust += 256
            goto loop

        if code == DISK_RX_EOD:
            // End-of-data: success
            return true

        if code == DISK_RX_ERR:
            // Error reported by drive
            return false

        if code == DISK_RX_SYNC:
            // Resync bus and continue
            iec_sync()
            goto loop

        // Unknown control code: fatal debug path
        set_vic_border_color(DISK_FAULT_BORDER)
        while true:
            ; // hang


function disk_ensure_correct_side(desired_digit_char):
    // desired_digit_char = '1' or '2' (ASCII)
    // Patch prompt and remember requested side
    SIDE_ID_MSG[SIDE_ID_DIGIT_OFF] = desired_digit_char
    active_side_id = desired_digit_char

    pause_game()

    loop:
        // Read side-ID sector: Track=SIDE_ID_TRACK, Sector=SIDE_ID_SECTOR
        disk_init_chain_and_read(sector = SIDE_ID_SECTOR,
                                 track  = SIDE_ID_TRACK)

        // First byte of that sector is the side ID on media
        side_id_from_disk = disk_stream_next_byte()

        if side_id_from_disk == active_side_id:
            // Correct side inserted
            unpause_game()
            return

        // Wrong side: show prompt and wait for user to swap disks
        print_msg_ptr = &SIDE_ID_MSG
        print_message_wait_for_button()

        // Try again (loop)

*/