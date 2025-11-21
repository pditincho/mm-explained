/*
================================================================================
 Overview
================================================================================

 These routines initialize the Commodore 64’s communication with the disk drive
 and upload a custom loader into the drive’s memory.

 1. Initial setup:
    - Configures the CPU port so I/O and KERNAL remain mapped in while BASIC
      is replaced by RAM.
    - Installs new interrupt vectors for IRQ, BRK, and NMI pointing to stable
      KERNAL handlers.

 2. CIA #2 configuration:
    - Enables all interrupt sources on CIA #2 (the serial/user-port interface).
    - Adjusts the data direction so most user-port pins are inputs, keeping one
      preserved bit.
    - Prepares Port A lines that handle the IEC serial bus signals.

 3. Drive upload process:
    - Uses standard KERNAL routines (SETNAM, SETLFS, OPEN, CHROUT, etc.) to
      open command channel 15 on device 8 (the disk drive).
    - Sends the "I0" command to make the drive read its BAM.
    - Repeatedly issues the "M-W" command to write sixteen 32-byte blocks
      (total 512 bytes) from $8121 in C64 RAM into the drive’s RAM at $0500.
    - Each block is followed by a carriage return and CLOSE of the channel.
    - When done, sends "U3" to make the drive execute JMP $0500 and run the
      uploaded loader code.

 4. Serial re-initialization:
    - Reconfigures CIA #2 Port A for the custom high-speed communication mode.
    - Sets new input/output directions and output levels for handshake lines.
    - Performs a short software delay for signal stability.
    - Disables interrupts (SEI) before returning.

 In short: this code transfers a compact loader into the disk drive using the
 normal KERNAL routines, then switches both the C64 and drive into a faster,
 custom serial link that no longer depends on the KERNAL.
 
================================================================================
*/
#importonce
#import "constants.inc"
#import "registers.inc"

.label  group_counter           = $fa      // Countdown of 32-byte blocks remaining to send
.label  byte_counter            = $fb      // Index 0..31 within current 32-byte transfer
.label  destination             = $fc      // ZP ptr → drive RAM dest (hi at $fd)
.label  source                  = $fe      // ZP ptr → C64 source (hi at $ff)

.label  kernal_irq_vector       = $0314    // KERNAL IRQ vector (lo,hi)
.label  kernal_brk_vector       = $0316    // KERNAL BRK vector (lo,hi)
.label  kernal_nmi_vector       = $0318    // KERNAL NMI vector (lo,hi)

.label  CHROUT                  = $ffd2    // KERNAL: output A to current channel
.label  SETNAM                  = $ffbd    // KERNAL: set filename (A=len, name @FNADR)
.label  SETLFS                  = $ffba    // KERNAL: set LFN (A), device (X), secondary (Y)
.label  OPEN                    = $ffc0    // KERNAL: open file/channel using LFS+name
.label  CHKOUT                  = $ffc9    // KERNAL: set current output channel to X
.label  CLRCHN                  = $ffcc    // KERNAL: restore default I/O channels
.label  CLOSE                   = $ffc3    // KERNAL: close logical file in A

.const  CPU_PORT_MAP_IO_KERNAL_RAM_NBASIC = $06    // cpu_port: I/O+KERNAL on, BASIC off→RAM

.const  KERNAL_IRQ_VECTOR_ADDR            = $ea31  // Target handler for IRQ/BRK
.const  KERNAL_NMI_VECTOR_ADDR            = $fe47  // Target handler for NMI

.const  CIA2_IRQ_ENABLE_ALL               = $ff    // CIA2: enable all interrupt sources
.const  CIA2_DDRB_KEEP_BIT5_MASK          = $20    // CIA2 DDRB: preserve PB5, clear others

.const  CIA2_PRA_INIT_LEVELS              = $07    // CIA2 PRA init: clear ATN/CLOCK/DATA outs, set TXD
.const  CIA2_DDRA_DIR_MASK                = $3f    // CIA2 DDRA: bits 7..6 in, 5..0 out

.const  DRIVE_LOADER_SRC_ADDR             = $8121  // Host source start for upload
.const  DRIVE_LOADER_DEST_ADDR            = $0500  // Drive buffer #2 base address

.const  DRIVE_GROUP_COUNT_32B             = $10    // Number of 32-byte blocks to send (16)
.const  DRIVE_GROUP_SIZE_BYTES            = $20    // Bytes per block (32)

.const  ASCII_CR                          = $0d    // Carriage return

.const  DRIVE_DEVICE_ID                   = $08    // Default IEC device number
.const  CMD_CHANNEL_LFN                   = $0f    // Command channel logical file #15
.const  EMPTY_NAME_LEN                    = $00    // SETNAM: empty filename length

.const  CMD_U3_CHAR_U                     = $55    // 'U'
.const  CMD_U3_CHAR_3                     = $33    // '3'

.const  CMD_MEM_WRITE_CHAR_M              = $4d    // 'M'
.const  CMD_MEM_WRITE_CHAR_HYPHEN         = $2d    // '-'
.const  CMD_MEM_WRITE_CHAR_W              = $57    // 'W'

.const  CMD_INIT_CHAR_I                   = $49    // 'I'
.const  CMD_INIT_DRIVE_0                  = $30    // '0' (drive number)

.const  CIA2_PRA_KEEP_VIC_BANK            = $03    // Mask to preserve VIC bank bits 1..0
.const  CIA2_PRA_SET_ATN_TXD              = $0c    // Set ATN OUT and TXD outputs

.const  SERIAL_STABILIZE_OUTER            = $c0    // Outer delay loop count (X)
.const  SERIAL_STABILIZE_INNER            = $00    // Inner delay start (Y)

/*
================================================================================
  setup_vectors_and_drive_code
================================================================================
Summary
        Initialize CPU port mapping and interrupt vectors, configure CIA2 for
        IEC access, upload a 512-byte loader to the drive at $0500 using KERNAL
        I/O (M-W blocks), command the drive to execute it via U3, then switch
        CIA2 Port A to the game’s custom communication settings.

Description
        - CPU port: map I/O and KERNAL in, BASIC out; disable IRQs during vector
          updates.
        - Interrupt vectors: point IRQ and BRK to $EA31; NMI to $FE47.
        - CIA2 setup: enable interrupt masks; narrow DDRB to preserve PB5; set
          initial PRA/DDRA for command-channel transfers.
        - Drive init: open command channel 15 to device 8; issue "I0".
        - Upload: for 16 groups, emit "M-W <addr_lo><addr_hi><$20>", stream 32
          bytes from source, send <CR>, close channel; advance source/dest.
        - Execute: send "U3<CR>" to jump to $0500 in drive RAM.
        - Finalize: prepare CIA2 Port A for the custom high-speed link and jump
          to setup_serial_communication.

Notes
        - Writing $FF to CIA2 ICR sets mask bits; it does not clear pending IFR.
        - Initial PRA write sets VIC bank bits 1..0 to %11; later code preserves
          current bank selection.
        - Routine leaves interrupts disabled (SEI) until later re-enable.
================================================================================
*/
* = $7FFF
setup_vectors_and_drive_code:
        // ----------------------------------------------------------------------
        // Interrupt handler vectors setup
        // ----------------------------------------------------------------------

        // Set bits 2..0 output, 7..3 input. Allows changing memory map bits.
        lda     #CPU_PORT_DDR_INIT
        sta     cpu_port_ddr

        // Map I/O and KERNAL, map RAM instead of BASIC.
        lda     #CPU_PORT_MAP_IO_KERNAL_RAM_NBASIC
        sta     cpu_port

        // Disable interrupts while changing vectors.
        sei

        // Set IRQ handler to $EA31.
        lda     #<KERNAL_IRQ_VECTOR_ADDR
        sta     kernal_irq_vector
        lda     #>KERNAL_IRQ_VECTOR_ADDR
        sta     kernal_irq_vector + 1

        // Set BRK handler to $EA31.
        lda     #<KERNAL_IRQ_VECTOR_ADDR
        sta     kernal_brk_vector
        lda     #>KERNAL_IRQ_VECTOR_ADDR
        sta     kernal_brk_vector + 1

        // Set NMI handler to $FE47 (original NMI, skipping hardware NMI).
        lda     #<KERNAL_NMI_VECTOR_ADDR
        sta     kernal_nmi_vector
        lda     #>KERNAL_NMI_VECTOR_ADDR
        sta     kernal_nmi_vector + 1

		// Enables all five CIA2 interrupt sources (mask bits); does not clear pending IFR flags
        lda     #CIA2_IRQ_ENABLE_ALL
        sta     cia2_irq_status_reg

        // Clear all port B direction bits except bit5.
        // Preserves user port pin J direction. Sets C,D,E,F,H,K,L to input.
        lda     cia2_ddrb
        and     #CIA2_DDRB_KEEP_BIT5_MASK
        sta     cia2_ddrb

        // ----------------------------------------------------------------------
        // Drive code setup
        //
        // Uploads the loader to drive buffer #2 at $0500 in drive RAM as 16
        // groups of 32 bytes (total 512 bytes). After upload, instruct the
        // drive to execute the code, replacing the default ROM loop.
        // ----------------------------------------------------------------------

        // Port A: clear DATA IN/OUT, CLOCK IN/OUT, ATN OUT; set TXD; 
		// This sets VIC bank to #3 (bits 1..0 = %11 → $C000–$FFFF)
        lda     #CIA2_PRA_INIT_LEVELS                        // 0000 0111
        sta     cia2_pra

        // Set DATA IN and CLOCK IN (bits 7..6) as input. Others as output.
        lda     #CIA2_DDRA_DIR_MASK                        // 0011 1111
        sta     cia2_ddra

        // Issue drive initialize command.
        jsr     drive_initialize_cmd

        // Set source pointer = $8121.
        lda     #<DRIVE_LOADER_SRC_ADDR
        sta     source
        lda     #>DRIVE_LOADER_SRC_ADDR
        sta     source + 1

        // Set destination pointer = $0500 (drive buffer #2).
        lda     #<DRIVE_LOADER_DEST_ADDR
        sta     destination
        lda     #>DRIVE_LOADER_DEST_ADDR
        sta     destination + 1

        // Write 16 groups of 32 bytes each.
        ldx     #DRIVE_GROUP_COUNT_32B
copy_next_group_to_drive:
        stx     group_counter
        jsr     drive_issue_write_memory_32       // returns with y := 0

drive_write_loop:
        sty     byte_counter

        // Load source byte.
        lda     (source),y

        // Write byte to serial.
        jsr     CHROUT

        // Increment byte counter.
        ldy     byte_counter
        iny

        // Group complete after 32 bytes?
        cpy     #DRIVE_GROUP_SIZE_BYTES
        bne     drive_write_loop

        // Send <CR> to finish the command string.
        lda     #ASCII_CR
        jsr     CHROUT

        // Close the drive channel.
        jsr     drive_close_channel

        // Advance pointers by 32 bytes.
        jsr     advance_io_ptrs_by_group

        // More groups pending?
        ldx     group_counter
        dex
        bne     copy_next_group_to_drive

        // No groups left: instruct drive to execute uploaded code.
        jsr     drive_issue_u3_command

        // Prepare serial port for custom communication.
        jmp     setup_serial_communication
/*
================================================================================
  drive_open_command_channel
================================================================================
Summary
        Open the drive command channel using KERNAL I/O routines and select it
        as the current output channel.

Returns
        Output channel set to command channel (LFN 15). Carry/flags per KERNAL.

Description
        - Call SETNAM with an empty filename.
        - Call SETLFS with logical file 15, device 8, secondary address 15.
        - Call OPEN to create the command channel.
        - Call CHKOUT with X := 15 to select the channel for output.

Notes
        Assumes IEC device number 8. 
================================================================================
*/
* = $807B		
drive_open_command_channel:
        // Set filename to empty
        lda     #EMPTY_NAME_LEN
        jsr     SETNAM

        // Set LFS: logical=15, device=8, secondary=15
        lda     #CMD_CHANNEL_LFN
        ldx     #DRIVE_DEVICE_ID
        tay
        jsr     SETLFS

        // Open the logical file
        jsr     OPEN

        // Select channel 15 for output
        ldx     #CMD_CHANNEL_LFN
        jmp     CHKOUT
/*
================================================================================
  drive_issue_u3_command
================================================================================
Summary
        Open the drive command channel, send the "U3" command, terminate with
        <CR>, and close the channel. This instructs the drive to JMP $0500
        (buffer #2), starting the previously uploaded loader.

Description
        - Opens command channel 15 on device 8 using KERNAL I/O.
        - Transmits ASCII 'U', then '3', then a carriage return.
        - Closes the command channel to finalize the command.
        - Drive ROM interprets "U3" as a jump to $0500.

Notes
        Assumes the loader was written to $0500 beforehand (e.g., via "M-W").
        Device number is fixed to 8 in the channel-open helper.
================================================================================
*/
* = $8090		
drive_issue_u3_command:
        jsr     drive_open_command_channel    // open command channel 15
        lda     #CMD_U3_CHAR_U                // 'U'
        jsr     CHROUT
        lda     #CMD_U3_CHAR_3                // '3'
        jsr     CHROUT
        lda     #ASCII_CR                     // <CR>
        jsr     CHROUT
        jmp     drive_close_channel           // close channel 15

/*
================================================================================
  drive_issue_write_memory_32
================================================================================
Summary
        Open the drive command channel and emit an IEC "M-W" command header to
        write a fixed-size block to drive RAM. Leaves the channel open so the
        caller can stream the 32 data bytes and terminate with <CR>.

Arguments
        destination  ZP pointer ($FC/$FD) to target address in the drive's
                     address space; low byte sent first, then high.

Returns
        Y := $00 for the caller's byte-streaming loop.

Description
        - Opens command channel 15 on device 8 using KERNAL I/O.
        - Sends ASCII 'M','-','W' to select the "write memory" command.
        - Sends destination address bytes in order: <addr_lo>, <addr_hi>.
        - Sends the byte count (#$20 = 32).
        - Returns immediately so the caller can:
              • transmit exactly 32 raw data bytes via CHROUT
              • send a final <CR>
              • close the channel (via drive_close_channel)

Notes
        - Uses the fixed block size constant DRIVE_GROUP_SIZE_BYTES (= $20).
        - Device number is assumed to be 8
================================================================================
*/
* = $80A5
drive_issue_write_memory_32:
        jsr     drive_open_command_channel   // open command channel 15

        // Send "M-W" command header
        lda     #CMD_MEM_WRITE_CHAR_M        // 'M'
        jsr     CHROUT
        lda     #CMD_MEM_WRITE_CHAR_HYPHEN   // '-'
        jsr     CHROUT
        lda     #CMD_MEM_WRITE_CHAR_W        // 'W'
        jsr     CHROUT

        // Send destination address (lo, hi)
        lda     destination
        jsr     CHROUT
        lda     destination + 1
        jsr     CHROUT

        // Send byte count (32)
        lda     #DRIVE_GROUP_SIZE_BYTES
        jsr     CHROUT

        // Return with Y := 0 for caller’s streaming loop
        ldy     #$00
        rts
/*
================================================================================
  drive_initialize_cmd
================================================================================
Summary
        Open the drive command channel (LFN 15, device 8), transmit the
        "I0" initialize command followed by <CR>, then close the channel.
Description
        - Opens command channel 15 on device 8 using KERNAL I/O.
        - Sends ASCII 'I' then '0' to request a drive initialization.
        - Sends a carriage return to terminate the command string.
        - Closes the command channel before returning.

Notes
        Relies on the current device number being 8. 
================================================================================
*/
* = $80C9		
drive_initialize_cmd:
        jsr     drive_open_command_channel   // open command channel 15
        lda     #CMD_INIT_CHAR_I             // 'I'
        jsr     CHROUT
        lda     #CMD_INIT_DRIVE_0            // '0' (drive 0)
        jsr     CHROUT
        lda     #ASCII_CR                    // <CR>
        jsr     CHROUT
        jmp     drive_close_channel          // close channel 15
/*
================================================================================
  advance_io_ptrs_by_group
================================================================================
Summary
        Advance the host source pointer and the drive destination pointer by
        one fixed block of 32 bytes, propagating carry to the high byte.

Arguments
        source (ZP)        Low/high bytes at $FE/$FF. Host-side read pointer.
        destination (ZP)   Low/high bytes at $FC/$FD. Drive-side write pointer.

Description
        - Adds 32 to source.lo; adds carry into source.hi.
        - Adds 32 to destination.lo; adds carry into destination.hi.
        - Uses the fixed group size constant (32) to step to the next block.
================================================================================
*/
* = $80DE
advance_io_ptrs_by_group:
        lda     source                   
        clc                               
        adc     #DRIVE_GROUP_SIZE_BYTES   // +32
        sta     source                   
		
        lda     source + 1
        adc     #$00                      
        sta     source + 1

        lda     destination              
        clc
        adc     #DRIVE_GROUP_SIZE_BYTES   // +32
        sta     destination              
		
        lda     destination + 1
        adc     #$00                      
        sta     destination + 1
        rts
/*
================================================================================
  drive_close_channel
================================================================================
Summary
        Reset active I/O channels to defaults, then close logical file 15
        (the drive command channel).

Description
        - Call CLRCHN to restore default input/output channels.
        - Load A with the command channel LFN (#$0F).
        - Tail-call CLOSE to close LFN #$0F.
================================================================================
*/
* = $80F9		
 drive_close_channel:
        jsr     CLRCHN				// restore default I/O channels
        lda     #CMD_CHANNEL_LFN    // LFN = 15 (command channel)
        jmp     CLOSE               // CLOSE 15
/*
================================================================================
  setup_serial_communication
================================================================================
Summary
        Configure CIA2 Port A direction and output levels for the custom
        serial protocol with the disk drive. Preserve VIC bank select bits.

Description
        - Set DDRA: bits 7..6 as input (DATA IN, CLOCK IN); bits 5..2 as
          output (DATA OUT, CLOCK OUT, ATN OUT, TXD); keep bits 1..0
          unchanged (VIC bank).
        - Read-modify-write PRA: preserve bits 1..0; set ATN OUT and TXD
          high. DATA OUT and CLOCK OUT levels are not forced here.
        - Run a short software delay to stabilize line levels.
        - Disable maskable interrupts with SEI before returning.

Notes
        Leaves SEI set on exit. 
================================================================================
*/
* = $8101		
setup_serial_communication:
        // ------------------------------------------------------------
        // CIA2 Port A data direction:
        // - DATA IN (bit7) and CLOCK IN (bit6) as input
        // - ATN OUT (bit3), CLOCK OUT (bit4), DATA OUT (bit5), TXD (bit2) as output
        // ------------------------------------------------------------
        lda     cia2_ddra                     // read current DDR
        and     #$03                          // clear bits 7..2, keep VIC bank
        ora     #$3c                          // set bits 5..2 as outputs
        sta     cia2_ddra

		// Initialize Port A output levels:
		// - Inputs (DATA IN/CLOCK IN) are unaffected by PRA writes
		// - Set ATN OUT and TXD high; DATA OUT and CLOCK OUT are left unchanged here
		// - Preserve VIC bank bits (1..0)
        lda     cia2_pra
        and     #CIA2_PRA_KEEP_VIC_BANK       // keep VIC bank bits
        ora     #CIA2_PRA_SET_ATN_TXD         // set ATN OUT, TXD
        sta     cia2_pra

        // ------------------------------------------------------------
        // Short busy-wait to stabilize lines
        // ------------------------------------------------------------
        ldx     #SERIAL_STABILIZE_OUTER
        ldy     #SERIAL_STABILIZE_INNER
wait_loop:
        dey
        bne     wait_loop
        dex
        bne     wait_loop

        // ------------------------------------------------------------
        // Disable interrupts and return
        // ------------------------------------------------------------
        sei
        rts


/*
Pseudo-code

function setup_vectors_and_drive_code()
    // Configure CPU port data direction so lower bits (memory map) are outputs
    cpu_port_ddr := CPU_PORT_DDR_INIT

    // Map I/O and KERNAL ROM in, BASIC out (replaced by RAM)
    cpu_port := CPU_PORT_MAP_IO_KERNAL_RAM_NBASIC

    // Disable interrupts while vectors are being changed
    disable_interrupts()

    // Set IRQ and BRK vectors to KERNAL_IRQ_VECTOR_ADDR ($EA31)
    kernal_irq_vector   := low_byte(KERNAL_IRQ_VECTOR_ADDR)
    kernal_irq_vector+1 := high_byte(KERNAL_IRQ_VECTOR_ADDR)
    kernal_brk_vector   := low_byte(KERNAL_IRQ_VECTOR_ADDR)
    kernal_brk_vector+1 := high_byte(KERNAL_IRQ_VECTOR_ADDR)

    // Set NMI vector to KERNAL_NMI_VECTOR_ADDR ($FE47)
    kernal_nmi_vector   := low_byte(KERNAL_NMI_VECTOR_ADDR)
    kernal_nmi_vector+1 := high_byte(KERNAL_NMI_VECTOR_ADDR)

    // ----- CIA2 initial setup for serial -----
    // Enable all CIA2 interrupt sources via mask register
    cia2_irq_status_reg := CIA2_IRQ_ENABLE_ALL

    // Preserve PB5 direction; force all other port B bits to input
    cia2_ddrb := cia2_ddrb AND CIA2_DDRB_KEEP_BIT5_MASK

    // Set CIA2 PRA to initial IEC levels:
    //  - ATN / CLOCK / DATA outputs cleared
    //  - TXD set
    //  - VIC bank forced to bank 3 via low bits
    cia2_pra := CIA2_PRA_INIT_LEVELS

    // Configure CIA2 DDRA so:
    //  - DATA IN / CLOCK IN bits are inputs
    //  - remaining bits are outputs
    cia2_ddra := CIA2_DDRA_DIR_MASK

    // ----- Drive initialize and loader upload -----
    // Ask the drive to initialize (I0)
    drive_initialize_cmd()

    // Set host source pointer to start of loader in C64 RAM
    source := DRIVE_LOADER_SRC_ADDR

    // Set drive destination pointer to buffer #2 ($0500) in drive RAM
    destination := DRIVE_LOADER_DEST_ADDR

    // Number of 32-byte groups to send (512 bytes total)
    X := DRIVE_GROUP_COUNT_32B   // e.g., 16 groups

    // Loop over groups of 32 bytes
    while X > 0 do
        // Snapshot remaining-group count so it survives inner operations
        group_counter := X

        // Emit "M-W dest.lo dest.hi 32" header for this destination
        drive_issue_write_memory_32()

        // Y will have been set to 0 by drive_issue_write_memory_32
        // Stream 32 data bytes from host to drive via CHROUT
        Y := 0
        while Y < DRIVE_GROUP_SIZE_BYTES do
            A := read_byte_at(source + Y)
            CHROUT(A)                      // send byte over KERNAL channel
            Y := Y + 1
        end while

        // Terminate the M-W command line with carriage return
        CHROUT(ASCII_CR)

        // Close command channel 15
        drive_close_channel()

        // Advance source and destination pointers by 32 bytes
        advance_io_ptrs_by_group()

        // Decrement group counter and loop if more blocks remain
        X := group_counter
        X := X - 1
    end while

    // ----- Command drive to execute loader and switch to custom link -----
    // Instruct drive to jump to $0500 (U3 command)
    drive_issue_u3_command()

    // Reconfigure CIA2 to the game’s custom serial protocol and delay briefly
    setup_serial_communication()

    // Leave interrupts disabled; higher-level code decides when to re-enable
end function


function drive_open_command_channel()
    // Prepare an empty filename
    SETNAM(name_length = EMPTY_NAME_LEN, name_ptr = null_string)

    // Logical file 15, device 8, secondary address 15
    SETLFS(logical_number = CMD_CHANNEL_LFN,
           device_number  = DRIVE_DEVICE_ID,
           secondary_addr = CMD_CHANNEL_LFN)

    // Open the command channel on the drive
    OPEN()

    // Make logical file 15 the current output channel
    CHKOUT(CMD_CHANNEL_LFN)
end function


function drive_issue_u3_command()
    // Open drive command channel 15
    drive_open_command_channel()

    // Send "U3" sequence to command execution of buffer #2 at $0500
    CHROUT(CMD_U3_CHAR_U)      // 'U'
    CHROUT(CMD_U3_CHAR_3)      // '3'

    // Terminate command with carriage return
    CHROUT(ASCII_CR)

    // Close the command channel and restore default I/O
    drive_close_channel()
end function


function drive_issue_write_memory_32()
    // Open drive command channel 15
    drive_open_command_channel()

    // Send "M-W" to indicate a memory-write command
    CHROUT(CMD_MEM_WRITE_CHAR_M)       // 'M'
    CHROUT(CMD_MEM_WRITE_CHAR_HYPHEN)  // '-'
    CHROUT(CMD_MEM_WRITE_CHAR_W)       // 'W'

    // Send destination address in drive RAM (low, then high byte)
    CHROUT(low_byte(destination))
    CHROUT(high_byte(destination))

    // Send the number of bytes to write (fixed at 32 here)
    CHROUT(DRIVE_GROUP_SIZE_BYTES)

    // Initialize Y so caller can use (source + Y) loop
    Y := 0

    // Channel remains open; caller will:
    //   - write 32 data bytes via CHROUT
    //   - send <CR>
    //   - call drive_close_channel()
    return
end function


function drive_initialize_cmd()
    // Open drive command channel 15
    drive_open_command_channel()

    // Send "I0" to reinitialize the drive
    CHROUT('I')
    CHROUT('0')

    // Terminate command with carriage return
    CHROUT(ASCII_CR)

    // Close command channel 15 and restore default I/O
    drive_close_channel()
end function


function advance_io_ptrs_by_group()
    // Advance host source pointer by one 32-byte group
    source := source + DRIVE_GROUP_SIZE_BYTES

    // Advance drive destination pointer by one 32-byte group
    destination := destination + DRIVE_GROUP_SIZE_BYTES
end function


function drive_close_channel()
    // Restore default input/output devices
    CLRCHN()

    // Close logical file 15 (command channel)
    CLOSE(CMD_CHANNEL_LFN)
end function


function setup_serial_communication()
    // Reconfigure CIA2 Port A for custom serial link while preserving VIC bank
    // --- Configure CIA2 DDRA (data direction) ---
    current_ddra := cia2_ddra

    // Clear bits 7..2 (DATA/CLOCK/ATN/TXD directions), preserve bits 1..0 (VIC bank)
    current_ddra := current_ddra AND %00000011

    // Set bits 5..2 as outputs (DATA OUT, CLOCK OUT, ATN OUT, TXD)
    current_ddra := current_ddra OR %00111100

    cia2_ddra := current_ddra

    // --- Configure CIA2 PRA (output levels) ---
    current_pra := cia2_pra

    // Preserve VIC bank selection in bits 1..0
    current_pra := current_pra AND CIA2_PRA_KEEP_VIC_BANK

    // Force ATN and TXD high; leave DATA/CLOCK outputs untouched
    current_pra := current_pra OR CIA2_PRA_SET_ATN_TXD

    cia2_pra := current_pra

    // --- Short stabilization delay for the serial lines ---
    X := SERIAL_STABILIZE_OUTER
    while X > 0 do
        Y := SERIAL_STABILIZE_INNER
        while Y > 0 do
            Y := Y - 1       // inner busy-wait
        end while
        X := X - 1           // outer loop decrements
    end while

    // Ensure interrupts remain disabled on exit
    disable_interrupts()
end function
*/