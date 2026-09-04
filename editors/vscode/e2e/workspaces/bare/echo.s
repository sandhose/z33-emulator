// Polling serial echo.
//
// Busy-waits on the serial console (ports 110-111), echoing every byte it
// receives back to the host. Sending EOT (Ctrl-D, byte 4) ends the program.
//
// Run with:   z33-cli run samples/echo.s main

#define STATUS 110      // read: status register, write: control register
#define DATA   111      // read: rx data,        write: tx data
#define RX_READY 1      // status bit 0: a byte is waiting to be read
#define EOT      4       // Ctrl-D: end of transmission

.addr 1000
main:
poll:
    in  [STATUS], %a    // read the status register
    and RX_READY, %a    // isolate the R bit
    jeq poll            // nothing received yet: keep polling

    in  [DATA], %a      // pop the received byte
    cmp EOT, %a         // was it Ctrl-D?
    jeq done            // yes: stop
    out %a, [DATA]      // no: echo it back
    jmp poll

done:
    reset
