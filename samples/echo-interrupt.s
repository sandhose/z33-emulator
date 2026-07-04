// Interrupt-driven serial echo.
//
// Instead of busy-polling, this program arms the serial console to raise a
// hardware interrupt whenever a byte arrives, then idles. The handler at
// address 200 echoes each received byte. Sending EOT (Ctrl-D, byte 4) ends the
// program.
//
// Run with:   z33-cli run samples/echo-interrupt.s main

#define CONTROL 110     // write: control register (E bit enables rx interrupts)
#define DATA    111     // read: rx data, write: tx data
#define RX_IRQ_ENABLE 1 // control bit 0: interrupt when a byte is received
#define EOT     4       // Ctrl-D: end of transmission

#define EXCEPTION 102   // where the CPU stores the exception code
#define HARDWARE_INTERRUPT 0    // exception code for a device interrupt

// %sr layout: supervisor is bit 9 (512), interrupt-enable is bit 8 (256).
#define SR_SUPERVISOR_IE 768

.addr 200
handler:
    push %a
    ld  [EXCEPTION], %a         // why were we interrupted?
    cmp HARDWARE_INTERRUPT, %a
    jne return                  // not a device interrupt: ignore it

    in  [DATA], %a              // read the received byte
    cmp EOT, %a                 // Ctrl-D ends the session
    jeq halt
    out %a, [DATA]              // echo it back

return:
    pop %a
    rti

halt:
    reset

.addr 1000
main:
    out RX_IRQ_ENABLE, [CONTROL]    // raise an interrupt on every byte received
    ld  SR_SUPERVISOR_IE, %sr       // stay supervisor, enable interrupts
idle:
    jmp idle                        // do nothing until interrupted
