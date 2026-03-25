# Exceptions and Interrupts

## Overview

The Z33 supports six types of events that can interrupt normal execution. These are divided into:

- **Hardware interrupts:** Asynchronous events from external devices
- **Exceptions:** Synchronous events caused by the current instruction
- **Traps:** Deliberate software-triggered exceptions

All events are handled through the same mechanism.

## Exception Codes

r[exc.code.hardware-interrupt]
Code 0 — Hardware interrupt: External device signal.

r[exc.code.division-by-zero]
Code 1 — Division by zero: `div` with divisor = 0.

r[exc.code.invalid-instruction]
Code 2 — Invalid instruction: Attempt to execute a non-instruction cell, or invalid shift amount.

r[exc.code.privileged-instruction]
Code 3 — Privileged instruction: Privileged operation in user mode.

r[exc.code.trap]
Code 4 — Trap: `trap` instruction executed.

r[exc.code.invalid-memory-access]
Code 5 — Invalid memory access: Access to an address outside 0–9,999.

## Exception Handling Sequence

When an exception or interrupt occurs, the processor performs the following steps:

r[exc.handling.check-handler]
1. **Check for handler:** Verify that address 200 contains an Instruction. If it does not, the exception is fatal (cannot be recovered).

r[exc.handling.save-state]
2. **Save state:** `memory[100] ← %pc`, `memory[101] ← %sr`, `memory[102] ← exception_code`.

r[exc.handling.enter-supervisor]
5. **Enter supervisor mode:** Set the S bit in `%sr` to 1.

r[exc.handling.disable-interrupts]
6. **Disable interrupts (for hardware interrupts only):** If the event is a hardware interrupt (code 0), set IE to 0. For all other events (exceptions and traps), IE is **left unchanged**.

r[exc.handling.jump]
7. **Jump to handler:** `%pc ← 200`.

After these steps, execution continues at address 200 in supervisor mode.

## Return from Exception

r[exc.rti]
The `rti` (return from interrupt) instruction reverses the exception entry sequence: `%pc ← memory[100]`, `%sr ← memory[101]`. Since the status register is fully restored, `rti` also restores the previous privilege level and interrupt enable state.

## Handler Behavior

The exception handler at address 200 is responsible for:

1. Examining the exception code at address 102 to determine the cause
2. Taking appropriate action (e.g., terminating a process, servicing an I/O request, implementing a system call)
3. Optionally modifying the saved PC at address 100 (e.g., to skip the faulting instruction or to context-switch)
4. Returning with `rti`

### System Call Convention

The `trap` instruction is used for system calls. By convention:
- The system call number is placed in `%a` before executing `trap`.
- Arguments are pushed onto the stack.
- The return value is placed in `%a` by the handler.

This convention is not enforced by the hardware — it is a software agreement between user programs and the operating system.

## Limitations

- There is only **one exception vector** (address 200). All exceptions and interrupts go to the same handler, which must dispatch based on the code at address 102.
- There is only **one save area** (addresses 100–102). Nested exceptions will overwrite the saved state unless the handler explicitly preserves it.
- If no handler is installed at address 200 (the cell does not contain an Instruction), the exception is **unrecoverable** and the processor halts with an error.
