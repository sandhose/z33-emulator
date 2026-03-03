# Exceptions and Interrupts

## Overview

The Z33 supports six types of events that can interrupt normal execution. These are divided into:

- **Hardware interrupts:** Asynchronous events from external devices
- **Exceptions:** Synchronous events caused by the current instruction
- **Traps:** Deliberate software-triggered exceptions

All events are handled through the same mechanism.

## Exception Codes

| Code | Name | Type | Cause |
|---|---|---|---|
| 0 | Hardware interrupt | Interrupt | External device signal |
| 1 | Division by zero | Exception | `div` with divisor = 0 |
| 2 | Invalid instruction | Exception | Attempt to execute a non-instruction cell, or invalid shift amount |
| 3 | Privileged instruction | Exception | Privileged operation in user mode |
| 4 | Trap | Trap | `trap` instruction executed |
| 5 | Invalid memory access | Exception | Access to an address outside 0–9,999 |

## Exception Handling Sequence

When an exception or interrupt occurs, the processor performs the following steps:

1. **Check for handler:** Verify that address 200 contains an Instruction. If it does not, the exception is fatal (cannot be recovered).
2. **Save PC:** `memory[100] ← %pc`
3. **Save SR:** `memory[101] ← %sr`
4. **Save exception code:** `memory[102] ← exception_code`
5. **Enter supervisor mode:** Set the S bit in `%sr` to 1.
6. **Disable interrupts (for hardware interrupts only):** If the event is a hardware interrupt (code 0), set IE to 0. For all other events (exceptions and traps), IE is **left unchanged**.
7. **Jump to handler:** `%pc ← 200`

After these steps, execution continues at address 200 in supervisor mode.

## Return from Exception

The `rti` (return from interrupt) instruction reverses the exception entry sequence:

1. `%pc ← memory[100]` — restore the saved program counter
2. `%sr ← memory[101]` — restore the saved status register (including privilege level and interrupt enable state)

Since the status register is fully restored, `rti` also restores the previous privilege level and interrupt enable state.

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
