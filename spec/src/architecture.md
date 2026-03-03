# Architecture Overview

## Data Types

The Z33 defines two fundamental data types:

| Type | Width | Representation | Usage |
|---|---|---|---|
| **Word** | 64 bits | Signed two's complement (`i64`) | Data values, immediate operands |
| **Address** | 32 bits | Unsigned (`u32`) | Memory addresses, program counter |

There is no byte-level addressing. The smallest addressable unit is one memory cell.

## Memory Model

Memory consists of **10,000 cells**, addressed from 0 to 9,999. Each cell can hold one of:

- A **Word** (64-bit signed integer)
- An **Instruction** (opaque, not encoded as bits)
- **Empty** (default state, reads as 0 when interpreted as a Word)

There is no distinction between code and data memory at the hardware level — any cell can hold either an instruction or a data word. However, attempting to execute a cell that contains a Word (or is empty) raises an *invalid instruction* exception.

## Special Addresses

Several memory addresses have architectural significance:

| Address | Name | Purpose |
|---|---|---|
| 100 | `INTERRUPT_PC_SAVE` | Saved `%pc` on exception/interrupt |
| 101 | `INTERRUPT_SR_SAVE` | Saved `%sr` on exception/interrupt |
| 102 | `INTERRUPT_EXCEPTION` | Exception code on exception/interrupt |
| 200 | `INTERRUPT_HANDLER` | Entry point of the exception/interrupt handler |
| 1000 | `PROGRAM_START` | Default program entry point |

## Stack

The stack grows **downward** from the top of memory:

- The initial stack pointer value is **10,000** (one past the last valid address).
- `push` **decrements** `%sp` first, then writes to `[%sp]`.
- `pop` reads from `[%sp]`, then **increments** `%sp`.
- `%sp` always points to the **top of stack** (the last occupied cell).

> **Note:** Since valid addresses are 0–9,999 and `%sp` starts at 10,000, the stack pointer initially points outside the addressable memory. The first `push` decrements it to 9,999 before writing.

## Initial State

On startup (or after a `reset`), the processor state is:

| Register | Initial Value |
|---|---|
| `%a` | 0 (Empty) |
| `%b` | 0 (Empty) |
| `%pc` | 1000 |
| `%sp` | 10000 |
| `%sr` | S=1 (supervisor mode), all other bits 0 |
