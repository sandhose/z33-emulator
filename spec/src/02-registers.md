# Registers

The Z33 has five programmer-visible registers.

## General Purpose Registers

r[reg.general-purpose]
General purpose registers `%a` and `%b` can hold any cell value (Word, Instruction, or Empty). When an arithmetic or logical operation reads from a register, it extracts the Word value; reading from an Empty register yields 0. Attempting to extract a Word from an Instruction cell raises an error.

| Register | Description |
|---|---|
| `%a` | General purpose register. Holds a Word or is Empty. |
| `%b` | General purpose register. Holds a Word or is Empty. |

## Special Registers

r[reg.pc]
`%pc` is the program counter, an Address (u32) register that points to the next instruction to fetch.

r[reg.sp]
`%sp` is the stack pointer, an Address (u32) register that points to the top of the stack.

r[reg.sr]
`%sr` is the status register, a Word register containing flags and mode bits.

Writing a value to `%pc` or `%sp` that cannot be represented as a valid Address (u32) is an error.

r[reg.sr.privileged-write]
Writing to `%sr` is a **privileged operation** — it raises a *privileged instruction* exception if executed in user mode.

## Status Register (`%sr`)

The status register is a bitmask with the following fields:

```
Bit:  ...  9    8   ...  3    2    1    0
       ... S    IE  ...  O    N    Z    C
```

### Condition Flags (bits 0–3)

These flags are readable in any mode.

r[reg.flags.carry]
Bit 0 — Carry (C): **Reserved.** Currently unused by any instruction.

r[reg.flags.zero]
Bit 1 — Zero (Z): Set when the result of an arithmetic, bitwise, or comparison instruction is zero.

r[reg.flags.negative]
Bit 2 — Negative (N): Set when the result of an arithmetic, bitwise, or comparison instruction is negative.

r[reg.flags.overflow]
Bit 3 — Overflow (O): Set by `add`, `sub`, `mul`, `neg` on arithmetic overflow.

### Privileged Flags (bits 8–9)

These flags are writable only in supervisor mode.

r[reg.flags.interrupt-enable]
Bit 8 — Interrupt Enable (IE): 1 = hardware interrupts are enabled.

r[reg.flags.supervisor]
Bit 9 — Supervisor (S): 1 = supervisor mode, 0 = user mode.

### Flag Behavior by Instruction

| Instruction | Flags Modified |
|---|---|
| `add` | O, Z, N |
| `sub` | O, Z, N |
| `mul` | O, Z, N |
| `div` | Z, N |
| `neg` | O, Z, N |
| `and` | Z, N |
| `or` | Z, N |
| `xor` | Z, N |
| `not` | Z, N |
| `shl` | Z, N |
| `shr` | Z, N |
| `cmp` | Z, N |
| All others | None |
