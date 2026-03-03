# Registers

The Z33 has five programmer-visible registers.

## General Purpose Registers

| Register | Description |
|---|---|
| `%a` | General purpose register. Holds a Word or is Empty. |
| `%b` | General purpose register. Holds a Word or is Empty. |

General purpose registers can hold any cell value (Word, Instruction, or Empty). When an arithmetic or logical operation reads from a register, it extracts the Word value; reading from an Empty register yields 0. Attempting to extract a Word from an Instruction cell raises an error.

## Special Registers

| Register | Type | Description |
|---|---|---|
| `%pc` | Address | Program counter. Points to the next instruction to fetch. |
| `%sp` | Address | Stack pointer. Points to the top of the stack. |
| `%sr` | Word | Status register. Contains flags and mode bits. |

Writing a value to `%pc` or `%sp` that cannot be represented as a valid Address (u32) is an error. Writing to `%sr` is a **privileged operation** — it raises a *privileged instruction* exception if executed in user mode.

## Status Register (`%sr`)

The status register is a bitmask with the following fields:

```
Bit:  ...  9    8   ...  3    2    1    0
       ... S    IE  ...  O    N    Z    C
```

### Condition Flags (bits 0–3)

These flags are readable in any mode.

| Bit | Name | Symbol | Description |
|---|---|---|---|
| 0 | Carry | C | **Reserved.** Currently unused by any instruction. |
| 1 | Zero | Z | Set when the result of an arithmetic, bitwise, or comparison instruction is zero. |
| 2 | Negative | N | Set when the result of an arithmetic, bitwise, or comparison instruction is negative. |
| 3 | Overflow | O | Set by `add`, `sub`, `mul`, `neg` on arithmetic overflow. |

### Privileged Flags (bits 8–9)

These flags are writable only in supervisor mode.

| Bit | Name | Symbol | Description |
|---|---|---|---|
| 8 | Interrupt Enable | IE | 1 = hardware interrupts are enabled. |
| 9 | Supervisor | S | 1 = supervisor mode, 0 = user mode. |

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
