# Instruction Set Reference

The Z33 has **33 instructions** organized into seven categories.

## Instruction Entry Format

Each instruction entry documents:

- **Syntax** — mnemonic and operand types
- **Operation** — pseudocode description of the semantics
- **Flags** — which status register flags are affected
- **Cycles** — execution cost formula
- **Privileged** — whether the instruction requires supervisor mode
- **Exceptions** — conditions that may trigger an exception

## Quick Reference Table

| Mnemonic | Operands | Category | Description |
|---|---|---|---|
| `add` | *imm/reg/dir/ind/idx*, *reg* | Arithmetic | Add source to register |
| `sub` | *imm/reg/dir/ind/idx*, *reg* | Arithmetic | Subtract source from register |
| `mul` | *imm/reg/dir/ind/idx*, *reg* | Arithmetic | Multiply register by source |
| `div` | *imm/reg/dir/ind/idx*, *reg* | Arithmetic | Divide register by source |
| `neg` | *reg* | Arithmetic | Negate register |
| `and` | *imm/reg/dir/ind/idx*, *reg* | Bitwise | Bitwise AND |
| `or` | *imm/reg/dir/ind/idx*, *reg* | Bitwise | Bitwise OR |
| `xor` | *imm/reg/dir/ind/idx*, *reg* | Bitwise | Bitwise XOR |
| `not` | *reg* | Bitwise | Bitwise NOT |
| `shl` | *imm/reg/dir/ind/idx*, *reg* | Bitwise | Shift left |
| `shr` | *imm/reg/dir/ind/idx*, *reg* | Bitwise | Shift right |
| `cmp` | *imm/reg/dir/ind/idx*, *reg* | Comparison | Compare source to register |
| `jmp` | *imm/reg/dir/ind/idx* | Branch | Unconditional jump |
| `jeq` | *imm/reg/dir/ind/idx* | Branch | Jump if equal |
| `jne` | *imm/reg/dir/ind/idx* | Branch | Jump if not equal |
| `jlt` | *imm/reg/dir/ind/idx* | Branch | Jump if less than |
| `jle` | *imm/reg/dir/ind/idx* | Branch | Jump if less or equal |
| `jgt` | *imm/reg/dir/ind/idx* | Branch | Jump if greater than |
| `jge` | *imm/reg/dir/ind/idx* | Branch | Jump if greater or equal |
| `ld` | *imm/reg/dir/ind/idx*, *reg* | Data Movement | Load value into register |
| `st` | *reg*, *dir/ind/idx* | Data Movement | Store register to memory |
| `swap` | *reg/dir/ind/idx*, *reg* | Data Movement | Swap two values |
| `push` | *imm/reg* | Data Movement | Push value onto stack |
| `pop` | *reg* | Data Movement | Pop value from stack |
| `call` | *imm/reg/dir/ind/idx* | Control Flow | Call subroutine |
| `rtn` | — | Control Flow | Return from subroutine |
| `trap` | — | Control Flow | Trigger trap exception |
| `rti` | — | Control Flow | Return from interrupt (privileged) |
| `reset` | — | Control Flow | Halt the processor |
| `nop` | — | Control Flow | No operation |
| `fas` | *dir/ind/idx*, *reg* | Synchronization | Fetch-and-set |
| `in` | *dir/ind/idx*, *reg* | I/O | Read from I/O port (privileged) |
| `out` | *imm/reg*, *dir/ind/idx* | I/O | Write to I/O port (privileged) |

## Cycle Cost

Every instruction costs **1 base cycle** plus the cost of its addressing modes:

| Addressing Mode | Cost |
|---|---|
| Immediate (*imm*) | 0 cycles |
| Register (*reg*) | 0 cycles |
| Direct (*dir*) | 1 cycle |
| Indirect (*ind*) | 1 cycle |
| Indexed (*idx*) | 1 cycle |

For example, `add [%a+5], %b` costs 1 (base) + 1 (indexed source) + 0 (register dest) = **2 cycles**.
