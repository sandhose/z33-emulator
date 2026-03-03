# Addressing Modes

The Z33 supports five addressing modes. Not all modes are available for every instruction — each instruction's entry in the [Instruction Set Reference](./instructions/README.md) specifies which modes are permitted.

## Summary

| Mode | Syntax | Value Produced | Cycle Cost |
|---|---|---|---|
| Immediate | `42` | The literal value itself | 0 |
| Register | `%a` | Contents of the register | 0 |
| Direct | `[100]` | Contents of memory at the given address | 1 |
| Indirect | `[%a]` | Contents of memory at the address held in the register | 1 |
| Indexed | `[%a+5]` | Contents of memory at (register value + offset) | 1 |

## Immediate (*imm*)

The operand is a literal value embedded in the instruction.

- **Syntax:** a numeric constant or expression (e.g., `42`, `0xFF`, `label`)
- **Value:** the constant itself
- **Cycle cost:** 0

Example: `ld 100, %a` — loads the value 100 into `%a`.

## Register (*reg*)

The operand is the contents of a register.

- **Syntax:** `%a`, `%b`, `%pc`, `%sp`, `%sr`
- **Value:** current contents of the named register
- **Cycle cost:** 0

Example: `ld %b, %a` — loads the value of `%b` into `%a`.

## Direct (*dir*)

The operand is the contents of memory at a fixed address.

- **Syntax:** `[addr]` where *addr* is a numeric constant or expression (e.g., `[100]`, `[label]`)
- **Value:** contents of the memory cell at the given address
- **Cycle cost:** 1

Example: `ld [100], %a` — loads the contents of memory cell 100 into `%a`.

## Indirect (*ind*)

The operand is the contents of memory at the address held in a register.

- **Syntax:** `[%reg]` (e.g., `[%a]`, `[%b]`)
- **Value:** contents of the memory cell whose address is the current value of the register
- **Cycle cost:** 1

Example: `ld [%b], %a` — if `%b` = 200, loads the contents of memory cell 200 into `%a`.

## Indexed (*idx*)

The operand is the contents of memory at the address computed by adding a displacement to a register value.

- **Syntax:** `[%reg±offset]` (e.g., `[%b+3]`, `[%sp-1]`)
- **Value:** contents of the memory cell at address (register value + offset)
- **Cycle cost:** 1

The offset is a signed Word value that is added to the register's current value to compute the effective address.

Example: `ld [%b+3], %a` — if `%b` = 200, loads the contents of memory cell 203 into `%a`.

## Operand Type Combinations

Instructions use different subsets of addressing modes for their operands. The common combinations are:

| Abbreviation | Allowed Modes |
|---|---|
| *imm/reg/dir/ind/idx* | All five modes |
| *reg/dir/ind/idx* | Register, Direct, Indirect, Indexed |
| *dir/ind/idx* | Direct, Indirect, Indexed |
| *imm/reg* | Immediate, Register |
| *reg* | Register only |

When an operand is used as a **destination** (e.g., writing to memory in `st`), only modes that designate a memory location or register are valid — the *imm* mode cannot be a destination.
