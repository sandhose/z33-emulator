# Addressing Modes

The Z33 supports five addressing modes. Not all modes are available for every instruction — each instruction's entry in the [Instruction Set Reference](./04-instructions/00-README.md) specifies which modes are permitted.

## Summary

| Mode | Syntax | Value Produced | Cycle Cost |
|---|---|---|---|
| Immediate | `42` | The literal value itself | 0 |
| Register | `%a` | Contents of the register | 0 |
| Direct | `[100]` | Contents of memory at the given address | 1 |
| Indirect | `[%a]` | Contents of memory at the address held in the register | 1 |
| Indexed | `[%a+5]` | Contents of memory at (register value + offset) | 1 |

## Immediate (*imm*)

r[addr.immediate]
The operand is a literal value embedded in the instruction. Cycle cost: 0.

- **Syntax:** a numeric constant or expression (e.g., `42`, `0xFF`, `label`)
- **Value:** the constant itself

Example: `ld 100, %a` — loads the value 100 into `%a`.

## Register (*reg*)

r[addr.register]
The operand is the contents of a register. Cycle cost: 0.

- **Syntax:** `%a`, `%b`, `%pc`, `%sp`, `%sr`
- **Value:** current contents of the named register

Example: `ld %b, %a` — loads the value of `%b` into `%a`.

## Direct (*dir*)

r[addr.direct]
The operand is the contents of memory at a fixed address. Cycle cost: 1.

- **Syntax:** `[addr]` where *addr* is a numeric constant or expression (e.g., `[100]`, `[label]`)
- **Value:** contents of the memory cell at the given address

Example: `ld [100], %a` — loads the contents of memory cell 100 into `%a`.

## Indirect (*ind*)

r[addr.indirect]
The operand is the contents of memory at the address held in a register. Cycle cost: 1.

- **Syntax:** `[%reg]` (e.g., `[%a]`, `[%b]`)
- **Value:** contents of the memory cell whose address is the current value of the register

Example: `ld [%b], %a` — if `%b` = 200, loads the contents of memory cell 200 into `%a`.

## Indexed (*idx*)

r[addr.indexed]
The operand is the contents of memory at the address computed by adding a displacement to a register value. Cycle cost: 1.

- **Syntax:** `[%reg±offset]` (e.g., `[%b+3]`, `[%sp-1]`)
- **Value:** contents of the memory cell at address (register value + offset)

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

r[addr.destination]
When an operand is used as a **destination** (e.g., writing to memory in `st`), only modes that designate a memory location or register are valid — the *imm* mode cannot be a destination.
