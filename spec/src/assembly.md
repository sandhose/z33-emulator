# Assembly Language

This chapter describes the Z33 assembly language syntax as accepted by the emulator's built-in assembler.

## Line Format

Each line of a Z33 assembly program has the following general form:

```
[label:] [label:] ... [instruction | directive]  [// comment]
```

- **Labels:** Zero or more symbol definitions, each followed by a colon (`:`). Multiple labels may be defined on the same line.
- **Content:** An instruction or an assembler directive (optional).
- **Comments:** Everything from `//` to the end of the line is ignored.

Blank lines and lines containing only comments are permitted.

### Examples

```z33
start:                      // label only
    ld 1, %a                // instruction only
loop: sub 1, %a             // label + instruction
end: done: reset            // multiple labels + instruction
```

## Labels

A label associates a symbolic name with the current memory address. Labels are identifiers followed by a colon.

- Labels may contain letters, digits, and underscores.
- Labels are case-sensitive.
- Multiple labels may be defined on the same line (they all refer to the same address).
- Labels can be used as immediate values or in expressions anywhere a numeric constant is expected.

## Comments

Line comments are supported:

- **Line comments:** `//` introduces a comment that extends to the end of the line.

## Instructions

Instructions follow the syntax described in the [Instruction Set Reference](./instructions/README.md). The general form is:

```
mnemonic [operand [, operand]]
```

Mnemonics are **case-insensitive** (e.g., `ADD`, `add`, and `Add` are all equivalent).

## Assembler Directives

Directives control the assembly process rather than generating instructions.

### `.addr` — Set Current Address

```
.addr expression
```

Sets the current assembly address to the given value. Subsequent instructions and data will be placed starting at this address.

Example:
```z33
.addr 200
    // The next instruction will be at address 200
```

### `.space` — Reserve Space

```
.space expression
```

Reserves the specified number of cells at the current address. The cells are left uninitialized (Empty).

> **Note:** The reference card describes `.space` as reserving "bytes", but since the Z33 has no byte-level addressing, this directive actually reserves *cells* (one cell per unit).

Example:
```z33
buffer: .space 10     // reserve 10 cells
```

### `.word` — Define a Word

```
.word expression
```

Places a Word value at the current address and advances the address by one cell.

Example:
```z33
max_value: .word 0x1000
counter:   .word 0
```

### `.string` — Define a String

```
.string "text"
```

Places each character of the string as a separate Word value at consecutive addresses, followed by a null terminator (0). Advances the address by the length of the string plus one (for the null terminator).

Standard escape sequences are supported: `\n`, `\"`, `\\`.

Example:
```z33
msg: .string "hello, world!\n"
// Occupies 15 cells (14 characters + null terminator)
```

## Preprocessor

The assembler includes a C-like preprocessor that processes the source before assembly. Preprocessor directives begin with `#`.

### `#define` — Define a Symbol

```
#define SYMBOL [value]
```

Defines a preprocessor symbol with an optional replacement value. When the symbol appears in subsequent source text, it is replaced with its value.

Example:
```z33
#define NBYTES  14
#define P_WRITE 7
    push NBYTES          // equivalent to: push 14
```

### `#undefine` — Undefine a Symbol

```
#undefine SYMBOL
```

Removes a previously defined preprocessor symbol.

### `#include` — Include a File

```
#include "filename"
```

Inserts the contents of the specified file at the current position in the source.

Example:
```z33
#include "defs.h"
```

### `#if` / `#elif` / `#else` / `#endif` — Conditional Assembly

```
#if expression
    // assembled if expression is true
#elif expression
    // assembled if previous conditions were false and this is true
#else
    // assembled if all conditions were false
#endif
```

Conditionally includes or excludes blocks of source code based on preprocessor expressions.

Example:
```z33
#if defined(DEBUG)
    debugreg
#endif
```

### `#error` — Emit an Error

```
#error "message"
```

Causes the assembler to emit an error with the specified message. Useful for detecting invalid preprocessor configurations.

## Expressions

Numeric expressions can be used wherever a value is expected (immediate operands, directive arguments, etc.). Expressions support:

- **Integer literals:** Decimal (`42`), hexadecimal (`0xFF`), binary (`0b1010`), octal (`0o17`)
- **Label references:** The address associated with a label
- **Arithmetic operators:** `+`, `-`, `*`, `/`
- **Bitwise operators:** `&` (AND), `|` (OR), `^` (XOR), `~` (NOT), `<<` (left shift), `>>` (right shift)
- **Parentheses** for grouping

Expressions are evaluated at assembly time. All arithmetic is performed on signed 128-bit integers and then truncated to the target type (Word or Address).
