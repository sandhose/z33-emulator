# Assembly Language

This chapter describes the Z33 assembly language syntax as accepted by the emulator's built-in assembler.

## Line Format

r[asm.line-format]
Each line of a Z33 assembly program has the form: `[label:] [label:] ... [instruction | directive] [// comment]`. Labels, content, and comments are all optional. Blank lines and comment-only lines are permitted.

### Examples

```z33
start:                      // label only
    ld 1, %a                // instruction only
loop: sub 1, %a             // label + instruction
end: done: reset            // multiple labels + instruction
```

## Labels

r[asm.labels]
A label associates a symbolic name with the current memory address. Labels are identifiers (letters, digits, underscores) followed by a colon. Labels are case-sensitive. Multiple labels may be defined on the same line. Labels can be used as immediate values or in expressions anywhere a numeric constant is expected.

## Comments

Line comments are supported:

- **Line comments:** `//` introduces a comment that extends to the end of the line.

## Instructions

r[asm.mnemonics]
Mnemonics are **case-insensitive** (e.g., `ADD`, `add`, and `Add` are all equivalent). The general form is `mnemonic [operand [, operand]]`.

Instructions follow the syntax described in the [Instruction Set Reference](./04-instructions/00-README.md).

## Assembler Directives

Directives control the assembly process rather than generating instructions.

### `.addr` — Set Current Address

r[asm.directive.addr]
`.addr expression` — sets the current assembly address to the given value. Subsequent instructions and data will be placed starting at this address.

Example:
```z33
.addr 200
    // The next instruction will be at address 200
```

### `.space` — Reserve Space

r[asm.directive.space]
`.space expression` — reserves the specified number of cells at the current address. The cells are left uninitialized (Empty).

> **Note:** The reference card describes `.space` as reserving "bytes", but since the Z33 has no byte-level addressing, this directive actually reserves *cells* (one cell per unit).

Example:
```z33
buffer: .space 10     // reserve 10 cells
```

### `.word` — Define a Word

r[asm.directive.word]
`.word expression` — places a Word value at the current address and advances the address by one cell.

Example:
```z33
max_value: .word 0x1000
counter:   .word 0
```

### `.string` — Define a String

r[asm.directive.string]
`.string "text"` — places each character of the string as a separate Word value at consecutive addresses, followed by a null terminator (0). Advances the address by the length of the string plus one.

Standard escape sequences are supported: `\n`, `\"`, `\\`.

Example:
```z33
msg: .string "hello, world!\n"
// Occupies 15 cells (14 characters + null terminator)
```

## Preprocessor

The assembler includes a C-like preprocessor that processes the source before assembly. Preprocessor directives begin with `#`.

### `#define` — Define a Symbol

r[asm.preprocessor.define]
`#define SYMBOL [value]` — defines a preprocessor symbol with an optional replacement value. When the symbol appears in subsequent source text, it is replaced with its value.

Example:
```z33
#define NBYTES  14
#define P_WRITE 7
    push NBYTES          // equivalent to: push 14
```

### `#undefine` — Undefine a Symbol

r[asm.preprocessor.undefine]
`#undefine SYMBOL` — removes a previously defined preprocessor symbol so it is no longer substituted in subsequent source text.

### `#include` — Include a File

r[asm.preprocessor.include]
`#include "filename"` — inserts the contents of the specified file at the current position in the source.

Example:
```z33
#include "defs.h"
```

### `#if` / `#elif` / `#else` / `#endif` — Conditional Assembly

r[asm.preprocessor.conditional]
`#if`/`#elif`/`#else`/`#endif` — conditionally includes or excludes blocks of source code based on preprocessor expressions.

```
#if expression
    // assembled if expression is true
#elif expression
    // assembled if previous conditions were false and this is true
#else
    // assembled if all conditions were false
#endif
```

Example:
```z33
#if defined(DEBUG)
    debugreg
#endif
```

### `#error` — Emit an Error

r[asm.preprocessor.error]
`#error "message"` — causes the assembler to emit an error with the specified message. Useful for detecting invalid preprocessor configurations.

## Expressions

r[asm.expressions]
Numeric expressions can be used wherever a value is expected. Expressions support integer literals (decimal, hex, binary, octal), label references, arithmetic operators (`+`, `-`, `*`, `/`), bitwise operators (`&`, `|`, `^`, `~`, `<<`, `>>`), and parentheses. All arithmetic is performed on signed 128-bit integers and truncated to the target type.
