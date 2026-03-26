# Introduction

## Purpose

This document is the authoritative specification for the **Zorglub-33 (Z33)** instruction set architecture, a fictional processor designed by Pierre David at the University of Strasbourg for the *Architecture des Systèmes d'Exploitation* (ASE) course.

The Z33 is an educational architecture: it is deliberately simplified compared to real-world processors, but retains enough complexity to teach operating system concepts such as privileged execution, exceptions, interrupts, memory management, and system calls.

## Scope

This specification defines:

- The programmer-visible state (registers, memory, status flags)
- All addressing modes
- The complete instruction set (33 instructions)
- The execution model (fetch-decode-execute cycle, privilege levels)
- Exception and interrupt handling
- The assembly language syntax and directives
- Three memory management variants (Z33-M, Z33-S, Z33-V)

This specification does **not** define:

- A binary encoding for instructions. The Z33 is not intended to model hardware-level instruction encoding. Each instruction occupies exactly one memory cell as an opaque object.
- Pipeline or microarchitectural details beyond cycle counting.

## Conventions

Throughout this document:

- **Word** refers to a 64-bit signed integer (`i64`).
- **Address** refers to a 32-bit unsigned integer (`u32`).
- Register names are prefixed with `%` (e.g., `%a`, `%sp`).
- `src` denotes a source operand; `reg` or `dst` denotes a destination register.
- Pseudocode uses C-like syntax. `←` denotes assignment.
- *Normative* text describes required behavior. Sections marked *(Informative)* provide guidance but are not binding.

## Notation for Operand Types

| Abbreviation | Meaning |
|---|---|
| *imm* | Immediate value |
| *reg* | Register |
| *dir* | Direct memory access `[addr]` |
| *ind* | Indirect memory access `[%reg]` |
| *idx* | Indexed memory access `[%reg±offset]` |

## Relationship to Existing Materials

This specification consolidates information from three sources:

1. **Reference card** — a two-page LaTeX cheat sheet summarizing the ISA
2. **Emulator** — a Rust implementation that serves as the de facto ground truth for instruction semantics
3. **Course materials** — exercises, exams, and slides from the ASE course

Where these sources disagree, discrepancies are documented in the [Open Questions](./11-appendices/01-open-questions.md) appendix. Unless otherwise noted, the emulator's behavior is taken as the normative baseline.
