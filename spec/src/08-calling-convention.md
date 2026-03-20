# Calling Convention

*This chapter is informative. The calling convention is a software agreement, not enforced by the hardware.*

## Overview

The Z33 calling convention defines how subroutines receive arguments, return values, and preserve registers. It is used by the course materials and example programs.

## Rules

1. **Arguments** are pushed onto the stack **in reverse order** (last argument pushed first, so that the first argument is at the lowest stack offset).

2. **Return value** is placed in `%a`.

3. **Register preservation:** The callee must save and restore `%b` if it uses it. `%a` is caller-saved (the callee may freely modify it).

4. **Stack cleanup:** The caller is responsible for removing arguments from the stack after the call returns, typically using `add N, %sp` where N is the number of arguments pushed.

## Stack Frame Layout

After `call` and before the callee's prologue, the stack looks like:

```
        ┌─────────────────┐  (high addresses)
        │   arg N          │  [%sp + N]
        │   ...            │
        │   arg 2          │  [%sp + 2]
        │   arg 1          │  [%sp + 1]
%sp →   │   return address │  [%sp]
        └─────────────────┘  (low addresses, stack grows down)
```

Arguments are accessed relative to `%sp`: the first argument is at `[%sp+1]`, the second at `[%sp+2]`, and so on.

## Example: Factorial

```z33
// Computes n! (n given as an argument on the stack)
factorial:
    ld   [%sp+1], %a      // load argument n
    cmp  1, %a
    jge  specialcase       // jump if 1 >= n (i.e., n <= 1)

// General case
    sub  1, %a             // a ← n - 1
    push %a
    call factorial          // a ← (n-1)!
    add  1, %sp            // pop argument n-1
    push %b                // save b
    ld   [%sp+2], %b       // b ← n (original argument)
    mul  %b, %a            // a ← n * (n-1)!
    pop  %b                // restore b
    rtn

specialcase:
    ld   1, %a             // return 1
    rtn
```

## System Call Convention

For `trap`-based system calls:

1. Push arguments onto the stack (same reverse order convention).
2. Load the system call number into `%a`.
3. Execute `trap`.
4. On return, `%a` contains the return value.
5. Clean up arguments with `add N, %sp`.

```z33
    push NBYTES            // arg 3: byte count
    push str               // arg 2: buffer address
    push 1                 // arg 1: file descriptor (stdout)
    ld   P_WRITE, %a       // system call number
    trap                   // invoke kernel
    add  3, %sp            // clean up 3 arguments
```
