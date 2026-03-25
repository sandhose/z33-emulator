# Control Flow Instructions

## `call` — Call Subroutine

r[inst.call]
`call target` — pushes the current program counter onto the stack, then jumps to the target address. Since `%pc` was already incremented during fetch, the saved address is the instruction after the `call`. Cycles: 1 + cost(target).

**Syntax:** `call target`
where *target* is *imm/reg/dir/ind/idx*.

**Operation:**

```
push(%pc)          // save return address (already incremented past 'call')
%pc ← target       // jump to subroutine
```

**Flags:** None.

**Privileged:** No

**Exceptions:**
- *Invalid memory access* if the stack push fails or *target* is out of bounds

---

## `rtn` — Return from Subroutine

r[inst.rtn]
`rtn` — pops a value from the stack and loads it into the program counter, returning to the instruction after the corresponding `call`. Cycles: 1.

**Syntax:** `rtn`

**Operation:**

```
%pc ← pop()
```

**Flags:** None.

**Privileged:** No

**Exceptions:**
- *Invalid memory access* if the stack pop fails

---

## `trap` — Trap

r[inst.trap]
`trap` — raises a *trap* exception (exception code 4). This is the mechanism for user-mode programs to request operating system services (system calls). Cycles: 1.

**Syntax:** `trap`

**Operation:**

The exception handling sequence saves `%pc`, `%sr`, and the exception code, then transfers control to the exception handler at address 200. See [Exceptions and Interrupts](../06-exceptions.md).

**Flags:** None (flags may be modified by the exception handler).

**Privileged:** No

**Exceptions:**
- *Trap* (always; this is the instruction's purpose)

---

## `rti` — Return from Interrupt

r[inst.rti]
`rti` — restores the program counter and status register from the save area (addresses 100 and 101), returning to the state before the exception or interrupt occurred. Cycles: 1. **Privileged.**

**Syntax:** `rti`

**Operation:**

```
%pc ← memory[100]
%sr ← memory[101]
```

The status register is restored from the raw Word value (with bit truncation).

**Flags:** All flags are restored from the saved `%sr` value.

**Privileged:** **Yes** — raises a *privileged instruction* exception if executed in user mode.

**Exceptions:**
- *Privileged instruction* if executed in user mode
- *Invalid memory access* if addresses 100 or 101 are not readable

---

## `reset` — Reset Processor

r[inst.reset]
`reset` — halts the processor. The program counter is decremented by 1 to point back to the `reset` instruction itself, then execution stops. Cycles: 1.

**Syntax:** `reset`

**Flags:** None.

**Privileged:** No

**Exceptions:** None (halts instead of continuing to the exception handler).

---

## `nop` — No Operation

r[inst.nop]
`nop` — does nothing. Execution continues with the next instruction. Cycles: 1.

**Syntax:** `nop`

**Flags:** None.

**Privileged:** No

**Exceptions:** None.
