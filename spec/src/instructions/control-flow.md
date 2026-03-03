# Control Flow Instructions

## `call` — Call Subroutine

**Syntax:** `call target`
where *target* is *imm/reg/dir/ind/idx*.

**Operation:**

```
push(%pc)          // save return address (already incremented past 'call')
%pc ← target       // jump to subroutine
```

Pushes the current program counter onto the stack (this is the address of the instruction *after* the `call`, since `%pc` was already incremented during the fetch phase), then jumps to the target address.

**Flags:** None.

**Cycles:** 1 + cost(target)

**Privileged:** No

**Exceptions:**
- *Invalid memory access* if the stack push fails or *target* is out of bounds

---

## `rtn` — Return from Subroutine

**Syntax:** `rtn`

**Operation:**

```
%pc ← pop()
```

Pops a value from the stack and loads it into the program counter, returning to the instruction after the corresponding `call`.

**Flags:** None.

**Cycles:** 1

**Privileged:** No

**Exceptions:**
- *Invalid memory access* if the stack pop fails

---

## `trap` — Trap

**Syntax:** `trap`

**Operation:**

Raises a *trap* exception (exception code 4). This is the mechanism for user-mode programs to request operating system services (system calls).

The exception handling sequence saves `%pc`, `%sr`, and the exception code, then transfers control to the exception handler at address 200. See [Exceptions and Interrupts](../exceptions.md).

**Flags:** None (flags may be modified by the exception handler).

**Cycles:** 1

**Privileged:** No

**Exceptions:**
- *Trap* (always; this is the instruction's purpose)

---

## `rti` — Return from Interrupt

**Syntax:** `rti`

**Operation:**

```
%pc ← memory[100]
%sr ← memory[101]
```

Restores the program counter and status register from the save area, returning to the state before the exception or interrupt occurred. The status register is restored from the raw Word value (with bit truncation).

**Flags:** All flags are restored from the saved `%sr` value.

**Cycles:** 1

**Privileged:** **Yes** — raises a *privileged instruction* exception if executed in user mode.

**Exceptions:**
- *Privileged instruction* if executed in user mode
- *Invalid memory access* if addresses 100 or 101 are not readable

---

## `reset` — Reset Processor

**Syntax:** `reset`

**Operation:**

Halts the processor. The program counter is decremented by 1 to point back to the `reset` instruction itself, then execution stops.

In the emulator, this causes the `run` loop to return successfully.

**Flags:** None.

**Cycles:** 1

**Privileged:** No

**Exceptions:** None (halts instead of continuing to the exception handler).

---

## `nop` — No Operation

**Syntax:** `nop`

**Operation:**

Does nothing. Execution continues with the next instruction.

**Flags:** None.

**Cycles:** 1

**Privileged:** No

**Exceptions:** None.
