# Data Movement Instructions

## `ld` — Load

**Syntax:** `ld src, reg`
where *src* is *imm/reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← src
```

Loads the source value into the destination register. Unlike most instructions, `ld` transfers the full cell value (including Instructions), not just a Word.

**Flags:** None.

**Cycles:** 1 + cost(src) + cost(reg)

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address

---

## `st` — Store

**Syntax:** `st reg, dst`
where *reg* is a register and *dst* is *dir/ind/idx*.

**Operation:**

```
memory[dst] ← reg
```

Stores the Word value of the register into the memory cell at the destination address. The register's value is extracted as a Word.

**Flags:** None.

**Cycles:** 1 + cost(reg) + cost(dst)

**Privileged:** No

**Exceptions:**
- *Invalid memory access* if *dst* refers to an out-of-bounds address

---

## `swap` — Swap

**Syntax:** `swap src, reg`
where *src* is *reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
temp ← src
src ← reg
reg ← temp
```

Atomically exchanges the values of the two operands. Both operands are read first, then both are written. When *src* is a register, both registers are swapped. When *src* is a memory location, the register's cell value is written to memory and the memory cell's value is loaded into the register.

The swap transfers full cell values (including Instructions), not just Words. When writing back to a memory location, the register's full cell value is preserved (the emulator's `write` method is generic and accepts any cell type). This differs from `st`, which explicitly extracts a Word value before writing.

**Flags:** None.

**Cycles:** 1 + cost(src) + cost(reg)

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address

---

## `push` — Push to Stack

**Syntax:** `push src`
where *src* is *imm/reg*.

**Operation:**

```
%sp ← %sp - 1
memory[%sp] ← src
```

Decrements the stack pointer, then stores the source value at the new stack pointer location. The value is transferred as a full cell (including Instructions for register operands).

**Flags:** None.

**Cycles:** 1 + cost(src)

**Privileged:** No

**Exceptions:**
- *Invalid memory access* if `%sp - 1` is out of bounds

---

## `pop` — Pop from Stack

**Syntax:** `pop reg`
where *reg* is a register.

**Operation:**

```
reg ← memory[%sp]
%sp ← %sp + 1
```

Reads the cell at the current stack pointer into the destination register, then increments the stack pointer. The value is transferred as a full cell.

**Flags:** None.

**Cycles:** 1 + cost(reg)

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if `%sp` is out of bounds
