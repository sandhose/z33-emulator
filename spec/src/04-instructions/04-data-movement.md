# Data Movement Instructions

## `ld` — Load

r[inst.ld]
`ld src, reg` — loads the source value into the destination register. Unlike most instructions, `ld` transfers the full cell value (including Instructions), not just a Word. Cycles: 1 + cost(src) + cost(reg).

**Syntax:** `ld src, reg`
where *src* is *imm/reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← src
```

**Flags:** None.

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address

---

## `st` — Store

r[inst.st]
`st reg, dst` — stores the Word value of the register into the memory cell at the destination address. The register's value is extracted as a Word. Cycles: 1 + cost(reg) + cost(dst).

**Syntax:** `st reg, dst`
where *reg* is a register and *dst* is *dir/ind/idx*.

**Operation:**

```
memory[dst] ← reg
```

**Flags:** None.

**Privileged:** No

**Exceptions:**
- *Invalid memory access* if *dst* refers to an out-of-bounds address

---

## `swap` — Swap

r[inst.swap]
`swap src, reg` — atomically exchanges the values of the two operands. Both operands are read first, then both are written. Transfers full cell values (including Instructions). Cycles: 1 + cost(src) + cost(reg).

**Syntax:** `swap src, reg`
where *src* is *reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
temp ← src
src ← reg
reg ← temp
```

When *src* is a register, both registers are swapped. When *src* is a memory location, the register's cell value is written to memory and the memory cell's value is loaded into the register.

The swap transfers full cell values (including Instructions), not just Words. When writing back to a memory location, the register's full cell value is preserved (the emulator's `write` method is generic and accepts any cell type). This differs from `st`, which explicitly extracts a Word value before writing.

**Flags:** None.

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address

---

## `push` — Push to Stack

r[inst.push]
`push src` — decrements the stack pointer, then stores the source value at the new stack pointer location. The value is transferred as a full cell. Cycles: 1 + cost(src).

**Syntax:** `push src`
where *src* is *imm/reg*.

**Operation:**

```
%sp ← %sp - 1
memory[%sp] ← src
```

**Flags:** None.

**Privileged:** No

**Exceptions:**
- *Invalid memory access* if `%sp - 1` is out of bounds

---

## `pop` — Pop from Stack

r[inst.pop]
`pop reg` — reads the cell at the current stack pointer into the destination register, then increments the stack pointer. The value is transferred as a full cell. Cycles: 1 + cost(reg).

**Syntax:** `pop reg`
where *reg* is a register.

**Operation:**

```
reg ← memory[%sp]
%sp ← %sp + 1
```

**Flags:** None.

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if `%sp` is out of bounds
