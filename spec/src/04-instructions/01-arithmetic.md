# Arithmetic Instructions

## `add` — Add

r[inst.add]
`add src, reg` — adds the source value to the register. Both operands are interpreted as Words (i64) with two's complement wrapping. Sets O, Z, N flags. Cycles: 1 + cost(src) + cost(reg).

**Syntax:** `add src, reg`
where *src* is *imm/reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← reg + src
```

**Flags:**
- O — set if the addition overflows (signed wrapping occurs); cleared otherwise.
- Z — set if the result is zero; cleared otherwise.
- N — set if the result is negative; cleared otherwise.

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address

---

## `sub` — Subtract

r[inst.sub]
`sub src, reg` — subtracts the source value from the register. Both operands are interpreted as Words (i64) with two's complement wrapping. Sets O, Z, N flags. Cycles: 1 + cost(src) + cost(reg).

**Syntax:** `sub src, reg`
where *src* is *imm/reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← reg - src
```

**Flags:**
- O — set if the subtraction overflows; cleared otherwise.
- Z — set if the result is zero; cleared otherwise.
- N — set if the result is negative; cleared otherwise.

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address

---

## `mul` — Multiply

r[inst.mul]
`mul src, reg` — multiplies the register by the source value. Both operands are interpreted as Words (i64) with two's complement wrapping (truncated to 64 bits). Sets O, Z, N flags. Cycles: 1 + cost(src) + cost(reg).

**Syntax:** `mul src, reg`
where *src* is *imm/reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← reg * src
```

**Flags:**
- O — set if the multiplication overflows; cleared otherwise.
- Z — set if the result is zero; cleared otherwise.
- N — set if the result is negative; cleared otherwise.

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address

---

## `div` — Divide

r[inst.div]
`div src, reg` — divides the register by the source value. Integer division truncated toward zero. Both operands are interpreted as Words (i64). Sets Z, N flags. Cycles: 1 + cost(src) + cost(reg).

**Syntax:** `div src, reg`
where *src* is *imm/reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← reg / src
```

**Flags:**
- Z — set if the result is zero; cleared otherwise.
- N — set if the result is negative; cleared otherwise.

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**

r[inst.div.exception.division-by-zero]
*Division by zero* exception is raised if src = 0.

- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address

---

## `neg` — Negate

r[inst.neg]
`neg reg` — arithmetically negates the register value using two's complement. Sets O, Z, N flags. Cycles: 1 + cost(reg).

**Syntax:** `neg reg`
where *reg* is a register.

**Operation:**

```
reg ← -reg
```

**Flags:**
- O — set if the negation overflows (operand is `i64::MIN`); cleared otherwise.
- Z — set if the result is zero; cleared otherwise.
- N — set if the result is negative; cleared otherwise.

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode

> **Note:** Negation of the minimum `i64` value (`-2^63`) overflows back to itself (`i64::MIN`), sets the O flag, and sets the N flag. See [Undefined Behavior](../11-appendices/02-undefined-behavior.md).
