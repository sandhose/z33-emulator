# Arithmetic Instructions

## `add` — Add

**Syntax:** `add src, reg`
where *src* is *imm/reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← reg + src
```

Both operands are interpreted as Words (i64). The addition uses two's complement wrapping semantics.

**Flags:**
- O — set if the addition overflows (signed wrapping occurs); cleared otherwise.
- Z — set if the result is zero; cleared otherwise.
- N — set if the result is negative; cleared otherwise.

**Cycles:** 1 + cost(src) + cost(reg)

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address

---

## `sub` — Subtract

**Syntax:** `sub src, reg`
where *src* is *imm/reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← reg - src
```

Both operands are interpreted as Words (i64). The subtraction uses two's complement wrapping semantics.

**Flags:**
- O — set if the subtraction overflows; cleared otherwise.
- Z — set if the result is zero; cleared otherwise.
- N — set if the result is negative; cleared otherwise.

**Cycles:** 1 + cost(src) + cost(reg)

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address

---

## `mul` — Multiply

**Syntax:** `mul src, reg`
where *src* is *imm/reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← reg * src
```

Both operands are interpreted as Words (i64). The multiplication uses two's complement wrapping semantics (the result is truncated to 64 bits).

**Flags:**
- O — set if the multiplication overflows; cleared otherwise.
- Z — set if the result is zero; cleared otherwise.
- N — set if the result is negative; cleared otherwise.

**Cycles:** 1 + cost(src) + cost(reg)

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address

---

## `div` — Divide

**Syntax:** `div src, reg`
where *src* is *imm/reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← reg / src
```

Integer division (truncated toward zero). Both operands are interpreted as Words (i64).

**Flags:**
- Z — set if the result is zero; cleared otherwise.
- N — set if the result is negative; cleared otherwise.

**Cycles:** 1 + cost(src) + cost(reg)

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Division by zero* if src = 0
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address

---

## `neg` — Negate

**Syntax:** `neg reg`
where *reg* is a register.

**Operation:**

```
reg ← -reg
```

The register value is interpreted as a Word (i64) and arithmetically negated using two's complement.

**Flags:**
- O — set if the negation overflows (operand is `i64::MIN`); cleared otherwise.
- Z — set if the result is zero; cleared otherwise.
- N — set if the result is negative; cleared otherwise.

**Cycles:** 1 + cost(reg)

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode

> **Note:** Negation of the minimum `i64` value (`-2^63`) overflows back to itself (`i64::MIN`), sets the O flag, and sets the N flag. See [Undefined Behavior](../appendices/undefined-behavior.md).
