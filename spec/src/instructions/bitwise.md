# Bitwise Instructions

## `and` — Bitwise AND

**Syntax:** `and src, reg`
where *src* is *imm/reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← reg & src
```

Performs a bitwise AND of the source value and the register value.

**Flags:**
- Z — set if the result is zero; cleared otherwise.
- N — set if the result is negative; cleared otherwise.

**Cycles:** 1 + cost(src) + cost(reg)

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address

---

## `or` — Bitwise OR

**Syntax:** `or src, reg`
where *src* is *imm/reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← reg | src
```

Performs a bitwise OR of the source value and the register value.

**Flags:**
- Z — set if the result is zero; cleared otherwise.
- N — set if the result is negative; cleared otherwise.

**Cycles:** 1 + cost(src) + cost(reg)

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address

---

## `xor` — Bitwise Exclusive OR

**Syntax:** `xor src, reg`
where *src* is *imm/reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← reg ^ src
```

Performs a bitwise exclusive OR of the source value and the register value.

**Flags:**
- Z — set if the result is zero; cleared otherwise.
- N — set if the result is negative; cleared otherwise.

**Cycles:** 1 + cost(src) + cost(reg)

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address

---

## `not` — Bitwise NOT

**Syntax:** `not reg`
where *reg* is a register.

**Operation:**

```
reg ← ~reg
```

Performs a bitwise complement (inversion of all bits) of the register value.

**Flags:**
- Z — set if the result is zero; cleared otherwise.
- N — set if the result is negative; cleared otherwise.

**Cycles:** 1 + cost(reg)

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode

---

## `shl` — Shift Left

**Syntax:** `shl src, reg`
where *src* is *imm/reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← reg << src
```

Shifts the register value left by the number of bits specified by the source operand. The shift amount (from the source) is first converted to a `u32`. If the conversion fails (negative value or too large), an *invalid instruction* exception is raised. If the shift amount exceeds the bit width, an *invalid instruction* exception is raised.

**Flags:**
- Z — set if the result is zero; cleared otherwise.
- N — set if the result is negative; cleared otherwise.

**Cycles:** 1 + cost(src) + cost(reg)

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Invalid instruction* if the shift amount cannot be converted to u32 or exceeds the bit width
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address

---

## `shr` — Shift Right

**Syntax:** `shr src, reg`
where *src* is *imm/reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← reg >> src
```

Shifts the register value right by the number of bits specified by the source operand. The shift is arithmetic (sign-extending) since the operand is a signed `i64`.

The shift amount (from the source) is first converted to a `u32`. If the conversion fails (negative value or too large), an *invalid instruction* exception is raised. If the shift amount exceeds the bit width, an *invalid instruction* exception is raised.

**Flags:**
- Z — set if the result is zero; cleared otherwise.
- N — set if the result is negative; cleared otherwise.

**Cycles:** 1 + cost(src) + cost(reg)

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Invalid instruction* if the shift amount cannot be converted to u32 or exceeds the bit width
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address
