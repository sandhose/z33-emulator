# Bitwise Instructions

## `and` — Bitwise AND

r[inst.and]
`and src, reg` — performs a bitwise AND of the source value and the register value. Sets Z, N flags. Cycles: 1 + cost(src) + cost(reg).

**Syntax:** `and src, reg`
where *src* is *imm/reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← reg & src
```

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address

---

## `or` — Bitwise OR

r[inst.or]
`or src, reg` — performs a bitwise OR of the source value and the register value. Sets Z, N flags. Cycles: 1 + cost(src) + cost(reg).

**Syntax:** `or src, reg`
where *src* is *imm/reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← reg | src
```

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address

---

## `xor` — Bitwise Exclusive OR

r[inst.xor]
`xor src, reg` — performs a bitwise exclusive OR of the source value and the register value. Sets Z, N flags. Cycles: 1 + cost(src) + cost(reg).

**Syntax:** `xor src, reg`
where *src* is *imm/reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← reg ^ src
```

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address

---

## `not` — Bitwise NOT

r[inst.not]
`not reg` — performs a bitwise complement (inversion of all bits) of the register value. Sets Z, N flags. Cycles: 1 + cost(reg).

**Syntax:** `not reg`
where *reg* is a register.

**Operation:**

```
reg ← ~reg
```

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode

---

## `shl` — Shift Left

r[inst.shl]
`shl src, reg` — shifts the register value left by the number of bits specified by the source operand. Sets Z, N flags. Cycles: 1 + cost(src) + cost(reg).

**Syntax:** `shl src, reg`
where *src* is *imm/reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← reg << src
```

The shift amount (from the source) is first converted to a `u32`. If the conversion fails (negative value or too large), an *invalid instruction* exception is raised. If the shift amount exceeds the bit width, an *invalid instruction* exception is raised.

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**

r[inst.shl.exception.invalid-shift]
*Invalid instruction* exception if the shift amount cannot be converted to u32 or exceeds the bit width.

- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address

---

## `shr` — Shift Right

r[inst.shr]
`shr src, reg` — shifts the register value right (arithmetic, sign-extending) by the number of bits specified by the source operand. Sets Z, N flags. Cycles: 1 + cost(src) + cost(reg).

**Syntax:** `shr src, reg`
where *src* is *imm/reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← reg >> src
```

The shift is arithmetic (sign-extending) since the operand is a signed `i64`.

The shift amount (from the source) is first converted to a `u32`. If the conversion fails (negative value or too large), an *invalid instruction* exception is raised. If the shift amount exceeds the bit width, an *invalid instruction* exception is raised.

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**

r[inst.shr.exception.invalid-shift]
*Invalid instruction* exception if the shift amount cannot be converted to u32 or exceeds the bit width.

- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address
