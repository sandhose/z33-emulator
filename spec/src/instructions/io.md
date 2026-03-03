# I/O Instructions

The Z33 uses port-mapped I/O. These instructions are **privileged** and can only be executed in supervisor mode.

> **Note:** The `in` and `out` instructions are defined in the ISA but are **not currently implemented** in the emulator. Executing either instruction in the emulator will cause a runtime panic (`todo!()`). The semantics described here are based on the reference card.

## `in` — Input from I/O Port

**Syntax:** `in src, reg`
where *src* is *dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← io_read(src)
```

Reads a value from the I/O controller at the port address specified by the source operand and stores it in the destination register. The source operand specifies a port address (not a memory address).

**Flags:** None.

**Cycles:** 1 + cost(src) + cost(reg)

**Privileged:** **Yes**

**Exceptions:**
- *Privileged instruction* if executed in user mode

---

## `out` — Output to I/O Port

**Syntax:** `out src, dst`
where *src* is *imm/reg* and *dst* is *dir/ind/idx*.

**Operation:**

```
io_write(dst, src)
```

Writes the source value to the I/O controller at the port address specified by the destination operand.

**Flags:** None.

**Cycles:** 1 + cost(src) + cost(dst)

**Privileged:** **Yes**

**Exceptions:**
- *Privileged instruction* if executed in user mode
