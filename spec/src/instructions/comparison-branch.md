# Comparison and Branch Instructions

## `cmp` — Compare

**Syntax:** `cmp src, reg`
where *src* is *imm/reg/dir/ind/idx* and *reg* is a register.

**Operation:**

```
Z ← (src == reg)
N ← (src < reg)
```

Compares the source value to the register value and sets the Z and N flags accordingly. No other state is modified — the register value is unchanged.

**Important:** The comparison semantics use the **source operand's perspective**:
- Z is set when *src* equals *reg*
- N is set when *src* is **less than** *reg*

This means that in `cmp 1, %a`:
- If `%a` = 1: Z=1, N=0
- If `%a` = 0: Z=0, N=1 (because 1 < 0 is false... wait: src=1, reg=0, so N = (1 < 0) = false)
- If `%a` = 5: Z=0, N=1 (because src=1, reg=5, so N = (1 < 5) = true)

The conditional jump instructions test these flags from the same perspective: `jlt` jumps when *src* < *reg*.

**Flags:** Z — set if src == reg; N — set if src < reg.

**Cycles:** 1 + cost(src) + cost(reg)

**Privileged:** No

**Exceptions:**
- *Invalid memory access* if *src* refers to an out-of-bounds address

---

## `jmp` — Unconditional Jump

**Syntax:** `jmp target`
where *target* is *imm/reg/dir/ind/idx*.

**Operation:**

```
%pc ← target
```

The operand is interpreted as an address and loaded into the program counter.

**Flags:** None.

**Cycles:** 1 + cost(target)

**Privileged:** No

**Exceptions:**
- *Invalid memory access* if *target* refers to an out-of-bounds address (when using dir/ind/idx modes)

---

## Conditional Jumps

All conditional jumps share the same syntax and behavior, differing only in the condition tested.

**Syntax:** `jXX target`
where *target* is *imm/reg/dir/ind/idx*.

**Operation:**

```
if condition then
    %pc ← target
```

If the condition is false, the instruction has no effect (execution continues at the next instruction). The target address is only evaluated if the condition is true.

**Flags:** None.

**Cycles:** 1 + cost(target) (the target cost is always counted, regardless of whether the jump is taken)

**Privileged:** No

### Condition Table

Given `cmp src, reg` was the most recent flag-setting instruction:

| Mnemonic | Condition | Flags Tested | Meaning |
|---|---|---|---|
| `jeq` | src == reg | Z = 1 | Jump if equal |
| `jne` | src ≠ reg | Z = 0 | Jump if not equal |
| `jlt` | src < reg | Z = 0 **and** N = 1 | Jump if less than |
| `jle` | src ≤ reg | Z = 1 **or** N = 1 | Jump if less or equal |
| `jgt` | src > reg | Z = 0 **and** N = 0 | Jump if greater than |
| `jge` | src ≥ reg | Z = 1 **or** N = 0 | Jump if greater or equal |

### Example

```z33
    cmp  1, %a       // compare 1 with %a
    jge  special      // jump if 1 >= %a (i.e., %a <= 1)
```

In this example from the factorial function: `cmp 1, %a` sets the flags from the perspective of `src=1`. Then `jge special` jumps when `src >= reg`, i.e., when `1 >= %a`, which is equivalent to `%a <= 1`.

**Exceptions:**
- *Invalid memory access* if *target* uses dir/ind/idx modes and the address is out of bounds. This exception is only raised when the condition is true (since the target address is only evaluated on a taken branch).
