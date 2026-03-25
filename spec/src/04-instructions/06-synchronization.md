# Synchronization Instructions

## `fas` — Fetch and Set

r[inst.fas]
`fas src, reg` — atomically reads the cell at the source memory address into the destination register, then sets that memory cell to the Word value 1. This is an atomic test-and-set operation for implementing mutual exclusion primitives. Cycles: 1 + cost(src) + cost(reg).

**Syntax:** `fas src, reg`
where *src* is *dir/ind/idx* and *reg* is a register.

**Operation:**

```
reg ← memory[src]
memory[src] ← 1
```

The read and write are performed as a single indivisible operation — no interrupt or exception can occur between them.

**Typical usage:**

```z33
// Spinlock acquire
spin:
    fas  [lock], %a       // atomically read lock, set lock = 1
    cmp  0, %a            // was it 0 (unlocked)?
    jne  spin             // no — someone else holds it, retry

// Critical section...

// Spinlock release
    ld   0, %a
    st   %a, [lock]       // set lock = 0
```

**Flags:** None.

**Privileged:** No (unless *reg* is `%sr`)

**Exceptions:**
- *Privileged instruction* if writing to `%sr` in user mode
- *Invalid memory access* if *src* refers to an out-of-bounds address
