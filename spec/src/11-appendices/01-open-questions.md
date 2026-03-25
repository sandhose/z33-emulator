# Open Questions

This appendix documents known discrepancies between the reference card, the emulator, and the course materials, as well as non-obvious design decisions.

## 1. Word Size and `.space` Directive

The reference card describes `.space` as reserving "bytes" (`octets` in the French version). However, the Z33 has no byte-level addressing — all memory cells are Word-sized (64-bit). The emulator treats `.space N` as reserving N cells, not N bytes.

This specification documents `.space` as reserving cells. The reference card's use of "bytes" is considered a simplification for familiarity.

---

## 2. `debugreg` Instruction

The `debugreg` instruction is present in the emulator but not documented in the reference card. It has zero cycle cost and no architectural effect — it simply logs the current register state for debugging purposes.

---

## 3. `swap` with Memory — Cell Type Preservation

When `swap` operates on a memory location and a register, both the memory read and the register read use `extract_cell()`, which preserves the full Cell type (including Instruction cells). The emulator's `write` method is generic (`T: Into<Cell>`), so `swap` actually preserves full cell types in both directions. This contrasts with `st`, which calls `extract_word()` and only stores a Word value.

In practice, `swap` is typically used with Word values. However, a `swap` involving an Instruction cell would preserve that cell type, while `st` would not.

---

## 4. Z33-V Page Table Entry Bit Layout

**Status:** Unresolved

**The discrepancy:**

The reference card's page table entry format has a duplicate bit number:

```
Reference card:  ... | 11 | 10 | 9 | 8 | 8 | 7 | 6...0
                 ... |  P |  R | X | S | A | D | framenum
```

Bit 8 is listed for both S (Supervisor) and A (Accessed). Additionally, if framenum occupies bits 6–0 (7 bits, needed for 100 possible frames), then bit 6 is assigned to both D (Dirty) and the framenum field.

**Why it matters:**

100 frames require 7 bits for the frame number (2^6 = 64 < 100 ≤ 128 = 2^7). With 6 flag bits (P, R, X, S, A, D), a non-overlapping layout requires bits 0–6 for framenum and bits 7–12 for flags.

**Most likely correct layout:**

```
Bit:  ...  12   11   10   9    8    7    6..0
       ...  P    R    X    S    A    D    framenum
```

Or equivalently, starting flags at bit 7 instead of the refcard's bit 11:

| Bit(s) | Field |
|---|---|
| 6–0 | framenum (7 bits, values 0–99) |
| 7 | D (Dirty) |
| 8 | A (Accessed) |
| 9 | S (Supervisor) |
| 10 | X (Execute) |
| 11 | R (Read-only) |
| 12 | P (Present) |

**Possible resolutions:**

1. **Shift all flags up by one bit** (P at bit 12) — gives 7 bits for framenum, no overlaps.
2. **Accept 6-bit framenum** (max 64 frames) — changes the architecture but avoids bit overlap.

**Note:** The course slides describe the flags generically without showing the specific bit layout, so they do not help resolve this. None of the variants are implemented in the emulator.
