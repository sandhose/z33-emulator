# Open Questions

This appendix documents known discrepancies between the reference card, the emulator, and the course materials. These items require resolution with the course author.

## 1. Shift Instruction Operand Order

**Status:** Resolved

The emulator previously computed `shl src, reg` as `reg ← src << reg` (shift the *source* by the *register* amount). This was inconsistent with all other binary instructions which follow the `reg ← reg OP src` pattern.

**Resolution:** The emulator has been fixed to compute `reg ← reg << src`, matching the reference card description and the pattern of other binary instructions. The shift amount now comes from the source operand.

---

## 2. Flag Semantics: Refcard vs. Emulator

**Status:** Resolved

The reference card implied all four flags (O, N, Z, C) are set by both `cmp` and arithmetic instructions. The emulator previously only set O on `add`/`sub`/`mul` and Z/N on `cmp`.

**Resolution:** Arithmetic instructions (`add`, `sub`, `mul`, `neg`) now set O, Z, and N. `div` sets Z and N. Bitwise instructions (`and`, `or`, `xor`, `not`, `shl`, `shr`) now set Z and N. The C (Carry) flag remains reserved and unused.

---

## 3. Word Size and `.space` Directive

**Status:** Documented

The reference card describes `.space` as reserving "bytes" (`octets` in the French version). However, the Z33 has no byte-level addressing — all memory cells are Word-sized (64-bit). The emulator treats `.space N` as reserving N cells, not N bytes.

**Resolution:** This specification documents `.space` as reserving cells. The reference card's use of "bytes" is considered a simplification for familiarity.

---

## 4. `debugreg` Instruction

**Status:** Documented

The `debugreg` instruction is present in the emulator but not documented in the reference card. It has zero cycle cost and no architectural effect — it simply logs the current register state for debugging purposes.

**Resolution:** Documented in this specification as a debug-only instruction.

---

## 5. `swap` with Memory — Cell Type Preservation

**Status:** Minor

When `swap` operates on a memory location and a register, both the memory read and the register read use `extract_cell()`, which preserves the full Cell type (including Instruction cells). The emulator's `write` method is generic (`T: Into<Cell>`), so `swap` actually preserves full cell types in both directions. This contrasts with `st`, which calls `extract_word()` and only stores a Word value.

In practice, `swap` is typically used with Word values. However, a `swap` involving an Instruction cell would preserve that cell type, while `st` would not.

---

## 6. IE Behavior on Non-Interrupt Exceptions

**Status:** Resolved

The emulator previously set `IE = !is_hardware_interrupt()`, forcing IE to 1 for software exceptions regardless of its previous value. The course slides state "le bit IE du registre SR n'est pas modifié" (IE is not modified) for non-interrupt exceptions.

**Resolution:** The emulator has been fixed to match the course slides. IE is now only modified for hardware interrupts (set to 0). For all other exceptions and traps, IE is left unchanged.

---

## 7. `from_bits_truncate` vs `from_bits_retain` for Status Register

**Status:** Resolved

The emulator previously used `from_bits_truncate` in `rti` but `from_bits_retain` in `ld value, %sr`, creating an inconsistency where unknown bit positions survived through `ld` but not through `rti`.

**Resolution:** Both paths now use `from_bits_retain`. All bits are preserved, including those in undefined positions. This is consistent with how other registers work and allows future extensibility of the status register without losing bits across exception boundaries.

---

## 8. Z33-V Page Table Entry Bit Layout

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
