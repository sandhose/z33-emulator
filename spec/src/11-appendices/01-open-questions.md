# Open Questions

This appendix documents known discrepancies between the reference card, the emulator, and the course materials, as well as non-obvious design decisions.

## 1. Word Size and `.space` Directive

The reference card describes `.space` as reserving "bytes" (`octets` in the French version). However, the Z33 has no byte-level addressing — all memory cells are Word-sized (64-bit). The emulator treats `.space N` as reserving N cells, not N bytes.

This specification documents `.space` as reserving cells. The reference card's use of "bytes" is considered a simplification for familiarity.

---

## 2. `swap` with Memory — Cell Type Preservation

When `swap` operates on a memory location and a register, both the memory read and the register read use `extract_cell()`, which preserves the full Cell type (including Instruction cells). The emulator's `write` method is generic (`T: Into<Cell>`), so `swap` actually preserves full cell types in both directions. This contrasts with `st`, which calls `extract_word()` and only stores a Word value.

In practice, `swap` is typically used with Word values. However, a `swap` involving an Instruction cell would preserve that cell type, while `st` would not.

