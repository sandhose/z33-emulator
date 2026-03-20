# Recommended Changes

This appendix proposes improvements to the Z33 specification and emulator. Items marked as implemented have been incorporated into the current specification and emulator.

## 1. Fuller Flag Semantics

**Status:** Implemented

Arithmetic instructions (`add`, `sub`, `mul`, `neg`) now set O, Z, and N. `div` sets Z and N. Bitwise instructions (`and`, `or`, `xor`, `not`, `shl`, `shr`) now set Z and N. The C (Carry) flag remains reserved for potential future use.

---

## 2. Fix Shift Instruction Operand Order

**Status:** Implemented

`shl src, reg` now computes `reg ← reg << src` and `shr src, reg` computes `reg ← reg >> src`, matching the pattern of all other binary instructions and the reference card description.

---

## 3. Set Flags on Bitwise Instructions

**Status:** Implemented

Bitwise instructions (`and`, `or`, `xor`, `not`, `shl`, `shr`) now set Z and N based on the result.

---

## 4. Consistent `neg` Overflow Handling

**Status:** Implemented

`neg` now uses `overflowing_neg` to detect overflow (when the operand is `i64::MIN`). The O (Overflow) flag is set when this occurs, and Z and N flags are set based on the result.
