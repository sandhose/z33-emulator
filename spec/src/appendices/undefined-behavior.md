# Undefined Behavior

This appendix catalogs behaviors that are not fully specified or produce implementation-defined results.

## Arithmetic Edge Cases

### Negation of Minimum Integer

`neg %a` when `%a` contains `i64::MIN` (-2^63 = -9,223,372,036,854,775,808):

The two's complement negation of the minimum value overflows back to itself. The emulator uses `overflowing_neg` to detect this case.

**Current behavior:** `neg` of `i64::MIN` produces `i64::MIN`, sets the O (Overflow) flag, sets the N (Negative) flag, and clears the Z (Zero) flag.

### Division Overflow

`div` when dividing `i64::MIN` by -1 would overflow. The emulator uses `checked_div`, which returns `None` for this case, resulting in a *division by zero* exception being raised.

**Current behavior:** `i64::MIN / -1` raises a division by zero exception.

## Memory Access Edge Cases

### Stack Overflow

If `push` is executed when `%sp` = 0, then `%sp` is decremented (wrapping to `u32::MAX` = 4,294,967,295), and the subsequent memory write at that address will raise an *invalid memory access* exception.

**Current behavior:** Exception raised on the memory write, but `%sp` has already been decremented.

### Stack Underflow

If `pop` is executed when `%sp` = 10,000 (the initial value), the memory read at address 10,000 will raise an *invalid memory access* exception.

**Current behavior:** Exception raised on the memory read. `%sp` is not modified (the read happens before the increment).

### Out-of-Bounds Program Counter

If `%pc` points to an address ≥ 10,000, the fetch phase will attempt to read from an invalid address, raising an *invalid memory access* exception.

## Instruction Edge Cases

### Executing a Non-Instruction Cell

If `%pc` points to a cell containing a Word or an Empty cell, the decode phase raises an *invalid instruction* exception.

### Self-Modifying Code

It is possible for an instruction to modify the memory cell containing itself or a nearby instruction (e.g., via `st`). The emulator clones the instruction before executing it, so self-modification during execution is safe — the currently executing instruction is not affected. However, subsequent fetches from modified cells will see the new values.

### Writing to Special Addresses

Writing to addresses 100–102 (the exception save area) or address 200 (the exception handler entry point) is permitted and will affect subsequent exception handling behavior. No warning or exception is raised.

### Shift Amounts

`shl` and `shr` require the shift amount (taken from the source operand) to be convertible to a `u32` and within the valid range for the word size. Invalid shift amounts raise an *invalid instruction* exception:

- Negative shift amounts (the source value is negative)
- Shift amounts ≥ 64 (exceeds the bit width of a Word)

## Register Edge Cases

### Writing to `%pc` or `%sp` with Invalid Values

If an instruction stores a value in `%pc` or `%sp` that cannot be represented as a `u32` (e.g., a negative number or a value > 2^32 - 1), an error is raised. This is distinct from an exception — it is a fatal processor error.

### Loading an Instruction into `%pc` or `%sp`

If `ld` loads a cell containing an Instruction into `%pc` or `%sp`, the attempt to convert the Instruction to an Address will fail with a cell type error.

## Control Flow Edge Cases

### Fatal Errors in `rtn` and `rti`

The `rtn` instruction pops a value from the stack and loads it into `%pc`. If the popped cell is not convertible to an Address (e.g., it is an Instruction cell), a `CellError` is raised. This is a **fatal processor error**, not a recoverable exception — it bypasses the exception handler entirely.

Similarly, `rti` reads `%pc` from address 100 and `%sr` from address 101. If either address contains a cell that cannot be converted to the expected type, a fatal error occurs.

### Partial State Modification on Error

Some instructions modify state before performing operations that may fail:

- **`call`:** Decrements `%sp` and pushes the return address onto the stack *before* resolving the jump target. If the target address resolution fails (e.g., invalid memory access in indirect mode), the stack has already been modified.
- **`rti`:** Restores `%pc` from address 100 *before* restoring `%sr` from address 101. If the `%sr` restoration fails, `%pc` has already been modified but `%sr` retains its pre-`rti` value.

## Addressing Mode Edge Cases

### Indexed Mode with Invalid Register Values

The indexed addressing mode computes an effective address as `memory[address] + register_value`. Several edge cases exist:

- **Negative effective address:** If the register value is negative and its absolute value exceeds the base address, the effective address computation may wrap or produce an invalid result.
- **Integer overflow:** The addition of the base address (i64 from memory) and the register value (i64) can overflow. The emulator uses standard i64 addition, which wraps on overflow in release mode.
- **Instruction cell in register:** If the register contains an Instruction cell rather than a Word, `extract_word()` will fail when trying to use it as an offset.

## Empty Cells

Reading a Word from an Empty cell yields 0. This means:
- Uninitialized memory reads as 0
- An uninitialized register (`%a` or `%b`) has value 0 when used in arithmetic
- Executing an Empty cell raises an *invalid instruction* exception (Empty is not an Instruction)
