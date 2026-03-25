# Execution Model

## Fetch-Decode-Execute Cycle

The Z33 executes instructions in a standard fetch-decode-execute loop:

r[exec.fetch]
1. **Fetch:** Read the cell at the address pointed to by `%pc`.

r[exec.pc-increment]
2. **Increment PC:** `%pc ← %pc + 1` (before execution).

r[exec.decode]
3. **Decode:** Verify the cell contains an Instruction. If it contains a Word or is Empty, raise an *invalid instruction* exception.

4. **Execute:** Perform the operation defined by the instruction.

r[exec.cycles]
5. **Count cycles:** The instruction's cycle cost is `1 (base) + sum(operand costs)`, added to the total cycle counter.

The program counter is incremented **after fetch but before execute**. This means:
- When a `call` instruction saves `%pc`, it saves the address of the instruction *after* the `call` (the correct return address).
- When an exception occurs during execution, the saved `%pc` (at address 100) points to the instruction *after* the faulting instruction.

### Exception Recovery

r[exec.cycles.exception]
Exception recovery adds 1 cycle. If an instruction raises a processor exception, the processor attempts to recover by invoking the exception handler. If recovery fails (e.g., because no handler is installed at address 200), the exception propagates as a fatal error.

If the `reset` instruction is encountered, the processor halts gracefully instead of entering the exception handler.

## Cycle Counting

Each instruction has an associated cost in CPU cycles:

```
total_cost = 1 (base) + sum(operand_costs)
```

The operand cost depends on the addressing mode used:

| Mode | Cost |
|---|---|
| Immediate | 0 |
| Register | 0 |
| Direct | 1 |
| Indirect | 1 |
| Indexed | 1 |

r[exec.cycles.debugreg]
The only exception is `debugreg`, which costs 0 cycles.

## Privilege Levels

The Z33 has two privilege levels, determined by the S (Supervisor) bit in `%sr`:

| S bit | Mode | Description |
|---|---|---|
| 1 | Supervisor | Full access to all instructions and registers |
| 0 | User | Restricted access |

r[exec.privilege.supervisor]
When S=1, the processor is in supervisor mode with full access to all instructions and registers.

### Privileged Operations

The following operations require supervisor mode (S=1):

| Operation | Description |
|---|---|
| Writing to `%sr` | Any instruction that stores a result in `%sr` (including `ld`, `add`, `pop`, etc.) |
| `rti` | Return from interrupt/exception |
| `in` | Read from I/O port |
| `out` | Write to I/O port |

r[exec.privilege.user]
Attempting a privileged operation in user mode (S=0) raises a *privileged instruction* exception (code 3).

### Mode Transitions

r[exec.privilege.transition-up]
**User → Supervisor:** Occurs automatically when an exception or interrupt is handled (the handler sets S=1).

r[exec.privilege.transition-down]
**Supervisor → User:** The exception handler modifies the saved `%sr` at address 101 to clear the S bit, then executes `rti` to restore it.

## Processor State

The complete processor state consists of:

- Five registers: `%a`, `%b`, `%pc`, `%sp`, `%sr`
- 10,000 memory cells (indices 0–9,999)
- A cycle counter (not architecturally visible, used for performance measurement)
