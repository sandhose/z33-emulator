# Memory Map

This appendix provides a visual overview of the Z33 memory layout.

## Address Space

```
Address
  0 ┌──────────────────────────────┐
    │                              │
    │   (available for OS/data)    │
    │                              │
100 ├──────────────────────────────┤
    │ 100: Saved %pc               │  ← Exception save area
    │ 101: Saved %sr               │
    │ 102: Exception code          │
103 ├──────────────────────────────┤
    │                              │
    │   (available for OS/data)    │
    │                              │
200 ├──────────────────────────────┤
    │ 200: Exception handler entry │  ← Exception/interrupt vector
    │ 201+: Handler code...        │
    ├──────────────────────────────┤
    │                              │
    │   (available for OS/data)    │
    │                              │
1000├──────────────────────────────┤
    │ 1000: Default program start  │  ← %pc initial value
    │ 1001: ...                    │
    │                              │
    │   Program code and data      │
    │                              │
    │                              │
    ├ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─┤
    │                              │
    │   ↓ Code/data grows up       │
    │                              │
    │                              │
    │   ↑ Stack grows down         │
    │                              │
    ├ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─┤
    │                              │
    │   Stack space                │
    │                              │
9999├──────────────────────────────┤
    │ 9999: First stack cell used  │  ← First push goes here
10000└──────────────────────────────┘  ← %sp initial value (past end)
```

## Reserved Addresses

| Address | Purpose | Access |
|---|---|---|
| 100 | Saved `%pc` on exception | Read/Write |
| 101 | Saved `%sr` on exception | Read/Write |
| 102 | Exception code | Read/Write |
| 200 | Exception handler entry point | Must contain an instruction |
| 1000 | Default program entry point | Convention, not enforced |

## Notes

- The memory has **10,000 cells** (addresses 0–9,999).
- The initial stack pointer is **10,000**, which is one past the last valid address. The first `push` decrements it to 9,999.
- There is **no enforced boundary** between code/data and stack space. Programs and the OS are responsible for avoiding collisions.
- Addresses 0–99 and 103–199 are not reserved by the architecture and may be used freely. However, the OS typically occupies the low memory region.
- The program start address (1000) is a convention used by the emulator when loading programs. It is not architecturally enforced — programs can be loaded at any address by setting `%pc` accordingly.
