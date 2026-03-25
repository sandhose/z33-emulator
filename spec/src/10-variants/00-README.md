# Architecture Variants

*This chapter is informative. These variants are described in the reference card and course materials for teaching memory management concepts. They are not implemented in the current emulator.*

The base Z33 architecture has no memory protection or address translation — all programs share the same flat address space. Three variants add progressively more sophisticated memory management:

| Variant | Name | Mechanism | Used in Memory? |
|---|---|---|---|
| **Z33-M** | Base/Limit | Single base/limit register pair | Supervisor: no, User: yes |
| **Z33-S** | Segmented | 10 segment descriptor pairs | Supervisor: no, User: yes |
| **Z33-V** | Paging | Page table with per-page flags | Both supervisor and user |

All variants share the same instruction set — they differ only in how memory addresses are translated and protected.
