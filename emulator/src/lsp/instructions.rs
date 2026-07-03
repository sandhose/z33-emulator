//! Single source of truth for the LSP's per-instruction and per-directive
//! metadata.
//!
//! Hover documentation, completion details, and the argument-type tables used
//! by completion all read from the exhaustive [`meta`] / [`directive_meta`]
//! matches below. Because they are `match`es (no wildcard arm), adding a new
//! [`InstructionKind`] / [`DirectiveKind`] variant is a compile error until its
//! metadata is filled in.

use crate::parser::value::{DirectiveKind, InstructionKind};

/// What kind of argument an instruction expects at a given position.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum ArgType {
    /// Only a register
    Reg,
    /// Immediate or register
    ImmReg,
    /// Any addressing mode
    ImmRegDirIndIdx,
    /// Memory addressing only (direct, indirect, indexed)
    DirIndIdx,
    /// Register or memory addressing
    RegDirIndIdx,
}

/// Metadata for a single instruction mnemonic.
pub(super) struct InstrMeta {
    /// Short one-line description (completion `detail`).
    pub summary: &'static str,
    /// Argument signature shown next to the completion label, e.g.
    /// `"src, %dst"`. Empty for 0-argument instructions.
    pub signature: &'static str,
    /// Full hover documentation (Markdown).
    pub hover: &'static str,
    /// Expected argument types, one entry per positional argument.
    pub args: &'static [ArgType],
}

/// Every instruction kind that can appear in source — i.e. all variants except
/// the [`InstructionKind::Error`] parse-error placeholder. Used to build the
/// static completion list.
pub(super) const INSTRUCTION_KINDS: &[InstructionKind] = {
    use InstructionKind as K;
    &[
        K::Add,
        K::And,
        K::Call,
        K::Cmp,
        K::Div,
        K::Fas,
        K::In,
        K::Jmp,
        K::Jeq,
        K::Jne,
        K::Jle,
        K::Jlt,
        K::Jge,
        K::Jgt,
        K::Ld,
        K::Mul,
        K::Neg,
        K::Nop,
        K::Not,
        K::Or,
        K::Out,
        K::Pop,
        K::Push,
        K::Reset,
        K::Rti,
        K::Rtn,
        K::Shl,
        K::Shr,
        K::St,
        K::Sub,
        K::Swap,
        K::Trap,
        K::Xor,
    ]
};

use ArgType::{DirIndIdx, ImmReg, ImmRegDirIndIdx, Reg, RegDirIndIdx};

/// Metadata for an instruction kind. Exhaustive `match`: adding a variant is a
/// compile error.
#[allow(clippy::too_many_lines)]
pub(super) fn meta(kind: InstructionKind) -> InstrMeta {
    use InstructionKind as K;
    match kind {
        K::Add => InstrMeta {
            summary: "Add a value to a register",
            signature: "src, %dst",
            hover: "**add** src, %dst\n\n`%dst ← %dst + src`\n\nSets flags: Z, N, O",
            args: &[ImmRegDirIndIdx, Reg],
        },
        K::And => InstrMeta {
            summary: "Bitwise AND with a register",
            signature: "src, %dst",
            hover: "**and** src, %dst\n\n`%dst ← %dst & src`\n\nSets flags: Z",
            args: &[ImmRegDirIndIdx, Reg],
        },
        K::Call => InstrMeta {
            summary: "Push PC and jump to address",
            signature: "addr",
            hover: "**call** addr\n\n`push %pc; %pc ← addr`\n\n\
                 Pushes the return address onto the stack and jumps to `addr`. \
                 Use `rtn` to return.",
            args: &[ImmRegDirIndIdx],
        },
        K::Cmp => InstrMeta {
            summary: "Compare a value with a register",
            signature: "src, %dst",
            hover: "**cmp** src, %dst\n\n`%dst - src` (result discarded)\n\n\
                 Sets flags: Z, N, O. Use before conditional jumps.",
            args: &[ImmRegDirIndIdx, Reg],
        },
        K::Div => InstrMeta {
            summary: "Divide a register by a value",
            signature: "src, %dst",
            hover: "**div** src, %dst\n\n`%dst ← %dst / src`\n\nSets flags: Z, N",
            args: &[ImmRegDirIndIdx, Reg],
        },
        K::Fas => InstrMeta {
            summary: "Fetch-and-set (atomic)",
            signature: "[addr], %dst",
            hover: "**fas** [addr], %dst\n\n`%dst ← mem[addr]; mem[addr] ← 1`\n\n\
                 Fetch-and-set. Atomic operation for synchronization. \
                 Sets flags: Z, N",
            args: &[DirIndIdx, Reg],
        },
        K::In => InstrMeta {
            summary: "Read from I/O controller",
            signature: "[port], %dst",
            hover: "**in** [port], %dst\n\n`%dst ← IO[port]`\n\n\
                 Read from an I/O controller. Privileged instruction (supervisor mode only).",
            args: &[DirIndIdx, Reg],
        },
        K::Jmp => InstrMeta {
            summary: "Unconditional jump",
            signature: "addr",
            hover: "**jmp** addr\n\n`%pc ← addr`\n\nUnconditional jump.",
            args: &[ImmRegDirIndIdx],
        },
        K::Jeq => InstrMeta {
            summary: "Jump if equal (zero flag set)",
            signature: "addr",
            hover: "**jeq** addr\n\n`if Z then %pc ← addr`\n\nJump if equal (zero flag set).",
            args: &[ImmRegDirIndIdx],
        },
        K::Jne => InstrMeta {
            summary: "Jump if not equal",
            signature: "addr",
            hover: "**jne** addr\n\n`if ¬Z then %pc ← addr`\n\nJump if not equal.",
            args: &[ImmRegDirIndIdx],
        },
        K::Jle => InstrMeta {
            summary: "Jump if less or equal",
            signature: "addr",
            hover: "**jle** addr\n\n`if Z∨N then %pc ← addr`\n\nJump if less or equal (signed).",
            args: &[ImmRegDirIndIdx],
        },
        K::Jlt => InstrMeta {
            summary: "Jump if strictly less",
            signature: "addr",
            hover: "**jlt** addr\n\n`if N then %pc ← addr`\n\nJump if strictly less (signed).",
            args: &[ImmRegDirIndIdx],
        },
        K::Jge => InstrMeta {
            summary: "Jump if greater or equal",
            signature: "addr",
            hover: "**jge** addr\n\n`if ¬N then %pc ← addr`\n\nJump if greater or equal (signed).",
            args: &[ImmRegDirIndIdx],
        },
        K::Jgt => InstrMeta {
            summary: "Jump if strictly greater",
            signature: "addr",
            hover: "**jgt** addr\n\n`if ¬Z∧¬N then %pc ← addr`\n\nJump if strictly greater (signed).",
            args: &[ImmRegDirIndIdx],
        },
        K::Ld => InstrMeta {
            summary: "Load a value into a register",
            signature: "src, %dst",
            hover: "**ld** src, %dst\n\n`%dst ← src`\n\nLoad a value into a register.",
            args: &[ImmRegDirIndIdx, Reg],
        },
        K::Mul => InstrMeta {
            summary: "Multiply a register by a value",
            signature: "src, %dst",
            hover: "**mul** src, %dst\n\n`%dst ← %dst × src`\n\nSets flags: Z, N, O",
            args: &[ImmRegDirIndIdx, Reg],
        },
        K::Neg => InstrMeta {
            summary: "Negate a register",
            signature: "%reg",
            hover: "**neg** %reg\n\n`%reg ← −%reg`\n\nTwo's complement negation. Sets flags: Z, N",
            args: &[Reg],
        },
        K::Nop => InstrMeta {
            summary: "No operation",
            signature: "",
            hover: "**nop**\n\nNo operation. Does nothing for one cycle.",
            args: &[],
        },
        K::Not => InstrMeta {
            summary: "Bitwise NOT of a register",
            signature: "%reg",
            hover: "**not** %reg\n\n`%reg ← ~%reg`\n\nBitwise NOT. Sets flags: Z",
            args: &[Reg],
        },
        K::Or => InstrMeta {
            summary: "Bitwise OR with a register",
            signature: "src, %dst",
            hover: "**or** src, %dst\n\n`%dst ← %dst | src`\n\nSets flags: Z",
            args: &[ImmRegDirIndIdx, Reg],
        },
        K::Out => InstrMeta {
            summary: "Write to I/O controller",
            signature: "src, [port]",
            hover: "**out** src, [port]\n\n`IO[port] ← src`\n\n\
                 Write to an I/O controller. Privileged instruction (supervisor mode only).",
            args: &[ImmReg, DirIndIdx],
        },
        K::Pop => InstrMeta {
            summary: "Pop value from stack",
            signature: "%reg",
            hover: "**pop** %reg\n\n`%reg ← mem[%sp]; %sp ← %sp + 1`\n\nPop a value from the stack.",
            args: &[Reg],
        },
        K::Push => InstrMeta {
            summary: "Push value onto stack",
            signature: "src",
            hover: "**push** src\n\n`%sp ← %sp − 1; mem[%sp] ← src`\n\nPush a value onto the stack.",
            args: &[ImmReg],
        },
        K::Reset => InstrMeta {
            summary: "Reset the computer",
            signature: "",
            hover: "**reset**\n\nReset the computer. All registers and memory are cleared.",
            args: &[],
        },
        K::Rti => InstrMeta {
            summary: "Return from interrupt",
            signature: "",
            hover: "**rti**\n\n`%pc ← mem[100]; %sr ← mem[101]`\n\n\
                 Return from interrupt. Restores PC and SR from fixed addresses.",
            args: &[],
        },
        K::Rtn => InstrMeta {
            summary: "Return from call",
            signature: "",
            hover: "**rtn**\n\n`pop %pc`\n\nReturn from a `call`. Pops the return address from the stack.",
            args: &[],
        },
        K::Shl => InstrMeta {
            summary: "Shift left",
            signature: "src, %dst",
            hover: "**shl** src, %dst\n\n`%dst ← %dst << src`\n\nBitwise shift left. Sets flags: Z, N, C",
            args: &[ImmRegDirIndIdx, Reg],
        },
        K::Shr => InstrMeta {
            summary: "Shift right",
            signature: "src, %dst",
            hover: "**shr** src, %dst\n\n`%dst ← %dst >> src`\n\nBitwise shift right (arithmetic). Sets flags: Z, N, C",
            args: &[ImmRegDirIndIdx, Reg],
        },
        K::St => InstrMeta {
            summary: "Store register to memory",
            signature: "%src, [addr]",
            hover: "**st** %src, [addr]\n\n`mem[addr] ← %src`\n\nStore a register value to memory.",
            args: &[Reg, DirIndIdx],
        },
        K::Sub => InstrMeta {
            summary: "Subtract a value from a register",
            signature: "src, %dst",
            hover: "**sub** src, %dst\n\n`%dst ← %dst − src`\n\nSets flags: Z, N, O",
            args: &[ImmRegDirIndIdx, Reg],
        },
        K::Swap => InstrMeta {
            summary: "Swap a value and a register",
            signature: "src, %dst",
            hover: "**swap** src, %dst\n\n`src ↔ %dst`\n\n\
                 Swap the values of a memory location (or register) and a register.",
            args: &[RegDirIndIdx, Reg],
        },
        K::Trap => InstrMeta {
            summary: "Trigger trap exception",
            signature: "",
            hover: "**trap**\n\nTrigger a trap exception.\n\n\
                 Saves %pc to address 100 and %sr to address 101, \
                 then jumps to the interrupt handler at address 200.",
            args: &[],
        },
        K::Xor => InstrMeta {
            summary: "Bitwise XOR with a register",
            signature: "src, %dst",
            hover: "**xor** src, %dst\n\n`%dst ← %dst ^ src`\n\nSets flags: Z",
            args: &[ImmRegDirIndIdx, Reg],
        },
        // Parse-error placeholder: never surfaced to the user.
        K::Error => InstrMeta {
            summary: "",
            signature: "",
            hover: "",
            args: &[],
        },
    }
}

/// Metadata for a single directive.
pub(super) struct DirectiveMeta {
    /// Short one-line description (completion label description).
    pub summary: &'static str,
    /// Full hover documentation (Markdown).
    pub hover: &'static str,
}

/// Every directive kind, in completion display order.
pub(super) const DIRECTIVE_KINDS: &[DirectiveKind] = {
    use DirectiveKind as K;
    &[K::Addr, K::Space, K::String, K::Word]
};

/// Metadata for a directive kind. Exhaustive `match`: adding a variant is a
/// compile error.
pub(super) fn directive_meta(kind: DirectiveKind) -> DirectiveMeta {
    use DirectiveKind as K;
    match kind {
        K::Addr => DirectiveMeta {
            summary: "Set current address",
            hover: "**.addr** n\n\nSet the current assembly address to `n`. Subsequent instructions are placed starting at this address.",
        },
        K::Space => DirectiveMeta {
            summary: "Reserve memory cells",
            hover: "**.space** n\n\nReserve `n` memory cells (initialized to empty).",
        },
        K::String => DirectiveMeta {
            summary: "Store a string literal",
            hover: "**.string** \"text\"\n\n\
                 Store a null-terminated string. Each character occupies one memory cell.",
        },
        K::Word => DirectiveMeta {
            summary: "Store a word value",
            hover: "**.word** expr\n\nStore a word (64-bit integer) value at the current address.",
        },
    }
}

#[cfg(test)]
mod tests {
    use super::{directive_meta, meta, DIRECTIVE_KINDS, INSTRUCTION_KINDS};
    use crate::parser::value::{DirectiveKind, InstructionKind};

    #[test]
    fn every_instruction_kind_is_populated_and_roundtrips() {
        for &kind in INSTRUCTION_KINDS {
            let m = meta(kind);
            assert!(!m.summary.is_empty(), "empty summary for {kind}");
            assert!(!m.hover.is_empty(), "empty hover for {kind}");
            // Display / FromStr round-trip through the derived parse_display impls.
            assert_eq!(
                format!("{kind}").parse::<InstructionKind>().ok(),
                Some(kind),
                "round-trip failed for {kind}",
            );
        }
        // Lock the list length so a variant added to `meta` (a compile error
        // until handled) but forgotten here is caught.
        assert_eq!(INSTRUCTION_KINDS.len(), 33);
    }

    #[test]
    fn error_kind_does_not_parse_from_a_mnemonic() {
        // `Error` displays as `<error>`, so a real mnemonic never resolves to it.
        assert!("error".parse::<InstructionKind>().is_err());
        assert_eq!(
            "<error>".parse::<InstructionKind>().ok(),
            Some(InstructionKind::Error)
        );
    }

    #[test]
    fn every_directive_kind_is_populated_and_roundtrips() {
        for &kind in DIRECTIVE_KINDS {
            let m = directive_meta(kind);
            assert!(!m.summary.is_empty(), "empty summary for {kind}");
            assert!(!m.hover.is_empty(), "empty hover for {kind}");
            assert_eq!(
                format!("{kind}").parse::<DirectiveKind>().ok(),
                Some(kind),
                "round-trip failed for {kind}",
            );
        }
        assert_eq!(DIRECTIVE_KINDS.len(), 4);
    }
}
