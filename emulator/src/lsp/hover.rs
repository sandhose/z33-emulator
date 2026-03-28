use super::document::DocumentState;
use crate::parser::shared::is_identifier_char;

/// Result of a hover lookup.
pub struct HoverResult {
    /// Markdown content to display.
    pub contents: String,
    /// Byte range in the original source that the hover applies to.
    pub span: std::ops::Range<usize>,
}

/// Look up hover documentation at the given byte offset in the original source.
///
/// `analysis` may be `None` — label hover won't work in that case.
pub fn hover(
    analysis: Option<&DocumentState>,
    source: &str,
    byte_offset: usize,
) -> Option<HoverResult> {
    if byte_offset > source.len() {
        return None;
    }

    if let Some(result) = try_register_hover(source, byte_offset) {
        return Some(result);
    }

    // Macros and labels before mnemonics — user-defined names take
    // priority over built-in instruction documentation.
    if let Some(state) = analysis {
        if let Some(result) = try_macro_hover(state, source, byte_offset) {
            return Some(result);
        }

        if let Some(result) = try_label_hover(state, source, byte_offset) {
            return Some(result);
        }
    }

    if let Some(result) = try_mnemonic_hover(source, byte_offset) {
        return Some(result);
    }

    None
}

fn try_register_hover(source: &str, offset: usize) -> Option<HoverResult> {
    let bytes = source.as_bytes();

    // Find the start of a potential register (look for %)
    let mut start = offset;
    while start > 0 && bytes[start - 1].is_ascii_alphabetic() {
        start -= 1;
    }
    if start == 0 || bytes[start - 1] != b'%' {
        // Also check if cursor is right on the %
        if offset < bytes.len() && bytes[offset] == b'%' {
            start = offset;
        } else {
            return None;
        }
    } else {
        start -= 1; // include the %
    }

    // Find the end
    let mut end = offset;
    while end < bytes.len() && bytes[end].is_ascii_alphabetic() {
        end += 1;
    }
    // If we started on %, skip it for the name
    if start < bytes.len() && bytes[start] == b'%' && end > start + 1 {
        // ok
    } else {
        return None;
    }

    let reg_text = &source[start..end];
    let doc = register_doc(reg_text)?;
    Some(HoverResult {
        contents: doc.to_string(),
        span: start..end,
    })
}

fn try_mnemonic_hover(source: &str, offset: usize) -> Option<HoverResult> {
    let bytes = source.as_bytes();

    // Find the word boundaries
    let mut start = offset;
    while start > 0 && (bytes[start - 1].is_ascii_alphabetic() || bytes[start - 1] == b'_') {
        start -= 1;
    }
    let mut end = offset;
    while end < bytes.len() && (bytes[end].is_ascii_alphabetic() || bytes[end] == b'_') {
        end += 1;
    }

    if start == end {
        return None;
    }

    // Check for directive (preceded by '.')
    if start > 0 && bytes[start - 1] == b'.' {
        let word = &source[start..end];
        let doc = directive_doc(word)?;
        return Some(HoverResult {
            contents: doc.to_string(),
            span: (start - 1)..end, // include the dot
        });
    }

    let word = &source[start..end];
    let doc = instruction_doc(&word.to_ascii_lowercase())?;
    Some(HoverResult {
        contents: doc.to_string(),
        span: start..end,
    })
}

fn try_macro_hover(state: &DocumentState, source: &str, offset: usize) -> Option<HoverResult> {
    let bytes = source.as_bytes();

    let mut start = offset;
    while start > 0 && is_identifier_char(bytes[start - 1] as char) {
        start -= 1;
    }
    let mut end = offset;
    while end < bytes.len() && is_identifier_char(bytes[end] as char) {
        end += 1;
    }

    if start == end {
        return None;
    }

    let word = &source[start..end];
    let annotations = state.annotations()?;

    // Find the last #define for this name (later defines override earlier ones)
    let def = annotations
        .definitions
        .iter()
        .rev()
        .find(|d| d.key == word)?;

    let value_display = def.value.as_deref().unwrap_or("(defined without value)");

    Some(HoverResult {
        contents: format!("**{word}** — macro\n\n`#define {word} {value_display}`"),
        span: start..end,
    })
}

fn try_label_hover(state: &DocumentState, source: &str, offset: usize) -> Option<HoverResult> {
    let bytes = source.as_bytes();

    let mut start = offset;
    while start > 0 && is_identifier_char(bytes[start - 1] as char) {
        start -= 1;
    }
    let mut end = offset;
    while end < bytes.len() && is_identifier_char(bytes[end] as char) {
        end += 1;
    }

    if start == end {
        return None;
    }

    let word = &source[start..end];
    let addr = state.labels().get(word)?;

    Some(HoverResult {
        contents: format!("**{word}** — label at address {addr}"),
        span: start..end,
    })
}

fn register_doc(reg: &str) -> Option<&'static str> {
    match reg {
        "%a" => Some(
            "**%a** — General-purpose register\n\n\
             64-bit signed integer register. Used as source or destination \
             for arithmetic, logic, and data transfer instructions.",
        ),
        "%b" => Some(
            "**%b** — General-purpose register\n\n\
             64-bit signed integer register. Used as source or destination \
             for arithmetic, logic, and data transfer instructions.",
        ),
        "%pc" => Some(
            "**%pc** — Program counter\n\n\
             Points to the address of the next instruction to execute. \
             Automatically incremented after each instruction. Modified by \
             jump and call instructions. Read-only in user mode.",
        ),
        "%sp" => Some(
            "**%sp** — Stack pointer\n\n\
             Points to the top of the stack. Starts at 10000 (top of memory) \
             and grows downward. Modified by `push`, `pop`, `call`, and `rtn`.",
        ),
        "%sr" => Some(
            "**%sr** — Status register\n\n\
             Contains processor flags:\n\
             - Bit 0: **C** (Carry)\n\
             - Bit 1: **Z** (Zero)\n\
             - Bit 2: **N** (Negative)\n\
             - Bit 3: **O** (Overflow)\n\
             - Bit 4: **I** (Interrupt enable)\n\
             - Bit 5: **S** (Supervisor mode)",
        ),
        _ => None,
    }
}

#[allow(clippy::too_many_lines)]
fn instruction_doc(mnemonic: &str) -> Option<&'static str> {
    match mnemonic {
        "add" => Some("**add** src, %dst\n\n`%dst ← %dst + src`\n\nSets flags: Z, N, O"),
        "and" => Some("**and** src, %dst\n\n`%dst ← %dst & src`\n\nSets flags: Z"),
        "call" => Some(
            "**call** addr\n\n`push %pc; %pc ← addr`\n\n\
             Pushes the return address onto the stack and jumps to `addr`. \
             Use `rtn` to return.",
        ),
        "cmp" => Some(
            "**cmp** src, %dst\n\n`%dst - src` (result discarded)\n\n\
             Sets flags: Z, N, O. Use before conditional jumps.",
        ),
        "div" => Some("**div** src, %dst\n\n`%dst ← %dst / src`\n\nSets flags: Z, N"),
        "fas" => Some(
            "**fas** [addr], %dst\n\n`%dst ← mem[addr]; mem[addr] ← 1`\n\n\
             Fetch-and-set. Atomic operation for synchronization. \
             Sets flags: Z, N",
        ),
        "in" => Some(
            "**in** [port], %dst\n\n`%dst ← IO[port]`\n\n\
             Read from an I/O controller. Privileged instruction (supervisor mode only).",
        ),
        "jmp" => Some("**jmp** addr\n\n`%pc ← addr`\n\nUnconditional jump."),
        "jeq" => Some("**jeq** addr\n\n`if Z then %pc ← addr`\n\nJump if equal (zero flag set)."),
        "jne" => Some("**jne** addr\n\n`if ¬Z then %pc ← addr`\n\nJump if not equal."),
        "jle" => Some(
            "**jle** addr\n\n`if Z∨N then %pc ← addr`\n\nJump if less or equal (signed).",
        ),
        "jlt" => Some("**jlt** addr\n\n`if N then %pc ← addr`\n\nJump if strictly less (signed)."),
        "jge" => Some(
            "**jge** addr\n\n`if ¬N then %pc ← addr`\n\nJump if greater or equal (signed).",
        ),
        "jgt" => Some(
            "**jgt** addr\n\n`if ¬Z∧¬N then %pc ← addr`\n\nJump if strictly greater (signed).",
        ),
        "ld" => Some("**ld** src, %dst\n\n`%dst ← src`\n\nLoad a value into a register."),
        "mul" => Some("**mul** src, %dst\n\n`%dst ← %dst × src`\n\nSets flags: Z, N, O"),
        "neg" => Some("**neg** %reg\n\n`%reg ← −%reg`\n\nTwo's complement negation. Sets flags: Z, N"),
        "nop" => Some("**nop**\n\nNo operation. Does nothing for one cycle."),
        "not" => Some("**not** %reg\n\n`%reg ← ~%reg`\n\nBitwise NOT. Sets flags: Z"),
        "or" => Some("**or** src, %dst\n\n`%dst ← %dst | src`\n\nSets flags: Z"),
        "out" => Some(
            "**out** src, [port]\n\n`IO[port] ← src`\n\n\
             Write to an I/O controller. Privileged instruction (supervisor mode only).",
        ),
        "pop" => Some("**pop** %reg\n\n`%reg ← mem[%sp]; %sp ← %sp + 1`\n\nPop a value from the stack."),
        "push" => Some(
            "**push** src\n\n`%sp ← %sp − 1; mem[%sp] ← src`\n\nPush a value onto the stack.",
        ),
        "reset" => Some("**reset**\n\nReset the computer. All registers and memory are cleared."),
        "rti" => Some(
            "**rti**\n\n`%pc ← mem[100]; %sr ← mem[101]`\n\n\
             Return from interrupt. Restores PC and SR from fixed addresses.",
        ),
        "rtn" => Some("**rtn**\n\n`pop %pc`\n\nReturn from a `call`. Pops the return address from the stack."),
        "shl" => Some("**shl** src, %dst\n\n`%dst ← %dst << src`\n\nBitwise shift left. Sets flags: Z, N, C"),
        "shr" => Some("**shr** src, %dst\n\n`%dst ← %dst >> src`\n\nBitwise shift right (arithmetic). Sets flags: Z, N, C"),
        "st" => Some("**st** %src, [addr]\n\n`mem[addr] ← %src`\n\nStore a register value to memory."),
        "sub" => Some("**sub** src, %dst\n\n`%dst ← %dst − src`\n\nSets flags: Z, N, O"),
        "swap" => Some(
            "**swap** src, %dst\n\n`src ↔ %dst`\n\n\
             Swap the values of a memory location (or register) and a register.",
        ),
        "trap" => Some(
            "**trap**\n\nTrigger a trap exception.\n\n\
             Saves %pc to address 100 and %sr to address 101, \
             then jumps to the interrupt handler at address 200.",
        ),
        "xor" => Some("**xor** src, %dst\n\n`%dst ← %dst ^ src`\n\nSets flags: Z"),
        _ => None,
    }
}

fn directive_doc(name: &str) -> Option<&'static str> {
    match name {
        "word" => Some("**.word** expr\n\nStore a word (64-bit integer) value at the current address."),
        "space" => Some("**.space** n\n\nReserve `n` memory cells (initialized to empty)."),
        "string" => Some(
            "**.string** \"text\"\n\n\
             Store a null-terminated string. Each character occupies one memory cell.",
        ),
        "addr" => Some("**.addr** n\n\nSet the current assembly address to `n`. Subsequent instructions are placed starting at this address."),
        _ => None,
    }
}
