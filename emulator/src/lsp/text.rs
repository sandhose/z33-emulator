//! Small text-scanning helpers shared across the LSP feature modules.
//!
//! These operate directly on the original source text (byte offsets), never on
//! preprocessed spans, so they stay valid for hover and completion lookups
//! alike.

use std::ops::Range;

use crate::parser::shared::is_identifier_char;

/// Find the maximal run of characters satisfying `is_part` that contains (or is
/// adjacent to the left of) `offset`.
///
/// Returns the byte range of the run, or `None` if there is no such character
/// on either side of the cursor. Scanning is byte-wise, matching the previous
/// hand-rolled scanners: non-ASCII bytes never satisfy the ASCII predicates
/// used here, so they act as word boundaries.
#[must_use]
pub fn word_at(
    source: &str,
    offset: usize,
    is_part: impl Fn(char) -> bool,
) -> Option<Range<usize>> {
    let bytes = source.as_bytes();
    let mut start = offset;
    while start > 0 && is_part(bytes[start - 1] as char) {
        start -= 1;
    }
    let mut end = offset;
    while end < bytes.len() && is_part(bytes[end] as char) {
        end += 1;
    }
    if start == end {
        None
    } else {
        Some(start..end)
    }
}

/// Skip leading whitespace and any `label:` definitions at the start of a line,
/// returning the byte offset of the first content character (mnemonic /
/// directive / argument).
#[must_use]
pub fn skip_labels(line: &str) -> usize {
    let bytes = line.as_bytes();
    let mut p = 0;
    loop {
        while p < bytes.len() && (bytes[p] == b' ' || bytes[p] == b'\t') {
            p += 1;
        }
        let ident_start = p;
        if p < bytes.len() && (bytes[p].is_ascii_alphabetic() || bytes[p] == b'_') {
            p += 1;
            while p < bytes.len() && is_identifier_char(bytes[p] as char) {
                p += 1;
            }
            while p < bytes.len() && (bytes[p] == b' ' || bytes[p] == b'\t') {
                p += 1;
            }
            if p < bytes.len() && bytes[p] == b':' {
                p += 1;
                continue; // Found a label, look for more
            }
            p = ident_start; // Not a label — this identifier is the content
        }
        break;
    }
    p
}

/// Strip a trailing `// ...` line comment, returning the code portion.
///
/// Z33 line comments start with `//` (see the parser's `inline_comment`). A
/// lone `#` only introduces a **preprocessor directive** at the start of a
/// line, so it must never be treated as a comment marker here.
#[must_use]
pub fn strip_inline_comment(line: &str) -> &str {
    match line.find("//") {
        Some(i) => &line[..i],
        None => line,
    }
}

#[cfg(test)]
mod tests {
    use super::{skip_labels, strip_inline_comment, word_at};

    #[test]
    fn word_at_finds_identifier() {
        let src = "  add %a";
        // Cursor inside "add"
        assert_eq!(word_at(src, 3, |c| c.is_ascii_alphabetic()), Some(2..5));
        // Cursor in whitespace -> nothing
        assert_eq!(word_at(src, 1, |c| c.is_ascii_alphabetic()), None);
    }

    #[test]
    fn skip_labels_skips_definitions() {
        assert_eq!(skip_labels("main: add"), 6); // past "main: "
        assert_eq!(skip_labels("a: b: reset"), 6); // two labels
        assert_eq!(skip_labels("    add"), 4); // leading whitespace only
        assert_eq!(skip_labels("add %a"), 0); // no label
    }

    #[test]
    fn strips_double_slash_comment() {
        assert_eq!(strip_inline_comment("add %a, %b // note"), "add %a, %b ");
    }

    #[test]
    fn keeps_hash_untouched() {
        // `#` is a preprocessor marker, not a comment.
        assert_eq!(strip_inline_comment("#define X 1"), "#define X 1");
    }

    #[test]
    fn no_comment_returns_whole_line() {
        assert_eq!(strip_inline_comment("reset"), "reset");
    }
}
