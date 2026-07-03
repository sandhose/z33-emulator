//! Small text-scanning helpers shared across the LSP feature modules.
//!
//! These operate directly on the original source text (byte offsets), never on
//! preprocessed spans, so they stay valid for hover, completion, and signature
//! lookups alike.

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
    use super::strip_inline_comment;

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
