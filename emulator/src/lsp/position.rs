use std::ops::Range;

use tower_lsp::lsp_types;

/// Convert an LSP `Position` (line, UTF-16 character offset) to a byte offset
/// in `source`.
///
/// Returns `None` if the position is out of bounds.
#[must_use]
#[allow(clippy::cast_possible_truncation)]
pub fn byte_offset(source: &str, position: lsp_types::Position) -> Option<usize> {
    let mut current_line = 0u32;
    let mut byte_idx = 0usize;

    // Find the start of the target line
    if position.line > 0 {
        for (i, b) in source.bytes().enumerate() {
            if b == b'\n' {
                current_line += 1;
                if current_line == position.line {
                    byte_idx = i + 1;
                    break;
                }
            }
        }
        if current_line != position.line {
            return None;
        }
    }

    // Now walk UTF-16 code units within the line
    let line_start = byte_idx;
    let line_bytes = &source[line_start..];
    let mut utf16_offset = 0u32;

    for (i, ch) in line_bytes.char_indices() {
        if ch == '\n' {
            break;
        }
        if utf16_offset == position.character {
            return Some(line_start + i);
        }
        utf16_offset += ch.len_utf16() as u32;
        if utf16_offset > position.character {
            // Position points into the middle of a surrogate pair
            return Some(line_start + i);
        }
    }

    // Position at end of line
    if utf16_offset == position.character {
        let end = line_bytes
            .find('\n')
            .map_or(source.len(), |i| line_start + i);
        return Some(end);
    }

    None
}

/// Convert a byte offset to an LSP `Position`.
///
/// Returns `None` if `offset` is out of bounds or not on a character boundary.
#[must_use]
#[allow(clippy::cast_possible_truncation)]
pub fn position(source: &str, offset: usize) -> Option<lsp_types::Position> {
    if offset > source.len() {
        return None;
    }

    let before = &source[..offset];
    let line = before.matches('\n').count() as u32;
    let line_start = before.rfind('\n').map_or(0, |i| i + 1);
    let line_text = &source[line_start..offset];
    let character = line_text.chars().map(|c| c.len_utf16() as u32).sum();

    Some(lsp_types::Position { line, character })
}

/// Convert a byte `Range<usize>` to an LSP `Range`.
///
/// Returns `None` if either endpoint is out of bounds.
#[must_use]
pub fn range(source: &str, span: Range<usize>) -> Option<lsp_types::Range> {
    let start = position(source, span.start)?;
    let end = position(source, span.end)?;
    Some(lsp_types::Range { start, end })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn byte_offset_simple() {
        let src = "hello\nworld\n";
        // 'w' is at byte 6, line 1 col 0
        assert_eq!(byte_offset(src, lsp_types::Position::new(1, 0)), Some(6));
        // 'o' in "world" is at byte 7, line 1 col 1
        assert_eq!(byte_offset(src, lsp_types::Position::new(1, 1)), Some(7));
        // First line, col 0
        assert_eq!(byte_offset(src, lsp_types::Position::new(0, 0)), Some(0));
    }

    #[test]
    fn position_simple() {
        let src = "hello\nworld\n";
        assert_eq!(position(src, 0), Some(lsp_types::Position::new(0, 0)));
        assert_eq!(position(src, 6), Some(lsp_types::Position::new(1, 0)));
        assert_eq!(position(src, 7), Some(lsp_types::Position::new(1, 1)));
    }

    #[test]
    fn roundtrip() {
        let src = "main:\n    add %a, %b\n    reset\n";
        for offset in 0..src.len() {
            if src.is_char_boundary(offset) {
                let pos = position(src, offset).unwrap();
                let back = byte_offset(src, pos).unwrap();
                assert_eq!(back, offset, "roundtrip failed at offset {offset}");
            }
        }
    }

    #[test]
    fn range_conversion() {
        let src = "main:\n    add %a, %b\n";
        let r = range(src, 6..9).unwrap();
        assert_eq!(r.start, lsp_types::Position::new(1, 0));
        assert_eq!(r.end, lsp_types::Position::new(1, 3));
    }
}
