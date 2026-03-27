use tower_lsp::lsp_types::{DocumentSymbol, SymbolKind};

use super::document::DocumentState;
use super::position;

/// Produce document symbols (outline) for the file — one entry per label.
pub fn document_symbols(state: &DocumentState) -> Vec<DocumentSymbol> {
    let source = state.source();

    state
        .labels()
        .iter()
        .filter_map(|(name, addr)| {
            // Find the label definition in the original source (text-based)
            let span = find_label_definition(source, name)?;
            let range = position::range(source, span.clone())?;

            // The line containing the label (from label start to end of line)
            let line_start = span.start;
            let line_end = source[span.end..]
                .find('\n')
                .map_or(source.len(), |i| span.end + i);
            let full_range = position::range(source, line_start..line_end)?;

            #[allow(deprecated)] // `deprecated` field is required but deprecated
            Some(DocumentSymbol {
                name: name.clone(),
                detail: Some(format!("address {addr}")),
                kind: SymbolKind::FUNCTION,
                range: full_range,
                selection_range: range,
                children: None,
                tags: None,
                deprecated: None,
            })
        })
        .collect()
}

/// Find the byte range of a label definition in the source (the identifier
/// before `:`).
fn find_label_definition(source: &str, label: &str) -> Option<std::ops::Range<usize>> {
    let bytes = source.as_bytes();
    let label_len = label.len();
    let mut start = 0;

    while let Some(rel) = source[start..].find(label) {
        let abs = start + rel;
        let end = abs + label_len;
        start = end;

        // Check word boundaries
        if abs > 0 && (bytes[abs - 1].is_ascii_alphanumeric() || bytes[abs - 1] == b'_') {
            continue;
        }
        if end < bytes.len() && (bytes[end].is_ascii_alphanumeric() || bytes[end] == b'_') {
            continue;
        }

        // Must be followed by optional whitespace then ':'
        let after = &source[end..];
        let trimmed = after.trim_start_matches([' ', '\t']);
        if trimmed.starts_with(':') {
            return Some(abs..end);
        }
    }

    None
}
