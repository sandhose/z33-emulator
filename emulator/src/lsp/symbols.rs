use tower_lsp::lsp_types::{DocumentSymbol, SymbolKind};

use super::document::DocumentState;
use super::position;

/// Produce document symbols (outline) for the file — labels and macros.
pub fn document_symbols(state: &DocumentState) -> Vec<DocumentSymbol> {
    let source = state.source();
    let mut symbols = Vec::new();

    // Labels
    let mut label_defs: Vec<(&str, u32, std::ops::Range<usize>)> = state
        .labels()
        .iter()
        .filter_map(|(name, addr)| {
            let span = find_label_definition(source, name)?;
            Some((name.as_str(), *addr, span))
        })
        .collect();

    label_defs.sort_by_key(|(_, _, span)| span.start);

    for (i, (name, addr, def_span)) in label_defs.iter().enumerate() {
        let Some(selection_range) = position::range(source, def_span.clone()) else {
            continue;
        };

        let body_end = if let Some((_, _, next_span)) = label_defs.get(i + 1) {
            let next_line_start = source[..next_span.start]
                .rfind('\n')
                .map_or(next_span.start, |nl| nl + 1);
            if next_line_start > def_span.start {
                next_line_start
            } else {
                next_span.start
            }
        } else {
            source.len()
        };

        let body = &source[def_span.start..body_end];
        let trimmed_len = body.trim_end().len();
        let body_end = def_span.start + trimmed_len;

        let Some(range) = position::range(source, def_span.start..body_end) else {
            continue;
        };

        #[allow(deprecated)]
        symbols.push(DocumentSymbol {
            name: (*name).to_string(),
            detail: Some(format!("address {addr}")),
            kind: SymbolKind::FUNCTION,
            range,
            selection_range,
            children: None,
            tags: None,
            deprecated: None,
        });
    }

    // Macros (#define)
    if let Some(annotations) = state.annotations() {
        for def in &annotations.definitions {
            let Some(range) = position::range(source, def.span.clone()) else {
                continue;
            };

            #[allow(deprecated)]
            symbols.push(DocumentSymbol {
                name: def.key.clone(),
                detail: def.value.as_ref().map(|v| format!("= {v}")),
                kind: SymbolKind::CONSTANT,
                range,
                selection_range: range,
                children: None,
                tags: None,
                deprecated: None,
            });
        }
    }

    // Sort by position in file
    symbols.sort_by_key(|s| (s.range.start.line, s.range.start.character));

    symbols
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
