use tower_lsp::lsp_types::{DocumentSymbol, SymbolKind};

use super::document::DocumentState;
use super::position;
use super::references::OccurrenceKind;

/// Produce document symbols (outline) for the file — labels and macros.
pub fn document_symbols(state: &DocumentState) -> Vec<DocumentSymbol> {
    let source = state.source();
    let mut symbols = Vec::new();

    // Labels — use occurrence map for definition spans
    let mut label_defs: Vec<(&str, u32, std::ops::Range<usize>)> = state
        .labels()
        .iter()
        .filter_map(|(name, addr)| {
            let def = state
                .occurrences_of(name)
                .find(|o| o.kind == OccurrenceKind::Definition)?;
            Some((name.as_str(), *addr, def.span.clone()))
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

    symbols.sort_by_key(|s| (s.range.start.line, s.range.start.character));

    symbols
}
