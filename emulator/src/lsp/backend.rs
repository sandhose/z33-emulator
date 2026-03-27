use dashmap::DashMap;
use tower_lsp::lsp_types::{
    CompletionOptions, CompletionParams, CompletionResponse, DidChangeTextDocumentParams,
    DidCloseTextDocumentParams, DidOpenTextDocumentParams, GotoDefinitionParams,
    GotoDefinitionResponse, InitializeParams, InitializeResult, InitializedParams, Location, OneOf,
    ReferenceParams, ServerCapabilities, ServerInfo, TextDocumentSyncCapability,
    TextDocumentSyncKind, Url,
};
use tower_lsp::{jsonrpc, Client, LanguageServer};

use super::document::DocumentState;
use super::{completion, diagnostics, position};

/// LSP backend for Z33 assembly.
///
/// Holds a reference to the LSP client (for pushing diagnostics) and a
/// concurrent map of open documents.
pub struct Backend {
    client: Client,
    documents: DashMap<Url, DocumentState>,
}

impl Backend {
    #[must_use]
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: DashMap::new(),
        }
    }

    async fn on_change(&self, uri: Url, text: String, version: i32) {
        let state = DocumentState::new(text);
        let diags = diagnostics::diagnostics(&state);
        self.documents.insert(uri.clone(), state);
        self.client
            .publish_diagnostics(uri, diags, Some(version))
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _params: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec!["%".to_string(), ".".to_string()]),
                    ..Default::default()
                }),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "z33-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        tracing::info!("Z33 LSP server initialized");
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let doc = params.text_document;
        self.on_change(doc.uri, doc.text, doc.version).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        // We use Full sync, so there's exactly one change with the full text.
        if let Some(change) = params.content_changes.into_iter().next() {
            self.on_change(
                params.text_document.uri,
                change.text,
                params.text_document.version,
            )
            .await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents.remove(&params.text_document.uri);
        // Clear diagnostics
        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> jsonrpc::Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        let Some(state) = self.documents.get(uri) else {
            return Ok(None);
        };

        let Some(offset) = position::byte_offset(state.source(), pos) else {
            return Ok(None);
        };

        let items = completion::completions(&state, offset);
        if items.is_empty() {
            Ok(None)
        } else {
            Ok(Some(CompletionResponse::Array(items)))
        }
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> jsonrpc::Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let Some(state) = self.documents.get(uri) else {
            return Ok(None);
        };

        let Some(offset) = position::byte_offset(state.source(), pos) else {
            return Ok(None);
        };

        // Find the word at the cursor position
        let source = state.source();
        let word = word_at_offset(source, offset);

        if word.is_empty() {
            return Ok(None);
        }

        // Check if it's a label
        if !state.labels().contains_key(word) {
            return Ok(None);
        }

        // Find the label definition in the AST
        if let Some(program) = state.program() {
            for line in &program.lines {
                for symbol in &line.inner.symbols {
                    if symbol.inner == word {
                        // Symbol location is relative to line start (in
                        // preprocessed source). Resolve to original.
                        let abs_start = line.location.start + symbol.location.start;
                        let abs_end = line.location.start + symbol.location.end;
                        let original_span = state.resolve_span(abs_start..abs_end);
                        if let Some(range) = original_span.and_then(|s| position::range(source, s))
                        {
                            return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                                uri: uri.clone(),
                                range,
                            })));
                        }
                    }
                }
            }
        }

        Ok(None)
    }
    async fn references(&self, params: ReferenceParams) -> jsonrpc::Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        let Some(state) = self.documents.get(uri) else {
            return Ok(None);
        };

        let Some(offset) = position::byte_offset(state.source(), pos) else {
            return Ok(None);
        };

        let source = state.source();
        let word = word_at_offset(source, offset);

        if word.is_empty() || !state.labels().contains_key(word) {
            return Ok(None);
        }

        let locations = find_label_references(source, word, params.context.include_declaration)
            .filter_map(|span| {
                position::range(source, span).map(|range| Location {
                    uri: uri.clone(),
                    range,
                })
            })
            .collect::<Vec<_>>();

        if locations.is_empty() {
            Ok(None)
        } else {
            Ok(Some(locations))
        }
    }
}

/// Find all occurrences of `label` in `source`, yielding byte ranges.
///
/// If `include_declaration` is false, occurrences that are label definitions
/// (identifier followed by `:`) are excluded.
fn find_label_references<'a>(
    source: &'a str,
    label: &'a str,
    include_declaration: bool,
) -> impl Iterator<Item = std::ops::Range<usize>> + 'a {
    let label_len = label.len();
    let bytes = source.as_bytes();

    let mut start = 0;
    std::iter::from_fn(move || {
        while let Some(rel) = source[start..].find(label) {
            let abs = start + rel;
            start = abs + label_len;

            // Check word boundaries
            if abs > 0 && (bytes[abs - 1].is_ascii_alphanumeric() || bytes[abs - 1] == b'_') {
                continue;
            }
            if start < bytes.len() && (bytes[start].is_ascii_alphanumeric() || bytes[start] == b'_')
            {
                continue;
            }

            // Check if this is a declaration (followed by optional whitespace
            // then ':')
            if !include_declaration {
                let after = &source[start..];
                let trimmed = after.trim_start_matches([' ', '\t']);
                if trimmed.starts_with(':') {
                    continue;
                }
            }

            return Some(abs..start);
        }
        None
    })
}
fn word_at_offset(source: &str, offset: usize) -> &str {
    if offset > source.len() {
        return "";
    }

    let bytes = source.as_bytes();

    // Walk backwards to find start of identifier
    let mut start = offset;
    while start > 0 && (bytes[start - 1].is_ascii_alphanumeric() || bytes[start - 1] == b'_') {
        start -= 1;
    }

    // Walk forwards to find end of identifier
    let mut end = offset;
    while end < bytes.len() && (bytes[end].is_ascii_alphanumeric() || bytes[end] == b'_') {
        end += 1;
    }

    &source[start..end]
}
