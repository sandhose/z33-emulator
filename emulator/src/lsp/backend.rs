use std::collections::HashMap;
use std::sync::Arc;

use dashmap::DashMap;
use tower_lsp::lsp_types::{
    CodeLens, CodeLensOptions, Command, CompletionOptions, CompletionParams, CompletionResponse,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DocumentHighlight, DocumentHighlightKind, DocumentHighlightParams, DocumentSymbolParams,
    DocumentSymbolResponse, GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents,
    HoverParams, HoverProviderCapability, InitializeParams, InitializeResult, InitializedParams,
    Location, MarkupContent, MarkupKind, OneOf, PrepareRenameResponse, ReferenceParams,
    RenameParams, SemanticTokensFullOptions, SemanticTokensOptions, SemanticTokensParams,
    SemanticTokensResult, SemanticTokensServerCapabilities, ServerCapabilities, ServerInfo,
    SignatureHelpOptions, SignatureHelpParams, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextEdit, Url, WorkspaceEdit,
};
use tower_lsp::{jsonrpc, Client, LanguageServer};

use super::document::DocumentState;
use super::{completion, diagnostics, hover, position, semantic_tokens, signature, symbols};

/// Per-document state.
struct Document {
    /// Always-current source text, updated synchronously on every didChange.
    source: String,
    /// Latest completed analysis.
    analysis: Option<Arc<DocumentState>>,
}

/// LSP backend for Z33 assembly.
pub struct Backend {
    client: Client,
    documents: Arc<DashMap<Url, Document>>,
}

impl Backend {
    #[must_use]
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(DashMap::new()),
        }
    }

    async fn on_change(&self, uri: Url, text: String) {
        let state = Arc::new(DocumentState::new(text.clone()));
        let diags = diagnostics::diagnostics(&state);

        self.documents.insert(
            uri.clone(),
            Document {
                source: text,
                analysis: Some(state),
            },
        );

        self.client.publish_diagnostics(uri, diags, None).await;
    }

    fn get_source(&self, uri: &Url) -> Option<String> {
        self.documents.get(uri).map(|d| d.source.clone())
    }

    fn get_analysis(&self, uri: &Url) -> Option<Arc<DocumentState>> {
        self.documents.get(uri).and_then(|d| d.analysis.clone())
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
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec![",".to_string()]),
                    retrigger_characters: None,
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                document_highlight_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                code_lens_provider: Some(CodeLensOptions {
                    resolve_provider: None,
                }),
                rename_provider: Some(OneOf::Right(tower_lsp::lsp_types::RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options:
                        tower_lsp::lsp_types::WorkDoneProgressOptions::default(),
                })),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: semantic_tokens::legend(),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: None,
                            ..Default::default()
                        },
                    ),
                ),
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
        self.on_change(doc.uri, doc.text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().next() {
            self.on_change(params.text_document.uri, change.text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents.remove(&params.text_document.uri);
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

        let Some(source) = self.get_source(uri) else {
            return Ok(None);
        };
        let analysis = self.get_analysis(uri);

        let Some(offset) = position::byte_offset(&source, pos) else {
            return Ok(None);
        };

        let items = completion::completions(analysis.as_deref(), &source, offset);
        if items.is_empty() {
            Ok(None)
        } else {
            Ok(Some(CompletionResponse::Array(items)))
        }
    }

    async fn signature_help(
        &self,
        params: SignatureHelpParams,
    ) -> jsonrpc::Result<Option<tower_lsp::lsp_types::SignatureHelp>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let Some(source) = self.get_source(uri) else {
            return Ok(None);
        };

        let Some(offset) = position::byte_offset(&source, pos) else {
            return Ok(None);
        };

        Ok(signature::signature_help(&source, offset))
    }

    async fn hover(&self, params: HoverParams) -> jsonrpc::Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let Some(source) = self.get_source(uri) else {
            return Ok(None);
        };
        let analysis = self.get_analysis(uri);

        let Some(offset) = position::byte_offset(&source, pos) else {
            return Ok(None);
        };

        let Some(result) = hover::hover(analysis.as_deref(), &source, offset) else {
            return Ok(None);
        };

        let range = position::range(&source, result.span);

        Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: result.contents,
            }),
            range,
        }))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> jsonrpc::Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let Some(source) = self.get_source(uri) else {
            return Ok(None);
        };
        let Some(analysis) = self.get_analysis(uri) else {
            return Ok(None);
        };

        let Some(offset) = position::byte_offset(&source, pos) else {
            return Ok(None);
        };

        let word = word_at_offset(&source, offset);
        if word.is_empty() {
            return Ok(None);
        }

        // Try label definition
        if analysis.labels().contains_key(word) {
            if let Some(program) = analysis.program() {
                for line in &program.lines {
                    for symbol in &line.inner.symbols {
                        if symbol.inner == word {
                            let original_span = analysis.resolve_span(symbol.location.clone());
                            if let Some(range) =
                                original_span.and_then(|s| position::range(&source, s))
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
        }

        // Try macro definition (#define)
        if let Some(annotations) = analysis.annotations() {
            if let Some(def) = annotations.definitions.iter().rev().find(|d| d.key == word) {
                if let Some(range) = position::range(&source, def.span.clone()) {
                    return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                        uri: uri.clone(),
                        range,
                    })));
                }
            }
        }

        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> jsonrpc::Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        let Some(source) = self.get_source(uri) else {
            return Ok(None);
        };
        let analysis = self.get_analysis(uri);

        let Some(offset) = position::byte_offset(&source, pos) else {
            return Ok(None);
        };

        let word = word_at_offset(&source, offset);
        if word.is_empty() {
            return Ok(None);
        }

        let is_label = analysis
            .as_ref()
            .is_some_and(|a| a.labels().contains_key(word));
        let is_macro = analysis
            .as_ref()
            .and_then(|a| a.annotations())
            .is_some_and(|ann| ann.definitions.iter().any(|d| d.key == word));

        if !is_label && !is_macro {
            return Ok(None);
        }

        let include_decl = params.context.include_declaration;
        let iter: Box<dyn Iterator<Item = std::ops::Range<usize>> + '_> = if is_label {
            Box::new(find_label_references(&source, word, include_decl))
        } else if include_decl {
            find_word_occurrences(&source, word)
        } else {
            Box::new(find_word_occurrences(&source, word).filter(|span| {
                let before = &source[..span.start];
                !before.ends_with("define ") && !before.ends_with("define\t")
            }))
        };
        let locations: Vec<Location> = iter
            .filter_map(|span| {
                position::range(&source, span).map(|range| Location {
                    uri: uri.clone(),
                    range,
                })
            })
            .collect();

        if locations.is_empty() {
            Ok(None)
        } else {
            Ok(Some(locations))
        }
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> jsonrpc::Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;

        let Some(analysis) = self.get_analysis(uri) else {
            return Ok(None);
        };

        let syms = symbols::document_symbols(&analysis);
        if syms.is_empty() {
            Ok(None)
        } else {
            Ok(Some(DocumentSymbolResponse::Nested(syms)))
        }
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> jsonrpc::Result<Option<Vec<DocumentHighlight>>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let Some(source) = self.get_source(uri) else {
            return Ok(None);
        };
        let analysis = self.get_analysis(uri);

        let Some(offset) = position::byte_offset(&source, pos) else {
            return Ok(None);
        };

        let word = word_at_offset(&source, offset);
        if word.is_empty() {
            return Ok(None);
        }

        // Check if this is a known label or macro
        let is_label = analysis
            .as_ref()
            .is_some_and(|a| a.labels().contains_key(word));
        let is_macro = analysis
            .as_ref()
            .and_then(|a| a.annotations())
            .is_some_and(|ann| ann.definitions.iter().any(|d| d.key == word));

        if !is_label && !is_macro {
            return Ok(None);
        }

        let highlights: Vec<DocumentHighlight> = find_word_occurrences(&source, word)
            .filter_map(|span| {
                let range = position::range(&source, span.clone())?;
                let after = &source[span.end..];
                let trimmed = after.trim_start_matches([' ', '\t']);
                let before = &source[..span.start];
                // Label definitions (foo:) and #define lines are WRITE
                let is_label_def = trimmed.starts_with(':');
                let is_macro_def = before.ends_with("define ") || before.ends_with("define\t");
                let kind = if is_label_def || is_macro_def {
                    DocumentHighlightKind::WRITE
                } else {
                    DocumentHighlightKind::READ
                };
                Some(DocumentHighlight {
                    range,
                    kind: Some(kind),
                })
            })
            .collect();

        if highlights.is_empty() {
            Ok(None)
        } else {
            Ok(Some(highlights))
        }
    }

    async fn code_lens(
        &self,
        params: tower_lsp::lsp_types::CodeLensParams,
    ) -> jsonrpc::Result<Option<Vec<CodeLens>>> {
        let uri = &params.text_document.uri;

        let Some(source) = self.get_source(uri) else {
            return Ok(None);
        };
        let Some(analysis) = self.get_analysis(uri) else {
            return Ok(None);
        };

        let mut lenses = Vec::new();

        for name in analysis.labels().keys() {
            let Some(def_span) = find_label_definition(&source, name) else {
                continue;
            };
            let Some(range) = position::range(&source, def_span) else {
                continue;
            };

            let ref_count = find_label_references(&source, name, false).count();

            let title = match ref_count {
                0 => "0 references".to_string(),
                1 => "1 reference".to_string(),
                n => format!("{n} references"),
            };

            lenses.push(CodeLens {
                range,
                command: Some(Command {
                    title,
                    command: String::new(),
                    arguments: None,
                }),
                data: None,
            });
        }

        if lenses.is_empty() {
            Ok(None)
        } else {
            Ok(Some(lenses))
        }
    }

    async fn prepare_rename(
        &self,
        params: tower_lsp::lsp_types::TextDocumentPositionParams,
    ) -> jsonrpc::Result<Option<PrepareRenameResponse>> {
        let uri = &params.text_document.uri;
        let pos = params.position;

        let Some(source) = self.get_source(uri) else {
            return Ok(None);
        };
        let Some(analysis) = self.get_analysis(uri) else {
            return Ok(None);
        };

        let Some(offset) = position::byte_offset(&source, pos) else {
            return Ok(None);
        };

        let word = word_at_offset(&source, offset);
        if word.is_empty() || !analysis.labels().contains_key(word) {
            return Ok(None);
        }

        let word_start = word.as_ptr() as usize - source.as_ptr() as usize;
        let word_end = word_start + word.len();
        let range = position::range(&source, word_start..word_end);

        Ok(range.map(PrepareRenameResponse::Range))
    }

    async fn rename(&self, params: RenameParams) -> jsonrpc::Result<Option<WorkspaceEdit>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        let Some(source) = self.get_source(uri) else {
            return Ok(None);
        };
        let Some(analysis) = self.get_analysis(uri) else {
            return Ok(None);
        };

        let Some(offset) = position::byte_offset(&source, pos) else {
            return Ok(None);
        };

        let word = word_at_offset(&source, offset);
        if word.is_empty() || !analysis.labels().contains_key(word) {
            return Ok(None);
        }

        let new_name = &params.new_name;
        let edits: Vec<TextEdit> = find_label_references(&source, word, true)
            .filter_map(|span| {
                position::range(&source, span).map(|range| TextEdit {
                    range,
                    new_text: new_name.clone(),
                })
            })
            .collect();

        if edits.is_empty() {
            return Ok(None);
        }

        let mut changes = HashMap::new();
        changes.insert(uri.clone(), edits);

        Ok(Some(WorkspaceEdit {
            changes: Some(changes),
            ..Default::default()
        }))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> jsonrpc::Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;

        let Some(source) = self.get_source(uri) else {
            return Ok(None);
        };
        let analysis = self.get_analysis(uri);

        let tokens = semantic_tokens::semantic_tokens(analysis.as_deref(), &source);
        Ok(Some(SemanticTokensResult::Tokens(tokens)))
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

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

            if abs > 0 && (bytes[abs - 1].is_ascii_alphanumeric() || bytes[abs - 1] == b'_') {
                continue;
            }
            if start < bytes.len() && (bytes[start].is_ascii_alphanumeric() || bytes[start] == b'_')
            {
                continue;
            }

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

/// Find all whole-word occurrences of `word` in `source`.
fn find_word_occurrences<'a>(
    source: &'a str,
    word: &'a str,
) -> Box<dyn Iterator<Item = std::ops::Range<usize>> + 'a> {
    let word_len = word.len();
    let bytes = source.as_bytes();

    let mut start = 0;
    Box::new(std::iter::from_fn(move || {
        while let Some(rel) = source[start..].find(word) {
            let abs = start + rel;
            start = abs + word_len;

            if abs > 0 && (bytes[abs - 1].is_ascii_alphanumeric() || bytes[abs - 1] == b'_') {
                continue;
            }
            if start < bytes.len() && (bytes[start].is_ascii_alphanumeric() || bytes[start] == b'_')
            {
                continue;
            }

            return Some(abs..start);
        }
        None
    }))
}

fn word_at_offset(source: &str, offset: usize) -> &str {
    if offset > source.len() {
        return "";
    }
    let bytes = source.as_bytes();
    let mut start = offset;
    while start > 0 && (bytes[start - 1].is_ascii_alphanumeric() || bytes[start - 1] == b'_') {
        start -= 1;
    }
    let mut end = offset;
    while end < bytes.len() && (bytes[end].is_ascii_alphanumeric() || bytes[end] == b'_') {
        end += 1;
    }
    &source[start..end]
}

fn find_label_definition(source: &str, label: &str) -> Option<std::ops::Range<usize>> {
    let label_len = label.len();
    let bytes = source.as_bytes();
    let mut start = 0;
    while let Some(rel) = source[start..].find(label) {
        let abs = start + rel;
        let end = abs + label_len;
        start = end;
        if abs > 0 && (bytes[abs - 1].is_ascii_alphanumeric() || bytes[abs - 1] == b'_') {
            continue;
        }
        if end < bytes.len() && (bytes[end].is_ascii_alphanumeric() || bytes[end] == b'_') {
            continue;
        }
        let after = &source[end..];
        let trimmed = after.trim_start_matches([' ', '\t']);
        if trimmed.starts_with(':') {
            return Some(abs..end);
        }
    }
    None
}
