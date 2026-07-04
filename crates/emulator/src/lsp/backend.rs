use std::collections::HashMap;
use std::sync::Mutex;

use camino::Utf8PathBuf;
use tower_lsp::lsp_types::{
    CodeLens, CodeLensOptions, Command, CompletionOptions, CompletionParams, CompletionResponse,
    Diagnostic, DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DocumentHighlight, DocumentHighlightParams, DocumentSymbolParams, DocumentSymbolResponse,
    GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents, HoverParams,
    HoverProviderCapability, InitializeParams, InitializeResult, InitializedParams, Location,
    MarkupContent, MarkupKind, OneOf, PrepareRenameResponse, ReferenceParams, RenameParams,
    SemanticTokensFullOptions, SemanticTokensOptions, SemanticTokensParams, SemanticTokensResult,
    SemanticTokensServerCapabilities, ServerCapabilities, ServerInfo, TextDocumentSyncCapability,
    TextDocumentSyncKind, Url, WorkspaceEdit,
};
use tower_lsp::{jsonrpc, Client, LanguageServer};

use super::document::LabelKind;
use super::references::OccurrenceKind;
use super::workspace::WorkspaceManager;
use super::{completion, hover, position, semantic_tokens, symbols};

/// The custom notification the host uses to push in-memory workspace files.
pub const WORKSPACE_FILES_METHOD: &str = "zorglub33/workspaceFiles";

/// Client-side command carried by the run code lens. Only emitted when the
/// client advertised it in the `experimental.commands` capability.
pub const RUN_COMMAND: &str = "zorglub33.run";

/// LSP backend for Z33 assembly.
///
/// # Multi-file model
///
/// The backend resolves `#include` directives against a workspace overlay: open
/// documents take precedence over a base file map, which in turn falls back to
/// disk (native mode only). The base source can come from two places, selected
/// at runtime with no cargo features:
///
/// - **Native**: when the client sends a `file://` `rootUri` (or workspace
///   folder) in `initialize`, unopened files are read from disk relative to
///   that root.
/// - **Host-pushed**: the client may send a custom `zorglub33/workspaceFiles`
///   notification whose params are `{ "files": { "relative/path.s": "content",
///   ... } }`. This replaces the in-memory base file map and triggers
///   re-analysis. The web IDE and the VS Code web extension use this because
///   they have no disk access.
///
/// Every open document is analyzed as its own preprocessing root, under its
/// real (workspace-relative) path, so cross-file features — go-to-definition,
/// references, rename, and diagnostics — resolve across files.
pub struct Backend {
    client: Client,
    manager: Mutex<WorkspaceManager>,
}

impl Backend {
    #[must_use]
    pub fn new(client: Client) -> Self {
        Self {
            client,
            manager: Mutex::new(WorkspaceManager::new()),
        }
    }

    /// Publish a batch of per-URI diagnostics.
    async fn publish(&self, batches: Vec<(Url, Vec<Diagnostic>)>) {
        for (uri, diagnostics) in batches {
            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
        }
    }

    fn lock(&self) -> std::sync::MutexGuard<'_, WorkspaceManager> {
        self.manager
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
    }

    /// Custom `zorglub33/workspaceFiles` notification handler.
    ///
    /// Params: `{ "files": { "<relative path>": "<content>", ... } }`.
    pub async fn workspace_files(&self, params: serde_json::Value) {
        let files = parse_workspace_files(&params);
        let batches = self.lock().set_base_files(files);
        self.publish(batches).await;
    }
}

/// Parse the params of the `zorglub33/workspaceFiles` notification into a
/// relative path -> content map. Unknown or malformed entries are silently
/// ignored. Extract the client-side commands advertised in the `experimental`
/// client capability: `{"experimental": {"commands": ["zorglub33.run", ...]}}`.
fn parse_client_commands(
    experimental: Option<&serde_json::Value>,
) -> std::collections::HashSet<String> {
    experimental
        .and_then(|e| e.get("commands"))
        .and_then(|c| c.as_array())
        .map(|cmds| {
            cmds.iter()
                .filter_map(|c| c.as_str().map(str::to_owned))
                .collect()
        })
        .unwrap_or_default()
}

/// Build the code lenses for a document: an informational lens (resolved
/// address + reference count) on every label defined in it, preceded by a
/// `zorglub33.run` lens on executable labels when `run_lens_path` is set (i.e.
/// the client advertised it can execute the command).
fn build_code_lenses(
    source: &str,
    analysis: &super::document::DocumentState,
    run_lens_path: Option<&camino::Utf8Path>,
) -> Vec<CodeLens> {
    let root = analysis.root_file_id();
    let mut lenses = Vec::new();

    for (name, addr) in analysis.labels() {
        // Only place a lens on labels defined in this document.
        let Some(def) = analysis
            .occurrences_of(name)
            .find(|o| o.kind == OccurrenceKind::Definition && o.file_id == root)
        else {
            continue;
        };
        let Some(range) = position::range(source, def.span.clone()) else {
            continue;
        };

        // Executable labels get a run lens (start execution with this label
        // as the entrypoint), when the client can handle it.
        if let Some(path) = run_lens_path {
            if analysis.label_kind(name) == Some(LabelKind::Code) {
                lenses.push(CodeLens {
                    range,
                    command: Some(Command {
                        title: format!("\u{25b6} Run {name}"),
                        command: RUN_COMMAND.to_string(),
                        arguments: Some(vec![serde_json::json!({
                            "path": path,
                            "label": name,
                        })]),
                    }),
                    data: None,
                });
            }
        }

        let ref_count = analysis
            .occurrences_of(name)
            .filter(|o| o.kind != OccurrenceKind::Definition)
            .count();

        let refs = match ref_count {
            0 => "0 references".to_string(),
            1 => "1 reference".to_string(),
            n => format!("{n} references"),
        };

        lenses.push(CodeLens {
            range,
            command: Some(Command {
                title: format!("address {addr} | {refs}"),
                command: String::new(),
                arguments: None,
            }),
            data: None,
        });
    }

    lenses
}

fn parse_workspace_files(params: &serde_json::Value) -> HashMap<Utf8PathBuf, String> {
    let mut files = HashMap::new();
    if let Some(map) = params.get("files").and_then(serde_json::Value::as_object) {
        for (path, content) in map {
            if let Some(content) = content.as_str() {
                files.insert(Utf8PathBuf::from(path), content.to_string());
            }
        }
    }
    files
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        // Prefer the (deprecated but widely sent) root_uri, then the first
        // workspace folder.
        #[allow(deprecated)]
        let root_uri = params.root_uri.or_else(|| {
            params
                .workspace_folders
                .and_then(|folders| folders.into_iter().next())
                .map(|folder| folder.uri)
        });
        // Client-side commands the client can execute, advertised through the
        // open-ended `experimental` capability (same convention as
        // rust-analyzer): `{"experimental": {"commands": ["zorglub33.run", ...]}}`.
        // Server-produced `Command`s (e.g. the run code lens) are only
        // emitted when the client declared it can handle them.
        let client_commands = parse_client_commands(params.capabilities.experimental.as_ref());

        {
            let mut manager = self.lock();
            manager.set_root(root_uri);
            manager.set_client_commands(client_commands);
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec!["%".to_string(), ".".to_string()]),
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
                name: "zorglub33-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        tracing::info!("Zorglub33 LSP server initialized");
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let doc = params.text_document;
        let batches = self.lock().upsert(doc.uri, doc.text);
        self.publish(batches).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().next() {
            let batches = self.lock().upsert(params.text_document.uri, change.text);
            self.publish(batches).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let batches = self.lock().close(&params.text_document.uri);
        self.publish(batches).await;
    }

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> jsonrpc::Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        let (source, analysis) = {
            let manager = self.lock();
            let Some(source) = manager.source(uri) else {
                return Ok(None);
            };
            (source, manager.analysis(uri))
        };

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

    async fn hover(&self, params: HoverParams) -> jsonrpc::Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let (source, analysis) = {
            let manager = self.lock();
            let Some(source) = manager.source(uri) else {
                return Ok(None);
            };
            (source, manager.analysis(uri))
        };

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

        let manager = self.lock();
        let Some(source) = manager.source(uri) else {
            return Ok(None);
        };
        let Some(offset) = position::byte_offset(&source, pos) else {
            return Ok(None);
        };

        Ok(manager
            .goto_definition(uri, offset)
            .map(GotoDefinitionResponse::Scalar))
    }

    async fn references(&self, params: ReferenceParams) -> jsonrpc::Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        let manager = self.lock();
        let Some(source) = manager.source(uri) else {
            return Ok(None);
        };
        let Some(offset) = position::byte_offset(&source, pos) else {
            return Ok(None);
        };

        Ok(manager.references(uri, offset, params.context.include_declaration))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> jsonrpc::Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;

        let Some(analysis) = self.lock().analysis(uri) else {
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

        let manager = self.lock();
        let Some(source) = manager.source(uri) else {
            return Ok(None);
        };
        let Some(offset) = position::byte_offset(&source, pos) else {
            return Ok(None);
        };

        Ok(manager.document_highlight(uri, offset))
    }

    async fn code_lens(
        &self,
        params: tower_lsp::lsp_types::CodeLensParams,
    ) -> jsonrpc::Result<Option<Vec<CodeLens>>> {
        let uri = &params.text_document.uri;

        let (source, analysis, run_lens_path) = {
            let manager = self.lock();
            let Some(source) = manager.source(uri) else {
                return Ok(None);
            };
            let Some(analysis) = manager.analysis(uri) else {
                return Ok(None);
            };
            // The run lens only makes sense where a client-side handler
            // exists (VS Code extension, web IDE). Other clients (e.g. Zed)
            // get the informational lens only.
            let run_lens_path = manager
                .supports_client_command(RUN_COMMAND)
                .then(|| manager.relative_for_uri(uri));
            (source, analysis, run_lens_path)
        };

        let lenses = build_code_lenses(&source, &analysis, run_lens_path.as_deref());
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

        let (source, analysis) = {
            let manager = self.lock();
            let Some(source) = manager.source(uri) else {
                return Ok(None);
            };
            let Some(analysis) = manager.analysis(uri) else {
                return Ok(None);
            };
            (source, analysis)
        };

        let Some(offset) = position::byte_offset(&source, pos) else {
            return Ok(None);
        };

        let Some(occ) = analysis.occurrence_at(offset) else {
            return Ok(None);
        };

        let range = position::range(&source, occ.span.clone());
        Ok(range.map(PrepareRenameResponse::Range))
    }

    async fn rename(&self, params: RenameParams) -> jsonrpc::Result<Option<WorkspaceEdit>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        let manager = self.lock();
        let Some(source) = manager.source(uri) else {
            return Ok(None);
        };
        let Some(offset) = position::byte_offset(&source, pos) else {
            return Ok(None);
        };

        Ok(manager.rename(uri, offset, &params.new_name))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> jsonrpc::Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;

        let (source, analysis) = {
            let manager = self.lock();
            let Some(source) = manager.source(uri) else {
                return Ok(None);
            };
            (source, manager.analysis(uri))
        };

        let tokens = semantic_tokens::semantic_tokens(analysis.as_deref(), &source);
        Ok(Some(SemanticTokensResult::Tokens(tokens)))
    }
}

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;
    use serde_json::json;

    use super::{build_code_lenses, parse_client_commands, parse_workspace_files, RUN_COMMAND};
    use crate::lsp::document::DocumentState;

    #[test]
    fn parses_workspace_files_params() {
        let params = json!({
            "files": {
                "main.s": "reset\n",
                "lib/util.s": "target:\n",
            }
        });
        let files = parse_workspace_files(&params);
        assert_eq!(files.len(), 2);
        assert_eq!(
            files.get(&Utf8PathBuf::from("main.s")).map(String::as_str),
            Some("reset\n")
        );
        assert_eq!(
            files
                .get(&Utf8PathBuf::from("lib/util.s"))
                .map(String::as_str),
            Some("target:\n")
        );
    }

    #[test]
    fn ignores_malformed_workspace_files_params() {
        assert!(parse_workspace_files(&json!({})).is_empty());
        assert!(parse_workspace_files(&json!({ "files": 42 })).is_empty());
        // Non-string values are skipped.
        let files = parse_workspace_files(&json!({ "files": { "a.s": 1, "b.s": "ok" } }));
        assert_eq!(files.len(), 1);
        assert!(files.contains_key(&Utf8PathBuf::from("b.s")));
    }

    #[test]
    fn parses_client_commands_capability() {
        let caps = json!({ "commands": ["zorglub33.run", "zorglub33.other"] });
        let cmds = parse_client_commands(Some(&caps));
        assert!(cmds.contains(RUN_COMMAND));
        assert!(cmds.contains("zorglub33.other"));

        assert!(parse_client_commands(None).is_empty());
        assert!(parse_client_commands(Some(&json!({}))).is_empty());
        assert!(parse_client_commands(Some(&json!({ "commands": "zorglub33.run" }))).is_empty());
    }

    #[test]
    fn run_lens_gated_on_client_capability_and_label_kind() {
        let src = "main:\n    reset\nvalue:\n    .word 42\n";
        let analysis = DocumentState::new(src.to_string());

        // Client did not advertise zorglub33.run: informational lenses only.
        let lenses = build_code_lenses(src, &analysis, None);
        assert_eq!(lenses.len(), 2);
        assert!(lenses
            .iter()
            .all(|l| l.command.as_ref().unwrap().command.is_empty()));

        // Client advertised it: the code label gains a run lens, the data
        // label does not.
        let lenses = build_code_lenses(src, &analysis, Some(camino::Utf8Path::new("main.s")));
        assert_eq!(lenses.len(), 3);
        let run: Vec<_> = lenses
            .iter()
            .filter(|l| l.command.as_ref().unwrap().command == RUN_COMMAND)
            .collect();
        assert_eq!(run.len(), 1);
        let cmd = run[0].command.as_ref().unwrap();
        assert_eq!(cmd.title, "\u{25b6} Run main");
        assert_eq!(
            cmd.arguments,
            Some(vec![json!({ "path": "main.s", "label": "main" })])
        );
    }
}
