//! The transport-agnostic LSP session.
//!
//! [`LspSession`] contains **no I/O**. The caller owns the transport (stdio
//! for the native CLI, a JS bridge for the WASM host) and the framing. The
//! session is driven through two entry points, mirroring
//! [`DebugSession`](crate::dap::DebugSession):
//!
//! * [`LspSession::handle_message`] — feed it one decoded client message (a
//!   [`serde_json::Value`]); it returns the responses and notifications to
//!   emit, each a complete JSON-RPC message ready to serialize. Diagnostics are
//!   part of the returned batch (as `textDocument/publishDiagnostics`
//!   notifications) rather than a client callback.
//! * [`LspSession::exit_requested`] — whether the client sent the `exit`
//!   notification and the transport should shut down.

use std::collections::HashMap;

use camino::Utf8PathBuf;
use lsp_types::{
    CodeLens, CodeLensOptions, CodeLensParams, Command, CompletionOptions, CompletionParams,
    CompletionResponse, Diagnostic, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DocumentHighlightParams, DocumentSymbolParams,
    DocumentSymbolResponse, GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents,
    HoverParams, HoverProviderCapability, InitializeParams, InitializeResult, MarkupContent,
    MarkupKind, OneOf, PrepareRenameResponse, PublishDiagnosticsParams, ReferenceParams,
    RenameOptions, RenameParams, SemanticTokensFullOptions, SemanticTokensOptions,
    SemanticTokensParams, SemanticTokensResult, SemanticTokensServerCapabilities,
    ServerCapabilities, ServerInfo, TextDocumentPositionParams, TextDocumentSyncCapability,
    TextDocumentSyncKind, Uri, WorkDoneProgressOptions,
};
use serde_json::{json, Map, Value};

use super::document::LabelKind;
use super::references::OccurrenceKind;
use super::workspace::WorkspaceManager;
use super::{completion, hover, position, semantic_tokens, symbols};

/// The custom notification the host uses to push in-memory workspace files.
pub const WORKSPACE_FILES_METHOD: &str = "zorglub33/workspaceFiles";

/// Client-side command carried by the run code lens. Only emitted when the
/// client advertised it in the `experimental.commands` capability.
pub const RUN_COMMAND: &str = "zorglub33.run";

// Standard JSON-RPC / LSP error codes.
const INVALID_REQUEST: i64 = -32600;
const METHOD_NOT_FOUND: i64 = -32601;
const INVALID_PARAMS: i64 = -32602;
const INTERNAL_ERROR: i64 = -32603;
const SERVER_NOT_INITIALIZED: i64 = -32002;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum State {
    Uninitialized,
    Initialized,
    ShutDown,
}

/// A failed request, turned into a JSON-RPC error response by the dispatcher.
struct RequestError {
    code: i64,
    message: String,
}

type RequestResult = Result<Value, RequestError>;

/// LSP session for Z33 assembly.
///
/// # Multi-file model
///
/// The session resolves `#include` directives against a workspace overlay: open
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
pub struct LspSession {
    state: State,
    exit_requested: bool,
    manager: WorkspaceManager,
}

impl Default for LspSession {
    fn default() -> Self {
        Self::new()
    }
}

impl LspSession {
    #[must_use]
    pub fn new() -> Self {
        Self {
            state: State::Uninitialized,
            exit_requested: false,
            manager: WorkspaceManager::new(),
        }
    }

    /// Whether the client sent the `exit` notification and the transport
    /// should shut down.
    #[must_use]
    pub fn exit_requested(&self) -> bool {
        self.exit_requested
    }

    // ----------------------------------------------------------------------
    // Message handling
    // ----------------------------------------------------------------------

    /// Handle one decoded client JSON-RPC message, returning the messages to
    /// emit (the response for a request, plus any notifications — e.g.
    /// `textDocument/publishDiagnostics` after a document change).
    #[must_use]
    pub fn handle_message(&mut self, message: &Value) -> Vec<Value> {
        let method = message.get("method").and_then(Value::as_str);
        let id = message.get("id");
        let params = message.get("params").cloned().unwrap_or(Value::Null);

        match (method, id) {
            // No method: a response to a server-initiated request. This server
            // never sends requests, so there is nothing to route it to.
            (None, _) => Vec::new(),
            (Some(method), Some(id)) => self.handle_request(id, method, params),
            (Some(method), None) => self.handle_notification(method, params),
        }
    }

    fn handle_request(&mut self, id: &Value, method: &str, params: Value) -> Vec<Value> {
        // Lifecycle gates (LSP spec): before `initialize`, every other request
        // is answered with ServerNotInitialized; a duplicate `initialize` is
        // rejected without touching session state (the workspace root and
        // client commands were captured by the first one); after `shutdown`,
        // every request is answered with InvalidRequest.
        match self.state {
            State::Uninitialized if method != "initialize" => {
                return vec![error_response(
                    id,
                    SERVER_NOT_INITIALIZED,
                    "server not initialized",
                )];
            }
            State::Initialized if method == "initialize" => {
                return vec![error_response(
                    id,
                    INVALID_REQUEST,
                    "server is already initialized",
                )];
            }
            State::ShutDown => {
                return vec![error_response(id, INVALID_REQUEST, "server is shut down")];
            }
            _ => {}
        }

        let result = match method {
            "initialize" => self.on_initialize(params),
            "shutdown" => {
                self.state = State::ShutDown;
                Ok(Value::Null)
            }
            "textDocument/completion" => self.on_completion(params),
            "textDocument/hover" => self.on_hover(params),
            "textDocument/definition" => self.on_goto_definition(params),
            "textDocument/references" => self.on_references(params),
            "textDocument/documentSymbol" => self.on_document_symbol(params),
            "textDocument/documentHighlight" => self.on_document_highlight(params),
            "textDocument/codeLens" => self.on_code_lens(params),
            "textDocument/prepareRename" => self.on_prepare_rename(params),
            "textDocument/rename" => self.on_rename(params),
            "textDocument/semanticTokens/full" => self.on_semantic_tokens_full(params),
            other => {
                return vec![error_response(
                    id,
                    METHOD_NOT_FOUND,
                    format!("method not found: {other}"),
                )];
            }
        };

        match result {
            Ok(result) => vec![response(id, result)],
            Err(e) => vec![error_response(id, e.code, e.message)],
        }
    }

    fn handle_notification(&mut self, method: &str, params: Value) -> Vec<Value> {
        // `exit` is honored in every state.
        if method == "exit" {
            self.exit_requested = true;
            return Vec::new();
        }
        // Per the LSP spec, other notifications are dropped before
        // `initialize` and after `shutdown`.
        if self.state != State::Initialized {
            return Vec::new();
        }

        match method {
            "initialized" => {
                tracing::info!("Zorglub33 LSP server initialized");
                Vec::new()
            }
            "textDocument/didOpen" => {
                let Ok(params) = serde_json::from_value::<DidOpenTextDocumentParams>(params) else {
                    return Vec::new();
                };
                let doc = params.text_document;
                publish(self.manager.upsert(doc.uri, doc.text))
            }
            "textDocument/didChange" => {
                let Ok(params) = serde_json::from_value::<DidChangeTextDocumentParams>(params)
                else {
                    return Vec::new();
                };
                match params.content_changes.into_iter().next() {
                    Some(change) => {
                        publish(self.manager.upsert(params.text_document.uri, change.text))
                    }
                    None => Vec::new(),
                }
            }
            "textDocument/didClose" => {
                let Ok(params) = serde_json::from_value::<DidCloseTextDocumentParams>(params)
                else {
                    return Vec::new();
                };
                publish(self.manager.close(&params.text_document.uri))
            }
            WORKSPACE_FILES_METHOD => {
                let files = parse_workspace_files(&params);
                publish(self.manager.set_base_files(files))
            }
            // Unknown notifications (`$/cancelRequest`, `$/setTrace`, ...) are
            // ignored, as the spec prescribes.
            _ => Vec::new(),
        }
    }

    // ----------------------------------------------------------------------
    // Individual request handlers
    // ----------------------------------------------------------------------

    fn on_initialize(&mut self, params: Value) -> RequestResult {
        let params: InitializeParams = parse_params(params)?;

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

        self.manager.set_root(root_uri);
        self.manager.set_client_commands(client_commands);
        self.state = State::Initialized;

        to_result(InitializeResult {
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
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: WorkDoneProgressOptions::default(),
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

    fn on_completion(&self, params: Value) -> RequestResult {
        let params: CompletionParams = parse_params(params)?;
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        let Some(source) = self.manager.source(uri) else {
            return Ok(Value::Null);
        };
        let analysis = self.manager.analysis(uri);

        let Some(offset) = position::byte_offset(&source, pos) else {
            return Ok(Value::Null);
        };

        let items = completion::completions(analysis.as_deref(), &source, offset);
        if items.is_empty() {
            Ok(Value::Null)
        } else {
            to_result(CompletionResponse::Array(items))
        }
    }

    fn on_hover(&self, params: Value) -> RequestResult {
        let params: HoverParams = parse_params(params)?;
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let Some(source) = self.manager.source(uri) else {
            return Ok(Value::Null);
        };
        let analysis = self.manager.analysis(uri);

        let Some(offset) = position::byte_offset(&source, pos) else {
            return Ok(Value::Null);
        };

        let Some(result) = hover::hover(analysis.as_deref(), &source, offset) else {
            return Ok(Value::Null);
        };

        let range = position::range(&source, result.span);

        to_result(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: result.contents,
            }),
            range,
        })
    }

    fn on_goto_definition(&self, params: Value) -> RequestResult {
        let params: GotoDefinitionParams = parse_params(params)?;
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let Some(source) = self.manager.source(uri) else {
            return Ok(Value::Null);
        };
        let Some(offset) = position::byte_offset(&source, pos) else {
            return Ok(Value::Null);
        };

        to_result(
            self.manager
                .goto_definition(uri, offset)
                .map(GotoDefinitionResponse::Scalar),
        )
    }

    fn on_references(&self, params: Value) -> RequestResult {
        let params: ReferenceParams = parse_params(params)?;
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        let Some(source) = self.manager.source(uri) else {
            return Ok(Value::Null);
        };
        let Some(offset) = position::byte_offset(&source, pos) else {
            return Ok(Value::Null);
        };

        to_result(
            self.manager
                .references(uri, offset, params.context.include_declaration),
        )
    }

    fn on_document_symbol(&self, params: Value) -> RequestResult {
        let params: DocumentSymbolParams = parse_params(params)?;
        let uri = &params.text_document.uri;

        let Some(analysis) = self.manager.analysis(uri) else {
            return Ok(Value::Null);
        };

        let syms = symbols::document_symbols(&analysis);
        if syms.is_empty() {
            Ok(Value::Null)
        } else {
            to_result(DocumentSymbolResponse::Nested(syms))
        }
    }

    fn on_document_highlight(&self, params: Value) -> RequestResult {
        let params: DocumentHighlightParams = parse_params(params)?;
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let Some(source) = self.manager.source(uri) else {
            return Ok(Value::Null);
        };
        let Some(offset) = position::byte_offset(&source, pos) else {
            return Ok(Value::Null);
        };

        to_result(self.manager.document_highlight(uri, offset))
    }

    fn on_code_lens(&self, params: Value) -> RequestResult {
        let params: CodeLensParams = parse_params(params)?;
        let uri = &params.text_document.uri;

        let Some(source) = self.manager.source(uri) else {
            return Ok(Value::Null);
        };
        let Some(analysis) = self.manager.analysis(uri) else {
            return Ok(Value::Null);
        };
        // The run lens only makes sense where a client-side handler exists (VS
        // Code extension, web IDE). Other clients (e.g. Zed) get the
        // informational lens only.
        let run_lens_path = self
            .manager
            .supports_client_command(RUN_COMMAND)
            .then(|| self.manager.relative_for_uri(uri));

        let lenses = build_code_lenses(&source, &analysis, run_lens_path.as_deref());
        if lenses.is_empty() {
            Ok(Value::Null)
        } else {
            to_result(lenses)
        }
    }

    fn on_prepare_rename(&self, params: Value) -> RequestResult {
        let params: TextDocumentPositionParams = parse_params(params)?;
        let uri = &params.text_document.uri;
        let pos = params.position;

        let Some(source) = self.manager.source(uri) else {
            return Ok(Value::Null);
        };
        let Some(analysis) = self.manager.analysis(uri) else {
            return Ok(Value::Null);
        };

        let Some(offset) = position::byte_offset(&source, pos) else {
            return Ok(Value::Null);
        };

        let Some(occ) = analysis.occurrence_at(offset) else {
            return Ok(Value::Null);
        };

        let range = position::range(&source, occ.span.clone());
        to_result(range.map(PrepareRenameResponse::Range))
    }

    fn on_rename(&self, params: Value) -> RequestResult {
        let params: RenameParams = parse_params(params)?;
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        let Some(source) = self.manager.source(uri) else {
            return Ok(Value::Null);
        };
        let Some(offset) = position::byte_offset(&source, pos) else {
            return Ok(Value::Null);
        };

        to_result(self.manager.rename(uri, offset, &params.new_name))
    }

    fn on_semantic_tokens_full(&self, params: Value) -> RequestResult {
        let params: SemanticTokensParams = parse_params(params)?;
        let uri = &params.text_document.uri;

        let Some(source) = self.manager.source(uri) else {
            return Ok(Value::Null);
        };
        let analysis = self.manager.analysis(uri);

        let tokens = semantic_tokens::semantic_tokens(analysis.as_deref(), &source);
        to_result(SemanticTokensResult::Tokens(tokens))
    }
}

// --------------------------------------------------------------------------
// Message envelope builders
// --------------------------------------------------------------------------

fn response(id: &Value, result: Value) -> Value {
    let mut m = Map::new();
    m.insert("jsonrpc".to_owned(), "2.0".into());
    m.insert("id".to_owned(), id.clone());
    // `result` is REQUIRED on success, even when null.
    m.insert("result".to_owned(), result);
    Value::Object(m)
}

fn error_response(id: &Value, code: i64, message: impl Into<String>) -> Value {
    let mut m = Map::new();
    m.insert("jsonrpc".to_owned(), "2.0".into());
    m.insert("id".to_owned(), id.clone());
    m.insert(
        "error".to_owned(),
        json!({ "code": code, "message": message.into() }),
    );
    Value::Object(m)
}

fn notification(method: &str, params: Value) -> Value {
    let mut m = Map::new();
    m.insert("jsonrpc".to_owned(), "2.0".into());
    m.insert("method".to_owned(), method.into());
    m.insert("params".to_owned(), params);
    Value::Object(m)
}

/// Turn a batch of per-URI diagnostics into `textDocument/publishDiagnostics`
/// notifications.
fn publish(batches: Vec<(Uri, Vec<Diagnostic>)>) -> Vec<Value> {
    batches
        .into_iter()
        .map(|(uri, diagnostics)| {
            let params = PublishDiagnosticsParams {
                uri,
                diagnostics,
                version: None,
            };
            notification(
                "textDocument/publishDiagnostics",
                serde_json::to_value(params).unwrap_or(Value::Null),
            )
        })
        .collect()
}

fn parse_params<T: serde::de::DeserializeOwned>(params: Value) -> Result<T, RequestError> {
    serde_json::from_value(params).map_err(|e| RequestError {
        code: INVALID_PARAMS,
        message: format!("invalid params: {e}"),
    })
}

fn to_result<T: serde::Serialize>(value: T) -> RequestResult {
    serde_json::to_value(value).map_err(|e| RequestError {
        code: INTERNAL_ERROR,
        message: format!("failed to serialize response: {e}"),
    })
}

// --------------------------------------------------------------------------
// Free helpers
// --------------------------------------------------------------------------

/// Extract the client-side commands advertised in the `experimental` client
/// capability: `{"experimental": {"commands": ["zorglub33.run", ...]}}`.
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

/// Parse the params of the `zorglub33/workspaceFiles` notification into a
/// relative path -> content map. Unknown or malformed entries are silently
/// ignored.
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
