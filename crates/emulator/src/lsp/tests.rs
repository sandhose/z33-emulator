//! Protocol-level tests driving an [`LspSession`] through raw JSON-RPC
//! messages, mirroring the style of [`crate::dap::tests`].

use pretty_assertions::assert_eq;
use serde_json::{json, Value};

use super::LspSession;

const ROOT_URI: &str = "file:///ws";

/// A tiny harness wrapping a session and a monotonic request-id counter.
struct Harness {
    session: LspSession,
    id: i64,
}

impl Harness {
    fn new() -> Self {
        Self {
            session: LspSession::new(),
            id: 0,
        }
    }

    // By-value keeps the many `request("x", json!({...}))` call sites free of
    // `&` noise; the harness does not care about the extra move.
    #[allow(clippy::needless_pass_by_value)]
    fn request(&mut self, method: &str, params: Value) -> Vec<Value> {
        self.id += 1;
        let msg = json!({
            "jsonrpc": "2.0",
            "id": self.id,
            "method": method,
            "params": params,
        });
        self.session.handle_message(&msg)
    }

    #[allow(clippy::needless_pass_by_value)]
    fn notify(&mut self, method: &str, params: Value) -> Vec<Value> {
        let msg = json!({
            "jsonrpc": "2.0",
            "method": method,
            "params": params,
        });
        self.session.handle_message(&msg)
    }

    /// Run the standard `initialize`/`initialized` handshake and return the
    /// `initialize` result.
    fn initialize(&mut self) -> Value {
        let out = self.request(
            "initialize",
            json!({
                "capabilities": {},
                "rootUri": ROOT_URI,
            }),
        );
        assert_eq!(out.len(), 1);
        let result = out[0]["result"].clone();
        assert!(!result.is_null(), "initialize must return a result");
        self.notify("initialized", json!({}));
        result
    }

    fn open(&mut self, name: &str, text: &str) -> Vec<Value> {
        self.notify(
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": format!("{ROOT_URI}/{name}"),
                    "languageId": "zorglub33-assembly",
                    "version": 1,
                    "text": text,
                }
            }),
        )
    }
}

/// Find the `textDocument/publishDiagnostics` notification for a URI.
fn find_diagnostics<'a>(messages: &'a [Value], uri: &str) -> Option<&'a Value> {
    messages
        .iter()
        .find(|m| m["method"] == "textDocument/publishDiagnostics" && m["params"]["uri"] == uri)
}

#[test]
fn initialize_advertises_capabilities() {
    let mut h = Harness::new();
    let result = h.initialize();

    let caps = &result["capabilities"];
    assert_eq!(caps["textDocumentSync"], 1, "full sync");
    assert_eq!(
        caps["completionProvider"]["triggerCharacters"],
        json!(["%", "."])
    );
    assert_eq!(caps["hoverProvider"], true);
    assert_eq!(caps["definitionProvider"], true);
    assert_eq!(caps["referencesProvider"], true);
    assert_eq!(caps["documentHighlightProvider"], true);
    assert_eq!(caps["documentSymbolProvider"], true);
    assert!(caps["codeLensProvider"].is_object());
    assert_eq!(caps["renameProvider"]["prepareProvider"], true);
    assert_eq!(caps["semanticTokensProvider"]["full"], true);
    assert!(caps["semanticTokensProvider"]["legend"]["tokenTypes"]
        .as_array()
        .is_some_and(|t| !t.is_empty()));

    assert_eq!(result["serverInfo"]["name"], "zorglub33-lsp");
}

#[test]
fn responses_echo_the_request_id() {
    let mut h = Harness::new();
    h.initialize();

    // String ids must round-trip untouched.
    let out = h.session.handle_message(&json!({
        "jsonrpc": "2.0",
        "id": "abc-1",
        "method": "shutdown",
    }));
    assert_eq!(out.len(), 1);
    assert_eq!(out[0]["jsonrpc"], "2.0");
    assert_eq!(out[0]["id"], "abc-1");
    assert_eq!(out[0]["result"], Value::Null);
}

#[test]
fn did_open_with_broken_program_publishes_diagnostics() {
    let mut h = Harness::new();
    h.initialize();

    let out = h.open("main.s", "    $$bad$$\n");
    let uri = format!("{ROOT_URI}/main.s");
    let diags = find_diagnostics(&out, &uri).expect("diagnostics published");
    assert!(
        diags["params"]["diagnostics"]
            .as_array()
            .is_some_and(|d| !d.is_empty()),
        "broken program should carry diagnostics"
    );

    // Fixing the program re-publishes an empty list to clear them.
    let out = h.notify(
        "textDocument/didChange",
        json!({
            "textDocument": { "uri": uri, "version": 2 },
            "contentChanges": [{ "text": "    reset\n" }],
        }),
    );
    let diags = find_diagnostics(&out, &uri).expect("clearing publish");
    assert_eq!(diags["params"]["diagnostics"], json!([]));
}

#[test]
fn workspace_files_seed_cross_file_goto_definition() {
    let mut h = Harness::new();
    h.initialize();

    let main_src = "#include \"util.s\"\n    jmp target\n";
    h.open("main.s", main_src);
    h.notify(
        super::WORKSPACE_FILES_METHOD,
        json!({ "files": { "util.s": "target:\n    reset\n" } }),
    );

    // `target` on line 1 (`    jmp target`), character 8.
    let out = h.request(
        "textDocument/definition",
        json!({
            "textDocument": { "uri": format!("{ROOT_URI}/main.s") },
            "position": { "line": 1, "character": 8 },
        }),
    );
    assert_eq!(out.len(), 1);
    let location = &out[0]["result"];
    assert_eq!(location["uri"], format!("{ROOT_URI}/util.s"));
    assert_eq!(location["range"]["start"]["line"], 0);
    assert_eq!(location["range"]["start"]["character"], 0);
}

#[test]
fn duplicate_initialize_is_rejected_and_preserves_state() {
    let mut h = Harness::new();
    h.initialize();

    let main_src = "#include \"util.s\"\n    jmp target\n";
    h.open("main.s", main_src);
    h.notify(
        super::WORKSPACE_FILES_METHOD,
        json!({ "files": { "util.s": "target:\n    reset\n" } }),
    );

    // A second `initialize` (with a different root) must be rejected with
    // InvalidRequest, exactly like tower-lsp did...
    let out = h.request(
        "initialize",
        json!({ "capabilities": {}, "rootUri": "file:///other" }),
    );
    assert_eq!(out.len(), 1);
    assert_eq!(out[0]["error"]["code"], -32600);

    // ...and must not have touched the session state: cross-file
    // go-to-definition still resolves against the original workspace root.
    let out = h.request(
        "textDocument/definition",
        json!({
            "textDocument": { "uri": format!("{ROOT_URI}/main.s") },
            "position": { "line": 1, "character": 8 },
        }),
    );
    assert_eq!(out[0]["result"]["uri"], format!("{ROOT_URI}/util.s"));
}

#[test]
fn requests_before_initialize_are_rejected() {
    let mut h = Harness::new();
    let out = h.request("textDocument/hover", json!({}));
    assert_eq!(out.len(), 1);
    assert_eq!(out[0]["error"]["code"], -32002);
}

#[test]
fn notifications_before_initialize_are_dropped() {
    let mut h = Harness::new();
    let out = h.open("main.s", "    $$bad$$\n");
    assert!(out.is_empty(), "no diagnostics before initialize");
}

#[test]
fn unknown_request_method_is_method_not_found() {
    let mut h = Harness::new();
    h.initialize();
    let out = h.request("textDocument/signatureHelp", json!({}));
    assert_eq!(out.len(), 1);
    assert_eq!(out[0]["error"]["code"], -32601);
}

#[test]
fn unknown_notifications_are_ignored() {
    let mut h = Harness::new();
    h.initialize();
    assert!(h.notify("$/cancelRequest", json!({ "id": 1 })).is_empty());
    assert!(h.notify("$/setTrace", json!({ "value": "off" })).is_empty());
}

#[test]
fn responses_to_server_requests_are_ignored() {
    let mut h = Harness::new();
    h.initialize();
    // A client response has an id but no method; the server never issues
    // requests, so this must be silently dropped, not crash.
    let out = h
        .session
        .handle_message(&json!({ "jsonrpc": "2.0", "id": 7, "result": null }));
    assert!(out.is_empty());
}

#[test]
fn shutdown_and_exit_lifecycle() {
    let mut h = Harness::new();
    h.initialize();

    let out = h.request("shutdown", json!(null));
    assert_eq!(out.len(), 1);
    assert_eq!(out[0]["result"], Value::Null);
    assert!(out[0].get("error").is_none());
    assert!(!h.session.exit_requested());

    // After shutdown every request is invalid...
    let out = h.request("textDocument/hover", json!({}));
    assert_eq!(out[0]["error"]["code"], -32600);

    // ...but `exit` still goes through and flags the transport to stop.
    let out = h.notify("exit", json!(null));
    assert!(out.is_empty());
    assert!(h.session.exit_requested());
}

#[test]
fn hover_returns_markdown_contents() {
    let mut h = Harness::new();
    h.initialize();
    h.open("main.s", "main:\n    reset\n");

    // Hover over the `reset` mnemonic (line 1, character 5).
    let out = h.request(
        "textDocument/hover",
        json!({
            "textDocument": { "uri": format!("{ROOT_URI}/main.s") },
            "position": { "line": 1, "character": 5 },
        }),
    );
    assert_eq!(out.len(), 1);
    let result = &out[0]["result"];
    assert_eq!(result["contents"]["kind"], "markdown");
    assert!(result["contents"]["value"]
        .as_str()
        .is_some_and(|v| !v.is_empty()));
    assert!(result["range"].is_object());

    // Hovering a document the server does not know yields a null result, not
    // an error.
    let out = h.request(
        "textDocument/hover",
        json!({
            "textDocument": { "uri": format!("{ROOT_URI}/unknown.s") },
            "position": { "line": 0, "character": 0 },
        }),
    );
    assert_eq!(out[0]["result"], Value::Null);
}

#[test]
fn invalid_params_yield_invalid_params_error() {
    let mut h = Harness::new();
    h.initialize();
    let out = h.request("textDocument/hover", json!({ "bogus": true }));
    assert_eq!(out.len(), 1);
    assert_eq!(out[0]["error"]["code"], -32602);
}
