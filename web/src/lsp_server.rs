//! WASM bridge for the Zorglub33 Language Server.
//!
//! Wraps the runtime-agnostic tower-lsp [`Backend`] and exposes a plain
//! JSON message-passing interface suitable for `@codemirror/lsp-client`'s
//! `Transport`. Messages are **unframed** JSON-RPC strings — there is no
//! `Content-Length` header; the JS transport is responsible for delivery.
//!
//! Contract:
//! * `new(on_message)` — `on_message` is a JS function called with a single
//!   JSON string for every server-to-client message (notifications such as
//!   `textDocument/publishDiagnostics`, server-initiated requests, and the
//!   responses to client requests).
//! * `send(message)` — feed one client-to-server JSON-RPC string. Requests and
//!   notifications are routed to the service; their responses (if any) are
//!   delivered back through `on_message`. Client responses to server-initiated
//!   requests are routed to the response sink.

use futures::channel::mpsc;
use futures::{SinkExt, StreamExt};
use tower::Service;
use wasm_bindgen::prelude::*;
use z33_emulator::lsp::tower_lsp::jsonrpc::{Request, Response};
use z33_emulator::lsp::tower_lsp::LspService;
use z33_emulator::lsp::{Backend, WORKSPACE_FILES_METHOD};

#[wasm_bindgen]
extern "C" {
    /// JS callback for every server-to-client message (JSON string).
    #[wasm_bindgen(typescript_type = "(message: string) => void")]
    pub type MessageCallback;
}

/// A Language Server Protocol server running in WASM.
#[wasm_bindgen]
pub struct WasmLspServer {
    service: LspService<Backend>,
    /// Sink used to route client responses back to server-initiated requests.
    response_tx: mpsc::UnboundedSender<Response>,
    on_message: js_sys::Function,
}

#[wasm_bindgen]
impl WasmLspServer {
    /// Create a new WASM LSP server.
    ///
    /// `on_message` is invoked with a JSON string for every message the server
    /// wants to deliver to the client.
    #[wasm_bindgen(constructor)]
    #[must_use]
    pub fn new(on_message: MessageCallback) -> Self {
        let on_message: js_sys::Function = on_message.unchecked_into();

        // Register the custom multi-file workspace notification, exactly like
        // the native CLI does (see cli/src/commands/lsp.rs).
        let (service, socket) = LspService::build(Backend::new)
            .custom_method(WORKSPACE_FILES_METHOD, Backend::workspace_files)
            .finish();

        let (requests, responses) = socket.split();

        // Forward server-to-client messages (notifications + server requests)
        // to the JS callback.
        let forward_cb = on_message.clone();
        wasm_bindgen_futures::spawn_local(async move {
            futures::pin_mut!(requests);
            while let Some(request) = requests.next().await {
                match serde_json::to_string(&request) {
                    Ok(json) => {
                        let _ = forward_cb.call1(&JsValue::NULL, &JsValue::from_str(&json));
                    }
                    Err(e) => {
                        tracing::error!(
                            error = %e,
                            "failed to serialize server-to-client message; dropping it"
                        );
                    }
                }
            }
        });

        // Pump client responses (to server-initiated requests) into the socket.
        let (response_tx, response_rx) = mpsc::unbounded::<Response>();
        wasm_bindgen_futures::spawn_local(async move {
            let mut responses = responses;
            let mut response_rx = response_rx;
            while let Some(response) = response_rx.next().await {
                if responses.send(response).await.is_err() {
                    break;
                }
            }
        });

        Self {
            service,
            response_tx,
            on_message,
        }
    }

    /// Feed one client-to-server JSON-RPC message.
    ///
    /// Requests/notifications are dispatched to the service; any response is
    /// delivered through the `on_message` callback. Messages that are responses
    /// to server-initiated requests are routed to the response sink.
    ///
    /// # Re-entrancy invariant
    ///
    /// This method holds `&mut self` across `self.service.call(...).await`. A
    /// `Backend` handler must therefore **never** await a server-to-client
    /// *request* (e.g. `self.client.workspace_folders()` and friends): the
    /// client's reply would arrive via a second `send` call, which cannot run
    /// while the first `send` still borrows `self` — an immediate deadlock.
    /// Server-to-client *notifications* (such as `publish_diagnostics`) are
    /// fire-and-forget and remain safe. This is currently upheld because
    /// `Backend` never touches `self.client`; keep it that way, or rework this
    /// bridge to stop borrowing `self` across the `await` first.
    ///
    /// # Errors
    ///
    /// Returns an error if the message is not valid JSON-RPC.
    pub async fn send(&mut self, message: String) -> Result<(), JsValue> {
        let value: serde_json::Value = serde_json::from_str(&message)
            .map_err(|e| JsValue::from_str(&format!("Invalid JSON-RPC message: {e}")))?;

        // A message with a "method" field is a request or a notification; one
        // without is a response to a server-initiated request.
        if value.get("method").is_some() {
            let request: Request = serde_json::from_value(value)
                .map_err(|e| JsValue::from_str(&format!("Invalid JSON-RPC request: {e}")))?;

            // tower services must be polled ready before being called.
            futures::future::poll_fn(|cx| self.service.poll_ready(cx))
                .await
                .map_err(|e| JsValue::from_str(&format!("LSP service error: {e}")))?;

            let response = self
                .service
                .call(request)
                .await
                .map_err(|e| JsValue::from_str(&format!("LSP service error: {e}")))?;

            if let Some(response) = response {
                match serde_json::to_string(&response) {
                    Ok(json) => {
                        let _ = self
                            .on_message
                            .call1(&JsValue::NULL, &JsValue::from_str(&json));
                    }
                    Err(e) => {
                        tracing::error!(
                            error = %e,
                            "failed to serialize response to client request; dropping it"
                        );
                    }
                }
            }
        } else {
            let response: Response = serde_json::from_value(value)
                .map_err(|e| JsValue::from_str(&format!("Invalid JSON-RPC response: {e}")))?;
            self.response_tx
                .unbounded_send(response)
                .map_err(|e| JsValue::from_str(&format!("Response channel closed: {e}")))?;
        }

        Ok(())
    }
}
