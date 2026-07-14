//! WASM bridge for the Zorglub33 Language Server.
//!
//! Thin synchronous wrapper over the transport-agnostic [`LspSession`],
//! mirroring `WasmDapServer` (see `dap_server.rs`). The JS host
//! owns the transport; messages are **unframed** JSON-RPC strings — there is
//! no `Content-Length` header.
//!
//! Contract:
//! * `new(on_message)` — `on_message` is a JS function called with a single
//!   JSON string for every server-to-client message (notifications such as
//!   `textDocument/publishDiagnostics` and the responses to client requests).
//! * `send(message)` — feed one client-to-server JSON-RPC string; every
//!   resulting message is delivered through `on_message` synchronously, before
//!   `send` returns. Client responses to server-initiated requests are ignored
//!   (this server never sends requests).

use wasm_bindgen::prelude::*;
use z33_emulator::lsp::LspSession;

#[wasm_bindgen]
extern "C" {
    /// JS callback for every server-to-client message (JSON string).
    #[wasm_bindgen(typescript_type = "(message: string) => void")]
    pub type MessageCallback;
}

/// A Language Server Protocol server running in WASM.
#[wasm_bindgen]
pub struct WasmLspServer {
    session: LspSession,
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
        Self {
            session: LspSession::new(),
            on_message: on_message.unchecked_into(),
        }
    }

    /// Feed one client-to-server JSON-RPC message.
    ///
    /// The resulting server-to-client messages (the response for a request,
    /// plus any `textDocument/publishDiagnostics` notifications) are delivered
    /// through the `on_message` callback before this returns.
    ///
    /// # Errors
    ///
    /// Returns an error if the message is not valid JSON.
    pub fn send(&mut self, message: &str) -> Result<(), JsValue> {
        let value: serde_json::Value = serde_json::from_str(message)
            .map_err(|e| JsValue::from_str(&format!("Invalid JSON-RPC message: {e}")))?;

        for out in self.session.handle_message(&value) {
            match serde_json::to_string(&out) {
                Ok(json) => {
                    let _ = self
                        .on_message
                        .call1(&JsValue::NULL, &JsValue::from_str(&json));
                }
                Err(e) => {
                    tracing::error!(
                        error = %e,
                        "failed to serialize server-to-client message; dropping it"
                    );
                }
            }
        }

        Ok(())
    }
}
