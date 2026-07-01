//! WASM bridge for the Z33 Debug Adapter.
//!
//! Thin synchronous wrapper over the transport-agnostic [`DebugSession`]. The
//! JS host owns the transport and framing; this only decodes/encodes DAP
//! messages as plain JS objects (no `Content-Length` header).
//!
//! Contract mirrors [`DebugSession`]:
//! * `handle_message(msg)` — feed one decoded client DAP message (a JS object);
//!   returns an array of complete, sequence-stamped DAP messages to emit.
//! * `run_chunk()` — advance a *running* program by a bounded number of
//!   instructions; returns the resulting DAP messages (empty while still
//!   running).
//! * `is_running()` / `exit_requested()` — drive/terminate the run loop.
//!
//! Launch requests must always carry an inline `files` map (path → content) so
//! the session uses the in-memory filesystem — WASM hosts have no disk.

use serde::Serialize;
use wasm_bindgen::prelude::*;
use z33_emulator::dap::DebugSession;

/// A Debug Adapter Protocol session running in WASM.
#[wasm_bindgen]
pub struct WasmDapServer {
    session: DebugSession,
}

impl Default for WasmDapServer {
    fn default() -> Self {
        Self::new()
    }
}

#[wasm_bindgen]
impl WasmDapServer {
    #[wasm_bindgen(constructor)]
    #[must_use]
    pub fn new() -> Self {
        Self {
            session: DebugSession::new(),
        }
    }

    /// Handle one decoded client DAP message.
    ///
    /// # Errors
    ///
    /// Returns an error if `message` cannot be decoded into a JSON value or the
    /// responses cannot be serialized back to JS.
    pub fn handle_message(&mut self, message: JsValue) -> Result<Vec<JsValue>, JsValue> {
        let value: serde_json::Value = serde_wasm_bindgen::from_value(message)
            .map_err(|e| JsValue::from_str(&format!("Invalid DAP message: {e}")))?;
        let responses = self.session.handle_message(&value);
        to_js_array(&responses)
    }

    /// Advance a running program by one bounded chunk of instructions.
    ///
    /// # Errors
    ///
    /// Returns an error if the emitted messages cannot be serialized to JS.
    pub fn run_chunk(&mut self) -> Result<Vec<JsValue>, JsValue> {
        let responses = self.session.run_chunk();
        to_js_array(&responses)
    }

    /// Whether the program is running and the caller should keep calling
    /// [`run_chunk`](Self::run_chunk).
    #[must_use]
    pub fn is_running(&self) -> bool {
        self.session.is_running()
    }

    /// Whether the client asked to disconnect and the transport should stop.
    #[must_use]
    pub fn exit_requested(&self) -> bool {
        self.session.exit_requested()
    }
}

fn to_js_array(values: &[serde_json::Value]) -> Result<Vec<JsValue>, JsValue> {
    // DAP messages must be plain JS objects (the VS Code inline debug adapter
    // reads properties like `type`/`event`/`command` directly). Without this,
    // `serde_wasm_bindgen` would emit ES `Map`s and the host would silently
    // drop every event.
    let serializer = serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true);
    values
        .iter()
        .map(|v| {
            v.serialize(&serializer)
                .map_err(|e| JsValue::from_str(&format!("DAP serialization error: {e}")))
        })
        .collect()
}
