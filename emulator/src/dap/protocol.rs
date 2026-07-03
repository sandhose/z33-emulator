//! Serde types for the subset of the Debug Adapter Protocol implemented here.
//!
//! Only the request arguments, response/event bodies and shared structures that
//! the [`DebugSession`](super::DebugSession) actually uses are modelled. The
//! wire envelopes (`seq`, `type`, `request_seq`, `success`, ...) are assembled
//! in [`super`] so that sequence numbers stay centralised.

use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use serde_json::Value;

/// An incoming DAP message. The adapter only ever receives requests from the
/// client, so `arguments` is kept as a raw [`Value`] and deserialized per
/// command.
#[derive(Debug, Deserialize)]
pub struct IncomingRequest {
    pub seq: i64,
    #[serde(rename = "type")]
    pub kind: String,
    pub command: String,
    #[serde(default)]
    pub arguments: Value,
}

/// Capabilities advertised in the `initialize` response.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
#[allow(clippy::struct_excessive_bools)] // mirrors the DAP capabilities schema
pub struct Capabilities {
    pub supports_configuration_done_request: bool,
    pub supports_set_variable: bool,
    pub supports_evaluate_for_hovers: bool,
    pub supports_restart_request: bool,
    pub supports_terminate_request: bool,
    /// DAP "stepping granularity" means the client may attach a
    /// `statement`/`line`/`instruction` granularity to step requests — it is
    /// *not* step-over versus step-into (those are the separate `next` and
    /// `stepIn` requests). We advertise `false` because a Z33 source line is
    /// always exactly one instruction, so the granularities are
    /// indistinguishable.
    pub supports_stepping_granularity: bool,
    pub supports_read_memory_request: bool,
    pub supports_write_memory_request: bool,
}

impl Default for Capabilities {
    fn default() -> Self {
        Self {
            supports_configuration_done_request: true,
            supports_set_variable: true,
            supports_evaluate_for_hovers: true,
            supports_restart_request: true,
            supports_terminate_request: true,
            supports_stepping_granularity: false,
            supports_read_memory_request: true,
            supports_write_memory_request: true,
        }
    }
}

/// A value-formatting hint carried by `variables`, `evaluate` and
/// `setVariable`. Only the `hex` flag is honoured.
#[derive(Debug, Clone, Copy, Default, Deserialize)]
pub struct ValueFormat {
    #[serde(default)]
    pub hex: bool,
}

/// A source reference (subset).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Source {
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub path: Option<String>,
}

/// Arguments to the `launch` request.
///
/// `program` is the entry file. When `files` is present the adapter reads from
/// that in-memory map (used by the future WASM host); otherwise it falls back
/// to the native filesystem. `entrypoint` selects the label whose address the
/// program counter starts at; if omitted a common default is used.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LaunchArguments {
    pub program: String,
    #[serde(default)]
    pub entrypoint: Option<String>,
    #[serde(default)]
    pub stop_on_entry: bool,
    #[serde(default)]
    pub files: Option<HashMap<String, String>>,
}

/// A breakpoint requested by the client.
#[derive(Debug, Deserialize)]
pub struct SourceBreakpoint {
    pub line: u32,
    #[serde(default)]
    #[allow(dead_code)]
    pub column: Option<u32>,
}

/// Arguments to `setBreakpoints`.
#[derive(Debug, Deserialize)]
pub struct SetBreakpointsArguments {
    pub source: Source,
    #[serde(default)]
    pub breakpoints: Vec<SourceBreakpoint>,
}

/// A breakpoint as reported back to the client.
#[derive(Debug, Serialize)]
pub struct Breakpoint {
    pub verified: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub line: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<Source>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
}

/// A stack frame in a `stackTrace` response.
#[derive(Debug, Serialize)]
pub struct StackFrame {
    pub id: i64,
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<Source>,
    pub line: u32,
    pub column: u32,
}

/// A variable scope.
#[derive(Debug, Default, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Scope {
    pub name: String,
    pub variables_reference: i64,
    pub expensive: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub named_variables: Option<i64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub indexed_variables: Option<i64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub presentation_hint: Option<String>,
}

/// A single variable.
#[derive(Debug, Default, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Variable {
    pub name: String,
    pub value: String,
    #[serde(rename = "type", skip_serializing_if = "Option::is_none")]
    pub kind: Option<String>,
    pub variables_reference: i64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub memory_reference: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub named_variables: Option<i64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub indexed_variables: Option<i64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub presentation_hint: Option<Value>,
}

/// Arguments to `scopes`.
#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ScopesArguments {
    #[serde(default)]
    pub frame_id: i64,
}

/// Arguments to `variables`.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct VariablesArguments {
    pub variables_reference: i64,
    #[serde(default)]
    pub start: Option<i64>,
    #[serde(default)]
    pub count: Option<i64>,
    #[serde(default)]
    pub format: Option<ValueFormat>,
}

/// Arguments to `setVariable`.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SetVariableArguments {
    pub variables_reference: i64,
    pub name: String,
    pub value: String,
    #[serde(default)]
    pub format: Option<ValueFormat>,
}

/// Arguments to `evaluate`.
#[derive(Debug, Deserialize)]
pub struct EvaluateArguments {
    pub expression: String,
    #[serde(default)]
    #[allow(dead_code)]
    pub context: Option<String>,
    #[serde(default)]
    pub format: Option<ValueFormat>,
}

/// Arguments to `readMemory`.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ReadMemoryArguments {
    pub memory_reference: String,
    #[serde(default)]
    pub offset: i64,
    pub count: i64,
}

/// Arguments to `writeMemory`.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WriteMemoryArguments {
    pub memory_reference: String,
    #[serde(default)]
    pub offset: i64,
    pub data: String,
    #[serde(default)]
    pub allow_partial: bool,
}

/// Arguments to `source`.
#[derive(Debug, Deserialize)]
pub struct SourceArguments {
    #[serde(default)]
    pub source: Option<Source>,
}
