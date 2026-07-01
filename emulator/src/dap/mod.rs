//! A transport-agnostic Debug Adapter Protocol (DAP) implementation for the
//! Z33 emulator.
//!
//! # Design
//!
//! [`DebugSession`] contains **no I/O**. The caller owns the transport (stdio
//! for the native CLI, a JS bridge for the future WASM host) and does the
//! framing. The session is driven through three entry points:
//!
//! * [`DebugSession::handle_message`] — feed it one decoded client message (a
//!   [`serde_json::Value`]); it returns the responses and events to emit, each
//!   a complete, sequence-stamped DAP message ready to serialize.
//! * [`DebugSession::run_chunk`] — advance a *running* program by a bounded
//!   number of instructions. Requests that start execution (`continue`, `next`,
//!   `stepIn`, `stepOut`) only set the run mode and return immediately; the
//!   actual stepping happens here so the transport is never blocked.
//! * [`DebugSession::is_running`] — whether the caller should keep calling
//!   `run_chunk`.
//!
//! ## Chunked-run contract
//!
//! ```text
//! loop {
//!     if session.is_running() {
//!         match poll_incoming_nonblocking() {
//!             Some(msg) => emit(session.handle_message(&msg)),   // e.g. pause
//!             None      => emit(session.run_chunk()),            // advance
//!         }
//!     } else {
//!         emit(session.handle_message(&recv_blocking()));
//!     }
//! }
//! ```
//!
//! `run_chunk` executes at most [`CHUNK_SIZE`] instructions. If the program
//! stops (breakpoint, step completion, halt, exception, or a pending pause) it
//! returns the corresponding events and clears the running state. If the budget
//! is exhausted without stopping it returns an empty vector and stays running,
//! giving the caller a chance to poll for an incoming `pause`.

mod index;
pub mod protocol;

use std::collections::{BTreeMap, HashMap, HashSet};
use std::str::FromStr;

use camino::{Utf8Path, Utf8PathBuf};
use serde_json::{json, Map, Value};

use self::index::LineIndex;
use self::protocol::{
    Breakpoint, Capabilities, EvaluateArguments, IncomingRequest, LaunchArguments, Scope,
    SetBreakpointsArguments, SetVariableArguments, Source, SourceArguments, StackFrame, Variable,
    VariablesArguments,
};
use crate::constants as C;
use crate::constants::Address;
use crate::diagnostic::{
    preprocessor_error_to_diagnostics, render_to_string, resolve_diagnostic_spans,
};
use crate::preprocessor::{
    Filesystem, InMemoryFilesystem, NativeFilesystem, ReferencingSourceMap, Workspace,
};
use crate::runtime::registers::StatusRegister;
use crate::runtime::{Cell, Computer, Instruction, ProcessorError, Reg};

/// Maximum number of instructions executed per [`DebugSession::run_chunk`].
pub const CHUNK_SIZE: usize = 10_000;

/// The single thread exposed to the client.
const THREAD_ID: i64 = 1;
/// `variablesReference` for the registers scope.
const REGISTERS_REF: i64 = 1;
/// `variablesReference` for the status-register flags.
const FLAGS_REF: i64 = 2;
/// `variablesReference` for the execution scope (cycle counter and other
/// run-state that is not an architectural register).
const EXECUTION_REF: i64 = 3;
/// Upper bound on synthesized caller frames.
const MAX_FRAMES: usize = 64;

const EXIT_OK: i64 = 0;
const EXIT_EXCEPTION: i64 = 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum State {
    Uninitialized,
    Initialized,
    Running,
    Stopped,
    Terminated,
}

#[derive(Debug, Clone, Copy)]
enum RunMode {
    Continue,
    StepIn,
    /// Step over: run until the stack pointer returns to (>=) this value.
    StepOver {
        sp: Address,
    },
    /// Step out: run until the stack pointer rises above this value.
    StepOut {
        sp: Address,
    },
}

#[derive(Debug, Clone, Copy)]
enum StopReason {
    Entry,
    Breakpoint,
    Step,
    Pause,
}

impl StopReason {
    fn as_str(self) -> &'static str {
        match self {
            StopReason::Entry => "entry",
            StopReason::Breakpoint => "breakpoint",
            StopReason::Step => "step",
            StopReason::Pause => "pause",
        }
    }
}

/// Outcome of executing an instruction budget.
enum Outcome {
    /// Budget exhausted, still running.
    Pending,
    Stopped(StopReason),
    Terminated(i64),
    Exception(String),
}

/// A compiled, runnable program plus everything needed to debug it.
struct LoadedProgram {
    computer: Computer,
    index: LineIndex,
    labels: BTreeMap<String, Address>,
    /// Breakpoint addresses per source path (last `setBreakpoints` wins).
    bp_by_file: HashMap<String, Vec<Address>>,
    /// Union of all breakpoint addresses.
    breakpoints: HashSet<Address>,
}

impl LoadedProgram {
    fn recompute_breakpoints(&mut self) {
        self.breakpoints = self.bp_by_file.values().flatten().copied().collect();
    }
}

/// A DAP debug session.
#[allow(clippy::struct_excessive_bools)] // independent protocol/session flags
pub struct DebugSession {
    seq: i64,
    state: State,
    configured: bool,
    entered: bool,
    stop_on_entry: bool,
    exit_requested: bool,
    exception_pending: bool,
    pause_requested: bool,
    run_mode: RunMode,
    program: Option<LoadedProgram>,
    launch_args: Option<LaunchArguments>,
}

impl Default for DebugSession {
    fn default() -> Self {
        Self::new()
    }
}

impl DebugSession {
    #[must_use]
    pub fn new() -> Self {
        Self {
            seq: 0,
            state: State::Uninitialized,
            configured: false,
            entered: false,
            stop_on_entry: false,
            exit_requested: false,
            exception_pending: false,
            pause_requested: false,
            run_mode: RunMode::Continue,
            program: None,
            launch_args: None,
        }
    }

    /// Whether the program is currently running and the caller should keep
    /// calling [`run_chunk`](Self::run_chunk).
    #[must_use]
    pub fn is_running(&self) -> bool {
        self.state == State::Running
    }

    /// Whether the client has asked to disconnect and the transport should
    /// shut down.
    #[must_use]
    pub fn exit_requested(&self) -> bool {
        self.exit_requested
    }

    // ----------------------------------------------------------------------
    // Message handling
    // ----------------------------------------------------------------------

    /// Handle one decoded client message, returning the messages to emit.
    #[must_use]
    pub fn handle_message(&mut self, message: &Value) -> Vec<Value> {
        let Ok(req) = serde_json::from_value::<IncomingRequest>(message.clone()) else {
            return Vec::new();
        };
        if req.kind != "request" {
            return Vec::new();
        }

        match req.command.as_str() {
            "initialize" => self.on_initialize(&req),
            "launch" => self.on_launch(&req),
            "setBreakpoints" => self.on_set_breakpoints(&req),
            "setExceptionBreakpoints" => {
                vec![self.response(&req, json!({ "breakpoints": [] }))]
            }
            "configurationDone" => self.on_configuration_done(&req),
            "threads" => {
                vec![self.response(
                    &req,
                    json!({ "threads": [{ "id": THREAD_ID, "name": "CPU" }] }),
                )]
            }
            "stackTrace" => self.on_stack_trace(&req),
            "scopes" => self.on_scopes(&req),
            "variables" => self.on_variables(&req),
            "setVariable" => self.on_set_variable(&req),
            "continue" => self.on_continue(&req),
            "next" => self.on_next(&req),
            "stepIn" => self.on_step_in(&req),
            "stepOut" => self.on_step_out(&req),
            "pause" => self.on_pause(&req),
            "evaluate" => self.on_evaluate(&req),
            "source" => self.on_source(&req),
            "restart" => self.on_restart(&req),
            "disconnect" => {
                self.exit_requested = true;
                self.state = State::Terminated;
                vec![self.response(&req, Value::Null)]
            }
            "terminate" => {
                self.state = State::Terminated;
                let resp = self.response(&req, Value::Null);
                vec![resp, self.make_event("terminated", Value::Null)]
            }
            other => {
                vec![self.response_err(&req, format!("unsupported request: {other}"))]
            }
        }
    }

    // ----------------------------------------------------------------------
    // Individual request handlers
    // ----------------------------------------------------------------------

    fn on_initialize(&mut self, req: &IncomingRequest) -> Vec<Value> {
        self.state = State::Initialized;
        let caps = serde_json::to_value(Capabilities::default()).unwrap_or(Value::Null);
        let resp = self.response(req, caps);
        let ev = self.make_event("initialized", Value::Null);
        vec![resp, ev]
    }

    fn on_launch(&mut self, req: &IncomingRequest) -> Vec<Value> {
        let args: LaunchArguments = match serde_json::from_value(req.arguments.clone()) {
            Ok(a) => a,
            Err(e) => {
                return vec![self.response_err(req, format!("invalid launch arguments: {e}"))];
            }
        };

        match load_program(&args) {
            Ok(program) => {
                self.stop_on_entry = args.stop_on_entry;
                self.program = Some(program);
                self.launch_args = Some(args);
                let resp = self.response(req, Value::Null);
                let mut out = vec![resp];
                out.extend(self.maybe_enter());
                out
            }
            Err(msg) => vec![self.response_err(req, msg)],
        }
    }

    fn on_set_breakpoints(&mut self, req: &IncomingRequest) -> Vec<Value> {
        let args: SetBreakpointsArguments = match serde_json::from_value(req.arguments.clone()) {
            Ok(a) => a,
            Err(e) => return vec![self.response_err(req, format!("invalid arguments: {e}"))],
        };

        let body = {
            let Some(program) = self.program.as_mut() else {
                return vec![self.response_err(req, "no program launched")];
            };
            let source = args.source;
            let path = source
                .path
                .clone()
                .or_else(|| source.name.clone())
                .unwrap_or_default();

            let mut result: Vec<Breakpoint> = Vec::new();
            let mut addrs: Vec<Address> = Vec::new();
            for bp in args.breakpoints {
                match program.index.resolve_breakpoint(&path, bp.line) {
                    Some((line, address)) => {
                        addrs.push(address);
                        result.push(Breakpoint {
                            verified: true,
                            line: Some(line),
                            source: Some(source.clone()),
                            message: None,
                        });
                    }
                    None => result.push(Breakpoint {
                        verified: false,
                        line: Some(bp.line),
                        source: Some(source.clone()),
                        message: Some("no code at or after this line".to_owned()),
                    }),
                }
            }
            program.bp_by_file.insert(path, addrs);
            program.recompute_breakpoints();
            json!({ "breakpoints": result })
        };

        vec![self.response(req, body)]
    }

    fn on_configuration_done(&mut self, req: &IncomingRequest) -> Vec<Value> {
        self.configured = true;
        let resp = self.response(req, Value::Null);
        let mut out = vec![resp];
        out.extend(self.maybe_enter());
        out
    }

    fn on_stack_trace(&mut self, req: &IncomingRequest) -> Vec<Value> {
        if self.program.is_none() {
            return vec![self.response_err(req, "no program launched")];
        }
        let frames = self.stack_frames();
        let total = frames.len();
        let body = json!({ "stackFrames": frames, "totalFrames": total });
        vec![self.response(req, body)]
    }

    fn on_scopes(&mut self, req: &IncomingRequest) -> Vec<Value> {
        let scopes = vec![
            Scope {
                name: "Registers".to_owned(),
                variables_reference: REGISTERS_REF,
                expensive: false,
            },
            Scope {
                name: "Execution".to_owned(),
                variables_reference: EXECUTION_REF,
                expensive: false,
            },
        ];
        vec![self.response(req, json!({ "scopes": scopes }))]
    }

    fn on_variables(&mut self, req: &IncomingRequest) -> Vec<Value> {
        let args: VariablesArguments = match serde_json::from_value(req.arguments.clone()) {
            Ok(a) => a,
            Err(e) => return vec![self.response_err(req, format!("invalid arguments: {e}"))],
        };
        let Some(program) = self.program.as_ref() else {
            return vec![self.response_err(req, "no program launched")];
        };

        let variables = match args.variables_reference {
            REGISTERS_REF => registers_variables(&program.computer),
            FLAGS_REF => flags_variables(&program.computer),
            EXECUTION_REF => execution_variables(&program.computer),
            _ => Vec::new(),
        };
        vec![self.response(req, json!({ "variables": variables }))]
    }

    fn on_set_variable(&mut self, req: &IncomingRequest) -> Vec<Value> {
        let args: SetVariableArguments = match serde_json::from_value(req.arguments.clone()) {
            Ok(a) => a,
            Err(e) => return vec![self.response_err(req, format!("invalid arguments: {e}"))],
        };

        let outcome = {
            let Some(program) = self.program.as_mut() else {
                return vec![self.response_err(req, "no program launched")];
            };
            if args.variables_reference != REGISTERS_REF {
                Err("only register variables can be set".to_owned())
            } else if let Some(value) = parse_int(&args.value) {
                let reg = match args.name.as_str() {
                    "a" => Some(Reg::A),
                    "b" => Some(Reg::B),
                    "pc" => Some(Reg::PC),
                    "sp" => Some(Reg::SP),
                    _ => None,
                };
                match reg {
                    Some(reg) => program
                        .computer
                        .registers
                        .set(reg, Cell::Word(value))
                        .map(|()| format!("{}", program.computer.registers.get(&reg)))
                        .map_err(|e| e.to_string()),
                    None => Err(format!("cannot set variable '{}'", args.name)),
                }
            } else {
                Err(format!("invalid integer value '{}'", args.value))
            }
        };

        match outcome {
            Ok(value) => {
                vec![self.response(req, json!({ "value": value, "variablesReference": 0 }))]
            }
            Err(msg) => vec![self.response_err(req, msg)],
        }
    }

    /// Shared precondition check for the four resume requests (`continue`,
    /// `next`, `stepIn`, `stepOut`). Returns `Some(messages)` when the request
    /// must be answered immediately instead of starting execution:
    ///
    /// * no program is loaded, or the session has already terminated → error;
    /// * an unhandled exception is pending → the same terminate sequence
    ///   `continue` takes, since any resume after a stopped-on-exception ends
    ///   the session.
    ///
    /// `resume_body` is the success-response body used on the exception path
    /// (`continue` reports `allThreadsContinued`, the step requests use null).
    fn resume_precheck(&mut self, req: &IncomingRequest, resume_body: Value) -> Option<Vec<Value>> {
        if self.program.is_none() {
            return Some(vec![self.response_err(req, "no program launched")]);
        }
        if self.state == State::Terminated {
            return Some(vec![self.response_err(req, "program has terminated")]);
        }
        if self.exception_pending {
            self.exception_pending = false;
            let resp = self.response(req, resume_body);
            let mut out = vec![resp];
            out.extend(self.terminate_now(EXIT_EXCEPTION));
            return Some(out);
        }
        None
    }

    fn on_continue(&mut self, req: &IncomingRequest) -> Vec<Value> {
        if let Some(out) = self.resume_precheck(req, json!({ "allThreadsContinued": true })) {
            return out;
        }
        self.run_mode = RunMode::Continue;
        self.state = State::Running;
        vec![self.response(req, json!({ "allThreadsContinued": true }))]
    }

    fn on_next(&mut self, req: &IncomingRequest) -> Vec<Value> {
        if let Some(out) = self.resume_precheck(req, Value::Null) {
            return out;
        }
        let program = self.program.as_ref().expect("program loaded");
        let pc = program.computer.registers.pc;
        let sp = program.computer.registers.sp;
        let is_call = program
            .computer
            .memory
            .get(pc)
            .ok()
            .and_then(|c| c.extract_instruction().ok())
            .is_some_and(|i| matches!(i, Instruction::Call(_)));
        self.run_mode = if is_call {
            RunMode::StepOver { sp }
        } else {
            RunMode::StepIn
        };
        self.state = State::Running;
        vec![self.response(req, Value::Null)]
    }

    fn on_step_in(&mut self, req: &IncomingRequest) -> Vec<Value> {
        if let Some(out) = self.resume_precheck(req, Value::Null) {
            return out;
        }
        self.run_mode = RunMode::StepIn;
        self.state = State::Running;
        vec![self.response(req, Value::Null)]
    }

    fn on_step_out(&mut self, req: &IncomingRequest) -> Vec<Value> {
        if let Some(out) = self.resume_precheck(req, Value::Null) {
            return out;
        }
        let program = self.program.as_ref().expect("program loaded");
        let sp = program.computer.registers.sp;
        self.run_mode = RunMode::StepOut { sp };
        self.state = State::Running;
        vec![self.response(req, Value::Null)]
    }

    fn on_pause(&mut self, req: &IncomingRequest) -> Vec<Value> {
        if self.is_running() {
            self.pause_requested = true;
        }
        vec![self.response(req, Value::Null)]
    }

    fn on_evaluate(&mut self, req: &IncomingRequest) -> Vec<Value> {
        let args: EvaluateArguments = match serde_json::from_value(req.arguments.clone()) {
            Ok(a) => a,
            Err(e) => return vec![self.response_err(req, format!("invalid arguments: {e}"))],
        };

        let result = {
            let Some(program) = self.program.as_ref() else {
                return vec![self.response_err(req, "no program launched")];
            };
            evaluate_expression(program, args.expression.trim())
        };

        match result {
            Some((value, kind)) => vec![self.response(
                req,
                json!({ "result": value, "type": kind, "variablesReference": 0 }),
            )],
            None => vec![self.response_err(req, "could not evaluate expression")],
        }
    }

    fn on_source(&mut self, req: &IncomingRequest) -> Vec<Value> {
        let args: SourceArguments = match serde_json::from_value(req.arguments.clone()) {
            Ok(a) => a,
            Err(e) => return vec![self.response_err(req, format!("invalid arguments: {e}"))],
        };
        let content = {
            let Some(program) = self.program.as_ref() else {
                return vec![self.response_err(req, "no program launched")];
            };
            args.source
                .and_then(|s| s.path.or(s.name))
                .and_then(|p| program.index.source_of(&p).map(str::to_owned))
        };
        match content {
            Some(content) => vec![self.response(req, json!({ "content": content }))],
            None => vec![self.response_err(req, "source not found")],
        }
    }

    fn on_restart(&mut self, req: &IncomingRequest) -> Vec<Value> {
        let Some(args) = self.launch_args.clone() else {
            return vec![self.response_err(req, "nothing to restart")];
        };
        match load_program(&args) {
            Ok(program) => {
                self.program = Some(program);
                self.entered = false;
                self.exception_pending = false;
                self.pause_requested = false;
                self.stop_on_entry = args.stop_on_entry;
                self.state = State::Initialized;
                let resp = self.response(req, Value::Null);
                let mut out = vec![resp];
                out.extend(self.maybe_enter());
                out
            }
            Err(msg) => vec![self.response_err(req, msg)],
        }
    }

    // ----------------------------------------------------------------------
    // Execution driving
    // ----------------------------------------------------------------------

    /// Advance a running program by up to [`CHUNK_SIZE`] instructions.
    ///
    /// Returns the events produced if the program stopped or terminated, or an
    /// empty vector if the budget was exhausted while still running.
    #[must_use]
    pub fn run_chunk(&mut self) -> Vec<Value> {
        if self.state != State::Running {
            return Vec::new();
        }
        let outcome = self.execute_budget();
        self.finish_run(outcome)
    }

    fn execute_budget(&mut self) -> Outcome {
        if self.pause_requested {
            self.pause_requested = false;
            return Outcome::Stopped(StopReason::Pause);
        }
        let run_mode = self.run_mode;
        let Some(program) = self.program.as_mut() else {
            return Outcome::Terminated(EXIT_OK);
        };

        for _ in 0..CHUNK_SIZE {
            match program.computer.step() {
                Ok(()) => {
                    let pc = program.computer.registers.pc;
                    let sp = program.computer.registers.sp;
                    if program.breakpoints.contains(&pc) {
                        return Outcome::Stopped(StopReason::Breakpoint);
                    }
                    match run_mode {
                        RunMode::Continue => {}
                        RunMode::StepIn => return Outcome::Stopped(StopReason::Step),
                        RunMode::StepOver { sp: target } => {
                            if sp >= target {
                                return Outcome::Stopped(StopReason::Step);
                            }
                        }
                        RunMode::StepOut { sp: target } => {
                            if sp > target {
                                return Outcome::Stopped(StopReason::Step);
                            }
                        }
                    }
                }
                Err(ProcessorError::Reset) => return Outcome::Terminated(EXIT_OK),
                Err(e) => return Outcome::Exception(e.to_string()),
            }
        }
        Outcome::Pending
    }

    fn finish_run(&mut self, outcome: Outcome) -> Vec<Value> {
        match outcome {
            Outcome::Pending => Vec::new(),
            Outcome::Stopped(reason) => {
                self.state = State::Stopped;
                vec![self.stopped_event(reason, None, None)]
            }
            Outcome::Terminated(code) => self.terminate_now(code),
            Outcome::Exception(msg) => {
                self.state = State::Stopped;
                self.exception_pending = true;
                let output = self.make_event(
                    "output",
                    json!({ "category": "console", "output": format!("Exception: {msg}\n") }),
                );
                let stopped = self.stopped_event_named("exception", Some(msg.clone()), Some(msg));
                vec![output, stopped]
            }
        }
    }

    fn terminate_now(&mut self, code: i64) -> Vec<Value> {
        self.state = State::Terminated;
        vec![
            self.make_event("terminated", Value::Null),
            self.make_event("exited", json!({ "exitCode": code })),
        ]
    }

    /// If the program is loaded and configuration is done, deliver the initial
    /// stop (or start running if `stopOnEntry` was not set).
    fn maybe_enter(&mut self) -> Vec<Value> {
        if self.program.is_none() || !self.configured || self.entered {
            return Vec::new();
        }
        self.entered = true;
        if self.stop_on_entry {
            self.state = State::Stopped;
            return vec![self.stopped_event(StopReason::Entry, None, None)];
        }
        // `execute_budget` steps before testing breakpoints, so a breakpoint on
        // the entry instruction would be run past on a fresh launch. Test the
        // initial pc here so it fires before the first step.
        let program = self.program.as_ref().expect("program loaded");
        let pc = program.computer.registers.pc;
        if program.breakpoints.contains(&pc) {
            self.state = State::Stopped;
            return vec![self.stopped_event(StopReason::Breakpoint, None, None)];
        }
        self.run_mode = RunMode::Continue;
        self.state = State::Running;
        Vec::new()
    }

    // ----------------------------------------------------------------------
    // Stack frames
    // ----------------------------------------------------------------------

    fn stack_frames(&self) -> Vec<StackFrame> {
        let Some(program) = self.program.as_ref() else {
            return Vec::new();
        };
        let mut frames = Vec::new();
        let pc = program.computer.registers.pc;
        frames.push(self.frame(0, pc));

        // Best-effort synthesis of caller frames: scan the stack for words that
        // look like return addresses (the cell just before them is a `call`).
        let sp = program.computer.registers.sp;
        let mut id = 1;
        for addr in sp..C::STACK_START {
            if frames.len() >= MAX_FRAMES {
                break;
            }
            let Ok(cell) = program.computer.memory.get(addr) else {
                continue;
            };
            let Cell::Word(word) = cell else { continue };
            let Ok(target) = Address::try_from(*word) else {
                continue;
            };
            if target == 0 {
                continue;
            }
            let is_return = program
                .computer
                .memory
                .get(target - 1)
                .ok()
                .and_then(|c| c.extract_instruction().ok())
                .is_some_and(|i| matches!(i, Instruction::Call(_)));
            if is_return && program.index.location(target).is_some() {
                frames.push(self.frame(id, target));
                id += 1;
            }
        }
        frames
    }

    fn frame(&self, id: i64, address: Address) -> StackFrame {
        let program = self.program.as_ref().expect("program loaded");
        let name = enclosing_label(&program.labels, address);
        match program.index.location(address) {
            Some(loc) => {
                let path = program.index.path(loc.file_index).to_owned();
                StackFrame {
                    id,
                    name,
                    source: Some(Source {
                        name: Some(base_name(&path).to_owned()),
                        path: Some(path),
                    }),
                    line: loc.line,
                    column: loc.column,
                }
            }
            None => StackFrame {
                id,
                name,
                source: None,
                line: 0,
                column: 0,
            },
        }
    }

    // ----------------------------------------------------------------------
    // Message envelope builders
    // ----------------------------------------------------------------------

    fn next_seq(&mut self) -> i64 {
        self.seq += 1;
        self.seq
    }

    fn response(&mut self, req: &IncomingRequest, body: Value) -> Value {
        self.build_response(req, true, body, None)
    }

    fn response_err(&mut self, req: &IncomingRequest, message: impl Into<String>) -> Value {
        self.build_response(req, false, Value::Null, Some(message.into()))
    }

    fn build_response(
        &mut self,
        req: &IncomingRequest,
        success: bool,
        body: Value,
        message: Option<String>,
    ) -> Value {
        let seq = self.next_seq();
        let mut m = Map::new();
        m.insert("seq".to_owned(), seq.into());
        m.insert("type".to_owned(), "response".into());
        m.insert("request_seq".to_owned(), req.seq.into());
        m.insert("success".to_owned(), success.into());
        m.insert("command".to_owned(), req.command.clone().into());
        if let Some(message) = message {
            m.insert("message".to_owned(), message.into());
        }
        if !body.is_null() {
            m.insert("body".to_owned(), body);
        }
        Value::Object(m)
    }

    fn make_event(&mut self, name: &str, body: Value) -> Value {
        let seq = self.next_seq();
        let mut m = Map::new();
        m.insert("seq".to_owned(), seq.into());
        m.insert("type".to_owned(), "event".into());
        m.insert("event".to_owned(), name.into());
        if !body.is_null() {
            m.insert("body".to_owned(), body);
        }
        Value::Object(m)
    }

    fn stopped_event(
        &mut self,
        reason: StopReason,
        description: Option<String>,
        text: Option<String>,
    ) -> Value {
        self.stopped_event_named(reason.as_str(), description, text)
    }

    fn stopped_event_named(
        &mut self,
        reason: &str,
        description: Option<String>,
        text: Option<String>,
    ) -> Value {
        let mut body = json!({
            "reason": reason,
            "threadId": THREAD_ID,
            "allThreadsStopped": true,
        });
        if let Some(description) = description {
            body["description"] = description.into();
        }
        if let Some(text) = text {
            body["text"] = text.into();
        }
        self.make_event("stopped", body)
    }
}

// --------------------------------------------------------------------------
// Free helpers
// --------------------------------------------------------------------------

fn registers_variables(computer: &Computer) -> Vec<Variable> {
    let regs = &computer.registers;
    vec![
        Variable {
            name: "a".to_owned(),
            value: format!("{}", regs.a),
            kind: Some("register".to_owned()),
            variables_reference: 0,
        },
        Variable {
            name: "b".to_owned(),
            value: format!("{}", regs.b),
            kind: Some("register".to_owned()),
            variables_reference: 0,
        },
        Variable {
            name: "pc".to_owned(),
            value: regs.pc.to_string(),
            kind: Some("register".to_owned()),
            variables_reference: 0,
        },
        Variable {
            name: "sp".to_owned(),
            value: regs.sp.to_string(),
            kind: Some("register".to_owned()),
            variables_reference: 0,
        },
        Variable {
            name: "sr".to_owned(),
            value: format!("0x{:x}", regs.sr.bits()),
            kind: Some("flags".to_owned()),
            variables_reference: FLAGS_REF,
        },
    ]
}

fn execution_variables(computer: &Computer) -> Vec<Variable> {
    vec![Variable {
        name: "cycles".to_owned(),
        value: computer.cycles.to_string(),
        kind: Some("counter".to_owned()),
        variables_reference: 0,
    }]
}

fn flags_variables(computer: &Computer) -> Vec<Variable> {
    let sr = computer.registers.sr;
    // Derive the child variables straight from `StatusRegister` so the list can
    // never drift from the bitflags definition. `iter_names` yields the flags in
    // declaration order with their uppercase identifiers; lowercase them to keep
    // the historical variable names (`carry`, `interrupt_enable`, ...).
    StatusRegister::all()
        .iter_names()
        .map(|(name, flag)| Variable {
            name: name.to_ascii_lowercase(),
            value: sr.contains(flag).to_string(),
            kind: Some("bool".to_owned()),
            variables_reference: 0,
        })
        .collect()
}

fn evaluate_expression(program: &LoadedProgram, expr: &str) -> Option<(String, String)> {
    if expr.is_empty() {
        return None;
    }
    // Register?
    if let Ok(reg) = Reg::from_str(expr) {
        let value = format!("{}", program.computer.registers.get(&reg));
        return Some((value, "register".to_owned()));
    }
    // Label?
    if let Some(&address) = program.labels.get(expr) {
        let cell = program
            .computer
            .memory
            .get(address)
            .map_or_else(|_| "?".to_owned(), |c| format!("{c}"));
        return Some((format!("{address} ({cell})"), "address".to_owned()));
    }
    // Integer literal?
    if let Some(value) = parse_int(expr) {
        return Some((value.to_string(), "integer".to_owned()));
    }
    None
}

fn enclosing_label(labels: &BTreeMap<String, Address>, address: Address) -> String {
    labels
        .iter()
        .filter(|(_, &a)| a <= address)
        .max_by_key(|(_, &a)| a)
        .map_or_else(|| format!("@{address}"), |(name, _)| name.clone())
}

fn parse_int(input: &str) -> Option<i64> {
    let input = input.trim();
    if let Some(hex) = input
        .strip_prefix("0x")
        .or_else(|| input.strip_prefix("0X"))
    {
        i64::from_str_radix(hex, 16).ok()
    } else if let Some(hex) = input
        .strip_prefix("-0x")
        .or_else(|| input.strip_prefix("-0X"))
    {
        i64::from_str_radix(hex, 16).ok().map(|v| -v)
    } else {
        input.parse::<i64>().ok()
    }
}

fn base_name(path: &str) -> &str {
    path.rsplit(['/', '\\']).next().unwrap_or(path)
}

fn choose_entrypoint(
    provided: Option<&str>,
    labels: &BTreeMap<String, Address>,
) -> Result<String, String> {
    if let Some(entrypoint) = provided {
        return Ok(entrypoint.to_owned());
    }
    for candidate in ["main", "start", "run", "entry"] {
        if labels.contains_key(candidate) {
            return Ok(candidate.to_owned());
        }
    }
    let available: Vec<&str> = labels.keys().map(String::as_str).collect();
    Err(format!(
        "no entrypoint specified and none of main/start/run/entry found; available labels: {}",
        available.join(", ")
    ))
}

/// Build a [`LoadedProgram`] from launch arguments, selecting the filesystem
/// based on whether an in-memory file map was provided.
fn load_program(args: &LaunchArguments) -> Result<LoadedProgram, String> {
    if let Some(files) = &args.files {
        let map: HashMap<Utf8PathBuf, String> = files
            .iter()
            .map(|(k, v)| (Utf8PathBuf::from(k), v.clone()))
            .collect();
        let fs = InMemoryFilesystem::new(map);
        compile_program(&fs, &args.program, args.entrypoint.as_deref())
    } else {
        let fs = NativeFilesystem::from_env().map_err(|e| e.to_string())?;
        compile_program(&fs, &args.program, args.entrypoint.as_deref())
    }
}

fn compile_program<FS: Filesystem>(
    fs: &FS,
    program: &str,
    entrypoint: Option<&str>,
) -> Result<LoadedProgram, String> {
    let mut workspace = Workspace::new(fs, Utf8Path::new(program));
    let preprocess = workspace.preprocess().map_err(|e| {
        preprocessor_error_to_diagnostics(&e)
            .iter()
            .map(|d| render_to_string(d, workspace.file_db()))
            .collect::<String>()
    })?;

    let ref_map: ReferencingSourceMap = preprocess.source_map.into();
    let parse_result = crate::parse(&preprocess.source);

    // First pass: gather labels and surface any diagnostics.
    let check = crate::compile(
        &parse_result.program.inner,
        &parse_result.diagnostics,
        None,
        preprocess.preprocessed_file_id,
    );
    if !check.diagnostics.is_empty() {
        return Err(render_diagnostics(&check.diagnostics, &ref_map, &workspace));
    }

    let entrypoint = choose_entrypoint(entrypoint, &check.debug_info.labels)?;

    // Second pass: build the computer with the chosen entrypoint.
    let built = crate::compile(
        &parse_result.program.inner,
        &parse_result.diagnostics,
        Some(&entrypoint),
        preprocess.preprocessed_file_id,
    );
    let Some(computer) = built.computer else {
        return Err(render_diagnostics(&built.diagnostics, &ref_map, &workspace));
    };

    let file_db = workspace.into_file_db();
    let index = LineIndex::build(&built.debug_info, &ref_map, &file_db);

    Ok(LoadedProgram {
        computer,
        index,
        labels: built.debug_info.labels,
        bp_by_file: HashMap::new(),
        breakpoints: HashSet::new(),
    })
}

fn render_diagnostics(
    diagnostics: &[codespan_reporting::diagnostic::Diagnostic<crate::diagnostic::FileId>],
    source_map: &ReferencingSourceMap,
    workspace: &Workspace,
) -> String {
    diagnostics
        .iter()
        .map(|d| {
            let resolved = resolve_diagnostic_spans(d, source_map);
            render_to_string(&resolved, workspace.file_db())
        })
        .collect()
}

#[cfg(test)]
mod tests;
