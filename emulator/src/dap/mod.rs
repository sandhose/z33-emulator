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
    Breakpoint, Capabilities, EvaluateArguments, IncomingRequest, LaunchArguments,
    ReadMemoryArguments, Scope, ScopesArguments, SetBreakpointsArguments, SetVariableArguments,
    Source, SourceArguments, StackFrame, Variable, VariablesArguments, WriteMemoryArguments,
};
use crate::constants as C;
use crate::constants::Address;
use crate::diagnostic::{
    preprocessor_error_to_diagnostics, render_to_string, resolve_diagnostic_spans,
};
use crate::parser::ExpressionNode;
use crate::preprocessor::{
    Filesystem, InMemoryFilesystem, NativeFilesystem, ReferencingSourceMap, Workspace,
};
use crate::runtime::registers::StatusRegister;
use crate::runtime::{Cell, Computer, Instruction, ProcessorError, Reg};

/// Maximum number of instructions executed per [`DebugSession::run_chunk`].
pub const CHUNK_SIZE: usize = 10_000;

/// The single thread exposed to the client.
const THREAD_ID: i64 = 1;
/// `variablesReference` for the registers scope (all frames share the live
/// registers).
const REGISTERS_REF: i64 = 1;
/// `variablesReference` for the status-register flags.
const FLAGS_REF: i64 = 2;
/// `variablesReference` for the execution scope (cycle counter and other
/// run-state that is not an architectural register).
const EXECUTION_REF: i64 = 3;
/// `variablesReference` for the top frame's stack scope.
const STACK_REF: i64 = 4;
/// `variablesReference` for the globals (labels) scope.
const GLOBALS_REF: i64 = 5;
/// First value used for dynamically allocated, per-stop `variablesReference`
/// handles (region expansions and per-frame stack scopes). Monotonic across the
/// session so a stale handle from a previous stop never collides with a live
/// one.
const DYN_BASE: i64 = 1_000;
/// Default page size when a `variables` request does not specify a `count`.
const DEFAULT_PAGE: u32 = 256;
/// Upper bound on synthesized caller frames.
const MAX_FRAMES: usize = 64;
/// Bytes on the wire for a single Z33 cell (one `i64` word, little-endian).
const CELL_BYTES: i64 = 8;

/// What a dynamically allocated `variablesReference` expands to. Rebuilt on
/// every stop; see [`DynRef`] allocation in [`DebugSession::alloc_dyn`].
#[derive(Debug, Clone)]
enum DynRef {
    /// Indexed children of a multi-cell global (label) region.
    GlobalLabel {
        label: String,
        base: Address,
        len: u32,
    },
    /// The words a synthesized caller frame pushed, `[start, end)`.
    FrameStack { start: Address, end: Address },
}

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
    /// Requested breakpoint source lines per path (last `setBreakpoints` wins).
    /// Kept as the raw client request so they can be re-resolved against a
    /// fresh [`LineIndex`] after a `restart` (addresses may shift).
    requested_lines: HashMap<String, Vec<u32>>,
    /// Breakpoint addresses per source path (last `setBreakpoints` wins).
    bp_by_file: HashMap<String, Vec<Address>>,
    /// Union of all breakpoint addresses.
    breakpoints: HashSet<Address>,
}

impl LoadedProgram {
    fn recompute_breakpoints(&mut self) {
        self.breakpoints = self.bp_by_file.values().flatten().copied().collect();
    }

    /// Re-resolve every previously requested source breakpoint against the
    /// current [`LineIndex`], dropping lines that no longer map to code, and
    /// recompute the address set. Used after a `restart`, where the layout may
    /// have shifted but the client will not re-send `setBreakpoints`.
    fn reresolve_breakpoints(&mut self) {
        self.bp_by_file = self
            .requested_lines
            .iter()
            .map(|(path, lines)| {
                let addrs = lines
                    .iter()
                    .filter_map(|&line| self.index.resolve_breakpoint(path, line).map(|(_, a)| a))
                    .collect();
                (path.clone(), addrs)
            })
            .collect();
        self.recompute_breakpoints();
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
    /// Live dynamic `variablesReference` handles for the current stop.
    dyn_handles: HashMap<i64, DynRef>,
    /// Monotonic allocator for dynamic handles (never reset, so stale handles
    /// stay invalid once cleared).
    next_dyn: i64,
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
            dyn_handles: HashMap::new(),
            next_dyn: 0,
        }
    }

    /// Allocate a fresh dynamic `variablesReference` for the current stop.
    fn alloc_dyn(&mut self, r: DynRef) -> i64 {
        let id = DYN_BASE + self.next_dyn;
        self.next_dyn += 1;
        self.dyn_handles.insert(id, r);
        id
    }

    /// Invalidate every dynamic handle. Called on both stop and resume so a
    /// `variablesReference` handed out during one stop cannot be read across a
    /// resume (standard DAP practice — references live only while stopped).
    fn invalidate_handles(&mut self) {
        self.dyn_handles.clear();
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
            "readMemory" => self.on_read_memory(&req),
            "writeMemory" => self.on_write_memory(&req),
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
            let requested: Vec<u32> = args.breakpoints.iter().map(|bp| bp.line).collect();
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
            program.requested_lines.insert(path.clone(), requested);
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
        let args: ScopesArguments =
            serde_json::from_value(req.arguments.clone()).unwrap_or_default();
        if self.program.is_none() {
            return vec![self.response_err(req, "no program launched")];
        }

        let mut scopes = vec![
            Scope {
                name: "Registers".to_owned(),
                variables_reference: REGISTERS_REF,
                expensive: false,
                ..Default::default()
            },
            Scope {
                name: "Execution".to_owned(),
                variables_reference: EXECUTION_REF,
                expensive: false,
                ..Default::default()
            },
        ];

        if args.frame_id == 0 {
            // Top frame: live stack + globals.
            let program = self.program.as_ref().expect("program loaded");
            // `sp` is client-controllable (e.g. via `setVariable`); guard the
            // subtraction so an `sp` above `STACK_START` yields an empty stack
            // instead of underflowing.
            let depth = i64::from(C::STACK_START.saturating_sub(program.computer.registers.sp));
            scopes.push(Scope {
                name: "Stack".to_owned(),
                variables_reference: STACK_REF,
                expensive: false,
                indexed_variables: Some(depth),
                ..Default::default()
            });
        } else if let Some((start, end)) = self.frame_stack_range(args.frame_id) {
            // Synthesized caller frame: best-effort view of the words it pushed.
            let handle = self.alloc_dyn(DynRef::FrameStack { start, end });
            scopes.push(Scope {
                name: "Frame stack".to_owned(),
                variables_reference: handle,
                expensive: false,
                indexed_variables: Some(i64::from(end - start)),
                presentation_hint: Some("arguments".to_owned()),
                ..Default::default()
            });
        }

        scopes.push(Scope {
            name: "Globals".to_owned(),
            variables_reference: GLOBALS_REF,
            expensive: false,
            named_variables: Some(self.globals_count()),
            ..Default::default()
        });

        vec![self.response(req, json!({ "scopes": scopes }))]
    }

    /// Number of labels exposed by the globals scope.
    fn globals_count(&self) -> i64 {
        self.program
            .as_ref()
            .map_or(0, |p| i64::try_from(p.labels.len()).unwrap_or(i64::MAX))
    }

    /// Best-effort `[start, end)` stack range a synthesized caller frame
    /// pushed, or `None` when the return-address scan found nothing
    /// reliable for it.
    ///
    /// Frame `j` (1-based) was found at return slot `s = slots[j-1]`; the words
    /// it pushed for its callee live just above that slot, up to the next outer
    /// return slot (or the stack base). Returns `None` if the region is empty.
    fn frame_stack_range(&self, frame_id: i64) -> Option<(Address, Address)> {
        let program = self.program.as_ref()?;
        let slots = scan_return_slots(program);
        let idx = usize::try_from(frame_id - 1).ok()?;
        let slot = *slots.get(idx)?;
        let start = slot + 1;
        let end = slots.get(idx + 1).copied().unwrap_or(C::STACK_START);
        if start >= end {
            None
        } else {
            Some((start, end))
        }
    }

    fn on_variables(&mut self, req: &IncomingRequest) -> Vec<Value> {
        let args: VariablesArguments = match serde_json::from_value(req.arguments.clone()) {
            Ok(a) => a,
            Err(e) => return vec![self.response_err(req, format!("invalid arguments: {e}"))],
        };
        if self.program.is_none() {
            return vec![self.response_err(req, "no program launched")];
        }
        let hex = args.format.unwrap_or_default().hex;
        let start = args.start.unwrap_or(0).max(0);
        let start = u32::try_from(start).unwrap_or(u32::MAX);
        let count = args.count.and_then(|c| u32::try_from(c).ok());

        // Multi-cell / frame-stack expansions live in the dynamic table.
        if args.variables_reference >= DYN_BASE {
            let Some(dref) = self.dyn_handles.get(&args.variables_reference).cloned() else {
                // Stale handle from a previous stop: DAP references are only
                // valid while stopped, so we return an empty list.
                return vec![self.response(req, json!({ "variables": [] }))];
            };
            let program = self.program.as_ref().expect("program loaded");
            let variables = match dref {
                DynRef::GlobalLabel { label, base, len } => {
                    global_label_children(program, &label, base, len, start, count, hex)
                }
                DynRef::FrameStack { start: s, end } => {
                    frame_stack_variables(program, s, end, start, count, hex)
                }
            };
            return vec![self.response(req, json!({ "variables": variables }))];
        }

        let variables = match args.variables_reference {
            REGISTERS_REF => {
                let program = self.program.as_ref().expect("program loaded");
                registers_variables(&program.computer, hex)
            }
            FLAGS_REF => {
                let program = self.program.as_ref().expect("program loaded");
                flags_variables(&program.computer)
            }
            EXECUTION_REF => {
                let program = self.program.as_ref().expect("program loaded");
                execution_variables(&program.computer)
            }
            STACK_REF => {
                let program = self.program.as_ref().expect("program loaded");
                stack_variables(program, start, count, hex)
            }
            GLOBALS_REF => self.globals_variables(start, count, hex),
            _ => Vec::new(),
        };
        vec![self.response(req, json!({ "variables": variables }))]
    }

    /// Build the globals (labels) scope, allocating dynamic handles for the
    /// multi-cell regions so the client can expand them.
    fn globals_variables(&mut self, start: u32, count: Option<u32>, hex: bool) -> Vec<Variable> {
        let regions = {
            let program = self.program.as_ref().expect("program loaded");
            label_regions(&program.labels)
        };
        let count = count.map_or(usize::MAX, |c| c as usize);
        let mut out = Vec::new();
        for (label, base, len) in regions.into_iter().skip(start as usize).take(count) {
            let program = self.program.as_ref().expect("program loaded");
            let cell = program.computer.memory.get(base).ok();
            if len <= 1 {
                out.push(Variable {
                    name: label,
                    value: cell.map_or_else(|| "?".to_owned(), |c| format_cell(c, hex)),
                    kind: Some(cell.map_or("word", cell_type).to_owned()),
                    memory_reference: Some(base.to_string()),
                    ..Default::default()
                });
            } else {
                let first = cell.map_or_else(|| "?".to_owned(), |c| format_cell(c, hex));
                let handle = self.alloc_dyn(DynRef::GlobalLabel {
                    label: label.clone(),
                    base,
                    len,
                });
                out.push(Variable {
                    name: label,
                    value: format!("[{len} cells] first={first}"),
                    kind: Some("region".to_owned()),
                    variables_reference: handle,
                    memory_reference: Some(base.to_string()),
                    indexed_variables: Some(i64::from(len)),
                    ..Default::default()
                });
            }
        }
        out
    }

    fn on_set_variable(&mut self, req: &IncomingRequest) -> Vec<Value> {
        let args: SetVariableArguments = match serde_json::from_value(req.arguments.clone()) {
            Ok(a) => a,
            Err(e) => return vec![self.response_err(req, format!("invalid arguments: {e}"))],
        };

        let hex = args.format.unwrap_or_default().hex;
        // Resolve the target address for cell scopes before borrowing mutably.
        let cell_addr = self.resolve_cell_address(args.variables_reference, &args.name);

        let outcome = {
            let Some(program) = self.program.as_mut() else {
                return vec![self.response_err(req, "no program launched")];
            };
            if args.variables_reference == REGISTERS_REF {
                set_register(program, &args.name, &args.value, hex)
            } else if let Some(addr) = cell_addr {
                set_cell(program, addr, &args.value, hex)
            } else {
                Err(format!("cannot set variable '{}'", args.name))
            }
        };

        match outcome {
            Ok(value) => {
                vec![self.response(req, json!({ "value": value, "variablesReference": 0 }))]
            }
            Err(msg) => vec![self.response_err(req, msg)],
        }
    }

    /// Resolve the memory address a settable cell variable names, for the
    /// Stack, Globals and Frame-stack scopes. Returns `None` for the
    /// register scope or unknown references.
    fn resolve_cell_address(&self, reference: i64, name: &str) -> Option<Address> {
        let program = self.program.as_ref()?;
        match reference {
            STACK_REF => {
                let n = name.strip_prefix("sp+")?.trim().parse::<u32>().ok()?;
                Some(program.computer.registers.sp + n)
            }
            GLOBALS_REF => {
                // Only single-cell labels are settable at the scope level.
                let &base = program.labels.get(name)?;
                let regions = label_regions(&program.labels);
                let len = regions
                    .iter()
                    .find(|(l, _, _)| l == name)
                    .map_or(1, |&(_, _, len)| len);
                (len <= 1).then_some(base)
            }
            r if r >= DYN_BASE => match self.dyn_handles.get(&r)? {
                // Both children encode the absolute address in `name (addr)`.
                DynRef::GlobalLabel { .. } | DynRef::FrameStack { .. } => parse_addr_suffix(name),
            },
            _ => None,
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
        // Resuming: any handle handed out during the last stop is now stale.
        self.invalidate_handles();
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

        let hex = args.format.unwrap_or_default().hex;
        let result = {
            let Some(program) = self.program.as_ref() else {
                return vec![self.response_err(req, "no program launched")];
            };
            evaluate_expression(program, args.expression.trim(), hex)
        };

        match result {
            Ok(eval) => {
                let mut body = json!({
                    "result": eval.result,
                    "type": eval.kind,
                    "variablesReference": 0,
                });
                if let Some(addr) = eval.memory_reference {
                    body["memoryReference"] = addr.to_string().into();
                }
                vec![self.response(req, body)]
            }
            Err(msg) => vec![self.response_err(req, msg)],
        }
    }

    fn on_read_memory(&mut self, req: &IncomingRequest) -> Vec<Value> {
        let args: ReadMemoryArguments = match serde_json::from_value(req.arguments.clone()) {
            Ok(a) => a,
            Err(e) => return vec![self.response_err(req, format!("invalid arguments: {e}"))],
        };
        let Some(program) = self.program.as_ref() else {
            return vec![self.response_err(req, "no program launched")];
        };
        let Ok(base) = args.memory_reference.trim().parse::<i64>() else {
            return vec![self.response_err(req, "memoryReference must be a decimal cell address")];
        };
        if args.count < 0 {
            return vec![self.response_err(req, "count must be non-negative")];
        }
        let (address, data, unreadable) =
            match read_memory(&program.computer, base, args.offset, args.count) {
                Ok(t) => t,
                Err(msg) => return vec![self.response_err(req, msg)],
            };
        let mut body = json!({ "address": address.to_string(), "data": base64_encode(&data) });
        if unreadable > 0 {
            body["unreadableBytes"] = unreadable.into();
        }
        vec![self.response(req, body)]
    }

    fn on_write_memory(&mut self, req: &IncomingRequest) -> Vec<Value> {
        let args: WriteMemoryArguments = match serde_json::from_value(req.arguments.clone()) {
            Ok(a) => a,
            Err(e) => return vec![self.response_err(req, format!("invalid arguments: {e}"))],
        };
        let Some(program) = self.program.as_mut() else {
            return vec![self.response_err(req, "no program launched")];
        };
        let Ok(base) = args.memory_reference.trim().parse::<i64>() else {
            return vec![self.response_err(req, "memoryReference must be a decimal cell address")];
        };
        let Some(bytes) = base64_decode(&args.data) else {
            return vec![self.response_err(req, "data is not valid base64")];
        };
        match write_memory(
            &mut program.computer,
            base,
            args.offset,
            &bytes,
            args.allow_partial,
        ) {
            Ok(written) => vec![self.response(req, json!({ "bytesWritten": written }))],
            Err(msg) => vec![self.response_err(req, msg)],
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
            Ok(mut program) => {
                // Carry breakpoints across the restart: the client will not
                // re-send `setBreakpoints` (no `initialized` is re-emitted), so
                // re-resolve the previously requested source lines against the
                // freshly built line index (addresses may have shifted).
                if let Some(old) = self.program.take() {
                    program.requested_lines = old.requested_lines;
                    program.reresolve_breakpoints();
                }
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
                self.invalidate_handles();
                vec![self.stopped_event(reason, None, None)]
            }
            Outcome::Terminated(code) => self.terminate_now(code),
            Outcome::Exception(msg) => {
                self.state = State::Stopped;
                self.exception_pending = true;
                self.invalidate_handles();
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

        // Best-effort synthesis of caller frames from the return-address scan.
        for (id, (_slot, target)) in scan_frames(program).into_iter().enumerate() {
            frames.push(self.frame(i64::try_from(id + 1).unwrap_or(i64::MAX), target));
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

fn register_var(name: &str, value: String) -> Variable {
    Variable {
        name: name.to_owned(),
        value,
        kind: Some("register".to_owned()),
        ..Default::default()
    }
}

fn registers_variables(computer: &Computer, hex: bool) -> Vec<Variable> {
    let regs = &computer.registers;
    vec![
        register_var("a", format_cell(&regs.a, hex)),
        register_var("b", format_cell(&regs.b, hex)),
        register_var("pc", format_address(regs.pc, hex)),
        register_var("sp", format_address(regs.sp, hex)),
        Variable {
            name: "sr".to_owned(),
            value: format!("0x{:x}", regs.sr.bits()),
            kind: Some("flags".to_owned()),
            variables_reference: FLAGS_REF,
            ..Default::default()
        },
    ]
}

fn execution_variables(computer: &Computer) -> Vec<Variable> {
    vec![Variable {
        name: "cycles".to_owned(),
        value: computer.cycles.to_string(),
        kind: Some("counter".to_owned()),
        ..Default::default()
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
            ..Default::default()
        })
        .collect()
}

/// The result of an `evaluate` request.
struct Evaluated {
    result: String,
    kind: String,
    memory_reference: Option<Address>,
}

/// Evaluate an `evaluate` expression. Supported forms:
///
/// * `%a`/`%pc`/... — a register value (`register`).
/// * an integer literal (`0x10`, `-3`) — the literal (`integer`).
/// * `label`, `label+2`, `4*label` — the resolved **address** value
///   (`address`), with `memoryReference` set.
/// * `[<expr>]` — dereference: read the cell at the address `<expr>` computes
///   to (`cell`), with `memoryReference` set. `<expr>` may itself be a
///   register-indexed form (`[%sp]`, `[%sp+2]`, `[%a-1]`) or a label/arithmetic
///   expression.
fn evaluate_expression(
    program: &LoadedProgram,
    expr: &str,
    hex: bool,
) -> Result<Evaluated, String> {
    if expr.is_empty() {
        return Err("empty expression".to_owned());
    }
    let computer = &program.computer;

    // Dereference: `[<expr>]`.
    if let Some(inner) = expr.strip_prefix('[').and_then(|s| s.strip_suffix(']')) {
        let addr = eval_address(program, inner.trim())?;
        let cell = computer.memory.get(addr).map_err(|e| e.to_string())?;
        return Ok(Evaluated {
            result: format!(
                "{} ({})",
                format_cell(cell, hex),
                format_address(addr, false)
            ),
            kind: cell_type(cell).to_owned(),
            memory_reference: Some(addr),
        });
    }

    // Register?
    if let Ok(reg) = Reg::from_str(expr) {
        return Ok(Evaluated {
            result: format_cell(&computer.registers.get(&reg), hex),
            kind: "register".to_owned(),
            memory_reference: None,
        });
    }

    // Integer literal?
    if let Some(value) = parse_int(expr) {
        return Ok(Evaluated {
            result: format_word(value, hex),
            kind: "integer".to_owned(),
            memory_reference: None,
        });
    }

    // Label / arithmetic → the address value.
    let addr = eval_address(program, expr)?;
    let cell = computer
        .memory
        .get(addr)
        .map_or_else(|_| "?".to_owned(), |c| format_cell(c, hex));
    Ok(Evaluated {
        result: format!("{} ({cell})", format_address(addr, hex)),
        kind: "address".to_owned(),
        memory_reference: Some(addr),
    })
}

/// Evaluate an address expression (a register-indexed form or a label /
/// arithmetic expression) against the live computer state.
fn eval_address(program: &LoadedProgram, input: &str) -> Result<Address, String> {
    let arg = parse_addr_arg(input)?;
    let value: i128 = match arg {
        AddrArg::Expr(node) => node
            .evaluate::<_, i128>(&program.labels)
            .map_err(|e| e.to_string())?,
        AddrArg::Reg(reg) => i128::from(
            program
                .computer
                .registers
                .get_word(reg)
                .map_err(|e| e.to_string())?,
        ),
        AddrArg::Indexed(reg, is_plus, node) => {
            let base = i128::from(
                program
                    .computer
                    .registers
                    .get_word(reg)
                    .map_err(|e| e.to_string())?,
            );
            let offset: i128 = node
                .evaluate::<_, i128>(&program.labels)
                .map_err(|e| e.to_string())?;
            if is_plus {
                base + offset
            } else {
                base - offset
            }
        }
    };
    Address::try_from(value).map_err(|_| format!("address out of range: {value}"))
}

fn enclosing_label(labels: &BTreeMap<String, Address>, address: Address) -> String {
    labels
        .iter()
        .filter(|(_, &a)| a <= address)
        .max_by_key(|(_, &a)| a)
        .map_or_else(|| format!("@{address}"), |(name, _)| name.clone())
}

// --------------------------------------------------------------------------
// Address-expression parsing (adapted from cli::interactive::parse)
// --------------------------------------------------------------------------

/// A parsed address expression for `evaluate`'s dereference forms.
enum AddrArg {
    /// A plain expression (label / literal / arithmetic).
    Expr(ExpressionNode),
    /// A bare register (`%sp`).
    Reg(Reg),
    /// A register-indexed form (`%sp+2`, `%a-1`); the bool is `+`.
    Indexed(Reg, bool, ExpressionNode),
}

fn parse_addr_arg(input: &str) -> Result<AddrArg, String> {
    use chumsky::prelude::*;

    use crate::parser::shared::{expression, register};

    let indexed = register()
        .then(choice((just('+').to(true), just('-').to(false))).then(expression()))
        .map(|(reg, (is_plus, expr))| AddrArg::Indexed(reg, is_plus, expr));

    let parser = choice((
        indexed,
        register().map(AddrArg::Reg),
        expression().map(AddrArg::Expr),
    ))
    .then_ignore(end());

    parser.parse(input).into_result().map_err(|errs| {
        errs.into_iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("; ")
    })
}

// --------------------------------------------------------------------------
// Stack scan
// --------------------------------------------------------------------------

/// Scan the stack for `(slot, target)` pairs that look like return addresses:
/// a `Word` cell whose value points just past a `call` instruction that maps to
/// source. Returned in ascending slot order (innermost call first), capped at
/// [`MAX_FRAMES`].
fn scan_frames(program: &LoadedProgram) -> Vec<(Address, Address)> {
    let sp = program.computer.registers.sp;
    let mut out = Vec::new();
    for addr in sp..C::STACK_START {
        if out.len() >= MAX_FRAMES {
            break;
        }
        let Ok(Cell::Word(word)) = program.computer.memory.get(addr) else {
            continue;
        };
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
            out.push((addr, target));
        }
    }
    out
}

/// The return-address slots on the stack, ascending.
fn scan_return_slots(program: &LoadedProgram) -> Vec<Address> {
    scan_frames(program)
        .into_iter()
        .map(|(slot, _)| slot)
        .collect()
}

/// Map of return-address slot → target, for annotating stack cells.
fn return_slot_targets(program: &LoadedProgram) -> BTreeMap<Address, Address> {
    scan_frames(program).into_iter().collect()
}

// --------------------------------------------------------------------------
// Variable builders for the cell-backed scopes
// --------------------------------------------------------------------------

fn cell_variable(name: String, addr: Address, cell: &Cell, hex: bool) -> Variable {
    Variable {
        name,
        value: format_cell(cell, hex),
        kind: Some(cell_type(cell).to_owned()),
        memory_reference: Some(addr.to_string()),
        ..Default::default()
    }
}

/// Build the top frame's Stack scope, top-of-stack (`sp+0`) first, paged by
/// `start`/`count`.
fn stack_variables(
    program: &LoadedProgram,
    start: u32,
    count: Option<u32>,
    hex: bool,
) -> Vec<Variable> {
    let sp = program.computer.registers.sp;
    let total = C::STACK_START.saturating_sub(sp);
    let returns = return_slot_targets(program);
    let limit = count.unwrap_or(DEFAULT_PAGE) as usize;
    let mut out = Vec::new();
    for n in start..total {
        if out.len() >= limit {
            break;
        }
        let addr = sp + n;
        let Ok(cell) = program.computer.memory.get(addr) else {
            continue;
        };
        let mut var = cell_variable(format!("sp+{n}"), addr, cell, hex);
        if let Some(&target) = returns.get(&addr) {
            let label = enclosing_label(&program.labels, target);
            var.value = format!("{} (return to {label})", var.value);
            var.presentation_hint = Some(json!({ "kind": "data", "attributes": ["readOnly"] }));
        }
        out.push(var);
    }
    out
}

/// Indexed children of a multi-cell global region, paged by `start`/`count`.
fn global_label_children(
    program: &LoadedProgram,
    label: &str,
    base: Address,
    len: u32,
    start: u32,
    count: Option<u32>,
    hex: bool,
) -> Vec<Variable> {
    let limit = count.unwrap_or(DEFAULT_PAGE) as usize;
    let mut out = Vec::new();
    for k in start..len {
        if out.len() >= limit {
            break;
        }
        let addr = base + k;
        let Ok(cell) = program.computer.memory.get(addr) else {
            continue;
        };
        out.push(cell_variable(
            format!("{label}+{k} ({addr})"),
            addr,
            cell,
            hex,
        ));
    }
    out
}

/// The words a synthesized caller frame pushed, `arg1` (just above the return
/// slot) first.
fn frame_stack_variables(
    program: &LoadedProgram,
    start_addr: Address,
    end_addr: Address,
    start: u32,
    count: Option<u32>,
    hex: bool,
) -> Vec<Variable> {
    let total = end_addr.saturating_sub(start_addr);
    let limit = count.unwrap_or(DEFAULT_PAGE) as usize;
    let mut out = Vec::new();
    for k in start..total {
        if out.len() >= limit {
            break;
        }
        let addr = start_addr + k;
        let Ok(cell) = program.computer.memory.get(addr) else {
            continue;
        };
        out.push(cell_variable(
            format!("arg{} ({addr})", k + 1),
            addr,
            cell,
            hex,
        ));
    }
    out
}

/// The label regions `(name, base, len)`, sorted by address, each bounded by
/// the next label's address and the memory size (at least one cell). The full
/// `len` is reported as `indexedVariables`; per-request paging (see
/// [`DEFAULT_PAGE`]) keeps large regions like a 5000-word `.space` responsive.
fn label_regions(labels: &BTreeMap<String, Address>) -> Vec<(String, Address, u32)> {
    let mut by_addr: Vec<(Address, String)> = labels.iter().map(|(n, &a)| (a, n.clone())).collect();
    by_addr.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)));

    let mut out = Vec::with_capacity(by_addr.len());
    for i in 0..by_addr.len() {
        let (addr, name) = &by_addr[i];
        let next = by_addr
            .get(i + 1)
            .map_or(C::MEMORY_SIZE, |(a, _)| *a)
            .max(*addr);
        let len = (next - addr).max(1);
        out.push((name.clone(), *addr, len));
    }
    out
}

// --------------------------------------------------------------------------
// setVariable helpers
// --------------------------------------------------------------------------

fn set_register(
    program: &mut LoadedProgram,
    name: &str,
    value: &str,
    hex: bool,
) -> Result<String, String> {
    let Some(word) = parse_int(value) else {
        return Err(format!("invalid integer value '{value}'"));
    };
    let reg = match name {
        "a" => Reg::A,
        "b" => Reg::B,
        "pc" => Reg::PC,
        "sp" => Reg::SP,
        _ => return Err(format!("cannot set variable '{name}'")),
    };
    program
        .computer
        .registers
        .set(reg, Cell::Word(word))
        .map_err(|e| e.to_string())?;
    Ok(format_cell(&program.computer.registers.get(&reg), hex))
}

fn set_cell(
    program: &mut LoadedProgram,
    addr: Address,
    value: &str,
    hex: bool,
) -> Result<String, String> {
    let Some(word) = parse_int(value) else {
        return Err(format!("invalid integer value '{value}'"));
    };
    let cell = program
        .computer
        .memory
        .get_mut(addr)
        .map_err(|e| e.to_string())?;
    *cell = Cell::Word(word);
    Ok(format_word(word, hex))
}

/// Extract the address from a `... (<decimal>)` variable name suffix.
fn parse_addr_suffix(name: &str) -> Option<Address> {
    let inner = name.rsplit_once('(')?.1;
    let inner = inner.strip_suffix(')')?;
    inner.trim().parse::<Address>().ok()
}

// --------------------------------------------------------------------------
// read/write memory (one cell = one i64 word = 8 bytes little-endian)
// --------------------------------------------------------------------------

/// A single cell as a word for the byte protocol: `Empty` and `Instruction`
/// cells read as zero (documented — instructions have no meaningful word
/// value).
fn cell_word(cell: &Cell) -> i64 {
    match cell {
        Cell::Word(w) => *w,
        Cell::Empty | Cell::Instruction(_) => 0,
    }
}

/// Read `count` bytes starting `offset` bytes past `base` (a cell address).
/// Returns `(first_cell_address, data, unreadable_trailing_bytes)`, or an error
/// if the client-supplied `base`/`offset` overflow when converted to a byte
/// address.
fn read_memory(
    computer: &Computer,
    base: i64,
    offset: i64,
    count: i64,
) -> Result<(i64, Vec<u8>, i64), String> {
    let start_byte = base
        .checked_mul(CELL_BYTES)
        .and_then(|b| b.checked_add(offset))
        .ok_or_else(|| "memory address out of range".to_owned())?;
    let mut data = Vec::new();
    let mut unreadable = 0;
    for i in 0..count {
        let Some(p) = start_byte.checked_add(i) else {
            unreadable = count - i;
            break;
        };
        let cell_index = p.div_euclid(CELL_BYTES);
        let byte_in = usize::try_from(p.rem_euclid(CELL_BYTES)).unwrap_or(0);
        if p < 0 || cell_index >= i64::from(C::MEMORY_SIZE) {
            unreadable = count - i;
            break;
        }
        let word = Address::try_from(cell_index)
            .ok()
            .and_then(|a| computer.memory.get(a).ok())
            .map_or(0, cell_word);
        data.push(word.to_le_bytes()[byte_in]);
    }
    Ok((start_byte.div_euclid(CELL_BYTES), data, unreadable))
}

/// Write `bytes` starting `offset` bytes past `base`. Writes whole 8-byte words
/// as `Cell::Word`; sub-word (unaligned or partial) writes are rejected unless
/// `allow_partial`, in which case affected words are read-modified-written.
fn write_memory(
    computer: &mut Computer,
    base: i64,
    offset: i64,
    bytes: &[u8],
    allow_partial: bool,
) -> Result<usize, String> {
    let start_byte = base
        .checked_mul(CELL_BYTES)
        .and_then(|b| b.checked_add(offset))
        .ok_or_else(|| "memory address out of range".to_owned())?;
    let len = i64::try_from(bytes.len()).map_err(|_| "data too large".to_owned())?;
    let aligned = start_byte.rem_euclid(CELL_BYTES) == 0 && len % CELL_BYTES == 0;
    if !aligned && !allow_partial {
        return Err(
            "writeMemory must cover whole 8-byte words (set allowPartial to override)".to_owned(),
        );
    }

    // Pre-validate the entire span *before* mutating anything, so a write that
    // straddles the end of memory (or below address 0) fails atomically instead
    // of leaving a partial write behind.
    if start_byte < 0 {
        return Err("write below address 0".to_owned());
    }
    if len > 0 {
        let last_byte = start_byte
            .checked_add(len - 1)
            .ok_or_else(|| "memory address out of range".to_owned())?;
        let last_cell = last_byte.div_euclid(CELL_BYTES);
        if last_cell >= i64::from(C::MEMORY_SIZE) || Address::try_from(last_cell).is_err() {
            return Err(format!("write out of range at byte {last_byte}"));
        }
    }

    for (i, &byte) in bytes.iter().enumerate() {
        // The span was validated above, so every address here is in range.
        let p = start_byte + i64::try_from(i).map_err(|_| "data too large".to_owned())?;
        let cell_index = p.div_euclid(CELL_BYTES);
        let byte_in = usize::try_from(p.rem_euclid(CELL_BYTES)).unwrap_or(0);
        let addr =
            Address::try_from(cell_index).map_err(|_| format!("write out of range at byte {p}"))?;
        let cell = computer.memory.get_mut(addr).map_err(|e| e.to_string())?;
        let mut word = cell_word(cell).to_le_bytes();
        word[byte_in] = byte;
        *cell = Cell::Word(i64::from_le_bytes(word));
    }
    Ok(bytes.len())
}

// --------------------------------------------------------------------------
// Formatting
// --------------------------------------------------------------------------

fn format_word(word: i64, hex: bool) -> String {
    if hex {
        if word < 0 {
            format!("-0x{:x}", word.unsigned_abs())
        } else {
            format!("0x{word:x}")
        }
    } else {
        word.to_string()
    }
}

fn format_address(addr: Address, hex: bool) -> String {
    if hex {
        format!("0x{addr:x}")
    } else {
        addr.to_string()
    }
}

fn format_cell(cell: &Cell, hex: bool) -> String {
    match cell {
        Cell::Word(w) => format_word(*w, hex),
        Cell::Empty => format_word(0, hex),
        Cell::Instruction(i) => format!("{i}"),
    }
}

fn cell_type(cell: &Cell) -> &'static str {
    match cell {
        Cell::Instruction(_) => "instruction",
        Cell::Word(_) => "word",
        Cell::Empty => "empty",
    }
}

// --------------------------------------------------------------------------
// Base64 (RFC 4648, standard alphabet with padding) — no external dependency
// --------------------------------------------------------------------------

const B64: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

fn base64_encode(data: &[u8]) -> String {
    let mut out = String::with_capacity(data.len().div_ceil(3) * 4);
    for chunk in data.chunks(3) {
        let b = [
            chunk[0],
            *chunk.get(1).unwrap_or(&0),
            *chunk.get(2).unwrap_or(&0),
        ];
        let n = (u32::from(b[0]) << 16) | (u32::from(b[1]) << 8) | u32::from(b[2]);
        out.push(B64[(n >> 18 & 0x3f) as usize] as char);
        out.push(B64[(n >> 12 & 0x3f) as usize] as char);
        out.push(if chunk.len() > 1 {
            B64[(n >> 6 & 0x3f) as usize] as char
        } else {
            '='
        });
        out.push(if chunk.len() > 2 {
            B64[(n & 0x3f) as usize] as char
        } else {
            '='
        });
    }
    out
}

fn base64_decode(input: &str) -> Option<Vec<u8>> {
    fn val(c: u8) -> Option<u32> {
        match c {
            b'A'..=b'Z' => Some(u32::from(c - b'A')),
            b'a'..=b'z' => Some(u32::from(c - b'a' + 26)),
            b'0'..=b'9' => Some(u32::from(c - b'0' + 52)),
            b'+' => Some(62),
            b'/' => Some(63),
            _ => None,
        }
    }
    let cleaned: Vec<u8> = input.bytes().filter(|b| !b.is_ascii_whitespace()).collect();
    let mut out = Vec::new();
    for chunk in cleaned.chunks(4) {
        if chunk.len() < 2 {
            return None;
        }
        let pad = chunk.iter().rev().take_while(|&&c| c == b'=').count();
        let mut n = 0u32;
        for &c in chunk {
            n = (n << 6) | if c == b'=' { 0 } else { val(c)? };
        }
        // The four sextets pack into the low 24 bits; big-endian bytes 1..=3
        // are the decoded output.
        let [_, hi, mid, lo] = n.to_be_bytes();
        out.push(hi);
        if pad < 2 {
            out.push(mid);
        }
        if pad < 1 {
            out.push(lo);
        }
    }
    Some(out)
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
        requested_lines: HashMap::new(),
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
