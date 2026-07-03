//! Protocol-level tests driving a [`DebugSession`] against an in-memory copy of
//! `samples/fact.s`.

use pretty_assertions::assert_eq;
use serde_json::{json, Value};

use super::DebugSession;

const FACT_SOURCE: &str = indoc::indoc! {r"
    main:
        push 5
        call factorielle
        reset

    factorielle:
        ld   [%sp+1],%a
        cmp  1,%a
        jge  casparticulier

        sub  1,%a
        push %a
        call factorielle
        add  1,%sp
        push %b
        ld   [%sp+2],%b
        mul  %b,%a
        pop  %b
        rtn

    casparticulier:
        ld  1, %a
        rtn
"};

/// A program that raises an unhandled exception on its first instruction: it
/// reads out-of-bounds memory (`%sp` starts at `STACK_START`) with no exception
/// handler installed at address 200.
const FAULT_SOURCE: &str = indoc::indoc! {r"
    main:
        ld   [%sp+1],%a
        reset
"};

/// A tiny harness wrapping a session and a monotonic request-sequence counter.
struct Harness {
    session: DebugSession,
    seq: i64,
}

impl Harness {
    fn new() -> Self {
        Self {
            session: DebugSession::new(),
            seq: 0,
        }
    }

    fn send(&mut self, command: &str, arguments: Value) -> Vec<Value> {
        self.seq += 1;
        let msg = json!({
            "seq": self.seq,
            "type": "request",
            "command": command,
            "arguments": arguments,
        });
        self.session.handle_message(&msg)
    }

    /// Drive execution until the program stops or terminates, returning the
    /// collected events.
    fn drive(&mut self) -> Vec<Value> {
        let mut events = Vec::new();
        let mut guard = 0;
        while self.session.is_running() {
            let out = self.session.run_chunk();
            events.extend(out);
            guard += 1;
            assert!(guard < 1000, "run loop did not converge");
        }
        events
    }

    /// Launch the sample program, optionally stopping on entry, and run through
    /// the standard initialize/launch/configurationDone handshake.
    fn launch(&mut self, stop_on_entry: bool) -> Vec<Value> {
        self.send("initialize", json!({}));
        self.send(
            "launch",
            json!({
                "program": "/fact.s",
                "entrypoint": "main",
                "stopOnEntry": stop_on_entry,
                "files": { "/fact.s": FACT_SOURCE },
            }),
        );
        let mut out = self.send("configurationDone", json!({}));
        out.extend(self.drive());
        out
    }
}

/// Find the first event with the given name in a list of messages.
fn find_event<'a>(messages: &'a [Value], name: &str) -> Option<&'a Value> {
    messages
        .iter()
        .find(|m| m["type"] == "event" && m["event"] == name)
}

/// Find the response for a given command.
fn find_response<'a>(messages: &'a [Value], command: &str) -> Option<&'a Value> {
    messages
        .iter()
        .find(|m| m["type"] == "response" && m["command"] == command)
}

#[test]
fn initialize_advertises_capabilities() {
    let mut h = Harness::new();
    let out = h.send("initialize", json!({}));
    let resp = find_response(&out, "initialize").expect("initialize response");
    assert_eq!(resp["success"], json!(true));
    let caps = &resp["body"];
    assert_eq!(caps["supportsConfigurationDoneRequest"], json!(true));
    assert_eq!(caps["supportsSetVariable"], json!(true));
    assert_eq!(caps["supportsEvaluateForHovers"], json!(true));
    // The `initialized` event must follow.
    assert!(find_event(&out, "initialized").is_some());
}

#[test]
fn stop_on_entry_halts_at_entrypoint() {
    let mut h = Harness::new();
    let out = h.launch(true);
    let stopped = find_event(&out, "stopped").expect("stopped event");
    assert_eq!(stopped["body"]["reason"], json!("entry"));

    // The top frame should be at `main`, line 2 (the `push 5`).
    let st = h.send("stackTrace", json!({ "threadId": 1 }));
    let resp = find_response(&st, "stackTrace").expect("stackTrace response");
    let frame = &resp["body"]["stackFrames"][0];
    assert_eq!(frame["name"], json!("main"));
    assert_eq!(frame["line"], json!(2));
    assert_eq!(frame["source"]["path"], json!("/fact.s"));
}

#[test]
fn breakpoint_inside_factorielle_is_hit() {
    let mut h = Harness::new();
    h.launch(true);

    // Line 8 is `cmp 1,%a` inside `factorielle`.
    let bp = h.send(
        "setBreakpoints",
        json!({
            "source": { "path": "/fact.s" },
            "breakpoints": [{ "line": 8 }],
        }),
    );
    let resp = find_response(&bp, "setBreakpoints").expect("setBreakpoints response");
    let breakpoint = &resp["body"]["breakpoints"][0];
    assert_eq!(breakpoint["verified"], json!(true));
    assert_eq!(breakpoint["line"], json!(8));

    // Continue: we should stop at the breakpoint.
    h.send("continue", json!({ "threadId": 1 }));
    let events = h.drive();
    let stopped = find_event(&events, "stopped").expect("stopped at breakpoint");
    assert_eq!(stopped["body"]["reason"], json!("breakpoint"));

    let st = h.send("stackTrace", json!({ "threadId": 1 }));
    let resp = find_response(&st, "stackTrace").expect("stackTrace response");
    let frame = &resp["body"]["stackFrames"][0];
    assert_eq!(frame["name"], json!("factorielle"));
    assert_eq!(frame["line"], json!(8));
}

#[test]
fn breakpoints_survive_restart() {
    let mut h = Harness::new();
    h.launch(true);

    // Line 8 is `cmp 1,%a` inside `factorielle`.
    let bp = h.send(
        "setBreakpoints",
        json!({
            "source": { "path": "/fact.s" },
            "breakpoints": [{ "line": 8 }],
        }),
    );
    let resp = find_response(&bp, "setBreakpoints").expect("setBreakpoints response");
    assert_eq!(resp["body"]["breakpoints"][0]["verified"], json!(true));

    // Restart: the client does NOT re-send setBreakpoints, so the session must
    // carry the breakpoint over and re-resolve it against the fresh program.
    let restart = h.send("restart", json!({}));
    assert_eq!(
        find_response(&restart, "restart").expect("restart response")["success"],
        json!(true)
    );

    // Continue from entry: we must still stop at the breakpoint on line 8.
    h.send("continue", json!({ "threadId": 1 }));
    let events = h.drive();
    let stopped = find_event(&events, "stopped").expect("stopped at breakpoint after restart");
    assert_eq!(stopped["body"]["reason"], json!("breakpoint"));

    let st = h.send("stackTrace", json!({ "threadId": 1 }));
    let resp = find_response(&st, "stackTrace").expect("stackTrace response");
    let frame = &resp["body"]["stackFrames"][0];
    assert_eq!(frame["name"], json!("factorielle"));
    assert_eq!(frame["line"], json!(8));
}

#[test]
fn breakpoint_on_comment_line_adjusts_forward() {
    let mut h = Harness::new();
    h.launch(true);

    // Line 5 is blank (between `reset` and `factorielle:`); the next code line
    // with an instruction is line 7 (`ld [%sp+1],%a`).
    let bp = h.send(
        "setBreakpoints",
        json!({
            "source": { "path": "/fact.s" },
            "breakpoints": [{ "line": 5 }],
        }),
    );
    let resp = find_response(&bp, "setBreakpoints").expect("setBreakpoints response");
    let breakpoint = &resp["body"]["breakpoints"][0];
    assert_eq!(breakpoint["verified"], json!(true));
    assert_eq!(breakpoint["line"], json!(7));
}

#[test]
fn next_steps_over_the_recursive_call() {
    let mut h = Harness::new();
    h.launch(true);

    // At entry (line 2, `push 5`): step to reach `call factorielle` (line 3).
    h.send("next", json!({ "threadId": 1 }));
    let events = h.drive();
    assert!(find_event(&events, "stopped").is_some());

    let st = h.send("stackTrace", json!({ "threadId": 1 }));
    let resp = find_response(&st, "stackTrace").expect("stackTrace response");
    assert_eq!(resp["body"]["stackFrames"][0]["line"], json!(3));

    // Record %sp before stepping over the call.
    let vars = h.send("variables", json!({ "variablesReference": 1 }));
    let sp_before = register_value(&vars, "sp");

    // `next` over `call factorielle` must run the whole recursion and land on
    // the following instruction (`reset`, line 4) with %sp restored.
    h.send("next", json!({ "threadId": 1 }));
    let events = h.drive();
    assert!(find_event(&events, "stopped").is_some());

    let st = h.send("stackTrace", json!({ "threadId": 1 }));
    let resp = find_response(&st, "stackTrace").expect("stackTrace response");
    assert_eq!(resp["body"]["stackFrames"][0]["line"], json!(4));

    let vars = h.send("variables", json!({ "variablesReference": 1 }));
    let sp_after = register_value(&vars, "sp");
    assert_eq!(
        sp_before, sp_after,
        "step-over must restore the stack pointer"
    );
}

#[test]
fn step_out_returns_to_caller() {
    let mut h = Harness::new();
    h.launch(true);

    // Break at the start of `factorielle` (line 7) and continue into it.
    h.send(
        "setBreakpoints",
        json!({
            "source": { "path": "/fact.s" },
            "breakpoints": [{ "line": 7 }],
        }),
    );
    h.send("continue", json!({ "threadId": 1 }));
    let events = h.drive();
    assert_eq!(
        find_event(&events, "stopped").expect("stopped")["body"]["reason"],
        json!("breakpoint")
    );

    // We are inside factorielle called from main. Remove the breakpoint so
    // stepOut is not re-triggered by the recursion, then step out.
    h.send(
        "setBreakpoints",
        json!({ "source": { "path": "/fact.s" }, "breakpoints": [] }),
    );
    h.send("stepOut", json!({ "threadId": 1 }));
    let events = h.drive();
    assert!(find_event(&events, "stopped").is_some());

    // After returning to main we should be at the `reset` (line 4).
    let st = h.send("stackTrace", json!({ "threadId": 1 }));
    let resp = find_response(&st, "stackTrace").expect("stackTrace response");
    assert_eq!(resp["body"]["stackFrames"][0]["name"], json!("main"));
    assert_eq!(resp["body"]["stackFrames"][0]["line"], json!(4));
}

#[test]
fn variables_show_registers() {
    let mut h = Harness::new();
    h.launch(true);

    let scopes = h.send("scopes", json!({ "frameId": 0 }));
    let resp = find_response(&scopes, "scopes").expect("scopes response");
    assert_eq!(resp["body"]["scopes"][0]["name"], json!("Registers"));
    assert_eq!(resp["body"]["scopes"][1]["name"], json!("Execution"));

    let vars = h.send("variables", json!({ "variablesReference": 1 }));
    let resp = find_response(&vars, "variables").expect("variables response");
    let names: Vec<&str> = resp["body"]["variables"]
        .as_array()
        .unwrap()
        .iter()
        .map(|v| v["name"].as_str().unwrap())
        .collect();
    assert!(names.contains(&"a"));
    assert!(names.contains(&"b"));
    assert!(names.contains(&"pc"));
    assert!(names.contains(&"sp"));
    assert!(names.contains(&"sr"));
    // `cycles` is no longer a register: it lives in the "Execution" scope.
    assert!(!names.contains(&"cycles"));

    let exec = h.send("variables", json!({ "variablesReference": 3 }));
    let resp = find_response(&exec, "variables").expect("variables response");
    let names: Vec<&str> = resp["body"]["variables"]
        .as_array()
        .unwrap()
        .iter()
        .map(|v| v["name"].as_str().unwrap())
        .collect();
    assert_eq!(names, vec!["cycles"]);
}

#[test]
fn set_variable_updates_register() {
    let mut h = Harness::new();
    h.launch(true);

    let resp = h.send(
        "setVariable",
        json!({ "variablesReference": 1, "name": "a", "value": "42" }),
    );
    let resp = find_response(&resp, "setVariable").expect("setVariable response");
    assert_eq!(resp["success"], json!(true));
    assert_eq!(resp["body"]["value"], json!("42"));

    let vars = h.send("variables", json!({ "variablesReference": 1 }));
    assert_eq!(register_value(&vars, "a"), "42");
}

#[test]
fn evaluate_resolves_a_label() {
    let mut h = Harness::new();
    h.launch(true);

    let resp = h.send(
        "evaluate",
        json!({ "expression": "factorielle", "context": "repl" }),
    );
    let resp = find_response(&resp, "evaluate").expect("evaluate response");
    assert_eq!(resp["success"], json!(true));
    assert_eq!(resp["body"]["type"], json!("address"));

    // Also resolve a register and an integer literal.
    let reg = h.send("evaluate", json!({ "expression": "%pc" }));
    assert_eq!(
        find_response(&reg, "evaluate").unwrap()["body"]["type"],
        json!("register")
    );
    let lit = h.send("evaluate", json!({ "expression": "0x10" }));
    assert_eq!(
        find_response(&lit, "evaluate").unwrap()["body"]["result"],
        json!("16")
    );
}

#[test]
fn running_to_completion_terminates() {
    let mut h = Harness::new();
    // Do not stop on entry: the program runs to the `reset` immediately.
    let out = h.launch(false);
    assert!(find_event(&out, "terminated").is_some());
    let exited = find_event(&out, "exited").expect("exited event");
    assert_eq!(exited["body"]["exitCode"], json!(0));
}

#[test]
fn continue_from_stop_runs_to_completion() {
    let mut h = Harness::new();
    h.launch(true);
    h.send("continue", json!({ "threadId": 1 }));
    let events = h.drive();
    assert!(find_event(&events, "terminated").is_some());
    assert_eq!(
        find_event(&events, "exited").unwrap()["body"]["exitCode"],
        json!(0)
    );
}

#[test]
fn breakpoint_on_entry_stops_when_not_stopping_on_entry() {
    let mut h = Harness::new();
    h.send("initialize", json!({}));
    h.send(
        "launch",
        json!({
            "program": "/fact.s",
            "entrypoint": "main",
            "stopOnEntry": false,
            "files": { "/fact.s": FACT_SOURCE },
        }),
    );
    // Breakpoint on the entry instruction (line 2, `push 5`).
    h.send(
        "setBreakpoints",
        json!({
            "source": { "path": "/fact.s" },
            "breakpoints": [{ "line": 2 }],
        }),
    );
    let mut out = h.send("configurationDone", json!({}));
    out.extend(h.drive());

    // Even with stopOnEntry=false, a breakpoint on the entry line must fire
    // instead of being run past.
    let stopped = find_event(&out, "stopped").expect("stopped event");
    assert_eq!(stopped["body"]["reason"], json!("breakpoint"));
    assert!(find_event(&out, "terminated").is_none());

    // We stopped before executing anything: the top frame is at `main`, line 2.
    let st = h.send("stackTrace", json!({ "threadId": 1 }));
    let resp = find_response(&st, "stackTrace").expect("stackTrace response");
    assert_eq!(resp["body"]["stackFrames"][0]["name"], json!("main"));
    assert_eq!(resp["body"]["stackFrames"][0]["line"], json!(2));
}

#[test]
fn stop_on_entry_breakpoint_does_not_double_fire() {
    let mut h = Harness::new();
    h.send("initialize", json!({}));
    h.send(
        "launch",
        json!({
            "program": "/fact.s",
            "entrypoint": "main",
            "stopOnEntry": true,
            "files": { "/fact.s": FACT_SOURCE },
        }),
    );
    h.send(
        "setBreakpoints",
        json!({
            "source": { "path": "/fact.s" },
            "breakpoints": [{ "line": 2 }],
        }),
    );
    let mut out = h.send("configurationDone", json!({}));
    out.extend(h.drive());
    // stopOnEntry wins: we stop once at entry.
    let stopped = find_event(&out, "stopped").expect("stopped at entry");
    assert_eq!(stopped["body"]["reason"], json!("entry"));

    // Continue must move PAST the entry breakpoint, not re-fire on it. With the
    // breakpoint only on the entry line, the program runs to completion.
    h.send("continue", json!({ "threadId": 1 }));
    let events = h.drive();
    assert!(
        find_event(&events, "stopped").is_none(),
        "continue must not re-fire the breakpoint at the entry address"
    );
    assert!(find_event(&events, "terminated").is_some());
}

#[test]
fn resume_after_unhandled_exception_terminates() {
    let mut h = Harness::new();
    h.send("initialize", json!({}));
    h.send(
        "launch",
        json!({
            "program": "/boom.s",
            "entrypoint": "main",
            "stopOnEntry": false,
            "files": { "/boom.s": FAULT_SOURCE },
        }),
    );
    let mut out = h.send("configurationDone", json!({}));
    out.extend(h.drive());
    // The unhandled exception surfaces as a stopped(exception) event.
    let stopped = find_event(&out, "stopped").expect("stopped event");
    assert_eq!(stopped["body"]["reason"], json!("exception"));

    // stepIn after the exception must end the session, not re-fault forever.
    let events = h.send("stepIn", json!({ "threadId": 1 }));
    assert!(find_event(&events, "terminated").is_some());
    let exited = find_event(&events, "exited").expect("exited event");
    assert_eq!(exited["body"]["exitCode"], json!(1));
    assert!(!h.session.is_running());

    // A subsequent resume is rejected because the session has terminated.
    let again = h.send("continue", json!({ "threadId": 1 }));
    let resp = find_response(&again, "continue").expect("continue response");
    assert_eq!(resp["success"], json!(false));
}

#[test]
fn continue_after_termination_is_rejected() {
    let mut h = Harness::new();
    // Runs straight to `reset` and terminates.
    let out = h.launch(false);
    assert!(find_event(&out, "terminated").is_some());
    assert!(!h.session.is_running());

    // A continue after termination must be an error and must not re-run the
    // program (no second terminated/exited pair).
    let events = h.send("continue", json!({ "threadId": 1 }));
    let resp = find_response(&events, "continue").expect("continue response");
    assert_eq!(resp["success"], json!(false));
    assert!(find_event(&events, "terminated").is_none());
    assert!(find_event(&events, "exited").is_none());
    assert!(!h.session.is_running());
}

/// A program exercising the globals scope: code labels, a single `.word`, a
/// multi-cell `.word` region and a larger `.space` array for paging.
const GLOBALS_SOURCE: &str = indoc::indoc! {r"
    main:
        reset

    value:
        .word 42

    array:
        .word 1
        .word 2
        .word 3

    tail:
        .word 99

    big:
        .space 300

    last:
        .word 7
"};

/// Launch the globals program, stopped on entry.
fn launch_globals(h: &mut Harness) {
    h.send("initialize", json!({}));
    h.send(
        "launch",
        json!({
            "program": "/g.s",
            "entrypoint": "main",
            "stopOnEntry": true,
            "files": { "/g.s": GLOBALS_SOURCE },
        }),
    );
    h.send("configurationDone", json!({}));
}

/// Fetch the scopes for a frame and return the `variablesReference` of the
/// scope with the given name.
fn scope_ref(h: &mut Harness, frame_id: i64, name: &str) -> i64 {
    let out = h.send("scopes", json!({ "frameId": frame_id }));
    let resp = find_response(&out, "scopes").expect("scopes response");
    resp["body"]["scopes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|s| s["name"] == name)
        .unwrap_or_else(|| panic!("scope {name} present"))["variablesReference"]
        .as_i64()
        .unwrap()
}

/// Fetch the variables for a reference (optionally paged) as an array.
fn variables(h: &mut Harness, reference: i64, extra: Value) -> Vec<Value> {
    let mut args = json!({ "variablesReference": reference });
    if let Value::Object(map) = extra {
        for (k, v) in map {
            args[k] = v;
        }
    }
    let out = h.send("variables", args);
    let resp = find_response(&out, "variables").expect("variables response");
    resp["body"]["variables"].as_array().cloned().unwrap()
}

/// Find a variable by name in a list.
fn var<'a>(vars: &'a [Value], name: &str) -> &'a Value {
    vars.iter()
        .find(|v| v["name"] == name)
        .unwrap_or_else(|| panic!("variable {name} present"))
}

/// Break inside `factorielle` (line 8, `cmp 1,%a`) and stop there.
fn stop_in_factorielle(h: &mut Harness) {
    h.launch(true);
    h.send(
        "setBreakpoints",
        json!({ "source": { "path": "/fact.s" }, "breakpoints": [{ "line": 8 }] }),
    );
    h.send("continue", json!({ "threadId": 1 }));
    let events = h.drive();
    assert_eq!(
        find_event(&events, "stopped").expect("stopped")["body"]["reason"],
        json!("breakpoint")
    );
}

#[test]
fn stack_scope_shows_pushed_arguments() {
    let mut h = Harness::new();
    stop_in_factorielle(&mut h);

    let stack = scope_ref(&mut h, 0, "Stack");
    let vars = variables(&mut h, stack, json!({}));

    // Top of stack is the return address into main; just above it is the
    // pushed argument (5).
    let sp0 = var(&vars, "sp+0");
    assert!(
        sp0["value"].as_str().unwrap().contains("return to main"),
        "sp+0 = {}",
        sp0["value"]
    );
    assert!(sp0["memoryReference"].is_string());

    let sp1 = var(&vars, "sp+1");
    assert_eq!(sp1["value"], json!("5"));
    assert!(sp1["memoryReference"].is_string());
}

#[test]
fn caller_frame_has_frame_stack_scope() {
    let mut h = Harness::new();
    stop_in_factorielle(&mut h);

    // Frame 0 is factorielle; frame 1 is its caller (main). main pushed the
    // argument 5 for the call, exposed by the best-effort Frame stack scope.
    let st = h.send("stackTrace", json!({ "threadId": 1 }));
    let frames = find_response(&st, "stackTrace").unwrap()["body"]["stackFrames"]
        .as_array()
        .unwrap()
        .len();
    assert!(frames >= 2, "expected a synthesized caller frame");

    let fs = scope_ref(&mut h, 1, "Frame stack");
    let vars = variables(&mut h, fs, json!({}));
    assert_eq!(var(&vars, "arg1 (9999)")["value"], json!("5"));
}

#[test]
fn globals_scope_single_and_multi_cell() {
    let mut h = Harness::new();
    launch_globals(&mut h);

    let globals = scope_ref(&mut h, 0, "Globals");
    let vars = variables(&mut h, globals, json!({}));

    // Single-cell data label.
    let value = var(&vars, "value");
    assert_eq!(value["value"], json!("42"));
    assert_eq!(value["type"], json!("word"));
    assert_eq!(value["variablesReference"], json!(0));
    assert!(value["memoryReference"].is_string());

    // Single-cell code label renders the instruction.
    assert_eq!(var(&vars, "main")["type"], json!("instruction"));

    // Multi-cell region expands and reports its length.
    let array = var(&vars, "array");
    assert_eq!(array["indexedVariables"], json!(3));
    let array_ref = array["variablesReference"].as_i64().unwrap();
    assert!(array_ref >= 1000, "multi-cell region gets a dynamic handle");

    let children = variables(&mut h, array_ref, json!({}));
    assert_eq!(children.len(), 3);
    assert_eq!(children[0]["value"], json!("1"));
    assert_eq!(children[1]["value"], json!("2"));
    assert_eq!(children[2]["value"], json!("3"));

    // Paging: start/count are honoured.
    let page = variables(&mut h, array_ref, json!({ "start": 1, "count": 1 }));
    assert_eq!(page.len(), 1);
    assert_eq!(page[0]["value"], json!("2"));

    // A large .space is paged (default page cap), not dumped whole.
    let big = var(&vars, "big");
    assert_eq!(big["indexedVariables"], json!(300));
    let big_ref = big["variablesReference"].as_i64().unwrap();
    let big_children = variables(&mut h, big_ref, json!({}));
    assert_eq!(
        big_children.len(),
        256,
        "default page size caps the response"
    );
}

#[test]
fn set_variable_writes_stack_and_global_cells() {
    let mut h = Harness::new();
    // Global cell.
    launch_globals(&mut h);
    let globals = scope_ref(&mut h, 0, "Globals");
    let resp = h.send(
        "setVariable",
        json!({ "variablesReference": globals, "name": "value", "value": "1234" }),
    );
    let resp = find_response(&resp, "setVariable").expect("setVariable response");
    assert_eq!(resp["success"], json!(true));
    assert_eq!(resp["body"]["value"], json!("1234"));
    // Verify via evaluate.
    let ev = h.send("evaluate", json!({ "expression": "[value]" }));
    assert!(find_response(&ev, "evaluate").unwrap()["body"]["result"]
        .as_str()
        .unwrap()
        .starts_with("1234"));

    // Stack cell.
    let mut h = Harness::new();
    stop_in_factorielle(&mut h);
    let stack = scope_ref(&mut h, 0, "Stack");
    let resp = h.send(
        "setVariable",
        json!({ "variablesReference": stack, "name": "sp+1", "value": "77" }),
    );
    assert_eq!(
        find_response(&resp, "setVariable").unwrap()["body"]["value"],
        json!("77")
    );
    let ev = h.send("evaluate", json!({ "expression": "[%sp+1]" }));
    assert_eq!(
        find_response(&ev, "evaluate").unwrap()["body"]["result"],
        json!("77 (9999)")
    );
}

#[test]
fn evaluate_dereference_and_arithmetic() {
    let mut h = Harness::new();
    launch_globals(&mut h);

    // `[array]` and `[array+1]` dereference memory.
    let d0 = h.send("evaluate", json!({ "expression": "[array]" }));
    assert_eq!(
        find_response(&d0, "evaluate").unwrap()["body"]["result"]
            .as_str()
            .unwrap()
            .split_whitespace()
            .next()
            .unwrap(),
        "1"
    );
    let d1 = h.send("evaluate", json!({ "expression": "[array+1]" }));
    let r1 = find_response(&d1, "evaluate").unwrap();
    assert_eq!(
        r1["body"]["result"]
            .as_str()
            .unwrap()
            .split_whitespace()
            .next()
            .unwrap(),
        "2"
    );
    assert!(r1["body"]["memoryReference"].is_string());

    // Bare `array+2` evaluates to the address value.
    let a = h.send("evaluate", json!({ "expression": "array+2" }));
    let ra = find_response(&a, "evaluate").unwrap();
    assert_eq!(ra["body"]["type"], json!("address"));
    assert!(ra["body"]["memoryReference"].is_string());
}

#[test]
fn evaluate_register_indexed_dereference() {
    let mut h = Harness::new();
    stop_in_factorielle(&mut h);
    // `[%sp+1]` is the pushed argument, 5.
    let ev = h.send("evaluate", json!({ "expression": "[%sp+1]" }));
    assert_eq!(
        find_response(&ev, "evaluate").unwrap()["body"]["result"],
        json!("5 (9999)")
    );
}

#[test]
fn read_and_write_memory_roundtrip() {
    let mut h = Harness::new();
    launch_globals(&mut h);
    // Resolve the address of `value` (holds 42).
    let ev = h.send("evaluate", json!({ "expression": "value" }));
    let mref = find_response(&ev, "evaluate").unwrap()["body"]["memoryReference"]
        .as_str()
        .unwrap()
        .to_owned();

    // Read one cell (8 bytes): little-endian 42.
    let rd = h.send(
        "readMemory",
        json!({ "memoryReference": mref, "offset": 0, "count": 8 }),
    );
    let data = find_response(&rd, "readMemory").unwrap()["body"]["data"]
        .as_str()
        .unwrap()
        .to_owned();
    // base64 of 42u64 little-endian = "KgAAAAAAAAA=".
    assert_eq!(data, "KgAAAAAAAAA=");

    // Write 0x0100 = 256 into that cell and read it back.
    let wr = h.send(
        "writeMemory",
        json!({ "memoryReference": mref, "offset": 0, "data": "AAEAAAAAAAA=" }),
    );
    assert_eq!(
        find_response(&wr, "writeMemory").unwrap()["body"]["bytesWritten"],
        json!(8)
    );
    let ev = h.send("evaluate", json!({ "expression": "[value]" }));
    assert!(find_response(&ev, "evaluate").unwrap()["body"]["result"]
        .as_str()
        .unwrap()
        .starts_with("256"));

    // Offset conversion: reading the next cell via a byte offset of 8 sees
    // `array`'s first word (1).
    let rd2 = h.send(
        "readMemory",
        json!({ "memoryReference": mref, "offset": 8, "count": 8 }),
    );
    assert_eq!(
        find_response(&rd2, "readMemory").unwrap()["body"]["data"],
        json!("AQAAAAAAAAA=")
    );
}

#[test]
fn scopes_with_sp_above_stack_start_is_empty() {
    let mut h = Harness::new();
    h.launch(true);

    // Push `sp` past the stack base via setVariable. The stack depth is
    // `STACK_START - sp`, which must saturate to 0 instead of underflowing.
    let resp = h.send(
        "setVariable",
        json!({ "variablesReference": 1, "name": "sp", "value": "10001" }),
    );
    assert_eq!(
        find_response(&resp, "setVariable").expect("setVariable response")["success"],
        json!(true)
    );

    // Requesting scopes must not panic and the Stack scope is empty.
    let out = h.send("scopes", json!({ "frameId": 0 }));
    let scopes = find_response(&out, "scopes").expect("scopes response");
    let stack = scopes["body"]["scopes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|s| s["name"] == "Stack")
        .expect("Stack scope present");
    assert_eq!(stack["indexedVariables"], json!(0));
}

#[test]
fn write_memory_straddling_end_is_atomic() {
    let mut h = Harness::new();
    launch_globals(&mut h);

    // Seed the last valid cell (9999) with a known pattern.
    let seed = h.send(
        "writeMemory",
        json!({ "memoryReference": "9999", "offset": 0, "data": "AQIDBAUGBwg=" }),
    );
    assert_eq!(
        find_response(&seed, "writeMemory").unwrap()["body"]["bytesWritten"],
        json!(8)
    );

    // A 16-byte write from cell 9999 straddles the end of memory (cell 10000
    // is out of range) and must be rejected atomically.
    let wr = h.send(
        "writeMemory",
        json!({ "memoryReference": "9999", "offset": 0, "data": "/////////////////////w==" }),
    );
    assert_eq!(
        find_response(&wr, "writeMemory").expect("writeMemory response")["success"],
        json!(false)
    );

    // The seeded cell must be unchanged (no partial write leaked through).
    let rd = h.send(
        "readMemory",
        json!({ "memoryReference": "9999", "offset": 0, "count": 8 }),
    );
    assert_eq!(
        find_response(&rd, "readMemory").unwrap()["body"]["data"],
        json!("AQIDBAUGBwg=")
    );
}

#[test]
fn read_memory_overflowing_reference_is_rejected() {
    let mut h = Harness::new();
    launch_globals(&mut h);

    // A cell address this large overflows when multiplied by the cell size
    // (8 bytes); it must produce an error response, not panic.
    let rd = h.send(
        "readMemory",
        json!({ "memoryReference": "1500000000000000000", "offset": 0, "count": 8 }),
    );
    assert_eq!(
        find_response(&rd, "readMemory").expect("readMemory response")["success"],
        json!(false)
    );
}

#[test]
fn read_memory_unaligned_offset_reports_cell_address() {
    let mut h = Harness::new();
    launch_globals(&mut h);
    let ev = h.send("evaluate", json!({ "expression": "value" }));
    let mref = find_response(&ev, "evaluate").unwrap()["body"]["memoryReference"]
        .as_str()
        .unwrap()
        .to_owned();

    // Seed a known little-endian byte pattern into the cell: bytes 01..08.
    h.send(
        "writeMemory",
        json!({ "memoryReference": mref, "offset": 0, "data": "AQIDBAUGBwg=" }),
    );

    // Read 2 bytes at an unaligned byte offset of 1 (mid-cell): bytes 02, 03.
    let rd = h.send(
        "readMemory",
        json!({ "memoryReference": mref, "offset": 1, "count": 2 }),
    );
    let body = &find_response(&rd, "readMemory").unwrap()["body"];
    // base64 of [0x02, 0x03].
    assert_eq!(body["data"], json!("AgM="));
    // The `address` field is cell-granular: it names the containing cell, not
    // the byte offset, so it still equals the original memory reference.
    assert_eq!(body["address"], json!(mref));
}

#[test]
fn read_memory_past_end_reports_unreadable_tail() {
    let mut h = Harness::new();
    launch_globals(&mut h);

    // Cell 9999 is the last valid cell (MEMORY_SIZE == 10000). Reading two
    // cells' worth of bytes from there leaves the second cell unreadable.
    let rd = h.send(
        "readMemory",
        json!({ "memoryReference": "9999", "offset": 0, "count": 16 }),
    );
    let body = &find_response(&rd, "readMemory").unwrap()["body"];
    assert_eq!(body["address"], json!("9999"));
    assert_eq!(body["unreadableBytes"], json!(8));
    // Only the readable 8 bytes are returned (base64 of 8 zero bytes).
    assert_eq!(body["data"], json!("AAAAAAAAAAA="));
}

#[test]
fn write_memory_unaligned_requires_allow_partial() {
    let mut h = Harness::new();
    launch_globals(&mut h);
    let ev = h.send("evaluate", json!({ "expression": "value" }));
    let mref = find_response(&ev, "evaluate").unwrap()["body"]["memoryReference"]
        .as_str()
        .unwrap()
        .to_owned();

    // Unaligned (mid-cell) write without allowPartial is rejected.
    let wr = h.send(
        "writeMemory",
        json!({ "memoryReference": mref, "offset": 1, "data": "/w==" }),
    );
    let resp = find_response(&wr, "writeMemory").expect("writeMemory response");
    assert_eq!(resp["success"], json!(false));

    // `value` holds 42; it must be untouched by the rejected write.
    let ev = h.send("evaluate", json!({ "expression": "[value]" }));
    assert!(find_response(&ev, "evaluate").unwrap()["body"]["result"]
        .as_str()
        .unwrap()
        .starts_with("42"));

    // With allowPartial the same write succeeds via read-modify-write, setting
    // byte 1 of the cell to 0xff: 42 | (0xff << 8) == 65322.
    let wr = h.send(
        "writeMemory",
        json!({ "memoryReference": mref, "offset": 1, "data": "/w==", "allowPartial": true }),
    );
    assert_eq!(
        find_response(&wr, "writeMemory").unwrap()["body"]["bytesWritten"],
        json!(1)
    );
    let ev = h.send("evaluate", json!({ "expression": "[value]" }));
    assert!(find_response(&ev, "evaluate").unwrap()["body"]["result"]
        .as_str()
        .unwrap()
        .starts_with("65322"));
}

#[test]
fn set_variable_parses_negative_hex() {
    let mut h = Harness::new();
    launch_globals(&mut h);
    let globals = scope_ref(&mut h, 0, "Globals");
    let resp = h.send(
        "setVariable",
        json!({ "variablesReference": globals, "name": "value", "value": "-0x2a" }),
    );
    let resp = find_response(&resp, "setVariable").expect("setVariable response");
    assert_eq!(resp["success"], json!(true));
    // -0x2a == -42.
    assert_eq!(resp["body"]["value"], json!("-42"));
    let ev = h.send("evaluate", json!({ "expression": "[value]" }));
    assert!(find_response(&ev, "evaluate").unwrap()["body"]["result"]
        .as_str()
        .unwrap()
        .starts_with("-42"));
}

#[test]
fn hex_format_rendering() {
    let mut h = Harness::new();
    launch_globals(&mut h);
    let globals = scope_ref(&mut h, 0, "Globals");
    let vars = variables(&mut h, globals, json!({ "format": { "hex": true } }));
    // 42 == 0x2a.
    assert_eq!(var(&vars, "value")["value"], json!("0x2a"));

    // evaluate also honours the hex format.
    let ev = h.send(
        "evaluate",
        json!({ "expression": "0x2a", "format": { "hex": true } }),
    );
    assert_eq!(
        find_response(&ev, "evaluate").unwrap()["body"]["result"],
        json!("0x2a")
    );
}

#[test]
fn handles_invalidated_after_resume() {
    let mut h = Harness::new();
    launch_globals(&mut h);
    let globals = scope_ref(&mut h, 0, "Globals");
    let vars = variables(&mut h, globals, json!({}));
    let array_ref = var(&vars, "array")["variablesReference"].as_i64().unwrap();
    // Valid while stopped.
    assert_eq!(variables(&mut h, array_ref, json!({})).len(), 3);

    // Resume; the dynamic handle is now stale and returns an empty list.
    h.send("continue", json!({ "threadId": 1 }));
    h.drive();
    // Relaunch a stop so the session is inspectable again would allocate fresh
    // handles; the OLD handle must not resolve.
    let stale = variables(&mut h, array_ref, json!({}));
    assert!(stale.is_empty(), "stale handle must return no variables");
}

/// Extract a register's string value from a `variables` response.
fn register_value(messages: &[Value], name: &str) -> String {
    let resp = find_response(messages, "variables").expect("variables response");
    resp["body"]["variables"]
        .as_array()
        .unwrap()
        .iter()
        .find(|v| v["name"] == name)
        .and_then(|v| v["value"].as_str())
        .expect("register present")
        .to_owned()
}
