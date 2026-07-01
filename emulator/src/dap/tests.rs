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
