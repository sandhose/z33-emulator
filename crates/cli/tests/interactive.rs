//! End-to-end tests for the interactive mode of `z33-cli run -i`.
//!
//! rustyline falls back to plain reads when stdin is not a terminal, so the
//! debugger can be scripted by piping commands into it.

use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Output, Stdio};

fn sample(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../samples")
        .join(name)
}

/// Run `z33-cli run -i <sample> main` with the given commands on stdin.
fn run_interactive(sample_name: &str, commands: &str) -> Output {
    let mut child = Command::new(env!("CARGO_BIN_EXE_z33-cli"))
        .args(["run", "-i", sample(sample_name).to_str().unwrap(), "main"])
        .env("RUST_LOG", "info")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn z33-cli");

    child
        .stdin
        .take()
        .expect("stdin")
        .write_all(commands.as_bytes())
        .expect("write stdin");

    child.wait_with_output().expect("wait for z33-cli")
}

#[test]
fn list_with_a_huge_count_stays_within_memory() {
    let output = run_interactive("fact.s", "list 4294967295\nexit\n");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "stderr: {stderr}");
    assert!(!stderr.contains("panicked"), "stderr: {stderr}");
    // The listing stops at the last cell of the 10_000-cell memory.
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("9999"), "{stdout}");
}

#[test]
fn list_warns_once_the_cursor_is_at_the_end_of_memory() {
    let output = run_interactive("fact.s", "list 4294967295\nlist\nexit\n");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "stderr: {stderr}");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Nothing to list"), "{stdout}");
}

#[test]
fn quit_and_q_leave_the_debugger() {
    for command in ["quit\n", "q\n"] {
        let output = run_interactive("fact.s", command);
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(output.status.success(), "{command:?}: stderr: {stderr}");
        // Interactive log lines go to stdout, clap's parse errors included.
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(!stdout.contains("WARN"), "{command:?}: {stdout}");
        assert!(stdout.contains("End of program"), "{command:?}: {stdout}");
    }
}

#[test]
fn negative_numbers_are_accepted_as_arguments() {
    let output = run_interactive("fact.s", "set %a -1\nregisters a\nmemory 1000 -3\nexit\n");
    assert!(output.status.success());
    // Interactive log lines go to stdout, clap's parse errors included.
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("%a = -1"), "{stdout}");
    assert!(!stdout.contains("unexpected argument"), "{stdout}");
    assert!(!stdout.contains("Invalid input"), "{stdout}");
    for address in ["address=1000", "address=999", "address=998"] {
        assert!(stdout.contains(address), "{stdout}");
    }
}

#[test]
fn a_negative_memory_count_stops_at_the_bottom_of_memory() {
    let output = run_interactive("fact.s", "memory 0 -3\nexit\n");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "stderr: {stderr}");
    assert!(!stderr.contains("panicked"), "stderr: {stderr}");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("address=0"), "{stdout}");
}

#[test]
fn a_faulting_program_makes_interactive_mode_exit_non_zero() {
    let path = std::path::Path::new(env!("CARGO_TARGET_TMPDIR")).join("fault.s");
    std::fs::write(&path, "main:\n    ld [99999], %a\n    reset\n").unwrap();
    let mut child = Command::new(env!("CARGO_BIN_EXE_z33-cli"))
        .args(["run", "-i", path.to_str().unwrap(), "main"])
        .env("RUST_LOG", "info")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn z33-cli");
    child
        .stdin
        .take()
        .unwrap()
        .write_all(b"continue\nexit\n")
        .unwrap();
    let output = child.wait_with_output().unwrap();
    assert!(!output.status.success(), "a fault must not exit 0");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Program faulted at address 1000"),
        "{stdout}"
    );
    assert!(stdout.contains("invalid address 99999"), "{stdout}");
}

#[test]
fn a_reset_is_reported_as_a_normal_end() {
    let output = run_interactive("fact.s", "continue\nexit\n");
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Program ended (reset)"), "{stdout}");
    assert!(!stdout.contains("Halted"), "{stdout}");
}
