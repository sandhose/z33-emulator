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
