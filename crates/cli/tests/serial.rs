//! End-to-end tests for the serial console wired to `z33-cli run`.
//!
//! These drive the compiled binary directly (via the `CARGO_BIN_EXE_*` env var
//! cargo provides to integration tests), pipe bytes to its stdin, and assert on
//! what the guest echoes back to stdout. The interactive (`-i`) path uses
//! rustyline and reads the terminal device directly, so it cannot be driven
//! from a pipe and is not covered here.

use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};

/// Path to a file in the repository's top-level `samples/` directory.
fn sample(name: &str) -> PathBuf {
    // CARGO_MANIFEST_DIR is crates/cli; samples/ lives at the workspace root.
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../samples")
        .join(name)
}

/// Run `z33-cli run <sample> main`, feed `input` to stdin, and return the raw
/// stdout bytes.
///
/// Logs go to stderr (routed there for the `run` subcommand precisely so they
/// never garble the guest's serial output on stdout), which we discard.
/// `RUST_LOG=warn` additionally quiets the per-instruction info logging so the
/// discarded stderr stays cheap; it is no longer needed for stdout cleanliness.
fn run_echo_raw(sample_name: &str, input: &[u8]) -> Vec<u8> {
    let mut child = Command::new(env!("CARGO_BIN_EXE_z33-cli"))
        .args(["run", sample(sample_name).to_str().unwrap(), "main"])
        .env("RUST_LOG", "warn")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .expect("failed to spawn z33-cli");

    child
        .stdin
        .take()
        .expect("stdin")
        .write_all(input)
        .expect("write stdin");

    let output = child.wait_with_output().expect("wait for z33-cli");
    assert!(
        output.status.success(),
        "z33-cli exited with {:?}",
        output.status
    );
    output.stdout
}

/// Like [`run_echo_raw`], but decodes stdout as UTF-8 for text assertions.
fn run_echo(sample_name: &str, input: &[u8]) -> String {
    String::from_utf8(run_echo_raw(sample_name, input)).expect("stdout is utf-8")
}

#[test]
fn polling_echo_replays_input_and_exits_on_eot() {
    // "hi" then Ctrl-D (EOT, byte 4) to end the program.
    let stdout = run_echo("echo.s", b"hi\x04");
    assert_eq!(stdout, "hi");
}

#[test]
fn polling_echo_handles_multiple_lines() {
    // A full line (echoed verbatim, newline included) followed by EOT on the
    // next read. Exercises multi-byte input and EOF-after-partial-line.
    let stdout = run_echo("echo.s", b"hey\n\x04");
    assert_eq!(stdout, "hey\n");
}

#[test]
fn polling_echo_passes_non_utf8_bytes_through() {
    // A lone 0xFF byte is not valid UTF-8. The stdin reader must forward raw
    // bytes (not decode a `String`, which would error and drop the input), so
    // the byte is echoed verbatim before EOT (byte 4) ends the program.
    let stdout = run_echo_raw("echo.s", b"\xff\x04");
    assert_eq!(stdout, b"\xff");
}
