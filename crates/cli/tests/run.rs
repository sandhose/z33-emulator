//! End-to-end tests for `z33-cli run`.

use std::process::{Command, Output, Stdio};

/// Run `z33-cli run <extra_args> samples/fact.s main` with stdin closed.
fn run_fact(extra_args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_z33-cli"))
        .arg("run")
        .args(extra_args)
        .arg(concat!(env!("CARGO_MANIFEST_DIR"), "/../../samples/fact.s"))
        .arg("main")
        .stdin(Stdio::null())
        .output()
        .expect("failed to run z33-cli")
}

#[test]
fn dumps_registers_at_end_of_program() {
    let output = run_fact(&[]);
    assert!(output.status.success());
    let logs = [output.stdout, output.stderr].concat();
    assert!(String::from_utf8_lossy(&logs).contains("End of program registers=%a = 120"));
}
