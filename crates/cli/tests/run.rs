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
fn dumps_registers_on_stdout() {
    let output = run_fact(&[]);
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("End of program registers=%a = 120"));
}

#[test]
fn log_flag_appends_to_file() {
    let path = std::path::Path::new(env!("CARGO_TARGET_TMPDIR")).join("z33-log-test.log");
    let _ = std::fs::remove_file(&path);
    let path = path.to_str().unwrap();

    for _ in 0..2 {
        let output = run_fact(&["--log", path]);
        assert!(output.status.success());
        assert!(output.stdout.is_empty());
    }

    let logs = std::fs::read_to_string(path).unwrap();
    assert_eq!(logs.matches("End of program registers=%a = 120").count(), 2);
}
