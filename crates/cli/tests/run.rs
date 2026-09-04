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

#[test]
fn a_fault_names_the_faulting_address_and_the_reason() {
    let path = std::path::Path::new(env!("CARGO_TARGET_TMPDIR")).join("run-fault.s");
    std::fs::write(&path, "main:\n    nop\n    ld [99999], %a\n    reset\n").unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_z33-cli"))
        .args(["run", path.to_str().unwrap(), "main"])
        .env("RUST_LOG", "warn")
        .stdin(Stdio::null())
        .output()
        .expect("failed to run z33-cli");
    assert!(!output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("address 1001"), "{stdout}");
    assert!(stdout.contains("invalid address 99999"), "{stdout}");
}

#[test]
fn json_can_be_combined_with_the_colour_flags() {
    for flag in ["--color", "--no-color"] {
        let output = run_fact(&["--json", flag]);
        assert!(
            output.status.success(),
            "{flag}: stderr: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(!stdout.contains('\u{1b}'), "{flag}: {stdout}");
        for line in stdout.lines().filter(|l| !l.trim().is_empty()) {
            serde_json::from_str::<serde_json::Value>(line)
                .unwrap_or_else(|e| panic!("{flag}: {line:?} is not JSON: {e}"));
        }
    }
}
