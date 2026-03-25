mod helpers;

use helpers::{run_program, Steps};
use indoc::indoc;

// =============================================================================
// ADD
// =============================================================================

#[test]
fn add_immediate() {
    // r[verify inst.add]
    let state = run_program(
        indoc! {"
            main:
                ld 10, %a
                add 32, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 42
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn add_register_to_register() {
    // r[verify inst.add]
    let state = run_program(
        indoc! {"
            main:
                ld 100, %a
                ld 200, %b
                add %b, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 300
      %b  = 200
      %pc = 1003
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    ");
}

#[test]
fn add_negative_values() {
    // r[verify inst.add]
    let state = run_program(
        indoc! {"
            main:
                ld -10, %a
                add -20, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = -30
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = NEGATIVE | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn add_sets_zero_flag() {
    // r[verify inst.add]
    let state = run_program(
        indoc! {"
            main:
                ld 5, %a
                add -5, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 0
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = ZERO | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn add_sets_negative_flag() {
    // r[verify inst.add]
    let state = run_program(
        indoc! {"
            main:
                ld -10, %a
                add 3, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = -7
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = NEGATIVE | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn add_overflow() {
    // r[verify inst.add]
    let state = run_program(
        indoc! {"
            main:
                ld 9223372036854775807, %a
                add 1, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = -9223372036854775808
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = NEGATIVE | OVERFLOW | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

// =============================================================================
// SUB
// =============================================================================

#[test]
fn sub_immediate() {
    // r[verify inst.sub]
    let state = run_program(
        indoc! {"
            main:
                ld 100, %a
                sub 58, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 42
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn sub_register() {
    // r[verify inst.sub]
    let state = run_program(
        indoc! {"
            main:
                ld 50, %a
                ld 30, %b
                sub %b, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 20
      %b  = 30
      %pc = 1003
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    ");
}

#[test]
fn sub_sets_zero_flag() {
    // r[verify inst.sub]
    let state = run_program(
        indoc! {"
            main:
                ld 42, %a
                sub 42, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 0
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = ZERO | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn sub_sets_negative_flag() {
    // r[verify inst.sub]
    let state = run_program(
        indoc! {"
            main:
                ld 10, %a
                sub 20, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = -10
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = NEGATIVE | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn sub_overflow() {
    // r[verify inst.sub]
    let state = run_program(
        indoc! {"
            main:
                ld -9223372036854775808, %a
                sub 1, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 9223372036854775807
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = OVERFLOW | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

// =============================================================================
// MUL
// =============================================================================

#[test]
fn mul_immediate() {
    // r[verify inst.mul]
    let state = run_program(
        indoc! {"
            main:
                ld 6, %a
                mul 7, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 42
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn mul_by_zero() {
    // r[verify inst.mul]
    let state = run_program(
        indoc! {"
            main:
                ld 999, %a
                mul 0, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 0
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = ZERO | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn mul_negative_values() {
    // r[verify inst.mul]
    let state = run_program(
        indoc! {"
            main:
                ld -3, %a
                mul 4, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = -12
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = NEGATIVE | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn mul_overflow() {
    // r[verify inst.mul]
    let state = run_program(
        indoc! {"
            main:
                ld 9223372036854775807, %a
                mul 2, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = -2
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = NEGATIVE | OVERFLOW | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

// =============================================================================
// DIV
// =============================================================================

#[test]
fn div_exact() {
    // r[verify inst.div]
    let state = run_program(
        indoc! {"
            main:
                ld 42, %a
                div 6, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 7
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn div_truncation_toward_zero_positive() {
    // r[verify inst.div]
    let state = run_program(
        indoc! {"
            main:
                ld 7, %a
                div 2, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 3
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn div_truncation_toward_zero_negative() {
    // r[verify inst.div]
    let state = run_program(
        indoc! {"
            main:
                ld -7, %a
                div 2, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = -3
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = NEGATIVE | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn div_negative_by_negative() {
    // r[verify inst.div]
    let state = run_program(
        indoc! {"
            main:
                ld -10, %a
                div -3, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 3
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn div_by_zero_triggers_exception() {
    // r[verify inst.div.exception.division-by-zero]
    let state = run_program(
        indoc! {"
            .addr 200
            handler: reset

            .addr 1000
            main:
                ld 42, %a
                div 0, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 42
      %b  = 0
      %pc = 200
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    Memory:
      [100] = 1002
      [101] = 512
      [102] = 1
    ");
}

#[test]
fn div_register_by_zero_triggers_exception() {
    // r[verify inst.div.exception.division-by-zero]
    let state = run_program(
        indoc! {"
            .addr 200
            handler: reset

            .addr 1000
            main:
                ld 100, %a
                ld 0, %b
                div %b, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 100
      %b  = 0
      %pc = 200
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    Memory:
      [100] = 1003
      [101] = 512
      [102] = 1
    ");
}

// =============================================================================
// NEG
// =============================================================================

#[test]
fn neg_positive() {
    // r[verify inst.neg]
    let state = run_program(
        indoc! {"
            main:
                ld 42, %a
                neg %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = -42
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = NEGATIVE | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn neg_negative() {
    // r[verify inst.neg]
    let state = run_program(
        indoc! {"
            main:
                ld -15, %a
                neg %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 15
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn neg_zero() {
    // r[verify inst.neg]
    let state = run_program(
        indoc! {"
            main:
                ld 0, %a
                neg %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 0
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = ZERO | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn neg_min_value_overflows() {
    // r[verify inst.neg]
    let state = run_program(
        indoc! {"
            main:
                ld -9223372036854775808, %a
                neg %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = -9223372036854775808
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = NEGATIVE | OVERFLOW | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}
