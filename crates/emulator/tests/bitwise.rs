mod helpers;

use helpers::{run_program, Steps};
use indoc::indoc;

// =============================================================================
// AND
// =============================================================================

#[test]
fn and_immediate() {
    // r[verify inst.and]
    let state = run_program(
        indoc! {"
            main:
                ld 0xFF, %a
                and 0x0F, %a
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
fn and_register() {
    // r[verify inst.and]
    let state = run_program(
        indoc! {"
            main:
                ld 0b11001100, %a
                ld 0b10101010, %b
                and %b, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 136
      %b  = 170
      %pc = 1003
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    ");
}

#[test]
fn and_zero_flag() {
    // r[verify inst.and]
    let state = run_program(
        indoc! {"
            main:
                ld 0xF0, %a
                and 0x0F, %a
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
fn and_negative_flag() {
    // r[verify inst.and]
    let state = run_program(
        indoc! {"
            main:
                ld -1, %a
                and -2, %a
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
      %sr = NEGATIVE | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

// =============================================================================
// OR
// =============================================================================

#[test]
fn or_immediate() {
    // r[verify inst.or]
    let state = run_program(
        indoc! {"
            main:
                ld 0xF0, %a
                or 0x0F, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 255
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn or_register() {
    // r[verify inst.or]
    let state = run_program(
        indoc! {"
            main:
                ld 0b11001100, %a
                ld 0b00110011, %b
                or %b, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 255
      %b  = 51
      %pc = 1003
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    ");
}

#[test]
fn or_zero_flag() {
    // r[verify inst.or]
    let state = run_program(
        indoc! {"
            main:
                ld 0, %a
                or 0, %a
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
fn or_negative_flag() {
    // r[verify inst.or]
    let state = run_program(
        indoc! {"
            main:
                ld 0, %a
                or -1, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = -1
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = NEGATIVE | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

// =============================================================================
// XOR
// =============================================================================

#[test]
fn xor_immediate() {
    // r[verify inst.xor]
    let state = run_program(
        indoc! {"
            main:
                ld 0xFF, %a
                xor 0x0F, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 240
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn xor_self_is_zero() {
    // r[verify inst.xor]
    let state = run_program(
        indoc! {"
            main:
                ld 12345, %a
                ld 12345, %b
                xor %b, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 0
      %b  = 12345
      %pc = 1003
      %sp = 10000
      %sr = ZERO | SUPERVISOR
    Cycles: 3
    Halted: reset
    ");
}

#[test]
fn xor_register() {
    // r[verify inst.xor]
    let state = run_program(
        indoc! {"
            main:
                ld 0b11001100, %a
                ld 0b10101010, %b
                xor %b, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 102
      %b  = 170
      %pc = 1003
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    ");
}

#[test]
fn xor_negative_flag() {
    // r[verify inst.xor]
    let state = run_program(
        indoc! {"
            main:
                ld 0, %a
                xor -1, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = -1
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = NEGATIVE | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

// =============================================================================
// NOT
// =============================================================================

#[test]
fn not_zero() {
    // r[verify inst.not]
    let state = run_program(
        indoc! {"
            main:
                ld 0, %a
                not %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = -1
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = NEGATIVE | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn not_all_ones() {
    // r[verify inst.not]
    let state = run_program(
        indoc! {"
            main:
                ld -1, %a
                not %a
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
fn not_pattern() {
    // r[verify inst.not]
    let state = run_program(
        indoc! {"
            main:
                ld 0xFF, %a
                not %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = -256
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = NEGATIVE | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn not_double_is_identity() {
    // r[verify inst.not]
    let state = run_program(
        indoc! {"
            main:
                ld 42, %a
                not %a
                not %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 42
      %b  = 0
      %pc = 1003
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    ");
}

// =============================================================================
// SHL (shift left)
// =============================================================================

#[test]
fn shl_by_one() {
    // r[verify inst.shl]
    let state = run_program(
        indoc! {"
            main:
                ld 1, %a
                shl 1, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 2
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn shl_by_four() {
    // r[verify inst.shl]
    let state = run_program(
        indoc! {"
            main:
                ld 0xFF, %a
                shl 4, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 4080
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn shl_by_zero() {
    // r[verify inst.shl]
    let state = run_program(
        indoc! {"
            main:
                ld 42, %a
                shl 0, %a
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
fn shl_zero_flag() {
    // r[verify inst.shl]
    let state = run_program(
        indoc! {"
            main:
                ld 0, %a
                shl 8, %a
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
fn shl_register_operand() {
    // r[verify inst.shl]
    let state = run_program(
        indoc! {"
            main:
                ld 3, %a
                ld 2, %b
                shl %b, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 12
      %b  = 2
      %pc = 1003
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    ");
}

// =============================================================================
// SHR (arithmetic shift right)
// =============================================================================

#[test]
fn shr_by_one() {
    // r[verify inst.shr]
    let state = run_program(
        indoc! {"
            main:
                ld 16, %a
                shr 1, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 8
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn shr_by_four() {
    // r[verify inst.shr]
    let state = run_program(
        indoc! {"
            main:
                ld 0xFF0, %a
                shr 4, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 255
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn shr_preserves_sign() {
    // r[verify inst.shr]
    let state = run_program(
        indoc! {"
            main:
                ld -8, %a
                shr 1, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = -4
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = NEGATIVE | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn shr_by_zero() {
    // r[verify inst.shr]
    let state = run_program(
        indoc! {"
            main:
                ld 42, %a
                shr 0, %a
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
fn shr_zero_flag() {
    // r[verify inst.shr]
    let state = run_program(
        indoc! {"
            main:
                ld 1, %a
                shr 1, %a
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
fn shr_register_operand() {
    // r[verify inst.shr]
    let state = run_program(
        indoc! {"
            main:
                ld 64, %a
                ld 3, %b
                shr %b, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 8
      %b  = 3
      %pc = 1003
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    ");
}

// =============================================================================
// SHL invalid shift (exception)
// =============================================================================

#[test]
fn shl_negative_shift_raises_exception() {
    // r[verify inst.shl.exception.invalid-shift]
    let state = run_program(
        indoc! {"
            .addr 200
            handler: reset

            .addr 1000
            main:
                ld 1, %a
                shl -1, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 1
      %b  = 0
      %pc = 200
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    Memory:
      [100] = 1002
      [101] = 512
      [102] = 2
    ");
}

#[test]
fn shl_shift_64_raises_exception() {
    // r[verify inst.shl.exception.invalid-shift]
    let state = run_program(
        indoc! {"
            .addr 200
            handler: reset

            .addr 1000
            main:
                ld 1, %a
                shl 64, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 1
      %b  = 0
      %pc = 200
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    Memory:
      [100] = 1002
      [101] = 512
      [102] = 2
    ");
}

#[test]
fn shl_large_shift_raises_exception() {
    // r[verify inst.shl.exception.invalid-shift]
    let state = run_program(
        indoc! {"
            .addr 200
            handler: reset

            .addr 1000
            main:
                ld 1, %a
                shl 100, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 1
      %b  = 0
      %pc = 200
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    Memory:
      [100] = 1002
      [101] = 512
      [102] = 2
    ");
}

// =============================================================================
// SHR invalid shift (exception)
// =============================================================================

#[test]
fn shr_negative_shift_raises_exception() {
    // r[verify inst.shr.exception.invalid-shift]
    let state = run_program(
        indoc! {"
            .addr 200
            handler: reset

            .addr 1000
            main:
                ld 1, %a
                shr -1, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 1
      %b  = 0
      %pc = 200
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    Memory:
      [100] = 1002
      [101] = 512
      [102] = 2
    ");
}

#[test]
fn shr_shift_64_raises_exception() {
    // r[verify inst.shr.exception.invalid-shift]
    let state = run_program(
        indoc! {"
            .addr 200
            handler: reset

            .addr 1000
            main:
                ld 1, %a
                shr 64, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 1
      %b  = 0
      %pc = 200
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    Memory:
      [100] = 1002
      [101] = 512
      [102] = 2
    ");
}

#[test]
fn shr_large_shift_raises_exception() {
    // r[verify inst.shr.exception.invalid-shift]
    let state = run_program(
        indoc! {"
            .addr 200
            handler: reset

            .addr 1000
            main:
                ld 1, %a
                shr 100, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 1
      %b  = 0
      %pc = 200
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    Memory:
      [100] = 1002
      [101] = 512
      [102] = 2
    ");
}
