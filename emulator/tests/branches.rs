mod helpers;

use helpers::{run_program, Steps};
use indoc::indoc;

// =============================================================================
// cmp instruction
// =============================================================================

#[test]
fn cmp_equal_values() {
    // r[verify inst.cmp]
    let state = run_program(
        indoc! {"
            main:
                ld 42, %a
                cmp 42, %a
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
      %sr = ZERO | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn cmp_first_less_than_second() {
    // r[verify inst.cmp]
    let state = run_program(
        indoc! {"
            main:
                ld 10, %a
                cmp 5, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 10
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = NEGATIVE | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn cmp_first_greater_than_second() {
    // r[verify inst.cmp]
    let state = run_program(
        indoc! {"
            main:
                ld 5, %a
                cmp 10, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 5
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn cmp_does_not_modify_registers() {
    // r[verify inst.cmp]
    let state = run_program(
        indoc! {"
            main:
                ld 7, %a
                ld 3, %b
                cmp %b, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 7
      %b  = 3
      %pc = 1003
      %sp = 10000
      %sr = NEGATIVE | SUPERVISOR
    Cycles: 3
    Halted: reset
    ");
}

#[test]
fn cmp_negative_values() {
    // r[verify inst.cmp]
    let state = run_program(
        indoc! {"
            main:
                ld -5, %a
                cmp -5, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = -5
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = ZERO | SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

// =============================================================================
// jmp — unconditional jump
// =============================================================================

#[test]
fn jmp_unconditional() {
    // r[verify inst.jmp]
    let state = run_program(
        indoc! {"
            main:
                ld 42, %a
                jmp skip
                ld 99, %a
            skip:
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
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn jmp_backward() {
    // r[verify inst.jmp]
    let state = run_program(
        indoc! {"
            main:
                ld 0, %a
            loop:
                add 1, %a
                cmp 3, %a
                jeq done
                jmp loop
            done:
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 3
      %b  = 0
      %pc = 1005
      %sp = 10000
      %sr = ZERO | SUPERVISOR
    Cycles: 12
    Halted: reset
    ");
}

// =============================================================================
// jeq — jump if equal (Z set)
// =============================================================================

#[test]
fn jeq_taken_when_equal() {
    // r[verify inst.jeq]
    let state = run_program(
        indoc! {"
            main:
                ld 5, %a
                cmp 5, %a
                jeq equal
                ld 99, %a
                jmp done
            equal:
                ld 1, %b
            done:
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 5
      %b  = 1
      %pc = 1006
      %sp = 10000
      %sr = ZERO | SUPERVISOR
    Cycles: 4
    Halted: reset
    ");
}

#[test]
fn jeq_not_taken_when_not_equal() {
    // r[verify inst.jeq]
    let state = run_program(
        indoc! {"
            main:
                ld 5, %a
                cmp 10, %a
                jeq skip
                ld 1, %b
                jmp done
            skip:
                ld 99, %b
            done:
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 5
      %b  = 1
      %pc = 1006
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 5
    Halted: reset
    ");
}

// =============================================================================
// jne — jump if not equal (Z not set)
// =============================================================================

#[test]
fn jne_taken_when_not_equal() {
    // r[verify inst.jne]
    let state = run_program(
        indoc! {"
            main:
                ld 5, %a
                cmp 10, %a
                jne not_equal
                ld 99, %a
                jmp done
            not_equal:
                ld 1, %b
            done:
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 5
      %b  = 1
      %pc = 1006
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 4
    Halted: reset
    ");
}

#[test]
fn jne_not_taken_when_equal() {
    // r[verify inst.jne]
    let state = run_program(
        indoc! {"
            main:
                ld 5, %a
                cmp 5, %a
                jne skip
                ld 1, %b
                jmp done
            skip:
                ld 99, %b
            done:
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 5
      %b  = 1
      %pc = 1006
      %sp = 10000
      %sr = ZERO | SUPERVISOR
    Cycles: 5
    Halted: reset
    ");
}

// =============================================================================
// jlt — jump if strictly less than (N set, Z not set)
// =============================================================================

#[test]
fn jlt_taken_when_less() {
    // r[verify inst.jlt]
    // cmp 3, %a with %a=5: 3 < 5, so N is set
    let state = run_program(
        indoc! {"
            main:
                ld 5, %a
                cmp 3, %a
                jlt is_less
                ld 99, %b
                jmp done
            is_less:
                ld 1, %b
            done:
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 5
      %b  = 1
      %pc = 1006
      %sp = 10000
      %sr = NEGATIVE | SUPERVISOR
    Cycles: 4
    Halted: reset
    ");
}

#[test]
fn jlt_not_taken_when_equal() {
    // r[verify inst.jlt]
    let state = run_program(
        indoc! {"
            main:
                ld 5, %a
                cmp 5, %a
                jlt skip
                ld 1, %b
                jmp done
            skip:
                ld 99, %b
            done:
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 5
      %b  = 1
      %pc = 1006
      %sp = 10000
      %sr = ZERO | SUPERVISOR
    Cycles: 5
    Halted: reset
    ");
}

#[test]
fn jlt_not_taken_when_greater() {
    // r[verify inst.jlt]
    // cmp 10, %a with %a=5: 10 > 5, so N is not set
    let state = run_program(
        indoc! {"
            main:
                ld 5, %a
                cmp 10, %a
                jlt skip
                ld 1, %b
                jmp done
            skip:
                ld 99, %b
            done:
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 5
      %b  = 1
      %pc = 1006
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 5
    Halted: reset
    ");
}

// =============================================================================
// jle — jump if less or equal (Z set OR N set)
// =============================================================================

#[test]
fn jle_taken_when_less() {
    // r[verify inst.jle]
    let state = run_program(
        indoc! {"
            main:
                ld 5, %a
                cmp 3, %a
                jle is_le
                ld 99, %b
                jmp done
            is_le:
                ld 1, %b
            done:
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 5
      %b  = 1
      %pc = 1006
      %sp = 10000
      %sr = NEGATIVE | SUPERVISOR
    Cycles: 4
    Halted: reset
    ");
}

#[test]
fn jle_taken_when_equal() {
    // r[verify inst.jle]
    let state = run_program(
        indoc! {"
            main:
                ld 5, %a
                cmp 5, %a
                jle is_le
                ld 99, %b
                jmp done
            is_le:
                ld 1, %b
            done:
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 5
      %b  = 1
      %pc = 1006
      %sp = 10000
      %sr = ZERO | SUPERVISOR
    Cycles: 4
    Halted: reset
    ");
}

#[test]
fn jle_not_taken_when_greater() {
    // r[verify inst.jle]
    let state = run_program(
        indoc! {"
            main:
                ld 5, %a
                cmp 10, %a
                jle skip
                ld 1, %b
                jmp done
            skip:
                ld 99, %b
            done:
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 5
      %b  = 1
      %pc = 1006
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 5
    Halted: reset
    ");
}

// =============================================================================
// jgt — jump if strictly greater (Z not set AND N not set)
// =============================================================================

#[test]
fn jgt_taken_when_greater() {
    // r[verify inst.jgt]
    // cmp 10, %a with %a=5: 10 > 5
    let state = run_program(
        indoc! {"
            main:
                ld 5, %a
                cmp 10, %a
                jgt is_gt
                ld 99, %b
                jmp done
            is_gt:
                ld 1, %b
            done:
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 5
      %b  = 1
      %pc = 1006
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 4
    Halted: reset
    ");
}

#[test]
fn jgt_not_taken_when_equal() {
    // r[verify inst.jgt]
    let state = run_program(
        indoc! {"
            main:
                ld 5, %a
                cmp 5, %a
                jgt skip
                ld 1, %b
                jmp done
            skip:
                ld 99, %b
            done:
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 5
      %b  = 1
      %pc = 1006
      %sp = 10000
      %sr = ZERO | SUPERVISOR
    Cycles: 5
    Halted: reset
    ");
}

#[test]
fn jgt_not_taken_when_less() {
    // r[verify inst.jgt]
    let state = run_program(
        indoc! {"
            main:
                ld 5, %a
                cmp 3, %a
                jgt skip
                ld 1, %b
                jmp done
            skip:
                ld 99, %b
            done:
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 5
      %b  = 1
      %pc = 1006
      %sp = 10000
      %sr = NEGATIVE | SUPERVISOR
    Cycles: 5
    Halted: reset
    ");
}

// =============================================================================
// jge — jump if greater or equal (Z set OR N not set)
// =============================================================================

#[test]
fn jge_taken_when_greater() {
    // r[verify inst.jge]
    let state = run_program(
        indoc! {"
            main:
                ld 5, %a
                cmp 10, %a
                jge is_ge
                ld 99, %b
                jmp done
            is_ge:
                ld 1, %b
            done:
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 5
      %b  = 1
      %pc = 1006
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 4
    Halted: reset
    ");
}

#[test]
fn jge_taken_when_equal() {
    // r[verify inst.jge]
    let state = run_program(
        indoc! {"
            main:
                ld 5, %a
                cmp 5, %a
                jge is_ge
                ld 99, %b
                jmp done
            is_ge:
                ld 1, %b
            done:
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 5
      %b  = 1
      %pc = 1006
      %sp = 10000
      %sr = ZERO | SUPERVISOR
    Cycles: 4
    Halted: reset
    ");
}

#[test]
fn jge_not_taken_when_less() {
    // r[verify inst.jge]
    let state = run_program(
        indoc! {"
            main:
                ld 5, %a
                cmp 3, %a
                jge skip
                ld 1, %b
                jmp done
            skip:
                ld 99, %b
            done:
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 5
      %b  = 1
      %pc = 1006
      %sp = 10000
      %sr = NEGATIVE | SUPERVISOR
    Cycles: 5
    Halted: reset
    ");
}

// =============================================================================
// Combined / integration scenarios
// =============================================================================

#[test]
fn branch_chain_finds_maximum() {
    // r[verify inst.cmp]
    // r[verify inst.jgt]
    // Finds the max of three values using comparisons and branches
    let state = run_program(
        indoc! {"
            main:
                ld 10, %a
                // compare with 25
                cmp 25, %a
                jgt keep_a_1
                ld 25, %a
            keep_a_1:
                // compare with 15
                cmp 15, %a
                jgt keep_a_2
                ld 15, %a
            keep_a_2:
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 10
      %b  = 0
      %pc = 1007
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 5
    Halted: reset
    ");
}

#[test]
fn countdown_loop_with_jne() {
    // r[verify inst.jne]
    // r[verify inst.cmp]
    let state = run_program(
        indoc! {"
            main:
                ld 5, %a
            loop:
                sub 1, %a
                cmp 0, %a
                jne loop
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 0
      %b  = 0
      %pc = 1004
      %sp = 10000
      %sr = ZERO | SUPERVISOR
    Cycles: 16
    Halted: reset
    ");
}

#[test]
fn cmp_with_register_operand() {
    // r[verify inst.cmp]
    // r[verify inst.jlt]
    let state = run_program(
        indoc! {"
            main:
                ld 3, %a
                ld 7, %b
                cmp %a, %b
                jlt a_less
                ld 0, %a
                jmp done
            a_less:
                ld 1, %a
            done:
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 1
      %b  = 7
      %pc = 1007
      %sp = 10000
      %sr = NEGATIVE | SUPERVISOR
    Cycles: 5
    Halted: reset
    ");
}

#[test]
fn all_branches_in_sequence() {
    // r[verify inst.jeq]
    // r[verify inst.jne]
    // r[verify inst.jlt]
    // r[verify inst.jle]
    // r[verify inst.jgt]
    // r[verify inst.jge]
    // When values are equal: jeq taken, jne not taken, jlt not taken,
    // jle taken, jgt not taken, jge taken
    let state = run_program(
        indoc! {"
            main:
                ld 5, %a
                cmp 5, %a
                // Z=1, N=0 — equal
                jeq step1
                jmp fail
            step1:
                jne fail
                jlt fail
                jle step2
                jmp fail
            step2:
                jgt fail
                jge step3
                jmp fail
            step3:
                ld 1, %b
                reset
            fail:
                ld 99, %b
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 5
      %b  = 1
      %pc = 1012
      %sp = 10000
      %sr = ZERO | SUPERVISOR
    Cycles: 9
    Halted: reset
    ");
}
