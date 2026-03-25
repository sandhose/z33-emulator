mod helpers;

use helpers::{run_program, Steps};
use indoc::indoc;

// =============================================================================
// LD — Load register
// =============================================================================

#[test]
fn ld_immediate_to_a() {
    // r[verify inst.ld]
    let state = run_program(
        indoc! {"
            main:
                ld 42, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 42
      %b  = 0
      %pc = 1001
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 1
    Halted: reset
    ");
}

#[test]
fn ld_immediate_to_b() {
    // r[verify inst.ld]
    let state = run_program(
        indoc! {"
            main:
                ld 99, %b
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 0
      %b  = 99
      %pc = 1001
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 1
    Halted: reset
    ");
}

#[test]
fn ld_negative_immediate() {
    // r[verify inst.ld]
    let state = run_program(
        indoc! {"
            main:
                ld -100, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = -100
      %b  = 0
      %pc = 1001
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 1
    Halted: reset
    ");
}

#[test]
fn ld_zero() {
    // r[verify inst.ld]
    let state = run_program(
        indoc! {"
            main:
                ld 55, %a
                ld 0, %a
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
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn ld_register_to_register() {
    // r[verify inst.ld]
    let state = run_program(
        indoc! {"
            main:
                ld 77, %a
                ld %a, %b
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 77
      %b  = 77
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn ld_direct_memory() {
    // r[verify inst.ld]
    let state = run_program(
        indoc! {"
            .addr 500
            mydata: .word 12345

            .addr 1000
            main:
                ld [500], %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 12345
      %b  = 0
      %pc = 1001
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    Memory:
      [500] = 12345
    ");
}

#[test]
fn ld_indirect_memory() {
    // r[verify inst.ld]
    let state = run_program(
        indoc! {"
            .addr 500
            mydata: .word 9876

            .addr 1000
            main:
                ld 500, %b
                ld [%b], %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 9876
      %b  = 500
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    Memory:
      [500] = 9876
    ");
}

#[test]
fn ld_indexed_memory() {
    // r[verify inst.ld]
    let state = run_program(
        indoc! {"
            .addr 500
            first: .word 10
            second: .word 20
            third: .word 30

            .addr 1000
            main:
                ld 500, %b
                ld [%b+2], %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 30
      %b  = 500
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    Memory:
      [500] = 10
      [501] = 20
      [502] = 30
    ");
}

#[test]
fn ld_indexed_negative_offset() {
    // r[verify inst.ld]
    let state = run_program(
        indoc! {"
            .addr 500
            first: .word 111
            second: .word 222
            third: .word 333

            .addr 1000
            main:
                ld 502, %b
                ld [%b-2], %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 111
      %b  = 502
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    Memory:
      [500] = 111
      [501] = 222
      [502] = 333
    ");
}

// =============================================================================
// ST — Store to memory
// =============================================================================

#[test]
fn st_direct() {
    // r[verify inst.st]
    let state = run_program(
        indoc! {"
            main:
                ld 42, %a
                st %a, [500]
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
    Cycles: 3
    Halted: reset
    Memory:
      [500] = 42
    ");
}

#[test]
fn st_indirect() {
    // r[verify inst.st]
    let state = run_program(
        indoc! {"
            main:
                ld 600, %b
                ld 99, %a
                st %a, [%b]
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 99
      %b  = 600
      %pc = 1003
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 4
    Halted: reset
    Memory:
      [600] = 99
    ");
}

#[test]
fn st_indexed() {
    // r[verify inst.st]
    let state = run_program(
        indoc! {"
            main:
                ld 500, %b
                ld 77, %a
                st %a, [%b+3]
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 77
      %b  = 500
      %pc = 1003
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 4
    Halted: reset
    Memory:
      [503] = 77
    ");
}

#[test]
fn st_indexed_negative_offset() {
    // r[verify inst.st]
    let state = run_program(
        indoc! {"
            main:
                ld 505, %b
                ld 88, %a
                st %a, [%b-5]
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 88
      %b  = 505
      %pc = 1003
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 4
    Halted: reset
    Memory:
      [500] = 88
    ");
}

#[test]
fn st_both_registers() {
    // r[verify inst.st]
    let state = run_program(
        indoc! {"
            main:
                ld 11, %a
                ld 22, %b
                st %a, [500]
                st %b, [501]
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 11
      %b  = 22
      %pc = 1004
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 6
    Halted: reset
    Memory:
      [500] = 11
      [501] = 22
    ");
}

// =============================================================================
// SWAP — Swap values
// =============================================================================

#[test]
fn swap_registers() {
    // r[verify inst.swap]
    let state = run_program(
        indoc! {"
            main:
                ld 10, %a
                ld 20, %b
                swap %b, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 20
      %b  = 10
      %pc = 1003
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    ");
}

#[test]
fn swap_direct_memory() {
    // r[verify inst.swap]
    let state = run_program(
        indoc! {"
            .addr 500
            mydata: .word 100

            .addr 1000
            main:
                ld 200, %a
                swap [500], %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 100
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    Memory:
      [500] = 200
    ");
}

#[test]
fn swap_indirect_memory() {
    // r[verify inst.swap]
    let state = run_program(
        indoc! {"
            .addr 500
            mydata: .word 55

            .addr 1000
            main:
                ld 500, %b
                ld 66, %a
                swap [%b], %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 55
      %b  = 500
      %pc = 1003
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 4
    Halted: reset
    Memory:
      [500] = 66
    ");
}

#[test]
fn swap_indexed_memory() {
    // r[verify inst.swap]
    let state = run_program(
        indoc! {"
            .addr 500
            first: .word 1
            second: .word 2
            third: .word 3

            .addr 1000
            main:
                ld 500, %b
                ld 99, %a
                swap [%b+1], %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 2
      %b  = 500
      %pc = 1003
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 4
    Halted: reset
    Memory:
      [500] = 1
      [501] = 99
      [502] = 3
    ");
}

// =============================================================================
// PUSH — Push to stack
// =============================================================================

#[test]
fn push_immediate() {
    // r[verify inst.push]
    let state = run_program(
        indoc! {"
            main:
                push 42
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 0
      %b  = 0
      %pc = 1001
      %sp = 9999
      %sr = SUPERVISOR
    Cycles: 1
    Halted: reset
    Memory:
      [9999] = 42
    ");
}

#[test]
fn push_register() {
    // r[verify inst.push]
    let state = run_program(
        indoc! {"
            main:
                ld 123, %a
                push %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 123
      %b  = 0
      %pc = 1002
      %sp = 9999
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    Memory:
      [9999] = 123
    ");
}

#[test]
fn push_multiple_values() {
    // r[verify inst.push]
    let state = run_program(
        indoc! {"
            main:
                push 10
                push 20
                push 30
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 0
      %b  = 0
      %pc = 1003
      %sp = 9997
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    Memory:
      [9997] = 30
      [9998] = 20
      [9999] = 10
    ");
}

#[test]
fn push_decrements_sp() {
    // r[verify inst.push]
    let state = run_program(
        indoc! {"
            main:
                push 1
                push 2
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
      %sp = 9998
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    Memory:
      [9998] = 2
      [9999] = 1
    ");
}

// =============================================================================
// POP — Pop from stack
// =============================================================================

#[test]
fn pop_single_value() {
    // r[verify inst.pop]
    let state = run_program(
        indoc! {"
            main:
                push 42
                pop %a
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
    Memory:
      [9999] = 42
    ");
}

#[test]
fn pop_restores_sp() {
    // r[verify inst.pop]
    let state = run_program(
        indoc! {"
            main:
                push 10
                push 20
                pop %a
                pop %b
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 20
      %b  = 10
      %pc = 1004
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 4
    Halted: reset
    Memory:
      [9998] = 20
      [9999] = 10
    ");
}

#[test]
fn pop_lifo_order() {
    // r[verify inst.pop]
    let state = run_program(
        indoc! {"
            main:
                push 111
                push 222
                pop %a
                pop %b
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 222
      %b  = 111
      %pc = 1004
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 4
    Halted: reset
    Memory:
      [9998] = 222
      [9999] = 111
    ");
}

#[test]
fn push_pop_round_trip() {
    // r[verify inst.push]
    // r[verify inst.pop]
    let state = run_program(
        indoc! {"
            main:
                ld 999, %a
                push %a
                ld 0, %a
                pop %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 999
      %b  = 0
      %pc = 1004
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 4
    Halted: reset
    Memory:
      [9999] = 999
    ");
}

// =============================================================================
// FAS — Fetch and set
// =============================================================================

#[test]
fn fas_direct_zero() {
    // r[verify inst.fas]
    let state = run_program(
        indoc! {"
            .addr 500
            lock: .word 0

            .addr 1000
            main:
                ld 99, %a
                fas [500], %a
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
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    Memory:
      [500] = 1
    ");
}

#[test]
fn fas_direct_nonzero() {
    // r[verify inst.fas]
    let state = run_program(
        indoc! {"
            .addr 500
            lock: .word 77

            .addr 1000
            main:
                ld 99, %a
                fas [500], %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 77
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    Memory:
      [500] = 1
    ");
}

#[test]
fn fas_indirect() {
    // r[verify inst.fas]
    let state = run_program(
        indoc! {"
            .addr 500
            lock: .word 0

            .addr 1000
            main:
                ld 500, %b
                ld 50, %a
                fas [%b], %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 0
      %b  = 500
      %pc = 1003
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 4
    Halted: reset
    Memory:
      [500] = 1
    ");
}

#[test]
fn fas_indexed() {
    // r[verify inst.fas]
    let state = run_program(
        indoc! {"
            .addr 500
            first: .word 0
            second: .word 0
            third: .word 42

            .addr 1000
            main:
                ld 500, %b
                ld 0, %a
                fas [%b+2], %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 42
      %b  = 500
      %pc = 1003
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 4
    Halted: reset
    Memory:
      [500] = 0
      [501] = 0
      [502] = 1
    ");
}

#[test]
fn fas_sets_memory_to_one() {
    // r[verify inst.fas]
    let state = run_program(
        indoc! {"
            .addr 500
            lock: .word 0

            .addr 1000
            main:
                ld 0, %a
                fas [500], %a
                // %a should now be 0 (old value), mem[500] should be 1
                ld 0, %b
                fas [500], %b
                // %b should now be 1 (old value), mem[500] still 1
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 0
      %b  = 1
      %pc = 1004
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 6
    Halted: reset
    Memory:
      [500] = 1
    ");
}

// =============================================================================
// Combined data movement patterns
// =============================================================================

#[test]
fn ld_st_round_trip() {
    // r[verify inst.ld]
    // r[verify inst.st]
    let state = run_program(
        indoc! {"
            main:
                ld 42, %a
                st %a, [500]
                ld 0, %a
                ld [500], %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 42
      %b  = 0
      %pc = 1004
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 6
    Halted: reset
    Memory:
      [500] = 42
    ");
}

#[test]
fn st_then_ld_indirect() {
    // r[verify inst.ld]
    // r[verify inst.st]
    let state = run_program(
        indoc! {"
            main:
                ld 500, %b
                ld 77, %a
                st %a, [%b]
                ld 0, %a
                ld [%b], %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 77
      %b  = 500
      %pc = 1005
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 7
    Halted: reset
    Memory:
      [500] = 77
    ");
}

#[test]
fn push_pop_multiple_registers() {
    // r[verify inst.push]
    // r[verify inst.pop]
    let state = run_program(
        indoc! {"
            main:
                ld 100, %a
                ld 200, %b
                push %a
                push %b
                ld 0, %a
                ld 0, %b
                pop %b
                pop %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 100
      %b  = 200
      %pc = 1008
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 8
    Halted: reset
    Memory:
      [9998] = 200
      [9999] = 100
    ");
}
