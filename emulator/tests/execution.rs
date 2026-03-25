mod helpers;

use helpers::{run_program, Steps};
use indoc::indoc;

// =============================================================================
// FETCH
// =============================================================================

#[test]
fn fetch_from_pc() {
    // r[verify exec.fetch]
    // Instruction is fetched from the address in %pc (the entrypoint).
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

// =============================================================================
// PC INCREMENT
// =============================================================================

#[test]
fn pc_incremented_after_fetch() {
    // r[verify exec.pc-increment]
    // After fetching at 1000, %pc should be 1001 before the instruction executes.
    // We run 1 step (ld 42, %a) and check that %pc has advanced past the
    // instruction.
    let state = run_program(
        indoc! {"
            main:
                ld 42, %a
                reset
        "},
        "main",
        Steps::Count(1),
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 42
      %b  = 0
      %pc = 1001
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 1
    ");
}

// =============================================================================
// DECODE — non-instruction cell raises exception
// =============================================================================

#[test]
fn decode_word_as_instruction_raises_exception() {
    // r[verify exec.decode]
    // Place a .word where %pc will try to fetch — should raise invalid instruction
    // exception (code 2). The exception handler at 200 does a reset.
    let state = run_program(
        indoc! {"
            .addr 200
            handler: reset

            .addr 1000
            main:
                jmp target
            target:
                .word 12345
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 0
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
      [1001] = 12345
    ");
}

// =============================================================================
// CYCLES
// =============================================================================

#[test]
fn cycles_immediate_operands() {
    // r[verify exec.cycles]
    // ld 1, %a costs 1 cycle (base=1, immediate=0, register=0)
    // add 2, %a costs 1 cycle
    // Total: 2 cycles after 2 steps
    let state = run_program(
        indoc! {"
            main:
                ld 1, %a
                add 2, %a
                reset
        "},
        "main",
        Steps::Count(2),
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 3
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    ");
}

#[test]
fn cycles_direct_memory_operand() {
    // r[verify exec.cycles]
    // ld [500], %a costs 2 cycles (base=1, direct memory=1)
    let state = run_program(
        indoc! {"
            .addr 500
            data: .word 99

            .addr 1000
            main:
                ld [500], %a
                reset
        "},
        "main",
        Steps::Count(1),
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 99
      %b  = 0
      %pc = 1001
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Memory:
      [500] = 99
    ");
}

#[test]
fn cycles_register_operand() {
    // r[verify exec.cycles]
    // add %b, %a costs 1 cycle (base=1, register=0)
    let state = run_program(
        indoc! {"
            main:
                add %b, %a
                reset
        "},
        "main",
        Steps::Count(1),
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 0
      %b  = 0
      %pc = 1001
      %sp = 10000
      %sr = ZERO | SUPERVISOR
    Cycles: 1
    ");
}

#[test]
fn cycles_exception_recovery() {
    // r[verify exec.cycles.exception]
    // Division by zero triggers an exception. The exception recovery costs 1 cycle
    // (replacing the instruction's normal cost).
    // ld 42, %a = 1 cycle, div 0, %a triggers exception = 1 cycle (recovery).
    // Reset doesn't count (returns before adding cost). Total = 2 cycles.
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
fn cycles_debugreg_costs_zero() {
    // r[verify exec.cycles.debugreg]
    // debugreg costs 0 cycles. So ld + debugreg = 1 cycle after 2 steps.
    let state = run_program(
        indoc! {"
            main:
                ld 1, %a
                debugreg
                reset
        "},
        "main",
        Steps::Count(2),
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 1
      %b  = 0
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 1
    ");
}

// =============================================================================
// PRIVILEGE MODE
// =============================================================================

#[test]
fn starts_in_supervisor_mode() {
    // r[verify exec.privilege.supervisor]
    // At startup, the S bit should be set in %sr.
    let state = run_program(
        indoc! {"
            main:
                reset
        "},
        "main",
        Steps::Count(0),
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 0
      %b  = 0
      %pc = 1000
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 0
    ");
}

#[test]
fn rti_enters_user_mode() {
    // r[verify exec.privilege.user]
    // r[verify exec.privilege.transition-down]
    // Use rti to return to user_code with SR=0 (user mode).
    // user_code does a reset which is not privileged, so we can observe SR.
    let state = run_program(
        indoc! {"
            .addr 100
            .word user_code
            .word 0

            .addr 200
            handler: reset

            .addr 1000
            main:
                rti
            user_code:
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
      %sp = 10000
      %sr = (none)
    Cycles: 1
    Halted: reset
    Memory:
      [100] = 1001
      [101] = 0
    ");
}

#[test]
fn exception_enters_supervisor_mode() {
    // r[verify exec.privilege.transition-up]
    // Enter user mode via rti, then trigger an exception (e.g., division by zero).
    // The exception handler should run in supervisor mode.
    let state = run_program(
        indoc! {"
            .addr 100
            .word user_code
            .word 0

            .addr 200
            handler: reset

            .addr 1000
            main:
                rti
            user_code:
                div 0, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 0
      %b  = 0
      %pc = 200
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    Memory:
      [100] = 1002
      [101] = 0
      [102] = 1
    ");
}

#[test]
fn privileged_instruction_in_user_mode_raises_exception() {
    // r[verify exec.privilege.user]
    // In user mode, writing %sr is privileged and should raise an exception.
    let state = run_program(
        indoc! {"
            .addr 100
            .word user_code
            .word 0

            .addr 200
            handler: reset

            .addr 1000
            main:
                rti
            user_code:
                ld 0, %sr
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 0
      %b  = 0
      %pc = 200
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    Memory:
      [100] = 1002
      [101] = 0
      [102] = 3
    ");
}
