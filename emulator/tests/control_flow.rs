mod helpers;

use helpers::{run_program, Steps};
use indoc::indoc;

// =============================================================================
// CALL
// =============================================================================

#[test]
fn call_basic() {
    // r[verify inst.call]
    // Call pushes return address onto stack and jumps to target
    let state = run_program(
        indoc! {"
            main:
                call sub
                reset
            sub:
                ld 42, %a
                rtn
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
    Cycles: 3
    Halted: reset
    Memory:
      [9999] = 1001
    ");
}

#[test]
fn call_pushes_return_address() {
    // r[verify inst.call]
    // After call, SP should be decremented and stack should contain return address
    let state = run_program(
        indoc! {"
            main:
                call sub
                reset
            sub:
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
      %sp = 9999
      %sr = SUPERVISOR
    Cycles: 1
    Halted: reset
    Memory:
      [9999] = 1001
    ");
}

#[test]
fn call_nested() {
    // r[verify inst.call]
    // Nested calls: main -> outer -> inner, each pushes a return address
    let state = run_program(
        indoc! {"
            main:
                ld 1, %a
                call outer
                reset
            outer:
                ld 2, %b
                call inner
                rtn
            inner:
                add %b, %a
                rtn
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 3
      %b  = 2
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 7
    Halted: reset
    Memory:
      [9998] = 1005
      [9999] = 1002
    ");
}

#[test]
fn call_preserves_registers() {
    // r[verify inst.call]
    // Registers set before call should still be accessible in subroutine
    let state = run_program(
        indoc! {"
            main:
                ld 100, %a
                ld 200, %b
                call sub
                reset
            sub:
                add 1, %a
                rtn
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 101
      %b  = 200
      %pc = 1003
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 5
    Halted: reset
    Memory:
      [9999] = 1003
    ");
}

// =============================================================================
// RTN
// =============================================================================

#[test]
fn rtn_returns_to_caller() {
    // r[verify inst.rtn]
    // rtn pops address from stack and jumps to it
    let state = run_program(
        indoc! {"
            main:
                call sub
                ld 99, %b
                reset
            sub:
                ld 42, %a
                rtn
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 42
      %b  = 99
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 4
    Halted: reset
    Memory:
      [9999] = 1001
    ");
}

#[test]
fn rtn_restores_sp() {
    // r[verify inst.rtn]
    // After call+rtn, SP should be restored to its original value
    let state = run_program(
        indoc! {"
            main:
                call sub
                reset
            sub:
                rtn
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
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    Memory:
      [9999] = 1001
    ");
}

#[test]
fn rtn_nested_returns() {
    // r[verify inst.rtn]
    // Multiple nested call/rtn pairs unwind correctly
    let state = run_program(
        indoc! {"
            main:
                call first
                ld 30, %a
                reset
            first:
                call second
                ld 20, %b
                rtn
            second:
                ld 10, %a
                rtn
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 30
      %b  = 20
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 7
    Halted: reset
    Memory:
      [9998] = 1004
      [9999] = 1001
    ");
}

// =============================================================================
// TRAP
// =============================================================================

#[test]
fn trap_jumps_to_handler() {
    // r[verify inst.trap]
    // trap raises exception and jumps to handler at address 200
    let state = run_program(
        indoc! {"
            .addr 200
            handler:
                reset

            .addr 1000
            main:
                trap
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
    Cycles: 1
    Halted: reset
    Memory:
      [100] = 1001
      [101] = 512
      [102] = 4
    ");
}

#[test]
fn trap_saves_pc_and_sr() {
    // r[verify inst.trap]
    // trap saves PC to mem[100], SR to mem[101], exception code to mem[102]
    let state = run_program(
        indoc! {"
            .addr 200
            handler:
                reset

            .addr 1000
            main:
                ld 42, %a
                trap
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
      [102] = 4
    ");
}

#[test]
fn trap_sets_supervisor_mode() {
    // r[verify inst.trap]
    // After trap, SR should have SUPERVISOR set
    let state = run_program(
        indoc! {"
            .addr 200
            handler:
                reset

            .addr 1000
            main:
                trap
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
    Cycles: 1
    Halted: reset
    Memory:
      [100] = 1001
      [101] = 512
      [102] = 4
    ");
}

#[test]
fn trap_exception_code() {
    // r[verify inst.trap]
    // trap stores exception code 4 in mem[102]
    let state = run_program(
        indoc! {"
            .addr 200
            handler:
                ld [102], %b
                reset

            .addr 1000
            main:
                trap
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 0
      %b  = 4
      %pc = 201
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    Memory:
      [100] = 1001
      [101] = 512
      [102] = 4
    ");
}

// =============================================================================
// RTI
// =============================================================================

#[test]
fn rti_returns_from_exception() {
    // r[verify inst.rti]
    // rti restores PC from mem[100] and SR from mem[101]
    // Use trap to enter handler, then rti to return
    let state = run_program(
        indoc! {"
            .addr 200
            handler:
                rti

            .addr 1000
            main:
                trap
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
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    Memory:
      [100] = 1001
      [101] = 512
      [102] = 4
    ");
}

#[test]
fn rti_restores_sr() {
    // r[verify inst.rti]
    // rti should restore the saved SR, clearing SUPERVISOR if it was not set before
    let state = run_program(
        indoc! {"
            .addr 200
            handler:
                rti

            .addr 1000
            main:
                ld 0, %a
                trap
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
      [100] = 1002
      [101] = 512
      [102] = 4
    ");
}

#[test]
fn rti_after_trap_continues_execution() {
    // r[verify inst.rti]
    // After trap + rti, execution continues at the instruction after trap
    let state = run_program(
        indoc! {"
            .addr 200
            handler:
                rti

            .addr 1000
            main:
                ld 10, %a
                trap
                ld 20, %b
                add %b, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 30
      %b  = 20
      %pc = 1004
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 5
    Halted: reset
    Memory:
      [100] = 1002
      [101] = 512
      [102] = 4
    ");
}

// =============================================================================
// RESET
// =============================================================================

#[test]
fn reset_halts_computer() {
    // r[verify inst.reset]
    // reset stops execution
    let state = run_program(
        indoc! {"
            main:
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 0
      %b  = 0
      %pc = 1000
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 0
    Halted: reset
    ");
}

#[test]
fn reset_pc_points_at_reset_instruction() {
    // r[verify inst.reset]
    // After reset, PC should point at the reset instruction itself (decremented by
    // 1)
    let state = run_program(
        indoc! {"
            main:
                ld 42, %a
                ld 99, %b
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 42
      %b  = 99
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn reset_preserves_register_state() {
    // r[verify inst.reset]
    // Registers set before reset should be preserved in final state
    let state = run_program(
        indoc! {"
            main:
                ld 123, %a
                ld 456, %b
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 123
      %b  = 456
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

// =============================================================================
// NOP
// =============================================================================

#[test]
fn nop_does_nothing() {
    // r[verify inst.nop]
    // nop should not change any registers or memory
    let state = run_program(
        indoc! {"
            main:
                ld 42, %a
                nop
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
fn nop_costs_one_cycle() {
    // r[verify inst.nop]
    // Compare cycle count with and without nop to verify it costs exactly 1 cycle
    let without_nop = run_program(
        indoc! {"
            main:
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    let with_nop = run_program(
        indoc! {"
            main:
                nop
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(without_nop, @r"
    Registers:
      %a  = 0
      %b  = 0
      %pc = 1000
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 0
    Halted: reset
    ");
    insta::assert_snapshot!(with_nop, @r"
    Registers:
      %a  = 0
      %b  = 0
      %pc = 1001
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 1
    Halted: reset
    ");
}

#[test]
fn nop_multiple() {
    // r[verify inst.nop]
    // Multiple nops should each cost 1 cycle
    let state = run_program(
        indoc! {"
            main:
                nop
                nop
                nop
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
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    ");
}

// =============================================================================
// CALL + RTN STACK BEHAVIOR
// =============================================================================

#[test]
fn call_rtn_stack_grows_downward() {
    // r[verify inst.call]
    // r[verify inst.rtn]
    // Stack starts at 10000 and grows downward; after call, SP should be 9999
    let state = run_program(
        indoc! {"
            main:
                call sub
                reset
            sub:
                reset
        "},
        "main",
        Steps::Count(2),
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 0
      %b  = 0
      %pc = 1002
      %sp = 9999
      %sr = SUPERVISOR
    Cycles: 1
    Halted: reset
    Memory:
      [9999] = 1001
    ");
}

#[test]
fn call_rtn_multiple_calls_sequential() {
    // r[verify inst.call]
    // r[verify inst.rtn]
    // Sequential calls should each properly push and pop
    let state = run_program(
        indoc! {"
            main:
                call first
                call second
                reset
            first:
                ld 10, %a
                rtn
            second:
                ld 20, %b
                rtn
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 10
      %b  = 20
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 6
    Halted: reset
    Memory:
      [9999] = 1002
    ");
}

// =============================================================================
// TRAP + RTI ROUND-TRIP
// =============================================================================

#[test]
fn trap_rti_round_trip_preserves_state() {
    // r[verify inst.trap]
    // r[verify inst.rti]
    // Full round trip: set registers, trap, handler uses rti, execution resumes
    let state = run_program(
        indoc! {"
            .addr 200
            handler:
                rti

            .addr 1000
            main:
                ld 100, %a
                ld 200, %b
                trap
                add 1, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 101
      %b  = 200
      %pc = 1004
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 5
    Halted: reset
    Memory:
      [100] = 1003
      [101] = 512
      [102] = 4
    ");
}

#[test]
fn trap_handler_can_modify_registers() {
    // r[verify inst.trap]
    // r[verify inst.rti]
    // Exception handler can modify registers before returning
    let state = run_program(
        indoc! {"
            .addr 200
            handler:
                ld 999, %b
                rti

            .addr 1000
            main:
                ld 42, %a
                trap
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 42
      %b  = 999
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 4
    Halted: reset
    Memory:
      [100] = 1002
      [101] = 512
      [102] = 4
    ");
}
