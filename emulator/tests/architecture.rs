mod helpers;

use helpers::{run_program, Steps};
use indoc::indoc;

// =============================================================================
// INITIAL STATE
// =============================================================================

#[test]
fn initial_state() {
    // r[verify arch.initial-state]
    // %a=0, %b=0, %pc=entrypoint, %sp=10000, %sr=SUPERVISOR
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

// =============================================================================
// WORD SIZE
// =============================================================================

#[test]
fn word_large_positive() {
    // r[verify arch.word]
    // Word is i64 — test near i64::MAX
    let state = run_program(
        indoc! {"
            main:
                ld 9223372036854775807, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 9223372036854775807
      %b  = 0
      %pc = 1001
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 1
    Halted: reset
    ");
}

#[test]
fn word_large_negative() {
    // r[verify arch.word]
    // Word is i64 — test near i64::MIN
    let state = run_program(
        indoc! {"
            main:
                ld -9223372036854775808, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = -9223372036854775808
      %b  = 0
      %pc = 1001
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 1
    Halted: reset
    ");
}

// =============================================================================
// MEMORY
// =============================================================================

#[test]
fn memory_size_valid_access() {
    // r[verify arch.memory.size]
    // Address 9999 is the last valid cell.
    let state = run_program(
        indoc! {"
            main:
                st %a, [9999]
                ld [9999], %b
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
    Cycles: 4
    Halted: reset
    Memory:
      [9999] = 0
    ");
}

#[test]
fn memory_size_invalid_access() {
    // r[verify arch.memory.size]
    // Address 10000 is out of bounds and should raise an exception.
    let state = run_program(
        indoc! {"
            .addr 200
            handler: reset

            .addr 1000
            main:
                st %a, [10000]
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
      [102] = 5
    ");
}

#[test]
fn memory_cell_types() {
    // r[verify arch.memory.cell-types]
    // Cells can hold Word values. Empty cells read as 0.
    let state = run_program(
        indoc! {"
            .addr 500
            data: .word 42

            .addr 1000
            main:
                ld [500], %a
                ld [501], %b
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
    Cycles: 4
    Halted: reset
    Memory:
      [500] = 42
    ");
}

#[test]
fn memory_unified_code_data() {
    // r[verify arch.memory.unified]
    // No code/data distinction — store a value over code area and read it back.
    let state = run_program(
        indoc! {"
            .addr 500
            data: .word 0

            .addr 1000
            main:
                ld 99, %a
                st %a, [500]
                ld [500], %b
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 99
      %b  = 99
      %pc = 1003
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 5
    Halted: reset
    Memory:
      [500] = 99
    ");
}

// =============================================================================
// STACK
// =============================================================================

#[test]
fn stack_grows_downward() {
    // r[verify arch.stack]
    // %sp starts at 10000, push decrements it.
    let state = run_program(
        indoc! {"
            main:
                push %a
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
    ");
}

// =============================================================================
// SPECIAL ADDRESSES
// =============================================================================

#[test]
fn interrupt_saves_pc_at_100() {
    // r[verify arch.special-addr.interrupt-pc-save]
    // On exception, PC is saved at address 100.
    let state = run_program(
        indoc! {"
            .addr 200
            handler: reset

            .addr 1000
            main:
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
    Cycles: 1
    Halted: reset
    Memory:
      [100] = 1001
      [101] = 512
      [102] = 1
    ");
}

#[test]
fn interrupt_saves_sr_at_101() {
    // r[verify arch.special-addr.interrupt-sr-save]
    // On exception, SR is saved at address 101.
    let state = run_program(
        indoc! {"
            .addr 200
            handler: reset

            .addr 1000
            main:
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
    Cycles: 1
    Halted: reset
    Memory:
      [100] = 1001
      [101] = 512
      [102] = 1
    ");
}

#[test]
fn interrupt_saves_exception_code_at_102() {
    // r[verify arch.special-addr.interrupt-exception]
    // On exception, exception code is saved at address 102.
    // Division by zero is exception code 1.
    let state = run_program(
        indoc! {"
            .addr 200
            handler: reset

            .addr 1000
            main:
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
    Cycles: 1
    Halted: reset
    Memory:
      [100] = 1001
      [101] = 512
      [102] = 1
    ");
}

#[test]
fn interrupt_handler_at_200() {
    // r[verify arch.special-addr.interrupt-handler]
    // Exception jumps to address 200.
    let state = run_program(
        indoc! {"
            .addr 200
            handler:
                ld 77, %b
                reset

            .addr 1000
            main:
                div 0, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 0
      %b  = 77
      %pc = 201
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    Memory:
      [100] = 1001
      [101] = 512
      [102] = 1
    ");
}

#[test]
fn program_starts_at_1000() {
    // r[verify arch.special-addr.program-start]
    // Default program start address is 1000 — %pc should be 1000 initially.
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

// =============================================================================
// REGISTERS — GENERAL PURPOSE
// =============================================================================

#[test]
fn general_purpose_registers() {
    // r[verify reg.general-purpose]
    // %a and %b hold values independently.
    let state = run_program(
        indoc! {"
            main:
                ld 100, %a
                ld 200, %b
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 100
      %b  = 200
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

#[test]
fn empty_register_reads_as_zero() {
    // r[verify reg.general-purpose]
    // Uninitialized registers read as 0.
    let state = run_program(
        indoc! {"
            main:
                add %a, %b
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
      %sr = ZERO | SUPERVISOR
    Cycles: 1
    Halted: reset
    ");
}

// =============================================================================
// REGISTERS — PC
// =============================================================================

#[test]
fn pc_is_program_counter() {
    // r[verify reg.pc]
    // %pc holds the address of the next instruction.
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
// REGISTERS — SP
// =============================================================================

#[test]
fn sp_is_stack_pointer() {
    // r[verify reg.sp]
    // %sp starts at 10000 and changes on push/pop.
    let state = run_program(
        indoc! {"
            main:
                push %a
                push %b
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
    ");
}

// =============================================================================
// REGISTERS — SR (Status Register)
// =============================================================================

#[test]
fn sr_is_status_register() {
    // r[verify reg.sr]
    // %sr contains flags. Initially has SUPERVISOR set.
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
fn sr_privileged_write() {
    // r[verify reg.sr.privileged-write]
    // Writing %sr in user mode should raise an exception.
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

// =============================================================================
// FLAGS
// =============================================================================

#[test]
fn zero_flag() {
    // r[verify reg.flags.zero]
    // Z flag set when result is 0.
    let state = run_program(
        indoc! {"
            main:
                ld 5, %a
                sub 5, %a
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
fn negative_flag() {
    // r[verify reg.flags.negative]
    // N flag set when result < 0.
    let state = run_program(
        indoc! {"
            main:
                ld -1, %a
                add 0, %a
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
fn overflow_flag() {
    // r[verify reg.flags.overflow]
    // O flag on signed overflow.
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

#[test]
fn supervisor_flag() {
    // r[verify reg.flags.supervisor]
    // S flag is set in supervisor mode.
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
fn interrupt_enable_flag() {
    // r[verify reg.flags.interrupt-enable]
    // IE flag can be observed in %sr after setting it.
    let state = run_program(
        indoc! {"
            main:
                ld 768, %sr
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
      %sr = INTERRUPT_ENABLE | SUPERVISOR
    Cycles: 1
    Halted: reset
    ");
}

#[test]
fn carry_flag_reserved() {
    // r[verify reg.flags.carry]
    // Carry flag exists in SR but is not set by arithmetic instructions.
    // After arithmetic, CARRY bit should remain unset.
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
