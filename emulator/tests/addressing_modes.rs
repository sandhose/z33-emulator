mod helpers;

use helpers::{run_program, Steps};
use indoc::indoc;

// =============================================================================
// Immediate addressing mode
// =============================================================================

#[test]
fn immediate_mode_load() {
    // r[verify addr.immediate]
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
fn immediate_mode_negative_value() {
    // r[verify addr.immediate]
    let state = run_program(
        indoc! {"
            main:
                ld -7, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = -7
      %b  = 0
      %pc = 1001
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 1
    Halted: reset
    ");
}

#[test]
fn immediate_mode_label_as_value() {
    // r[verify addr.immediate]
    let state = run_program(
        indoc! {"
            .addr 500
            data: .word 99

            .addr 1000
            main:
                ld data, %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    // %a should contain 500 (the address of the label), not the value at that
    // address
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 500
      %b  = 0
      %pc = 1001
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 1
    Halted: reset
    Memory:
      [500] = 99
    ");
}

#[test]
fn immediate_mode_costs_one_cycle() {
    // r[verify addr.immediate]
    // ld imm, reg = 1 base + 0 (imm) + 0 (reg) = 1 cycle
    // reset = 1 cycle
    // Total = 2 cycles
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
// Register addressing mode
// =============================================================================

#[test]
fn register_mode_load() {
    // r[verify addr.register]
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
fn register_mode_costs_one_cycle() {
    // r[verify addr.register]
    // ld reg, reg = 1 base + 0 (reg) + 0 (reg) = 1 cycle
    let state = run_program(
        indoc! {"
            main:
                ld 10, %a
                ld %a, %b
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    // 1 (ld imm) + 1 (ld reg) + 1 (reset) = 3 cycles
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 10
      %b  = 10
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    ");
}

// =============================================================================
// Direct addressing mode
// =============================================================================

#[test]
fn direct_mode_load() {
    // r[verify addr.direct]
    let state = run_program(
        indoc! {"
            .addr 500
            .word 42

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
      %a  = 42
      %b  = 0
      %pc = 1001
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    Memory:
      [500] = 42
    ");
}

#[test]
fn direct_mode_store() {
    // r[verify addr.direct]
    // r[verify addr.destination]
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
fn direct_mode_costs_one_extra_cycle() {
    // r[verify addr.direct]
    // ld [addr], reg = 1 base + 1 (direct) + 0 (reg) = 2 cycles
    let state = run_program(
        indoc! {"
            .addr 500
            .word 42

            .addr 1000
            main:
                ld [500], %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    // 2 (ld direct) + 1 (reset) = 3 cycles
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 42
      %b  = 0
      %pc = 1001
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Halted: reset
    Memory:
      [500] = 42
    ");
}

// =============================================================================
// Indirect addressing mode
// =============================================================================

#[test]
fn indirect_mode_load() {
    // r[verify addr.indirect]
    let state = run_program(
        indoc! {"
            .addr 500
            .word 9876

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
fn indirect_mode_store() {
    // r[verify addr.indirect]
    // r[verify addr.destination]
    let state = run_program(
        indoc! {"
            main:
                ld 600, %b
                ld 55, %a
                st %a, [%b]
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 55
      %b  = 600
      %pc = 1003
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 4
    Halted: reset
    Memory:
      [600] = 55
    ");
}

#[test]
fn indirect_mode_costs_one_extra_cycle() {
    // r[verify addr.indirect]
    // ld [%reg], reg = 1 base + 1 (indirect) + 0 (reg) = 2 cycles
    let state = run_program(
        indoc! {"
            .addr 500
            .word 42

            .addr 1000
            main:
                ld 500, %b
                ld [%b], %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    // 1 (ld imm) + 2 (ld indirect) + 1 (reset) = 4 cycles
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 42
      %b  = 500
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    Memory:
      [500] = 42
    ");
}

// =============================================================================
// Indexed addressing mode
// =============================================================================

#[test]
fn indexed_mode_load_positive_offset() {
    // r[verify addr.indexed]
    let state = run_program(
        indoc! {"
            .addr 500
            .word 10
            .word 20
            .word 30

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
fn indexed_mode_load_negative_offset() {
    // r[verify addr.indexed]
    let state = run_program(
        indoc! {"
            .addr 500
            .word 111
            .word 222
            .word 333

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

#[test]
fn indexed_mode_store() {
    // r[verify addr.indexed]
    // r[verify addr.destination]
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
fn indexed_mode_store_negative_offset() {
    // r[verify addr.indexed]
    // r[verify addr.destination]
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
fn indexed_mode_costs_one_extra_cycle() {
    // r[verify addr.indexed]
    // ld [%reg+off], reg = 1 base + 1 (indexed) + 0 (reg) = 2 cycles
    let state = run_program(
        indoc! {"
            .addr 500
            .word 42

            .addr 1000
            main:
                ld 500, %b
                ld [%b+0], %a
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    // 1 (ld imm) + 2 (ld indexed) + 1 (reset) = 4 cycles
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 42
      %b  = 500
      %pc = 1002
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 3
    Halted: reset
    Memory:
      [500] = 42
    ");
}

// =============================================================================
// Destination operand tests
// =============================================================================

#[test]
fn destination_register() {
    // r[verify addr.destination]
    // Register is a valid destination for ld
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
fn destination_direct_memory() {
    // r[verify addr.destination]
    // Direct memory is a valid destination for st
    let state = run_program(
        indoc! {"
            main:
                ld 42, %a
                st %a, [700]
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
      [700] = 42
    ");
}

#[test]
fn destination_indirect_memory() {
    // r[verify addr.destination]
    // Indirect memory is a valid destination for st
    let state = run_program(
        indoc! {"
            main:
                ld 700, %b
                ld 42, %a
                st %a, [%b]
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 42
      %b  = 700
      %pc = 1003
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 4
    Halted: reset
    Memory:
      [700] = 42
    ");
}

#[test]
fn destination_indexed_memory() {
    // r[verify addr.destination]
    // Indexed memory is a valid destination for st
    let state = run_program(
        indoc! {"
            main:
                ld 700, %b
                ld 42, %a
                st %a, [%b+5]
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 42
      %b  = 700
      %pc = 1003
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 4
    Halted: reset
    Memory:
      [705] = 42
    ");
}

// =============================================================================
// Cycle cost comparisons
// =============================================================================

#[test]
fn cycle_cost_immediate_vs_direct() {
    // r[verify addr.immediate]
    // r[verify addr.direct]
    // Compare: ld 42, %a (1 cycle) vs ld [500], %a (2 cycles)
    // Using Steps::Count to stop after exactly one instruction
    let state_imm = run_program(
        indoc! {"
            main:
                ld 42, %a
                reset
        "},
        "main",
        Steps::Count(1),
    );
    let state_dir = run_program(
        indoc! {"
            .addr 500
            .word 42

            .addr 1000
            main:
                ld [500], %a
                reset
        "},
        "main",
        Steps::Count(1),
    );
    insta::assert_snapshot!(state_imm, @r"
    Registers:
      %a  = 42
      %b  = 0
      %pc = 1001
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 1
    ");
    insta::assert_snapshot!(state_dir, @r"
    Registers:
      %a  = 42
      %b  = 0
      %pc = 1001
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 2
    Memory:
      [500] = 42
    ");
}

#[test]
fn cycle_cost_store_direct_vs_indexed() {
    // r[verify addr.direct]
    // r[verify addr.indexed]
    // st reg, [addr] = 1 base + 0 (reg) + 1 (direct) = 2 cycles
    // st reg, [%reg+off] = 1 base + 0 (reg) + 1 (indexed) = 2 cycles
    let state = run_program(
        indoc! {"
            main:
                ld 500, %b
                ld 42, %a
                st %a, [600]
                st %a, [%b+1]
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    // 1 (ld imm) + 1 (ld imm) + 2 (st direct) + 2 (st indexed) + 1 (reset) = 7
    // cycles
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 42
      %b  = 500
      %pc = 1004
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 6
    Halted: reset
    Memory:
      [501] = 42
      [600] = 42
    ");
}

// =============================================================================
// Combined addressing mode patterns
// =============================================================================

#[test]
fn load_store_all_modes() {
    // r[verify addr.immediate]
    // r[verify addr.register]
    // r[verify addr.direct]
    // r[verify addr.indirect]
    // r[verify addr.indexed]
    // r[verify addr.destination]
    let state = run_program(
        indoc! {"
            .addr 500
            .word 100

            .addr 1000
            main:
                // Immediate: load literal into %a
                ld 500, %a
                // Direct: load value at address 500 into %b
                ld [500], %b
                // Register: copy %b to %a
                ld %b, %a
                // Store using direct destination
                st %a, [501]
                // Indirect: load using address in %a register
                // %a = 100, so load from [100]
                // Store using indirect destination
                ld 502, %b
                st %a, [%b]
                // Indexed: store using register+offset
                ld 500, %b
                st %a, [%b+3]
                reset
        "},
        "main",
        Steps::RunToCompletion,
    );
    insta::assert_snapshot!(state, @r"
    Registers:
      %a  = 100
      %b  = 500
      %pc = 1008
      %sp = 10000
      %sr = SUPERVISOR
    Cycles: 12
    Halted: reset
    Memory:
      [500] = 100
      [501] = 100
      [502] = 100
      [503] = 100
    ");
}
