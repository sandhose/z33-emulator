//! Integration tests for the serial console peripheral and the `in`/`out`
//! instructions.

#![allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]

use indoc::indoc;
use pretty_assertions::assert_eq;
use z33_emulator::compiler::DebugInfo;
use z33_emulator::preprocessor::{InMemoryFilesystem, Workspace};
use z33_emulator::runtime::registers::StatusRegister;
use z33_emulator::runtime::{Cell, Computer, ProcessorError};
use z33_emulator::{compile, constants as C, parse};

/// Compile a program to a ready-to-run [`Computer`] and its debug info.
fn build(source: &str, entrypoint: &str) -> (Computer, DebugInfo) {
    let fs = InMemoryFilesystem::new([("/main.S".into(), source.into())]);
    let mut workspace = Workspace::new(&fs, "/main.S");
    let preprocessed = workspace.preprocess().expect("preprocess failed").source;
    let result = parse(&preprocessed);
    assert!(
        result.diagnostics.is_empty(),
        "parse errors: {:?}",
        result
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
    let compile_result = compile(
        &result.program.inner,
        &result.diagnostics,
        Some(entrypoint),
        0,
    );
    assert!(
        compile_result.diagnostics.is_empty(),
        "compilation errors: {:?}",
        compile_result
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
    (
        compile_result.computer.expect("no errors but no computer"),
        compile_result.debug_info,
    )
}

/// Read the word stored at a memory address (asserting it holds a word).
fn word_at(computer: &Computer, address: C::Address) -> C::Word {
    match computer.memory.get(address).unwrap() {
        Cell::Word(w) => *w,
        Cell::Empty => 0,
        Cell::Instruction(_) => panic!("expected a word at address {address}"),
    }
}

/// Run to completion (until `reset`), asserting no error along the way.
fn run(computer: &mut Computer) {
    match computer.run() {
        Ok(()) => {}
        Err(e) => panic!("unexpected error: {e}"),
    }
}

#[test]
fn out_writes_bytes_to_serial() {
    // r[verify inst.out]
    // r[verify io.serial.data.write]
    let (mut computer, _) = build(
        indoc! {"
            .addr 1000
            main:
                out 72, [111]   // 'H'
                out 105, [111]  // 'i'
                reset
        "},
        "main",
    );
    run(&mut computer);
    assert_eq!(computer.io.serial.drain_output(), b"Hi");
}

#[test]
fn in_reads_bytes_from_serial() {
    // r[verify inst.in]
    // r[verify io.serial.data.read]
    let (mut computer, _) = build(
        indoc! {"
            .addr 1000
            main:
                in [111], %a
                st %a, [10]
                in [111], %a
                st %a, [11]
                reset
        "},
        "main",
    );
    computer.io.serial.push_input(b"Zx");
    run(&mut computer);
    assert_eq!(word_at(&computer, 10), 90); // 'Z'
    assert_eq!(word_at(&computer, 11), 120); // 'x'
                                             // The queue is now empty; a further read would yield 0.
    assert!(!computer.io.serial.input_ready());
}

#[test]
fn in_never_blocks_on_empty_queue() {
    // r[verify inst.in]
    // Reading an empty serial console returns 0 immediately instead of blocking.
    let (mut computer, _) = build(
        indoc! {"
            .addr 1000
            main:
                in [111], %a
                st %a, [10]
                reset
        "},
        "main",
    );
    run(&mut computer);
    assert_eq!(word_at(&computer, 10), 0);
}

#[test]
fn hardware_interrupt_is_delivered_between_instructions() {
    // r[verify io.interrupt.delivery]
    // r[verify exc.code.hardware-interrupt]
    // r[verify io.serial.interrupt.enable-with-queued]
    let (mut computer, _) = build(
        indoc! {"
            .addr 200
            handler:
                reset

            .addr 1000
            main:
                out 1, [110]        // enable rx interrupts (E bit)
                ld 768, %sr         // supervisor (512) + interrupt-enable (256)
            idle:
                jmp idle
        "},
        "main",
    );

    // Deliver a byte before interrupts are enabled: enabling E while a byte is
    // queued raises the interrupt (enable-with-queued).
    computer.io.serial.push_input(b"A");

    // Step 1: `out 1, [110]` arms the controller (raises the interrupt), but
    // %sr.IE is not set yet so nothing is delivered.
    computer.step().unwrap();
    assert_eq!(computer.registers.pc, 1001);
    // Step 2: `ld 768, %sr` enables interrupts; the pending interrupt is then
    // delivered between this instruction and the next.
    computer.step().unwrap();

    // We jumped to the handler at 200.
    assert_eq!(computer.registers.pc, C::INTERRUPT_HANDLER);
    // The exception code (address 102) is 0: a hardware interrupt.
    assert_eq!(word_at(&computer, C::INTERRUPT_EXCEPTION), 0);
    // Interrupts were disabled on hardware-interrupt entry.
    assert!(!computer
        .registers
        .sr
        .contains(StatusRegister::INTERRUPT_ENABLE));
    // ...but we are still (and now definitely) in supervisor mode.
    assert!(computer.registers.sr.contains(StatusRegister::SUPERVISOR));
}

#[test]
fn masked_interrupt_edge_is_preserved() {
    // A pending interrupt raised while %sr.IE is clear is not lost: it is
    // delivered as soon as interrupts are re-enabled.
    // r[verify io.interrupt.delivery]
    let (mut computer, _) = build(
        indoc! {"
            .addr 200
            handler:
                reset

            .addr 1000
            main:
                out 1, [110]        // enable rx interrupts, but %sr.IE is clear
                nop
                ld 768, %sr         // now enable interrupts
            idle:
                jmp idle
        "},
        "main",
    );
    computer.io.serial.push_input(b"A"); // raises the edge (E already implied)

    computer.step().unwrap(); // out: arms E -> raises edge; IE still clear
    computer.step().unwrap(); // nop: IE still clear, edge preserved
    assert_eq!(computer.registers.pc, 1002);
    computer.step().unwrap(); // ld %sr: IE set -> edge delivered
    assert_eq!(computer.registers.pc, C::INTERRUPT_HANDLER);
}

#[test]
fn echo_sample_polling() {
    // r[verify inst.in]
    // r[verify inst.out]
    // End-to-end: the polling echo sample reflects its input back to the host.
    let (mut computer, _) = build(include_str!("../../../samples/echo.s"), "main");
    computer.io.serial.push_input(b"hi\x04"); // "hi" then Ctrl-D
    run(&mut computer);
    assert_eq!(computer.io.serial.drain_output(), b"hi");
}

#[test]
fn in_and_out_leave_flags_unchanged() {
    // r[verify inst.in]
    // r[verify inst.out]
    // `in`/`out` set no flags: force every flag on beforehand and check that
    // none of them moved after issuing both instructions.
    let (mut computer, _) = build(
        indoc! {"
            .addr 1000
            main:
                ld 42, %a
                out %a, [111]
                in  [111], %a
                reset
        "},
        "main",
    );
    computer.registers.sr = StatusRegister::all();
    let before = computer.registers.sr;
    run(&mut computer);
    assert_eq!(computer.registers.sr, before);
}

#[test]
fn hardware_interrupt_is_not_delivered_on_the_exception_recovery_step() {
    // r[verify io.interrupt.delivery]
    // r[verify exc.handling.save-state]
    // A software exception (trap) and a pending hardware interrupt must not
    // collide on the same step: the trap's save area (100-102) must survive
    // intact until a later step delivers the interrupt.
    let (mut computer, _) = build(
        indoc! {"
            .addr 200
            handler:
                nop
                reset

            .addr 1000
            main:
                ld 256, %sr     // interrupt-enable (supervisor stays set)
                trap
                reset
        "},
        "main",
    );

    // Step 1: enable interrupts. No pending edge yet, so nothing is delivered.
    computer.step().unwrap();
    assert!(computer
        .registers
        .sr
        .contains(StatusRegister::INTERRUPT_ENABLE));

    // Arm a pending serial interrupt edge *after* IE was enabled, so it
    // cannot be delivered as a side effect of the previous step.
    computer.io.write(110, 1); // enable rx interrupts (E bit)
    computer.io.serial.push_input(b"z"); // raises the edge

    // Step 2: `trap` triggers a software exception. Its recovery must not
    // also deliver the pending hardware interrupt on this same step.
    computer.step().unwrap();
    assert_eq!(computer.registers.pc, C::INTERRUPT_HANDLER);
    assert_eq!(word_at(&computer, C::INTERRUPT_PC_SAVE), 1002);
    assert_eq!(word_at(&computer, C::INTERRUPT_SR_SAVE), 256);
    assert_eq!(word_at(&computer, C::INTERRUPT_EXCEPTION), 4); // trap

    // Step 3: executing the handler's first instruction (`nop`) completes
    // normally, so the pending hardware interrupt is now delivered, clobbering
    // the save area with the interrupt's own state.
    computer.step().unwrap();
    assert_eq!(computer.registers.pc, C::INTERRUPT_HANDLER);
    assert_eq!(word_at(&computer, C::INTERRUPT_EXCEPTION), 0); // hardware interrupt
}

#[test]
fn echo_sample_interrupt_driven_batched() {
    // A single multi-byte delivery raises only one interrupt edge; the
    // sample's handler must drain the receive queue to see every byte
    // instead of assuming one byte per interrupt.
    // r[verify io.serial.interrupt.raise]
    let (mut computer, debug_info) =
        build(include_str!("../../../samples/echo-interrupt.s"), "main");
    let idle = *debug_info.labels.get("idle").expect("idle label");

    computer.step().unwrap(); // out RX_IRQ_ENABLE, [CONTROL]
    computer.step().unwrap(); // ld SR_SUPERVISOR_IE, %sr
    assert_eq!(computer.registers.pc, idle);

    // Deliver "hi" and Ctrl-D in a single push: one interrupt edge covers all
    // three bytes.
    computer.io.serial.push_input(b"hi\x04");

    run(&mut computer);
    assert_eq!(computer.io.serial.drain_output(), b"hi");
}

#[test]
fn echo_sample_interrupt_driven() {
    // End-to-end: the interrupt-driven echo sample reflects input pushed one
    // byte at a time (each delivery raises its own interrupt).
    // r[verify io.interrupt.delivery]
    let (mut computer, debug_info) =
        build(include_str!("../../../samples/echo-interrupt.s"), "main");
    let idle = *debug_info.labels.get("idle").expect("idle label");

    // Run the setup (arm the controller + enable interrupts) up to the idle
    // loop.
    computer.step().unwrap(); // out RX_IRQ_ENABLE, [CONTROL]
    computer.step().unwrap(); // ld SR_SUPERVISOR_IE, %sr
    assert_eq!(computer.registers.pc, idle);

    let mut halted = false;
    for &byte in b"hi\x04" {
        computer.io.serial.push_input(&[byte]);
        // Let the interrupt fire and the handler run until we are back idling
        // (byte serviced) or the machine resets (Ctrl-D).
        for _ in 0..100 {
            match computer.step() {
                Ok(()) => {}
                Err(ProcessorError::Reset) => {
                    halted = true;
                    break;
                }
                Err(e) => panic!("unexpected error: {e}"),
            }
            if !computer.io.serial.input_ready() && computer.registers.pc == idle {
                break;
            }
        }
        if halted {
            break;
        }
    }

    assert!(halted, "the sample should reset on Ctrl-D");
    assert_eq!(computer.io.serial.drain_output(), b"hi");
}
