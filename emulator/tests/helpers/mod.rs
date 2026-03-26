#![allow(dead_code)]

use std::fmt::Write;

use z33_emulator::compiler::DebugInfo;
use z33_emulator::preprocessor::{InMemoryFilesystem, Workspace};
use z33_emulator::runtime::{Cell, Computer, ProcessorError};
use z33_emulator::{compile, constants as C, parse};

#[derive(Clone, Copy)]
pub enum Steps {
    Count(usize),
    RunToCompletion,
}

#[must_use]
pub fn run_program(source: &str, entrypoint: &str, steps: Steps) -> String {
    let fs = InMemoryFilesystem::new([("/main.S".into(), source.into())]);
    let mut workspace = Workspace::new(&fs, "/main.S");
    let preprocess_result = workspace.preprocess().expect("preprocess failed");
    let preprocessed = preprocess_result.source;
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
    let (mut computer, _debug_info) =
        compile(&result.program.inner, entrypoint).expect("compilation failed");

    let halt_reason = match steps {
        Steps::Count(n) => {
            let mut reason = None;
            for _ in 0..n {
                match computer.step() {
                    Ok(()) => {}
                    Err(ProcessorError::Reset) => {
                        reason = Some("reset".to_string());
                        break;
                    }
                    Err(e) => {
                        reason = Some(format!("error: {e}"));
                        break;
                    }
                }
            }
            reason
        }
        Steps::RunToCompletion => match computer.run() {
            Ok(()) => Some("reset".to_string()),
            Err(e) => Some(format!("error: {e}")),
        },
    };

    format_state(&computer, halt_reason.as_deref())
}

fn format_sr(sr_bits: C::Word) -> String {
    let flags = [
        (0b000_0000_0001, "CARRY"),
        (0b000_0000_0010, "ZERO"),
        (0b000_0000_0100, "NEGATIVE"),
        (0b000_0000_1000, "OVERFLOW"),
        (0b001_0000_0000, "INTERRUPT_ENABLE"),
        (0b010_0000_0000, "SUPERVISOR"),
    ];

    let active: Vec<&str> = flags
        .iter()
        .filter(|(bit, _)| sr_bits & bit != 0)
        .map(|(_, name)| *name)
        .collect();

    if active.is_empty() {
        "(none)".to_string()
    } else {
        active.join(" | ")
    }
}

fn format_state(computer: &Computer, halt_reason: Option<&str>) -> String {
    let mut out = String::new();

    // Registers
    let a_val = format_cell(&computer.registers.a);
    let b_val = format_cell(&computer.registers.b);
    writeln!(out, "Registers:").unwrap();
    writeln!(out, "  %a  = {a_val}").unwrap();
    writeln!(out, "  %b  = {b_val}").unwrap();
    writeln!(out, "  %pc = {}", computer.registers.pc).unwrap();
    writeln!(out, "  %sp = {}", computer.registers.sp).unwrap();
    writeln!(out, "  %sr = {}", format_sr(computer.registers.sr.bits())).unwrap();

    // Cycles
    writeln!(out, "Cycles: {}", computer.cycles).unwrap();

    // Halt reason
    if let Some(reason) = halt_reason {
        writeln!(out, "Halted: {reason}").unwrap();
    }

    // Non-empty memory (skip instruction cells)
    let data_cells: Vec<(C::Address, &Cell)> = computer
        .memory
        .iter_non_empty()
        .filter(|(_, cell)| !matches!(cell, Cell::Instruction(_)))
        .collect();

    if !data_cells.is_empty() {
        writeln!(out, "Memory:").unwrap();
        for (addr, cell) in data_cells {
            writeln!(out, "  [{addr}] = {}", format_cell(cell)).unwrap();
        }
    }

    out
}

/// Compile a program and return a formatted snapshot of the memory layout and
/// labels. Unlike `run_program`, this does NOT execute — it only shows
/// compilation output.
#[must_use]
pub fn compile_program(source: &str, entrypoint: &str) -> String {
    let fs = InMemoryFilesystem::new([("/main.S".into(), source.into())]);
    let mut workspace = Workspace::new(&fs, "/main.S");
    let preprocess_result = workspace.preprocess().expect("preprocess failed");
    let preprocessed = preprocess_result.source;
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
    let (computer, debug_info) =
        compile(&result.program.inner, entrypoint).expect("compilation failed");

    format_compilation(&computer, &debug_info)
}

fn format_compilation(computer: &Computer, debug_info: &DebugInfo) -> String {
    let mut out = String::new();

    // Labels (sorted by address, then name)
    let mut labels: Vec<(&String, &C::Address)> = debug_info.labels.iter().collect();
    labels.sort_by_key(|(name, addr)| (**addr, name.as_str()));
    writeln!(out, "Labels:").unwrap();
    for (name, addr) in &labels {
        writeln!(out, "  {name} = {addr}").unwrap();
    }

    // Entrypoint
    writeln!(out, "Entrypoint: %pc = {}", computer.registers.pc).unwrap();

    // All non-empty memory cells (including instructions this time)
    writeln!(out, "Memory:").unwrap();
    for (addr, cell) in computer.memory.iter_non_empty() {
        writeln!(out, "  [{addr}] = {}", format_cell(cell)).unwrap();
    }

    out
}

fn format_cell(cell: &Cell) -> String {
    match cell {
        Cell::Empty => "0".to_string(),
        Cell::Word(w) => w.to_string(),
        Cell::Instruction(i) => format!("<{i}>"),
    }
}
