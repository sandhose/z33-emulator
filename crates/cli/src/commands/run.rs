use std::io::{BufRead, BufReader, Write};
use std::process::exit;
use std::sync::mpsc::{self, Receiver, TryRecvError};
use std::thread;

use camino::Utf8PathBuf;
use clap::{ArgAction, Parser, ValueHint};
use tracing::{debug, info};
use z33_emulator::diagnostic::{
    preprocessor_error_to_diagnostics, render_to_string, resolve_diagnostic_spans,
};
use z33_emulator::preprocessor::{NativeFilesystem, SourceMap, Workspace};
use z33_emulator::runtime::{Computer, ProcessorError};
use z33_emulator::{compile, parse};

use crate::interactive::run_interactive;

/// The number of instructions to execute between servicing host I/O. Small
/// enough that input latency stays imperceptible, large enough that the I/O
/// bookkeeping is negligible.
const IO_BATCH: usize = 10_000;

/// A message from the background stdin reader thread.
enum Input {
    /// A line read from stdin, including its trailing `\n`.
    Line(String),
    /// Stdin reached EOF; no further input will ever arrive.
    Eof,
}

#[derive(Parser, Debug)]
pub struct RunOpt {
    /// Input file
    #[clap(value_parser, value_hint = ValueHint::FilePath)]
    input: Utf8PathBuf,

    /// Start label
    #[clap(value_parser)]
    entrypoint: String,

    /// Run the program in interactive mode
    #[clap(short, long, action = ArgAction::SetTrue)]
    interactive: bool,
}

impl RunOpt {
    pub fn exec(self) -> anyhow::Result<()> {
        let fs = NativeFilesystem::from_env()?;
        info!(path = ?self.input, "Reading program");
        let mut workspace = Workspace::new(&fs, &self.input);
        let preprocess_result = match workspace.preprocess() {
            Ok(p) => p,
            Err(e) => {
                let diagnostics = preprocessor_error_to_diagnostics(&e);
                for diag in &diagnostics {
                    eprint!("{}", render_to_string(diag, workspace.file_db()));
                }
                exit(1);
            }
        };
        let source = preprocess_result.source.as_str();
        let source_map: SourceMap = preprocess_result.source_map;

        debug!("Parsing program");
        let parse_result = parse(source);

        debug!(entrypoint = %self.entrypoint, "Building computer");
        let compile_result = compile(
            &parse_result.program.inner,
            &parse_result.diagnostics,
            Some(&self.entrypoint),
            preprocess_result.preprocessed_file_id,
        );

        // Show all diagnostics (parse + compilation), resolved to original files
        if !compile_result.diagnostics.is_empty() {
            for diag in &compile_result.diagnostics {
                let resolved = resolve_diagnostic_spans(diag, &source_map);
                eprint!("{}", render_to_string(&resolved, workspace.file_db()));
            }
            exit(1);
        }

        let mut computer = compile_result.computer.expect("no errors but no computer");
        let debug_info = compile_result.debug_info;

        info!("Running program");
        if self.interactive {
            run_interactive(&mut computer, debug_info);
        } else {
            run_with_io(&mut computer)?;
        }

        Ok(())
    }
}

/// Spawn a thread that reads stdin line by line, forwarding each line (and a
/// final [`Input::Eof`] marker) over a channel. Blocking reads live off the
/// main thread so the emulator keeps running while waiting for input.
fn spawn_stdin_reader() -> Receiver<Input> {
    let (tx, rx) = mpsc::channel();
    thread::spawn(move || {
        let mut reader = BufReader::new(std::io::stdin());
        loop {
            let mut line = String::new();
            match reader.read_line(&mut line) {
                // EOF or a read error: signal EOF once and stop reading forever.
                Ok(0) | Err(_) => {
                    let _ = tx.send(Input::Eof);
                    break;
                }
                Ok(_) => {
                    if tx.send(Input::Line(line)).is_err() {
                        break;
                    }
                }
            }
        }
    });
    rx
}

/// Drain whatever the program has written to the serial console and write it
/// straight to stdout. Output bypasses `tracing` on purpose: it is raw program
/// output and must not be mangled by log formatting.
fn flush_serial_output(computer: &mut Computer) -> anyhow::Result<()> {
    let output = computer.io.serial.drain_output();
    if !output.is_empty() {
        let stdout = std::io::stdout();
        let mut handle = stdout.lock();
        handle.write_all(&output)?;
        handle.flush()?;
    }
    Ok(())
}

/// Run the program with the serial console wired to the process's stdio.
///
/// The core `Computer::run` loop cannot interleave host I/O, so the CLI owns
/// the loop: pending stdin lines are pushed into the serial receive queue (one
/// `push_input` per line, preserving one-interrupt-per-line semantics), then a
/// bounded burst of instructions runs before draining serial output back to
/// stdout. `in` never blocks; a program awaiting input simply busy-polls until
/// a line arrives.
fn run_with_io(computer: &mut Computer) -> anyhow::Result<()> {
    let rx = spawn_stdin_reader();
    // Once stdin hits EOF we latch it and never touch the channel again, so we
    // never busy-wait on input that will never come.
    let mut stdin_open = true;

    loop {
        // Feed any input that has arrived since the last iteration.
        while stdin_open {
            match rx.try_recv() {
                Ok(Input::Line(line)) => computer.io.serial.push_input(line.as_bytes()),
                Ok(Input::Eof) | Err(TryRecvError::Disconnected) => stdin_open = false,
                Err(TryRecvError::Empty) => break,
            }
        }

        // Advance the program by a bounded burst so input latency stays low.
        let mut reset = false;
        for _ in 0..IO_BATCH {
            match computer.step() {
                Ok(()) => {}
                Err(ProcessorError::Reset) => {
                    reset = true;
                    break;
                }
                Err(e) => {
                    // Flush any final output before surfacing the error.
                    flush_serial_output(computer)?;
                    return Err(e.into());
                }
            }
        }

        flush_serial_output(computer)?;

        if reset {
            return Ok(());
        }
    }
}
