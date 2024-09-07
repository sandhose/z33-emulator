use std::process::exit;

use camino::Utf8PathBuf;
use clap::{ArgAction, Parser, ValueHint};
use tracing::{debug, error, info};
use z33_emulator::compiler::CompilationError;
use z33_emulator::preprocessor::{NativeFilesystem, Workspace};
use z33_emulator::{compile, parse};

use crate::interactive::run_interactive;

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

fn char_offset(a: &str, b: &str) -> usize {
    let a = a.as_ptr();
    let b = b.as_ptr();
    b as usize - a as usize
}

impl RunOpt {
    #[allow(clippy::too_many_lines)]
    pub fn exec(self) -> anyhow::Result<()> {
        let fs = NativeFilesystem::from_env()?;
        info!(path = ?self.input, "Reading program");
        let preprocessor = Workspace::new(&fs, &self.input);
        let (source_map, source) = match preprocessor.preprocess() {
            Ok(p) => p,
            Err(e) => {
                let report = miette::Report::new(e);
                eprintln!("{report:?}");
                exit(1);
            }
        };
        let source = source.as_str();

        debug!("Parsing program");
        let program = match parse(source) {
            Ok(p) => p,
            Err(e) => {
                // The nom errors are… bad. Let's just print the first location
                let (location, _kind) = e.errors.first().expect("at least one error");
                let offset = char_offset(source, location);
                // Find the corresponding span from the source map
                let span = source_map
                    .find(offset)
                    .expect("source info to be available");
                let labels = vec![miette::LabeledSpan::underline(span.span.clone())];
                let report = miette::miette!(labels = labels, "Failed to parse program")
                    .with_source_code(span.source.clone());
                eprintln!("{report:?}");
                exit(1);
            }
        };

        debug!(entrypoint = %self.entrypoint, "Building computer");
        let (mut computer, debug_info) = match compile(program.inner, &self.entrypoint) {
            Ok(p) => p,
            Err(e) => {
                // TODO: some cleanup needed
                let mut last_error = &e as &dyn std::error::Error;
                for error in anyhow::Chain::new(&e) {
                    // TODO: get the location of individual errors
                    error!("{}", error);
                    last_error = error;
                }

                let msg = format!("{last_error}");

                let location = match &e {
                    CompilationError::MemoryLayout(e) => e.location(),
                    CompilationError::MemoryFill(e) => Some(e.location()),
                    CompilationError::UnknownEntrypoint(_e) => None,
                };

                if let Some(location) = location {
                    // Find the spans for the start and the end of the error
                    let start = source_map
                        .find(location.start)
                        .expect("source info to be available");
                    let end = source_map
                        .find(location.end)
                        .expect("source info to be available");

                    // Both spans should be in the same file. If not, we just use the start span
                    let range = if start.source == end.source {
                        start.span.start..end.span.end
                    } else {
                        start.span.clone()
                    };
                    let source = start.source;

                    let labels = vec![miette::LabeledSpan::at(range, msg)];
                    let report = miette::miette!(labels = labels, "Failed to compile program")
                        .with_source_code(source.clone());
                    eprintln!("{report:?}");
                }
                exit(1);
            }
        };

        info!("Running program");
        if self.interactive {
            run_interactive(&mut computer, debug_info);
        } else {
            computer.run()?;
        }

        info!(registers = %computer.registers, "End of program");

        Ok(())
    }
}
