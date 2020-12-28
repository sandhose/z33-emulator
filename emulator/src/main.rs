#![forbid(unsafe_code)]

use std::path::PathBuf;

use clap::Clap;
use nom::{combinator::all_consuming, Finish};
use tracing::{debug, info};
use tracing_subscriber::filter::{EnvFilter, LevelFilter};

mod compiler;
mod constants;
mod interactive;
mod parser;
mod preprocessor;
mod runtime;
mod util;

use crate::compiler::DebugInfo;
use crate::constants::STACK_START;
use crate::interactive::run_interactive;
use crate::parser::line::parse_program;
use crate::preprocessor::preprocess;
use crate::runtime::{Computer, Registers};

#[derive(Clap)]
#[clap(version, author, about)]
enum Opt {
    /// Preprocess, compile and run a program
    Run {
        /// Input file
        #[clap(parse(from_os_str))]
        input: PathBuf,

        /// Start label
        entrypoint: String,

        #[clap(short, long)]
        interactive: bool,
    },

    /// Run the preprocessor
    Preprocess {
        /// Input file
        #[clap(parse(from_os_str))]
        input: PathBuf,
    },

    /// Print the program as parsed
    Print {
        /// Input file
        #[clap(parse(from_os_str))]
        input: PathBuf,
    },
}

impl Opt {
    /// Run a subcommand
    fn exec(self) -> Result<(), Box<dyn std::error::Error>> {
        match self {
            Opt::Run {
                input,
                entrypoint,
                interactive,
            } => run(input, entrypoint, interactive),
            Opt::Preprocess { input } => run_preprocessor(input),
            Opt::Print { input } => print(input),
        }
    }
}

/// Run a program
fn run(
    input: PathBuf,
    entrypoint: String,
    interactive: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    info!(path = ?input, "Reading program");
    let source = preprocess(input)?;
    let source = source.as_str();

    debug!("Parsing program");
    // TODO: proper error handling & wrap those steps
    let (_, program) = all_consuming(parse_program)(source).finish().unwrap();
    let lines: Vec<_> = program.lines.into_iter().map(|l| l.inner).collect(); // TODO: do not strip location information
    let layout = crate::compiler::layout::layout_memory(&lines).unwrap();
    let memory = crate::compiler::memory::fill_memory(&layout).unwrap();

    info!(entrypoint = %entrypoint, "Building computer");
    let mut computer = Computer {
        memory,
        registers: Registers {
            pc: *layout.labels.get(entrypoint.as_str()).unwrap(),
            sp: STACK_START,
            ..Default::default()
        },
        ..Default::default()
    };
    let debug_info = DebugInfo {
        labels: layout
            .labels
            .iter()
            .map(|(key, value)| (key.to_string(), *value))
            .collect(),
    };

    if interactive {
        run_interactive(&mut computer, debug_info)?;
    } else {
        info!("Running program");
        computer.run()?;
    }

    info!("Registers: {}", computer.registers);
    Ok(())
}

/// Preprocess and print a program
fn print(input: PathBuf) -> Result<(), Box<dyn std::error::Error>> {
    info!(path = ?input, "Reading program");
    let source = preprocess(input)?;
    let source = source.as_str();

    debug!("Parsing program");
    // TODO: proper error handling & wrap those steps
    let (_, program) = all_consuming(parse_program)(source).finish().unwrap();

    for line in program.lines {
        println!("{}", line.inner);
    }

    Ok(())
}

/// Run the preprocessor on a file
fn run_preprocessor(input: PathBuf) -> Result<(), Box<dyn std::error::Error>> {
    info!("Reading program from file {:?}", input);
    let source = preprocess(input)?;
    println!("{}", source);
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // First, setup the tracing formatter for logging and instrumentation
    let format = tracing_subscriber::fmt::format()
        .without_time()
        .with_target(false);

    tracing_subscriber::fmt()
        .with_env_filter(
            // Parse log level from env
            EnvFilter::from_default_env()
                // With INFO enabled by default
                .add_directive(LevelFilter::INFO.into()),
        )
        .event_format(format)
        .init();

    // Parse the arguments
    let opt = Opt::parse();
    // And run the command
    opt.exec()
}
