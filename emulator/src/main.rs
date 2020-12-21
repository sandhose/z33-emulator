#![forbid(unsafe_code)]

use clap::Clap;
use compiler::DebugInfo;
use constants::STACK_START;
use nom::{combinator::all_consuming, Finish};
use processor::{Computer, Registers};
use std::path::PathBuf;
use tracing::{debug, info};
use tracing_subscriber::filter::{EnvFilter, LevelFilter};

mod compiler;
mod constants;
mod interactive;
mod parser;
mod preprocessor;
mod processor;
mod util;

use crate::interactive::run_interactive;
use crate::parser::parse_program;
use crate::preprocessor::preprocess;

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
    let (_, lines) = all_consuming(parse_program)(source).finish().unwrap();
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
        .compact()
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
