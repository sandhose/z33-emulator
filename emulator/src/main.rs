#![forbid(unsafe_code)]

use clap::Clap;
use std::path::PathBuf;
use tracing::{debug, info};
use tracing_subscriber::filter::{EnvFilter, LevelFilter};

mod compiler;
mod interactive;
mod memory;
mod parser;
mod preprocessor;
mod processor;
mod util;

use crate::compiler::CompilerState;
use crate::interactive::run_interactive;
use crate::parser::Parser;
use crate::preprocessor::preprocess;

#[derive(Clap)]
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
    let parser = Parser::new(source);

    let mut compiler = CompilerState::default();
    parser.compile(&mut compiler).map_err(|e| {
        // Display a nice message if it is a parser error
        if let parser::ParserError::ParserError { kind, offset } = e {
            let message = format!("{:?}", kind);
            util::display_error_offset(source, offset, message.as_str());
        };

        e
    })?;

    info!(entrypoint = %entrypoint, "Building computer");
    let mut computer = compiler.build(entrypoint)?;

    if interactive {
        run_interactive(&mut computer)?;
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
