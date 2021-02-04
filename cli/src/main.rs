#![forbid(unsafe_code)]

use clap::Clap;
use tracing::Level;
use tracing_subscriber::filter::EnvFilter;

mod commands;
mod interactive;

use crate::commands::Subcommand;

#[derive(Clap)]
#[clap(version, author, about)]
struct Opt {
    /// Increase the level of verbosity. Can be used multiple times.
    #[clap(short, long, parse(from_occurrences), global(true))]
    verbose: u8,

    #[clap(subcommand)]
    command: Subcommand,
}

impl Opt {
    fn log_level(&self) -> Level {
        match self.verbose {
            0 => Level::INFO,
            1 => Level::DEBUG,
            2..=u8::MAX => Level::TRACE,
        }
    }

    fn log_env_filter(&self) -> EnvFilter {
        // Parse log level from env
        EnvFilter::try_from_default_env()
            // or infer from args
            .unwrap_or_else(|_| EnvFilter::default().add_directive(self.log_level().into()))
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // First, parse the arguments
    let opt = Opt::parse();

    // Then, setup the tracing formatter for logging and instrumentation
    let format = tracing_subscriber::fmt::format()
        .without_time()
        .with_target(false);

    tracing_subscriber::fmt()
        .with_env_filter(opt.log_env_filter())
        .event_format(format)
        .init();

    // And run the command
    opt.command.exec()
}
