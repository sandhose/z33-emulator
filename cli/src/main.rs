#![forbid(unsafe_code)]

use clap::Clap;
use tracing_subscriber::filter::EnvFilter;

mod commands;
mod interactive;

use crate::commands::Opt;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // First, setup the tracing formatter for logging and instrumentation
    let format = tracing_subscriber::fmt::format()
        .without_time()
        .with_target(false);

    tracing_subscriber::fmt()
        .with_env_filter(
            // Parse log level from env
            EnvFilter::try_from_default_env()
                // or default to "info"
                .unwrap_or_else(|_| EnvFilter::new("info")),
        )
        .event_format(format)
        .init();

    // Parse the arguments
    let opt = Opt::parse();
    // And run the command
    opt.exec()
}
