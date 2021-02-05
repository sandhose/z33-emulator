#![forbid(unsafe_code)]

use std::process::exit;

use clap::Clap;
use tracing::error;
use tracing_subscriber::filter::EnvFilter;
use tracing_subscriber::prelude::*;

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
    const fn log_filter(&self) -> &'static str {
        match self.verbose {
            0 => "info",
            1 => "z33_emulator=debug,z33_cli=debug",
            2 => "z33_emulator=trace,z33_emulator=trace",
            3 => "z33_emulator=trace,z33_emulator=trace,debug",
            4..=u8::MAX => "trace",
        }
    }

    fn filter_layer(&self) -> EnvFilter {
        // Parse log level from env
        EnvFilter::try_from_default_env()
            // or infer from args
            .or_else(|_| EnvFilter::try_new(self.log_filter()))
            .unwrap()
    }
}

fn main() {
    // First, parse the arguments
    let opt = Opt::parse();

    // Then, setup the tracing formatter for logging and instrumentation
    let fmt_layer = tracing_subscriber::fmt::layer()
        .without_time()
        .with_target(false);

    tracing_subscriber::Registry::default()
        .with(opt.filter_layer())
        .with(fmt_layer)
        .init();

    // And run the command
    let res = opt.command.exec();
    if let Err(e) = res {
        error!("{}", e);
        exit(1);
    }
}
