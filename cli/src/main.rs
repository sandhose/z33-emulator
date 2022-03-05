#![forbid(unsafe_code)]

use std::process::exit;

use clap::{ArgGroup, Parser};
use tracing::error;
use tracing_subscriber::filter::EnvFilter;
use tracing_subscriber::prelude::*;

mod commands;
mod interactive;

use crate::commands::Subcommand;

#[derive(Parser)]
#[clap(version, author, about, group = ArgGroup::new("format"))]
struct Opt {
    /// Increase the level of verbosity. Can be used multiple times.
    #[clap(short, long, parse(from_occurrences), global(true))]
    verbose: u8,

    /// Force colored output. Default is to check if the output is a tty
    #[clap(short = 'c', long, global(true), group = "format")]
    color: bool,

    /// Force non-colored output. Default is to check if the output is a tty
    #[clap(short = 'C', long, global(true), group = "format")]
    no_color: bool,

    /// Use JSON output for log messages
    #[clap(short, long, global(true), group = "format")]
    json: bool,

    #[clap(subcommand)]
    command: Subcommand,
}

impl Opt {
    const fn log_filter(&self) -> &'static str {
        match self.verbose {
            0 => "info",
            1 => "z33_emulator=debug,z33_cli=debug,info",
            2 => "z33_emulator=trace,z33_cli=trace,info",
            3 => "z33_emulator=trace,z33_cli=trace,debug",
            4..=u8::MAX => "trace",
        }
    }

    fn should_use_colors(&self) -> bool {
        if self.color {
            true
        } else if self.no_color {
            false
        } else {
            atty::is(atty::Stream::Stdout)
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
    let registry = tracing_subscriber::Registry::default().with(opt.filter_layer());

    if opt.json {
        let json_layer = tracing_subscriber::fmt::layer().json();
        registry.with(json_layer).init();
    } else {
        let fmt_layer = tracing_subscriber::fmt::layer()
            .without_time()
            .with_ansi(opt.should_use_colors())
            .with_target(false);
        registry.with(fmt_layer).init();
    }

    // And run the command
    let res = opt.command.exec();
    if let Err(e) = res {
        error!("{}", e);
        exit(1);
    }
}
