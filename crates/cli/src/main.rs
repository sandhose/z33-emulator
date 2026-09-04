use std::fs::{File, OpenOptions};
use std::io::{self, IsTerminal, Write};
use std::process::exit;

use camino::Utf8PathBuf;
use clap::{ArgAction, ArgGroup, Parser, ValueHint};
use tracing::error;
use tracing_subscriber::filter::EnvFilter;
use tracing_subscriber::fmt::MakeWriter;
use tracing_subscriber::prelude::*;

mod commands;
mod framing;
mod interactive;

use crate::commands::Subcommand;

#[derive(Parser)]
#[clap(version, author, about, group = ArgGroup::new("colors"))]
struct Opt {
    /// Increase the level of verbosity. Can be used multiple times.
    #[clap(short, long, action = ArgAction::Count, global(true))]
    verbose: u8,

    /// Force colored output of the logs. Default is to check if the output is
    /// a tty
    #[clap(short = 'c', long, action = ArgAction::SetTrue, global(true), group = "colors")]
    color: bool,

    /// Force non-colored output of the logs. Default is to check if the output
    /// is a tty
    #[clap(short = 'C', long, action = ArgAction::SetTrue, global(true), group = "colors")]
    no_color: bool,

    /// Use JSON output for log messages
    #[clap(short, long, action = ArgAction::SetTrue, global(true))]
    json: bool,

    /// Append the logs to this file instead of printing them
    #[clap(long, global(true), value_hint = ValueHint::FilePath)]
    log: Option<Utf8PathBuf>,

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

    /// `--color` and `--no-color` win, then a non-empty `NO_COLOR` turns
    /// colors off. Otherwise colors are on when the target stream is a
    /// terminal, and off for a file.
    fn should_use_colors(&self, target: &LogTarget) -> bool {
        if self.color {
            return true;
        }
        if self.no_color {
            return false;
        }
        if std::env::var_os("NO_COLOR").is_some_and(|v| !v.is_empty()) {
            return false;
        }
        match target {
            LogTarget::Stdout => io::stdout().is_terminal(),
            LogTarget::Stderr => io::stderr().is_terminal(),
            LogTarget::File(_) => false,
        }
    }

    fn filter_layer(&self) -> EnvFilter {
        // Parse log level from env
        EnvFilter::try_from_default_env()
            // or infer from args
            .or_else(|_| EnvFilter::try_new(self.log_filter()))
            .unwrap()
    }

    /// Where log lines go: the file from `--log` if given; otherwise stdout,
    /// except for LSP and DAP, which use stdout for their own protocol and so
    /// log to stderr.
    fn log_target(&self) -> LogTarget {
        if let Some(path) = &self.log {
            let file = OpenOptions::new()
                .create(true)
                .append(true)
                .open(path)
                .unwrap_or_else(|e| {
                    eprintln!("error: could not open log file {path}: {e}");
                    exit(1);
                });
            LogTarget::File(file)
        } else if matches!(self.command, Subcommand::Lsp(_) | Subcommand::Dap(_)) {
            LogTarget::Stderr
        } else {
            LogTarget::Stdout
        }
    }
}

/// Where the tracing subscriber writes formatted log lines.
enum LogTarget {
    Stdout,
    Stderr,
    File(File),
}

impl<'a> MakeWriter<'a> for LogTarget {
    type Writer = Box<dyn Write + Send + 'a>;

    fn make_writer(&'a self) -> Self::Writer {
        match self {
            Self::Stdout => Box::new(io::stdout()),
            Self::Stderr => Box::new(io::stderr()),
            Self::File(file) => Box::new(file),
        }
    }
}

/// Build and install the global tracing subscriber, writing to `target` in
/// either the plain or JSON format.
fn init_tracing(filter: EnvFilter, json: bool, target: LogTarget, ansi: bool) {
    let registry = tracing_subscriber::Registry::default().with(filter);
    if json {
        registry
            .with(tracing_subscriber::fmt::layer().json().with_writer(target))
            .init();
    } else {
        registry
            .with(
                tracing_subscriber::fmt::layer()
                    .without_time()
                    .with_ansi(ansi)
                    .with_target(false)
                    .with_writer(target),
            )
            .init();
    }
}

fn main() {
    // First, parse the arguments
    let opt = Opt::parse();

    // Then, setup the tracing formatter for logging and instrumentation
    let target = opt.log_target();
    let ansi = opt.should_use_colors(&target);
    init_tracing(opt.filter_layer(), opt.json, target, ansi);

    // And run the command
    let res = opt.command.exec();
    if let Err(e) = res {
        // `{:#}` prints the whole anyhow chain on one line: the outermost
        // context alone rarely says what went wrong.
        error!("{e:#}");
        exit(1);
    }
}
