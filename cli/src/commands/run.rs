use std::process::exit;

use camino::Utf8PathBuf;
use clap::{ArgAction, Parser, ValueHint};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use tracing::{debug, error, info};
use z33_emulator::compiler::CompilationError;
use z33_emulator::preprocessor::{NativeFilesystem, Preprocessor};
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
    pub fn exec(&self) -> anyhow::Result<()> {
        let fs = NativeFilesystem::from_env()?;
        info!(path = ?self.input, "Reading program");
        let preprocessor = Preprocessor::new(fs).and_load(&self.input);
        let source = match preprocessor.preprocess(&self.input) {
            Ok(p) => p,
            Err(e) => {
                for error in anyhow::Chain::new(&e) {
                    // TODO: get the location of individual errors
                    error!("{}", error);
                }

                let msg = format!("{e}");
                let files = SimpleFiles::<String, String>::new();
                let labels = Vec::new();

                /*
                if let Some(location) = e.location() {
                    labels.push(Label::primary(file_ids[&location.file], *location));
                }
                */

                let diagnostic = Diagnostic::error().with_message(msg).with_labels(labels);

                let writer = StandardStream::stderr(ColorChoice::Auto);
                let config = codespan_reporting::term::Config {
                    before_label_lines: 3,
                    after_label_lines: 3,
                    ..Default::default()
                };

                codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic)?;
                exit(1);
            }
        };
        let source = source.as_str();

        let mut files = SimpleFiles::new();
        let file_id = files.add("preprocessed", source);

        debug!("Parsing program");
        let program = match parse(source) {
            Ok(p) => p,
            Err(e) => {
                let msg = format!("{e}");
                let labels: Vec<_> = e
                    .errors
                    .iter()
                    .map(|(location, kind)| {
                        let message = match kind {
                            nom::error::VerboseErrorKind::Context(s) => (*s).to_owned(),
                            nom::error::VerboseErrorKind::Char(c) => format!("expected '{c}'"),
                            nom::error::VerboseErrorKind::Nom(code) => format!("{code:?}"),
                        };
                        let offset = char_offset(source, location);

                        Label::primary(file_id, offset..offset).with_message(message)
                    })
                    .collect();

                let diagnostic = Diagnostic::error().with_message(msg).with_labels(labels);

                let writer = StandardStream::stderr(ColorChoice::Auto);
                let config = codespan_reporting::term::Config {
                    before_label_lines: 3,
                    after_label_lines: 3,
                    ..Default::default()
                };

                codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic)?;
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
                    let label = Label::primary(file_id, location.clone());

                    let diagnostic = Diagnostic::error()
                        .with_message(msg)
                        .with_labels(vec![label]);

                    let writer = StandardStream::stderr(ColorChoice::Auto);
                    let config = codespan_reporting::term::Config {
                        before_label_lines: 3,
                        after_label_lines: 3,
                        ..Default::default()
                    };

                    codespan_reporting::term::emit(
                        &mut writer.lock(),
                        &config,
                        &files,
                        &diagnostic,
                    )?;
                }
                exit(1);
            }
        };

        info!("Running program");
        if self.interactive {
            run_interactive(&mut computer, debug_info)?;
        } else {
            computer.run()?;
        }

        info!(registers = %computer.registers, "End of program");

        Ok(())
    }
}
