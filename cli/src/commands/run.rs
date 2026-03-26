use std::process::exit;

use camino::Utf8PathBuf;
use clap::{ArgAction, Parser, ValueHint};
use tracing::{debug, error, info};
use z33_emulator::diagnostic::{
    compilation_error_to_diagnostic, parse_diagnostic_to_codespan,
    preprocessor_error_to_diagnostics, render_to_string, resolve_to_original,
};
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

impl RunOpt {
    #[allow(clippy::too_many_lines)]
    pub fn exec(self) -> anyhow::Result<()> {
        let fs = NativeFilesystem::from_env()?;
        info!(path = ?self.input, "Reading program");
        let mut workspace = Workspace::new(&fs, &self.input);
        let preprocess_result = match workspace.preprocess() {
            Ok(p) => p,
            Err(e) => {
                let diagnostics = preprocessor_error_to_diagnostics(&e);
                for diag in &diagnostics {
                    eprint!("{}", render_to_string(diag, workspace.file_db()));
                }
                exit(1);
            }
        };
        let source = preprocess_result.source.as_str();

        debug!("Parsing program");
        let result = parse(source);
        if !result.diagnostics.is_empty() {
            for diag in &result.diagnostics {
                // Try to map through source map to original file
                if let Some((file_id, range)) =
                    resolve_to_original(&preprocess_result.source_map, diag.span.clone())
                {
                    let codespan_diag = codespan_reporting::diagnostic::Diagnostic::error()
                        .with_message(&diag.message)
                        .with_labels(vec![codespan_reporting::diagnostic::Label::primary(
                            file_id, range,
                        )]);
                    eprint!("{}", render_to_string(&codespan_diag, workspace.file_db()));
                } else {
                    // Fall back to rendering against preprocessed output
                    let codespan_diag =
                        parse_diagnostic_to_codespan(diag, preprocess_result.preprocessed_file_id);
                    eprint!("{}", render_to_string(&codespan_diag, workspace.file_db()));
                }
            }
            if result
                .diagnostics
                .iter()
                .any(|d| d.severity == z33_emulator::parser::DiagnosticSeverity::Error)
            {
                exit(1);
            }
        }
        let program = result.program;

        debug!(entrypoint = %self.entrypoint, "Building computer");
        let (mut computer, debug_info) = match compile(&program.inner, &self.entrypoint) {
            Ok(p) => p,
            Err(e) => {
                // TODO: some cleanup needed
                let mut last_error = &e as &dyn std::error::Error;
                for error in anyhow::Chain::new(&e) {
                    error!("{}", error);
                    last_error = error;
                }

                let _ = last_error;

                let codespan_diag =
                    compilation_error_to_diagnostic(&e, preprocess_result.preprocessed_file_id);
                eprint!("{}", render_to_string(&codespan_diag, workspace.file_db()));
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
