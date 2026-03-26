use std::process::exit;

use camino::Utf8PathBuf;
use clap::{ArgAction, Parser, ValueHint};
use tracing::{debug, info};
use z33_emulator::diagnostic::{
    preprocessor_error_to_diagnostics, render_to_string, resolve_diagnostic_spans,
};
use z33_emulator::preprocessor::{NativeFilesystem, ReferencingSourceMap, Workspace};
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
        let source_map: ReferencingSourceMap = preprocess_result.source_map.into();

        debug!("Parsing program");
        let parse_result = parse(source);

        debug!(entrypoint = %self.entrypoint, "Building computer");
        let compile_result = compile(
            &parse_result.program.inner,
            &parse_result.diagnostics,
            Some(&self.entrypoint),
            preprocess_result.preprocessed_file_id,
        );

        // Show all diagnostics (parse + compilation), resolved to original files
        if !compile_result.diagnostics.is_empty() {
            for diag in &compile_result.diagnostics {
                let resolved = resolve_diagnostic_spans(diag, &source_map);
                eprint!("{}", render_to_string(&resolved, workspace.file_db()));
            }
            exit(1);
        }

        let mut computer = compile_result.computer.expect("no errors but no computer");
        let debug_info = compile_result.debug_info;

        info!("Running program");
        if self.interactive {
            run_interactive(&mut computer, debug_info);
        } else {
            computer.run()?;
        }

        Ok(())
    }
}
