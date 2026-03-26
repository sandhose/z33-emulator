use camino::Utf8PathBuf;
use clap::{Parser, ValueHint};
use tracing::{debug, info};
use z33_emulator::diagnostic::{parse_diagnostic_to_codespan, render_to_string};
use z33_emulator::parse;
use z33_emulator::preprocessor::{NativeFilesystem, Workspace};

#[derive(Parser, Debug)]
pub struct PrintOpt {
    /// Input file
    #[clap(value_parser, value_hint = ValueHint::FilePath)]
    input: Utf8PathBuf,
}

impl PrintOpt {
    pub fn exec(self) -> anyhow::Result<()> {
        let fs = NativeFilesystem::from_env()?;
        info!(path = ?self.input, "Reading program");
        let mut preprocessor = Workspace::new(&fs, &self.input);

        let preprocess_result = preprocessor.preprocess()?;
        let source = preprocess_result.source.as_str();

        debug!("Parsing program");
        let result = parse(source);
        if !result.diagnostics.is_empty() {
            for diag in &result.diagnostics {
                let codespan_diag =
                    parse_diagnostic_to_codespan(diag, preprocess_result.preprocessed_file_id);
                eprint!(
                    "{}",
                    render_to_string(&codespan_diag, preprocessor.file_db())
                );
            }
        }
        println!("{}", result.program);

        Ok(())
    }
}
