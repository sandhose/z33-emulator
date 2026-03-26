use camino::Utf8PathBuf;
use clap::{Parser, ValueHint};
use tracing::{debug, info};
use z33_emulator::parse;
use z33_emulator::preprocessor::{NativeFilesystem, Workspace};

#[derive(Parser, Debug)]
pub struct DumpOpt {
    /// Input file
    #[clap(value_parser, value_hint = ValueHint::FilePath)]
    input: Utf8PathBuf,
}

impl DumpOpt {
    pub fn exec(self) -> anyhow::Result<()> {
        let fs = NativeFilesystem::from_env()?;
        info!(path = ?self.input, "Reading program");
        let mut preprocessor = Workspace::new(&fs, &self.input);

        let result = preprocessor.preprocess()?;
        let source = result.source.as_str();

        debug!("Parsing program");
        let result = parse(source);
        if !result.diagnostics.is_empty() {
            for diag in &result.diagnostics {
                eprintln!("parse error: {}", diag.message);
            }
        }

        debug!("Transforming AST");
        let ast = result.program.to_node();

        println!("{ast}");

        Ok(())
    }
}
