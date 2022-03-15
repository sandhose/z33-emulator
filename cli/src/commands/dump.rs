use std::path::PathBuf;

use clap::{Parser, ValueHint};

use tracing::{debug, info};
use z33_emulator::{
    parse,
    parser::location::{AbsoluteLocation, MapLocation},
    preprocessor::{preprocess, NativeFilesystem},
};

#[derive(Parser, Debug)]
pub struct DumpOpt {
    /// Input file
    #[clap(parse(from_os_str), value_hint = ValueHint::FilePath)]
    input: PathBuf,
}

impl DumpOpt {
    pub fn exec(&self) -> anyhow::Result<()> {
        let fs = NativeFilesystem::from_env()?;
        info!(path = ?self.input, "Reading program");
        let source = preprocess(&fs, &self.input).1?;
        let source = source.as_str();

        debug!("Parsing program");
        let program = parse(source).unwrap(); // TODO: the error is tied to the input

        debug!("Transforming AST");
        let ast = program.to_node();

        // Transform the AST relative locations to absolute ones
        let ast = ast.map_location(&AbsoluteLocation::default());

        println!("{}", ast);

        Ok(())
    }
}
