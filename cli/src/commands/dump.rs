use std::path::PathBuf;

use clap::Clap;

use tracing::{debug, info};
use z33_emulator::{
    parse,
    parser::location::{AbsoluteLocation, Lines, RelativeLocation},
    preprocess,
};

#[derive(Clap, Debug)]
pub struct DumpOpt {
    /// Input file
    #[clap(parse(from_os_str))]
    input: PathBuf,
}

impl DumpOpt {
    pub fn exec(&self) -> Result<(), Box<dyn std::error::Error>> {
        info!(path = ?self.input, "Reading program");
        let source = preprocess(&self.input)?;
        let source = source.as_str();

        debug!("Parsing program");
        let program = parse(source).unwrap(); // TODO: the error is tied to the input

        debug!("Transforming AST");
        let ast = program.to_node();

        // Transform the AST relative locations to absolute ones
        let ast = ast.transform_location(
            &AbsoluteLocation::default(),
            &RelativeLocation::into_absolute,
        );

        // Map the AST absolute offsets to line/col locations
        let lines = Lines::new(source);
        let ast = ast.map_location(&|l| l.to_line_aware(&lines));

        println!("{}", ast);

        Ok(())
    }
}
