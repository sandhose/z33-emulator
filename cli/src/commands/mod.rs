use clap::Clap;

mod dump;
mod preprocess;
mod print;
mod run;

#[derive(Clap)]
pub enum Subcommand {
    /// Preprocess, compile and run a program
    Run(self::run::RunOpt),

    /// Run the preprocessor
    Preprocess(self::preprocess::PreprocessOpt),

    /// Print the program as parsed
    Print(self::print::PrintOpt),

    /// Dump the AST of the program
    Dump(self::dump::DumpOpt),
}

impl Subcommand {
    /// Run a subcommand
    pub fn exec(self) -> Result<(), Box<dyn std::error::Error>> {
        use Subcommand::*;
        match self {
            Run(opt) => opt.exec(),
            Preprocess(opt) => opt.exec(),
            Print(opt) => opt.exec(),
            Dump(opt) => opt.exec(),
        }
    }
}
