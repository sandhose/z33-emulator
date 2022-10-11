use clap::Parser;

mod completion;
mod dump;
mod preprocess;
mod print;
mod run;

#[derive(Parser)]
pub enum Subcommand {
    /// Preprocess, compile and run a program
    Run(self::run::RunOpt),

    /// Run the preprocessor
    Preprocess(self::preprocess::PreprocessOpt),

    /// Print the program as parsed
    Print(self::print::PrintOpt),

    /// Dump the AST of the program
    Dump(self::dump::DumpOpt),

    /// Generate shell completion
    Completion(self::completion::CompletionOpt),
}

impl Subcommand {
    /// Run a subcommand
    pub fn exec(self) -> anyhow::Result<()> {
        match self {
            Self::Run(opt) => opt.exec()?,
            Self::Preprocess(opt) => opt.exec()?,
            Self::Print(opt) => opt.exec()?,
            Self::Dump(opt) => opt.exec()?,
            Self::Completion(opt) => opt.exec(),
        }

        Ok(())
    }
}
