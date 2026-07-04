use clap::Parser;

mod completion;
mod dap;
mod lsp;
mod run;

#[derive(Parser)]
pub enum Subcommand {
    /// Preprocess, compile and run a program
    Run(self::run::RunOpt),

    /// Generate shell completion
    Completion(self::completion::CompletionOpt),

    /// Start the Language Server (LSP)
    Lsp(self::lsp::LspOpt),

    /// Start the Debug Adapter Protocol server (DAP)
    Dap(self::dap::DapOpt),
}

impl Subcommand {
    /// Run a subcommand
    pub fn exec(self) -> anyhow::Result<()> {
        match self {
            Self::Run(opt) => opt.exec()?,
            Self::Completion(opt) => opt.exec(),
            Self::Lsp(opt) => opt.exec()?,
            Self::Dap(opt) => opt.exec()?,
        }

        Ok(())
    }
}
