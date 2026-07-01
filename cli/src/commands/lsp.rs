use clap::Parser;
use z33_emulator::lsp::tower_lsp::{LspService, Server};
use z33_emulator::lsp::{Backend, WORKSPACE_FILES_METHOD};

/// Start the Z33 Language Server (LSP)
#[derive(Parser, Debug)]
pub struct LspOpt {}

impl LspOpt {
    #[allow(clippy::unused_self)]
    pub fn exec(self) -> anyhow::Result<()> {
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;

        rt.block_on(async {
            let stdin = tokio::io::stdin();
            let stdout = tokio::io::stdout();

            let (service, socket) = LspService::build(Backend::new)
                .custom_method(WORKSPACE_FILES_METHOD, Backend::workspace_files)
                .finish();
            Server::new(stdin, stdout, socket).serve(service).await;
        });

        Ok(())
    }
}
