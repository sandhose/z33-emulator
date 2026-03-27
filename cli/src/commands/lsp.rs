use clap::Parser;
use tokio_util::compat::{TokioAsyncReadCompatExt, TokioAsyncWriteCompatExt};
use z33_emulator::lsp::tower_lsp::{LspService, Server};
use z33_emulator::lsp::Backend;

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
            let stdin = tokio::io::stdin().compat();
            let stdout = tokio::io::stdout().compat_write();

            let (service, socket) = LspService::new(Backend::new);
            Server::new(stdin, stdout, socket).serve(service).await;
        });

        Ok(())
    }
}
