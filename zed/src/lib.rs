use zed_extension_api::{self as zed, LanguageServerId, Result};

struct Z33Extension;

impl zed::Extension for Z33Extension {
    fn new() -> Self {
        Self
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        let path = worktree
            .which("z33-cli")
            .ok_or_else(|| "z33-cli must be installed and available on your PATH".to_string())?;

        Ok(zed::Command {
            command: path,
            args: vec!["lsp".to_string()],
            env: Vec::default(),
        })
    }
}

zed::register_extension!(Z33Extension);
