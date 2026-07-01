use zed_extension_api::serde_json::{json, Value};
use zed_extension_api::{
    self as zed, DebugAdapterBinary, DebugConfig, DebugRequest, DebugScenario, DebugTaskDefinition,
    LanguageServerId, Result, StartDebuggingRequestArguments,
    StartDebuggingRequestArgumentsRequest, Worktree,
};

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

    fn get_dap_binary(
        &mut self,
        _adapter_name: String,
        config: DebugTaskDefinition,
        _user_provided_debug_adapter_path: Option<String>,
        worktree: &Worktree,
    ) -> Result<DebugAdapterBinary, String> {
        let path = worktree.which("z33-cli").ok_or_else(|| {
            "z33-cli must be installed and available on your PATH to use the debugger".to_string()
        })?;

        Ok(DebugAdapterBinary {
            command: Some(path),
            arguments: vec!["dap".to_string()],
            envs: Vec::default(),
            cwd: Some(worktree.root_path()),
            connection: None,
            request_args: StartDebuggingRequestArguments {
                configuration: config.config,
                request: StartDebuggingRequestArgumentsRequest::Launch,
            },
        })
    }

    fn dap_request_kind(
        &mut self,
        _adapter_name: String,
        _config: Value,
    ) -> Result<StartDebuggingRequestArgumentsRequest, String> {
        // `z33-cli dap` only ever launches a fresh program; it can't attach to
        // an already-running debuggee.
        Ok(StartDebuggingRequestArgumentsRequest::Launch)
    }

    fn dap_config_to_scenario(&mut self, config: DebugConfig) -> Result<DebugScenario, String> {
        let launch = match config.request {
            DebugRequest::Launch(launch) => launch,
            DebugRequest::Attach(_) => {
                return Err(
                    "z33-cli dap only supports launching a program, not attaching".to_string(),
                );
            }
        };

        let configuration = json!({
            "program": launch.program,
            "stopOnEntry": config.stop_on_entry.unwrap_or(false),
        })
        .to_string();

        Ok(DebugScenario {
            adapter: config.adapter,
            label: config.label,
            build: None,
            config: configuration,
            tcp_connection: None,
        })
    }
}

zed::register_extension!(Z33Extension);
