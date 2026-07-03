use std::fs;

use zed_extension_api::serde_json::{json, Value};
use zed_extension_api::{
    self as zed, Architecture, DebugAdapterBinary, DebugConfig, DebugRequest, DebugScenario,
    DebugTaskDefinition, DownloadedFileType, GithubReleaseOptions, LanguageServerId,
    LanguageServerInstallationStatus, Os, Result, StartDebuggingRequestArguments,
    StartDebuggingRequestArgumentsRequest, Worktree,
};

/// The GitHub repository whose releases carry the prebuilt `z33-cli` binaries.
const REPO: &str = "sandhose/z33-emulator";

/// Release asset and on-disk layout for the current platform.
struct PlatformCli {
    asset_name: String,
    file_type: DownloadedFileType,
    binary_name: &'static str,
    needs_chmod: bool,
}

fn platform_cli() -> Result<PlatformCli> {
    let (os, arch) = zed::current_platform();
    let arch = match arch {
        Architecture::Aarch64 => "aarch64",
        Architecture::X8664 => "x86_64",
        Architecture::X86 => return Err("no prebuilt z33-cli for 32-bit x86".to_string()),
    };
    Ok(match os {
        Os::Mac => PlatformCli {
            asset_name: format!("z33-cli-{arch}-macos.tar.gz"),
            file_type: DownloadedFileType::GzipTar,
            binary_name: "z33-cli",
            needs_chmod: true,
        },
        Os::Linux => PlatformCli {
            asset_name: format!("z33-cli-{arch}-linux.tar.gz"),
            file_type: DownloadedFileType::GzipTar,
            binary_name: "z33-cli",
            needs_chmod: true,
        },
        Os::Windows => PlatformCli {
            asset_name: format!("z33-cli-{arch}-windows.exe"),
            file_type: DownloadedFileType::Uncompressed,
            binary_name: "z33-cli.exe",
            needs_chmod: false,
        },
    })
}

/// Turns a path relative to the extension's work directory into an absolute
/// one, so it stays valid when the debug adapter is spawned with the worktree
/// root as its working directory.
fn absolutize(path: &str) -> String {
    match std::env::current_dir() {
        Ok(cwd) => cwd.join(path).to_string_lossy().into_owned(),
        Err(_) => path.to_string(),
    }
}

fn is_file(path: &str) -> bool {
    fs::metadata(path).is_ok_and(|metadata| metadata.is_file())
}

/// Parses `z33-cli-vX.Y.Z` into a numerically comparable version key
/// (lexicographic comparison would rank v0.9.0 above v0.10.0).
fn version_key(dir: &str) -> Option<(u32, u32, u32)> {
    let mut parts = dir.strip_prefix("z33-cli-v")?.split('.');
    let major = parts.next()?.parse().ok()?;
    let minor = parts.next()?.parse().ok()?;
    let patch = parts.next()?.parse().ok()?;
    parts.next().is_none().then_some((major, minor, patch))
}

/// Newest previously downloaded copy, if any (used when GitHub is
/// unreachable).
fn find_downloaded_cli(binary_name: &str) -> Option<String> {
    fs::read_dir(".")
        .ok()?
        .flatten()
        .filter_map(|entry| {
            let dir = entry.file_name().into_string().ok()?;
            let key = version_key(&dir)?;
            let path = format!("{dir}/{binary_name}");
            is_file(&path).then_some((key, path))
        })
        .max_by_key(|(key, _)| *key)
        .map(|(_, path)| path)
}

/// Downloads the release asset into `version_dir` and verifies the binary is
/// actually there and executable. Any failure after this point must discard
/// `version_dir` (the caller does), otherwise a truncated or non-executable
/// binary would be found — and trusted — by every later lookup.
fn install(
    release: &zed::GithubRelease,
    platform: &PlatformCli,
    version_dir: &str,
    binary_path: &str,
) -> Result<()> {
    let asset = release
        .assets
        .iter()
        .find(|asset| asset.name == platform.asset_name)
        .ok_or_else(|| {
            format!(
                "release {} has no asset named {}",
                release.version, platform.asset_name
            )
        })?;

    fs::create_dir_all(version_dir)
        .map_err(|err| format!("failed to create {version_dir}: {err}"))?;

    // Archives extract into the version directory; a bare executable
    // downloads straight to its final path.
    let download_target = match platform.file_type {
        DownloadedFileType::Uncompressed => binary_path.to_string(),
        _ => version_dir.to_string(),
    };
    zed::download_file(&asset.download_url, &download_target, platform.file_type)
        .map_err(|err| format!("failed to download {}: {err}", platform.asset_name))?;

    if !is_file(binary_path) {
        return Err(format!(
            "{} did not contain the expected {} binary",
            platform.asset_name, platform.binary_name
        ));
    }
    if platform.needs_chmod {
        zed::make_file_executable(binary_path)?;
    }
    Ok(())
}

/// Resolves the path of a downloaded `z33-cli`, downloading the latest
/// release if needed.
fn download_cli(status: &dyn Fn(LanguageServerInstallationStatus)) -> Result<String> {
    let platform = platform_cli()?;

    let release = match zed::latest_github_release(
        REPO,
        GithubReleaseOptions {
            require_assets: true,
            pre_release: false,
        },
    ) {
        Ok(release) => release,
        Err(err) => {
            // GitHub unreachable: fall back to a previous download.
            return find_downloaded_cli(platform.binary_name).ok_or_else(|| {
                format!("z33-cli is not on your PATH and fetching the latest release failed: {err}")
            });
        }
    };

    let version_dir = format!("z33-cli-{}", release.version);
    let binary_path = format!("{}/{}", version_dir, platform.binary_name);

    if !is_file(&binary_path) {
        status(LanguageServerInstallationStatus::Downloading);
        if let Err(err) = install(&release, &platform, &version_dir, &binary_path) {
            // Never leave a partial download behind: it would shadow the
            // re-download on the next call.
            let _ = fs::remove_dir_all(&version_dir);
            return Err(err);
        }

        // Best-effort cleanup of downloads from older releases.
        if let Ok(entries) = fs::read_dir(".") {
            for entry in entries.flatten() {
                let name = entry.file_name();
                let name = name.to_string_lossy();
                if name.starts_with("z33-cli-") && name != version_dir {
                    let _ = fs::remove_dir_all(entry.path());
                }
            }
        }
    }

    Ok(binary_path)
}

struct Z33Extension {
    cached_cli_path: Option<String>,
}

impl Z33Extension {
    /// Locates `z33-cli`: a binary on the user's PATH always wins; otherwise
    /// the latest GitHub release is downloaded into the extension's work
    /// directory (and reused across calls).
    fn cli_path(
        &mut self,
        worktree: &Worktree,
        language_server_id: Option<&LanguageServerId>,
    ) -> Result<String> {
        if let Some(path) = worktree.which("z33-cli") {
            return Ok(path);
        }

        if let Some(path) = &self.cached_cli_path {
            if is_file(path) {
                return Ok(path.clone());
            }
        }

        let status = |status: LanguageServerInstallationStatus| {
            if let Some(id) = language_server_id {
                zed::set_language_server_installation_status(id, &status);
            }
        };

        // Every error path must go through the Failed arm below, or Zed's
        // server status UI stays stuck on "checking for update".
        status(LanguageServerInstallationStatus::CheckingForUpdate);
        match download_cli(&status) {
            Ok(path) => {
                status(LanguageServerInstallationStatus::None);
                let path = absolutize(&path);
                self.cached_cli_path = Some(path.clone());
                Ok(path)
            }
            Err(err) => {
                status(LanguageServerInstallationStatus::Failed(err.clone()));
                Err(err)
            }
        }
    }
}

impl zed::Extension for Z33Extension {
    fn new() -> Self {
        Self {
            cached_cli_path: None,
        }
    }

    fn language_server_command(
        &mut self,
        language_server_id: &LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        let path = self.cli_path(worktree, Some(language_server_id))?;

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
        user_provided_debug_adapter_path: Option<String>,
        worktree: &Worktree,
    ) -> Result<DebugAdapterBinary, String> {
        // Unlike the LSP path, this deliberately reports no installation
        // status: Zed's DAP API has no per-adapter status channel, so a cold
        // first debug may pause silently while `cli_path` downloads the CLI.
        let path = match user_provided_debug_adapter_path {
            Some(path) => path,
            None => self.cli_path(worktree, None)?,
        };

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

        // `LaunchRequest` has no dedicated entrypoint field; mirror the
        // `z33-cli run <program> <entrypoint>` convention and treat the first
        // launch argument as the entrypoint label when present.
        let mut configuration = json!({
            "program": launch.program,
            "stopOnEntry": config.stop_on_entry.unwrap_or(false),
        });
        if let Some(entrypoint) = launch.args.first() {
            configuration["entrypoint"] = json!(entrypoint);
        }
        let configuration = configuration.to_string();

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

#[cfg(test)]
mod tests {
    use zed_extension_api::serde_json::{self, Value};
    use zed_extension_api::{AttachRequest, DebugConfig, DebugRequest, Extension, LaunchRequest};

    use super::{version_key, Z33Extension};

    #[test]
    fn version_key_orders_numerically() {
        // Lexicographic ordering would rank v0.9.0 above v0.10.0; the key must not.
        let v0_9 = version_key("z33-cli-v0.9.0").expect("v0.9.0 parses");
        let v0_10 = version_key("z33-cli-v0.10.0").expect("v0.10.0 parses");
        assert_eq!(v0_9, (0, 9, 0));
        assert_eq!(v0_10, (0, 10, 0));
        assert!(v0_9 < v0_10);
    }

    #[test]
    fn version_key_rejects_garbage() {
        // Missing prefix, non-numeric parts, and too-few/too-many components.
        assert_eq!(version_key("garbage"), None);
        assert_eq!(version_key("z33-cli-v1.2"), None);
        assert_eq!(version_key("z33-cli-v1.2.x"), None);
        assert_eq!(version_key("z33-cli-v1.2.3.4"), None);
    }

    fn launch_config(program: &str, args: Vec<String>) -> DebugConfig {
        DebugConfig {
            label: "test".to_string(),
            adapter: "z33".to_string(),
            request: DebugRequest::Launch(LaunchRequest {
                program: program.to_string(),
                cwd: None,
                args,
                envs: Vec::new(),
            }),
            stop_on_entry: Some(true),
        }
    }

    fn scenario_config(config: DebugConfig) -> Value {
        let scenario = Z33Extension::new()
            .dap_config_to_scenario(config)
            .expect("launch config converts");
        serde_json::from_str(&scenario.config).expect("config is valid JSON")
    }

    #[test]
    fn scenario_includes_entrypoint_when_present() {
        let config = scenario_config(launch_config("main.s", vec!["main".to_string()]));
        assert_eq!(config["program"], Value::from("main.s"));
        assert_eq!(config["entrypoint"], Value::from("main"));
        assert_eq!(config["stopOnEntry"], Value::from(true));
    }

    #[test]
    fn scenario_omits_entrypoint_when_absent() {
        let config = scenario_config(launch_config("main.s", Vec::new()));
        assert_eq!(config["program"], Value::from("main.s"));
        assert!(config.get("entrypoint").is_none());
    }

    #[test]
    fn scenario_rejects_attach() {
        let config = DebugConfig {
            label: "test".to_string(),
            adapter: "z33".to_string(),
            request: DebugRequest::Attach(AttachRequest { process_id: None }),
            stop_on_entry: None,
        };
        assert!(Z33Extension::new().dap_config_to_scenario(config).is_err());
    }
}
