// Z33 web extension entry point.
//
// Runs entirely in the browser extension host (works in vscode.dev / github.dev
// and desktop VS Code alike). There is no native binary: the language server is
// a wasm worker and the debug adapter is an inline wasm implementation.
//
// Wiring:
//   * LSP  — spawn a classic worker, hand it the wasm bytes (handshake), then
//     attach a browser LanguageClient. Push all workspace `.s`/`.S` files via
//     the custom `z33/workspaceFiles` notification and keep it fresh with a
//     file-system watcher.
//   * DAP  — instantiate the wasm on the host once and register an inline debug
//     adapter factory (`Z33DebugAdapter`).

import * as vscode from "vscode";
import { LanguageClient, type LanguageClientOptions } from "vscode-languageclient/browser";

import initDapWasm, { WasmDapServer } from "../dist/pkg/z33_web.js";
import { Z33DebugAdapter } from "./debug-adapter.js";
import {
  collectWorkspaceFiles,
  FILE_GLOB,
  includeWorkspaceFolderInPaths,
} from "./workspace-paths.js";

const WORKSPACE_FILES_METHOD = "z33/workspaceFiles";
/**
 * Client-side command carried by the server's run code lens. Advertised to
 * the server via the `experimental.commands` client capability; the server
 * only emits the lens when it sees the advertisement.
 */
const RUN_COMMAND = "z33.run";
/** Give the wasm worker a generous window to instantiate before giving up. */
const HANDSHAKE_TIMEOUT_MS = 30_000;
/** Debounce window for coalescing rapid file-watcher events. */
const WATCH_DEBOUNCE_MS = 300;

let client: LanguageClient | undefined;

/** Read the wasm binary shipped in the extension as raw bytes. */
async function readWasmBytes(context: vscode.ExtensionContext): Promise<Uint8Array> {
  const uri = vscode.Uri.joinPath(context.extensionUri, "dist/pkg/z33_web_bg.wasm");
  return vscode.workspace.fs.readFile(uri);
}

/** Spawn the LSP worker and complete the init handshake before returning it. */
async function startLspWorker(
  context: vscode.ExtensionContext,
  wasmBytes: Uint8Array,
): Promise<Worker> {
  const workerUri = vscode.Uri.joinPath(context.extensionUri, "dist/lsp-server.worker.js");
  const worker = new Worker(workerUri.toString(true));

  await new Promise<void>((resolve, reject) => {
    const cleanup = () => {
      clearTimeout(timer);
      worker.removeEventListener("message", onMessage);
      worker.removeEventListener("error", onError);
    };
    const onMessage = (event: MessageEvent) => {
      const data = event.data as { type?: unknown; message?: unknown };
      if (typeof data !== "object" || data === null) {
        return;
      }
      if (data.type === "ready") {
        cleanup();
        resolve();
      } else if (data.type === "error") {
        // Init rejected inside the worker (e.g. wasm instantiate failed).
        cleanup();
        reject(new Error(String(data.message ?? "LSP worker failed to initialize")));
      }
    };
    const onError = (event: ErrorEvent) => {
      cleanup();
      reject(new Error(event.message || "LSP worker failed to start"));
    };
    // Never hang activation forever if the worker goes silent.
    const timer = setTimeout(() => {
      cleanup();
      reject(new Error("Timed out waiting for the Z33 language server to start"));
    }, HANDSHAKE_TIMEOUT_MS);
    worker.addEventListener("message", onMessage);
    worker.addEventListener("error", onError);

    // Transfer a standalone copy of the wasm bytes to the worker.
    const buffer = wasmBytes.buffer.slice(
      wasmBytes.byteOffset,
      wasmBytes.byteOffset + wasmBytes.byteLength,
    );
    worker.postMessage({ type: "init", wasmBytes: buffer }, [buffer]);
  });

  return worker;
}

async function pushWorkspaceFiles(lsp: LanguageClient): Promise<void> {
  const { files } = await collectWorkspaceFiles();
  await lsp.sendNotification(WORKSPACE_FILES_METHOD, { files });
}

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  try {
    await activateInner(context);
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    void vscode.window.showErrorMessage(`Z33 extension failed to activate: ${message}`);
    throw error;
  }
}

async function activateInner(context: vscode.ExtensionContext): Promise<void> {
  const wasmBytes = await readWasmBytes(context);

  // --- Language server (wasm worker) -------------------------------------
  const worker = await startLspWorker(context, wasmBytes);
  context.subscriptions.push({ dispose: () => worker.terminate() });

  const clientOptions: LanguageClientOptions = {
    // Manage on-disk, virtual (vscode.dev / github.dev) and unsaved documents;
    // deliberately exclude read-only virtual schemes (git/diff views).
    documentSelector: [
      { language: "z33-assembly", scheme: "file" },
      { language: "z33-assembly", scheme: "vscode-vfs" },
      { language: "z33-assembly", scheme: "untitled" },
      // @vscode/test-web mounts the workspace under this scheme; without it
      // the extension is untestable in a dev web host.
      { language: "z33-assembly", scheme: "vscode-test-web" },
    ],
  };

  client = new LanguageClient("z33-assembly", "Z33 Language Server", worker, clientOptions);
  // Tell the server which client-side commands we can execute, so it emits
  // the run code lens (rust-analyzer-style `experimental.commands`).
  client.registerFeature({
    fillClientCapabilities(capabilities) {
      capabilities.experimental = {
        ...(capabilities.experimental as Record<string, unknown> | undefined),
        commands: [RUN_COMMAND],
      };
    },
    initialize() {},
    getState() {
      return { kind: "static" };
    },
    clear() {},
  });
  await client.start();

  // Handler for the run code lens: start a debug session with the lens's
  // label as the entrypoint. The lens carries the server's workspace-relative
  // path; map it back to the real file URI (the debug adapter matches
  // `program` against `uri.toString()`).
  context.subscriptions.push(
    vscode.commands.registerCommand(RUN_COMMAND, async (args: { path: string; label: string }) => {
      const includeFolder = includeWorkspaceFolderInPaths();
      const uris = await vscode.workspace.findFiles(FILE_GLOB);
      const uri = uris.find((u) => vscode.workspace.asRelativePath(u, includeFolder) === args.path);
      if (uri === undefined) {
        void vscode.window.showErrorMessage(`Z33: could not find ${args.path} in the workspace`);
        return;
      }
      await vscode.debug.startDebugging(vscode.workspace.getWorkspaceFolder(uri), {
        type: "z33",
        request: "launch",
        name: `Run ${args.label}`,
        program: uri.toString(),
        entrypoint: args.label,
        // Start paused on the label's first instruction (mirrors the web
        // IDE, whose debug mode always starts stopped): the student lands in
        // the debugger looking at their code, not at an already-finished run.
        stopOnEntry: true,
      });
    }),
  );

  // Seed the server's include-resolution base map and keep it in sync. Watcher
  // events can arrive in bursts (multi-file save, branch switch); debounce and
  // serialize so pushes can't interleave and land stale (latest wins).
  await pushWorkspaceFiles(client);
  let debounceTimer: ReturnType<typeof setTimeout> | undefined;
  let inFlight: Promise<void> = Promise.resolve();
  const refresh = () => {
    if (debounceTimer !== undefined) {
      clearTimeout(debounceTimer);
    }
    debounceTimer = setTimeout(() => {
      debounceTimer = undefined;
      // Chain onto the previous push so two refreshes never run concurrently.
      // The body never rejects, so a failed push cannot break the chain.
      inFlight = inFlight.then(async () => {
        try {
          if (client !== undefined) {
            await pushWorkspaceFiles(client);
          }
        } catch (error) {
          console.error("[z33] failed to push workspace files", error);
        }
      });
    }, WATCH_DEBOUNCE_MS);
  };
  const watcher = vscode.workspace.createFileSystemWatcher(FILE_GLOB);
  watcher.onDidCreate(refresh);
  watcher.onDidChange(refresh);
  watcher.onDidDelete(refresh);
  context.subscriptions.push(watcher, {
    dispose: () => {
      if (debounceTimer !== undefined) {
        clearTimeout(debounceTimer);
      }
    },
  });

  // --- Debug adapter (inline wasm) ---------------------------------------
  await initDapWasm({ module_or_path: wasmBytes });
  const factory: vscode.DebugAdapterDescriptorFactory = {
    createDebugAdapterDescriptor() {
      return new vscode.DebugAdapterInlineImplementation(new Z33DebugAdapter(new WasmDapServer()));
    },
  };
  context.subscriptions.push(vscode.debug.registerDebugAdapterDescriptorFactory("z33", factory));
}

export function deactivate(): Promise<void> | undefined {
  return client?.stop();
}
