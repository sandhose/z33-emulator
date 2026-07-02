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
import { includeWorkspaceFolderInPaths } from "./workspace-paths.js";

const WORKSPACE_FILES_METHOD = "z33/workspaceFiles";
const FILE_GLOB = "**/*.{s,S}";
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

/** Collect every workspace `.s`/`.S` file as a relative-path → content map. */
async function collectWorkspaceFiles(): Promise<Record<string, string>> {
  const files: Record<string, string> = {};
  const uris = await vscode.workspace.findFiles(FILE_GLOB);
  const decoder = new TextDecoder();
  const includeFolder = includeWorkspaceFolderInPaths();
  for (const uri of uris) {
    const relative = vscode.workspace.asRelativePath(uri, includeFolder);
    const bytes = await vscode.workspace.fs.readFile(uri);
    files[relative] = decoder.decode(bytes);
  }
  return files;
}

async function pushWorkspaceFiles(lsp: LanguageClient): Promise<void> {
  const files = await collectWorkspaceFiles();
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
    ],
  };

  client = new LanguageClient("z33-assembly", "Z33 Language Server", worker, clientOptions);
  await client.start();

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
