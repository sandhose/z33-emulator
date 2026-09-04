// Web worker hosting the WASM Z33 language server.
//
// The main thread talks to this worker with a `vscode-jsonrpc`
// `MessageConnection`. Here we do NOT run a second JSON-RPC layer: the
// `WasmLspServer` *is* the JSON-RPC endpoint. We simply relay raw message
// objects between the transport and the server:
//   - messages arriving from the main thread  -> `server.send(JSON.stringify)`
//   - messages the server emits (`on_message`) -> posted back as parsed objects
//
// Messages received before the wasm has finished initializing are queued.
import "../lib/dispose-polyfill";
import {
  BrowserMessageReader,
  BrowserMessageWriter,
  type Message,
} from "vscode-jsonrpc/browser";
import init, { WasmLspServer } from "../../pkg/z33_web.js";
import { WORKER_ERROR, isWorkerInitFrame } from "./worker-protocol";

const reader = new BrowserMessageReader(self);
const writer = new BrowserMessageWriter(self);

let server: WasmLspServer | null = null;
const inbox: Message[] = [];

// `server.send` is synchronous: it fully processes the message (emitting any
// responses via `on_message`) before returning, so calls are naturally
// serialized without any queueing.
function forwardToServer(message: Message): void {
  if (!server) return;
  try {
    server.send(JSON.stringify(message));
  } catch (error) {
    console.error("[lsp.worker] server.send failed", error);
  }
}

reader.listen((message) => {
  if (isWorkerInitFrame(message)) {
    start(message.module);
    return;
  }
  if (server) {
    forwardToServer(message);
  } else {
    inbox.push(message);
  }
});

async function main(module: WebAssembly.Module): Promise<void> {
  await init({ module_or_path: module });

  server = new WasmLspServer((json: string) => {
    try {
      const parsed: Message = JSON.parse(json);
      void writer.write(parsed);
    } catch (error) {
      console.error("[lsp.worker] failed to post server message", error);
    }
  });

  // Drain anything received while initializing.
  for (const message of inbox.splice(0)) forwardToServer(message);
}

// The compiled module arrives as a message, so init cannot be awaited at module
// scope; and a rejected top-level await would never reach the main thread, which
// is what the catch below exists to prevent. Same shape in emulator.worker.ts.
function start(module: WebAssembly.Module): void {
  main(module).catch((error) => {
    // If wasm init fails there is no server, so the JSON-RPC handshake on the
    // main thread would await forever. Surface it as a console error plus a
    // dedicated error frame the client recognizes and turns into a rejection.
    console.error("[lsp.worker] failed to initialize", error);
    // In a dedicated worker `self.postMessage` takes no targetOrigin (that is the
    // window signature); the rule misfires here.
    // oxlint-disable unicorn/require-post-message-target-origin
    self.postMessage({
      type: WORKER_ERROR,
      message: error instanceof Error ? error.message : String(error),
    });
    // oxlint-enable unicorn/require-post-message-target-origin
  });
}
