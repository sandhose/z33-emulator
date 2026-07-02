// Z33 language server web worker.
//
// This is a *classic* (non-module) worker bundled as a self-contained IIFE. It
// cannot use the VS Code API. It runs the wasm-compiled tower-lsp `Backend`
// (`WasmLspServer`) and bridges it to the extension host over `postMessage`.
//
// Handshake (must complete before the LanguageClient attaches its own reader):
//   1. host → worker: { type: "init", wasmBytes: ArrayBuffer }
//   2. worker instantiates the wasm, creates the server, switches to JSON-RPC
//      mode, then posts { type: "ready" }.
//   3. host constructs the LanguageClient over this worker; from then on every
//      message is a raw JSON-RPC object (structured-cloned by the browser
//      transport).
//
// `WasmLspServer` speaks *unframed JSON strings*, whereas the browser transport
// exchanges JSON-RPC *objects*, so we JSON.stringify/parse at the boundary.

import initWasm, { WasmLspServer } from "../dist/pkg/z33_web.js";

interface InitMessage {
  type: "init";
  wasmBytes: ArrayBuffer;
}

let server: WasmLspServer | undefined;

function isInitMessage(data: unknown): data is InitMessage {
  return typeof data === "object" && data !== null && (data as { type?: unknown }).type === "init";
}

async function handleInit(message: InitMessage): Promise<void> {
  await initWasm({ module_or_path: message.wasmBytes });

  // Every server→client message arrives as a JSON string; forward it to the
  // host as a parsed JSON-RPC object for the browser transport.
  server = new WasmLspServer((json: string) => {
    postMessage(JSON.parse(json));
  });

  // Switch to JSON-RPC mode: subsequent messages are client→server payloads.
  self.onmessage = (event: MessageEvent) => {
    if (server === undefined) {
      return;
    }
    // The browser LanguageClient posts JSON-RPC message objects.
    void server.send(JSON.stringify(event.data));
  };

  postMessage({ type: "ready" });
}

// Initial listener: wait for the wasm bytes, then hand off to JSON-RPC mode.
self.onmessage = (event: MessageEvent) => {
  if (isInitMessage(event.data)) {
    handleInit(event.data).catch((error: unknown) => {
      // A rejected init never surfaces as a worker `error` event (it is a
      // handled-then-thrown async rejection), so the host would wait for a
      // `ready` that never comes. Post an explicit error frame the host treats
      // as a handshake failure.
      postMessage({
        type: "error",
        message: error instanceof Error ? error.message : String(error),
      });
    });
  }
};
