import { afterEach, describe, expect, it, vi } from "vitest";
import { FakeWorker, installFakeWorker } from "../testing/fake-worker";
import { WORKER_ERROR, WORKER_INIT } from "../workers/worker-protocol";

const WASM_MODULE = { fake: "module" };
vi.mock("./wasm-module", () => ({
  compiledWasmModule: () => Promise.resolve(WASM_MODULE),
}));

const LEGEND = { tokenTypes: ["keyword"], tokenModifiers: ["declaration"] };

interface JsonRpcFrame {
  jsonrpc: string;
  id?: number | string;
  method?: string;
  params?: unknown;
}

/** A fresh copy of the module: it keeps the client in a module-level singleton. */
function freshModule(): Promise<typeof import("./lsp-client")> {
  vi.resetModules();
  installFakeWorker();
  return import("./lsp-client");
}

/** Everything the client has written to the JSON-RPC channel. */
function frames(worker: FakeWorker): JsonRpcFrame[] {
  // oxlint-disable-next-line typescript/no-unsafe-type-assertion
  return worker.sent.filter(
    (message) =>
      typeof message === "object" && message !== null && "jsonrpc" in message,
  ) as JsonRpcFrame[];
}

/** Wait for the client to send this JSON-RPC method and hand back the frame. */
function sentFrame(worker: FakeWorker, method: string): Promise<JsonRpcFrame> {
  return vi.waitFor(() => {
    const found = frames(worker).find((frame) => frame.method === method);
    if (!found) throw new Error(`no ${method} frame was sent`);
    return found;
  });
}

/** Answer the client's `initialize` so its readiness promise resolves. */
async function completeHandshake(worker: FakeWorker): Promise<void> {
  const initialize = await sentFrame(worker, "initialize");
  worker.respond({
    jsonrpc: "2.0",
    id: initialize.id,
    result: { capabilities: { semanticTokensProvider: { legend: LEGEND } } },
  });
  await sentFrame(worker, "initialized");
}

afterEach(() => {
  vi.restoreAllMocks();
  vi.useRealTimers();
});

describe("startup", () => {
  it("hands the worker the compiled binary and opens the handshake", async () => {
    const { getLspClient } = await freshModule();
    getLspClient();
    const worker = FakeWorker.last();

    expect(worker.scriptUrl).toContain("lsp.worker");
    await vi.waitFor(() => {
      expect(worker.sent).toContainEqual({
        type: WORKER_INIT,
        module: WASM_MODULE,
      });
    });

    const initialize = await sentFrame(worker, "initialize");
    expect(initialize.params).toMatchObject({
      rootUri: null,
      capabilities: { experimental: { commands: ["zorglub33.run"] } },
    });
  });

  it("returns the same client on every call", async () => {
    const { getLspClient } = await freshModule();
    expect(getLspClient()).toBe(getLspClient());
    expect(FakeWorker.instances).toHaveLength(1);
  });

  it("takes the semantic-tokens legend from the initialize result", async () => {
    const { getLspClient } = await freshModule();
    const client = getLspClient();
    expect(client.legend).toBeNull();

    await completeHandshake(FakeWorker.last());
    await client.ready();
    expect(client.legend).toEqual(LEGEND);
  });

  it("leaves the legend unset when the server offers no semantic tokens", async () => {
    const { getLspClient } = await freshModule();
    const client = getLspClient();
    const worker = FakeWorker.last();
    const initialize = await sentFrame(worker, "initialize");
    worker.respond({
      jsonrpc: "2.0",
      id: initialize.id,
      result: { capabilities: {} },
    });

    await client.ready();
    expect(client.legend).toBeNull();
  });
});

describe("notifications", () => {
  it("holds a notification sent before the handshake finishes", async () => {
    const { getLspClient } = await freshModule();
    const client = getLspClient();
    const worker = FakeWorker.last();

    client.notify("zorglub33/workspaceFiles", { files: { "a.s": "nop" } });
    expect(
      frames(worker).some((f) => f.method === "zorglub33/workspaceFiles"),
    ).toBe(false);

    await completeHandshake(worker);
    const held = await sentFrame(worker, "zorglub33/workspaceFiles");
    expect(held.params).toEqual({ files: { "a.s": "nop" } });
  });

  it("fans a diagnostics notification out to its listeners", async () => {
    const { getLspClient } = await freshModule();
    const client = getLspClient();
    const worker = FakeWorker.last();
    await completeHandshake(worker);

    const first = vi.fn((..._args: unknown[]): void => {});
    const second = vi.fn((..._args: unknown[]): void => {});
    client.onDiagnostics(first);
    const unsubscribe = client.onDiagnostics(second);
    unsubscribe();

    worker.respond({
      jsonrpc: "2.0",
      method: "textDocument/publishDiagnostics",
      params: { uri: "file:///a.s", diagnostics: [{ message: "boom" }] },
    });

    await vi.waitFor(() => {
      expect(first).toHaveBeenCalledWith("file:///a.s", [{ message: "boom" }]);
    });
    expect(second).not.toHaveBeenCalled();
  });
});

describe("worker failure", () => {
  it("rejects readiness on the worker's error frame", async () => {
    vi.spyOn(console, "error").mockImplementation(() => {});
    const { getLspClient } = await freshModule();
    const client = getLspClient();

    FakeWorker.last().respond({
      type: WORKER_ERROR,
      message: "wasm init failed",
    });
    await expect(client.ready()).rejects.toThrow(
      "LSP worker failed to start: wasm init failed",
    );
    await expect(client.request("textDocument/hover", {})).rejects.toThrow(
      "wasm init failed",
    );
  });

  it("rejects readiness on a hard script failure", async () => {
    vi.spyOn(console, "error").mockImplementation(() => {});
    const { getLspClient } = await freshModule();
    const client = getLspClient();

    FakeWorker.last().crash("Unexpected token");
    await expect(client.ready()).rejects.toThrow("Unexpected token");
  });

  it("drops a notification that can no longer be delivered", async () => {
    vi.spyOn(console, "error").mockImplementation(() => {});
    const { getLspClient } = await freshModule();
    const client = getLspClient();
    const worker = FakeWorker.last();

    worker.respond({ type: WORKER_ERROR, message: "wasm init failed" });
    await expect(client.ready()).rejects.toThrow("wasm init failed");

    client.notify("zorglub33/workspaceFiles", { files: {} });
    // `notify` defers to the readiness promise, so give both of its handlers a
    // turn before concluding that nothing was written.
    await Promise.resolve();
    await Promise.resolve();
    expect(
      frames(worker).some((f) => f.method === "zorglub33/workspaceFiles"),
    ).toBe(false);
  });

  it("reports the failure once, however many times the worker dies", async () => {
    const logged = vi.spyOn(console, "error").mockImplementation(() => {});
    const { getLspClient } = await freshModule();
    const client = getLspClient();
    const worker = FakeWorker.last();

    worker.respond({ type: WORKER_ERROR, message: "first" });
    worker.crash("second");
    await expect(client.ready()).rejects.toThrow("first");
    expect(logged).toHaveBeenCalledOnce();
  });

  it("gives up on a handshake that never completes", async () => {
    vi.spyOn(console, "error").mockImplementation(() => {});
    vi.useFakeTimers();
    const { getLspClient } = await freshModule();
    const client = getLspClient();
    const readiness = expect(client.ready()).rejects.toThrow(
      "LSP handshake timed out after 30000ms",
    );

    // The budget starts once the binary has been handed to the worker, which
    // the mocked `compiledWasmModule` settles a microtask later.
    await vi.advanceTimersByTimeAsync(0);
    await vi.advanceTimersByTimeAsync(30_000);
    await readiness;
  });
});
