import { describe, expect, it } from "vitest";
import {
  WORKER_ERROR,
  WORKER_INIT,
  isWorkerErrorFrame,
  isWorkerInitFrame,
} from "./worker-protocol";

// The LSP worker's channel also carries JSON-RPC traffic, so both guards run
// over frames they are meant to say no to.
const JSON_RPC_FRAMES = [
  { jsonrpc: "2.0", id: 1, method: "initialize", params: {} },
  { jsonrpc: "2.0", id: 1, result: {} },
  { jsonrpc: "2.0", method: "textDocument/publishDiagnostics", params: {} },
];

describe("isWorkerInitFrame", () => {
  it("accepts the init frame", () => {
    expect(isWorkerInitFrame({ type: WORKER_INIT, module: {} })).toBe(true);
  });

  it("rejects JSON-RPC traffic and other frames", () => {
    for (const frame of [
      ...JSON_RPC_FRAMES,
      { type: WORKER_ERROR, message: "boom" },
      { type: "init" },
      null,
      undefined,
      WORKER_INIT,
      42,
    ]) {
      expect(isWorkerInitFrame(frame)).toBe(false);
    }
  });
});

describe("isWorkerErrorFrame", () => {
  it("accepts the error sentinel", () => {
    expect(isWorkerErrorFrame({ type: WORKER_ERROR, message: "boom" })).toBe(
      true,
    );
  });

  it("rejects JSON-RPC traffic and other frames", () => {
    for (const frame of [
      ...JSON_RPC_FRAMES,
      { type: WORKER_INIT, module: {} },
      { type: "workerError", message: "boom" },
      null,
      undefined,
      WORKER_ERROR,
    ]) {
      expect(isWorkerErrorFrame(frame)).toBe(false);
    }
  });
});
