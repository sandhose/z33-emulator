import { afterEach, describe, expect, it, vi } from "vitest";
import { FakeWorker, installFakeWorker } from "../testing/fake-worker";
import type { Cell, Registers } from "./wasm";
import type { Snapshot, WorkerRequest } from "./emulator-protocol";

// The proxy asks for the compiled binary in its constructor and forwards it to
// the worker; nothing here instantiates it.
const WASM_MODULE = { fake: "module" };
vi.mock("./wasm-module", () => ({
  compiledWasmModule: () => Promise.resolve(WASM_MODULE),
}));

type ProxyModule = typeof import("./computer-proxy");

const REGISTERS: Registers = {
  a: { type: "empty" },
  b: { type: "empty" },
  pc: 1000,
  sp: 10_000,
  sr: 0,
};

const word = (value: number): Cell => ({ type: "word", word: value });

/** A callback the proxy invokes, typed as the void return it expects. */
const listener = () => vi.fn((..._args: unknown[]): void => {});

function snapshot(overrides: Partial<Snapshot> = {}): Snapshot {
  return {
    registers: REGISTERS,
    cycles: 0,
    changedCells: [],
    status: "paused",
    pc: 1000,
    location: null,
    output: [],
    ...overrides,
  };
}

/**
 * A fresh copy of the module: it keeps one worker client in a module-level
 * singleton, and a client that has failed stays failed.
 */
function freshModule(): Promise<ProxyModule> {
  vi.resetModules();
  installFakeWorker();
  return import("./computer-proxy");
}

/**
 * The worker's outbox as the protocol it carries. `FakeWorker` is
 * protocol-agnostic, so the shape is asserted once, here.
 */
function sent(worker: FakeWorker): WorkerRequest[] {
  // oxlint-disable-next-line typescript/no-unsafe-type-assertion
  return worker.sent as WorkerRequest[];
}

/** The request ids the client assigns are private; read them off the wire. */
function idOf(request: WorkerRequest): number {
  if (!("id" in request)) throw new Error("message carries no request id");
  return request.id;
}

/**
 * Wait for the client to post a request of this type and hand it back. The
 * binary arrives asynchronously, so the `init` message can land either side of
 * a request that was issued synchronously.
 */
function sentRequest(
  worker: FakeWorker,
  type: WorkerRequest["type"],
): Promise<WorkerRequest> {
  return vi.waitFor(() => {
    const found = sent(worker).find((message) => message.type === type);
    if (!found) throw new Error(`no ${type} request was posted`);
    return found;
  });
}

/** Start a session and return the worker plus the proxy it produced. */
async function startAndAwaitSession(startSession: ProxyModule["startSession"]) {
  const pending = startSession({ "a.s": "nop" }, "a.s", "main");
  const worker = FakeWorker.last();
  const request = await sentRequest(worker, "start");
  worker.respond({
    id: idOf(request),
    type: "started",
    labels: [["main", 1000]],
    touchedFiles: ["a.s"],
    snapshot: snapshot(),
  });
  const { proxy, labels, touchedFiles } = await pending;
  return { worker, proxy, labels, touchedFiles };
}

afterEach(() => {
  vi.restoreAllMocks();
});

describe("worker startup", () => {
  it("spawns one module worker and hands it the compiled binary", async () => {
    const { checkProgram } = await freshModule();
    void checkProgram({ "a.s": "nop" }, "a.s");

    const worker = FakeWorker.last();
    expect(worker.scriptUrl).toContain("emulator.worker");
    await vi.waitFor(() => {
      expect(worker.sent).toContainEqual({
        type: "init",
        module: WASM_MODULE,
      });
    });
  });

  it("reuses the same worker across sessions", async () => {
    const { checkProgram } = await freshModule();
    void checkProgram({ "a.s": "nop" }, "a.s");
    void checkProgram({ "a.s": "nop" }, "a.s");
    expect(FakeWorker.instances).toHaveLength(1);
  });
});

describe("request / response", () => {
  it("resolves a check with the worker's result", async () => {
    const { checkProgram } = await freshModule();
    const pending = checkProgram({ "a.s": "nop" }, "a.s");
    const worker = FakeWorker.last();

    const request = await sentRequest(worker, "check");
    expect(request).toMatchObject({
      type: "check",
      files: { "a.s": "nop" },
      rootFile: "a.s",
    });

    worker.respond({
      id: idOf(request),
      type: "checked",
      result: { type: "success", labels: ["main"] },
    });
    await expect(pending).resolves.toEqual({
      type: "success",
      labels: ["main"],
    });
  });

  it("gives each request its own id and settles them independently", async () => {
    const { checkProgram } = await freshModule();
    const first = checkProgram({ "a.s": "" }, "a.s");
    const second = checkProgram({ "b.s": "" }, "b.s");
    const worker = FakeWorker.last();

    const requests = await vi.waitFor(() => {
      const found = sent(worker).filter((message) => "id" in message);
      if (found.length !== 2) throw new Error("expected two requests");
      return found;
    });
    const [one, two] = requests;
    if (!one || !two) throw new Error("expected two requests");
    expect(idOf(one)).not.toBe(idOf(two));

    // Answering out of order still settles the right promise.
    worker.respond({
      id: idOf(two),
      type: "checked",
      result: { type: "error", message: "boom", labels: [] },
    });
    worker.respond({
      id: idOf(one),
      type: "checked",
      result: { type: "success", labels: [] },
    });
    await expect(first).resolves.toMatchObject({ type: "success" });
    await expect(second).resolves.toMatchObject({ message: "boom" });
  });

  it("rejects a session the worker could not compile", async () => {
    const { startSession } = await freshModule();
    const pending = startSession({ "a.s": "?" }, "a.s", "main");
    const worker = FakeWorker.last();
    const request = await sentRequest(worker, "start");

    worker.respond({
      id: idOf(request),
      type: "startError",
      error: "unknown instruction",
    });
    await expect(pending).rejects.toThrow("unknown instruction");
  });

  it("resolves a breakpoint through the running session", async () => {
    const { startSession } = await freshModule();
    const { worker, proxy } = await startAndAwaitSession(startSession);

    const pending = proxy.resolveBreakpoint("a.s", 3);
    const request = await sentRequest(worker, "resolveBreakpoint");
    worker.respond({
      id: idOf(request),
      type: "resolved",
      resolved: { line: 4, address: 1004 },
    });
    await expect(pending).resolves.toEqual({ line: 4, address: 1004 });
  });
});

describe("session state", () => {
  it("carries the labels and touched files out of the started frame", async () => {
    const { startSession } = await freshModule();
    const { labels, touchedFiles, proxy } =
      await startAndAwaitSession(startSession);
    expect(labels).toEqual([["main", 1000]]);
    expect(touchedFiles).toEqual(["a.s"]);
    expect(proxy.getStatus()).toBe("paused");
    expect(proxy.registers()).toEqual(REGISTERS);
  });

  it("notifies subscribers of a pushed snapshot", async () => {
    const { startSession } = await freshModule();
    const { worker, proxy } = await startAndAwaitSession(startSession);

    const registers = listener();
    const cycles = listener();
    const status = listener();
    const output = listener();
    proxy.subscribe_registers(registers);
    proxy.subscribe_cycles(cycles);
    proxy.subscribeStatus(status);
    proxy.onOutput(output);

    worker.respond({
      type: "snapshot",
      snapshot: snapshot({
        cycles: 12,
        status: "running",
        output: [72, 105],
        registers: { ...REGISTERS, pc: 1004 },
      }),
    });

    expect(registers).toHaveBeenCalledWith({ ...REGISTERS, pc: 1004 });
    expect(cycles).toHaveBeenCalledWith(12);
    expect(status).toHaveBeenCalledWith("running");
    expect(output).toHaveBeenCalledWith([72, 105]);
    expect(proxy.cycles()).toBe(12);
    expect(proxy.getStatus()).toBe("running");
  });

  it("holds the cycle count and status steady when they do not move", async () => {
    const { startSession } = await freshModule();
    const { worker, proxy } = await startAndAwaitSession(startSession);
    const cycles = listener();
    const status = listener();
    proxy.subscribe_cycles(cycles);
    proxy.subscribeStatus(status);

    worker.respond({ type: "snapshot", snapshot: snapshot() });
    expect(cycles).not.toHaveBeenCalled();
    expect(status).not.toHaveBeenCalled();
  });

  it("keeps the panic message off a snapshot that is not a panic", async () => {
    const { startSession } = await freshModule();
    const { worker, proxy } = await startAndAwaitSession(startSession);

    worker.respond({
      type: "snapshot",
      snapshot: snapshot({ status: "panicked", error: "divide by zero" }),
    });
    expect(proxy.getError()).toBe("divide by zero");

    worker.respond({
      type: "snapshot",
      snapshot: snapshot({ status: "paused" }),
    });
    expect(proxy.getError()).toBeNull();
  });

  it("defaults a panic with no message", async () => {
    const { startSession } = await freshModule();
    const { worker, proxy } = await startAndAwaitSession(startSession);
    worker.respond({
      type: "snapshot",
      snapshot: snapshot({ status: "panicked" }),
    });
    expect(proxy.getError()).toBe("panicked");
  });
});

describe("watched memory", () => {
  it("watches an address on the first subscriber and drops it on the last", async () => {
    const { startSession } = await freshModule();
    const { worker, proxy } = await startAndAwaitSession(startSession);
    const watchTraffic = () =>
      sent(worker).filter(
        (m) => m.type === "watchCells" || m.type === "unwatchCells",
      );

    const unsubscribeFirst = proxy.subscribe_memory(1000, listener());
    const unsubscribeSecond = proxy.subscribe_memory(1000, listener());
    expect(watchTraffic()).toEqual([{ type: "watchCells", addresses: [1000] }]);

    unsubscribeFirst();
    expect(watchTraffic()).toHaveLength(1);

    unsubscribeSecond();
    expect(watchTraffic().at(-1)).toEqual({
      type: "unwatchCells",
      addresses: [1000],
    });
  });

  it("reads an unwatched address as an empty cell", async () => {
    const { startSession } = await freshModule();
    const { proxy } = await startAndAwaitSession(startSession);
    expect(proxy.memory(4242)).toEqual({ type: "empty" });
  });

  it("caches pushed cells and notifies that address only", async () => {
    const { startSession } = await freshModule();
    const { worker, proxy } = await startAndAwaitSession(startSession);
    const watcher = listener();
    proxy.subscribe_memory(1000, watcher);

    worker.respond({
      type: "cells",
      cells: [
        [1000, word(7)],
        [1001, word(8)],
      ],
    });

    expect(watcher).toHaveBeenCalledExactlyOnceWith(word(7));
    expect(proxy.memory(1000)).toEqual(word(7));
    expect(proxy.memory(1001)).toEqual(word(8));
  });

  it("delivers the cells a snapshot changed", async () => {
    const { startSession } = await freshModule();
    const { worker, proxy } = await startAndAwaitSession(startSession);
    const watcher = listener();
    proxy.subscribe_memory(1000, watcher);

    worker.respond({
      type: "snapshot",
      snapshot: snapshot({ changedCells: [[1000, word(9)]] }),
    });
    expect(watcher).toHaveBeenCalledWith(word(9));
  });

  it("stops notifying an unsubscribed watcher", async () => {
    const { startSession } = await freshModule();
    const { worker, proxy } = await startAndAwaitSession(startSession);
    const watcher = listener();
    proxy.subscribe_memory(1000, watcher)();

    worker.respond({ type: "cells", cells: [[1000, word(7)]] });
    expect(watcher).not.toHaveBeenCalled();
  });
});

describe("execution controls", () => {
  it("forwards each control to the worker", async () => {
    const { startSession } = await freshModule();
    const { worker, proxy } = await startAndAwaitSession(startSession);
    const before = worker.sent.length;

    proxy.step();
    proxy.step(5);
    proxy.run();
    proxy.pause();
    proxy.setSpeed(100);
    proxy.setBreakpoints([1000, 1004]);
    proxy.sendInput([65]);
    proxy.dispose();

    expect(sent(worker).slice(before)).toEqual([
      { type: "step", n: 1 },
      { type: "step", n: 5 },
      { type: "run" },
      { type: "pause" },
      { type: "setSpeed", speed: 100 },
      { type: "setBreakpoints", addresses: [1000, 1004] },
      { type: "input", bytes: [65] },
      { type: "stop" },
    ]);
  });

  it("stops pushing state into a disposed proxy", async () => {
    const { startSession } = await freshModule();
    const { worker, proxy } = await startAndAwaitSession(startSession);
    const cycles = listener();
    proxy.subscribe_cycles(cycles);
    proxy.dispose();

    worker.respond({ type: "snapshot", snapshot: snapshot({ cycles: 99 }) });
    expect(cycles).not.toHaveBeenCalled();
    expect(proxy.cycles()).toBe(0);
  });
});

describe("worker failure", () => {
  it("rejects every in-flight request on a worker error frame", async () => {
    vi.spyOn(console, "error").mockImplementation(() => {});
    const { checkProgram } = await freshModule();
    const pending = checkProgram({ "a.s": "" }, "a.s");
    const worker = FakeWorker.last();
    await sentRequest(worker, "check");

    worker.respond({ type: "workerError", message: "wasm init failed" });
    await expect(pending).rejects.toThrow("wasm init failed");
  });

  it("rejects a later request immediately, without posting it", async () => {
    vi.spyOn(console, "error").mockImplementation(() => {});
    const { checkProgram } = await freshModule();
    void checkProgram({ "a.s": "" }, "a.s").catch(() => {});
    const worker = FakeWorker.last();
    await sentRequest(worker, "check");

    worker.respond({ type: "workerError", message: "wasm init failed" });
    const after = worker.sent.length;
    await expect(checkProgram({ "a.s": "" }, "a.s")).rejects.toThrow(
      "wasm init failed",
    );
    expect(worker.sent).toHaveLength(after);
  });

  it("turns a hard script failure into the same rejection", async () => {
    vi.spyOn(console, "error").mockImplementation(() => {});
    const { checkProgram } = await freshModule();
    const pending = checkProgram({ "a.s": "" }, "a.s");
    const worker = FakeWorker.last();
    await sentRequest(worker, "check");

    worker.crash("Unexpected token");
    await expect(pending).rejects.toThrow(
      "Emulator worker error: Unexpected token",
    );
  });

  it("drops a live session into a panicked state", async () => {
    vi.spyOn(console, "error").mockImplementation(() => {});
    const { startSession } = await freshModule();
    const { worker, proxy } = await startAndAwaitSession(startSession);
    const status = listener();
    proxy.subscribeStatus(status);

    worker.respond({ type: "workerError", message: "worker died" });
    expect(status).toHaveBeenCalledWith("panicked");
    expect(proxy.getStatus()).toBe("panicked");
    expect(proxy.getError()).toBe("worker died");
  });

  it("reports the failure once, however many frames arrive", async () => {
    const logged = vi.spyOn(console, "error").mockImplementation(() => {});
    const { checkProgram } = await freshModule();
    void checkProgram({ "a.s": "" }, "a.s").catch(() => {});
    const worker = FakeWorker.last();
    await sentRequest(worker, "check");

    worker.respond({ type: "workerError", message: "first" });
    worker.crash("second");
    expect(logged).toHaveBeenCalledOnce();
  });
});
