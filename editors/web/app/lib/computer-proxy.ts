// Main-thread proxy over the emulator worker.
//
// `ComputerProxy` implements the reactive read surface used by the debug UI
// (`ComputerInterface`) plus the execution controls used by the step runner.
// It keeps a snapshot cache with stable references so `useSyncExternalStore`
// works: reads are synchronous from cache, and cache updates arrive as
// worker-pushed snapshots / cell values.
// This module intentionally colocates the worker transport and the reactive
// proxy it produces; they are two halves of one abstraction.
// oxlint-disable max-classes-per-file
import type {
  Cell,
  Registers,
  ResolvedBreakpoint,
  SourcePosition,
} from "./wasm";
import type { ComputerInterface } from "../computer-types";
import type {
  RunStatus,
  Snapshot,
  WorkerRequest,
  WorkerResponse,
} from "./emulator-protocol";

const EMPTY_CELL: Cell = { type: "empty" };

interface StartResult {
  proxy: ComputerProxy;
  labels: [string, number][];
  touchedFiles: string[];
}

type PendingResolver = {
  resolve: (value: unknown) => void;
  reject: (reason: unknown) => void;
};

class EmulatorWorkerClient {
  #worker: Worker;
  #nextId = 1;
  #pending = new Map<number, PendingResolver>();
  #proxy: ComputerProxy | null = null;
  /** Set once the worker dies; every later request rejects immediately. */
  #failure: Error | null = null;

  constructor() {
    this.#worker = new Worker(
      new URL("../workers/emulator.worker.ts", import.meta.url),
      { type: "module", name: "z33-emulator" },
    );
    this.#worker.addEventListener(
      "message",
      (event: MessageEvent<WorkerResponse>) => {
        this.#onMessage(event.data);
      },
    );
    // A hard script failure never surfaces as a message; catch it too.
    this.#worker.addEventListener("error", (event: ErrorEvent) => {
      this.#fail(new Error(`Emulator worker error: ${event.message}`));
    });
  }

  #onMessage(message: WorkerResponse): void {
    switch (message.type) {
      case "snapshot":
        this.#proxy?.applySnapshot(message.snapshot);
        return;
      case "cells":
        this.#proxy?.applyCells(message.cells);
        return;
      case "started": {
        this.#pending.get(message.id)?.resolve(message);
        this.#pending.delete(message.id);
        return;
      }
      case "startError": {
        this.#pending.get(message.id)?.reject(new Error(message.error));
        this.#pending.delete(message.id);
        return;
      }
      case "resolved": {
        this.#pending.get(message.id)?.resolve(message.resolved);
        this.#pending.delete(message.id);
        return;
      }
      case "workerError": {
        this.#fail(new Error(message.message));
      }
    }
  }

  /**
   * Mark the worker as dead: reject every in-flight request and push a panicked
   * state to the live proxy (if any) so the UI reflects the failure instead of
   * silently stalling.
   */
  #fail(error: Error): void {
    if (this.#failure) return;
    this.#failure = error;
    console.error("[emulator-proxy]", error);
    for (const pending of this.#pending.values()) pending.reject(error);
    this.#pending.clear();
    this.#proxy?.applyWorkerError(error.message);
  }

  send(message: WorkerRequest): void {
    // oxlint-disable-next-line unicorn/require-post-message-target-origin -- Worker.postMessage takes no targetOrigin
    this.#worker.postMessage(message);
  }

  #request<T>(build: (id: number) => WorkerRequest): Promise<T> {
    if (this.#failure) return Promise.reject(this.#failure);
    const id = this.#nextId++;
    return new Promise<T>((resolve, reject) => {
      this.#pending.set(id, {
        // Worker responses are untyped at this boundary; the typed request
        // wrappers guarantee the value shape.
        // oxlint-disable-next-line typescript/no-unsafe-type-assertion
        resolve: resolve as PendingResolver["resolve"],
        reject,
      });
      this.send(build(id));
    });
  }

  async start(
    files: Record<string, string>,
    rootFile: string,
    entrypoint: string,
  ): Promise<StartResult> {
    // Tear down any previous session before starting a new one.
    this.#proxy?.dispose();

    const response = await this.#request<
      Extract<WorkerResponse, { type: "started" }>
    >((id) => ({ id, type: "start", files, rootFile, entrypoint }));

    const proxy = new ComputerProxy(this, response.labels, response.snapshot);
    this.#proxy = proxy;
    return {
      proxy,
      labels: response.labels,
      touchedFiles: response.touchedFiles,
    };
  }

  resolveBreakpoint(
    file: string,
    line: number,
  ): Promise<ResolvedBreakpoint | null> {
    return this.#request<ResolvedBreakpoint | null>((id) => ({
      id,
      type: "resolveBreakpoint",
      file,
      line,
    }));
  }

  detach(proxy: ComputerProxy): void {
    if (this.#proxy === proxy) this.#proxy = null;
  }
}

let client: EmulatorWorkerClient | null = null;
function getClient(): EmulatorWorkerClient {
  client ??= new EmulatorWorkerClient();
  return client;
}

/** Start a debug session in the emulator worker. */
export function startSession(
  files: Record<string, string>,
  rootFile: string,
  entrypoint: string,
): Promise<StartResult> {
  return getClient().start(files, rootFile, entrypoint);
}

type Unsubscribe = () => void;

export class ComputerProxy implements ComputerInterface {
  readonly labels: [string, number][];

  #client: EmulatorWorkerClient;
  #registers: Registers;
  #cycles: number;
  #status: RunStatus;
  #error: string | null = null;
  #pcLocation: SourcePosition | null;
  #cells = new Map<number, Cell>();

  #registerSubs = new Set<(r: Registers) => void>();
  #cycleSubs = new Set<(c: number) => void>();
  #statusSubs = new Set<(s: RunStatus) => void>();
  #pcSubs = new Set<() => void>();
  #cellSubs = new Map<number, Set<(c: Cell) => void>>();

  constructor(
    workerClient: EmulatorWorkerClient,
    labels: [string, number][],
    snapshot: Snapshot,
  ) {
    this.#client = workerClient;
    this.labels = labels;
    this.#registers = snapshot.registers;
    this.#cycles = snapshot.cycles;
    this.#status = snapshot.status;
    this.#pcLocation = snapshot.location;
  }

  // --- reactive reads (ComputerInterface) --------------------------------

  registers(): Registers {
    return this.#registers;
  }

  cycles(): number {
    return this.#cycles;
  }

  memory(address: number): Cell {
    return this.#cells.get(address) ?? EMPTY_CELL;
  }

  subscribe_registers(cb: (r: Registers) => void): Unsubscribe {
    this.#registerSubs.add(cb);
    return () => {
      this.#registerSubs.delete(cb);
    };
  }

  subscribe_cycles(cb: (c: number) => void): Unsubscribe {
    this.#cycleSubs.add(cb);
    return () => {
      this.#cycleSubs.delete(cb);
    };
  }

  subscribe_memory(address: number, cb: (c: Cell) => void): Unsubscribe {
    let set = this.#cellSubs.get(address);
    if (!set) {
      set = new Set();
      this.#cellSubs.set(address, set);
      this.#client.send({ type: "watchCells", addresses: [address] });
    }
    set.add(cb);
    return () => {
      const current = this.#cellSubs.get(address);
      if (!current) return;
      current.delete(cb);
      if (current.size === 0) {
        this.#cellSubs.delete(address);
        this.#client.send({ type: "unwatchCells", addresses: [address] });
      }
    };
  }

  // --- execution controls -------------------------------------------------

  step(n = 1): void {
    this.#client.send({ type: "step", n });
  }

  run(): void {
    this.#client.send({ type: "run" });
  }

  pause(): void {
    this.#client.send({ type: "pause" });
  }

  setBreakpoints(addresses: number[]): void {
    this.#client.send({ type: "setBreakpoints", addresses });
  }

  setSpeed(speed: number | null): void {
    this.#client.send({ type: "setSpeed", speed });
  }

  resolveBreakpoint(
    file: string,
    line: number,
  ): Promise<ResolvedBreakpoint | null> {
    return this.#client.resolveBreakpoint(file, line);
  }

  getStatus(): RunStatus {
    return this.#status;
  }

  getError(): string | null {
    return this.#error;
  }

  subscribeStatus(cb: (s: RunStatus) => void): Unsubscribe {
    this.#statusSubs.add(cb);
    return () => {
      this.#statusSubs.delete(cb);
    };
  }

  getPcLocation(): SourcePosition | null {
    return this.#pcLocation;
  }

  subscribePc(cb: () => void): Unsubscribe {
    this.#pcSubs.add(cb);
    return () => {
      this.#pcSubs.delete(cb);
    };
  }

  dispose(): void {
    this.#client.send({ type: "stop" });
    this.#client.detach(this);
  }

  // --- worker push handlers ----------------------------------------------

  applySnapshot(snapshot: Snapshot): void {
    this.#registers = snapshot.registers;
    for (const cb of this.#registerSubs) cb(this.#registers);

    if (snapshot.cycles !== this.#cycles) {
      this.#cycles = snapshot.cycles;
      for (const cb of this.#cycleSubs) cb(this.#cycles);
    }

    this.#pcLocation = snapshot.location;
    for (const cb of this.#pcSubs) cb();

    if (snapshot.status !== this.#status) {
      this.#status = snapshot.status;
      this.#error =
        snapshot.status === "panicked" ? (snapshot.error ?? "panicked") : null;
      for (const cb of this.#statusSubs) cb(this.#status);
    }

    this.applyCells(snapshot.changedCells);
  }

  applyCells(cells: [number, Cell][]): void {
    for (const [address, cell] of cells) {
      this.#cells.set(address, cell);
      const subs = this.#cellSubs.get(address);
      if (subs) for (const cb of subs) cb(cell);
    }
  }

  /**
   * Drop into a panicked state after a fatal worker failure. Reuses the same
   * status/error channel as a program panic so the UI needs no special case.
   */
  applyWorkerError(message: string): void {
    if (this.#status === "panicked") return;
    this.#status = "panicked";
    this.#error = message;
    for (const cb of this.#statusSubs) cb(this.#status);
  }
}
