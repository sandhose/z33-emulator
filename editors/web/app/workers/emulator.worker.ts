// Web worker that owns Z33 compilation and the running `Computer`.
//
// The main thread drives execution through the message protocol in
// `../lib/emulator-protocol`. Continuous "run" is executed here in bounded
// batches (via the wasm `run_batch`), yielding to the event loop between
// batches so `pause` / `setBreakpoints` messages are processed promptly.
import type {
  RunStatus,
  Snapshot,
  WorkerRequest,
  WorkerResponse,
} from "../lib/emulator-protocol";
import init, {
  type Cell,
  Computer,
  InMemoryPreprocessor,
  type SourceIndex,
} from "../../pkg/z33_web.js";
import wasmUrl from "../../pkg/z33_web_bg.wasm?url";

/** Instructions per batch; tuned so pause latency stays well under a frame. */
const BATCH_SIZE = 50_000;
/** Minimum interval between pushed snapshots while running (ms). */
const SNAPSHOT_INTERVAL_MS = 40;

const ready = init({ module_or_path: wasmUrl });

// If wasm init fails, `ready.then` in the message handler never runs, so every
// request would hang. Surface it once as a fatal error frame; the proxy rejects
// its pending requests and drops into a panicked state. This must stay a
// `.catch` (not a top-level await): a rejected top-level await dies silently
// without ever posting the frame.
// oxlint-disable-next-line unicorn/prefer-top-level-await
ready.catch((error: unknown) => {
  console.error("[emulator.worker] failed to initialize", error);
  post({
    type: "workerError",
    message: error instanceof Error ? error.message : String(error),
  });
});

let computer: Computer | null = null;
let sourceIndex: SourceIndex | null = null;
let running = false;
let breakpoints: number[] = [];
const watched = new Map<number, () => void>();
const pendingCells = new Map<number, Cell>();
let lastSnapshotAt = 0;

function post(message: WorkerResponse): void {
  // oxlint-disable-next-line unicorn/require-post-message-target-origin -- Worker.postMessage takes no targetOrigin
  self.postMessage(message);
}

function statusFromBatch(status: string): RunStatus {
  switch (status) {
    case "halted":
      return "halted";
    case "panicked":
      return "panicked";
    case "breakpoint":
      return "paused";
    default:
      return "running";
  }
}

function buildSnapshot(status: RunStatus, error?: string): Snapshot {
  if (!computer) throw new Error("no active computer");
  const registers = computer.registers();
  const pc = registers.pc;
  const changedCells = [...pendingCells.entries()];
  pendingCells.clear();
  return {
    registers,
    cycles: computer.cycles(),
    changedCells,
    status,
    ...(error === undefined ? {} : { error }),
    pc,
    location: sourceIndex?.location_for(pc) ?? null,
  };
}

function pushSnapshot(status: RunStatus, error?: string): void {
  post({ type: "snapshot", snapshot: buildSnapshot(status, error) });
  lastSnapshotAt = performance.now();
}

// A macrotask trampoline: posting to `port2` queues a task on `port1`, letting
// any pending main-thread message (pause, setBreakpoints) run in between.
const trampoline = new MessageChannel();
trampoline.port1.addEventListener("message", () => {
  runTick();
});
trampoline.port1.start();
function scheduleTick(): void {
  // oxlint-disable-next-line unicorn/require-post-message-target-origin -- MessagePort.postMessage takes no targetOrigin
  trampoline.port2.postMessage(null);
}

function runTick(): void {
  if (!running || !computer) return;
  let result: ReturnType<Computer["run_batch"]>;
  try {
    result = computer.run_batch(BATCH_SIZE, Uint32Array.from(breakpoints));
  } catch (error) {
    running = false;
    pushSnapshot("panicked", String(error));
    return;
  }

  const status = statusFromBatch(result.status);
  if (status !== "running") {
    running = false;
    pushSnapshot(status, result.error ?? undefined);
    return;
  }

  if (performance.now() - lastSnapshotAt >= SNAPSHOT_INTERVAL_MS) {
    pushSnapshot("running");
  }
  scheduleTick();
}

function watch(address: number): void {
  if (!computer || watched.has(address)) return;
  const unsubscribe = computer.subscribe_memory(address, (cell: Cell) => {
    pendingCells.set(address, cell);
  });
  watched.set(address, unsubscribe);
  post({ type: "cells", cells: [[address, computer.memory(address)]] });
}

function unwatch(address: number): void {
  const unsubscribe = watched.get(address);
  if (!unsubscribe) return;
  unsubscribe();
  watched.delete(address);
  pendingCells.delete(address);
}

function stopSession(): void {
  running = false;
  breakpoints = [];
  for (const unsubscribe of watched.values()) unsubscribe();
  watched.clear();
  pendingCells.clear();
  computer?.free();
  computer = null;
  sourceIndex = null;
}

function start(
  files: Record<string, string>,
  rootFile: string,
  entrypoint: string,
): { labels: [string, number][]; touchedFiles: string[] } {
  stopSession();
  const preprocessor = new InMemoryPreprocessor(
    new Map(Object.entries(files)),
    rootFile,
  );
  const result = preprocessor.compile();
  const program = result.program;
  if (!program) {
    throw new Error(result.report ?? "Failed to preprocess program");
  }
  // Throws a JSON diagnostics string if the program does not assemble.
  computer = program.compile(entrypoint);
  sourceIndex = computer.source_index;

  const labels: [string, number][] = [...computer.labels];
  const touchedFiles = [
    ...new Set([...computer.source_map.values()].map((loc) => loc.file)),
  ];
  return { labels, touchedFiles };
}

self.addEventListener("message", (event: MessageEvent<WorkerRequest>) => {
  const message = event.data;
  void ready.then(() => {
    switch (message.type) {
      case "start": {
        try {
          const { labels, touchedFiles } = start(
            message.files,
            message.rootFile,
            message.entrypoint,
          );
          post({
            id: message.id,
            type: "started",
            labels,
            touchedFiles,
            snapshot: buildSnapshot("paused"),
          });
        } catch (error) {
          post({ id: message.id, type: "startError", error: String(error) });
        }
        break;
      }
      case "step": {
        // Ignore manual steps while a continuous run is in flight; the run loop
        // owns the computer and the UI disables stepping, but guard the
        // protocol so a stray `step` can't interleave with `run_batch`.
        if (!computer || running) return;
        try {
          const result = computer.run_batch(
            Math.max(1, message.n),
            Uint32Array.from([]),
          );
          const status = statusFromBatch(result.status);
          // A finished step budget reads back as "running"; surface it as a
          // stopped (paused) state since we are not continuously running.
          pushSnapshot(
            status === "running" ? "paused" : status,
            result.error ?? undefined,
          );
        } catch (error) {
          pushSnapshot("panicked", String(error));
        }
        break;
      }
      case "run": {
        if (!computer || running) return;
        running = true;
        scheduleTick();
        break;
      }
      case "pause": {
        if (!running) return;
        running = false;
        pushSnapshot("paused");
        break;
      }
      case "stop": {
        stopSession();
        break;
      }
      case "setBreakpoints": {
        breakpoints = message.addresses;
        break;
      }
      case "resolveBreakpoint": {
        const resolved =
          sourceIndex?.address_for(message.file, message.line) ?? null;
        post({ id: message.id, type: "resolved", resolved: resolved ?? null });
        break;
      }
      case "watchCells": {
        for (const address of message.addresses) watch(address);
        break;
      }
      case "unwatchCells": {
        for (const address of message.addresses) unwatch(address);
        break;
      }
    }
  });
});
