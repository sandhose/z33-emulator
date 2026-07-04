// Message protocol shared by the emulator worker (`emulator.worker.ts`) and the
// main-thread proxy (`computer-proxy.ts`).
//
// The worker owns compilation and the running `Computer`. The main thread never
// touches the wasm directly in debug mode; it drives execution and reads state
// exclusively through these messages.
import type {
  Cell,
  Registers,
  ResolvedBreakpoint,
  SourcePosition,
} from "./wasm";

/** High-level execution state of the emulator session. */
export type RunStatus = "running" | "paused" | "halted" | "panicked";

/** A throttled view of the machine state, pushed during and after execution. */
export interface Snapshot {
  registers: Registers;
  cycles: number;
  /** Cells that changed since the previous snapshot (watched addresses only). */
  changedCells: [number, Cell][];
  status: RunStatus;
  /** Present when `status === "panicked"`. */
  error?: string;
  pc: number;
  /** Resolved source location of `%pc`, if it maps to code. */
  location: SourcePosition | null;
}

// ---------------------------------------------------------------------------
// Main thread -> worker
// ---------------------------------------------------------------------------

export type WorkerRequest =
  | {
      id: number;
      type: "start";
      files: Record<string, string>;
      rootFile: string;
      entrypoint: string;
    }
  | { type: "step"; n: number }
  | { type: "run" }
  | { type: "pause" }
  | { type: "stop" }
  | { type: "setBreakpoints"; addresses: number[] }
  /**
   * Target clock speed in cycles per second; `null` = full speed. Stateful like
   * `setBreakpoints` and may arrive mid-run.
   */
  | { type: "setSpeed"; speed: number | null }
  | { id: number; type: "resolveBreakpoint"; file: string; line: number }
  | { type: "watchCells"; addresses: number[] }
  | { type: "unwatchCells"; addresses: number[] };

// ---------------------------------------------------------------------------
// Worker -> main thread
// ---------------------------------------------------------------------------

export type WorkerResponse =
  | {
      id: number;
      type: "started";
      labels: [string, number][];
      touchedFiles: string[];
      snapshot: Snapshot;
    }
  | { id: number; type: "startError"; error: string }
  | { id: number; type: "resolved"; resolved: ResolvedBreakpoint | null }
  /** Unsolicited push: cell values for freshly watched or changed addresses. */
  | { type: "cells"; cells: [number, Cell][] }
  /** Unsolicited push: a state snapshot during/after execution. */
  | { type: "snapshot"; snapshot: Snapshot }
  /**
   * Fatal worker failure (wasm init rejected, or an uncaught error). The proxy
   * rejects every pending request and surfaces a panicked state so callers such
   * as `startDebug` fail fast instead of awaiting forever.
   */
  | { type: "workerError"; message: string };
