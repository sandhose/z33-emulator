// Shared sentinel for fatal worker-initialization failures.
//
// A worker whose module-level init (`await init(wasm)`) rejects would otherwise
// just go dark: the rejection is unhandled and never reaches the main thread as
// an `error` event. Instead each worker catches that failure and posts a frame
// of this shape, which the owning client recognizes and turns into a rejected
// readiness promise (fail fast) rather than an indefinite hang.
export const WORKER_ERROR = "z33/workerError" as const;

export interface WorkerErrorFrame {
  type: typeof WORKER_ERROR;
  message: string;
}

/** Type guard for the worker-error sentinel frame. */
export function isWorkerErrorFrame(data: unknown): data is WorkerErrorFrame {
  return (
    typeof data === "object" &&
    data !== null &&
    (data as { type?: unknown }).type === WORKER_ERROR
  );
}
