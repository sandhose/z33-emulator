import { useDebouncer } from "@tanstack/react-pacer";
import { useCallback, useEffect, useRef, useState } from "react";
import { checkProgram } from "../lib/computer-proxy";
import type { CheckResult } from "../lib/emulator-protocol";
import { useFileStore } from "../stores/file-store";

/** `unavailable`: the worker never answered, so nothing was checked. */
type CompilationResult =
  | { type: "idle" }
  | { type: "unavailable" }
  | CheckResult;

type UICompilationStatus = "pending" | "success" | "error" | "unavailable";

/**
 * The status the toolbar shows. The first result only lands once the worker has
 * fetched and instantiated the wasm, seconds into the load on a slow link, so
 * "no result yet" is pending rather than idle. A dead worker stays reported as
 * such: every later check rejects the same way.
 */
function uiStatus(
  result: CompilationResult,
  busy: boolean,
): UICompilationStatus {
  if (result.type === "unavailable") return "unavailable";
  if (busy || result.type === "idle") return "pending";
  return result.type;
}

/**
 * Compiles the active file to drive the edit-mode toolbar: the entrypoint
 * selector (labels) and the Run button (success/error status).
 *
 * The program comes from the file store, which monaco-sync mirrors every editor
 * edit into, so the first check goes out as the app mounts instead of waiting
 * for the editor chunk. That puts the check a keystroke plus both debounces —
 * monaco-sync's and the one below — behind the buffer, while the LSP's
 * diagnostics follow Monaco's models directly, so squiggles can appear before
 * the toolbar status catches up. Diagnostics (squiggles, markers) are owned by
 * the LSP, not this hook. The actual runnable program is (re)built in the
 * emulator worker on Run; here we only surface whether it *can* be built and
 * its labels.
 */
export function useCompilation(activeFile: string) {
  const [compilationResult, setCompilationResult] = useState<CompilationResult>(
    { type: "idle" },
  );
  const compileGeneration = useRef(0);
  const workerFailed = useRef(false);
  const [checkInFlight, setCheckInFlight] = useState(false);

  const performCompile = useCallback(() => {
    if (workerFailed.current) return;
    const generation = ++compileGeneration.current;
    setCheckInFlight(true);
    void checkProgram(useFileStore.getState().files, activeFile).then(
      (result) => {
        // A newer compile superseded this one while it was in flight; it owns
        // the in-flight flag from here on.
        if (generation !== compileGeneration.current) return;
        setCheckInFlight(false);
        setCompilationResult(result);
      },
      (error: unknown) => {
        if (generation !== compileGeneration.current) return;
        setCheckInFlight(false);
        // A rejection is the worker failing (wasm never loaded, worker died);
        // a program the compiler refuses comes back as an `error` result. The
        // worker never recovers, so this is the last check we run.
        workerFailed.current = true;
        console.error("[z33] the emulator worker is unavailable:", error);
        setCompilationResult({ type: "unavailable" });
      },
    );
  }, [activeFile]);

  const compileDebouncer = useDebouncer(
    performCompile,
    { wait: 300 },
    (state) => ({ isPending: state.isPending }),
  );

  // The effects below depend on these stable methods rather than on
  // `compileDebouncer`: its identity changes whenever isPending flips, which
  // maybeExecute() itself does, so the effects would re-trigger themselves.
  const { maybeExecute, flush } = compileDebouncer;

  // Check once on mount, then whenever the file map changes: editor keystrokes
  // reach the store through monaco-sync, and file creation, deletion and upload
  // write to it directly. A write that hands back the same map — resetting an
  // untouched workspace — is not a change and checks nothing.
  useEffect(() => {
    maybeExecute();
    return useFileStore.subscribe((state, prev) => {
      if (state.files !== prev.files) maybeExecute();
    });
  }, [maybeExecute]);

  // For callers that just learned the answer on screen is stale; skips the
  // debounce because the caller already waited for a round trip.
  const recheck = useCallback(() => {
    maybeExecute();
    flush();
  }, [maybeExecute, flush]);

  // Re-trigger on activeFile change (new preprocessor entrypoint).
  useEffect(() => {
    maybeExecute();
    flush();
  }, [activeFile, maybeExecute, flush]);

  const compilationStatus = uiStatus(
    compilationResult,
    compileDebouncer.state.isPending || checkInFlight,
  );

  return { compilationResult, compilationStatus, recheck };
}
