import { useCallback, useSyncExternalStore } from "react";
import type { ComputerProxy } from "../lib/computer-proxy";

/**
 * Drives execution through the emulator worker. Status (idle / running /
 * halted / panicked) is reflected from worker-pushed snapshots; there is no
 * main-thread stepping loop.
 */
export function useStepRunner(computer: ComputerProxy) {
  const status = useSyncExternalStore(
    useCallback((cb) => computer.subscribeStatus(cb), [computer]),
    () => computer.getStatus(),
  );

  const stepOnce = useCallback(() => {
    computer.step(1);
  }, [computer]);

  const run = useCallback(() => {
    computer.run();
  }, [computer]);

  const pause = useCallback(() => {
    computer.pause();
  }, [computer]);

  return {
    halt: status === "halted",
    panicked:
      status === "panicked" ? (computer.getError() ?? "Panicked") : null,
    running: status === "running",
    stepOnce,
    run,
    pause,
  };
}
