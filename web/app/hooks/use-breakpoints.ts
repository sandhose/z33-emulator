import { useEffect } from "react";
import type { ComputerProxy } from "../lib/computer-proxy";
import {
  type ResolvedMap,
  useBreakpointStore,
} from "../stores/breakpoint-store";

/**
 * Resolve every requested breakpoint against the running program and push the
 * resulting addresses to the emulator worker. Runs on debug start and whenever
 * the breakpoint set changes during a session. Also stores the snapped /
 * unverified resolution so the gutter can render it.
 */
export function useBreakpointSync(
  computer: ComputerProxy,
  touchedFiles: string[],
): void {
  const breakpoints = useBreakpointStore((s) => s.breakpoints);
  const setResolved = useBreakpointStore((s) => s.setResolved);
  const clearResolved = useBreakpointStore((s) => s.clearResolved);

  useEffect(() => {
    let cancelled = false;

    void (async () => {
      // Resolve every breakpoint in the running program's files concurrently.
      const requests = Object.entries(breakpoints)
        .filter(([file]) => touchedFiles.includes(file))
        .flatMap(([file, lines]) =>
          lines.map(async (line) => ({
            file,
            line,
            result: await computer.resolveBreakpoint(file, line),
          })),
        );
      const results = await Promise.all(requests);
      if (cancelled) return;

      const resolved: ResolvedMap = {};
      const addresses: number[] = [];
      for (const { file, line, result } of results) {
        (resolved[file] ??= {})[line] = result
          ? { line: result.line, address: result.address }
          : null;
        if (result) addresses.push(result.address);
      }

      setResolved(resolved);
      computer.setBreakpoints(addresses);
    })();

    return () => {
      cancelled = true;
    };
  }, [computer, breakpoints, touchedFiles, setResolved]);

  // Drop resolution state when the session ends.
  useEffect(() => clearResolved, [clearResolved]);
}
