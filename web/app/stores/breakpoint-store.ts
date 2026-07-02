import { create } from "zustand";
import { createJSONStorage, persist } from "zustand/middleware";

const BREAKPOINTS_KEY = "z33:breakpoints";

/** A breakpoint resolved against a compiled program. */
export type ResolvedBreakpoint = { line: number; address: number };

/** Resolution result per file: requested line -> resolved info (or null). */
export type ResolvedMap = Record<
  string,
  Record<number, ResolvedBreakpoint | null>
>;

interface BreakpointState {
  /** Requested breakpoint lines (1-based) per file store key. Persisted. */
  breakpoints: Record<string, number[]>;
  /** Snapping/verification results, populated during a debug session. */
  resolved: ResolvedMap;
}

interface BreakpointActions {
  toggle: (file: string, line: number) => void;
  setResolved: (resolved: ResolvedMap) => void;
  clearResolved: () => void;
}

export const useBreakpointStore = create<BreakpointState & BreakpointActions>()(
  persist(
    (set) => ({
      breakpoints: {},
      resolved: {},

      toggle: (file, line) => {
        set((state) => {
          const current = state.breakpoints[file] ?? [];
          const next = current.includes(line)
            ? current.filter((l) => l !== line)
            : // oxlint-disable-next-line unicorn/no-array-sort -- sorting a fresh copy
              [...current, line].sort((a, b) => a - b);
          const breakpoints = { ...state.breakpoints };
          if (next.length === 0) delete breakpoints[file];
          else breakpoints[file] = next;
          return { breakpoints };
        });
      },

      setResolved: (resolved) => {
        set({ resolved });
      },
      clearResolved: () => {
        set({ resolved: {} });
      },
    }),
    {
      name: BREAKPOINTS_KEY,
      storage: createJSONStorage(() => localStorage),
      partialize: (state) => ({ breakpoints: state.breakpoints }),
    },
  ),
);
