import type { Computer, SourceMap } from "z33-web-bindings";
import { create } from "zustand";
import type { ComputerInterface, Labels } from "../computer";

export type AppMode =
  | { type: "edit" }
  | {
      type: "debug";
      computer: ComputerInterface;
      sourceMap: SourceMap;
      labels: Labels;
    };

interface AppState {
  mode: AppMode;
}

interface AppActions {
  startDebug: (
    compileFn: (entrypoint: string) => Computer,
    entrypoint: string,
  ) => void;
  stopDebug: () => void;
}

export const useAppStore = create<AppState & AppActions>()((set) => ({
  mode: { type: "edit" },

  startDebug: (compileFn, entrypoint) => {
    const computer = compileFn(entrypoint);
    const sourceMap = computer.source_map;

    const labels: Labels = new Map();
    for (const [label, address] of computer.labels) {
      const values = labels.get(address) ?? [];
      labels.set(address, [...values, label]);
    }

    set({ mode: { type: "debug", computer, sourceMap, labels } });
  },

  stopDebug: () => set({ mode: { type: "edit" } }),
}));
