import { create } from "zustand";
import type { Labels } from "../computer-types";
import { type ComputerProxy, startSession } from "../lib/computer-proxy";

type AppMode =
  | { type: "edit" }
  | {
      type: "debug";
      computer: ComputerProxy;
      labels: Labels;
      /** Files that contributed code to the running program (store keys). */
      touchedFiles: string[];
    };

interface AppState {
  mode: AppMode;
}

interface AppActions {
  /**
   * Compile and start a debug session in the emulator worker. Resolves once the
   * session is ready (or rejects on a compilation error).
   */
  startDebug: (
    files: Record<string, string>,
    rootFile: string,
    entrypoint: string,
  ) => Promise<void>;
  stopDebug: () => void;
}

export const useAppStore = create<AppState & AppActions>()((set, get) => ({
  mode: { type: "edit" },

  startDebug: async (files, rootFile, entrypoint) => {
    const { proxy, labels, touchedFiles } = await startSession(
      files,
      rootFile,
      entrypoint,
    );

    const labelMap: Labels = new Map();
    for (const [label, address] of labels) {
      const values = labelMap.get(address) ?? [];
      labelMap.set(address, [...values, label]);
    }

    set({
      mode: { type: "debug", computer: proxy, labels: labelMap, touchedFiles },
    });
  },

  stopDebug: () => {
    const { mode } = get();
    if (mode.type === "debug") mode.computer.dispose();
    set({ mode: { type: "edit" } });
  },
}));
