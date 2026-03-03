import { create } from "zustand";

export type DisplayFormat = "decimal" | "hex" | "binary";

interface DisplayState {
  format: DisplayFormat;
  setFormat: (format: DisplayFormat) => void;
}

export const useDisplayStore = create<DisplayState>()((set) => ({
  format: "decimal",
  setFormat: (format) => set({ format }),
}));
