import { create } from "zustand";
import { persist } from "zustand/middleware";

export type DisplayFormat = "decimal" | "hex" | "binary";

interface DisplayState {
  format: DisplayFormat;
  setFormat: (format: DisplayFormat) => void;
}

export const useDisplayStore = create<DisplayState>()(
  persist(
    (set) => ({
      format: "decimal",
      setFormat: (format) => set({ format }),
    }),
    { name: "z33:display" },
  ),
);
