import { create } from "zustand";
import { createJSONStorage, persist } from "zustand/middleware";
import { DISPLAY_STORAGE_KEY } from "./persist-keys";
import { oneOf, persistedField } from "./persisted";

export type DisplayFormat = "decimal" | "hex" | "binary";

const DISPLAY_FORMATS: readonly DisplayFormat[] = ["decimal", "hex", "binary"];

interface DisplayState {
  format: DisplayFormat;
  setFormat: (format: DisplayFormat) => void;
}

export const useDisplayStore = create<DisplayState>()(
  persist(
    (set) => ({
      format: "decimal",
      setFormat: (format) => {
        set({ format });
      },
    }),
    {
      name: DISPLAY_STORAGE_KEY,
      storage: createJSONStorage(() => localStorage),
      // Every formatter switches exhaustively on this, so a format that is no
      // longer one of the three throws on the next render of a memory cell.
      merge: (persisted, current) => ({
        ...current,
        format:
          oneOf(DISPLAY_FORMATS, persistedField(persisted, "format")) ??
          current.format,
      }),
    },
  ),
);
