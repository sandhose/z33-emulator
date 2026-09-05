import { create } from "zustand";
import { createJSONStorage, persist } from "zustand/middleware";
import { SPEED_STORAGE_KEY } from "./persist-keys";
import { persistedField } from "./persisted";

/** Clock-speed presets offered by the debug toolbar. */
export const SPEED_OPTIONS: { label: string; speed: number | null }[] = [
  { label: "Max", speed: null },
  { label: "1 kHz", speed: 1000 },
  { label: "100 Hz", speed: 100 },
  { label: "10 Hz", speed: 10 },
  { label: "2 Hz", speed: 2 },
];

interface SpeedState {
  /** Target clock speed in cycles per second; `null` = full speed. */
  speed: number | null;
  setSpeed: (speed: number | null) => void;
}

export const useSpeedStore = create<SpeedState>()(
  persist(
    (set) => ({
      speed: null,
      setSpeed: (speed) => {
        set({ speed });
      },
    }),
    {
      name: SPEED_STORAGE_KEY,
      storage: createJSONStorage(() => localStorage),
      // The toolbar select can only show one of the presets, so a persisted
      // speed that is no longer among them goes back to the default. `null`
      // is itself a preset (Max), so the match is against the option list
      // rather than a typeof check.
      merge: (persisted, current) => {
        const value = persistedField(persisted, "speed");
        const match = SPEED_OPTIONS.find((option) => option.speed === value);
        return { ...current, speed: match ? match.speed : current.speed };
      },
    },
  ),
);
