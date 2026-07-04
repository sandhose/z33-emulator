import { create } from "zustand";
import { persist } from "zustand/middleware";

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
      name: "z33:speed",
      // Normalize a persisted speed that is no longer a preset back to full
      // speed, so the toolbar select always reflects the worker's pacing.
      merge: (persisted, current) => {
        let speed: number | null = null;
        if (
          persisted &&
          typeof persisted === "object" &&
          "speed" in persisted
        ) {
          const value = persisted.speed;
          if (
            typeof value === "number" &&
            SPEED_OPTIONS.some((o) => o.speed === value)
          ) {
            speed = value;
          }
        }
        return { ...current, speed };
      },
    },
  ),
);
