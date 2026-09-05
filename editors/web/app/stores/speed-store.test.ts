import { beforeEach, describe, expect, it } from "vitest";
import { rehydrateStore } from "../testing/rehydrate-store";
import { SPEED_STORAGE_KEY } from "./persist-keys";
import { SPEED_OPTIONS, useSpeedStore } from "./speed-store";

const rehydrateFrom = (state: unknown) =>
  rehydrateStore(useSpeedStore, SPEED_STORAGE_KEY, state);

beforeEach(() => {
  useSpeedStore.setState({ speed: null });
});

describe("SPEED_OPTIONS", () => {
  it("offers full speed plus descending presets", () => {
    expect(SPEED_OPTIONS[0]?.speed).toBeNull();
    const paced = SPEED_OPTIONS.slice(1).map((o) => o.speed);
    expect(paced).toEqual(paced.toSorted((a, b) => Number(b) - Number(a)));
  });
});

describe("setSpeed", () => {
  it("stores a preset and full speed alike", () => {
    useSpeedStore.getState().setSpeed(100);
    expect(useSpeedStore.getState().speed).toBe(100);
    useSpeedStore.getState().setSpeed(null);
    expect(useSpeedStore.getState().speed).toBeNull();
  });
});

describe("rehydration", () => {
  // Max is `null`, which is also the default, so rehydrating onto the initial
  // state would let that row pass without the validator ever accepting it.
  // Park the store on another preset first and rehydrate over it.
  it.each(SPEED_OPTIONS)("loads the $label preset", async ({ speed }) => {
    useSpeedStore.setState({ speed: 1000 });
    localStorage.setItem(
      SPEED_STORAGE_KEY,
      JSON.stringify({ state: { speed } }),
    );
    await useSpeedStore.persist.rehydrate();
    expect(useSpeedStore.getState().speed).toBe(speed);
  });

  it.each([7, 0, -1, "1000", null, undefined, {}])(
    "falls back to full speed on %o, which is not a preset",
    async (speed) => {
      await rehydrateFrom({ speed });
      expect(useSpeedStore.getState().speed).toBeNull();
    },
  );

  it("falls back to full speed on a payload that is not an object", async () => {
    await rehydrateFrom(1000);
    expect(useSpeedStore.getState().speed).toBeNull();
  });
});
