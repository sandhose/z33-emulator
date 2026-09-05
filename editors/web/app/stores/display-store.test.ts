import { beforeEach, describe, expect, it } from "vitest";
import { rehydrateStore } from "../testing/rehydrate-store";
import { useDisplayStore } from "./display-store";
import { DISPLAY_STORAGE_KEY } from "./persist-keys";

const rehydrateFrom = (state: unknown) =>
  rehydrateStore(useDisplayStore, DISPLAY_STORAGE_KEY, state);

beforeEach(() => {
  useDisplayStore.setState({ format: "decimal" });
});

describe("setFormat", () => {
  it("switches between the three bases", () => {
    for (const format of ["hex", "binary", "decimal"] as const) {
      useDisplayStore.getState().setFormat(format);
      expect(useDisplayStore.getState().format).toBe(format);
    }
  });
});

describe("rehydration", () => {
  it("loads a known format", async () => {
    await rehydrateFrom({ format: "hex" });
    expect(useDisplayStore.getState().format).toBe("hex");
  });

  // `formatWord` and `formatAddress` switch exhaustively and throw on anything
  // else, which takes the whole debug view down with them.
  it.each(["octal", "", 3, null, ["hex"]])(
    "falls back to decimal on the unknown format %o",
    async (format) => {
      await rehydrateFrom({ format });
      expect(useDisplayStore.getState().format).toBe("decimal");
    },
  );

  it("falls back to decimal on a payload that is not an object", async () => {
    await rehydrateFrom("hex");
    expect(useDisplayStore.getState().format).toBe("decimal");
  });
});
