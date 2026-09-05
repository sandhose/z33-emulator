import { beforeEach, describe, expect, it } from "vitest";
import { rehydrateStore } from "../testing/rehydrate-store";
import { useBreakpointStore } from "./breakpoint-store";
import { BREAKPOINTS_STORAGE_KEY } from "./persist-keys";

const rehydrateFrom = (state: unknown) =>
  rehydrateStore(useBreakpointStore, BREAKPOINTS_STORAGE_KEY, state);

beforeEach(() => {
  useBreakpointStore.setState({ breakpoints: {}, resolved: {} });
});

describe("rehydration", () => {
  it("loads well-formed breakpoints", async () => {
    await rehydrateFrom({ breakpoints: { "a.s": [2, 5] } });
    expect(useBreakpointStore.getState().breakpoints).toEqual({
      "a.s": [2, 5],
    });
  });

  it.each([null, "2", 2, [2], { "a.s": 2 }, { "a.s": ["2"] }, { "a.s": [0] }])(
    "starts with no breakpoints when the payload holds %o",
    async (breakpoints) => {
      await rehydrateFrom({ breakpoints });
      expect(useBreakpointStore.getState().breakpoints).toEqual({});
    },
  );

  it("never restores resolution results, which belong to a session", async () => {
    await rehydrateFrom({
      breakpoints: { "a.s": [2] },
      resolved: { "a.s": { 2: { line: 2, address: 1002 } } },
    });
    expect(useBreakpointStore.getState().resolved).toEqual({});
  });
});

describe("toggle", () => {
  it("adds a breakpoint to a file that had none", () => {
    useBreakpointStore.getState().toggle("a.s", 4);
    expect(useBreakpointStore.getState().breakpoints).toEqual({ "a.s": [4] });
  });

  it("keeps the lines of a file sorted", () => {
    for (const line of [7, 2, 5])
      useBreakpointStore.getState().toggle("a.s", line);
    expect(useBreakpointStore.getState().breakpoints["a.s"]).toEqual([2, 5, 7]);
  });

  it("removes a line that was already set", () => {
    useBreakpointStore.getState().toggle("a.s", 2);
    useBreakpointStore.getState().toggle("a.s", 5);
    useBreakpointStore.getState().toggle("a.s", 2);
    expect(useBreakpointStore.getState().breakpoints).toEqual({ "a.s": [5] });
  });

  it("drops the file once its last breakpoint goes", () => {
    useBreakpointStore.getState().toggle("a.s", 2);
    useBreakpointStore.getState().toggle("a.s", 2);
    expect(useBreakpointStore.getState().breakpoints).toEqual({});
  });

  it("keeps files independent", () => {
    useBreakpointStore.getState().toggle("a.s", 2);
    useBreakpointStore.getState().toggle("b.s", 2);
    useBreakpointStore.getState().toggle("a.s", 2);
    expect(useBreakpointStore.getState().breakpoints).toEqual({ "b.s": [2] });
  });
});

describe("resolution results", () => {
  it("replaces the whole map, then clears it", () => {
    const resolved = { "a.s": { 2: { line: 3, address: 1003 } } };
    useBreakpointStore.getState().setResolved(resolved);
    expect(useBreakpointStore.getState().resolved).toEqual(resolved);
    useBreakpointStore.getState().clearResolved();
    expect(useBreakpointStore.getState().resolved).toEqual({});
  });
});
