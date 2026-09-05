import { describe, expect, it } from "vitest";
import {
  lineNumberRecord,
  oneOf,
  persistedField,
  stringRecord,
} from "./persisted";

describe("persistedField", () => {
  it("reads a present field", () => {
    expect(persistedField({ theme: "dark" }, "theme")).toBe("dark");
    expect(persistedField({ speed: 0 }, "speed")).toBe(0);
    expect(persistedField({ speed: null }, "speed")).toBeNull();
  });

  it("returns undefined for a missing field", () => {
    expect(persistedField({}, "theme")).toBeUndefined();
  });

  it("returns undefined for a payload that is not an object", () => {
    for (const payload of [null, undefined, "dark", 3, true]) {
      expect(persistedField(payload, "theme")).toBeUndefined();
    }
  });

  it("reads through an array payload by index", () => {
    expect(persistedField(["a", "b"], "1")).toBe("b");
  });

  // A payload straight out of JSON.parse inherits Object.prototype, so every
  // method on it is a name the payload appears to carry.
  it("reads own properties only", () => {
    expect(persistedField({}, "toString")).toBeUndefined();
    expect(persistedField({}, "constructor")).toBeUndefined();
    expect(persistedField({}, "hasOwnProperty")).toBeUndefined();
  });
});

describe("oneOf", () => {
  const themes = ["dark", "light", "system"] as const;

  it("accepts a member of the union", () => {
    expect(oneOf(themes, "dark")).toBe("dark");
    expect(oneOf(themes, "system")).toBe("system");
  });

  it("rejects anything else", () => {
    for (const value of [
      "DARK",
      "",
      "toString",
      0,
      null,
      undefined,
      {},
      ["dark"],
    ]) {
      expect(oneOf(themes, value)).toBeUndefined();
    }
  });
});

describe("stringRecord", () => {
  it("accepts a map of strings", () => {
    expect(stringRecord({})).toEqual({});
    expect(stringRecord({ "a.s": "nop", "b.s": "" })).toEqual({
      "a.s": "nop",
      "b.s": "",
    });
  });

  it("rejects a non-object", () => {
    for (const value of [null, undefined, "a.s", 3, true]) {
      expect(stringRecord(value)).toBeUndefined();
    }
  });

  it("rejects an array", () => {
    expect(stringRecord(["a.s"])).toBeUndefined();
    expect(stringRecord([])).toBeUndefined();
  });

  it("rejects a map with a non-string value", () => {
    expect(stringRecord({ "a.s": "nop", "b.s": 3 })).toBeUndefined();
    expect(stringRecord({ "a.s": null })).toBeUndefined();
    expect(stringRecord({ "a.s": ["nop"] })).toBeUndefined();
  });
});

describe("lineNumberRecord", () => {
  it("accepts a map of line-number arrays", () => {
    expect(lineNumberRecord({})).toEqual({});
    expect(lineNumberRecord({ "a.s": [], "b.s": [1, 2] })).toEqual({
      "a.s": [],
      "b.s": [1, 2],
    });
  });

  it("rejects a non-object", () => {
    for (const value of [null, undefined, "a.s", 3]) {
      expect(lineNumberRecord(value)).toBeUndefined();
    }
  });

  it("rejects an array", () => {
    expect(lineNumberRecord([[1, 2]])).toBeUndefined();
  });

  it("rejects a map whose values are not arrays of numbers", () => {
    expect(lineNumberRecord({ "a.s": 3 })).toBeUndefined();
    expect(lineNumberRecord({ "a.s": "12" })).toBeUndefined();
    expect(lineNumberRecord({ "a.s": [1, "2"] })).toBeUndefined();
    expect(lineNumberRecord({ "a.s": [1, null] })).toBeUndefined();
  });

  it("rejects numbers that could not be a 1-based line", () => {
    for (const line of [-5, 0, 1.5, Number.NaN, Number.POSITIVE_INFINITY]) {
      expect(lineNumberRecord({ "a.s": [line] })).toBeUndefined();
      expect(lineNumberRecord({ "a.s": [1, line, 3] })).toBeUndefined();
    }
  });
});
