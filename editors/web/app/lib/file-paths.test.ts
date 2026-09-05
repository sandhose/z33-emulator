import { describe, expect, it } from "vitest";
import { stripLeadingSlash, toMonacoPath } from "./file-paths";

describe("toMonacoPath", () => {
  it("prefixes a store key with a slash", () => {
    expect(toMonacoPath("fact.s")).toBe("/fact.s");
    expect(toMonacoPath("lib/util.s")).toBe("/lib/util.s");
    expect(toMonacoPath("")).toBe("/");
  });
});

describe("stripLeadingSlash", () => {
  it("strips exactly one leading slash", () => {
    expect(stripLeadingSlash("/fact.s")).toBe("fact.s");
    expect(stripLeadingSlash("//fact.s")).toBe("/fact.s");
  });

  it("leaves a path without a leading slash alone", () => {
    expect(stripLeadingSlash("fact.s")).toBe("fact.s");
    expect(stripLeadingSlash("a/b.s")).toBe("a/b.s");
    expect(stripLeadingSlash("")).toBe("");
  });
});

describe("round trip", () => {
  it("recovers the store key from the Monaco path", () => {
    for (const key of ["fact.s", "lib/util.s", "a b.s", "é.s"]) {
      expect(stripLeadingSlash(toMonacoPath(key))).toBe(key);
    }
  });
});
