import { describe, expect, it } from "vitest";
import { formatAddress, formatWord } from "./computer-types";

describe("formatWord", () => {
  it("formats zero", () => {
    expect(formatWord(0, "decimal")).toBe("0");
    expect(formatWord(0, "hex")).toBe("0x0");
    expect(formatWord(0, "binary")).toBe("0b0");
  });

  it("formats a positive word", () => {
    expect(formatWord(255, "decimal")).toBe("255");
    expect(formatWord(255, "hex")).toBe("0xFF");
    expect(formatWord(5, "binary")).toBe("0b101");
  });

  // Words are signed, and the panels render the sign in front of the base
  // prefix rather than a two's-complement bit pattern.
  it("formats a negative word with the sign before the prefix", () => {
    expect(formatWord(-1, "decimal")).toBe("-1");
    expect(formatWord(-255, "hex")).toBe("-0xFF");
    expect(formatWord(-5, "binary")).toBe("-0b101");
  });

  it("formats a multi-digit word in every base", () => {
    expect(formatWord(48_879, "decimal")).toBe("48879");
    expect(formatWord(48_879, "hex")).toBe("0xBEEF");
    expect(formatWord(48_879, "binary")).toBe("0b1011111011101111");
  });

  // A Word is an i64 in the emulator and crosses to the UI as a JS number, so
  // the largest one that survives the trip intact is 2**53 - 1.
  it("formats the largest word that reaches the UI intact", () => {
    expect(formatWord(Number.MAX_SAFE_INTEGER, "decimal")).toBe(
      "9007199254740991",
    );
    expect(formatWord(Number.MAX_SAFE_INTEGER, "hex")).toBe("0x1FFFFFFFFFFFFF");
    expect(formatWord(-Number.MAX_SAFE_INTEGER, "hex")).toBe(
      "-0x1FFFFFFFFFFFFF",
    );
  });
});

describe("formatAddress", () => {
  it("formats an address in every base", () => {
    expect(formatAddress(0, "decimal")).toBe("0");
    expect(formatAddress(0, "hex")).toBe("0x0");
    expect(formatAddress(1000, "decimal")).toBe("1000");
    expect(formatAddress(1000, "hex")).toBe("0x3E8");
    expect(formatAddress(2, "binary")).toBe("0b10");
  });
});
