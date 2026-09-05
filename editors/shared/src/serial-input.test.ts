import { describe, expect, it } from "vitest";
import { translateSerialInput } from "./serial-input";

const ESC = "\u001B";
const DEL = "\u007F";
const BS = "\u0008";
const EOT = "\u0004";

describe("translateSerialInput", () => {
  it("passes plain text through", () => {
    expect(translateSerialInput("hello")).toBe("hello");
  });

  it("turns a typed Enter into LF", () => {
    expect(translateSerialInput("\r")).toBe("\n");
  });

  it("turns a CRLF into a single LF", () => {
    expect(translateSerialInput("a\r\nb")).toBe("a\nb");
  });

  it("normalizes every newline of a multi-line paste", () => {
    expect(translateSerialInput("one\rtwo\r\nthree\n")).toBe("one\ntwo\nthree\n");
  });

  it("turns DEL into BS", () => {
    expect(translateSerialInput(DEL)).toBe(BS);
    expect(translateSerialInput(`a${DEL}b${DEL}`)).toBe(`a${BS}b${BS}`);
  });

  it("drops a chunk that starts with an escape sequence", () => {
    expect(translateSerialInput(`${ESC}[A`)).toBeNull();
    expect(translateSerialInput(`${ESC}OP`)).toBeNull();
  });

  it("drops a bare ESC chunk", () => {
    expect(translateSerialInput(ESC)).toBeNull();
  });

  it("passes an ESC that is not at the start through", () => {
    expect(translateSerialInput(`a${ESC}[Ab`)).toBe(`a${ESC}[Ab`);
  });

  it("keeps control characters the program reads", () => {
    expect(translateSerialInput(EOT)).toBe(EOT);
  });

  it("drops an empty chunk", () => {
    expect(translateSerialInput("")).toBeNull();
  });
});
