import { describe, expect, it } from "vitest";
import { fixSemanticTokenLengths } from "./semantic-tokens";

/**
 * A `getLineContent` over a document given as an array of lines (1-based),
 * empty past the end as the contract requires.
 */
const lines =
  (...content: string[]) =>
  (line: number): string =>
    content[line - 1] ?? "";

/** One token entry: [deltaLine, deltaStart, length, type, modifiers]. */
const token = (
  deltaLine: number,
  deltaStart: number,
  length: number,
): number[] => [deltaLine, deltaStart, length, 0, 0];

describe("fixSemanticTokenLengths", () => {
  it("leaves ASCII lengths untouched", () => {
    const fixed = fixSemanticTokenLengths(
      { data: [...token(0, 0, 4), ...token(0, 5, 2)] },
      lines("main: ld"),
    );
    expect(fixed.data).toEqual([...token(0, 0, 4), ...token(0, 5, 2)]);
  });

  // "é" is two UTF-8 bytes, so the server's byte length overshoots the line.
  it("shortens a length that counted multi-byte characters", () => {
    const fixed = fixSemanticTokenLengths(
      { data: token(0, 0, 4) },
      lines("héllo"),
    );
    expect(fixed.data).toEqual(token(0, 0, 3));
  });

  it("clamps a length that overruns the rest of the line", () => {
    const fixed = fixSemanticTokenLengths(
      { data: token(0, 2, 99) },
      lines("héllo"),
    );
    expect(fixed.data).toEqual(token(0, 2, 3));
  });

  it("counts an astral character as its two UTF-16 code units", () => {
    // "🙂" is four UTF-8 bytes and two UTF-16 code units.
    const fixed = fixSemanticTokenLengths(
      { data: token(0, 0, 4) },
      lines("🙂ab"),
    );
    expect(fixed.data).toEqual(token(0, 0, 2));
  });

  it("tracks the running line and column across deltas", () => {
    const fixed = fixSemanticTokenLengths(
      {
        data: [
          ...token(0, 0, 3), // line 1, col 0: "ré", 3 bytes
          ...token(0, 3, 4), // line 1, col 3: "sét", 4 bytes
          ...token(2, 1, 2), // line 3, col 1: "ç", 2 bytes
        ],
      },
      lines("ré sét", "", " ç"),
    );
    expect(fixed.data).toEqual([
      ...token(0, 0, 2),
      ...token(0, 3, 3),
      ...token(2, 1, 1),
    ]);
  });

  it("clamps a token on a line the document no longer has", () => {
    const fixed = fixSemanticTokenLengths(
      { data: [...token(0, 0, 2), ...token(5, 0, 3)] },
      lines("ab"),
    );
    expect(fixed.data).toEqual([...token(0, 0, 2), ...token(5, 0, 0)]);
  });

  it("does not mutate the input token data", () => {
    const data = token(0, 0, 4);
    fixSemanticTokenLengths({ data }, lines("héllo"));
    expect(data).toEqual(token(0, 0, 4));
  });

  it("keeps a result id when the server sent one", () => {
    expect(fixSemanticTokenLengths({ data: [] }, lines(""))).toEqual({
      data: [],
    });
    expect(
      fixSemanticTokenLengths({ resultId: "7", data: [] }, lines("")),
    ).toEqual({ resultId: "7", data: [] });
  });
});
