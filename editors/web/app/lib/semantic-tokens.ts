import type { SemanticTokens } from "vscode-languageserver-protocol";

const encoder = new TextEncoder();
const decoder = new TextDecoder();

/**
 * The server emits semantic-token *lengths* in UTF-8 bytes but token *offsets*
 * in UTF-16 code units. On lines with multi-byte characters (e.g. comments with
 * accents) that makes a token overrun the line, and Monaco rejects the whole
 * set. Rewrite each length from bytes to UTF-16 code units.
 *
 * `getLineContent` takes a 1-based line number, like Monaco's model, and
 * returns "" for a line past the end of the document.
 */
export function fixSemanticTokenLengths(
  tokens: SemanticTokens,
  getLineContent: (line: number) => string,
): SemanticTokens {
  const data = Array.from(tokens.data);
  let line = 0;
  let char = 0;
  for (let i = 0; i < data.length; i += 5) {
    const deltaLine = data[i] ?? 0;
    const deltaStart = data[i + 1] ?? 0;
    line += deltaLine;
    char = deltaLine === 0 ? char + deltaStart : deltaStart;

    const byteLength = data[i + 2] ?? 0;
    const lineText = getLineContent(line + 1);
    const rest = lineText.slice(char);
    const restBytes = encoder.encode(rest);
    const utf16Length =
      byteLength >= restBytes.length
        ? rest.length
        : decoder.decode(restBytes.slice(0, byteLength)).length;
    data[i + 2] = utf16Length;
  }
  return tokens.resultId === undefined
    ? { data }
    : { resultId: tokens.resultId, data };
}
