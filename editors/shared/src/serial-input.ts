// One translation from terminal input to serial input, shared by the web IDE
// and the VS Code extension so both consoles behave the same on the same
// program.

const ESCAPE = "\u001B";
const DELETE = "\u007F";
const BACKSPACE = "\u0008";

/**
 * Translate one terminal input chunk into what the serial port should receive,
 * as a string the caller sends or encodes. Returns `null` when the chunk
 * carries nothing for the emulator, so a caller never sends an empty edge.
 *
 * The conventions:
 *   * Enter arrives as CR, or, on the VS Code pty, as CRLF, and becomes a
 *     single LF, the host line convention. Matching `\r\n?` rather than `\r`
 *     keeps one CRLF from becoming two host newlines.
 *   * Backspace arrives as DEL and becomes BS.
 *   * A chunk starting with ESC is an arrow or function key, which carries no
 *     serial meaning.
 *   * Everything else, control characters included, passes through: the
 *     program decides what to do with Ctrl-D and friends.
 */
export function translateSerialInput(data: string): string | null {
  if (data === "" || data.startsWith(ESCAPE)) return null;
  return data.replaceAll(/\r\n?/gu, "\n").replaceAll(DELETE, BACKSPACE);
}
