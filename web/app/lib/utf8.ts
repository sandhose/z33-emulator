/**
 * Build a lookup table from UTF-8 byte offset → JS string character offset
 * (UTF-16 code unit index).
 *
 * Rust/miette produce byte offsets into UTF-8 source text, but Monaco's
 * `getPositionAt()` expects character (UTF-16 code unit) offsets. This map
 * lets us convert one to the other in O(1).
 */
export function buildByteToCharMap(text: string): Uint32Array {
  const encoded = new TextEncoder().encode(text);
  // Map from byte index → char index. +1 so we can look up the end-of-string offset.
  const map = new Uint32Array(encoded.length + 1);
  let charIdx = 0;
  let byteIdx = 0;
  while (charIdx < text.length) {
    const cp = text.codePointAt(charIdx);
    if (cp === undefined) break;
    const byteLen = cp <= 0x7f ? 1 : cp <= 0x7ff ? 2 : cp <= 0xffff ? 3 : 4;
    const charLen = cp > 0xffff ? 2 : 1; // surrogate pair
    for (let i = 0; i < byteLen; i++) {
      map[byteIdx + i] = charIdx;
    }
    byteIdx += byteLen;
    charIdx += charLen;
  }
  map[byteIdx] = charIdx;
  return map;
}

/**
 * Convert a UTF-8 byte offset to a UTF-16 code unit offset using a
 * precomputed lookup table.
 */
export function byteOffsetToCharOffset(
  byteToChar: Uint32Array,
  byteOffset: number,
): number {
  return byteToChar[byteOffset] ?? byteOffset;
}
