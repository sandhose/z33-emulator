// Shape checks for the payloads the zustand `persist` middleware reads back out
// of localStorage. That storage is user-editable and outlives every schema
// change the app has ever made, so a store's `merge` re-checks each field it
// takes from it and keeps its default when the check fails.

/** One field of an unvalidated `persist` payload, or undefined if absent. */
export function persistedField(persisted: unknown, key: string): unknown {
  if (typeof persisted !== "object" || persisted === null) return undefined;
  // Own properties only: `in` would answer for every name on Object.prototype.
  if (!Object.hasOwn(persisted, key)) return undefined;
  // The payload is JSON of unknown shape; the caller narrows the value.
  // oxlint-disable-next-line typescript/no-unsafe-type-assertion
  return (persisted as Record<string, unknown>)[key];
}

/** `value` if it is one of `values`, otherwise undefined. */
export function oneOf<T extends string>(
  values: readonly T[],
  value: unknown,
): T | undefined {
  return typeof value === "string" &&
    (values as readonly string[]).includes(value)
    ? // The includes() above is the narrowing TypeScript cannot express here.
      // oxlint-disable-next-line typescript/no-unsafe-type-assertion
      (value as T)
    : undefined;
}

/** `value` if it maps every key to a string, otherwise undefined. */
export function stringRecord(
  value: unknown,
): Record<string, string> | undefined {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    return undefined;
  }
  const entries = Object.entries(value);
  if (entries.some(([, item]) => typeof item !== "string")) return undefined;
  // oxlint-disable-next-line typescript/no-unsafe-type-assertion
  return value as Record<string, string>;
}

/**
 * `value` if it maps every key to an array of line numbers, otherwise
 * undefined. Lines are 1-based integers, so anything below 1 or fractional
 * could never name a line in the editor.
 */
export function lineNumberRecord(
  value: unknown,
): Record<string, number[]> | undefined {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    return undefined;
  }
  const entries = Object.entries(value);
  const valid = entries.every(
    ([, item]) =>
      Array.isArray(item) &&
      item.every((line) => Number.isInteger(line) && line >= 1),
  );
  if (!valid) return undefined;
  // oxlint-disable-next-line typescript/no-unsafe-type-assertion
  return value as Record<string, number[]>;
}
