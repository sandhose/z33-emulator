/**
 * Strip the leading slash from a Monaco URI path to get a file-store key.
 * Monaco models use `/filename.s`, file-store uses `filename.s`.
 */
export const stripLeadingSlash = (path: string): string =>
  path.replace(/^\//, "");

/**
 * Prepend a leading slash to a file-store key to get a Monaco URI path.
 */
export const toMonacoPath = (name: string): string => `/${name}`;
