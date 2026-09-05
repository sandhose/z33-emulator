// In-memory `localStorage` for the node test environment, installed as the
// unit project's setup file (vitest.config.ts) and emptied between tests so a
// persisted store never leaks state from one case into the next.
import { beforeEach } from "vitest";

class MemoryStorage implements Storage {
  #entries = new Map<string, string>();

  get length(): number {
    return this.#entries.size;
  }

  key(index: number): string | null {
    return [...this.#entries.keys()][index] ?? null;
  }

  getItem(key: string): string | null {
    return this.#entries.get(key) ?? null;
  }

  setItem(key: string, value: string): void {
    this.#entries.set(key, value);
  }

  removeItem(key: string): void {
    this.#entries.delete(key);
  }

  clear(): void {
    this.#entries.clear();
  }
}

// Installed once, and emptied rather than replaced between tests: zustand's
// `createJSONStorage` resolves the backing object when the store is created and
// holds on to it, so a fresh instance here would not be the one it writes to.
Object.defineProperty(globalThis, "localStorage", {
  value: new MemoryStorage(),
  configurable: true,
  writable: true,
});

beforeEach(() => {
  localStorage.clear();
});
