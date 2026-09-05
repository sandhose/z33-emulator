import type { StoreApi } from "zustand";

type PersistedStore<T> = Pick<StoreApi<T>, "setState" | "getInitialState"> & {
  persist: { rehydrate: () => Promise<void> | void };
};

/**
 * Seed `key` with a raw persisted payload and rehydrate `store` from it, the
 * way a page load would.
 *
 * The store is reset first because a page load rehydrates onto the initial
 * state, which is what a `merge` falls back to; the reset writes through to
 * storage, so it has to happen before the payload is seeded.
 */
export async function rehydrateStore<T>(
  store: PersistedStore<T>,
  key: string,
  state: unknown,
  version?: number,
): Promise<void> {
  store.setState(store.getInitialState());
  localStorage.setItem(
    key,
    JSON.stringify(version === undefined ? { state } : { state, version }),
  );
  await store.persist.rehydrate();
}
