// Persistence identifiers for the workspace store, kept in a dependency-free
// leaf module so they can be shared with e2e fixtures. Importing `file-store`
// itself from the Playwright process would eval Vite-only macros
// (`import.meta.glob`) and fail, so the single source of truth lives here.

/** localStorage key holding the persisted workspace (files + entrypoints). */
export const WORKSPACE_STORAGE_KEY = "z33:workspace-v2";

/** Schema version of the persisted workspace payload (zustand `persist`). */
export const WORKSPACE_PERSIST_VERSION = 0;
