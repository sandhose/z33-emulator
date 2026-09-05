// The localStorage keys every persisted store writes under, kept in a
// dependency-free leaf module so they can be shared with e2e fixtures and
// tests. Importing a store itself from the Playwright process would eval
// Vite-only macros (`import.meta.glob`) and fail, so the single source of
// truth lives here.

/** localStorage key holding the persisted workspace (files + entrypoints). */
export const WORKSPACE_STORAGE_KEY = "z33:workspace-v2";

/** Schema version of the persisted workspace payload (zustand `persist`). */
export const WORKSPACE_PERSIST_VERSION = 0;

/** localStorage key holding the requested breakpoint lines per file. */
export const BREAKPOINTS_STORAGE_KEY = "z33:breakpoints";

/**
 * localStorage key holding the chosen theme. The bootstrap script in
 * index.html reads it too, spelled out, so renaming it here means renaming it
 * there (see theme-store.ts).
 */
export const THEME_STORAGE_KEY = "z33:theme";

/** localStorage key holding the number display format. */
export const DISPLAY_STORAGE_KEY = "z33:display";

/** localStorage key holding the target clock speed. */
export const SPEED_STORAGE_KEY = "z33:speed";
