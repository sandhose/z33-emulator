import { fileURLToPath } from "node:url";
import { storybookTest } from "@storybook/addon-vitest/vitest-plugin";
import { playwright } from "@vitest/browser-playwright";
import { defaultServerConditions } from "vite";
import { defineConfig } from "vitest/config";

const web = fileURLToPath(new URL("./editors/web", import.meta.url));
const shared = fileURLToPath(new URL("./editors/shared", import.meta.url));

// Only `web-storybook` extends the app's vite config: merging that config into
// a node project duplicates its `oxc.target` entries, which the transform
// rejects, so `web-unit` carries its own copy of the `@` alias. The
// storybookTest plugin auto-injects the preview's project annotations
// (decorators/parameters), so no explicit setup file is needed.
export default defineConfig({
  test: {
    projects: [
      {
        root: web,
        resolve: {
          alias: { "@": `${web}/app` },
        },
        // The modules under test are browser code, so their dependencies
        // resolve the way a browser would as well as the way node does:
        // `vscode-jsonrpc/browser` has no export for node to pick.
        ssr: {
          resolve: { conditions: [...defaultServerConditions, "browser"] },
        },
        test: {
          name: "web-unit",
          environment: "node",
          include: ["app/**/*.test.{ts,tsx}"],
          // The persisted stores reach for `localStorage` on every write.
          setupFiles: ["./app/testing/local-storage.ts"],
        },
      },
      {
        root: web,
        extends: `${web}/vite.config.ts`,
        plugins: [
          await storybookTest({
            configDir: `${web}/.storybook`,
          }),
        ],
        test: {
          name: "web-storybook",
          browser: {
            enabled: true,
            provider: playwright(),
            headless: true,
            instances: [{ browser: "chromium" }],
          },
        },
      },
      {
        root: shared,
        test: {
          name: "shared-unit",
          environment: "node",
          include: ["src/**/*.test.ts"],
        },
      },
    ],
  },
});
