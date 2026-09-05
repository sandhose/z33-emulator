import { fileURLToPath } from "node:url";
import { storybookTest } from "@storybook/addon-vitest/vitest-plugin";
import { playwright } from "@vitest/browser-playwright";
import { defineConfig } from "vitest/config";

// Two projects: `unit` covers the app's pure logic in node, `storybook` runs
// the stories in a real browser (Playwright/chromium).
//
// Only the storybook project extends the app's ./vite.config.ts: merging that
// config into the node project duplicates its `oxc.target` entries, which the
// transform rejects, so the node project carries its own copy of the `@`
// alias. The storybookTest plugin auto-injects the preview's project
// annotations (decorators/parameters), so no explicit setup file is needed.
export default defineConfig({
  test: {
    projects: [
      {
        resolve: {
          alias: {
            "@": fileURLToPath(new URL("./app", import.meta.url)),
          },
        },
        test: {
          name: "unit",
          environment: "node",
          include: ["app/**/*.test.{ts,tsx}"],
          // The persisted stores reach for `localStorage` on every write.
          setupFiles: ["./app/testing/local-storage.ts"],
        },
      },
      {
        extends: "./vite.config.ts",
        plugins: [
          await storybookTest({
            configDir: fileURLToPath(new URL(".storybook", import.meta.url)),
          }),
        ],
        test: {
          name: "storybook",
          browser: {
            enabled: true,
            provider: playwright(),
            headless: true,
            instances: [{ browser: "chromium" }],
          },
        },
      },
    ],
  },
});
