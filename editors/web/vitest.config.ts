import { fileURLToPath } from "node:url";
import { storybookTest } from "@storybook/addon-vitest/vitest-plugin";
import { playwright } from "@vitest/browser-playwright";
import { defineConfig } from "vitest/config";

// Storybook stories are run as a dedicated Vitest project in a real browser
// (Playwright/chromium). The project extends the app's ./vite.config.ts so the
// Tailwind plugin and the `@` alias apply, just like the running app. The
// storybookTest plugin auto-injects the preview's project annotations
// (decorators/parameters), so no explicit setup file is needed.
export default defineConfig({
  test: {
    projects: [
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
