import { defineConfig, devices } from "@playwright/test";

// Override with PW_PORT to avoid reusing an unrelated dev server (e.g. one
// started from another checkout) — `reuseExistingServer` trusts whatever
// answers on the port.
const port = Number(process.env.PW_PORT ?? 5173);

export default defineConfig({
  testDir: "./e2e",
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  workers: process.env.CI ? 1 : undefined,
  // The `github` reporter emits annotations and nothing on disk, so the
  // workflow's report upload needs `html` too.
  reporter: process.env.CI ? [["github"], ["html", { open: "never" }]] : "html",
  timeout: 60_000,
  expect: { timeout: 10_000 },
  use: {
    baseURL: `http://localhost:${port}`,
    trace: "on-first-retry",
    screenshot: "only-on-failure",
  },
  projects: [
    { name: "chromium", use: { ...devices["Desktop Chrome"] } },
    // WebKit lags the other engines on new syntax, and the app it refuses to
    // parse is the app nobody can use. The startup spec loads the shell, the
    // editor chunk and the emulator worker, which is enough to catch that.
    {
      name: "webkit",
      use: { ...devices["Desktop Safari"] },
      testMatch: /startup\.spec\.ts/u,
    },
  ],
  webServer: {
    command: `pnpm start --port ${port} --strictPort`,
    url: `http://localhost:${port}`,
    reuseExistingServer: !process.env.CI,
    timeout: 120_000,
  },
});
