import { defineConfig, devices } from "@playwright/test";

/**
 * The VS Code web build the suite drives. Pinned so a workbench release cannot
 * change what the tests see between two runs of the same commit; CI keys its
 * `.vscode-test-web` cache on this value.
 */
export const VSCODE_WEB_COMMIT = "a44adf7f53e00964ab890f9f8758a334f1fc15bc";

// Override the ports to avoid reusing an unrelated server (e.g. one started
// from another checkout) — `reuseExistingServer` trusts whatever answers on
// the port. The VS Code hosts take the three ports from PW_VSCODE_PORT up.
const webPort = Number(process.env.PW_PORT ?? 5173);
const firstVscodePort = Number(process.env.PW_VSCODE_PORT ?? 3111);

// One host per workspace folder: `bare` has no launch.json (F5 must work from
// the active editor), `launch` covers the `program` spellings the README and
// the generated configuration use, and `untitled` turns `debug.saveBeforeStart`
// off so an unsaved buffer reaches the adapter without a Save As dialog.
const vscodeHosts = [
  { name: "bare", folder: "e2e/workspaces/bare" },
  { name: "launch", folder: "e2e/workspaces/launch" },
  { name: "untitled", folder: "e2e/workspaces/untitled" },
].map((host, index) => ({ ...host, port: firstVscodePort + index }));

export default defineConfig({
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  // CI's runners have too few cores to drive a browser and three extension
  // hosts at once. Locally Playwright's default (half the cores) applies, and
  // one pool serves both suites, so a full run has the web tests competing
  // with three VS Code hosts still loading their workbench.
  workers: process.env.CI ? 1 : undefined,
  // The `github` reporter emits annotations and nothing on disk, so the
  // workflow's report upload needs `html` too.
  reporter: process.env.CI ? [["github"], ["html", { open: "never" }]] : "html",
  use: {
    trace: "on-first-retry",
    screenshot: "only-on-failure",
  },
  projects: [
    {
      name: "web-chromium",
      testDir: "editors/web/e2e",
      fullyParallel: true,
      timeout: 60_000,
      expect: { timeout: 10_000 },
      use: { ...devices["Desktop Chrome"], baseURL: `http://localhost:${webPort}` },
    },
    // WebKit lags the other engines on new syntax, and the app it refuses to
    // parse is the app nobody can use. The startup spec loads the shell, the
    // editor chunk and the emulator worker, which is enough to catch that.
    {
      name: "web-webkit",
      testDir: "editors/web/e2e",
      testMatch: /startup\.spec\.ts/u,
      fullyParallel: true,
      timeout: 60_000,
      expect: { timeout: 10_000 },
      use: { ...devices["Desktop Safari"], baseURL: `http://localhost:${webPort}` },
    },
    // Each host directory holds one spec file, so `fullyParallel` has nothing
    // to split: a project is one test group on one worker.
    ...vscodeHosts.map(({ name, port }) => ({
      name: `vscode-${name}`,
      testDir: `editors/vscode/e2e/${name}`,
      // A cold test loads the workbench, activates the extension and
      // instantiates the wasm twice (language server worker and debug adapter)
      // before it can start a session.
      timeout: 90_000,
      // Every assertion here waits on the extension host: a round trip through
      // the language server or the debug adapter, not a DOM update.
      expect: { timeout: 15_000 },
      use: {
        ...devices["Desktop Chrome"],
        baseURL: `http://localhost:${port}`,
        // Wide enough to keep the editor title actions (the Run button) and
        // the explorer out of the overflow menus they collapse into when
        // cramped. It has to come after the device spread, which carries a
        // viewport of its own.
        viewport: { width: 1400, height: 900 },
      },
    })),
  ],
  webServer: [
    {
      command: `pnpm start --port ${webPort} --strictPort`,
      cwd: "editors/web",
      url: `http://localhost:${webPort}`,
      reuseExistingServer: !process.env.CI,
      timeout: 120_000,
    },
    // `vscode-test-web` wipes its data directory whenever the pinned build is
    // not already in it, so each host downloads into one of its own.
    ...vscodeHosts.map(({ name, folder, port }) => ({
      command: [
        "pnpm exec vscode-test-web",
        "--browser none",
        "--quality stable",
        `--commit ${VSCODE_WEB_COMMIT}`,
        `--testRunnerDataDir .vscode-test-web/${name}`,
        `--port ${port}`,
        "--extensionDevelopmentPath=.",
        folder,
      ].join(" "),
      cwd: "editors/vscode",
      url: `http://localhost:${port}`,
      reuseExistingServer: !process.env.CI,
      // The first run downloads the VS Code web build.
      timeout: 300_000,
    })),
  ],
});
