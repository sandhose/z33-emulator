import { defineConfig, devices } from "@playwright/test";

/**
 * The VS Code web build the suite drives. Pinned so a workbench release cannot
 * change what the tests see between two runs of the same commit; CI keys its
 * `.vscode-test-web` cache on this value.
 */
export const VSCODE_WEB_COMMIT = "a44adf7f53e00964ab890f9f8758a334f1fc15bc";

// One host per workspace folder: `bare` has no launch.json (F5 must work from
// the active editor), `launch` covers the `program` spellings the README and
// the generated configuration use, and `untitled` turns `debug.saveBeforeStart`
// off so an unsaved buffer reaches the adapter without a Save As dialog.
const hosts = [
  { name: "bare", folder: "e2e/workspaces/bare" },
  { name: "launch", folder: "e2e/workspaces/launch" },
  { name: "untitled", folder: "e2e/workspaces/untitled" },
];

const firstPort = Number(process.env.PW_PORT ?? 3111);

function portOf(index: number): number {
  return firstPort + index;
}

// `vscode-test-web` wipes its data directory whenever the pinned build is not
// already in it, so each host downloads into one of its own.
function command(index: number): string {
  const { name, folder } = hosts[index];
  return [
    "pnpm exec vscode-test-web",
    "--browser none",
    "--quality stable",
    `--commit ${VSCODE_WEB_COMMIT}`,
    `--testRunnerDataDir .vscode-test-web/${name}`,
    `--port ${portOf(index)}`,
    "--extensionDevelopmentPath=.",
    folder,
  ].join(" ");
}

export default defineConfig({
  testDir: "./e2e",
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  // Each test opens its own browser context, so no workbench state carries over
  // between tests; the hosts are all they share, and one worker per host is as
  // much parallelism as the suite has. CI's runners have too few cores to drive
  // three extension hosts at once.
  workers: process.env.CI ? 1 : hosts.length,
  reporter: process.env.CI ? [["github"], ["html", { open: "never" }]] : "html",
  // A cold test loads the workbench, activates the extension and instantiates
  // the wasm twice (language server worker and debug adapter) before it can
  // start a session.
  timeout: 90_000,
  // Every assertion here waits on the extension host: a round trip through the
  // language server or the debug adapter, not a DOM update.
  expect: { timeout: 15_000 },
  use: {
    trace: "on-first-retry",
    screenshot: "only-on-failure",
    // Wide enough to keep the editor title actions (the Run button) and the
    // explorer out of the overflow menus they collapse into when cramped.
    viewport: { width: 1400, height: 900 },
  },
  projects: hosts.map(({ name }, index) => ({
    name,
    testMatch: new RegExp(`${name}/.*\\.spec\\.ts`),
    use: { ...devices["Desktop Chrome"], baseURL: `http://localhost:${portOf(index)}` },
  })),
  webServer: hosts.map((_host, index) => ({
    command: command(index),
    url: `http://localhost:${portOf(index)}`,
    reuseExistingServer: !process.env.CI,
    // The first run downloads the VS Code web build.
    timeout: 300_000,
  })),
});
