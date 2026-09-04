import { expect, test } from "@playwright/test";
import {
  expectNoSession,
  expectStopped,
  openFile,
  openWorkbench,
  startConfiguration,
  type Stop,
  stopSession,
  waitForLanguageServer,
} from "../workbench";

test.beforeEach(async ({ page }) => {
  await openWorkbench(page);
  await openFile(page, "fact.s");
  await waitForLanguageServer(page, "factorielle");
});

const FACT_MAIN: Stop = { label: "main", line: 2, file: "fact.s" };

const configurations: { name: string; stop: Stop }[] = [
  { name: "workspaceFolder spelling", stop: FACT_MAIN },
  { name: "relative spelling", stop: { label: "factorielle", line: 7, file: "fact.s" } },
  { name: "fileDirname spelling", stop: FACT_MAIN },
];

for (const { name, stop } of configurations) {
  test(`launch.json: ${name}`, async ({ page }) => {
    await startConfiguration(page, name);
    await expectStopped(page, stop);
    await expect(page.locator(".monaco-dialog-box")).toBeHidden();
    await stopSession(page);
  });
}

// `double.s` is the active editor here and the only file declaring `double`, so
// the session can only come up if `${file}` resolved to it rather than to the
// file the other configurations name.
test("launch.json: file spelling", async ({ page }) => {
  await openFile(page, "double.s");
  await startConfiguration(page, "file spelling");
  await expectStopped(page, { label: "double", line: 4, file: "double.s" });
  await expect(page.locator(".monaco-dialog-box")).toBeHidden();
  await stopSession(page);
});

test("launch.json: a missing program names the files that exist", async ({ page }) => {
  await startConfiguration(page, "missing program");
  const dialog = page.locator(".monaco-dialog-box");
  await expect(dialog).toBeVisible();
  await expect(dialog).toContainText("nope.s");
  await expect(dialog).toContainText("fact.s");
  await dialog.getByRole("button", { name: "Cancel" }).click();
  await expectNoSession(page);
});
