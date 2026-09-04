import { expect, type Page, test } from "@playwright/test";
import {
  expectNoSession,
  expectStopped,
  focusTab,
  openFile,
  openWorkbench,
  pickQuickInput,
  runCommand,
  startConfiguration,
  type Stop,
  stopSession,
  typeAtLineStart,
  waitForLanguageServer,
} from "../workbench";

/** Where `fact.s` and `echo.s` stop when entered at their `main` label. */
const FACT_MAIN: Stop = { label: "main", line: 2, file: "fact.s" };
const ECHO_MAIN: Stop = { label: "poll", line: 16, file: "echo.s" };

const EDITED_STOP: Stop = { label: "main", line: 4, file: "fact.s" };

/**
 * Push `main` two lines down: where `fact.s` stops then says whether the
 * adapter read the edit or the file it started from.
 */
async function editFact(page: Page): Promise<void> {
  await typeAtLineStart(page, "main:", "\n\n");
}

test.beforeEach(async ({ page }) => {
  await openWorkbench(page);
});

test("F5 debugs the active .s file without a launch.json", async ({ page }) => {
  await openFile(page, "fact.s");
  await waitForLanguageServer(page, "factorielle");
  await page.keyboard.press("F5");
  await expectStopped(page, FACT_MAIN);
  await expect(page.locator(".monaco-dialog-box")).toBeHidden();
  await stopSession(page);
});

test("a second F5 starts a fresh session once the first was stopped", async ({ page }) => {
  await openFile(page, "fact.s");
  await waitForLanguageServer(page, "factorielle");
  await page.keyboard.press("F5");
  await expectStopped(page, FACT_MAIN);
  await stopSession(page);
  await page.keyboard.press("F5");
  await expectStopped(page, FACT_MAIN);
  await stopSession(page);
});

test("F5 follows the editor that has focus", async ({ page }) => {
  await openFile(page, "fact.s");
  await waitForLanguageServer(page, "factorielle");
  await openFile(page, "echo.s");
  await expect(page.locator(".editor-group-container.active .tab")).toHaveCount(2);

  await focusTab(page, "fact.s");
  await page.keyboard.press("F5");
  await expectStopped(page, FACT_MAIN);
  await stopSession(page);

  await focusTab(page, "echo.s");
  await page.keyboard.press("F5");
  await expectStopped(page, ECHO_MAIN);
  await stopSession(page);
});

test("the dynamic configuration lists the active file", async ({ page }) => {
  await openFile(page, "fact.s");
  await waitForLanguageServer(page, "factorielle");
  await startConfiguration(page, "Run fact.s", "Zorglub33 Debug");
  await expectStopped(page, FACT_MAIN);
  await stopSession(page);
});

// No editor is opened first, so this also covers activating the extension from
// the debug request alone.
test("asks for a .s file when none is open", async ({ page }) => {
  await page.keyboard.press("F5");
  // With no active editor VS Code asks which debugger to use.
  await pickQuickInput(page, "Zorglub33 Debug");
  await expect(
    page.locator(".notification-list-item-message", { hasText: "open a .s file to debug" }),
  ).toBeVisible();
  await expectNoSession(page);
});

test("the Run button asks for the entrypoint", async ({ page }) => {
  await openFile(page, "fact.s");
  await waitForLanguageServer(page, "factorielle");
  await page.locator(".editor-actions .action-label[aria-label='Run Current File']").click();
  const input = page.locator(".quick-input-widget input");
  await expect(input).toBeVisible();
  await expect(input).toHaveValue("main");
  await input.fill("factorielle");
  await page.keyboard.press("Enter");
  await expectStopped(page, { label: "factorielle", line: 7, file: "fact.s" });
  await stopSession(page);
});

// The workspace keeps VS Code's default `debug.saveBeforeStart`, so F5 saves
// the editor first and the adapter must see the saved text.
test("edits are picked up when F5 saves the file", async ({ page }) => {
  await openFile(page, "fact.s");
  await waitForLanguageServer(page, "factorielle");
  await editFact(page);
  await page.keyboard.press("F5");
  await expectStopped(page, EDITED_STOP);
  await stopSession(page);
});

test("a restart after an edit runs the new program", async ({ page }) => {
  await openFile(page, "fact.s");
  await waitForLanguageServer(page, "factorielle");
  await page.keyboard.press("F5");
  await expectStopped(page, FACT_MAIN);
  await focusTab(page, "fact.s");
  await editFact(page);
  await runCommand(page, "File: Save");
  await runCommand(page, "Debug: Restart");
  await expectStopped(page, EDITED_STOP);
  await stopSession(page);
});
