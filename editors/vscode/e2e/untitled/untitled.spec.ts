import { expect, type Page, test } from "@playwright/test";
import {
  expectActiveTab,
  expectStopped,
  openWorkbench,
  pickQuickInput,
  runCommand,
  type Stop,
  stopSession,
  typeInEditor,
  waitForLanguageServer,
} from "../workbench";

const UNTITLED_STOP: Stop = { label: "main", line: 2, file: "Untitled-1" };

/** A new unsaved buffer in the Zorglub33 language, holding a runnable program. */
async function newZ33Buffer(page: Page): Promise<void> {
  await runCommand(page, "File: New Untitled Text File");
  await runCommand(page, "Change Language Mode");
  await pickQuickInput(page, "Zorglub33 Assembly", "Zorglub33");
  await typeInEditor(page, "main:\nld 5, %a\nreset\n");
  await waitForLanguageServer(page, "main");
}

test.beforeEach(async ({ page }) => {
  await openWorkbench(page);
});

test("an untitled document can be debugged", async ({ page }) => {
  await newZ33Buffer(page);
  await page.keyboard.press("F5");
  await expectStopped(page, UNTITLED_STOP);
  await expectActiveTab(page, "Untitled-1");
  await stopSession(page);
});

test("the Run button starts an untitled buffer without asking where to save it", async ({
  page,
}) => {
  await newZ33Buffer(page);
  await page.locator(".editor-actions .action-label[aria-label='Run Current File']").click();
  const input = page.locator(".quick-input-widget input");
  await expect(input).toBeVisible();
  // A Save As dialog would take the quick input over before the entrypoint
  // prompt ever opens.
  await expect(input).toHaveValue("main");
  await page.keyboard.press("Enter");
  await expectStopped(page, UNTITLED_STOP);
  await expectActiveTab(page, "Untitled-1");
  await stopSession(page);
});
