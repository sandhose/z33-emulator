import { WORKSPACE_STORAGE_KEY } from "../app/stores/persist-keys";
import {
  enterDebugMode,
  exitDebugMode,
  expect,
  getCycleCount,
  SELECT_ALL,
  test,
  waitForCompileError,
  waitForCompileSuccess,
} from "./fixtures";

test.describe("Core flows", () => {
  test("app loads with default samples", async ({ cleanPage: page }) => {
    await expect(page.locator(".monaco-editor")).toBeVisible();
    await expect(page.getByRole("navigation", { name: "Files" })).toBeVisible();
    await expect(page.getByRole("button", { name: "fact.s" })).toBeVisible();
    await expect(page.getByRole("button", { name: "handler.s" })).toBeVisible();
  });

  test("fact.s auto-compiles successfully", async ({ cleanPage: page }) => {
    await waitForCompileSuccess(page);
    await expect(
      page.getByRole("button", { name: "Run", exact: true }),
    ).toBeVisible();
  });

  test("compilation error and recovery", async ({ cleanPage: page }) => {
    await waitForCompileSuccess(page);

    // Create a new file with only invalid content
    await page.getByRole("button", { name: "New file" }).click();
    const nameInput = page.getByRole("textbox", { name: "File name" });
    await nameInput.fill("err.s");
    await nameInput.press("Enter");

    // Delete the original files so only err.s remains (which is empty → no error yet)
    // Instead, type invalid content into the new file via the editor
    // Click the editor area to focus it, then type
    await page.locator(".view-lines").first().click();
    await page.keyboard.type("invalid garbage !!!");

    await waitForCompileError(page);

    // Fix it: select all and replace with valid assembly
    await page.keyboard.press(SELECT_ALL);
    await page.keyboard.type("main: reset");

    await waitForCompileSuccess(page);
  });

  test("a transient compile error keeps the Run controls in place", async ({
    cleanPage: page,
  }) => {
    await waitForCompileSuccess(page);
    const run = page.getByRole("button", { name: "Run", exact: true });
    await expect(run).toBeEnabled();

    await page.locator(".monaco-editor").click();
    await page.keyboard.press("Control+End");
    await page.keyboard.press("Enter");
    await page.keyboard.type("sh");
    await waitForCompileError(page);
    await expect(run).toBeDisabled();
    await expect(
      page.getByRole("combobox", { name: "Entrypoint" }),
    ).toBeVisible();
  });

  test("chosen entrypoint survives a change to the label set", async ({
    cleanPage: page,
  }) => {
    await waitForCompileSuccess(page);
    const entrypoint = page.getByRole("combobox", { name: "Entrypoint" });
    await entrypoint.click();
    await page.getByRole("option", { name: "casparticulier" }).click();
    await expect(entrypoint).toContainText("casparticulier");

    // Adding a label recompiles with a different label set.
    await page.locator(".monaco-editor").click();
    await page.keyboard.press("Control+End");
    await page.keyboard.press("Enter");
    await page.keyboard.type("newlabel:");

    // The check trails the keystroke by the sync and compile debounces, so the
    // status on screen is still the one from before the edit. The new label
    // reaching the list is the recompile itself.
    await entrypoint.click();
    await expect(page.getByRole("option", { name: "newlabel" })).toBeVisible();
    await page.keyboard.press("Escape");

    await waitForCompileSuccess(page);
    await expect(entrypoint).toContainText("casparticulier");
  });

  test("Run executes the buffer, not the bytes the last check saw", async ({
    cleanPage: page,
  }) => {
    await waitForCompileSuccess(page);
    await page.locator(".monaco-editor").click();
    await page.keyboard.press("Control+End");
    await page.keyboard.press("Enter");
    await page.keyboard.type("garbage !!!");

    // No wait: the toolbar still reports the program from before the edit, so
    // Run is enabled on a program that no longer compiles.
    await page.getByRole("button", { name: "Run", exact: true }).click();

    await waitForCompileError(page);
    await expect(page.getByRole("toolbar", { name: "Debug" })).toBeHidden();
  });

  test("an edit one click before Run survives the switch into debug mode", async ({
    cleanPage: page,
  }) => {
    await waitForCompileSuccess(page);
    await page.locator(".monaco-editor").click();
    await page.keyboard.press("Control+End");
    await page.keyboard.press("Enter");
    await page.keyboard.type("marker:");
    await page.getByRole("button", { name: "Run", exact: true }).click();
    await expect(page.getByRole("toolbar", { name: "Debug" })).toBeVisible();
    await exitDebugMode(page);

    // What a reload restores from: the edit has to have reached the store
    // before the editor unmounted, not just Monaco's model.
    const stored = await page.evaluate(
      (key) => localStorage.getItem(key),
      WORKSPACE_STORAGE_KEY,
    );
    expect(stored).toContain("marker:");
  });

  test("debug session lifecycle", async ({ cleanPage: page }) => {
    await enterDebugMode(page);

    const debugToolbar = page.getByRole("toolbar", { name: "Debug" });
    await expect(page.getByRole("region", { name: "Registers" })).toBeVisible();

    // Step once (execution is driven by the worker, so cycles update async)
    await debugToolbar
      .getByRole("button", { name: "Step", exact: true })
      .click();
    await expect.poll(() => getCycleCount(page)).toBeGreaterThan(0);

    // Exit debug mode
    await exitDebugMode(page);
  });
});
