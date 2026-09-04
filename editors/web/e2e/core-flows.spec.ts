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
    await waitForCompileSuccess(page);
    await expect(entrypoint).toContainText("casparticulier");
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
