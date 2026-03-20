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
    await expect(
      page.getByRole("navigation", { name: "Files" }),
    ).toBeVisible();
    await expect(
      page.getByRole("button", { name: "fact.s" }),
    ).toBeVisible();
    await expect(
      page.getByRole("button", { name: "handler.s" }),
    ).toBeVisible();
  });

  test("fact.s auto-compiles successfully", async ({ cleanPage: page }) => {
    await waitForCompileSuccess(page);
    await expect(page.getByRole("button", { name: "Run" })).toBeVisible();
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

  test("debug session lifecycle", async ({ cleanPage: page }) => {
    await enterDebugMode(page);

    const debugToolbar = page.getByRole("toolbar", { name: "Debug" });
    await expect(
      page.getByRole("region", { name: "Registers" }),
    ).toBeVisible();

    // Step once
    await debugToolbar
      .getByRole("button", { name: "Step", exact: true })
      .click();
    const cycles = await getCycleCount(page);
    expect(cycles).toBeGreaterThan(0);

    // Exit debug mode
    await exitDebugMode(page);
  });
});
