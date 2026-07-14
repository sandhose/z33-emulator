import {
  enterDebugMode,
  expect,
  getCycleCount,
  test,
  waitForCompileSuccess,
} from "./fixtures";

// `Mod` resolves to the platform primary modifier; Playwright's
// `ControlOrMeta` matches it (Meta on macOS, Control elsewhere), same as the
// library's platform detection inside the page.
const MOD = "ControlOrMeta";

test.describe("Keyboard shortcuts", () => {
  test("Mod+Enter starts a debug session from edit mode", async ({
    cleanPage: page,
  }) => {
    await waitForCompileSuccess(page);
    await page.keyboard.press(`${MOD}+Enter`);
    await expect(page.getByRole("toolbar", { name: "Debug" })).toBeVisible();
  });

  test("Step shortcut (F8) advances the cycle counter", async ({
    cleanPage: page,
  }) => {
    await enterDebugMode(page);
    await expect.poll(() => getCycleCount(page)).toBe(0);
    await page.keyboard.press("F8");
    await expect.poll(() => getCycleCount(page)).toBeGreaterThan(0);
  });

  test("Mod+Shift+Enter returns to the editor", async ({
    cleanPage: page,
  }) => {
    await enterDebugMode(page);
    await page.keyboard.press(`${MOD}+Shift+Enter`);
    await expect(page.getByRole("toolbar", { name: "Edit" })).toBeVisible();
  });

  test("help dialog opens with '?' and Mod+/ and closes", async ({
    cleanPage: page,
  }) => {
    await waitForCompileSuccess(page);
    // Move focus out of the Monaco editor (which owns Mod+/) so the document
    // level shortcuts fire.
    await page.getByRole("toolbar", { name: "Edit" }).click();

    // Open with "?" (Shift+/).
    await page.keyboard.press("Shift+Slash");
    const dialog = page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    await expect(
      dialog.getByRole("heading", { name: "Keyboard shortcuts" }),
    ).toBeVisible();
    // Every group is present.
    await expect(dialog.getByRole("heading", { name: "Session" })).toBeVisible();
    await expect(
      dialog.getByRole("heading", { name: "Panels" }),
    ).toBeVisible();

    // Escape closes it.
    await page.keyboard.press("Escape");
    await expect(dialog).toBeHidden();

    // Re-open with Mod+/.
    await page.getByRole("toolbar", { name: "Edit" }).click();
    await page.keyboard.press(`${MOD}+Slash`);
    await expect(page.getByRole("dialog")).toBeVisible();
    await page.getByRole("button", { name: "Close" }).click();
    await expect(page.getByRole("dialog")).toBeHidden();
  });
});
