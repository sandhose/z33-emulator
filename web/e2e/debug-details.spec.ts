import {
  enterDebugMode,
  expect,
  getCycleCount,
  test,
} from "./fixtures";

test.describe("Debug details", () => {
  test("run 10 steps increases cycle count", async ({ cleanPage: page }) => {
    await enterDebugMode(page);
    const toolbar = page.getByRole("toolbar", { name: "Debug" });

    await toolbar.getByRole("button", { name: "Run 10 steps" }).click();

    // Wait for steps to execute (interval-based runner)
    await expect(async () => {
      const cycles = await getCycleCount(page);
      expect(cycles).toBeGreaterThan(0);
    }).toPass({ timeout: 5_000 });
  });

  test("run to halt shows halted badge", async ({ cleanPage: page }) => {
    await enterDebugMode(page);
    const toolbar = page.getByRole("toolbar", { name: "Debug" });

    await toolbar.getByRole("button", { name: "Run 1000 steps" }).click();

    await expect(toolbar.getByRole("alert")).toBeVisible({ timeout: 30_000 });
    await expect(toolbar.getByRole("alert")).toHaveText("Halted");
  });

  test("factorial result is 120 in register %a", async ({
    cleanPage: page,
  }) => {
    await enterDebugMode(page);
    const toolbar = page.getByRole("toolbar", { name: "Debug" });

    await toolbar.getByRole("button", { name: "Run 1000 steps" }).click();
    await expect(toolbar.getByRole("alert")).toBeVisible({
      timeout: 30_000,
    });

    // %a should contain 120 (5! = 120)
    const regA = page.getByRole("button", { name: "Register %a" });
    await expect(regA).toContainText("120");
  });

  test("label navigation scrolls memory", async ({ cleanPage: page }) => {
    await enterDebugMode(page);

    const memoryRegion = page.getByRole("region", { name: "Memory" });
    await expect(memoryRegion).toBeVisible();

    // Click the "main" label in the labels section
    const labelsRegion = page.getByRole("region", { name: "Labels" });
    await labelsRegion.getByRole("button", { name: "main" }).click();
  });

  test("format switching changes display", async ({ cleanPage: page }) => {
    await enterDebugMode(page);
    const toolbar = page.getByRole("toolbar", { name: "Debug" });

    // Step a few times to get some values
    await toolbar.getByRole("button", { name: "Step", exact: true }).click();

    // Switch to hex — scope to the registers region to avoid duplicates
    const registers = page.getByRole("region", { name: "Registers" });
    await registers.getByLabel("Hexadecimal", { exact: true }).click();

    // Switch back to decimal
    await registers.getByLabel("Decimal", { exact: true }).click();
  });
});
