import { test as base, expect } from "@playwright/test";
import type { Page } from "@playwright/test";

// Chromium always uses Control for keyboard shortcuts, even on macOS
export const SELECT_ALL = "Control+a";

export async function waitForCompileSuccess(page: Page): Promise<void> {
  await expect(
    page.getByRole("status", { name: "Compilation succeeded" }),
  ).toBeVisible();
}

export async function waitForCompileError(page: Page): Promise<void> {
  await expect(
    page.getByRole("status", { name: "Compilation error" }),
  ).toBeVisible();
}

export async function enterDebugMode(page: Page): Promise<void> {
  await waitForCompileSuccess(page);
  await page.getByRole("button", { name: "Run" }).click();
  await expect(
    page.getByRole("toolbar", { name: "Debug" }),
  ).toBeVisible();
}

export async function exitDebugMode(page: Page): Promise<void> {
  await page.getByRole("button", { name: "Edit" }).click();
  await expect(
    page.getByRole("toolbar", { name: "Edit" }),
  ).toBeVisible();
}

export async function getCycleCount(page: Page): Promise<number> {
  const text = await page.getByLabel("Cycle count").textContent();
  return Number(text?.replace(/\D/g, "") ?? "0");
}

export const test = base.extend<{ cleanPage: Page }>({
  cleanPage: async ({ page }, use) => {
    await page.goto("/");
    await page.evaluate(() =>
      localStorage.removeItem("z33:workspace-v2"),
    );
    await page.reload();
    await page.waitForSelector(".monaco-editor", { timeout: 30_000 });
    await use(page);
  },
});

export { expect };
