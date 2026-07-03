import { test as base, expect } from "@playwright/test";
import type { Page } from "@playwright/test";
import {
  WORKSPACE_PERSIST_VERSION,
  WORKSPACE_STORAGE_KEY,
} from "../app/stores/persist-keys";

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
  await expect(page.getByRole("toolbar", { name: "Debug" })).toBeVisible();
}

export async function exitDebugMode(page: Page): Promise<void> {
  await page.getByRole("button", { name: "Edit" }).click();
  await expect(page.getByRole("toolbar", { name: "Edit" })).toBeVisible();
}

export async function getCycleCount(page: Page): Promise<number> {
  const text = await page.getByLabel("Cycle count").textContent();
  return Number(text?.replace(/\D/g, "") ?? "0");
}

export const test = base.extend<{ cleanPage: Page }>({
  cleanPage: async ({ page }, use) => {
    await page.goto("/");
    await page.evaluate((storageKey) => {
      localStorage.removeItem(storageKey);
      localStorage.removeItem("z33:breakpoints");
    }, WORKSPACE_STORAGE_KEY);
    await page.reload();
    await page.waitForSelector(".monaco-editor", { timeout: 30_000 });
    await use(page);
  },
});

/** Replace the workspace with the given files and reload the editor. */
export async function loadWorkspace(
  page: Page,
  files: Record<string, string>,
  activeFile: string,
): Promise<void> {
  await page.goto("/");
  await page.evaluate(
    ({ files, activeFile, storageKey, version }) => {
      localStorage.setItem(
        storageKey,
        JSON.stringify({
          state: { files, activeFile, entrypoints: {} },
          version,
        }),
      );
      localStorage.removeItem("z33:breakpoints");
    },
    {
      files,
      activeFile,
      storageKey: WORKSPACE_STORAGE_KEY,
      version: WORKSPACE_PERSIST_VERSION,
    },
  );
  await page.reload();
  await page.waitForSelector(".monaco-editor", { timeout: 30_000 });
}

/**
 * Toggle a gutter breakpoint on the source line containing `lineText`. Pass a
 * whitespace-free substring: Monaco renders indentation with non-breaking
 * spaces, so all whitespace is stripped on both sides before matching.
 */
export async function toggleBreakpointOnLine(
  page: Page,
  lineText: string,
): Promise<void> {
  const target = await page.evaluate((text) => {
    const strip = (s: string) => s.replace(/\s/g, "");
    const needle = strip(text);
    const line = [...document.querySelectorAll(".view-line")].find((l) =>
      strip(l.textContent ?? "").includes(needle),
    );
    if (!line) return null;
    const r = line.getBoundingClientRect();
    const margin = document.querySelector(".glyph-margin");
    const mr = margin?.getBoundingClientRect();
    return { y: r.top + r.height / 2, x: (mr?.x ?? 0) + 6 };
  }, lineText);
  if (!target) throw new Error(`line not found: ${lineText}`);
  await page.mouse.click(target.x, target.y);
}

export { expect };
