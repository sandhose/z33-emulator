import { expect, test } from "./fixtures";
import type { Locator, Page } from "@playwright/test";

/**
 * The theme switcher is a ToggleGroup inside the edit toolbar.
 * Buttons are in order: light (0), system (1), dark (2).
 * We locate them by index within the last toggle group in the toolbar.
 */
const THEME_INDEX = { light: 0, system: 1, dark: 2 } as const;

const inlineColorScheme = (page: Page): Promise<string> =>
  page.evaluate(() => document.documentElement.style.colorScheme);

const inlineBackground = (page: Page): Promise<string> =>
  page.evaluate(() => document.documentElement.style.backgroundColor);

function themeButton(page: Page, value: "light" | "system" | "dark"): Locator {
  return page
    .getByRole("toolbar", { name: "Edit" })
    .locator("[data-slot='toggle-group']")
    .last()
    .locator("button")
    .nth(THEME_INDEX[value]);
}

test.describe("Theme switching", () => {
  test("defaults to system theme and applies dark when system prefers dark", async ({
    cleanPage: page,
  }) => {
    await page.emulateMedia({ colorScheme: "dark" });
    await expect(themeButton(page, "system")).toHaveAttribute(
      "aria-pressed",
      "true",
    );
    await expect(page.locator("html")).toHaveClass(/dark/);
  });

  test("defaults to system theme and applies light when system prefers light", async ({
    cleanPage: page,
  }) => {
    await page.emulateMedia({ colorScheme: "light" });
    await expect(themeButton(page, "system")).toHaveAttribute(
      "aria-pressed",
      "true",
    );
    await expect(page.locator("html")).toHaveClass(/light/);
  });

  test("switching to dark theme applies dark class", async ({
    cleanPage: page,
  }) => {
    await page.emulateMedia({ colorScheme: "light" });
    await expect(page.locator("html")).toHaveClass(/light/);

    await themeButton(page, "dark").click();

    await expect(page.locator("html")).toHaveClass(/dark/);
    await expect(page.locator("html")).not.toHaveClass(/light/);
  });

  test("switching to light theme applies light class", async ({
    cleanPage: page,
  }) => {
    await page.emulateMedia({ colorScheme: "dark" });
    await expect(page.locator("html")).toHaveClass(/dark/);

    await themeButton(page, "light").click();

    await expect(page.locator("html")).toHaveClass(/light/);
    await expect(page.locator("html")).not.toHaveClass(/dark/);
  });

  test("system theme reacts to media query changes", async ({
    cleanPage: page,
  }) => {
    await page.emulateMedia({ colorScheme: "light" });
    await expect(page.locator("html")).toHaveClass(/light/);

    await page.emulateMedia({ colorScheme: "dark" });
    await expect(page.locator("html")).toHaveClass(/dark/);
    await expect(page.locator("html")).not.toHaveClass(/light/);

    await page.emulateMedia({ colorScheme: "light" });
    await expect(page.locator("html")).toHaveClass(/light/);
    await expect(page.locator("html")).not.toHaveClass(/dark/);
  });

  test("explicit theme ignores media query changes", async ({
    cleanPage: page,
  }) => {
    await page.emulateMedia({ colorScheme: "light" });

    await themeButton(page, "dark").click();
    await expect(page.locator("html")).toHaveClass(/dark/);

    // System changes — should stay dark
    await page.emulateMedia({ colorScheme: "light" });
    await expect(page.locator("html")).toHaveClass(/dark/);

    await page.emulateMedia({ colorScheme: "dark" });
    await expect(page.locator("html")).toHaveClass(/dark/);
  });

  test("theme persists across page reloads", async ({ cleanPage: page }) => {
    await page.emulateMedia({ colorScheme: "light" });

    await themeButton(page, "dark").click();
    await expect(page.locator("html")).toHaveClass(/dark/);

    await page.reload();
    await page.waitForSelector(".monaco-editor", { timeout: 30_000 });

    await expect(page.locator("html")).toHaveClass(/dark/);
    await expect(themeButton(page, "dark")).toHaveAttribute(
      "aria-pressed",
      "true",
    );
  });

  test("applies the stored theme before the app script runs", async ({
    page,
  }) => {
    await page.addInitScript(() => {
      localStorage.setItem(
        "z33:theme",
        JSON.stringify({ state: { theme: "dark" }, version: 0 }),
      );
    });
    // With the app module blocked, only index.html's bootstrap script runs, so
    // what the assertions see cannot have come from the store.
    await page.route(/index\.tsx/u, (route) => route.abort());
    await page.emulateMedia({ colorScheme: "light" });

    await page.goto("/", { waitUntil: "commit" });

    await expect(page.locator("html")).toHaveClass(/dark/);
    await expect(page.getByRole("toolbar", { name: "Edit" })).toHaveCount(0);
    await expect.poll(() => inlineColorScheme(page)).toBe("dark");
    // The bootstrap paints the background itself, before the stylesheet lands.
    await expect.poll(() => inlineBackground(page)).not.toBe("");
  });

  test("switching theme moves the inline color-scheme with it", async ({
    cleanPage: page,
  }) => {
    await page.emulateMedia({ colorScheme: "light" });

    await themeButton(page, "dark").click();
    await expect(page.locator("html")).toHaveClass(/dark/);
    await expect.poll(() => inlineColorScheme(page)).toBe("dark");

    await themeButton(page, "light").click();
    await expect(page.locator("html")).toHaveClass(/light/);
    await expect.poll(() => inlineColorScheme(page)).toBe("light");
    // Once the stylesheet is in charge, the bootstrap's background is gone.
    await expect.poll(() => inlineBackground(page)).toBe("");
  });

  test("switching back to system respects current media query", async ({
    cleanPage: page,
  }) => {
    await page.emulateMedia({ colorScheme: "dark" });

    await themeButton(page, "light").click();
    await expect(page.locator("html")).toHaveClass(/light/);

    await themeButton(page, "system").click();
    await expect(page.locator("html")).toHaveClass(/dark/);
  });
});
