import { expect, test } from "./fixtures";
import type { Locator, Page } from "@playwright/test";

/**
 * The theme switcher is a ToggleGroup inside the edit toolbar.
 * Buttons are in order: light (0), system (1), dark (2).
 * We locate them by index within the last toggle group in the toolbar.
 */
const THEME_INDEX = { light: 0, system: 1, dark: 2 } as const;

function themeButton(
  page: Page,
  value: "light" | "system" | "dark",
): Locator {
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
