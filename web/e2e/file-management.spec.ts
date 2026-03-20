import { expect, test } from "./fixtures";

test.describe("File management", () => {
  test("create a new file", async ({ cleanPage: page }) => {
    await page.getByRole("button", { name: "New file" }).click();

    const input = page.getByRole("textbox", { name: "File name" });
    await expect(input).toBeVisible();
    await input.fill("test-file.s");
    await input.press("Enter");

    await expect(
      page.getByRole("button", { name: "test-file.s" }),
    ).toBeVisible();
  });

  test("delete a file", async ({ cleanPage: page }) => {
    // Create a file first
    await page.getByRole("button", { name: "New file" }).click();
    const input = page.getByRole("textbox", { name: "File name" });
    await input.fill("to-delete.s");
    await input.press("Enter");

    await expect(
      page.getByRole("button", { name: "to-delete.s" }),
    ).toBeVisible();

    // Hover the file entry to reveal the delete button, then click it
    const fileEntry = page
      .getByRole("button", { name: "to-delete.s" })
      .locator("xpath=ancestor::div[contains(@class, 'group')]");
    await fileEntry.hover();
    await fileEntry.getByLabel("Delete").click();

    await expect(
      page.getByRole("button", { name: "to-delete.s" }),
    ).not.toBeVisible();
  });

  test("reset files to samples", async ({ cleanPage: page }) => {
    // Delete handler.s first
    const fileEntry = page
      .getByRole("button", { name: "handler.s" })
      .locator("xpath=ancestor::div[contains(@class, 'group')]");
    await fileEntry.hover();
    await fileEntry.getByLabel("Delete").click();

    await expect(
      page.getByRole("button", { name: "handler.s" }),
    ).not.toBeVisible();

    // Reset to samples
    await page.getByRole("button", { name: "Reset to samples" }).click();
    await page.getByRole("button", { name: "Reset" }).click();

    // Both sample files should be back
    await expect(
      page.getByRole("button", { name: "fact.s" }),
    ).toBeVisible();
    await expect(
      page.getByRole("button", { name: "handler.s" }),
    ).toBeVisible();
  });
});
