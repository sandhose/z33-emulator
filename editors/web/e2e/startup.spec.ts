import { expect, test } from "./fixtures";

test.describe("Startup", () => {
  test("shows the toolbar and a pending check before Monaco arrives", async ({
    page,
  }) => {
    // Hold back the two modules that carry Monaco, so the assertions below see
    // the shell without the editor instead of racing the (fast, local) load.
    // The delay only starts once the app asks for Monaco, which is after React
    // has committed the toolbar, so the whole two seconds are assertion budget.
    await page.route(
      (url) => /\/app\/monaco(-file-editor)?\.tsx?/u.test(url.pathname),
      async (route) => {
        await new Promise((resolve) => {
          setTimeout(resolve, 2000);
        });
        await route.continue();
      },
    );

    await page.goto("/", { waitUntil: "commit" });

    await expect(page.getByRole("toolbar", { name: "Edit" })).toBeVisible();
    await expect(page.locator(".monaco-editor")).toHaveCount(0);
    await expect(page.getByRole("status", { name: "Compiling" })).toBeVisible();
    await expect(page.getByText("Loading the editor…")).toBeVisible();

    await expect(page.locator(".monaco-editor")).toBeVisible({
      timeout: 30_000,
    });
    await expect(
      page.getByRole("status", { name: "Compilation succeeded" }),
    ).toBeVisible({ timeout: 30_000 });
    await expect(page.getByRole("status", { name: "Compiling" })).toBeHidden();
  });
});
