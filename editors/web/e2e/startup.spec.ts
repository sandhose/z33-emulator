import { expect, test } from "./fixtures";

test.describe("Startup", () => {
  test("checks the program while the editor chunk is held back", async ({
    page,
  }) => {
    // The two modules that carry Monaco wait on a gate this test opens, so the
    // assertions below are about ordering rather than about winning a race
    // against a local server.
    let releaseMonaco = (): void => {
      throw new Error("the Monaco gate was released before it was created");
    };
    const monacoGate = new Promise<void>((resolve) => {
      releaseMonaco = resolve;
    });
    await page.route(
      (url) => /\/app\/monaco(-file-editor)?\.tsx?/u.test(url.pathname),
      async (route) => {
        await monacoGate;
        await route.continue();
      },
    );

    await page.goto("/", { waitUntil: "commit" });

    await expect(page.getByRole("toolbar", { name: "Edit" })).toBeVisible();
    await expect(page.locator(".monaco-editor")).toHaveCount(0);
    await expect(page.getByText("Loading the editor…")).toBeVisible();

    // The check reads the file store, so it answers with the editor chunk
    // still on the wire.
    await expect(
      page.getByRole("status", { name: "Compilation succeeded" }),
    ).toBeVisible({ timeout: 30_000 });
    await expect(page.locator(".monaco-editor")).toHaveCount(0);

    releaseMonaco();
    await expect(page.locator(".monaco-editor")).toBeVisible({
      timeout: 30_000,
    });
  });
});
