import { expect, loadWorkspace, test } from "./fixtures";

test.describe("LSP", () => {
  test("diagnostics appear for a syntax error", async ({ page }) => {
    await loadWorkspace(
      page,
      { "bad.s": "this is not valid z33 !!!\n" },
      "bad.s",
    );

    // LSP publishes diagnostics -> Monaco renders squiggles.
    await expect
      .poll(
        () =>
          page.evaluate(
            () =>
              document.querySelectorAll(
                ".squiggly-error, .squiggly-warning, .squiggly-info",
              ).length,
          ),
        { timeout: 10_000 },
      )
      .toBeGreaterThan(0);
  });

  test("hover shows instruction docs", async ({ page }) => {
    // Cold wasm/LSP-worker startup plus module transforms on small CI runners
    // make this the slowest LSP spec; give it extra headroom.
    test.slow();
    await loadWorkspace(
      page,
      { "main.s": "main:\n    ld 1, %a\n    reset\n" },
      "main.s",
    );

    // Drive Monaco through its API (dev-only `__z33e2e` hook) instead of
    // hovering the rendered token spans: their DOM slicing differs across
    // platforms/builds (an exact-text span locator matched locally but never
    // matched in CI, even with the editor fully rendered).
    await expect
      .poll(() => page.evaluate(() => "__z33e2e" in window), {
        timeout: 90_000,
      })
      .toBe(true);

    // The LSP runs in a web worker that compiles the wasm on first load, so the
    // hover round-trip can be slow to become ready on CI. Retry the hover until
    // the LSP responds with the instruction docs instead of assuming a delay.
    const hover = page
      .locator(".monaco-hover")
      .filter({ hasText: /Load a value/i });
    await expect(async () => {
      // Column 6 sits inside the `ld` mnemonic on line 2.
      await page.evaluate(() => {
        (
          window as unknown as {
            __z33e2e: { showHoverAt(line: number, column: number): void };
          }
        ).__z33e2e.showHoverAt(2, 6);
      });
      await expect(hover).toBeVisible({ timeout: 5_000 });
    }).toPass({ timeout: 60_000 });
  });

  test("go to definition jumps to the label", async ({ page }) => {
    // The LSP worker can be slow to become ready on CI.
    test.slow();
    await loadWorkspace(
      page,
      { "nav.s": "main:\n    jmp target\n    reset\ntarget:\n    rtn\n" },
      "nav.s",
    );

    // Wait for the `target` reference token with an auto-retrying locator
    // (no one-shot DOM scan + id tag, which Monaco line re-renders destroy).
    // `.first()` is the reference in `jmp target`, not the `target:` label.
    const target = page
      .locator(".view-lines span")
      .filter({ hasText: /^target$/ })
      .first();
    await expect(target).toBeVisible({ timeout: 90_000 });
    await target.scrollIntoViewIfNeeded();
    await target.click();

    // The LSP worker can be slow to become ready on CI, so keep issuing
    // go-to-definition until it responds. Navigation is async, so each poll
    // presses F12 and then reads where the cursor ended up on the *previous*
    // press; once the LSP resolves, the cursor lands on the definition line
    // (line 4: "target:") and stays there.
    await expect
      .poll(
        async () => {
          await page.keyboard.press("F12");
          return page.evaluate(
            () =>
              document.querySelector(".line-numbers.active-line-number")
                ?.textContent ?? null,
          );
        },
        { timeout: 30_000 },
      )
      .toBe("4");
  });

  test("completion suggests a mnemonic", async ({ page }) => {
    await loadWorkspace(page, { "c.s": "main:\n    reset\n" }, "c.s");
    await page.waitForTimeout(500);

    await page.locator(".view-lines").click();
    await page.keyboard.press("Control+End");
    await page.keyboard.press("Enter");
    await page.keyboard.type("re");
    await page.keyboard.press("Control+Space");

    const widget = page.locator(".suggest-widget .monaco-list-row");
    await expect(widget.filter({ hasText: "reset" }).first()).toBeVisible({
      timeout: 5_000,
    });
  });
});
