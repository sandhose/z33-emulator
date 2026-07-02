import {
  enterDebugMode,
  expect,
  loadWorkspace,
  test,
  toggleBreakpointOnLine,
} from "./fixtures";

test.describe("Breakpoints & fast run", () => {
  test("breakpoint hit pauses at the right line", async ({
    cleanPage: page,
  }) => {
    // Default samples: fact.s. Set a breakpoint in factorielle's body.
    await toggleBreakpointOnLine(page, "[%sp+1],%a");
    await expect
      .poll(() =>
        page.evaluate(
          () =>
            document.querySelectorAll(".bp-glyph, .bp-glyph-unverified").length,
        ),
      )
      .toBeGreaterThan(0);

    await enterDebugMode(page);
    const toolbar = page.getByRole("toolbar", { name: "Debug" });
    await toolbar.getByRole("button", { name: "Run", exact: true }).click();

    // It should stop (paused, not halted) with the current-line highlight on the
    // breakpoint line.
    const highlightedLine = async () =>
      page.evaluate(() => {
        const el = document.querySelector(".debug-line-highlight");
        if (!el) return null;
        const top = el.getBoundingClientRect().top;
        const line = [...document.querySelectorAll(".view-line")].find(
          (l) => Math.abs(l.getBoundingClientRect().top - top) < 4,
        );
        return (line?.textContent ?? "").replace(/\s/g, "");
      });

    await expect
      .poll(highlightedLine, { timeout: 10_000 })
      .toContain("[%sp+1],%a");
    await expect(toolbar.getByRole("alert")).toHaveCount(0);
  });

  test("fast run completes a large loop quickly", async ({ page }) => {
    // ~200k iterations (~600k instructions).
    await loadWorkspace(
      page,
      {
        "loop.s":
          "main:\n    ld 0, %a\nloop:\n    add 1, %a\n    cmp 200000, %a\n    jne loop\n    reset\n",
      },
      "loop.s",
    );

    await enterDebugMode(page);
    const toolbar = page.getByRole("toolbar", { name: "Debug" });

    const start = Date.now();
    await toolbar.getByRole("button", { name: "Run", exact: true }).click();
    await expect(toolbar.getByRole("alert")).toHaveText("Halted", {
      timeout: 15_000,
    });
    const elapsed = Date.now() - start;
    // Even on the unoptimized dev wasm this finishes in ~1s; give ample margin.
    expect(elapsed).toBeLessThan(10_000);

    await expect(
      page.getByRole("button", { name: "Register %a" }),
    ).toContainText("200000");
  });
});
