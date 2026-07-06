import { enterDebugMode, expect, loadWorkspace, test } from "./fixtures";

// A polling serial echo: it busy-waits on the serial console and echoes every
// received byte back to the host, stopping on EOT (Ctrl-D, byte 4).
const ECHO_PROGRAM = `#define STATUS 110
#define DATA   111
#define RX_READY 1
#define EOT      4

.addr 1000
main:
poll:
    in  [STATUS], %a
    and RX_READY, %a
    jeq poll

    in  [DATA], %a
    cmp EOT, %a
    jeq done
    out %a, [DATA]
    jmp poll

done:
    reset
`;

// Read the visible terminal buffer through the test hook the component exposes
// (`window.__z33Terminal`). xterm renders to a canvas-less DOM grid, so there is
// no accessible text node to assert on — we read the buffer directly instead.
async function terminalText(page: import("@playwright/test").Page): Promise<string> {
  return page.evaluate(() => {
    const terminal = (globalThis as { __z33Terminal?: unknown }).__z33Terminal as
      | {
          buffer: {
            active: {
              length: number;
              getLine: (
                i: number,
              ) => { translateToString: (trim: boolean) => string } | undefined;
            };
          };
        }
      | undefined;
    if (!terminal) return "";
    const { active } = terminal.buffer;
    const lines: string[] = [];
    for (let i = 0; i < active.length; i++) {
      lines.push(active.getLine(i)?.translateToString(true) ?? "");
    }
    return lines.join("\n");
  });
}

test.describe("Serial console", () => {
  test("echoes typed input back to the console", async ({ page }) => {
    await loadWorkspace(page, { "echo.s": ECHO_PROGRAM }, "echo.s");
    await enterDebugMode(page);

    // Start continuous execution so the program busy-polls for input.
    await page
      .getByRole("toolbar", { name: "Debug" })
      .getByRole("button", { name: "Run", exact: true })
      .click();

    // Click the terminal screen to focus it (xterm moves focus to its hidden
    // helper textarea, which receives the keystrokes).
    const screen = page.locator(".xterm-screen");
    await expect(screen).toBeVisible();
    await screen.click();

    // Type: the program should echo each byte back into the terminal buffer.
    await page.keyboard.type("hi");
    await expect
      .poll(() => terminalText(page), { timeout: 10_000 })
      .toContain("hi");

    // Ctrl-D (EOT) ends the program.
    await page.keyboard.press("Control+d");
    await expect(
      page.getByRole("toolbar", { name: "Debug" }).getByRole("button", {
        name: "Run",
        exact: true,
      }),
    ).toBeVisible();
  });

  test("normalizes newlines within a paste to line feeds", async ({
    page,
  }) => {
    await loadWorkspace(page, { "echo.s": ECHO_PROGRAM }, "echo.s");
    await enterDebugMode(page);

    await page
      .getByRole("toolbar", { name: "Debug" })
      .getByRole("button", { name: "Run", exact: true })
      .click();

    const screen = page.locator(".xterm-screen");
    await expect(screen).toBeVisible();
    await screen.click();

    // Drive xterm's own `paste()` API instead of `keyboard.type`: real browser
    // paste events go through the same internal normalization (embedded
    // newlines become a bare `\r`), so this exercises the exact input our
    // `translateInput` regression fix targets, without needing a real
    // clipboard/paste simulation.
    await page.evaluate(() => {
      const terminal = (
        globalThis as { __z33Terminal?: { paste: (data: string) => void } }
      ).__z33Terminal;
      terminal?.paste("hi\nbye");
    });

    // With newlines correctly translated to LF, the echoed bytes produce two
    // separate lines. A regression that forwards the raw CR byte instead
    // would make "bye" overwrite "hi" in place (bare CR just returns the
    // cursor to column 0), collapsing both into a single line.
    await expect
      .poll(() => terminalText(page), { timeout: 10_000 })
      .toMatch(/hi\nbye/);

    // Ctrl-D (EOT) ends the program.
    await page.keyboard.press("Control+d");
    await expect(
      page.getByRole("toolbar", { name: "Debug" }).getByRole("button", {
        name: "Run",
        exact: true,
      }),
    ).toBeVisible();
  });
});
