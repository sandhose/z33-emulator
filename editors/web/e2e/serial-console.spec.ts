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

test.describe("Serial console", () => {
  test("echoes typed input back to the console", async ({ page }) => {
    await loadWorkspace(page, { "echo.s": ECHO_PROGRAM }, "echo.s");
    await enterDebugMode(page);

    // Start continuous execution so the program busy-polls for input.
    await page
      .getByRole("toolbar", { name: "Debug" })
      .getByRole("button", { name: "Run", exact: true })
      .click();

    const console = page.getByRole("textbox", { name: "Serial console output" });
    await expect(console).toBeVisible();

    // Focus the console and type: the program should echo each byte back.
    await console.click();
    await page.keyboard.type("hi");

    await expect(console).toHaveText(/hi/);

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
