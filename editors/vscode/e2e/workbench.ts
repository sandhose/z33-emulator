import { expect, type Locator, type Page } from "@playwright/test";

/** Where the top frame of a stopped session sits. */
export interface Stop {
  /** The label enclosing the program counter, as the adapter names the frame. */
  label: string;
  line: number;
  /** Base name of the source, as VS Code shows it in the call stack. */
  file: string;
}

/** VS Code keeps the editors of inactive groups in the DOM, so every editor
 * locator is scoped to the group the workbench commands act on. */
function activeGroup(page: Page): Locator {
  return page.locator(".editor-group-container.active");
}

function activeEditorLines(page: Page): Locator {
  return activeGroup(page).locator(".monaco-editor .view-lines");
}

/** Open the workbench and wait until the explorer has listed the folder. */
export async function openWorkbench(page: Page): Promise<void> {
  await page.goto("/");
  await expect(page.locator(".monaco-workbench")).toBeVisible({ timeout: 60_000 });
  await expect(page.locator(".explorer-folders-view .monaco-list-row").first()).toBeVisible({
    timeout: 60_000,
  });
  await runCommand(page, "View: Close All Editors");
}

/**
 * Take the entry labelled `label` from the open quick input, narrowing the list
 * with `filter` first when the picker holds more entries than it renders.
 * Entries are matched on their label element: their accessible name also
 * carries keybindings and group names, and the fuzzy ranking does not always
 * put an exact match first.
 */
export async function pickQuickInput(
  page: Page,
  label: string,
  filter?: string,
  prefix = "",
): Promise<void> {
  const input = page.locator(".quick-input-widget input");
  const rows = page.locator(".quick-input-list .monaco-list-row");
  await expect(input).toBeVisible();
  if (filter !== undefined) {
    // A pick that opens another picker swaps the widget's contents a beat
    // later, and keystrokes sent before then land in the editor underneath.
    // The picker's own prefix (">", "@") is the first thing it puts up; the
    // filter is written with it rather than typed after it, whose caret the
    // widget is still moving.
    await expect(input).toHaveValue(prefix);
    await input.fill(`${prefix}${filter}`);
  }
  const labels = async () =>
    (await rows.locator(".label-name").allTextContents()).map((text) => text.trim());
  // The list is filtered and re-ranked in several passes; taking a row from a
  // list that is still moving lands on whatever took that row over.
  let previous: string[] = [];
  await expect
    .poll(
      async () => {
        const current = await labels();
        const settled = current.includes(label) && current.join("\n") === previous.join("\n");
        previous = current;
        return settled;
      },
      { timeout: 10_000, intervals: [200] },
    )
    .toBe(true);
  await rows
    .filter({ has: page.locator(".label-name").filter({ hasText: exactly(label) }) })
    .first()
    .click({ timeout: 5_000 });
}

/** Matches an element whose whole text is `text`, icons and padding aside. */
function exactly(text: string): RegExp {
  return new RegExp(`^\\s*${text.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}\\s*$`);
}

/**
 * Run a command through the command palette, matching its full title. The
 * palette closes when the workbench takes focus back — a session stopping, a
 * view opening — which cancels the pick; the command has not run then, so
 * opening the palette again is safe.
 */
export async function runCommand(page: Page, title: string): Promise<void> {
  await expect(async () => {
    await page.keyboard.press("F1");
    await pickQuickInput(page, title, title, ">");
  }).toPass({ timeout: 45_000, intervals: [500] });
}

/**
 * Open a file of the workspace folder from the explorer. Double click so the
 * editor is pinned: a single click opens it in preview mode, which the next
 * file opened replaces instead of adding a second tab.
 */
export async function openFile(page: Page, name: string): Promise<void> {
  await page
    .locator(".explorer-folders-view")
    .getByRole("treeitem", { name, exact: true })
    .dblclick();
  await expectActiveTab(page, name);
  await expect(activeEditorLines(page)).toBeVisible();
}

/** Give focus back to an already open tab of the active group. */
export async function focusTab(page: Page, name: string): Promise<void> {
  await activeGroup(page).locator(`.tab[data-resource-name="${name}"]`).click();
  await expectActiveTab(page, name);
}

/** `name` is the tab's resource name: its accessible name also carries the
 * dirty marker and, for an untitled buffer, its first line. */
export async function expectActiveTab(page: Page, name: string): Promise<void> {
  await expect(activeGroup(page).locator(".tab.active")).toHaveAttribute(
    "data-resource-name",
    name,
  );
}

/**
 * Type into the active editor. Monaco reads keystrokes through an edit context,
 * which only sees them once the editor itself has focus.
 */
export async function typeInEditor(page: Page, text: string): Promise<void> {
  await activeEditorLines(page).click();
  await page.keyboard.type(text);
}

/** Type `text` at the start of the line holding `anchor`. */
export async function typeAtLineStart(page: Page, anchor: string, text: string): Promise<void> {
  await activeGroup(page).locator(".view-line", { hasText: anchor }).first().click();
  await page.keyboard.press("Home");
  await page.keyboard.type(text);
}

/**
 * The extension has activated once the language server answers with the
 * document's symbols. Until then VS Code fills the picker with a placeholder
 * row ("… first open a text editor with symbol information."), so wait for a
 * symbol the open file actually declares rather than for any row at all.
 */
export async function waitForLanguageServer(page: Page, symbol: string): Promise<void> {
  await expect
    .poll(
      async () => {
        await runCommand(page, "Go to Symbol in Editor...");
        // The picker's "@" prefix is in place before its rows are.
        await expect(page.locator(".quick-input-widget input")).toHaveValue("@");
        const labels = await page
          .locator(".quick-input-list .monaco-list-row .label-name")
          .allTextContents();
        await page.keyboard.press("Escape");
        // Symbol rows carry their kind icon inside the label.
        return labels.some((label) => label.trim() === symbol);
      },
      { timeout: 60_000, intervals: [1000] },
    )
    .toBe(true);
}

/**
 * A session is stopped on `stop`: the toolbar is up, the active editor
 * highlights the line and the call stack names the frame.
 */
export async function expectStopped(page: Page, stop: Stop): Promise<void> {
  await expect(page.locator(".debug-toolbar")).toBeVisible({ timeout: 30_000 });
  await expect(activeGroup(page).locator(".debug-top-stack-frame-line")).toBeVisible({
    timeout: 30_000,
  });
  await runCommand(page, "View: Show Run and Debug");
  await expect(
    page.getByRole("row", {
      name: `Stack Frame ${stop.label}, line ${stop.line}, ${stop.file}`,
      exact: true,
    }),
  ).toBeVisible({ timeout: 30_000 });
}

/**
 * No session started. Call it once the refusal is on screen (a notification or
 * a dialog), then give a session that did start time to raise its toolbar.
 */
export async function expectNoSession(page: Page): Promise<void> {
  const toolbar = page.locator(".debug-toolbar");
  await expect(toolbar).toBeHidden();
  await page.waitForTimeout(2_000);
  await expect(toolbar).toBeHidden();
}

export async function stopSession(page: Page): Promise<void> {
  await runCommand(page, "Debug: Stop");
  await expect(page.locator(".debug-toolbar")).toBeHidden({ timeout: 30_000 });
}

/**
 * Pick an entry of the "Select and Start Debugging" list; dynamic entries sit
 * under the debugger's submenu, whose row is labelled with a trailing ellipsis.
 */
export async function startConfiguration(
  page: Page,
  name: string,
  submenu?: string,
): Promise<void> {
  await runCommand(page, "Debug: Select and Start Debugging");
  if (submenu !== undefined) {
    await pickQuickInput(page, `${submenu}...`);
  }
  await pickQuickInput(page, name);
}
