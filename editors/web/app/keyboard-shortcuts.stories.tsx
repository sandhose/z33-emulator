import preview from "#.storybook/preview";
import { useState } from "react";
import { expect, screen, userEvent, waitFor, within } from "storybook/test";

import { Button } from "./components/ui/button";
import {
  type ShortcutGroup,
  ShortcutsDialogView,
  ShortcutsSheet,
} from "./keyboard-shortcuts";

// Hand-written groups, so the presentational components render a fixed set of
// rows without the live hotkey registry.
const GROUPS: ShortcutGroup[] = [
  {
    title: "Session",
    rows: [
      {
        name: "Run",
        description: "Start debugging at the default entrypoint",
        keys: ["⌘ ⏎"],
      },
      {
        name: "Stop",
        description: "Stop debugging and return to the editor",
        keys: ["⌘ ⇧ ⏎"],
      },
    ],
  },
  {
    title: "Execution",
    rows: [
      {
        name: "Step",
        description: "Execute a single instruction",
        keys: ["F8"],
        unavailable: "debug only",
      },
      {
        name: "Run / Pause",
        description: "Toggle continuous execution",
        keys: ["F9"],
      },
    ],
  },
  {
    title: "Panels",
    rows: [
      {
        name: "Focus editor",
        description: "Move focus to the program editor",
        keys: ["⌥ 1"],
      },
      {
        name: "Focus registers / files",
        description: "Focus the registers panel or the file list",
        keys: ["⌥ 2"],
      },
    ],
  },
  {
    title: "Help",
    rows: [
      {
        name: "Keyboard shortcuts",
        description: "Open this help dialog",
        keys: ["⌘ /", "?"],
      },
    ],
  },
];

const meta = preview.meta({
  title: "Organisms/ShortcutsHelp",
  component: ShortcutsSheet,
  parameters: { layout: "padded" },
});

export const Sheet = meta.story({
  args: { groups: GROUPS },
  decorators: [
    (Story) => (
      <div className="w-[28rem] rounded-md border p-4">
        <Story />
      </div>
    ),
  ],
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(
      canvas.getByRole("heading", { name: "Session" }),
    ).toBeInTheDocument();
    await expect(canvas.getByText("Step")).toBeInTheDocument();
    // The Help action lists both of its keys.
    await expect(canvas.getByText("⌘ /")).toBeInTheDocument();
    await expect(canvas.getByText("?")).toBeInTheDocument();
    // A shortcut that doesn't apply in the current mode says so.
    await expect(canvas.getByText("debug only")).toBeInTheDocument();
  },
});

const DialogHarness = () => {
  const [open, setOpen] = useState(false);
  return (
    <>
      <Button
        onClick={() => {
          setOpen(true);
        }}
      >
        Show shortcuts
      </Button>
      <ShortcutsDialogView open={open} onOpenChange={setOpen} groups={GROUPS} />
    </>
  );
};

export const InDialog = meta.story({
  render: () => <DialogHarness />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(
      canvas.getByRole("button", { name: "Show shortcuts" }),
    );
    // base-ui renders the dialog in a portal on document.body, mounting it at
    // opacity 0 and fading it in, so poll for visibility.
    const dialog = await screen.findByRole("dialog");
    await waitFor(() => expect(dialog).toBeVisible());
    await expect(
      within(dialog).getByRole("heading", { name: "Keyboard shortcuts" }),
    ).toBeVisible();
    await userEvent.click(
      within(dialog).getByRole("button", { name: "Close" }),
    );
    await waitFor(() =>
      expect(screen.queryByRole("dialog")).not.toBeInTheDocument(),
    );
  },
});
