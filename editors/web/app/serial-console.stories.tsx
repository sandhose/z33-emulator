import preview from "#.storybook/preview";
import { expect, fn, userEvent, within } from "storybook/test";

import { SerialConsoleShell } from "./serial-console";

const meta = preview.meta({
  title: "Organisms/SerialConsoleShell",
  component: SerialConsoleShell,
  parameters: { layout: "padded" },
  args: {
    onClear: fn(),
  },
  decorators: [
    (Story) => (
      <div className="flex h-64 w-96 flex-col overflow-hidden rounded-md border">
        <Story />
      </div>
    ),
  ],
});

export const Default = meta.story({
  args: {
    children: (
      <div className="h-full w-full rounded-sm bg-muted/40 p-2 font-mono text-xs">
        Hello, world!
      </div>
    ),
  },
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    await userEvent.click(
      canvas.getByRole("button", { name: "Clear serial console" }),
    );
    await expect(args.onClear).toHaveBeenCalled();
  },
});

export const EmptyBody = meta.story({
  args: {
    children: (
      <div className="h-full w-full animate-pulse rounded-sm bg-muted/40" />
    ),
  },
});
