import preview from "#.storybook/preview";
import { expect, fn, userEvent, within } from "storybook/test";

import { FormatToggle } from "./format-switcher";

const meta = preview.meta({
  title: "UI/FormatToggle",
  component: FormatToggle,
  args: {
    value: "decimal" as const,
    onValueChange: fn(),
  },
});

export const Decimal = meta.story({});

export const Hexadecimal = meta.story({
  args: { value: "hex" },
});

export const Binary = meta.story({
  args: { value: "binary" },
});

export const SwitchesFormat = meta.story({
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole("button", { name: "Hexadecimal" }));
    await expect(args.onValueChange).toHaveBeenCalledWith("hex");
  },
});
