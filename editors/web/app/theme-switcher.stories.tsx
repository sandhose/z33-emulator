import preview from "#.storybook/preview";
import { fn } from "storybook/test";

import { ThemeToggle } from "./theme-switcher";

const meta = preview.meta({
  title: "UI/ThemeToggle",
  component: ThemeToggle,
  args: {
    value: "system" as const,
    onValueChange: fn(),
  },
});

export const System = meta.story({});

export const Light = meta.story({
  args: { value: "light" },
});

export const Dark = meta.story({
  args: { value: "dark" },
});
