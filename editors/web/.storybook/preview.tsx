import addonA11y from "@storybook/addon-a11y";
import addonThemes, { withThemeByClassName } from "@storybook/addon-themes";
import addonVitest from "@storybook/addon-vitest";
import { definePreview } from "@storybook/react-vite";

import { TooltipProvider } from "../app/components/ui/tooltip";
import "../app/globals.css";

export default definePreview({
  addons: [addonA11y(), addonThemes(), addonVitest()],
  decorators: [
    withThemeByClassName({
      themes: { light: "", dark: "dark" },
      defaultTheme: "light",
    }),
    (Story) => (
      <TooltipProvider delay={0}>
        <Story />
      </TooltipProvider>
    ),
  ],
  parameters: {
    layout: "centered",
    a11y: {
      // "todo" surfaces a11y violations in the UI without failing the test run.
      // Flip to "error" once the primitives are audited.
      test: "todo",
    },
  },
  tags: ["autodocs"],
});
