import { defineMain } from "@storybook/react-vite/node";

export default defineMain({
  stories: ["../app/**/*.stories.@(ts|tsx)"],
  addons: [
    "@storybook/addon-a11y",
    "@storybook/addon-themes",
    "@storybook/addon-vitest",
  ],
  framework: "@storybook/react-vite",
});
