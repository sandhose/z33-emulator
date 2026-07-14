import preview from "#.storybook/preview";

import { SectionHeader } from "./section-header";

const meta = preview.meta({
  title: "UI/SectionHeader",
  component: SectionHeader,
  args: {
    children: "Registers",
  },
});

export const Default = meta.story({});

export const InPanel = meta.story({
  render: () => (
    <div className="w-64 overflow-hidden rounded-lg border">
      <SectionHeader>Memory</SectionHeader>
      <div className="p-3 text-sm text-muted-foreground">
        Panel body content
      </div>
    </div>
  ),
});
