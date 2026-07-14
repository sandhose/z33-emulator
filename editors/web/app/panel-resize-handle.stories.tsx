import preview from "#.storybook/preview";
import { Group, Panel } from "react-resizable-panels";

import { ResizeHandle } from "./panel-resize-handle";

const meta = preview.meta({
  title: "UI/ResizeHandle",
  component: ResizeHandle,
  parameters: { layout: "fullscreen" },
});

const PanelBody = ({ label }: { label: string }) => (
  <div className="flex h-full items-center justify-center bg-muted/30 text-sm text-muted-foreground">
    {label}
  </div>
);

export const Horizontal = meta.story({
  render: () => (
    <Group orientation="horizontal" id="sb-h" className="h-64 w-full border">
      <Panel defaultSize="50%" id="sb-h-left">
        <PanelBody label="Left" />
      </Panel>
      <ResizeHandle />
      <Panel defaultSize="50%" id="sb-h-right">
        <PanelBody label="Right" />
      </Panel>
    </Group>
  ),
});

export const Vertical = meta.story({
  render: () => (
    <Group orientation="vertical" id="sb-v" className="h-64 w-full border">
      <Panel defaultSize="50%" id="sb-v-top">
        <PanelBody label="Top" />
      </Panel>
      <ResizeHandle orientation="vertical" />
      <Panel defaultSize="50%" id="sb-v-bottom">
        <PanelBody label="Bottom" />
      </Panel>
    </Group>
  ),
});
