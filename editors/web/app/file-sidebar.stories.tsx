import preview from "#.storybook/preview";
import type * as React from "react";
import { useState } from "react";
import { expect, fn, userEvent, within } from "storybook/test";

import { FileSidebarView } from "./file-sidebar";

const noop = (): void => {};

// Stand-ins for the `useFileDrop`-derived props. The pure view only spreads
// them, so inert handlers and a detached ref are enough for stories.
const dropZoneRef: React.RefObject<HTMLDivElement | null> = { current: null };
const dropZoneProps = {
  onDragOver: noop,
  onDragEnter: noop,
  onDragLeave: noop,
  onDrop: noop,
};

const meta = preview.meta({
  title: "Organisms/FileSidebar",
  component: FileSidebarView,
  parameters: { layout: "padded" },
  args: {
    files: [],
    activeFile: "",
    isCreatingFile: false,
    isWindowDragging: false,
    isOverDropZone: false,
    dropZoneRef,
    dropZoneProps,
    onSelect: fn(),
    onStartCreate: fn(),
    onCreate: fn(),
    onCancelCreate: fn(),
    onDelete: fn(),
    onDownload: fn(),
    onUpload: fn(),
    onReset: fn(),
  },
  decorators: [
    (Story) => (
      <div className="flex h-96 overflow-hidden rounded-md border">
        <Story />
      </div>
    ),
  ],
});

export const Empty = meta.story({});

export const WithFiles = meta.story({
  args: {
    files: ["main.s", "lib.s", "util.s"],
    activeFile: "main.s",
  },
  render: (args) => {
    // Drive the create flow locally: the pure view keeps `isCreatingFile` in
    // its parent, so a small stateful wrapper reproduces the app's behaviour.
    const Wrapper: React.FC = () => {
      const [creating, setCreating] = useState(false);
      return (
        <FileSidebarView
          {...args}
          isCreatingFile={creating}
          onStartCreate={() => {
            args.onStartCreate();
            setCreating(true);
          }}
          onCancelCreate={() => {
            args.onCancelCreate();
            setCreating(false);
          }}
          onCreate={(name) => {
            args.onCreate(name);
            setCreating(false);
          }}
        />
      );
    };
    return <Wrapper />;
  },
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByText("lib.s")).toBeInTheDocument();
    await userEvent.click(canvas.getByRole("button", { name: "New file" }));
    const input = await canvas.findByRole("textbox", { name: "File name" });
    await userEvent.type(input, "hello.s{Enter}");
    await expect(args.onCreate).toHaveBeenCalledWith("hello.s");
  },
});

export const Creating = meta.story({
  args: {
    files: ["main.s", "lib.s"],
    activeFile: "main.s",
    isCreatingFile: true,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(
      canvas.getByRole("textbox", { name: "File name" }),
    ).toBeInTheDocument();
  },
});

export const DragOver = meta.story({
  args: {
    files: ["main.s"],
    activeFile: "main.s",
    isWindowDragging: true,
    isOverDropZone: true,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByText("Drop files here")).toBeInTheDocument();
  },
});
