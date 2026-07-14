import preview from "#.storybook/preview";
import { expect, within } from "storybook/test";

import { MemoryViewer } from "./computer";
import type { Pointers } from "./computer-types";
import { emptyScene, factorialScene, sceneLabels } from "./testing/fixtures";
import { FakeComputer } from "./testing/fake-computer";

const meta = preview.meta({
  title: "Organisms/MemoryViewer",
  component: MemoryViewer,
  parameters: { layout: "padded" },
  // The virtualizer only renders rows when its scroll parent has a real height.
  // Without this fixed-height flex column it would render zero rows.
  decorators: [
    (Story) => (
      <div className="flex h-96 w-80 flex-col overflow-hidden rounded-md border">
        <Story />
      </div>
    ),
  ],
});

export const AtProgramStart = meta.story({
  render: () => {
    const scene = factorialScene();
    return (
      <MemoryViewer
        computer={new FakeComputer(scene)}
        highlight={1000}
        labels={sceneLabels(scene)}
      />
    );
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    // Rows must actually render — assert a known instruction cell is visible.
    await expect(await canvas.findByText("push 5")).toBeInTheDocument();
    await expect(await canvas.findByText("main")).toBeInTheDocument();
  },
});

export const WithLabelsAndPointers = meta.story({
  render: () => {
    const scene = factorialScene();
    const pointers: Pointers = new Map([
      [1007, ["%pc"]],
      [1012, ["%sp"]],
    ]);
    return (
      <MemoryViewer
        computer={new FakeComputer(scene)}
        highlight={1000}
        labels={sceneLabels(scene)}
        pointers={pointers}
      />
    );
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(await canvas.findByText("loop")).toBeInTheDocument();
    await expect(await canvas.findByText("%pc")).toBeInTheDocument();
  },
});

export const EmptyMemory = meta.story({
  render: () => (
    <MemoryViewer
      computer={new FakeComputer(emptyScene())}
      highlight={null}
      labels={new Map()}
    />
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    // Empty cells render as a muted zero word.
    const zeros = await canvas.findAllByText("00000000");
    await expect(zeros.length).toBeGreaterThan(0);
  },
});
