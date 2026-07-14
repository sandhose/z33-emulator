import preview from "#.storybook/preview";
import { expect, userEvent, fn, within } from "storybook/test";

import { DebugToolbarInner } from "./debug-toolbar";
import { factorialScene, haltedScene, panickedScene } from "./testing/fixtures";
import { FakeComputer } from "./testing/fake-computer";

// A dedicated instance per story keeps the recorded `calls` isolated between
// vitest test cases (a shared instance would leak state across stories).
const idleComputer = new FakeComputer(factorialScene());

const meta = preview.meta({
  title: "Organisms/DebugToolbar",
  component: DebugToolbarInner,
  parameters: { layout: "padded" },
  args: {
    touchedFiles: ["main.s"],
    activeFile: "main.s",
    onFileChange: fn(),
    onStop: fn(),
  },
  decorators: [
    (Story) => (
      <div className="w-[48rem] rounded-md border">
        <Story />
      </div>
    ),
  ],
});

export const Idle = meta.story({
  args: { computer: idleComputer },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.click(canvas.getByRole("button", { name: /Step/u }));
    await expect(idleComputer.calls.some((c) => c.method === "step")).toBe(
      true,
    );

    await userEvent.click(canvas.getByRole("button", { name: /Run/u }));
    await expect(idleComputer.getStatus()).toBe("running");
    // Once running, the Run control flips to Pause.
    await expect(
      await canvas.findByRole("button", { name: /Pause/u }),
    ).toBeInTheDocument();
  },
});

export const Running = meta.story({
  args: {
    computer: new FakeComputer({ ...factorialScene(), status: "running" }),
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(
      canvas.getByRole("button", { name: /Pause/u }),
    ).toBeInTheDocument();
  },
});

export const Halted = meta.story({
  args: { computer: new FakeComputer(haltedScene()) },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByRole("alert")).toHaveTextContent("Halted");
  },
});

export const Panicked = meta.story({
  args: {
    computer: new FakeComputer(panickedScene("division by zero")),
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByRole("alert")).toHaveTextContent(
      "Panicked: division by zero",
    );
  },
});

export const MultiFile = meta.story({
  args: {
    computer: new FakeComputer(factorialScene()),
    touchedFiles: ["main.s", "lib.s"],
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    // The file switcher only appears with more than one touched file: two
    // comboboxes then exist (clock speed + file), versus one otherwise.
    await expect(canvas.getAllByRole("combobox")).toHaveLength(2);
  },
});
