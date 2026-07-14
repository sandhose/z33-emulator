import preview from "#.storybook/preview";
import { expect, fn, screen, userEvent, within } from "storybook/test";

import { EditToolbar, pickEntrypoint } from "./edit-toolbar";

const meta = preview.meta({
  title: "Organisms/EditToolbar",
  component: EditToolbar,
  parameters: { layout: "padded" },
  args: {
    onRun: fn(),
    compilationError: undefined,
    defaultEntrypoint: undefined,
    labels: [],
  },
  decorators: [
    (Story) => (
      <div className="w-[42rem] rounded-md border">
        <Story />
      </div>
    ),
  ],
});

export const Idle = meta.story({
  args: { compilationStatus: "idle" },
});

export const Pending = meta.story({
  args: { compilationStatus: "pending" },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(
      canvas.getByRole("status", { name: "Compiling" }),
    ).toBeInTheDocument();
  },
});

export const CompileError = meta.story({
  args: {
    compilationStatus: "error",
    compilationError: "expected a register, found `foo`",
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(
      canvas.getByText("expected a register, found `foo`"),
    ).toBeInTheDocument();
  },
});

export const Success = meta.story({
  args: {
    compilationStatus: "success",
    labels: ["main", "loop", "end"],
  },
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    // Open the entrypoint select (portal-rendered on document.body).
    await userEvent.click(canvas.getByRole("combobox"));
    await userEvent.click(await screen.findByRole("option", { name: "loop" }));
    await userEvent.click(canvas.getByRole("button", { name: /Run/u }));
    await expect(args.onRun).toHaveBeenCalledWith("loop");
  },
});

export const DefaultEntrypoint = meta.story({
  args: {
    compilationStatus: "success",
    // No "main" — pickEntrypoint falls to the next well-known name, "start".
    labels: ["helper", "start"],
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    // The rendered default matches pickEntrypoint's well-known-name fallback.
    const noPreferred: string | undefined = undefined;
    await expect(pickEntrypoint(["helper", "start"], noPreferred)).toBe(
      "start",
    );
    await expect(canvas.getByRole("combobox")).toHaveTextContent("start");
  },
});
