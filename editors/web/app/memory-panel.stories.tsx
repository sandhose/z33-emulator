import preview from "#.storybook/preview";
import { expect, fn, userEvent, within } from "storybook/test";

import { MemoryPanel } from "./memory-panel";
import { factorialScene, sceneLabels } from "./testing/fixtures";
import { FakeComputer } from "./testing/fake-computer";

const withLabels = factorialScene();
const followingLabel = factorialScene();

const meta = preview.meta({
  title: "Organisms/MemoryPanel",
  component: MemoryPanel,
  parameters: { layout: "padded" },
  args: {
    following: null,
    onFollow: fn(),
  },
  decorators: [
    (Story) => (
      <div className="flex h-[32rem] w-80 flex-col overflow-hidden rounded-md border">
        <Story />
      </div>
    ),
  ],
});

export const WithLabels = meta.story({
  args: {
    computer: new FakeComputer(withLabels),
    labels: sceneLabels(withLabels),
  },
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    const labels = canvas.getByRole("region", { name: "Labels" });
    await expect(within(labels).getByText("loop")).toBeInTheDocument();
    await userEvent.click(within(labels).getByText("main"));
    await expect(args.onFollow).toHaveBeenCalledWith("label:main");
  },
});

export const FollowingLabel = meta.story({
  args: {
    computer: new FakeComputer(followingLabel),
    labels: sceneLabels(followingLabel),
    following: "label:loop",
  },
});
