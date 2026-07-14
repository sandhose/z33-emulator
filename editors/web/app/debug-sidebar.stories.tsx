import preview from "#.storybook/preview";
import { expect, fn, userEvent, within } from "storybook/test";

import { RegisterPanel } from "./debug-sidebar";
import { factorialScene, haltedScene, sceneLabels } from "./testing/fixtures";
import { FakeComputer } from "./testing/fake-computer";

const running = factorialScene();
const halted = haltedScene();
const followingSp = factorialScene();

const meta = preview.meta({
  title: "Organisms/RegisterPanel",
  component: RegisterPanel,
  args: {
    following: null,
    onFollow: fn(),
  },
  decorators: [
    (Story) => (
      <div className="w-72 rounded-md border">
        <Story />
      </div>
    ),
  ],
});

export const Running = meta.story({
  args: {
    computer: new FakeComputer(running),
    labels: sceneLabels(running),
  },
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    // %a holds 120 in the factorial scene — assert the value actually renders.
    await expect(canvas.getByText("120")).toBeInTheDocument();
    await userEvent.click(canvas.getByRole("button", { name: "Register %a" }));
    await expect(args.onFollow).toHaveBeenCalledWith("%a");
  },
});

export const Halted = meta.story({
  args: {
    computer: new FakeComputer(halted),
    labels: sceneLabels(halted),
  },
});

export const FollowingSp = meta.story({
  args: {
    computer: new FakeComputer(followingSp),
    labels: sceneLabels(followingSp),
    following: "%sp",
  },
});
