import preview from "#.storybook/preview";
import { expect, screen, userEvent, waitFor, within } from "storybook/test";

import { Button } from "./button";
import { Tooltip, TooltipContent, TooltipTrigger } from "./tooltip";

const meta = preview.meta({
  title: "UI/Tooltip",
  component: Tooltip,
});

export const Default = meta.story({
  render: () => (
    <Tooltip>
      <TooltipTrigger render={<Button variant="outline">Hover me</Button>} />
      <TooltipContent>Add to library</TooltipContent>
    </Tooltip>
  ),
});

export const ShowsOnHover = meta.story({
  render: () => (
    <Tooltip>
      <TooltipTrigger render={<Button variant="outline">Hover me</Button>} />
      <TooltipContent>Add to library</TooltipContent>
    </Tooltip>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const trigger = canvas.getByRole("button", { name: "Hover me" });
    await userEvent.hover(trigger);
    // base-ui renders the tooltip popup in a portal on document.body, and it
    // stays hidden until the open animation has positioned it.
    const tooltip = await screen.findByText("Add to library");
    await waitFor(() => expect(tooltip).toBeVisible());
  },
});
