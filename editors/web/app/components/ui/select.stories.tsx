import preview from "#.storybook/preview";
import { useState } from "react";
import { expect, screen, userEvent, within } from "storybook/test";

import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "./select";

const FRUITS = ["Apple", "Banana", "Cherry", "Elderberry"];

const meta = preview.meta({
  title: "UI/Select",
  component: Select,
});

export const Default = meta.story({
  render: () => (
    <Select defaultValue="Apple">
      <SelectTrigger className="w-48">
        <SelectValue />
      </SelectTrigger>
      <SelectContent>
        {FRUITS.map((fruit) => (
          <SelectItem key={fruit} value={fruit}>
            {fruit}
          </SelectItem>
        ))}
      </SelectContent>
    </Select>
  ),
});

export const Placeholder = meta.story({
  render: () => (
    <Select>
      <SelectTrigger className="w-48">
        <SelectValue placeholder="Pick a fruit…" />
      </SelectTrigger>
      <SelectContent>
        {FRUITS.map((fruit) => (
          <SelectItem key={fruit} value={fruit}>
            {fruit}
          </SelectItem>
        ))}
      </SelectContent>
    </Select>
  ),
});

export const SelectsAnItem = meta.story({
  render: () => {
    const ControlledSelect = () => {
      const [value, setValue] = useState<string | null>(null);
      return (
        <Select value={value} onValueChange={setValue}>
          <SelectTrigger className="w-48">
            <SelectValue placeholder="Pick a fruit…" />
          </SelectTrigger>
          <SelectContent>
            {FRUITS.map((fruit) => (
              <SelectItem key={fruit} value={fruit}>
                {fruit}
              </SelectItem>
            ))}
          </SelectContent>
        </Select>
      );
    };
    return <ControlledSelect />;
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const trigger = canvas.getByRole("combobox");
    await userEvent.click(trigger);
    // base-ui renders the listbox in a portal on document.body.
    const option = await screen.findByRole("option", { name: "Banana" });
    await userEvent.click(option);
    await expect(trigger).toHaveTextContent("Banana");
  },
});
