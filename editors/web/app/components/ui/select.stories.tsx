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
      <SelectTrigger className="w-48" aria-label="Fruit">
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
      <SelectTrigger className="w-48" aria-label="Fruit">
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
  // The play function ends on the click that closes the select, and
  // SelectContent's exit transition (`data-closed:animate-out … duration-100`)
  // keeps the popup and base-ui's focus guards in the DOM while axe runs. On
  // @base-ui/react 1.7.0 `SelectList` carries `role="listbox"` without an
  // accessible name (it drops the trigger's) and `FocusGuard` renders
  // `tabIndex=0` `aria-hidden` spans.
  parameters: {
    a11y: {
      config: {
        rules: [
          { id: "aria-input-field-name", enabled: false },
          { id: "aria-hidden-focus", enabled: false },
        ],
      },
    },
  },
  render: () => {
    const ControlledSelect = () => {
      const [value, setValue] = useState<string | null>(null);
      return (
        <Select value={value} onValueChange={setValue}>
          <SelectTrigger className="w-48" aria-label="Fruit">
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
