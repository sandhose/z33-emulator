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
  // The play function opens the base-ui select. Two axe rules fire on base-ui's
  // own popup internals, both upstream behaviour we don't control:
  //  - `aria-input-field-name`: base-ui doesn't propagate the trigger's
  //    accessible name to its `role="listbox"` element.
  //  - `aria-hidden-focus`: base-ui's aria-hidden focus-guard spans are
  //    focusable while the popup is open.
  // Disable just those two rules for this open-popup story.
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
