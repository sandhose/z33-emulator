import preview from "#.storybook/preview";
import { useState } from "react";
import {
  AlignCenterIcon,
  AlignLeftIcon,
  AlignRightIcon,
  BoldIcon,
  ItalicIcon,
  UnderlineIcon,
} from "lucide-react";

import { ToggleGroup, ToggleGroupItem } from "./toggle-group";

const meta = preview.meta({
  title: "UI/ToggleGroup",
  component: ToggleGroup,
});

const SingleSelectionDemo = () => {
  const [value, setValue] = useState<string[]>(["left"]);
  return (
    <ToggleGroup value={value} onValueChange={setValue} variant="outline">
      <ToggleGroupItem value="left" aria-label="Align left">
        <AlignLeftIcon />
      </ToggleGroupItem>
      <ToggleGroupItem value="center" aria-label="Align center">
        <AlignCenterIcon />
      </ToggleGroupItem>
      <ToggleGroupItem value="right" aria-label="Align right">
        <AlignRightIcon />
      </ToggleGroupItem>
    </ToggleGroup>
  );
};

const MultipleSelectionDemo = () => {
  const [value, setValue] = useState<string[]>(["bold"]);
  return (
    <ToggleGroup
      multiple
      value={value}
      onValueChange={setValue}
      variant="outline"
    >
      <ToggleGroupItem value="bold" aria-label="Bold">
        <BoldIcon />
      </ToggleGroupItem>
      <ToggleGroupItem value="italic" aria-label="Italic">
        <ItalicIcon />
      </ToggleGroupItem>
      <ToggleGroupItem value="underline" aria-label="Underline">
        <UnderlineIcon />
      </ToggleGroupItem>
    </ToggleGroup>
  );
};

export const SingleSelection = meta.story({
  render: () => <SingleSelectionDemo />,
});

export const MultipleSelection = meta.story({
  render: () => <MultipleSelectionDemo />,
});

export const Spaced = meta.story({
  render: () => (
    <ToggleGroup defaultValue={["center"]} spacing={4} variant="outline">
      <ToggleGroupItem value="left" aria-label="Align left">
        <AlignLeftIcon />
      </ToggleGroupItem>
      <ToggleGroupItem value="center" aria-label="Align center">
        <AlignCenterIcon />
      </ToggleGroupItem>
      <ToggleGroupItem value="right" aria-label="Align right">
        <AlignRightIcon />
      </ToggleGroupItem>
    </ToggleGroup>
  ),
});
