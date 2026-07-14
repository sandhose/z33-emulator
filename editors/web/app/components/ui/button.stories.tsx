import preview from "#.storybook/preview";
import { ArrowRightIcon, PlusIcon } from "lucide-react";

import { Button } from "./button";

const meta = preview.meta({
  title: "UI/Button",
  component: Button,
  argTypes: {
    variant: {
      control: "select",
      options: [
        "default",
        "outline",
        "secondary",
        "ghost",
        "destructive",
        "link",
      ],
    },
    size: {
      control: "select",
      options: ["default", "xs", "sm", "lg", "icon"],
    },
    disabled: { control: "boolean" },
  },
  args: {
    children: "Button",
    variant: "default",
    size: "default",
    disabled: false,
  },
});

export const Default = meta.story({});

export const Secondary = meta.story({
  args: { variant: "secondary" },
});

export const Destructive = meta.story({
  args: { variant: "destructive" },
});

export const Disabled = meta.story({
  args: { disabled: true },
});

export const WithIcon = meta.story({
  args: {
    children: (
      <>
        <PlusIcon data-icon="inline-start" />
        Add file
      </>
    ),
  },
});

const VARIANTS = [
  "default",
  "outline",
  "secondary",
  "ghost",
  "destructive",
  "link",
] as const;

const SIZES = ["xs", "sm", "default", "lg"] as const;

export const VariantGrid = meta.story({
  render: () => (
    <div className="flex flex-col gap-3">
      {VARIANTS.map((variant) => (
        <div key={variant} className="flex items-center gap-2">
          <Button variant={variant}>{variant}</Button>
          <Button variant={variant} disabled>
            disabled
          </Button>
          <Button variant={variant} size="icon" aria-label={`${variant} icon`}>
            <ArrowRightIcon />
          </Button>
        </div>
      ))}
    </div>
  ),
});

export const SizeGrid = meta.story({
  render: () => (
    <div className="flex items-center gap-2">
      {SIZES.map((size) => (
        <Button key={size} size={size}>
          {size}
        </Button>
      ))}
    </div>
  ),
});
