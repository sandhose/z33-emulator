import preview from "#.storybook/preview";
import { CheckIcon } from "lucide-react";

import { Badge } from "./badge";

const meta = preview.meta({
  title: "UI/Badge",
  component: Badge,
  argTypes: {
    variant: {
      control: "select",
      options: [
        "default",
        "secondary",
        "destructive",
        "outline",
        "ghost",
        "link",
      ],
    },
  },
  args: {
    children: "Badge",
    variant: "default",
  },
});

export const Default = meta.story({});

export const Secondary = meta.story({ args: { variant: "secondary" } });

export const Destructive = meta.story({ args: { variant: "destructive" } });

export const Outline = meta.story({ args: { variant: "outline" } });

export const WithIcon = meta.story({
  args: {
    children: (
      <>
        <CheckIcon data-icon="inline-start" />
        Passed
      </>
    ),
  },
});

const VARIANTS = [
  "default",
  "secondary",
  "destructive",
  "outline",
  "ghost",
  "link",
] as const;

export const VariantGrid = meta.story({
  render: () => (
    <div className="flex flex-wrap items-center gap-2">
      {VARIANTS.map((variant) => (
        <Badge key={variant} variant={variant}>
          {variant}
        </Badge>
      ))}
    </div>
  ),
});
