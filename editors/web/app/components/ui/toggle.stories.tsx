import preview from "#.storybook/preview";
import { Toggle as TogglePrimitive } from "@base-ui/react/toggle";
import { type VariantProps } from "class-variance-authority";
import { BoldIcon } from "lucide-react";

import { cn } from "@/lib/utils";

import { toggleVariants } from "./toggle";

// `toggle.tsx` only ships the `toggleVariants` cva helper (the app styles
// base-ui's Toggle/ToggleGroupItem with it). This thin wrapper mirrors that
// usage so the variants can be showcased on their own.
function Toggle({
  className,
  variant,
  size,
  ...props
}: TogglePrimitive.Props & VariantProps<typeof toggleVariants>) {
  return (
    <TogglePrimitive
      className={cn(toggleVariants({ variant, size }), className)}
      {...props}
    />
  );
}

const meta = preview.meta({
  title: "UI/Toggle",
  component: Toggle,
  argTypes: {
    variant: { control: "select", options: ["default", "outline"] },
    size: { control: "select", options: ["default", "xs", "sm", "lg"] },
    disabled: { control: "boolean" },
  },
  args: {
    variant: "default",
    size: "default",
    disabled: false,
    "aria-label": "Bold",
    children: <BoldIcon />,
  },
});

export const Default = meta.story({});

export const Outline = meta.story({ args: { variant: "outline" } });

export const Pressed = meta.story({ args: { defaultPressed: true } });

export const Disabled = meta.story({ args: { disabled: true } });

const SIZES = ["xs", "sm", "default", "lg"] as const;

export const SizeGrid = meta.story({
  render: () => (
    <div className="flex items-center gap-2">
      {SIZES.map((size) => (
        <Toggle key={size} size={size} variant="outline" aria-label={size}>
          <BoldIcon />
        </Toggle>
      ))}
    </div>
  ),
});
