import { BinaryIcon, HashIcon } from "lucide-react";
import { ToggleGroup, ToggleGroupItem } from "./components/ui/toggle-group";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "./components/ui/tooltip";
import { type DisplayFormat, useDisplayStore } from "./stores/display-store";

const FORMATS: readonly DisplayFormat[] = ["decimal", "hex", "binary"];

function isDisplayFormat(value: string): value is DisplayFormat {
  return (FORMATS as readonly string[]).includes(value);
}

const ICONS: Record<DisplayFormat, React.FC> = {
  decimal: HashIcon,
  hex: () => (
    <span className="text-[10px] font-bold leading-none select-none">0x</span>
  ),
  binary: BinaryIcon,
};

const LABELS: Record<DisplayFormat, string> = {
  decimal: "Decimal",
  hex: "Hexadecimal",
  binary: "Binary",
};

export const FormatSwitcher: React.FC = () => {
  const format = useDisplayStore((s) => s.format);
  const setFormat = useDisplayStore((s) => s.setFormat);

  return (
    <ToggleGroup
      value={[format]}
      onValueChange={(values) => {
        const value = values[0];
        if (value && isDisplayFormat(value)) setFormat(value);
      }}
      size="xs"
      variant="outline"
    >
      {FORMATS.map((f) => {
        const Icon = ICONS[f];
        return (
          <Tooltip key={f}>
            <TooltipTrigger
              render={<ToggleGroupItem value={f} aria-label={LABELS[f]} />}
            >
              <Icon />
            </TooltipTrigger>
            <TooltipContent>{LABELS[f]}</TooltipContent>
          </Tooltip>
        );
      })}
    </ToggleGroup>
  );
};
