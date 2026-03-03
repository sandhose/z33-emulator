import { BinaryIcon, HashIcon } from "lucide-react";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "./components/ui/tooltip";
import { cn } from "./lib/utils";
import { type DisplayFormat, useDisplayStore } from "./stores/display-store";

const FORMATS = ["decimal", "hex", "binary"] as const;

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
    <div className="flex items-center rounded-md border border-border overflow-hidden">
      {FORMATS.map((f, i) => {
        const Icon = ICONS[f];
        return (
          <Tooltip key={f}>
            <TooltipTrigger
              render={
                <button
                  type="button"
                  className={cn(
                    "size-6 flex items-center justify-center [&_svg]:size-3 transition-colors",
                    format === f
                      ? "bg-muted text-foreground"
                      : "text-muted-foreground hover:bg-muted/50 hover:text-foreground",
                    i < FORMATS.length - 1 && "border-r border-border",
                  )}
                  onClick={() => setFormat(f)}
                />
              }
            >
              <Icon />
            </TooltipTrigger>
            <TooltipContent>{LABELS[f]}</TooltipContent>
          </Tooltip>
        );
      })}
    </div>
  );
};
