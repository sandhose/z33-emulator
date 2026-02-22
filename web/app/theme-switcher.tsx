import { MonitorIcon, MoonIcon, SunIcon } from "lucide-react";
import { useTheme } from "./components/theme-provider";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "./components/ui/tooltip";
import { cn } from "./lib/utils";

const THEMES = ["light", "system", "dark"] as const;

const ICONS = {
  light: SunIcon,
  system: MonitorIcon,
  dark: MoonIcon,
} as const;

const LABELS = { light: "Light", system: "System", dark: "Dark" } as const;

export const ThemeSwitcher: React.FC = () => {
  const { theme, setTheme } = useTheme();

  return (
    <div className="flex items-center rounded-md border border-border overflow-hidden">
      {THEMES.map((t) => {
        const Icon = ICONS[t];
        return (
          <Tooltip key={t}>
            <TooltipTrigger
              render={
                <button
                  type="button"
                  className={cn(
                    "size-6 flex items-center justify-center [&_svg]:size-3 transition-colors",
                    theme === t
                      ? "bg-muted text-foreground"
                      : "text-muted-foreground hover:bg-muted/50 hover:text-foreground",
                    t !== "dark" && "border-r border-border",
                  )}
                  onClick={() => setTheme(t)}
                />
              }
            >
              <Icon />
            </TooltipTrigger>
            <TooltipContent>{LABELS[t]}</TooltipContent>
          </Tooltip>
        );
      })}
    </div>
  );
};
