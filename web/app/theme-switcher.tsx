import { MonitorIcon, MoonIcon, SunIcon } from "lucide-react";
import { useTheme } from "./components/theme-provider";
import { ToggleGroup, ToggleGroupItem } from "./components/ui/toggle-group";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "./components/ui/tooltip";

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
    <ToggleGroup
      value={[theme]}
      onValueChange={(values) => {
        if (values.length > 0) setTheme(values[0] as (typeof THEMES)[number]);
      }}
      size="xs"
      variant="outline"
    >
      {THEMES.map((t) => {
        const Icon = ICONS[t];
        return (
          <Tooltip key={t}>
            <TooltipTrigger render={<ToggleGroupItem value={t} />}>
              <Icon />
            </TooltipTrigger>
            <TooltipContent>{LABELS[t]}</TooltipContent>
          </Tooltip>
        );
      })}
    </ToggleGroup>
  );
};
