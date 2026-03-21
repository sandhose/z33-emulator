import { MonitorIcon, MoonIcon, SunIcon } from "lucide-react";
import { useThemeStore } from "./stores/theme-store";
import { ToggleGroup, ToggleGroupItem } from "./components/ui/toggle-group";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "./components/ui/tooltip";

const THEMES = ["light", "system", "dark"] as const;
type ThemeValue = (typeof THEMES)[number];

function isThemeValue(value: string): value is ThemeValue {
  return (THEMES as readonly string[]).includes(value);
}

const ICONS = {
  light: SunIcon,
  system: MonitorIcon,
  dark: MoonIcon,
} as const;

const LABELS = { light: "Light", system: "System", dark: "Dark" } as const;

export const ThemeSwitcher: React.FC = () => {
  const theme = useThemeStore((s) => s.theme);
  const setTheme = useThemeStore((s) => s.setTheme);

  return (
    <ToggleGroup
      value={[theme]}
      onValueChange={(values) => {
        const value = values[0];
        if (value && isThemeValue(value)) setTheme(value);
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
