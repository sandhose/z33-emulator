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

const ARIA_LABELS = {
  light: "Light theme",
  system: "System theme",
  dark: "Dark theme",
} as const;

/** Pure theme toggle group; no store coupling. */
export const ThemeToggle: React.FC<{
  value: ThemeValue;
  onValueChange: (theme: ThemeValue) => void;
}> = ({ value, onValueChange }) => (
  <ToggleGroup
    value={[value]}
    onValueChange={(values) => {
      const next = values[0];
      if (next && isThemeValue(next)) onValueChange(next);
    }}
    size="xs"
    variant="outline"
  >
    {THEMES.map((t) => {
      const Icon = ICONS[t];
      return (
        <Tooltip key={t}>
          <TooltipTrigger
            render={<ToggleGroupItem value={t} aria-label={ARIA_LABELS[t]} />}
          >
            <Icon />
          </TooltipTrigger>
          <TooltipContent>{LABELS[t]}</TooltipContent>
        </Tooltip>
      );
    })}
  </ToggleGroup>
);

export const ThemeSwitcher: React.FC = () => {
  const theme = useThemeStore((s) => s.theme);
  const setTheme = useThemeStore((s) => s.setTheme);
  return <ThemeToggle value={theme} onValueChange={setTheme} />;
};
