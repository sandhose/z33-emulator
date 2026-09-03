import { create } from "zustand";
import { persist } from "zustand/middleware";

type EffectiveTheme = "dark" | "light";
type Theme = EffectiveTheme | "system";

const THEMES: readonly Theme[] = ["dark", "light", "system"];

const isTheme = (value: unknown): value is Theme =>
  typeof value === "string" && (THEMES as readonly string[]).includes(value);

const darkMediaQuery = window.matchMedia("(prefers-color-scheme: dark)");

function resolveEffective(theme: Theme): EffectiveTheme {
  if (theme === "system") return darkMediaQuery.matches ? "dark" : "light";
  return theme;
}

interface ThemeState {
  theme: Theme;
  effective: EffectiveTheme;
  setTheme: (theme: Theme) => void;
}

export const useThemeStore = create<ThemeState>()(
  persist(
    (set) => ({
      theme: "system",
      effective: resolveEffective("system"),
      setTheme: (theme) => {
        set({ theme, effective: resolveEffective(theme) });
      },
    }),
    {
      name: "z33:theme",
      partialize: (state) => ({ theme: state.theme }),
      merge: (persisted, current) => {
        const stored =
          persisted && typeof persisted === "object" && "theme" in persisted
            ? persisted.theme
            : undefined;
        const theme = isTheme(stored) ? stored : "system";
        return { ...current, theme, effective: resolveEffective(theme) };
      },
    },
  ),
);

// Sync system theme changes
darkMediaQuery.addEventListener("change", () => {
  const { theme } = useThemeStore.getState();
  if (theme === "system") {
    useThemeStore.setState({ effective: resolveEffective("system") });
  }
});

// Apply theme class to <html> on every change
useThemeStore.subscribe((state) => {
  const root = document.documentElement;
  root.classList.remove("light", "dark");
  root.classList.add(state.effective);
});

// Apply initial theme
const { effective } = useThemeStore.getState();
document.documentElement.classList.add(effective);
