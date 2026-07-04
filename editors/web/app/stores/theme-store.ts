import { create } from "zustand";
import { persist } from "zustand/middleware";

type EffectiveTheme = "dark" | "light";
type Theme = EffectiveTheme | "system";

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
      onRehydrateStorage: () => (state) => {
        if (state) {
          state.effective = resolveEffective(state.theme);
        }
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
