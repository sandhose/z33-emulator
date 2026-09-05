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

/**
 * Apply the theme to <html>. The bootstrap script in index.html mirrors this
 * module — the "z33:theme" storage key, the `state.theme` path inside it and the
 * "light"/"dark" class names — and applies the same theme inline before this
 * bundle loads; renaming any of them here means renaming them there too.
 *
 * Those inline styles outrank the stylesheet, so a switch has to move
 * color-scheme with it (native scrollbars, select popups, form controls) and
 * hand the background back to the stylesheet.
 */
function applyTheme(effective: EffectiveTheme): void {
  const root = document.documentElement;
  root.classList.remove("light", "dark");
  root.classList.add(effective);
  root.style.colorScheme = effective;
  root.style.backgroundColor = "";
}

useThemeStore.subscribe((state) => {
  applyTheme(state.effective);
});

applyTheme(useThemeStore.getState().effective);
