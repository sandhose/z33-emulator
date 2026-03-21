import {
  createContext,
  useCallback,
  useContext,
  useLayoutEffect,
  useMemo,
  useState,
  useSyncExternalStore,
} from "react";

type EffectiveTheme = "dark" | "light";
type Theme = EffectiveTheme | "system";

const VALID_THEMES: readonly Theme[] = ["dark", "light", "system"];
function isTheme(value: string): value is Theme {
  return (VALID_THEMES as readonly string[]).includes(value);
}

type ThemeProviderProps = {
  children: React.ReactNode;
  defaultTheme?: Theme;
  storageKey?: string;
};

type ThemeProviderState = {
  theme: Theme;
  effective: EffectiveTheme;
  setTheme: (theme: Theme) => void;
};

const darkMediaQuery = window.matchMedia("(prefers-color-scheme: dark)");

const initialState: ThemeProviderState = {
  theme: "system",
  effective: darkMediaQuery.matches ? "dark" : "light",
  setTheme: () => null,
};

const ThemeProviderContext = createContext<ThemeProviderState>(initialState);

export function ThemeProvider({
  children,
  defaultTheme = "system",
  storageKey = "vite-ui-theme",
  ...props
}: ThemeProviderProps) {
  const [theme, setTheme] = useState<Theme>(() => {
    const stored = localStorage.getItem(storageKey);
    return stored && isTheme(stored) ? stored : defaultTheme;
  });

  const isSystemDark = useSyncExternalStore(
    (callback) => {
      darkMediaQuery.addEventListener("change", callback);
      return () => darkMediaQuery.removeEventListener("change", callback);
    },
    () => darkMediaQuery.matches,
  );

  const effective =
    theme === "system" ? (isSystemDark ? "dark" : "light") : theme;

  useLayoutEffect(() => {
    const root = window.document.documentElement;
    root.classList.remove("light", "dark");
    root.classList.add(effective);
  }, [effective]);

  const handleSetTheme = useCallback(
    (newTheme: Theme) => {
      localStorage.setItem(storageKey, newTheme);
      setTheme(newTheme);
    },
    [storageKey],
  );

  const value = useMemo(
    () => ({ theme, effective, setTheme: handleSetTheme }),
    [theme, effective, handleSetTheme],
  );

  return (
    <ThemeProviderContext.Provider {...props} value={value}>
      {children}
    </ThemeProviderContext.Provider>
  );
}

export const useTheme = () => {
  const context = useContext(ThemeProviderContext);

  if (context === undefined)
    throw new Error("useTheme must be used within a ThemeProvider");

  return context;
};
