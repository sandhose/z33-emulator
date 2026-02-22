import {
  createContext,
  useContext,
  useLayoutEffect,
  useState,
  useSyncExternalStore,
} from "react";

type EffectiveTheme = "dark" | "light";
type Theme = EffectiveTheme | "system";

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
  const [theme, setTheme] = useState<Theme>(
    () => (localStorage.getItem(storageKey) as Theme) || defaultTheme,
  );

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

  const value = {
    theme,
    effective,
    setTheme: (theme: Theme) => {
      localStorage.setItem(storageKey, theme);
      setTheme(theme);
    },
  };

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
