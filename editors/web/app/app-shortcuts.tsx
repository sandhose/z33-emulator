import { memo } from "react";
import {
  type ShortcutActions,
  useAppShortcuts,
} from "./hooks/use-app-shortcuts";

/**
 * Registers the app's global keyboard shortcuts from a memoized leaf that
 * renders nothing and re-renders only when the mode changes (`actions` is
 * stable). The hotkey library calls the registry store's `setState` during
 * render, which warns "setState during render" against the help dialog's live
 * subscription when registration sits in a component that re-renders while the
 * dialog is open.
 */
export const AppShortcuts = memo<{
  mode: "edit" | "debug";
  actions: ShortcutActions;
}>(({ mode, actions }) => {
  useAppShortcuts(mode, actions);
  return null;
});
AppShortcuts.displayName = "AppShortcuts";
