import { memo } from "react";
import {
  type ShortcutActions,
  useAppShortcuts,
} from "./hooks/use-app-shortcuts";

/**
 * Registers the app's global keyboard shortcuts. Isolated into a memoized leaf
 * that renders nothing: the hotkey library re-syncs options during render (it
 * calls the registry store's `setState`), so keeping registration out of any
 * component that re-renders while the help dialog is open prevents a
 * "setState during render" warning against the dialog's live subscription.
 * With stable `actions`, this only re-renders on a genuine mode/handler change.
 */
export const AppShortcuts = memo<{
  mode: "edit" | "debug";
  actions: ShortcutActions;
}>(({ mode, actions }) => {
  useAppShortcuts(mode, actions);
  return null;
});
AppShortcuts.displayName = "AppShortcuts";
