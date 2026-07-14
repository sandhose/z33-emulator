// Thin wrapper around @tanstack/react-hotkeys (alpha). ALL app code imports
// keyboard-shortcut functionality from here and never from the library
// directly, so if the alpha API shifts under us the blast radius is this one
// file. Keep the surface small: only re-export/wrap what the app uses.
import {
  formatForDisplay,
  type FormatDisplayOptions,
  type HotkeyRegistrationView,
  type RegisterableHotkey,
  type UseHotkeyDefinition,
  type UseHotkeyOptions,
  useHotkeyRegistrations,
  useHotkeys,
} from "@tanstack/react-hotkeys";

/** A single hotkey registration: `{ hotkey, callback, options }`. */
export type AppHotkeyDefinition = UseHotkeyDefinition;
export type { HotkeyRegistrationView, RegisterableHotkey };

/**
 * Register a dynamic list of hotkeys. Callbacks/options are synced on every
 * render by the underlying manager, so closures never go stale and toggling
 * `enabled` updates the existing registration in place.
 */
export function useAppHotkeys(
  hotkeys: AppHotkeyDefinition[],
  commonOptions?: UseHotkeyOptions,
): void {
  useHotkeys(hotkeys, commonOptions);
}

/**
 * Live view of every registered hotkey (including soft-disabled ones), used to
 * drive the shortcuts help dialog. Returns only the `hotkeys` array; sequences
 * are unused in this app.
 */
export function useAppHotkeyRegistrations(): HotkeyRegistrationView[] {
  return useHotkeyRegistrations().hotkeys;
}

/** Format a hotkey for display (⌘-symbols on macOS, `Ctrl+…` elsewhere). */
export function formatHotkey(
  hotkey: RegisterableHotkey,
  options?: FormatDisplayOptions,
): string {
  return formatForDisplay(hotkey, options);
}
