// Thin wrapper around @tanstack/react-hotkeys (alpha). App code imports
// keyboard shortcuts from here and never from the library, so an alpha API
// change lands in this one file. Keep the surface small: wrap only what the
// app uses.
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
 * Register a dynamic list of hotkeys. The manager re-syncs callbacks and
 * options on every render, so closures stay current and toggling `enabled`
 * updates the existing registration in place.
 */
export function useAppHotkeys(
  hotkeys: AppHotkeyDefinition[],
  commonOptions?: UseHotkeyOptions,
): void {
  useHotkeys(hotkeys, commonOptions);
}

/**
 * Live view of every registered hotkey, soft-disabled ones included, for the
 * shortcuts help dialog. Returns the `hotkeys` array; this app registers no
 * sequences.
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
