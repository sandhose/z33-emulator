// The single source of truth for the app's keyboard shortcuts. The
// registration hook (`useAppShortcuts`) and the help dialog both read from
// here; that hook covers document-level registration and the F8/F9 mirroring
// into Monaco.
//
// Focus shortcuts use `Alt+1..4`: macOS browsers bind `Cmd+<digit>` to tab
// switching, so `Mod+<digit>` would never reach the page. `Alt+<digit>` is free
// on every platform, and the library matches it via `event.code`, which the
// glyph Option produces doesn't affect.
import type { RegisterableHotkey } from "./hooks/use-app-hotkey";

/** Stable id linking a spec to its handler in `ShortcutActions`. */
export type ShortcutActionId =
  | "run"
  | "stop"
  | "step"
  | "runPause"
  | "focusEditor"
  | "focusSecondary"
  | "focusMemory"
  | "focusSerial"
  | "help";

/** Heading buckets in the help dialog, in display order. */
export type ShortcutGroupTitle = "Session" | "Execution" | "Panels" | "Help";

export const SHORTCUT_GROUP_ORDER: ShortcutGroupTitle[] = [
  "Session",
  "Execution",
  "Panels",
  "Help",
];

/** Which app mode a shortcut is active in (`"any"` = both). */
export type ShortcutScope = "edit" | "debug" | "any";

export interface ShortcutSpec {
  action: ShortcutActionId;
  hotkey: RegisterableHotkey;
  /** Human-readable name; also the merge key when one action has two keys. */
  name: string;
  description: string;
  group: ShortcutGroupTitle;
  scope: ShortcutScope;
  /**
   * Overrides the formatted key chip in the help dialog, for combos whose
   * canonical form doesn't match how the key is typed or named.
   */
  display?: string;
}

export const SHORTCUTS: ShortcutSpec[] = [
  {
    action: "run",
    hotkey: "Mod+Enter",
    name: "Run",
    description: "Start debugging at the default entrypoint",
    group: "Session",
    scope: "edit",
  },
  {
    action: "stop",
    hotkey: "Mod+Shift+Enter",
    name: "Stop",
    description: "Stop debugging and return to the editor",
    group: "Session",
    scope: "debug",
  },
  {
    action: "step",
    hotkey: "F8",
    name: "Step",
    description: "Execute a single instruction",
    group: "Execution",
    scope: "debug",
  },
  {
    action: "runPause",
    hotkey: "F9",
    name: "Run / Pause",
    description: "Toggle continuous execution",
    group: "Execution",
    scope: "debug",
  },
  {
    action: "focusEditor",
    hotkey: "Alt+1",
    name: "Focus editor",
    description: "Move focus to the program editor",
    group: "Panels",
    scope: "any",
  },
  {
    action: "focusSecondary",
    hotkey: "Alt+2",
    name: "Focus registers / files",
    description:
      "Focus the registers panel (debugging) or the file list (editing)",
    group: "Panels",
    scope: "any",
  },
  {
    action: "focusMemory",
    hotkey: "Alt+3",
    name: "Focus memory",
    description: "Move focus to the memory panel",
    group: "Panels",
    scope: "debug",
  },
  {
    action: "focusSerial",
    hotkey: "Alt+4",
    name: "Focus serial console",
    description: "Move focus to the serial console",
    group: "Panels",
    scope: "debug",
  },
  {
    action: "help",
    hotkey: "Mod+/",
    name: "Keyboard shortcuts",
    description: "Open this help dialog",
    group: "Help",
    scope: "any",
  },
  {
    // Typing `?` means Shift+/, so the event carries key "?" with shiftKey true
    // and `matchesKeyboardEvent` needs the explicit `shift: true` to agree.
    // With no Ctrl or Meta on it, the library's `ignoreInputs` default keeps it
    // from firing in text inputs and Monaco's textarea, and `display` replaces
    // the formatter's "⇧ ?", since the glyph implies the Shift.
    action: "help",
    hotkey: { key: "?", shift: true },
    name: "Keyboard shortcuts",
    description: "Open this help dialog",
    group: "Help",
    scope: "any",
    display: "?",
  },
];
