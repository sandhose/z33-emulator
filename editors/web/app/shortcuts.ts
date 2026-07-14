// The single source of truth for the app's keyboard shortcuts. Both the
// registration hook (`useAppShortcuts`) and the help dialog read from here.
//
// Key choices (see the report / README of the a11y branch for the rationale):
//   - Mode/help/focus shortcuts fire at the document level via the library.
//   - Execution shortcuts (F8/F9) are ALSO mirrored into Monaco (App wires
//     `editor.addCommand`) because Monaco owns keydown while the editor is
//     focused and the library's `ignoreInputs` default suppresses bare keys
//     inside the editor's textarea.
//   - Focus shortcuts use `Alt+1..4` rather than `Mod+1..4`: on macOS the
//     browser binds `Cmd+<digit>` to tab switching, so `Mod` (=Cmd there)
//     would never reach us. `Alt+<digit>` is free on every platform and the
//     library matches it via `event.code` even though Option mangles the glyph.
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
type ShortcutScope = "edit" | "debug" | "any";

export interface ShortcutSpec {
  action: ShortcutActionId;
  hotkey: RegisterableHotkey;
  /** Human-readable name; also the merge key when one action has two keys. */
  name: string;
  description: string;
  group: ShortcutGroupTitle;
  scope: ShortcutScope;
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
    // `?` is Shift+/, so the KeyboardEvent's key is "?" with shiftKey true;
    // the library matches on `key` and requires shift to agree, hence the
    // explicit `shift: true`. It is deliberately a bare-ish combo so
    // `ignoreInputs` keeps it from firing while typing.
    action: "help",
    hotkey: { key: "?", shift: true },
    name: "Keyboard shortcuts",
    description: "Open this help dialog",
    group: "Help",
    scope: "any",
  },
];
