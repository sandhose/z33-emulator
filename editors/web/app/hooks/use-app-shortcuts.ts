import { SHORTCUTS, type ShortcutActionId } from "../shortcuts";
import { type AppHotkeyDefinition, useAppHotkeys } from "./use-app-hotkey";

/** One handler per action id; the same handler backs its toolbar button. */
export type ShortcutActions = Record<ShortcutActionId, () => void>;

/**
 * Registers every app shortcut at the document level. Each is always
 * registered (so the help dialog can list the full set) but soft-disabled via
 * `enabled` when its scope doesn't match the current mode. Focus shortcuts opt
 * out of `ignoreInputs` so panel navigation works even from a focused field.
 *
 * Execution shortcuts (F8/F9) are ALSO mirrored into Monaco by the caller —
 * the library can't see keydown while the editor's textarea is focused.
 */
export function useAppShortcuts(
  mode: "edit" | "debug",
  actions: ShortcutActions,
): void {
  const definitions: AppHotkeyDefinition[] = SHORTCUTS.map((spec) => ({
    hotkey: spec.hotkey,
    callback: () => {
      actions[spec.action]();
    },
    options: {
      enabled: spec.scope === "any" || spec.scope === mode,
      meta: { name: spec.name, description: spec.description },
      ...(spec.group === "Panels" ? { ignoreInputs: false } : {}),
    },
  }));

  useAppHotkeys(definitions);
}
