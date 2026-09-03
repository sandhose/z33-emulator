import { SHORTCUTS, type ShortcutActionId } from "../shortcuts";
import { type AppHotkeyDefinition, useAppHotkeys } from "./use-app-hotkey";

/** One handler per action id; the same handler backs its toolbar button. */
export type ShortcutActions = Record<ShortcutActionId, () => void>;

/**
 * Registers every app shortcut at the document level. A shortcut outside the
 * current mode's scope stays registered, so the help dialog lists the full set,
 * and `enabled` soft-disables it. Focus shortcuts opt out of `ignoreInputs` to
 * reach panels from a focused field.
 *
 * The caller mirrors the execution shortcuts (F8/F9) into Monaco, which owns
 * keydown while the editor's textarea has focus.
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
