import { Dialog } from "@base-ui/react/dialog";
import { XIcon } from "lucide-react";
import { useMemo } from "react";
import { Button } from "./components/ui/button";
import {
  formatHotkey,
  type HotkeyRegistrationView,
  useAppHotkeyRegistrations,
} from "./hooks/use-app-hotkey";
import { cn } from "./lib/utils";
import { SHORTCUT_GROUP_ORDER, SHORTCUTS } from "./shortcuts";

interface ShortcutRow {
  name: string;
  description?: string;
  /** Display strings for the key(s) that trigger this action. */
  keys: string[];
}

export interface ShortcutGroup {
  title: string;
  rows: ShortcutRow[];
}

const Kbd: React.FC<{ children: React.ReactNode }> = ({ children }) => (
  <kbd className="inline-flex h-5 min-w-5 items-center justify-center rounded border border-border bg-muted px-1.5 font-mono text-[11px] font-medium text-muted-foreground">
    {children}
  </kbd>
);

/**
 * Pure, presentational shortcuts list: grouped rows of name + description and
 * their key chip(s). No coupling to the hotkey registry, so it renders
 * standalone in stories and tests.
 */
export const ShortcutsSheet: React.FC<{ groups: ShortcutGroup[] }> = ({
  groups,
}) => (
  <div className="flex flex-col gap-4">
    {groups.map((group) => (
      <section
        key={group.title}
        aria-label={group.title}
        className="flex flex-col gap-1.5"
      >
        <h3 className="text-xs font-semibold uppercase tracking-wide text-muted-foreground">
          {group.title}
        </h3>
        <ul className="flex flex-col">
          {group.rows.map((row) => (
            <li
              key={row.name}
              className="flex items-center justify-between gap-6 py-1"
            >
              <span className="min-w-0">
                <span className="block text-sm text-foreground">
                  {row.name}
                </span>
                {row.description && (
                  <span className="block text-xs text-muted-foreground">
                    {row.description}
                  </span>
                )}
              </span>
              <span className="flex shrink-0 items-center gap-1.5">
                {row.keys.map((key, index) => (
                  <span key={key} className="flex items-center gap-1.5">
                    {index > 0 && (
                      <span className="text-[11px] text-muted-foreground">
                        or
                      </span>
                    )}
                    <Kbd>{key}</Kbd>
                  </span>
                ))}
              </span>
            </li>
          ))}
        </ul>
      </section>
    ))}
  </div>
);

/** Pure dialog chrome (title, description, close); the body is `children`. */
const ShortcutsDialogChrome: React.FC<{
  open: boolean;
  onOpenChange: (open: boolean) => void;
  children: React.ReactNode;
}> = ({ open, onOpenChange, children }) => (
  <Dialog.Root open={open} onOpenChange={onOpenChange}>
    <Dialog.Portal>
      <Dialog.Backdrop
        className={cn(
          "fixed inset-0 z-50 bg-black/10 supports-backdrop-filter:backdrop-blur-xs data-open:animate-in data-open:fade-in-0 data-closed:animate-out data-closed:fade-out-0",
        )}
      />
      <Dialog.Popup
        className={cn(
          "fixed top-1/2 left-1/2 z-50 flex max-h-[85vh] w-full max-w-md -translate-x-1/2 -translate-y-1/2 flex-col gap-4 overflow-y-auto rounded-xl bg-background p-4 ring-1 ring-foreground/10 outline-none data-open:animate-in data-open:fade-in-0 data-open:zoom-in-95 data-closed:animate-out data-closed:fade-out-0 data-closed:zoom-out-95",
        )}
      >
        <div className="flex items-center justify-between gap-4">
          <Dialog.Title className="text-base font-medium">
            Keyboard shortcuts
          </Dialog.Title>
          <Dialog.Close
            render={
              <Button variant="ghost" size="icon-xs" aria-label="Close">
                <XIcon />
              </Button>
            }
          />
        </div>
        <Dialog.Description className="sr-only">
          A list of every available keyboard shortcut, grouped by area.
        </Dialog.Description>
        {children}
      </Dialog.Popup>
    </Dialog.Portal>
  </Dialog.Root>
);

/**
 * Pure dialog fed groups via props (no registry). Kept separate from the
 * connected component so it can be storied and axe-checked deterministically.
 */
export const ShortcutsDialogView: React.FC<{
  open: boolean;
  onOpenChange: (open: boolean) => void;
  groups: ShortcutGroup[];
}> = ({ open, onOpenChange, groups }) => (
  <ShortcutsDialogChrome open={open} onOpenChange={onOpenChange}>
    <ShortcutsSheet groups={groups} />
  </ShortcutsDialogChrome>
);

/**
 * Builds the display groups from the live registry: dedupes by action name
 * (merging the keys of, e.g., `⌘/` and `?`), keeps only currently registered
 * shortcuts, and orders groups/rows per `SHORTCUTS`.
 */
function buildShortcutGroups(
  registrations: HotkeyRegistrationView[],
): ShortcutGroup[] {
  const specNames = new Set(SHORTCUTS.map((spec) => spec.name));
  const keysByName = new Map<string, string[]>();

  for (const registration of registrations) {
    const name = registration.options.meta?.name;
    if (!name || !specNames.has(name)) continue;
    const display = formatHotkey(registration.hotkey);
    const keys = keysByName.get(name) ?? [];
    if (!keys.includes(display)) keys.push(display);
    keysByName.set(name, keys);
  }

  const groups: ShortcutGroup[] = [];
  for (const title of SHORTCUT_GROUP_ORDER) {
    const rows: ShortcutRow[] = [];
    const seen = new Set<string>();
    for (const spec of SHORTCUTS) {
      if (spec.group !== title || seen.has(spec.name)) continue;
      const keys = keysByName.get(spec.name);
      if (!keys || keys.length === 0) continue;
      seen.add(spec.name);
      rows.push({ name: spec.name, description: spec.description, keys });
    }
    if (rows.length > 0) groups.push({ title, rows });
  }
  return groups;
}

/**
 * Reads the live registry and renders the sheet. Mounted ONLY while the dialog
 * is open: the registration hook re-`setOptions` during render, so a
 * permanently-mounted subscriber would be updated mid-render on every app
 * re-render. It is safe while open because registrations don't change then.
 */
const ConnectedShortcutsSheet: React.FC = () => {
  const registrations = useAppHotkeyRegistrations();
  const groups = useMemo(
    () => buildShortcutGroups(registrations),
    [registrations],
  );
  return <ShortcutsSheet groups={groups} />;
};

/** Connected help dialog: reads the live hotkey registry while open. */
export const ShortcutsHelpDialog: React.FC<{
  open: boolean;
  onOpenChange: (open: boolean) => void;
}> = ({ open, onOpenChange }) => (
  <ShortcutsDialogChrome open={open} onOpenChange={onOpenChange}>
    {open && <ConnectedShortcutsSheet />}
  </ShortcutsDialogChrome>
);
