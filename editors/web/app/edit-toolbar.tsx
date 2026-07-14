import {
  CheckCircle2Icon,
  Loader2Icon,
  PlayIcon,
  XCircleIcon,
} from "lucide-react";
import { memo } from "react";
import { Button } from "./components/ui/button";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "./components/ui/select";
import { DocsButton } from "./docs-button";
import { ThemeSwitcher } from "./theme-switcher";

const DEFAULT_ENTRYPOINT_NAMES = ["main", "start", "run", "entry"];

/**
 * Choose the entrypoint to preselect: the preferred label if still present,
 * else the first well-known entrypoint name that exists, else the first label,
 * else the empty string.
 */
export function pickEntrypoint(
  labels: string[],
  preferred: string | undefined,
): string {
  if (preferred && labels.includes(preferred)) return preferred;
  for (const candidate of DEFAULT_ENTRYPOINT_NAMES) {
    if (labels.includes(candidate)) return candidate;
  }
  return labels[0] ?? "";
}

type EditToolbarProps = {
  onRun: (entrypoint: string) => void;
  compilationStatus: "idle" | "pending" | "success" | "error";
  compilationError: string | undefined;
  labels: string[];
  defaultEntrypoint: string | undefined;
};

export const EditToolbar: React.FC<EditToolbarProps> = memo(
  ({
    onRun,
    compilationStatus,
    compilationError,
    labels,
    defaultEntrypoint,
  }) => {
    const canRun = compilationStatus === "success" && labels.length > 0;

    return (
      <div
        role="toolbar"
        aria-label="Edit"
        className="flex items-center gap-2 px-2 py-1 border-b border-border bg-muted/30 shrink-0"
      >
        {compilationStatus === "pending" && (
          <span role="status" aria-label="Compiling">
            <Loader2Icon className="shrink-0 size-3.5 animate-spin text-muted-foreground" />
          </span>
        )}
        {compilationStatus === "error" && (
          <span
            role="status"
            aria-label="Compilation error"
            className="flex items-center gap-2"
          >
            <XCircleIcon className="shrink-0 size-3.5 text-destructive" />
            {compilationError && (
              <span className="text-xs text-destructive truncate">
                {compilationError}
              </span>
            )}
          </span>
        )}
        {compilationStatus === "success" && (
          <span role="status" aria-label="Compilation succeeded">
            <CheckCircle2Icon className="shrink-0 size-3.5 text-green-600 dark:text-green-400" />
          </span>
        )}

        <div className="ml-auto flex items-center gap-1">
          {canRun && (
            // Remount on a changed label set so the uncontrolled Select picks
            // up the fresh default entrypoint.
            <form
              key={labels.join(" ")}
              className="flex items-center gap-1"
              onSubmit={(e: React.SubmitEvent<HTMLFormElement>) => {
                e.preventDefault();
                const value = new FormData(e.currentTarget).get("entrypoint");
                if (typeof value === "string" && value) onRun(value);
              }}
            >
              <span className="text-xs text-muted-foreground">Start at</span>
              <Select
                name="entrypoint"
                defaultValue={pickEntrypoint(labels, defaultEntrypoint)}
              >
                <SelectTrigger
                  size="xs"
                  className="font-mono w-32"
                  aria-label="Entrypoint"
                >
                  <SelectValue />
                </SelectTrigger>
                <SelectContent align="end" side="bottom">
                  {labels.map((label) => (
                    <SelectItem
                      key={label}
                      value={label}
                      className="font-mono text-xs"
                    >
                      {label}
                    </SelectItem>
                  ))}
                </SelectContent>
              </Select>
              <Button type="submit" size="xs">
                <PlayIcon data-icon="inline-start" />
                Run
              </Button>
            </form>
          )}
          <DocsButton />
          <ThemeSwitcher />
        </div>
      </div>
    );
  },
);
EditToolbar.displayName = "EditToolbar";
