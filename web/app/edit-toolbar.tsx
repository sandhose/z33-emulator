import {
  BookOpenIcon,
  CheckCircle2Icon,
  Loader2Icon,
  PlayIcon,
  XCircleIcon,
} from "lucide-react";
import { memo, useEffect, useState } from "react";
import { Button } from "./components/ui/button";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "./components/ui/select";
import { ThemeSwitcher } from "./theme-switcher";

const DEFAULT_ENTRYPOINT_NAMES = ["main", "start", "run", "entry"];

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
    const [selectedEntrypoint, setSelectedEntrypoint] = useState<string>("");

    useEffect(() => {
      if (defaultEntrypoint && labels.includes(defaultEntrypoint)) {
        setSelectedEntrypoint(defaultEntrypoint);
        return;
      }
      for (const candidate of DEFAULT_ENTRYPOINT_NAMES) {
        if (labels.includes(candidate)) {
          setSelectedEntrypoint(candidate);
          return;
        }
      }
      setSelectedEntrypoint(labels[0] ?? "");
    }, [labels, defaultEntrypoint]);

    const canRun = compilationStatus === "success" && labels.length > 0;

    return (
      <div className="flex items-center gap-2 px-2 py-1 border-b border-border bg-muted/30 shrink-0">
        {compilationStatus === "pending" && (
          <Loader2Icon className="shrink-0 size-3.5 animate-spin text-muted-foreground" />
        )}
        {compilationStatus === "error" && (
          <>
            <XCircleIcon className="shrink-0 size-3.5 text-destructive" />
            {compilationError && (
              <span className="text-xs text-destructive truncate">
                {compilationError}
              </span>
            )}
          </>
        )}
        {compilationStatus === "success" && (
          <CheckCircle2Icon className="shrink-0 size-3.5 text-green-600 dark:text-green-400" />
        )}

        <div className="ml-auto flex items-center gap-1">
          {canRun && (
            <>
              <Select
                value={selectedEntrypoint}
                onValueChange={(v) => {
                  if (v !== null) setSelectedEntrypoint(v);
                }}
              >
                <SelectTrigger size="xs" className="font-mono w-32">
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
              <Button
                type="button"
                size="xs"
                disabled={!selectedEntrypoint}
                onClick={() => {
                  if (selectedEntrypoint) onRun(selectedEntrypoint);
                }}
              >
                <PlayIcon data-icon="inline-start" />
                Run
              </Button>
            </>
          )}
          <Button
            variant="ghost"
            size="xs"
            render={
              // biome-ignore lint/a11y/useAnchorContent: Button renders its children inside this anchor
              <a
                href="https://pdagog.gitlab.io/ens/z33refcard-fr.pdf"
                target="_blank"
                rel="noopener noreferrer"
              />
            }
          >
            <BookOpenIcon data-icon="inline-start" />
            Docs
          </Button>
          <ThemeSwitcher />
        </div>
      </div>
    );
  },
);
EditToolbar.displayName = "EditToolbar";
