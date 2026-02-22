import {
  FilePlusIcon,
  Loader2Icon,
  PlayIcon,
  RotateCcwIcon,
  SquareIcon,
  TrashIcon,
  UploadIcon,
  XCircleIcon,
} from "lucide-react";
import type * as React from "react";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { Button } from "./components/ui/button";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "./components/ui/select";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "./components/ui/tooltip";
import { useAppStore } from "./stores/app-store";
import { useFileStore } from "./stores/file-store";

const sampleFiles = Object.fromEntries(
  Object.entries(
    import.meta.glob<string>("../../samples/*.S", {
      query: "?raw",
      import: "default",
      eager: true,
    }),
  ).map(([path, content]) => [path.replace(/^.*[\\/]/, ""), content]),
);

const DEFAULT_ENTRYPOINT_NAMES = ["main", "start", "run", "entry"];

const InlineFileInput: React.FC<{
  onSubmit: (filename: string) => void;
  onCancel: () => void;
}> = ({ onSubmit, onCancel }) => {
  const [value, setValue] = useState("");
  const inputRef = useRef<HTMLInputElement>(null);

  useEffect(() => {
    inputRef.current?.focus();
  }, []);

  return (
    <form
      className="px-2 py-1"
      onSubmit={(e) => {
        e.preventDefault();
        if (value.trim()) onSubmit(value.trim());
      }}
    >
      <input
        ref={inputRef}
        className="w-full bg-transparent text-sm font-mono outline-none border-b border-ring"
        value={value}
        onChange={(e) => setValue(e.target.value)}
        onBlur={onCancel}
        onKeyDown={(e) => {
          if (e.key === "Escape") onCancel();
        }}
        placeholder="filename.S"
      />
    </form>
  );
};

type FileSidebarProps = {
  onRun: (entrypoint: string) => void;
  compilationStatus: "idle" | "pending" | "success" | "error";
  labels: string[];
  defaultEntrypoint: string | undefined;
};

export const FileSidebar: React.FC<FileSidebarProps> = ({
  onRun,
  compilationStatus,
  labels,
  defaultEntrypoint,
}) => {
  const files = useFileStore((s) => s.files);
  const activeFile = useFileStore((s) => s.activeFile);
  const setActiveFile = useFileStore((s) => s.setActiveFile);
  const createFile = useFileStore((s) => s.createFile);
  const deleteFile = useFileStore((s) => s.deleteFile);
  const resetFiles = useFileStore((s) => s.resetFiles);
  const setContent = useFileStore((s) => s.setContent);

  const mode = useAppStore((s) => s.mode);
  const stopDebug = useAppStore((s) => s.stopDebug);
  const isDebugging = mode.type === "debug";

  const [isCreatingFile, setIsCreatingFile] = useState(false);
  const uploadInputRef = useRef<HTMLInputElement>(null);

  // Track which entrypoint is selected in the dropdown
  const [selectedEntrypoint, setSelectedEntrypoint] = useState<string>("");

  // Reset selection when labels or remembered entrypoint changes
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

  // Filter to touched files during debug, show all during edit
  const displayedFiles = useMemo(() => {
    const allFiles = Object.keys(files);
    if (mode.type !== "debug") return allFiles;
    const touchedFiles = new Set(
      Array.from(mode.sourceMap.values()).map((loc) =>
        loc.file.replace(/^\//, ""),
      ),
    );
    return allFiles.filter((name) => touchedFiles.has(name));
  }, [mode, files]);

  const resetToSamples = useCallback(() => {
    if (
      !confirm(
        "Reset all files to the built-in samples? Your changes will be lost.",
      )
    )
      return;
    resetFiles(sampleFiles, "fact.S");
  }, [resetFiles]);

  const canRun = compilationStatus === "success" && labels.length > 0;

  return (
    <div className="flex flex-col w-48 border-r border-border">
      <div className="flex items-center justify-between px-3 py-2">
        <span className="text-xs font-semibold uppercase tracking-wide text-muted-foreground">
          Files
        </span>
        {!isDebugging && (
          <div className="flex gap-0.5">
            <Tooltip>
              <TooltipTrigger
                render={
                  <Button
                    variant="ghost"
                    size="icon-xs"
                    onClick={() => setIsCreatingFile(true)}
                  />
                }
              >
                <FilePlusIcon />
              </TooltipTrigger>
              <TooltipContent side="bottom">New file</TooltipContent>
            </Tooltip>
            <Tooltip>
              <TooltipTrigger
                render={
                  <Button
                    variant="ghost"
                    size="icon-xs"
                    onClick={() => uploadInputRef.current?.click()}
                  />
                }
              >
                <UploadIcon />
              </TooltipTrigger>
              <TooltipContent side="bottom">Upload file</TooltipContent>
            </Tooltip>
            <Tooltip>
              <TooltipTrigger
                render={
                  <Button
                    variant="ghost"
                    size="icon-xs"
                    onClick={resetToSamples}
                  />
                }
              >
                <RotateCcwIcon />
              </TooltipTrigger>
              <TooltipContent side="bottom">Reset to samples</TooltipContent>
            </Tooltip>
          </div>
        )}
      </div>

      <div className="flex-1 flex flex-col overflow-auto">
        {displayedFiles.map((name) => (
          <div
            className="group flex items-center rounded-sm hover:bg-muted/50 data-[state=selected]:bg-muted"
            key={name}
            data-state={name === activeFile ? "selected" : ""}
          >
            <button
              type="button"
              className="flex-1 px-3 py-1.5 text-left text-sm font-mono truncate"
              onClick={() => setActiveFile(name)}
            >
              {name}
            </button>
            {!isDebugging && (
              <Button
                variant="ghost"
                size="icon-xs"
                className="opacity-0 group-hover:opacity-100 mr-1"
                onClick={() => deleteFile(name)}
              >
                <TrashIcon />
              </Button>
            )}
          </div>
        ))}

        {isCreatingFile && (
          <InlineFileInput
            onSubmit={(filename) => {
              createFile(filename, "");
              setIsCreatingFile(false);
            }}
            onCancel={() => setIsCreatingFile(false)}
          />
        )}
      </div>

      <div className="p-2 flex flex-col gap-1">
        {isDebugging ? (
          <Button
            type="button"
            variant="destructive"
            className="w-full"
            onClick={stopDebug}
          >
            <SquareIcon className="mr-2 h-4 w-4" />
            Stop Debug
          </Button>
        ) : canRun ? (
          <div className="flex gap-1">
            <div className="flex-1 min-w-0">
              <Select
                value={selectedEntrypoint}
                onValueChange={(v) => {
                  if (v !== null) setSelectedEntrypoint(v);
                }}
              >
                <SelectTrigger className="font-mono text-xs w-full">
                  <SelectValue />
                </SelectTrigger>
                <SelectContent>
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
            </div>
            <Button
              type="button"
              size="icon"
              disabled={!selectedEntrypoint}
              onClick={() => {
                if (selectedEntrypoint) onRun(selectedEntrypoint);
              }}
            >
              <PlayIcon className="h-4 w-4" />
            </Button>
          </div>
        ) : (
          <Button type="button" className="w-full" disabled>
            {compilationStatus === "pending" ? (
              <Loader2Icon className="animate-spin mr-2 h-4 w-4" />
            ) : compilationStatus === "error" ? (
              <XCircleIcon className="mr-2 h-4 w-4" />
            ) : null}
            {compilationStatus === "pending" ? "Compiling…" : "Run"}
          </Button>
        )}
      </div>

      <input
        ref={uploadInputRef}
        type="file"
        multiple
        className="hidden"
        onChange={(e) => {
          const uploadedFiles = e.target.files;
          if (!uploadedFiles) return;
          for (const file of uploadedFiles) {
            const reader = new FileReader();
            reader.onload = () => {
              const content = reader.result as string;
              setContent(file.name, content);
              setActiveFile(file.name);
            };
            reader.readAsText(file);
          }
          e.target.value = "";
        }}
      />
    </div>
  );
};
