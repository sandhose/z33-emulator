import {
  FilePlusIcon,
  RotateCcwIcon,
  TrashIcon,
  UploadIcon,
} from "lucide-react";
import type * as React from "react";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { Button } from "./components/ui/button";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "./components/ui/tooltip";
import { useAppStore } from "./stores/app-store";
import { useFileStore } from "./stores/file-store";

const sampleFiles = Object.fromEntries(
  Object.entries(
    import.meta.glob<string>("../../samples/*.s", {
      query: "?raw",
      import: "default",
      eager: true,
    }),
  ).map(([path, content]) => [path.replace(/^.*[\\/]/, ""), content]),
);

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

export const FileSidebar: React.FC = () => {
  const files = useFileStore((s) => s.files);
  const activeFile = useFileStore((s) => s.activeFile);
  const setActiveFile = useFileStore((s) => s.setActiveFile);
  const createFile = useFileStore((s) => s.createFile);
  const deleteFile = useFileStore((s) => s.deleteFile);
  const resetFiles = useFileStore((s) => s.resetFiles);
  const setContent = useFileStore((s) => s.setContent);

  const mode = useAppStore((s) => s.mode);
  const isDebugging = mode.type === "debug";

  const [isCreatingFile, setIsCreatingFile] = useState(false);
  const [isWindowDragging, setIsWindowDragging] = useState(false);
  const [isSidebarDragging, setIsSidebarDragging] = useState(false);
  const uploadInputRef = useRef<HTMLInputElement>(null);
  const dropZoneRef = useRef<HTMLDivElement>(null);
  const dismissTimeoutRef = useRef<ReturnType<typeof setTimeout>>(undefined);

  // Detect file-drag over the window using dragover as a heartbeat.
  // dragover fires continuously (~50-350ms depending on browser) while the drag
  // is inside the viewport. We show the overlay on dragenter and schedule a
  // dismiss timeout on every dragover; as long as events keep coming the timeout
  // resets. When the drag leaves the window dragover stops and the timeout fires.
  // This avoids all the dragleave/relatedTarget edge-cases that plague other
  // approaches (iframes, shadow DOM, DOM mutations from the overlay itself).
  useEffect(() => {
    const scheduleDismiss = () => {
      clearTimeout(dismissTimeoutRef.current);
      dismissTimeoutRef.current = setTimeout(() => {
        setIsWindowDragging(false);
        setIsSidebarDragging(false);
      }, 500);
    };

    const handleDragEnter = (e: DragEvent) => {
      e.preventDefault();
      if (e.dataTransfer?.types.includes("Files")) {
        setIsWindowDragging(true);
        scheduleDismiss();
      }
    };
    const handleDragOver = (e: DragEvent) => {
      e.preventDefault();
      if (e.dataTransfer?.types.includes("Files")) scheduleDismiss();
    };
    const handleDrop = (e: DragEvent) => {
      e.preventDefault();
      clearTimeout(dismissTimeoutRef.current);
      setIsWindowDragging(false);
      setIsSidebarDragging(false);
    };

    window.addEventListener("dragenter", handleDragEnter);
    window.addEventListener("dragover", handleDragOver);
    window.addEventListener("drop", handleDrop);
    return () => {
      clearTimeout(dismissTimeoutRef.current);
      window.removeEventListener("dragenter", handleDragEnter);
      window.removeEventListener("dragover", handleDragOver);
      window.removeEventListener("drop", handleDrop);
    };
  }, []);

  const processFiles = useCallback(
    (fileList: FileList) => {
      for (const file of fileList) {
        const reader = new FileReader();
        reader.onload = () => {
          setContent(file.name, reader.result as string);
          setActiveFile(file.name);
        };
        reader.readAsText(file);
      }
    },
    [setContent, setActiveFile],
  );

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
    resetFiles(sampleFiles, "fact.s");
  }, [resetFiles]);

  if (isDebugging) return null;

  return (
    // biome-ignore lint/a11y/noStaticElementInteractions: drop zone is fine right?
    <div
      ref={dropZoneRef}
      className="relative flex flex-col w-48 border-r border-border p-2"
      onDragOver={(e) => e.preventDefault()}
      onDragEnter={(e) => {
        e.preventDefault();
        if (e.dataTransfer.types.includes("Files")) setIsSidebarDragging(true);
      }}
      onDragLeave={(e) => {
        e.preventDefault();
        if (!dropZoneRef.current?.contains(e.relatedTarget as Node)) {
          setIsSidebarDragging(false);
        }
      }}
      onDrop={(e) => {
        e.preventDefault();
        e.stopPropagation();
        setIsWindowDragging(false);
        setIsSidebarDragging(false);
        if (e.dataTransfer.files.length > 0) processFiles(e.dataTransfer.files);
      }}
    >
      {isWindowDragging && (
        <div
          data-over={isSidebarDragging}
          className="absolute inset-0 z-10 flex flex-col items-center justify-center gap-2 rounded-sm border-2 border-dashed pointer-events-none transition-all border-primary/40 bg-primary/5 opacity-50 data-[over=true]:border-primary data-[over=true]:bg-primary/10 data-[over=true]:opacity-100"
        >
          <UploadIcon className="size-6 text-primary" />
          <span className="text-xs text-primary font-medium">
            Drop files here
          </span>
        </div>
      )}

      <div className="flex items-center justify-between px-3 py-2">
        <span className="text-xs font-semibold uppercase tracking-wide text-muted-foreground">
          Files
        </span>
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
      </div>

      <div className="flex-1 flex flex-col gap-1 overflow-auto">
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
            <Button
              variant="ghost"
              size="icon-xs"
              className="opacity-0 group-hover:opacity-100 mr-1"
              onClick={() => deleteFile(name)}
            >
              <TrashIcon />
            </Button>
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

      <input
        ref={uploadInputRef}
        type="file"
        multiple
        className="sr-only"
        onChange={(e) => {
          if (e.target.files) processFiles(e.target.files);
          e.target.value = "";
        }}
      />
    </div>
  );
};
