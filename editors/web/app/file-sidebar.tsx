import {
  DownloadIcon,
  FilePlusIcon,
  RotateCcwIcon,
  TrashIcon,
  UploadIcon,
} from "lucide-react";
import type * as React from "react";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import {
  AlertDialog,
  AlertDialogAction,
  AlertDialogCancel,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogHeader,
  AlertDialogTitle,
  AlertDialogTrigger,
} from "./components/ui/alert-dialog";
import { Button } from "./components/ui/button";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "./components/ui/tooltip";
import { useFileDrop } from "./hooks/use-file-drop";
import { useAppStore } from "./stores/app-store";
import { useFileStore } from "./stores/file-store";

const InlineFileInput: React.FC<{
  onSubmit: (filename: string) => void;
  onCancel: () => void;
}> = ({ onSubmit, onCancel }) => {
  const inputRef = useRef<HTMLInputElement>(null);

  useEffect(() => {
    inputRef.current?.focus();
  }, []);

  return (
    <form
      className="px-2 py-1"
      onSubmit={(e: React.SubmitEvent<HTMLFormElement>) => {
        e.preventDefault();
        const data = new FormData(e.target);
        const filename = data.get("filename");
        if (typeof filename !== "string") throw new Error();
        if (filename.trim()) onSubmit(filename.trim());
      }}
    >
      <input
        ref={inputRef}
        name="filename"
        aria-label="File name"
        className="w-full bg-transparent text-sm font-mono outline-none border-b border-ring"
        onBlur={onCancel}
        onKeyDown={(e) => {
          if (e.key === "Escape") onCancel();
        }}
        placeholder="filename.s"
      />
    </form>
  );
};

type FileSidebarViewProps = Pick<
  ReturnType<typeof useFileDrop>,
  "isWindowDragging" | "isOverDropZone" | "dropZoneRef" | "dropZoneProps"
> & {
  /** File names to show (already filtered to the current mode). */
  files: string[];
  activeFile: string;
  isCreatingFile: boolean;
  onSelect: (name: string) => void;
  onStartCreate: () => void;
  onCreate: (name: string) => void;
  onCancelCreate: () => void;
  onDelete: (name: string) => void;
  onDownload: (name: string) => void;
  onUpload: (files: readonly File[]) => void;
  onReset: () => void;
};

/**
 * Pure presentational file sidebar: all data and actions arrive as props, so it
 * has no store coupling and can be rendered standalone (stories/tests). The
 * reset-confirmation dialog's open state is ephemeral UI, kept local here.
 */
export const FileSidebarView: React.FC<FileSidebarViewProps> = ({
  files,
  activeFile,
  isCreatingFile,
  isWindowDragging,
  isOverDropZone,
  dropZoneRef,
  dropZoneProps,
  onSelect,
  onStartCreate,
  onCreate,
  onCancelCreate,
  onDelete,
  onDownload,
  onUpload,
  onReset,
}) => {
  const uploadInputRef = useRef<HTMLInputElement>(null);
  const [resetDialogOpen, setResetDialogOpen] = useState(false);

  return (
    <div
      ref={dropZoneRef}
      role="navigation"
      aria-label="Files"
      className="relative flex flex-col w-48 border-r border-border p-2"
      {...dropZoneProps}
    >
      {isWindowDragging && (
        <div
          data-over={isOverDropZone}
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
                  aria-label="New file"
                  onClick={onStartCreate}
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
                  aria-label="Upload file"
                  onClick={() => uploadInputRef.current?.click()}
                />
              }
            >
              <UploadIcon />
            </TooltipTrigger>
            <TooltipContent side="bottom">Upload file</TooltipContent>
          </Tooltip>
          <AlertDialog open={resetDialogOpen} onOpenChange={setResetDialogOpen}>
            <Tooltip>
              <TooltipTrigger
                render={
                  <AlertDialogTrigger
                    render={
                      <Button
                        variant="ghost"
                        size="icon-xs"
                        aria-label="Reset to samples"
                      />
                    }
                  />
                }
              >
                <RotateCcwIcon />
              </TooltipTrigger>
              <TooltipContent side="bottom">Reset to samples</TooltipContent>
            </Tooltip>
            <AlertDialogContent size="sm">
              <AlertDialogHeader>
                <AlertDialogTitle>Reset to samples?</AlertDialogTitle>
                <AlertDialogDescription>
                  This will replace all files with the built-in samples. Your
                  changes will be lost.
                </AlertDialogDescription>
              </AlertDialogHeader>
              <AlertDialogFooter>
                <AlertDialogCancel>Cancel</AlertDialogCancel>
                <AlertDialogAction
                  variant="destructive"
                  onClick={() => {
                    onReset();
                    setResetDialogOpen(false);
                  }}
                >
                  Reset
                </AlertDialogAction>
              </AlertDialogFooter>
            </AlertDialogContent>
          </AlertDialog>
        </div>
      </div>

      <div className="flex-1 flex flex-col gap-1 overflow-auto">
        {files.map((name) => (
          <div
            className="group flex items-center rounded-sm hover:bg-muted/50 data-[state=selected]:bg-muted"
            key={name}
            data-state={name === activeFile ? "selected" : ""}
          >
            <button
              type="button"
              className="flex-1 px-3 py-1.5 text-left text-sm font-mono truncate"
              onClick={() => {
                onSelect(name);
              }}
            >
              {name}
            </button>
            <Tooltip>
              <TooltipTrigger
                render={
                  <Button
                    variant="ghost"
                    size="icon-xs"
                    aria-label="Download"
                    className="opacity-0 group-hover:opacity-100"
                    onClick={() => {
                      onDownload(name);
                    }}
                  />
                }
              >
                <DownloadIcon />
              </TooltipTrigger>
              <TooltipContent side="bottom">Download</TooltipContent>
            </Tooltip>
            <Tooltip>
              <TooltipTrigger
                render={
                  <Button
                    variant="ghost"
                    size="icon-xs"
                    aria-label="Delete"
                    className="opacity-0 group-hover:opacity-100 mr-1"
                    onClick={() => {
                      onDelete(name);
                    }}
                  />
                }
              >
                <TrashIcon />
              </TooltipTrigger>
              <TooltipContent side="bottom">Delete</TooltipContent>
            </Tooltip>
          </div>
        ))}

        {isCreatingFile && (
          <InlineFileInput onSubmit={onCreate} onCancel={onCancelCreate} />
        )}
      </div>

      <input
        ref={uploadInputRef}
        type="file"
        multiple
        aria-label="Upload files"
        className="sr-only"
        onChange={(e: React.ChangeEvent<HTMLInputElement>) => {
          if (e.target.files) onUpload(Array.from(e.target.files));
          e.target.value = "";
        }}
      />
    </div>
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

  const processFiles = useCallback(
    (fileList: readonly File[]) => {
      for (const file of fileList) {
        void file.text().then((text) => {
          setContent(file.name, text);
          setActiveFile(file.name);
        });
      }
    },
    [setContent, setActiveFile],
  );

  const { isWindowDragging, isOverDropZone, dropZoneRef, dropZoneProps } =
    useFileDrop(processFiles);

  // Filter to touched files during debug, show all during edit
  const displayedFiles = useMemo(() => {
    const allFiles = Object.keys(files);
    if (mode.type !== "debug") return allFiles;
    const touched = new Set(mode.touchedFiles);
    return allFiles.filter((name) => touched.has(name));
  }, [mode, files]);

  const handleDownload = useCallback(
    (name: string) => {
      const content = files[name];
      if (content === undefined) return;
      const blob = new Blob([content], { type: "text/plain" });
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = name;
      a.click();
      URL.revokeObjectURL(url);
    },
    [files],
  );

  if (isDebugging) return null;

  return (
    <FileSidebarView
      files={displayedFiles}
      activeFile={activeFile}
      isCreatingFile={isCreatingFile}
      isWindowDragging={isWindowDragging}
      isOverDropZone={isOverDropZone}
      dropZoneRef={dropZoneRef}
      dropZoneProps={dropZoneProps}
      onSelect={setActiveFile}
      onStartCreate={() => {
        setIsCreatingFile(true);
      }}
      onCreate={(filename) => {
        createFile(filename, "");
        setIsCreatingFile(false);
      }}
      onCancelCreate={() => {
        setIsCreatingFile(false);
      }}
      onDelete={deleteFile}
      onDownload={handleDownload}
      onUpload={processFiles}
      onReset={resetFiles}
    />
  );
};
