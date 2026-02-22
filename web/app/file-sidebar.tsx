import type { Monaco } from "@monaco-editor/react";
import {
  FilePlusIcon,
  RotateCcwIcon,
  SquareIcon,
  TrashIcon,
  UploadIcon,
} from "lucide-react";
import type { Uri } from "monaco-editor/esm/vs/editor/editor.api.js";
import type * as React from "react";
import { useCallback, useEffect, useRef, useState } from "react";
import { Button } from "./components/ui/button";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "./components/ui/tooltip";
import { saveWorkspace } from "./file-store";

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

function collectFiles(monaco: Monaco): Map<string, string> {
  const files = new Map<string, string>();
  for (const model of monaco.editor.getModels()) {
    files.set(model.uri.path, model.getValue());
  }
  return files;
}

type FileSidebarProps = {
  monaco: Monaco | null;
  fileName: Uri;
  fileNames: Uri[];
  onSwitchFile: (uri: Uri) => void;
  onCompileAndRun: () => void;
  isDebugging: boolean;
  onStopDebug: () => void;
  sampleFiles: () => Map<string, string>;
  onSync: () => void;
};

export const FileSidebar: React.FC<FileSidebarProps> = ({
  monaco,
  fileName,
  fileNames,
  onSwitchFile,
  onCompileAndRun,
  isDebugging,
  onStopDebug,
  sampleFiles,
  onSync,
}) => {
  const [isCreatingFile, setIsCreatingFile] = useState(false);
  const uploadInputRef = useRef<HTMLInputElement>(null);

  const persistWorkspace = useCallback(() => {
    if (!monaco) return;
    saveWorkspace(collectFiles(monaco));
  }, [monaco]);

  function switchFile(name: Uri) {
    onSwitchFile(name);
  }

  function resetToSamples() {
    if (!monaco) return;
    if (
      !confirm(
        "Reset all files to the built-in samples? Your changes will be lost.",
      )
    )
      return;

    for (const model of monaco.editor.getModels()) {
      model.dispose();
    }

    const samples = sampleFiles();
    for (const [name, content] of samples) {
      monaco.editor.createModel(content, "z33", monaco.Uri.file(name));
    }

    const defaultFile = monaco.Uri.file("fact.S");
    onSwitchFile(defaultFile);
    persistWorkspace();
    onSync();
  }

  return (
    <div className="flex flex-col w-48 border-r border-border">
      <div className="flex items-center justify-between px-2 py-1">
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
        {fileNames.map((name) => (
          <div
            className="group flex items-center rounded-sm hover:bg-muted/50 data-[state=selected]:bg-muted"
            key={name.path}
            data-state={name.path === fileName.path ? "selected" : ""}
          >
            <button
              type="button"
              className="flex-1 px-2 py-1 text-left text-sm font-mono truncate"
              onClick={() => switchFile(name)}
            >
              {name.path.replace(/^\//, "")}
            </button>
            {!isDebugging && (
              <Button
                variant="ghost"
                size="icon-xs"
                className="opacity-0 group-hover:opacity-100 mr-1"
                onClick={() => {
                  if (!monaco) return;
                  for (const model of monaco.editor.getModels()) {
                    if (model.uri.path === name.path) {
                      model.dispose();
                      break;
                    }
                  }
                  persistWorkspace();
                  onSync();
                }}
              >
                <TrashIcon />
              </Button>
            )}
          </div>
        ))}

        {isCreatingFile && (
          <InlineFileInput
            onSubmit={(filename) => {
              if (!monaco) return;
              const uri = monaco.Uri.file(filename);
              if (!monaco.editor.getModel(uri)) {
                monaco.editor.createModel("", "z33", uri);
              }
              switchFile(uri);
              persistWorkspace();
              onSync();
              setIsCreatingFile(false);
            }}
            onCancel={() => setIsCreatingFile(false)}
          />
        )}
      </div>

      <div className="p-2">
        {isDebugging ? (
          <Button
            type="button"
            variant="destructive"
            className="w-full"
            onClick={onStopDebug}
          >
            <SquareIcon className="mr-2 h-4 w-4" />
            Stop Debug
          </Button>
        ) : (
          <Button type="button" className="w-full" onClick={onCompileAndRun}>
            Compile
          </Button>
        )}
      </div>

      <input
        ref={uploadInputRef}
        type="file"
        multiple
        className="hidden"
        onChange={(e) => {
          const files = e.target.files;
          if (!files || !monaco) return;
          for (const file of files) {
            const reader = new FileReader();
            reader.onload = () => {
              const uri = monaco.Uri.file(file.name);
              const existing = monaco.editor.getModel(uri);
              if (existing) {
                existing.setValue(reader.result as string);
              } else {
                monaco.editor.createModel(reader.result as string, "z33", uri);
              }
              switchFile(uri);
              persistWorkspace();
              onSync();
            };
            reader.readAsText(file);
          }
          e.target.value = "";
        }}
      />
    </div>
  );
};
