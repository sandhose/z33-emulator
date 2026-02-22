import { Editor, type Monaco, useMonaco } from "@monaco-editor/react";
import {
  FilePlusIcon,
  RotateCcwIcon,
  TrashIcon,
  UploadIcon,
} from "lucide-react";
import { Uri } from "monaco-editor/esm/vs/editor/editor.api.js";
import type * as React from "react";
import { useCallback, useEffect, useRef, useState } from "react";
import {
  type Computer,
  InMemoryPreprocessor,
  type Program,
} from "z33-web-bindings";
import type { ZodError } from "zod";
import { useTheme } from "./components/theme-provider";
import { Button } from "./components/ui/button";
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from "./components/ui/popover";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "./components/ui/tooltip";
import { EntrypointSelector } from "./entrypoint-selector";
import { saveActiveFile, saveWorkspace } from "./file-store";
import { reportSchema, toMonacoDecoration } from "./report";

type Props = {
  initialFiles: Map<string, string>;
  initialSelected: string;
  sampleFiles: () => Map<string, string>;
  onCompile?: (files: Map<string, string>, selected: string) => void;
  onComputer?: (computer: Computer) => void;
};

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

export const MultiFileEditor: React.FC<Props> = ({
  initialFiles,
  initialSelected,
  sampleFiles,
  onComputer,
}: Props) => {
  const monaco = useMonaco();
  const [fileName, setFileName] = useState(Uri.file(initialSelected));
  const [fileNames, setFileNames] = useState<Uri[]>([]);
  const [program, setProgram] = useState<Program | null>(null);
  const [isCreatingFile, setIsCreatingFile] = useState(false);
  const uploadInputRef = useRef<HTMLInputElement>(null);
  const theme = useTheme();
  const saveTimerRef = useRef<ReturnType<typeof setTimeout> | null>(null);

  const persistWorkspace = useCallback(() => {
    if (!monaco) return;
    saveWorkspace(collectFiles(monaco));
  }, [monaco]);

  const debouncedSave = useCallback(() => {
    if (saveTimerRef.current) {
      clearTimeout(saveTimerRef.current);
    }
    saveTimerRef.current = setTimeout(() => {
      persistWorkspace();
      saveTimerRef.current = null;
    }, 500);
  }, [persistWorkspace]);

  // Auto-save: listen for content changes on all models
  useEffect(() => {
    if (!monaco) return;

    const disposables: { dispose(): void }[] = [];

    for (const model of monaco.editor.getModels()) {
      disposables.push(model.onDidChangeContent(() => debouncedSave()));
    }

    const modelCreated = monaco.editor.onDidCreateModel((model) => {
      disposables.push(model.onDidChangeContent(() => debouncedSave()));
    });
    disposables.push(modelCreated);

    return () => {
      if (saveTimerRef.current) {
        clearTimeout(saveTimerRef.current);
      }
      for (const d of disposables) {
        d.dispose();
      }
    };
  }, [monaco, debouncedSave]);

  function sync() {
    if (!monaco) {
      return;
    }

    const models = monaco.editor.getModels();
    setFileNames(models.map((model) => model.uri));
  }

  function compile() {
    if (!monaco) {
      return;
    }

    const models = monaco.editor.getModels();
    const files = new Map(
      models.map((model) => [model.uri.path, model.getValue()]),
    );

    const preprocessor = new InMemoryPreprocessor(files, fileName.path);
    const result = preprocessor.compile();
    const program = result.program;
    const report = result.report;
    if ((program && report) || (!program && !report)) {
      throw Error("Invalid return value");
    }

    if (program) {
      setProgram(program);
    } else if (report) {
      try {
        const reportObject = reportSchema.parse(JSON.parse(report));
        for (const model of monaco.editor.getModels()) {
          const decorations = toMonacoDecoration(model, reportObject);
          console.log(decorations);
          model.deltaDecorations([], decorations);
        }
      } catch (e: unknown) {
        console.log((e as ZodError).toString());
        return;
      }
    }
  }

  function handleEditorWillMount(monaco: Monaco) {
    // Destroy all existing models
    for (const model of monaco.editor.getModels()) {
      model.dispose();
    }

    for (const [path, content] of initialFiles) {
      monaco.editor.createModel(
        content,
        "z33",
        monaco.Uri.file(path.replace(/^.*[\\/]/, "")),
      );
    }
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

    const defaultFile = Uri.file("fact.S");
    setFileName(defaultFile);
    saveActiveFile(defaultFile.path);
    persistWorkspace();
    sync();
  }

  function switchFile(name: Uri) {
    setFileName(name);
    saveActiveFile(name.path);
  }

  return (
    <main className="flex h-screen bg-background">
      <div className="flex flex-col w-64">
        <div className="flex items-center justify-between px-2 py-1">
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
                  sync();
                }}
              >
                <TrashIcon />
              </Button>
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
                sync();
                setIsCreatingFile(false);
              }}
              onCancel={() => setIsCreatingFile(false)}
            />
          )}
        </div>

        <div className="p-2">
          <Popover
            onOpenChange={(open) => (open ? compile() : setProgram(null))}
            open={!!program}
          >
            <PopoverTrigger
              render={
                <Button type="button" className="w-full">
                  Compile
                </Button>
              }
            />
            <PopoverContent>
              {program && (
                <EntrypointSelector
                  entrypoints={program.labels}
                  onRun={(entrypoint) =>
                    onComputer?.(program?.compile(entrypoint))
                  }
                />
              )}
            </PopoverContent>
          </Popover>
        </div>
      </div>

      <div className="flex-1">
        <Editor
          className="editor"
          theme={theme.effective === "dark" ? "vs-dark" : "light"}
          path={fileName.path}
          beforeMount={handleEditorWillMount}
          onMount={() => sync()}
        />
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
              sync();
            };
            reader.readAsText(file);
          }
          e.target.value = "";
        }}
      />
    </main>
  );
};
