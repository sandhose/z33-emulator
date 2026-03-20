import type { Monaco } from "@monaco-editor/react";
import type { editor as MonacoEditor } from "monaco-editor";
import { stripLeadingSlash } from "./file-paths";

// oxlint-disable-next-line typescript/no-redundant-type-constituents -- Monaco is not `any`, false positive
let monacoRef: Monaco | null = null;

export type MonacoSyncCallbacks = {
  /** Called when Monaco model content changes (debounced). */
  onEdit: (name: string, content: string) => void;
  /** Returns the current file map (name → content). */
  getFiles: () => Record<string, string>;
  /** Subscribe to external file changes. Returns unsubscribe function. */
  subscribe: (
    listener: (
      files: Record<string, string>,
      prevFiles: Record<string, string>,
    ) => void,
  ) => () => void;
};

function attachContentListener(
  name: string,
  model: MonacoEditor.ITextModel,
  onEdit: (name: string, content: string) => void,
): { dispose(): void } {
  let timer: ReturnType<typeof setTimeout> | null = null;
  const listener = model.onDidChangeContent(() => {
    if (timer) clearTimeout(timer);
    timer = setTimeout(() => {
      timer = null;
      onEdit(name, model.getValue());
    }, 500);
  });
  return {
    dispose() {
      if (timer) clearTimeout(timer);
      timer = null;
      listener.dispose();
    },
  };
}

export function initMonacoSync(
  monaco: Monaco,
  callbacks: MonacoSyncCallbacks,
): () => void {
  monacoRef = monaco;

  const disposables: { dispose(): void }[] = [];

  // Create models for all files if none exist yet
  if (monaco.editor.getModels().length === 0) {
    const files = callbacks.getFiles();
    for (const [name, content] of Object.entries(files)) {
      monaco.editor.createModel(content, "z33", monaco.Uri.file(name));
    }
  }

  // Attach content listeners to all existing models
  for (const model of monaco.editor.getModels()) {
    const name = stripLeadingSlash(model.uri.path);
    disposables.push(attachContentListener(name, model, callbacks.onEdit));
  }

  // Attach content listeners to future models
  disposables.push(
    monaco.editor.onDidCreateModel((newModel: MonacoEditor.ITextModel) => {
      const name = stripLeadingSlash(newModel.uri.path);
      disposables.push(attachContentListener(name, newModel, callbacks.onEdit));
    }),
  );

  // Subscribe to store changes and sync to Monaco
  const unsubscribe = callbacks.subscribe((files, prevFiles) => {
    for (const [name, content] of Object.entries(files)) {
      if (prevFiles[name] === content) continue; // no change
      const uri = monaco.Uri.file(name);
      const model = monaco.editor.getModel(uri);
      if (model) {
        // Only call setValue when Monaco doesn't already have this content.
        // This is the circular-sync guard: if onEdit triggered this
        // subscription, Monaco already has the content and we skip.
        if (model.getValue() !== content) model.setValue(content);
      } else {
        const newModel = monaco.editor.createModel(content, "z33", uri);
        disposables.push(
          attachContentListener(name, newModel, callbacks.onEdit),
        );
      }
    }

    // Dispose models for deleted files
    for (const name of Object.keys(prevFiles)) {
      if (!(name in files)) {
        monaco.editor.getModel(monaco.Uri.file(name))?.dispose();
      }
    }
  });

  disposables.push({ dispose: unsubscribe });

  return () => {
    for (const d of disposables) d.dispose();
    if (monacoRef === monaco) monacoRef = null;
  };
}

/** Returns all current Monaco model contents, keyed by URI path (with leading slash). */
export function getMonacoFiles(): Record<string, string> {
  if (!monacoRef) return {};
  const files: Record<string, string> = {};
  for (const model of monacoRef.editor.getModels()) {
    files[model.uri.path] = model.getValue();
  }
  return files;
}
