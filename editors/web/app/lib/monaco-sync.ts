import type { Monaco } from "@monaco-editor/react";
import type { editor as MonacoEditor } from "monaco-editor";
import { stripLeadingSlash } from "./file-paths";

type MonacoSyncCallbacks = {
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

/** The debounced edits still owed to the store, one entry per model. */
const pendingEdits = new Set<() => void>();

/**
 * Write every debounced Monaco edit to the store now. The store trails Monaco
 * by the debounce below, so anything that reads it as the program to run calls
 * this first. It is a no-op until the editor chunk has attached its models,
 * and this module only type-imports Monaco, so a caller on the main thread
 * does not pull the editor in.
 */
export function flushMonacoSync(): void {
  // Taken and emptied first: each flush writes to the store, which syncs back
  // into Monaco and can add to this set again.
  const owed = [...pendingEdits];
  pendingEdits.clear();
  for (const flush of owed) flush();
}

function attachContentListener(
  name: string,
  model: MonacoEditor.ITextModel,
  onEdit: (name: string, content: string) => void,
): { dispose(): void } {
  let timer: ReturnType<typeof setTimeout> | null = null;
  const flush = () => {
    if (timer === null) return;
    clearTimeout(timer);
    timer = null;
    pendingEdits.delete(flush);
    // A model disposed with an edit still owed belongs to a file that was
    // deleted; writing it back would resurrect the file.
    if (model.isDisposed()) return;
    onEdit(name, model.getValue());
  };
  const listener = model.onDidChangeContent(() => {
    if (timer !== null) clearTimeout(timer);
    pendingEdits.add(flush);
    timer = setTimeout(flush, 500);
  });
  return {
    dispose() {
      // The editor unmounts on every switch into debug mode, which is one
      // click after a keystroke: an edit still owed here is lost otherwise.
      flush();
      listener.dispose();
    },
  };
}

export function initMonacoSync(
  monaco: Monaco,
  callbacks: MonacoSyncCallbacks,
): () => void {
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
  };
}
