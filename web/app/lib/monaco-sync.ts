import type { Monaco } from "@monaco-editor/react";
import type { editor as MonacoEditor } from "monaco-editor";
import { useFileStore } from "../stores/file-store";

let monacoRef: Monaco | null = null;

function attachContentListener(
  name: string,
  model: MonacoEditor.ITextModel,
): { dispose(): void } {
  let timer: ReturnType<typeof setTimeout> | null = null;
  const listener = model.onDidChangeContent(() => {
    if (timer) clearTimeout(timer);
    timer = setTimeout(() => {
      timer = null;
      useFileStore.getState()._onMonacoEdit(name, model.getValue());
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

export function initMonacoSync(monaco: Monaco): () => void {
  monacoRef = monaco;

  const disposables: { dispose(): void }[] = [];

  // Create models for all files if none exist yet
  if (monaco.editor.getModels().length === 0) {
    const { files } = useFileStore.getState();
    for (const [name, content] of Object.entries(files)) {
      monaco.editor.createModel(content, "z33", monaco.Uri.file(name));
    }
  }

  // Attach content listeners to all existing models
  for (const model of monaco.editor.getModels()) {
    const name = model.uri.path.replace(/^\//, "");
    disposables.push(attachContentListener(name, model));
  }

  // Attach content listeners to future models
  disposables.push(
    monaco.editor.onDidCreateModel((newModel: MonacoEditor.ITextModel) => {
      const name = newModel.uri.path.replace(/^\//, "");
      disposables.push(attachContentListener(name, newModel));
    }),
  );

  // Subscribe to store changes and sync to Monaco
  const unsubscribe = useFileStore.subscribe((state, prev) => {
    for (const [name, content] of Object.entries(state.files)) {
      if (prev.files[name] === content) continue; // no change
      const uri = monaco.Uri.file(name);
      const model = monaco.editor.getModel(uri);
      if (model) {
        // Only call setValue when Monaco doesn't already have this content.
        // This is the circular-sync guard: if _onMonacoEdit triggered this
        // subscription, Monaco already has the content and we skip.
        if (model.getValue() !== content) model.setValue(content);
      } else {
        const newModel = monaco.editor.createModel(content, "z33", uri);
        disposables.push(attachContentListener(name, newModel));
      }
    }

    // Dispose models for deleted files
    for (const name of Object.keys(prev.files)) {
      if (!(name in state.files)) {
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
