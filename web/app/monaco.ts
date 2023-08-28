import EditorWorker from "monaco-editor/esm/vs/editor/editor.worker.js?worker";
import "monaco-editor/esm/vs/editor/browser/coreCommands.js";
import "monaco-editor/esm/vs/editor/browser/widget/codeEditorWidget.js";
export * from "monaco-editor/esm/vs/editor/editor.api.js";

self.MonacoEnvironment = {
  getWorker(_moduleId: string, label: string): Worker {
    return new EditorWorker();
  },
};
