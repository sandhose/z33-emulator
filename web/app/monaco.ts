import EditorWorker from "web-worker:monaco-editor/esm/vs/editor/editor.worker.js";
import "monaco-editor/esm/vs/editor/browser/coreCommands.js";
import "monaco-editor/esm/vs/editor/browser/widget/codeEditorWidget.js";
export * from "monaco-editor/esm/vs/editor/editor.api.js";

declare global {
  interface Window {
    MonacoEnvironment: monaco.Environment;
  }
}

self.MonacoEnvironment = {
  getWorker(_moduleId: string, label: string): Worker {
    return new EditorWorker();
  },
};
