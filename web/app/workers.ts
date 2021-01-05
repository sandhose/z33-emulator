import EditorWorker from "web-worker:monaco-editor/esm/vs/editor/editor.worker.js";

export function getWorker(_moduleId: string, label: string): Worker {
  return new EditorWorker();
}
