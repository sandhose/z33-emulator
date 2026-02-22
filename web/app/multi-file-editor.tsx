import { Editor, type Monaco } from "@monaco-editor/react";
import type * as monaco from "monaco-editor";
import type { Uri } from "monaco-editor/esm/vs/editor/editor.api.js";
import { useTheme } from "./components/theme-provider";

type Props = {
  initialFiles: Map<string, string>;
  fileName: Uri;
  readOnly?: boolean;
  onEditorMount?: (editor: monaco.editor.IStandaloneCodeEditor) => void;
};

export const MultiFileEditor: React.FC<Props> = ({
  initialFiles,
  fileName,
  readOnly = false,
  onEditorMount,
}: Props) => {
  const theme = useTheme();

  function handleEditorWillMount(m: Monaco) {
    // Only create models on initial load; skip if models already exist
    if (m.editor.getModels().length > 0) return;

    for (const [path, content] of initialFiles) {
      m.editor.createModel(
        content,
        "z33",
        m.Uri.file(path.replace(/^.*[\\/]/, "")),
      );
    }
  }

  return (
    <Editor
      className="editor h-full"
      theme={theme.effective === "dark" ? "vs-dark" : "light"}
      path={fileName.path}
      keepCurrentModel
      beforeMount={handleEditorWillMount}
      onMount={(editor) => {
        onEditorMount?.(editor);
      }}
      options={{ readOnly }}
    />
  );
};
