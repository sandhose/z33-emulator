import { Editor } from "@monaco-editor/react";
import type * as monaco from "monaco-editor";
import { useTheme } from "./components/theme-provider";
import { toMonacoPath } from "./lib/file-paths";

type Props = {
  filePath: string;
  readOnly?: boolean;
  onEditorMount?: (editor: monaco.editor.IStandaloneCodeEditor) => void;
};

export const MultiFileEditor: React.FC<Props> = ({
  filePath,
  readOnly = false,
  onEditorMount,
}: Props) => {
  const theme = useTheme();

  return (
    <Editor
      className="editor h-full"
      theme={theme.effective === "dark" ? "vs-dark" : "light"}
      path={toMonacoPath(filePath)}
      keepCurrentModel
      onMount={(editor) => {
        onEditorMount?.(editor);
      }}
      options={{ readOnly, glyphMargin: true }}
    />
  );
};
