import { lazy, Suspense } from "react";
import { EditorSkeleton } from "./editor-skeleton";
import type { EditorProps } from "./monaco-file-editor";

const MonacoFileEditor = lazy(() => import("./monaco-file-editor"));

/**
 * The source editor. Monaco is a third of the app's bytes, so it is fetched
 * only once the shell has rendered; until it arrives the skeleton holds the
 * editor's place.
 */
export const MultiFileEditor: React.FC<EditorProps> = (props: EditorProps) => (
  <Suspense fallback={<EditorSkeleton />}>
    <MonacoFileEditor {...props} />
  </Suspense>
);
