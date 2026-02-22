import { useState } from "react";
import type { Computer } from "z33-web-bindings";
import { ComputerView } from "./computer";
import { loadActiveFile, loadWorkspace, saveWorkspace } from "./file-store";
import { MultiFileEditor } from "./multi-file-editor";

const sampleFiles = import.meta.glob<string>("../../samples/*.S", {
  query: "?raw",
  import: "default",
  eager: true,
});

function getSampleFiles(): Map<string, string> {
  return new Map(
    Object.entries(sampleFiles).map(([path, content]) => [
      path.replace(/^.*[\\/]/, ""),
      content,
    ]),
  );
}

function getInitialFiles(): { files: Map<string, string>; selected: string } {
  const stored = loadWorkspace();
  if (stored && stored.size > 0) {
    const active = loadActiveFile() ?? stored.keys().next().value ?? "";
    return { files: stored, selected: active };
  }

  const files = getSampleFiles();
  saveWorkspace(files);
  return { files, selected: "fact.S" };
}

const { files: initialFiles, selected: initialSelected } = getInitialFiles();

const App = () => {
  const [computer, setComputer] = useState<Computer | null>(null);

  return computer === null ? (
    <MultiFileEditor
      initialFiles={initialFiles}
      initialSelected={initialSelected}
      sampleFiles={getSampleFiles}
      onComputer={setComputer}
    />
  ) : (
    <ComputerView computer={computer} />
  );
};

export default App;
