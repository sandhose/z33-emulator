import { create } from "zustand";
import { createJSONStorage, persist } from "zustand/middleware";
import { loadActiveFile, loadWorkspace } from "../file-store";

const WORKSPACE_V2_KEY = "z33:workspace-v2";

const sampleFiles = Object.fromEntries(
  Object.entries(
    import.meta.glob<string>("../../../samples/*.S", {
      query: "?raw",
      import: "default",
      eager: true,
    }),
  ).map(([path, content]) => [path.replace(/^.*[\\/]/, ""), content]),
);

function computeInitialState(): {
  files: Record<string, string>;
  activeFile: string;
} {
  // If v2 key already exists, persist middleware will override this
  const v2Raw = localStorage.getItem(WORKSPACE_V2_KEY);
  if (v2Raw) {
    return { files: {}, activeFile: "" };
  }

  // Migrate from old storage format (paths had leading slashes)
  const oldWorkspace = loadWorkspace();
  if (oldWorkspace && oldWorkspace.size > 0) {
    const files = Object.fromEntries(
      Array.from(oldWorkspace.entries()).map(([k, v]) => [
        k.replace(/^\//, ""),
        v,
      ]),
    );
    const rawActive =
      loadActiveFile() ?? oldWorkspace.keys().next().value ?? "";
    const activeFile = rawActive.replace(/^\//, "");
    return { files, activeFile };
  }

  // Fall back to sample files
  return { files: sampleFiles, activeFile: "fact.S" };
}

const initial = computeInitialState();

interface FileState {
  files: Record<string, string>; // filename (no leading slash) → content
  activeFile: string;
  entrypoints: Record<string, string>; // filename → last confirmed function entrypoint
}

interface FileActions {
  setActiveFile: (name: string) => void;
  createFile: (name: string, content?: string) => void;
  deleteFile: (name: string) => void;
  /** Called from Monaco content-change listener — updates store only, no Monaco sync needed */
  _onMonacoEdit: (name: string, content: string) => void;
  /** Called for external operations (upload, reset) — Monaco sync follows via subscription */
  setContent: (name: string, content: string) => void;
  resetFiles: (files: Record<string, string>, activeFile: string) => void;
  setEntrypoint: (file: string, entrypoint: string) => void;
}

export const useFileStore = create<FileState & FileActions>()(
  persist(
    (set) => ({
      files: initial.files,
      activeFile: initial.activeFile,
      entrypoints: {},

      setActiveFile: (name) => set({ activeFile: name }),

      createFile: (name, content = "") =>
        set((state) => ({
          files: { ...state.files, [name]: content },
          activeFile: name,
        })),

      deleteFile: (name) =>
        set((state) => {
          const { [name]: _removed, ...rest } = state.files;
          const activeFile =
            state.activeFile === name
              ? (Object.keys(rest)[0] ?? "")
              : state.activeFile;
          return { files: rest, activeFile };
        }),

      _onMonacoEdit: (name, content) =>
        set((state) => ({
          files: { ...state.files, [name]: content },
        })),

      setContent: (name, content) =>
        set((state) => ({
          files: { ...state.files, [name]: content },
        })),

      resetFiles: (files, activeFile) => set({ files, activeFile }),

      setEntrypoint: (file, entrypoint) =>
        set((state) => ({
          entrypoints: { ...state.entrypoints, [file]: entrypoint },
        })),
    }),
    {
      name: WORKSPACE_V2_KEY,
      storage: createJSONStorage(() => localStorage),
      partialize: (state) => ({
        files: state.files,
        activeFile: state.activeFile,
        entrypoints: state.entrypoints,
      }),
    },
  ),
);
