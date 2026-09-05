import { create } from "zustand";
import { createJSONStorage, persist } from "zustand/middleware";
import {
  WORKSPACE_PERSIST_VERSION,
  WORKSPACE_STORAGE_KEY,
} from "./persist-keys";
import { persistedField, stringRecord } from "./persisted";

const sampleFiles = Object.fromEntries(
  Object.entries(
    import.meta.glob<string>("../../../../samples/*.s", {
      query: "?raw",
      import: "default",
      eager: true,
    }),
  ).map(([path, content]) => [path.replace(/^.*[\\/]/u, ""), content]),
);

const initial = { files: sampleFiles, activeFile: "fact.s" };

/**
 * The file the editor opens on. It has to be one of the files beside it, or
 * the editor opens on a name nothing answers to; only an empty workspace has
 * no active file. Candidates are tried in order of preference.
 */
function reconcileActiveFile(
  files: Record<string, string>,
  ...candidates: unknown[]
): string {
  for (const name of candidates) {
    if (typeof name === "string" && Object.hasOwn(files, name)) return name;
  }
  return Object.keys(files)[0] ?? "";
}

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
  onMonacoEdit: (name: string, content: string) => void;
  /** Called for external operations (upload, reset) — Monaco sync follows via subscription */
  setContent: (name: string, content: string) => void;
  resetFiles: () => void;
  setEntrypoint: (file: string, entrypoint: string) => void;
}

export const useFileStore = create<FileState & FileActions>()(
  persist(
    (set) => ({
      files: initial.files,
      activeFile: initial.activeFile,
      entrypoints: {},

      setActiveFile: (name) => {
        set({ activeFile: name });
      },

      createFile: (name, content = "") => {
        set((state) => ({
          files:
            name in state.files
              ? state.files
              : { ...state.files, [name]: content },
          activeFile: name,
        }));
      },

      deleteFile: (name) => {
        set((state) => {
          const { [name]: _removed, ...rest } = state.files;
          const activeFile =
            state.activeFile === name
              ? (Object.keys(rest)[0] ?? "")
              : state.activeFile;
          return { files: rest, activeFile };
        });
      },

      onMonacoEdit: (name, content) => {
        set((state) => ({
          files: { ...state.files, [name]: content },
        }));
      },

      setContent: (name, content) => {
        set((state) => ({
          files: { ...state.files, [name]: content },
        }));
      },

      resetFiles: () => {
        set({ files: initial.files, activeFile: initial.activeFile });
      },

      setEntrypoint: (file, entrypoint) => {
        set((state) => ({
          entrypoints: { ...state.entrypoints, [file]: entrypoint },
        }));
      },
    }),
    {
      name: WORKSPACE_STORAGE_KEY,
      version: WORKSPACE_PERSIST_VERSION,
      storage: createJSONStorage(() => localStorage),
      partialize: (state) => ({
        files: state.files,
        activeFile: state.activeFile,
        entrypoints: state.entrypoints,
      }),
      merge: (persisted, current) => {
        const files =
          stringRecord(persistedField(persisted, "files")) ?? current.files;
        return {
          ...current,
          files,
          activeFile: reconcileActiveFile(
            files,
            persistedField(persisted, "activeFile"),
            current.activeFile,
          ),
          entrypoints:
            stringRecord(persistedField(persisted, "entrypoints")) ??
            current.entrypoints,
        };
      },
    },
  ),
);
