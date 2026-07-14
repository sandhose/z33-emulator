import { useMonaco } from "@monaco-editor/react";
import type * as monaco from "monaco-editor";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { AppShortcuts } from "./app-shortcuts";
import { DebugLayout } from "./debug-layout";
import { EditToolbar, pickEntrypoint } from "./edit-toolbar";
import { FileSidebar } from "./file-sidebar";
import { useCompilation } from "./hooks/use-compilation";
import type { ShortcutActions } from "./hooks/use-app-shortcuts";
import { ShortcutsHelpDialog } from "./keyboard-shortcuts";
import { stripLeadingSlash } from "./lib/file-paths";
import { setRunCommandHandler } from "./lib/lsp-monaco";
import { getMonacoFiles } from "./lib/monaco-sync";
import { MultiFileEditor } from "./multi-file-editor";
import { useAppStore } from "./stores/app-store";
import { useFileStore } from "./stores/file-store";

/**
 * Move focus to a landmark region by its accessible name (from the a11y pass).
 * Regions aren't tabbable by default, so give them a programmatic-only
 * tabindex the first time they're focused.
 */
function focusLandmark(label: string): void {
  const el = document.querySelector<HTMLElement>(`[aria-label="${label}"]`);
  if (!el) return;
  if (!el.hasAttribute("tabindex")) el.setAttribute("tabindex", "-1");
  el.focus();
}

const App = () => {
  const monacoInstance = useMonaco();
  const editorRef = useRef<monaco.editor.IStandaloneCodeEditor | null>(null);

  const activeFile = useFileStore((s) => s.activeFile);
  const startDebug = useAppStore((s) => s.startDebug);
  const mode = useAppStore((s) => s.mode);
  const setEntrypoint = useFileStore((s) => s.setEntrypoint);
  const entrypoints = useFileStore((s) => s.entrypoints);

  const [helpOpen, setHelpOpen] = useState(false);

  const { compilationResult, compilationStatus } = useCompilation(
    activeFile,
    monacoInstance,
  );

  const runFile = useCallback(
    (file: string, entrypoint: string) => {
      setEntrypoint(file, entrypoint);
      // Use the freshest editor contents, keyed by file-store name (no leading
      // slash) to match the emulator worker / LSP URI conventions.
      const files = Object.fromEntries(
        Object.entries(getMonacoFiles()).map(([path, content]) => [
          stripLeadingSlash(path),
          content,
        ]),
      );
      void startDebug(files, file, entrypoint).catch((e: unknown) => {
        // The compile check should have caught errors before Run; this is a
        // safety net.
        console.error("Failed to start debug session:", e);
      });
    },
    [setEntrypoint, startDebug],
  );

  const handleRun = useCallback(
    (entrypoint: string) => {
      if (compilationResult.type !== "success") return;
      runFile(activeFile, entrypoint);
    },
    [compilationResult, activeFile, runFile],
  );

  // The LSP run code lens: start (or restart) a debug session with the lens's
  // file as the program and its label as the entrypoint. The lens path is the
  // server's workspace-relative path — the same no-leading-slash convention as
  // the file store.
  useEffect(() => {
    setRunCommandHandler(({ path, label }) => {
      const { mode: session, stopDebug } = useAppStore.getState();
      if (session.type === "debug") stopDebug();
      runFile(path, label);
    });
    return () => setRunCommandHandler(null);
  }, [runFile]);

  // --- shortcut handlers -----------------------------------------------------

  // Run at the default entrypoint (the same choice the Run button preselects).
  const runDefault = useCallback(() => {
    if (compilationResult.type !== "success") return;
    const entrypoint = pickEntrypoint(
      compilationResult.labels,
      entrypoints[activeFile],
    );
    if (entrypoint) handleRun(entrypoint);
  }, [compilationResult, entrypoints, activeFile, handleRun]);
  // Monaco commands are registered once at editor mount, so read the latest
  // run handler through a ref to avoid a stale closure across recompiles.
  const runDefaultRef = useRef(runDefault);
  runDefaultRef.current = runDefault;

  // Execution controls read the live session from the store, so they stay
  // stable across renders and mirror the debug toolbar's guards.
  const step = useCallback(() => {
    const { mode: session } = useAppStore.getState();
    if (session.type !== "debug") return;
    if (session.computer.getStatus() !== "paused") return;
    session.computer.step(1);
  }, []);

  const runPause = useCallback(() => {
    const { mode: session } = useAppStore.getState();
    if (session.type !== "debug") return;
    const status = session.computer.getStatus();
    if (status === "running") session.computer.pause();
    else if (status === "paused") session.computer.run();
  }, []);

  const stop = useCallback(() => {
    const { mode: session, stopDebug } = useAppStore.getState();
    if (session.type === "debug") stopDebug();
  }, []);

  const focusSecondary = useCallback(() => {
    const { mode: session } = useAppStore.getState();
    focusLandmark(session.type === "debug" ? "Registers" : "Files");
  }, []);

  const focusEditor = useCallback(() => {
    editorRef.current?.focus();
  }, []);
  const focusMemory = useCallback(() => {
    focusLandmark("Memory");
  }, []);
  const focusSerial = useCallback(() => {
    focusLandmark("Serial console");
  }, []);
  const openHelp = useCallback(() => {
    setHelpOpen(true);
  }, []);

  // Stable handler bundle: only `runDefault` changes (per recompile), so the
  // memoized <AppShortcuts> leaf stays put while the help dialog is open.
  const actions = useMemo<ShortcutActions>(
    () => ({
      run: runDefault,
      stop,
      step,
      runPause,
      focusEditor,
      focusSecondary,
      focusMemory,
      focusSerial,
      help: openHelp,
    }),
    [
      runDefault,
      stop,
      step,
      runPause,
      focusEditor,
      focusSecondary,
      focusMemory,
      focusSerial,
      openHelp,
    ],
  );

  const handleEditorMount = useCallback(
    (editor: monaco.editor.IStandaloneCodeEditor) => {
      editorRef.current = editor;
      if (!monacoInstance) return;
      const { KeyMod, KeyCode } = monacoInstance;
      // Mirror the shortcuts Monaco would otherwise swallow while it owns
      // keydown. Bind execution keys only in the (read-only) debug editor so
      // they don't shadow Monaco's own F8 while editing; bind Run only while
      // editing (debug Mod+Enter is a no-op by design).
      if (useAppStore.getState().mode.type === "debug") {
        editor.addCommand(KeyCode.F8, () => {
          step();
        });
        editor.addCommand(KeyCode.F9, () => {
          runPause();
        });
        editor.addCommand(KeyMod.CtrlCmd | KeyMod.Shift | KeyCode.Enter, () => {
          stop();
        });
      } else {
        editor.addCommand(KeyMod.CtrlCmd | KeyCode.Enter, () => {
          runDefaultRef.current();
        });
      }
    },
    [monacoInstance, step, runPause, stop],
  );

  const isDebugging = mode.type === "debug";

  return (
    <div className="flex flex-col h-screen bg-background">
      <AppShortcuts mode={mode.type} actions={actions} />
      {!isDebugging && (
        <header>
          <EditToolbar
            onRun={handleRun}
            compilationStatus={compilationStatus}
            compilationError={
              compilationResult.type === "error"
                ? compilationResult.message
                : undefined
            }
            labels={
              compilationResult.type === "idle" ? [] : compilationResult.labels
            }
            defaultEntrypoint={entrypoints[activeFile]}
          />
        </header>
      )}
      <div className="flex flex-1 min-h-0">
        <FileSidebar />
        <main
          className="flex-1 min-w-0 flex flex-col min-h-0"
          aria-label="Program editor"
        >
          {isDebugging ? (
            <DebugLayout onEditorMount={handleEditorMount} />
          ) : (
            <div className="flex-1 min-h-0">
              <MultiFileEditor
                filePath={activeFile}
                onEditorMount={handleEditorMount}
              />
            </div>
          )}
        </main>
      </div>
      <ShortcutsHelpDialog open={helpOpen} onOpenChange={setHelpOpen} />
    </div>
  );
};

export default App;
