import { beforeEach, describe, expect, it } from "vitest";
import { rehydrateStore } from "../testing/rehydrate-store";
import { useFileStore } from "./file-store";
import {
  WORKSPACE_PERSIST_VERSION,
  WORKSPACE_STORAGE_KEY,
} from "./persist-keys";

const defaults = {
  files: useFileStore.getState().files,
  activeFile: useFileStore.getState().activeFile,
  entrypoints: useFileStore.getState().entrypoints,
};

const rehydrateFrom = (state: unknown) =>
  rehydrateStore(
    useFileStore,
    WORKSPACE_STORAGE_KEY,
    state,
    WORKSPACE_PERSIST_VERSION,
  );

beforeEach(() => {
  useFileStore.setState(defaults);
});

describe("defaults", () => {
  it("starts on the bundled samples", () => {
    expect(Object.keys(defaults.files)).toContain("fact.s");
    expect(defaults.activeFile).toBe("fact.s");
    expect(defaults.entrypoints).toEqual({});
  });
});

describe("rehydration", () => {
  it("loads a well-formed workspace", async () => {
    await rehydrateFrom({
      files: { "a.s": "nop", "b.s": "" },
      activeFile: "b.s",
      entrypoints: { "a.s": "main" },
    });
    const state = useFileStore.getState();
    expect(state.files).toEqual({ "a.s": "nop", "b.s": "" });
    expect(state.activeFile).toBe("b.s");
    expect(state.entrypoints).toEqual({ "a.s": "main" });
  });

  it.each([null, "a.s", 42, ["a.s"], { "a.s": 3 }])(
    "keeps the sample files when the persisted map is %o",
    async (files) => {
      await rehydrateFrom({ files, activeFile: "a.s", entrypoints: {} });
      expect(useFileStore.getState().files).toEqual(defaults.files);
    },
  );

  it("falls back to a file that exists when the persisted name is not a string", async () => {
    await rehydrateFrom({ files: { "a.s": "nop", "b.s": "" }, activeFile: 3 });
    expect(useFileStore.getState().activeFile).toBe("a.s");
  });

  it("falls back to a file that exists when the persisted name is not among them", async () => {
    await rehydrateFrom({ files: { "a.s": "nop" }, activeFile: "gone.s" });
    expect(useFileStore.getState().activeFile).toBe("a.s");
  });

  // The mirror: an active file that only made sense beside the files it was
  // saved with cannot stand once those fall back to the samples.
  it("reconciles the active file when only the file map fell back", async () => {
    await rehydrateFrom({ files: 42, activeFile: "gone.s" });
    const state = useFileStore.getState();
    expect(state.files).toEqual(defaults.files);
    expect(state.activeFile).toBe(defaults.activeFile);
  });

  it("keeps empty entrypoints when the persisted ones are malformed", async () => {
    await rehydrateFrom({
      files: { "a.s": "nop" },
      activeFile: "a.s",
      entrypoints: ["main"],
    });
    expect(useFileStore.getState().entrypoints).toEqual({});
  });

  it("falls back entirely on a payload that is not an object", async () => {
    await rehydrateFrom("nonsense");
    const state = useFileStore.getState();
    expect(state.files).toEqual(defaults.files);
    expect(state.activeFile).toBe(defaults.activeFile);
  });

  it("keeps an empty workspace, which is reachable by deleting every file", async () => {
    await rehydrateFrom({ files: {}, activeFile: "", entrypoints: {} });
    const state = useFileStore.getState();
    expect(state.files).toEqual({});
    expect(state.activeFile).toBe("");
  });
});

describe("createFile", () => {
  it("adds a file and makes it active", () => {
    useFileStore.getState().createFile("new.s", "nop");
    const state = useFileStore.getState();
    expect(state.files["new.s"]).toBe("nop");
    expect(state.activeFile).toBe("new.s");
  });

  it("defaults the content to empty", () => {
    useFileStore.getState().createFile("new.s");
    expect(useFileStore.getState().files["new.s"]).toBe("");
  });

  it("does not overwrite an existing file, but does switch to it", () => {
    useFileStore.getState().createFile("keep.s", "original");
    useFileStore.getState().setActiveFile("fact.s");
    useFileStore.getState().createFile("keep.s", "replacement");
    const state = useFileStore.getState();
    expect(state.files["keep.s"]).toBe("original");
    expect(state.activeFile).toBe("keep.s");
  });
});

describe("deleteFile", () => {
  it("removes the file and leaves the active one alone", () => {
    useFileStore.getState().createFile("gone.s", "nop");
    useFileStore.getState().setActiveFile("fact.s");
    useFileStore.getState().deleteFile("gone.s");
    const state = useFileStore.getState();
    expect("gone.s" in state.files).toBe(false);
    expect(state.activeFile).toBe("fact.s");
  });

  it("moves to another file when the active one goes", () => {
    useFileStore.getState().createFile("gone.s", "nop");
    useFileStore.getState().deleteFile("gone.s");
    const state = useFileStore.getState();
    expect(state.activeFile).not.toBe("gone.s");
    expect(Object.keys(state.files)).toContain(state.activeFile);
  });

  it("leaves no active file once the last one goes", () => {
    useFileStore.setState({ files: { "only.s": "nop" }, activeFile: "only.s" });
    useFileStore.getState().deleteFile("only.s");
    const state = useFileStore.getState();
    expect(state.files).toEqual({});
    expect(state.activeFile).toBe("");
  });

  it("ignores a file that is not there", () => {
    useFileStore.getState().deleteFile("absent.s");
    expect(useFileStore.getState().files).toEqual(defaults.files);
  });
});

describe("content updates", () => {
  it("writes content from Monaco and from the outside alike", () => {
    useFileStore.getState().onMonacoEdit("fact.s", "typed");
    expect(useFileStore.getState().files["fact.s"]).toBe("typed");
    useFileStore.getState().setContent("fact.s", "uploaded");
    expect(useFileStore.getState().files["fact.s"]).toBe("uploaded");
  });

  it("resets to the samples, dropping files created since", () => {
    useFileStore.getState().createFile("scratch.s", "nop");
    useFileStore.getState().onMonacoEdit("fact.s", "clobbered");
    useFileStore.getState().resetFiles();
    const state = useFileStore.getState();
    expect(state.files).toEqual(defaults.files);
    expect(state.activeFile).toBe(defaults.activeFile);
  });
});

describe("setEntrypoint", () => {
  it("records one entrypoint per file", () => {
    useFileStore.getState().setEntrypoint("fact.s", "main");
    useFileStore.getState().setEntrypoint("echo.s", "start");
    useFileStore.getState().setEntrypoint("fact.s", "other");
    expect(useFileStore.getState().entrypoints).toEqual({
      "fact.s": "other",
      "echo.s": "start",
    });
  });
});
