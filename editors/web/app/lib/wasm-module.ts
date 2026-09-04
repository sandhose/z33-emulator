// One fetch and compile of the wasm binary per page. The main thread and both
// workers instantiate from the same `WebAssembly.Module` (structured-cloneable,
// so it crosses `postMessage`), instead of each downloading and compiling the
// binary on its own.
//
// Nothing calls this while the page is loading: the first caller is whichever
// worker client is constructed once Monaco is up, so the 1.8 MB binary and the
// 3.2 MB editor chunk never share a slow link.
import wasmUrl from "../../pkg/z33_web_bg.wasm?url";

let compiled: Promise<WebAssembly.Module> | null = null;

/** The MIME essence: type/subtype, lowercased, parameters stripped. */
function mimeEssence(response: Response): string {
  return (
    (response.headers.get("content-type") ?? "")
      .split(";")[0]
      ?.trim()
      .toLowerCase() ?? ""
  );
}

async function fetchAndCompile(): Promise<WebAssembly.Module> {
  // A fetch that never completes (captive portal, dead proxy) would leave the
  // toolbar loading for good; five minutes is 6 KB/s for this binary, below
  // any link the app is usable on.
  const response = await fetch(wasmUrl, {
    signal: AbortSignal.timeout(5 * 60 * 1000),
  });
  if (!response.ok) {
    throw new Error(
      `Failed to fetch the emulator binary: ${response.status} ${response.statusText}`,
    );
  }
  // Static hosts often serve the binary as octet-stream, which
  // compileStreaming refuses, so those bytes are buffered the way the wasm-pack
  // loader does. Choosing on the MIME essence up front rather than from a
  // rejection keeps every other failure — an aborted download, a truncated
  // binary — as itself.
  if (mimeEssence(response) === "application/wasm") {
    return WebAssembly.compileStreaming(response);
  }
  return WebAssembly.compile(await response.arrayBuffer());
}

export function compiledWasmModule(): Promise<WebAssembly.Module> {
  compiled ??= fetchAndCompile();
  return compiled;
}
