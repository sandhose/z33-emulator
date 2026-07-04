// Init shim for the wasm-pack (`--target web`) bindings.
//
// `--target web` requires calling the module's default `init()` export once,
// before any exported class or function is used. We do that here with a
// top-level await (Vite targets `esnext`, which supports TLA) and re-export the
// whole generated module, so app code imports the initialized bindings from
// this module (`./lib/wasm`) instead of touching the generated `pkg/` directly.
//
// The `?url` import lets Vite fingerprint and serve the wasm binary as a static
// asset (and is the same pattern the workers use).
import init from "../../pkg/z33_web.js";
import wasmUrl from "../../pkg/z33_web_bg.wasm?url";

try {
  await init({ module_or_path: wasmUrl });
} catch (error) {
  // The whole app depends on these bindings; if the wasm module fails to fetch
  // or instantiate, React never mounts and the user is left with a blank page.
  // Render a minimal, visible error with plain DOM (no framework available at
  // this point), then rethrow so the original cause still surfaces in the
  // console.
  const message = error instanceof Error ? error.message : String(error);
  const banner = document.createElement("div");
  banner.setAttribute("role", "alert");
  banner.style.cssText =
    "position:fixed;inset:0;z-index:2147483647;display:flex;flex-direction:column;" +
    "align-items:center;justify-content:center;gap:0.5rem;padding:2rem;" +
    "font-family:system-ui,sans-serif;text-align:center;background:#fff;color:#111";
  const title = document.createElement("h1");
  title.textContent = "Failed to load the Z33 emulator";
  title.style.cssText = "margin:0;font-size:1.25rem";
  const detail = document.createElement("p");
  detail.textContent =
    "The WebAssembly module could not be loaded. Try reloading the page.";
  detail.style.cssText = "margin:0;max-width:40rem";
  const cause = document.createElement("pre");
  cause.textContent = message;
  cause.style.cssText =
    "margin:0;max-width:40rem;overflow:auto;font-size:0.8rem;color:#b00";
  banner.append(title, detail, cause);
  document.body.append(banner);
  throw error;
}

export * from "../../pkg/z33_web.js";
