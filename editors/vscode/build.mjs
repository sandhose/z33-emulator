// esbuild driver for the Z33 web extension.
//
// Produces two fully self-contained browser bundles:
//   * dist/extension.js         — the extension host entry (CJS, `vscode` external)
//   * dist/lsp-server.worker.js — the LSP web worker (classic IIFE worker)
//
// Both bundle the wasm-pack glue from `z33-editor-shared`. The `import.meta.url`
// fallback in that glue (used only to locate the .wasm by URL) is dead code
// here — we always instantiate from bytes — so we define it to a harmless
// literal to keep esbuild happy in CJS/IIFE output.
//
// The binary itself is read at run time from the extension directory, so it is
// copied into dist/ rather than bundled.

import { copyFile, mkdir } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import * as esbuild from "esbuild";

const production = process.argv.includes("--production");
const watch = process.argv.includes("--watch");

// wasm-pack rewrites the glue on every build, so a wasm rebuild is also an
// esbuild rebuild and this keeps the copy in dist/ in step under `--watch`. One
// config carries it: two would race for the same output file.
/** @type {import("esbuild").Plugin} */
const copyWasmPlugin = {
  name: "copy-wasm",
  setup(build) {
    build.onEnd(async () => {
      const source = fileURLToPath(import.meta.resolve("z33-editor-shared/wasm-binary"));
      await mkdir("dist", { recursive: true });
      await copyFile(source, "dist/z33_web_bg.wasm");
    });
  },
};

/** @type {import("esbuild").BuildOptions} */
const shared = {
  bundle: true,
  platform: "browser",
  target: "es2022",
  sourcemap: !production,
  minify: production,
  logLevel: "info",
  define: {
    "import.meta.url": '"file:///z33_web.js"',
  },
};

/** @type {import("esbuild").BuildOptions} */
const extensionConfig = {
  ...shared,
  entryPoints: ["src/extension.ts"],
  outfile: "dist/extension.js",
  format: "cjs",
  external: ["vscode"],
  plugins: [copyWasmPlugin],
};

/** @type {import("esbuild").BuildOptions} */
const workerConfig = {
  ...shared,
  entryPoints: ["src/lsp-server.worker.ts"],
  outfile: "dist/lsp-server.worker.js",
  // Classic (non-module) worker: everything must be inlined.
  format: "iife",
};

async function main() {
  if (watch) {
    const [extCtx, workerCtx] = await Promise.all([
      esbuild.context(extensionConfig),
      esbuild.context(workerConfig),
    ]);
    await Promise.all([extCtx.watch(), workerCtx.watch()]);
    console.log("watching…");
    return;
  }

  await Promise.all([
    esbuild.build(extensionConfig),
    esbuild.build(workerConfig),
  ]);
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
