// esbuild driver for the Z33 web extension.
//
// Produces two fully self-contained browser bundles:
//   * dist/extension.js         — the extension host entry (CJS, `vscode` external)
//   * dist/lsp-server.worker.js — the LSP web worker (classic IIFE worker)
//
// Both bundle the wasm-pack glue (dist/pkg/z33_web.js). The `import.meta.url`
// fallback in that glue (used only to locate the .wasm by URL) is dead code
// here — we always instantiate from bytes — so we define it to a harmless
// literal to keep esbuild happy in CJS/IIFE output.

import * as esbuild from "esbuild";

const production = process.argv.includes("--production");
const watch = process.argv.includes("--watch");

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
