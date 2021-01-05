import webworker from "rollup-plugin-web-worker-loader";
import postcss from "rollup-plugin-postcss";
import sucrase from "@rollup/plugin-sucrase";
import alias from "@rollup/plugin-alias";
import resolve from "@rollup/plugin-node-resolve";
import rust from "@wasm-tool/rollup-plugin-rust";
import { string } from "rollup-plugin-string";
import { terser } from "rollup-plugin-terser";
import html from "@rollup/plugin-html";
import dev from "rollup-plugin-dev";

import cssnano from "cssnano";

export default {
  input: "app/index.ts",
  output: {
    dir: "dist/",
    format: "es",
    sourcemap: true,
  },
  plugins: [
    alias({
      entries: [{ find: "z33-web-bindings", replacement: "./Cargo.toml" }],
    }),
    string({
      include: "../samples/*.S",
    }),
    resolve({
      extensions: [".js", ".ts"],
    }),
    postcss({ plugins: [cssnano()], extract: true }),
    webworker({
      inline: false,
      targetPlatform: "browser",
    }),
    sucrase({
      exclude: ["node_modules/**"],
      transforms: ["typescript"],
    }),
    rust(),
    terser(),
    html(),
    dev("dist"),
  ],
};
