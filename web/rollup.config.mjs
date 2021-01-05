import webworker from "rollup-plugin-web-worker-loader";
import postcss from "rollup-plugin-postcss";
import sucrase from "@rollup/plugin-sucrase";
import resolve from "@rollup/plugin-node-resolve";
import rust from "@wasm-tool/rollup-plugin-rust";
import { string } from "rollup-plugin-string";
import { terser } from "rollup-plugin-terser";

export default {
  input: "app/index.ts",
  output: {
    dir: "dist/js",
    format: "es",
    sourcemap: true,
  },
  plugins: [
    string({
      include: "../samples/*.S",
    }),
    resolve({
      extensions: [".js", ".ts"],
    }),
    postcss(),
    webworker({
      inline: false,
      targetPlatform: "browser",
    }),
    sucrase({
      exclude: ["node_modules/**"],
      transforms: ["typescript"],
    }),
    rust({
      serverPath: "js/",
    }),
    terser(),
  ],
};
