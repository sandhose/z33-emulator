import rust from "@wasm-tool/rollup-plugin-rust";
import { string } from "rollup-plugin-string";

export default {
  input: "index.mjs",
  output: {
    dir: "dist/js",
    format: "es",
    sourcemap: true,
  },
  plugins: [
    rust({
      serverPath: "js/",
    }),
    string({
      include: "../samples/*.S",
    }),
  ],
};
