import rust from "@wasm-tool/rollup-plugin-rust";

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
  ],
};
