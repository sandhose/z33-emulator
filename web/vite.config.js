import { defineConfig } from "vite";
import rust from "@wasm-tool/rollup-plugin-rust";
import { fileURLToPath } from "node:url";

export default defineConfig({
  resolve: {
    alias: {
      "z33-web-bindings": fileURLToPath(
        new URL("./Cargo.toml", import.meta.url),
      ),
    },
  },
  plugins: [
    rust({
      experimental: {
        typescriptDeclarationDir: 'types',
      }
    })
  ],
});
