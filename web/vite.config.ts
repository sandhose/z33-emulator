import { fileURLToPath } from "node:url";
import react from "@vitejs/plugin-react";
import rust from "@wasm-tool/rollup-plugin-rust";
import { defineConfig } from "vite";

export default defineConfig({
  base: "./",
  build: {
    target: "esnext",
  },
  resolve: {
    alias: {
      "@": fileURLToPath(new URL("./app", import.meta.url)),
      "z33-web-bindings": fileURLToPath(
        new URL("./Cargo.toml", import.meta.url),
      ),
    },
  },
  plugins: [
    react(),
    rust({
      experimental: {
        directExports: true,
        typescriptDeclarationDir: "types",
      },
    }),
  ],
});
