import { fileURLToPath } from "node:url";
import tailwindcss from "@tailwindcss/vite";
import react from "@vitejs/plugin-react-swc";
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
    tailwindcss(),
    react(),
    rust({
      nodejs: false,
      experimental: {
        typescriptDeclarationDir: "types",
      },
    }),
  ],
});
