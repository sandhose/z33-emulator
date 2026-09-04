import { fileURLToPath } from "node:url";
import tailwindcss from "@tailwindcss/vite";
import react from "@vitejs/plugin-react";
import { defineConfig } from "vite";

// The engines this app supports. The `using` declarations in the emulator
// worker are the only syntax that needs lowering for them: Safari has no
// explicit resource management yet.
const browserTargets = ["chrome120", "edge120", "firefox120", "safari17"];

export default defineConfig({
  base: "./",
  // The dev server transforms the same sources, and its default target is
  // esnext, so without this the tests and the preview only agree by accident.
  oxc: { target: browserTargets },
  build: {
    target: browserTargets,
  },
  worker: {
    format: "es",
  },
  resolve: {
    alias: {
      "@": fileURLToPath(new URL("./app", import.meta.url)),
    },
  },
  plugins: [tailwindcss(), react({ compiler: true })],
});
