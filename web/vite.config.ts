import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import rust from "@wasm-tool/rollup-plugin-rust";
import { fileURLToPath } from "node:url";

export default defineConfig({
	build: {
		target: "esnext",
	},
	resolve: {
		alias: {
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
