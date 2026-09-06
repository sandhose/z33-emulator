# z33-web

The Z33 web IDE: a React + Vite app around Monaco, talking to the emulator and
the language server through the wasm bindings in `crates/wasm`.

## Commands

Run from this directory, or from the repository root with
`pnpm --filter z33-web run <script>`.

| Command | What it does |
| --- | --- |
| `pnpm run start` | Build the wasm bindings (dev profile) and serve the app |
| `pnpm run build` | Build the wasm bindings (release) and the production bundle |
| `pnpm --filter z33-editor-shared run build:wasm:dev` | Build the wasm bindings into `editors/shared/pkg/` on their own |
| `pnpm run check` | `tsc -b`, then oxlint and oxfmt |
| `pnpm run knip` | Unused files, dependencies and exports |
| `pnpm run storybook` | The Storybook dev server |

The app's tests belong to the workspace-wide Vitest and Playwright configs and
run from the repository root; see the Development section of the [root
README](../../README.md) for the project names and the port overrides.

`check` type-checks against the generated bindings, so `editors/shared/pkg/`
has to exist: run `pnpm --filter z33-editor-shared run build:wasm:dev` first in
a fresh checkout.
