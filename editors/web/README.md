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
| `pnpm run build:wasm:dev` | Build the wasm bindings into `pkg/` on their own |
| `pnpm run check` | `tsc -b`, then oxlint and oxfmt |
| `pnpm run test` | Vitest: both projects |
| `pnpm run test:unit` | The `unit` project alone, in node |
| `pnpm run test-storybook` | The `storybook` project alone, in chromium |
| `pnpm run e2e` | The Playwright suite, against a dev server it starts |
| `pnpm run knip` | Unused files, dependencies and exports |
| `pnpm run storybook` | The Storybook dev server |

`check` type-checks against the generated bindings, so `pkg/` has to exist:
run `pnpm run build:wasm:dev` first in a fresh checkout.
