// Type re-exports of the wasm-pack (`--target web`) bindings.
//
// The bindings themselves are instantiated inside the workers
// (`workers/emulator.worker.ts`, `workers/lsp.worker.ts`); the main thread only
// needs the types, so importing this module must never pull the wasm glue or
// the binary into the main bundle.
export type * from "z33-editor-shared/wasm";
