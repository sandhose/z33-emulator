/// <reference types="vite/client" />

declare module "z33-web-bindings" {
  type Exports = typeof import("./target/wasm-pack/z33-web/index");
  const Loader: () => Promise<Exports>;
  export default Loader;
}

declare module "@wasm-tool/rollup-plugin-rust" {
  const Plugin: () => import("rollup").Plugin;
  export default Plugin;
}