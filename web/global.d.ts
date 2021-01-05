import { Environment } from "monaco-editor";

declare global {
  interface Window {
    MonacoEnvironment: Environment;
  }
}

declare module "web-worker:*" {
  const WorkerFactory: new () => Worker;
  export default WorkerFactory;
}

declare module "z33-web-bindings" {
  type Exports = typeof import("./target/wasm-pack/z33-web/index");
  const Loader: () => Promise<Exports>;
  export default Loader;
}

declare module "../../samples/*" {
  const Sample: string;
  export default Sample;
}
