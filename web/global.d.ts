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

declare module "*/Cargo.toml" {
  type Module = {
    dump(string): string;
  };
  const Loader: () => Promise<Module>;
  export default Loader;
}

declare module "../../samples/*" {
  const Sample: string;
  export default Sample;
}
