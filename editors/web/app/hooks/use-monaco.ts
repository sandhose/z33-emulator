// oxlint-disable typescript/no-redundant-type-constituents -- Monaco is not `any`, false positive
import type { Monaco } from "@monaco-editor/react";
import { useEffect, useState } from "react";

/**
 * The Monaco namespace, `null` until its chunk has loaded. Mounting this hook
 * starts the fetch; the app needs Monaco's models for the compile check even
 * when the editor itself is not on screen.
 */
export function useMonacoInstance(): Monaco | null {
  const [instance, setInstance] = useState<Monaco | null>(null);

  useEffect(() => {
    let active = true;
    import("../monaco").then(
      (module) => {
        if (active) setInstance(module.monacoApi);
      },
      (error: unknown) => {
        // The editor renders through the same chunk, so its Suspense boundary
        // and the error boundary above it report the failure to the user.
        console.error("Failed to load the editor:", error);
      },
    );
    return () => {
      active = false;
    };
  }, []);

  return instance;
}
