import "./globals.css";
import "./monaco.ts";
import "./stores/theme-store.ts";
import { createRoot } from "react-dom/client";
import App from "./App";
import { ErrorBoundary } from "./components/error-boundary";
import { Button } from "./components/ui/button";
import { TooltipProvider } from "./components/ui/tooltip.tsx";

const rootElement = document.querySelector("#root");
if (!rootElement) {
  throw new Error("No root element found");
}
const root = createRoot(rootElement);

const FullPageError: React.FC<{ error: Error }> = ({ error }) => (
  <div className="flex h-screen flex-col items-center justify-center gap-4 bg-background p-8 text-center">
    <h1 className="text-lg font-semibold text-foreground">
      Something went wrong
    </h1>
    <p className="max-w-md text-sm text-muted-foreground">{error.message}</p>
    <Button
      onClick={() => {
        window.location.reload();
      }}
    >
      Reload
    </Button>
  </div>
);

root.render(
  <ErrorBoundary fallback={(error) => <FullPageError error={error} />}>
    <TooltipProvider delay={400}>
      <App />
    </TooltipProvider>
  </ErrorBoundary>,
);
