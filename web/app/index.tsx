import "./globals.css";
import "./monaco.ts";
import { createRoot } from "react-dom/client";
import App from "./App";
import { TooltipProvider } from "./components/ui/tooltip.tsx";

const rootElement = document.getElementById("root");
if (!rootElement) {
  throw new Error("No root element found");
}
const root = createRoot(rootElement);

root.render(
  <TooltipProvider delay={400}>
    <App />
  </TooltipProvider>,
);
