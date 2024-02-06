import "./globals.css";
import "./monaco.ts";
import { createRoot } from "react-dom/client";
import App from "./App";

const rootElement = document.getElementById("root");
if (!rootElement) {
	throw new Error("No root element found");
}
const root = createRoot(rootElement);

root.render(<App />);
