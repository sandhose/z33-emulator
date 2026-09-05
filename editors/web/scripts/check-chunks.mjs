// Guards the startup budget of a production build: the entry chunk must not
// pull Monaco in. A static import of the Monaco chunk, a modulepreload for it,
// or its stylesheet in <head> each put 3 MB in front of the first paint, and
// any of the three can come back from a chunking or import change without a
// visible symptom on a fast connection.
//
// Run after `vite build`, from editors/web: node scripts/check-chunks.mjs
import { existsSync, readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

const dist = fileURLToPath(new URL("../dist/", import.meta.url));
if (!existsSync(`${dist}index.html`)) {
  console.error("No dist/index.html: run vite build first.");
  process.exit(1);
}
const html = readFileSync(`${dist}index.html`, "utf8");
const failures = [];

const preloaded = [...html.matchAll(/<link[^>]+rel="modulepreload"[^>]+>/gu)]
  .map(([tag]) => /href="([^"]+)"/u.exec(tag)?.[1] ?? "")
  .filter((href) => href.includes("monaco"));
if (preloaded.length > 0) {
  failures.push(
    `index.html preloads the Monaco chunk: ${preloaded.join(", ")}`,
  );
}

const stylesheets = [...html.matchAll(/<link[^>]+rel="stylesheet"[^>]+>/gu)]
  .map(([tag]) => /href="([^"]+)"/u.exec(tag)?.[1] ?? "")
  .filter((href) => href.includes("monaco"));
if (stylesheets.length > 0) {
  failures.push(
    `index.html loads the Monaco stylesheet: ${stylesheets.join(", ")}`,
  );
}

const entry = /<script[^>]+type="module"[^>]+src="([^"]+)"/u.exec(html)?.[1];
if (entry === undefined) {
  failures.push("no module entry script in index.html");
} else {
  const source = readFileSync(`${dist}${entry.replace(/^\.\//u, "")}`, "utf8");
  // Static imports only: `import"./x.js"` and `from"./x.js"`, in whichever
  // quote the bundler picked. A dynamic `import("./x.js")` has a parenthesis
  // and does not match.
  const imported = [];
  for (const match of source.matchAll(
    /(?:from|import)(["'])(\.\/[^"']+)\1/gu,
  )) {
    const specifier = match[2] ?? "";
    if (specifier.includes("monaco")) imported.push(specifier);
  }
  if (imported.length > 0) {
    failures.push(`${entry} statically imports ${imported.join(", ")}`);
  }
}

if (failures.length > 0) {
  console.error(`Monaco is on the critical path:\n- ${failures.join("\n- ")}`);
  process.exit(1);
}
console.log("Monaco stays off the entry chunk's critical path.");
