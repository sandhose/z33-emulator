import "./style.css";
import bindings from "z33-web-bindings";

const samples = {
  directives: () => import("../../samples/directives.S"),
  fact: () => import("../../samples/fact.S"),
  handler: () => import("../../samples/handler.S"),
};

(async () => {
  const root = document.createElement("main");

  const editorContainer = document.createElement("div");
  editorContainer.classList.add("editor-container");
  editorContainer.classList.add("loading");
  root.appendChild(editorContainer);
  const result = document.createElement("pre");
  result.classList.add("result");
  root.appendChild(result);

  const output = document.createElement("output");
  output.value = "Loading compiler...";
  result.appendChild(output);

  document.body.appendChild(root);

  const { dump } = await bindings();

  const monaco = await import("./monaco");
  editorContainer.classList.remove("loading");

  const model = monaco.editor.createModel("", "text/plain");

  const selector = document.createElement("div");
  selector.classList.add("selector");
  editorContainer.appendChild(selector);
  selector.appendChild(document.createTextNode("Load sample: "));

  Object.entries(samples).forEach(([key, load]) => {
    const button = document.createElement("button");
    button.appendChild(document.createTextNode(key));
    button.addEventListener("click", async () => {
      const program = await load();
      model.setValue(program.default);
    });
    button.addEventListener("mouseover", () => load()); // Preload on mouseover
    selector.appendChild(button);
  });

  const editor = document.createElement("div");
  editor.classList.add("editor");
  editorContainer.appendChild(editor);

  monaco.editor
    .create(editor, {
      automaticLayout: true,
      theme: "vs-dark",
    })
    .setModel(model);

  const update = () => {
    const value = model.getValue();
    const ast = dump(value);
    output.value = ast;
  };

  model.onDidChangeContent(() => update());

  update();
})();
