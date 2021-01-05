const samples = {
  directives: () => import("../samples/directives.S"),
  fact: () => import("../samples/fact.S"),
  handler: () => import("../samples/handler.S"),
};

(async () => {
  const wasm = await import("./Cargo.toml");
  const { dump } = await wasm.default();

  const input = document.createElement("textarea");
  const output = document.createElement("output");
  const update = () => {
    output.value = dump(input.value);
  };

  const editor = document.createElement("div");
  editor.classList.add("editor");

  const selector = document.createElement("div");
  selector.classList.add("selector");
  editor.appendChild(selector);
  selector.appendChild(document.createTextNode("Load sample: "));

  Object.entries(samples).forEach(([key, load]) => {
    const button = document.createElement("button");
    button.appendChild(document.createTextNode(key));
    button.addEventListener("click", async (e) => {
      const program = await load();
      input.value = program.default;
      update();
    });
    selector.appendChild(button);
  });

  editor.appendChild(input);
  input.addEventListener("input", update);

  const wrapper = document.createElement("pre");

  wrapper.appendChild(output);

  const main = document.createElement("main");
  main.appendChild(editor);
  main.appendChild(wrapper);

  document.body.appendChild(main);
})();
