(async () => {
  const wasm = await import("./Cargo.toml");
  const { dump } = await wasm.default();

  const input = document.createElement("textarea");
  const output = document.createElement("output");

  input.addEventListener("input", (e) => {
    output.value = dump(input.value);
  });

  const main = document.createElement("main");
  main.appendChild(input);
  const wrapper = document.createElement("pre");
  wrapper.appendChild(output);
  main.appendChild(wrapper);

  document.body.appendChild(main);
})();
