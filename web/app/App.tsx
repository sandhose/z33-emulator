import { useRef, useState } from "react";
import { Uri, editor } from "monaco-editor";
import Editor, { Monaco } from "@monaco-editor/react";
import { InMemoryPreprocessor, Program } from "z33-web-bindings";

const importedSamples = import.meta.glob("../../samples/*.S", {
	as: "raw",
	eager: true,
});

const files = Object.fromEntries(
	Object.entries(importedSamples).map(([key, value]) => [
		key.replace(/^.*[\\/]/, ""),
		value,
	]),
);

const App = () => {
	const editorRef = useRef<editor.IStandaloneCodeEditor | null>(null);
	const monacoRef = useRef<Monaco | null>(null);
	const [fileName, setFileName] = useState(Uri.file("fact.S"));
	const [fileNames, setFileNames] = useState<Uri[]>([]);
	const [preprocessed, setPreprocessed] = useState<string | null>(null);
	const [ast, setAst] = useState<string | null>(null);
	const [memory, setMemory] = useState<string | null>(null);
	const [labels, setLabels] = useState<string | null>(null);
	const [console, setConsole] = useState<string | null>(null);

	function sync() {
		const monaco = monacoRef.current;
		const editor = editorRef.current;
		if (!monaco || !editor) {
			return;
		}

		const models = monaco.editor.getModels();
		setFileNames(models.map((model) => model.uri));
	}

	function compile() {
		const monaco = monacoRef.current;
		const editor = editorRef.current;
		if (!monaco || !editor) {
			return;
		}

		const models = monaco.editor.getModels();
		const files = new Map(
			models.map((model) => [model.uri.path, model.getValue()]),
		);

		try {
			const preprocessor = new InMemoryPreprocessor(files);
			const result = preprocessor.preprocess(fileName.path);
			setPreprocessed(result);

			const program = Program.parse(result);
			setAst(program.ast);
			const layout = program.layout();
			setMemory(
				Array.from(layout.memory.entries())
					.map(([k, v]) => `${k}\t${v}`)
					.join("\n"),
			);
			setLabels(
				Array.from(layout.labels.entries())
					.map(([k, v]) => `${v}\t${k}`)
					.join("\n"),
			);
			setConsole(null);
			preprocessor.free();
			layout.free();
		} catch (e) {
			setConsole(String(e));
		}
	}

	function handleEditorWillMount(monaco: Monaco) {
		for (const [name, value] of Object.entries(files)) {
			monaco.editor.createModel(value, "assembly", monaco.Uri.file(name));
		}
	}

	return (
		<main>
			<div className="editor-container">
				<div className="selector">
					<button type="button" onClick={() => compile()}>
						Compile
					</button>
					{fileNames.map((name) => (
						<button
							type="button"
							className={
								name.toString() === fileName.toString() ? "active" : ""
							}
							key={name.toString()}
							onClick={() => setFileName(name)}
						>
							{name.path}
						</button>
					))}

					<form
						onSubmit={(e) => {
							e.preventDefault();
							const form = e.target as HTMLFormElement;
							const data = new FormData(form);
							const filename = data.get("filename");
							const monaco = monacoRef.current;
							if (!monaco || !filename || typeof filename !== "string") {
								return;
							}

							const uri = monaco.Uri.file(filename);
							monaco.editor.createModel("", "assembly", uri);
							form.reset();
							setFileName(uri);
							sync();
						}}
					>
						<input type="input" name="filename" />
						<button type="submit">Create</button>
					</form>
				</div>

				<Editor
					className="editor"
					theme="vs-dark"
					path={fileName.path}
					beforeMount={handleEditorWillMount}
					onMount={(editor, monaco) => {
						editorRef.current = editor;
						monacoRef.current = monaco;
						sync();
					}}
				/>
			</div>
			<pre className="result">
				<section>
					<h4>Console</h4>
					{console}
				</section>
				<section>
					<h4>Labels</h4>
					{labels}
				</section>
				<section>
					<h4>Memory</h4>
					{memory}
				</section>
				<section>
					<h4>Preprocessor</h4>
					{preprocessed}
				</section>
				<section>
					<h4>AST</h4>
					{ast}
				</section>
			</pre>
		</main>
	);
};

export default App;
