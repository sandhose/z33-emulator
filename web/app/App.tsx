import { useState } from "react";
import { InMemoryPreprocessor, Program, Computer } from "z33-web-bindings";
import { MultiFileEditor } from "./multi-file-editor";
import { EntrypointSelector } from "./entrypoint-selector";
import { ComputerView } from "./computer";

const initialFiles = import.meta.glob("../../samples/*.S", {
	as: "raw",
	eager: true,
});

const App = () => {
	const [files, setFiles] = useState<Map<string, string>>(
		new Map(Object.entries(initialFiles)),
	);
	const [selected, setSelected] = useState<string>("fact.S");
	const [program, setProgram] = useState<Program | null>(null);
	const [computer, setComputer] = useState<Computer | null>(null);

	function preprocess(files: Map<string, string>, selected: string) {
		console.log(files.keys());
		setSelected(selected);
		setFiles(files);
		const preprocessor = new InMemoryPreprocessor(files);
		const preprocessed = preprocessor.preprocess(selected);
		preprocessor.free();

		if (program !== null) {
			program.free();
		}
		const newProgram = Program.parse(preprocessed);
		setProgram(newProgram);
	}

	function compile(entrypoint: string) {
		if (program === null) {
			return;
		}

		const computer = program.compile(entrypoint);
		setComputer(computer);
	}

	return program === null ? (
		<MultiFileEditor
			initialFiles={files}
			initialSelected={selected}
			onCompile={preprocess}
		/>
	) : computer === null ? (
		<EntrypointSelector
			onRun={compile}
			onReset={() => {
				program?.free();
				setProgram(null);
			}}
			entrypoints={program.labels}
		/>
	) : (
		<ComputerView computer={computer} />
	);
};

export default App;
