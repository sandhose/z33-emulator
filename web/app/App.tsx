import { useState } from "react";
import { Uri } from "monaco-editor";
import Editor, { Monaco, useMonaco } from "@monaco-editor/react";
import {
	InMemoryPreprocessor,
	Labels,
	MemoryReport,
	Program,
} from "z33-web-bindings";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import {
	Table,
	TableBody,
	TableCaption,
	TableCell,
	TableHead,
	TableHeader,
	TableRow,
} from "@/components/ui/table";
import { FilePlusIcon } from "@radix-ui/react-icons";
import { Separator } from "./components/ui/separator";

const files = import.meta.glob("../../samples/*.S", {
	as: "raw",
	eager: true,
});

const App = () => {
	const monaco = useMonaco();
	const [fileName, setFileName] = useState(Uri.file("fact.S"));
	const [fileNames, setFileNames] = useState<Uri[]>([]);
	const [preprocessed, setPreprocessed] = useState<string | null>(null);
	const [ast, setAst] = useState<string | null>(null);
	const [memory, setMemory] = useState<MemoryReport | null>(null);
	const [labels, setLabels] = useState<Labels | null>(null);
	const [console, setConsole] = useState<string | null>(null);

	function sync() {
		if (!monaco) {
			return;
		}

		const models = monaco.editor.getModels();
		setFileNames(models.map((model) => model.uri));
	}

	function compile() {
		if (!monaco) {
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
			setMemory(layout.memory);
			setLabels(layout.labels);
			setConsole(null);
			preprocessor.free();
			layout.free();
		} catch (e) {
			setConsole(String(e));
		}
	}

	function handleEditorWillMount(monaco: Monaco) {
		for (const [name, value] of Object.entries(files)) {
			monaco.editor.createModel(
				value,
				"assembly",
				monaco.Uri.file(name.replace(/^.*[\\/]/, "")),
			);
		}
	}

	return (
		<main className="flex bg-background">
			<div className="flex flex-col gap-4 p-4">
				<Button type="button" onClick={() => compile()}>
					Compile
				</Button>

				<Separator />

				<form
					className="flex gap-2"
					onSubmit={(e) => {
						e.preventDefault();
						const form = e.target as HTMLFormElement;
						const data = new FormData(form);
						const filename = data.get("filename");
						if (!monaco || !filename || typeof filename !== "string") {
							return;
						}

						const uri = Uri.file(filename);
						monaco.editor.createModel("", "assembly", uri);
						form.reset();
						setFileName(uri);
						sync();
					}}
				>
					<Input type="input" name="filename" />
					<Button type="submit" variant="outline">
						<FilePlusIcon className="mr-2 h-4 w-4" />
						New
					</Button>
				</form>

				<Table>
					<TableHeader>
						<TableRow>
							<TableHead>Filename</TableHead>
						</TableRow>
					</TableHeader>
					<TableBody>
						{fileNames.map((name) => (
							<TableRow
								onClick={() => setFileName(name)}
								data-state={name.path === fileName.path ? "selected" : ""}
								key={name.path}
							>
								<TableCell>{name.path}</TableCell>
							</TableRow>
						))}
					</TableBody>
				</Table>
			</div>

			<div className="flex-1">
				<Editor
					className="editor"
					path={fileName.path}
					beforeMount={handleEditorWillMount}
					onMount={() => sync()}
					height="100vh"
				/>
			</div>
			<div className="flex flex-col flex-1 gap-4 p-4 h-screen *:flex-1 *:flex">
				<Table>
					<TableCaption>Labels</TableCaption>
					<TableHeader>
						<TableRow>
							<TableHead className="w-16">Address</TableHead>
							<TableHead>Label</TableHead>
						</TableRow>
					</TableHeader>
					<TableBody className="flex-1">
						{Array.from(labels?.entries() ?? []).map(([label, address]) => (
							<TableRow key={label}>
								<TableCell>{address}</TableCell>
								<TableCell>{label}</TableCell>
							</TableRow>
						))}
					</TableBody>
				</Table>

				<Table>
					<TableCaption>Memory</TableCaption>
					<TableHeader>
						<TableRow>
							<TableHead className="w-16">Address</TableHead>
							<TableHead>Value</TableHead>
						</TableRow>
					</TableHeader>
					<TableBody className="flex-1">
						{Array.from(memory?.entries() ?? []).map(([address, value]) => (
							<TableRow key={address}>
								<TableCell>{address}</TableCell>
								<TableCell>{value}</TableCell>
							</TableRow>
						))}
					</TableBody>
				</Table>
			</div>
		</main>
	);
};

export default App;
