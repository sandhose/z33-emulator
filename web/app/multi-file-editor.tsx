import { Editor, Monaco, useMonaco } from "@monaco-editor/react";
import type * as React from "react";
import { useState } from "react";
import { Uri } from "monaco-editor";
import { Button } from "./components/ui/button";
import { Separator } from "./components/ui/separator";
import { Input } from "./components/ui/input";
import { FilePlusIcon, TrashIcon } from "@radix-ui/react-icons";
import {
	Table,
	TableBody,
	TableCell,
	TableHead,
	TableHeader,
	TableRow,
} from "./components/ui/table";

type Props = {
	initialFiles: Map<string, string>;
	initialSelected: string;
	onCompile?: (files: Map<string, string>, selected: string) => void;
};

export const MultiFileEditor: React.FC<Props> = ({
	initialFiles,
	initialSelected,
	onCompile,
}: Props) => {
	const monaco = useMonaco();
	const [fileName, setFileName] = useState(Uri.file(initialSelected));
	const [fileNames, setFileNames] = useState<Uri[]>([]);

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

		onCompile?.(files, fileName.path);
	}

	function handleEditorWillMount(monaco: Monaco) {
		// Destroy all existing models
		for (const model of monaco.editor.getModels()) {
			model.dispose();
		}

		for (const [path, content] of initialFiles) {
			monaco.editor.createModel(
				content,
				"assembly",
				Uri.file(path.replace(/^.*[\\/]/, "")),
			);
		}
	}

	return (
		<main className="flex h-screen bg-background">
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
							<TableHead className="w-10">Delete</TableHead>
						</TableRow>
					</TableHeader>
					<TableBody>
						{fileNames.map((name) => (
							<TableRow
								key={name.path}
								data-state={name.path === fileName.path ? "selected" : ""}
							>
								<TableCell>
									<Button
										className="w-full text-left font-mono"
										variant="link"
										onClick={() => setFileName(name)}
									>
										{name.path}
									</Button>
								</TableCell>
								<TableCell>
									<Button
										variant="outline"
										size="icon"
										onClick={() => {
											if (!monaco) {
												return;
											}
											for (const model of monaco.editor.getModels()) {
												if (model.uri.path === name.path) {
													model.dispose();
													break;
												}
											}
											sync();
										}}
									>
										<TrashIcon />
									</Button>
								</TableCell>
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
				/>
			</div>
		</main>
	);
};
