import { Editor, Monaco, useMonaco } from "@monaco-editor/react";
import { useMediaQuery } from "usehooks-ts";
import type * as React from "react";
import { useState } from "react";
import { Uri } from "monaco-editor/esm/vs/editor/editor.api.js";
import { Button } from "./components/ui/button";
import { Separator } from "./components/ui/separator";
import { Input } from "./components/ui/input";
import { FilePlusIcon, TrashIcon, UploadIcon } from "@radix-ui/react-icons";
import { z } from "zod";
import { useForm } from "react-hook-form";
import { zodResolver } from "@hookform/resolvers/zod";
import {
	Form,
	FormControl,
	FormField,
	FormItem,
	FormLabel,
	FormMessage,
} from "./components/ui/form";
import { Computer, InMemoryPreprocessor, Program } from "z33-web-bindings";
import {
	Popover,
	PopoverTrigger,
	PopoverContent,
} from "./components/ui/popover";
import { EntrypointSelector } from "./entrypoint-selector";

type Props = {
	initialFiles: Map<string, string>;
	initialSelected: string;
	onCompile?: (files: Map<string, string>, selected: string) => void;
	onComputer?: (computer: Computer) => void;
};

const newFileFormSchema = z.object({
	filename: z.string().min(1),
});

const NewFileForm: React.FC<{
	onSubmit: (filename: string) => void;
}> = ({ onSubmit }) => {
	const form = useForm<z.infer<typeof newFileFormSchema>>({
		resolver: zodResolver(newFileFormSchema),
	});

	function handleSubmit(values: z.infer<typeof newFileFormSchema>) {
		try {
			onSubmit(values.filename);
			form.reset({ filename: "" });
		} catch (error) {
			form.setError("filename", { message: String(error) });
		}
	}

	return (
		<Form {...form}>
			<form
				className="flex flex-col gap-2"
				onSubmit={form.handleSubmit(handleSubmit)}
			>
				<FormField
					control={form.control}
					name="filename"
					render={({ field }) => (
						<FormItem>
							<FormLabel>Create a new empty file</FormLabel>
							<div className="flex gap-2">
								<FormControl>
									<Input placeholder="file.S" {...field} type="text" />
								</FormControl>
							</div>
							<FormMessage />
						</FormItem>
					)}
				/>
				<Button type="submit" variant="secondary">
					<FilePlusIcon className="mr-2 h-4 w-4" />
					New
				</Button>
			</form>
		</Form>
	);
};

const uploadFileFormSchema = z.object({
	files: z.instanceof(FileList),
});

const UploadFileForm: React.FC<{
	onSubmit: (file: File) => void;
}> = ({ onSubmit }) => {
	const form = useForm<z.infer<typeof uploadFileFormSchema>>({
		resolver: zodResolver(uploadFileFormSchema),
	});

	return (
		<Form {...form}>
			<form
				className="flex flex-col gap-2"
				onSubmit={form.handleSubmit((values, event) => {
					for (const file of values.files) {
						onSubmit(file);
					}
					(event?.target as HTMLFormElement).reset();
				})}
			>
				<FormField
					control={form.control}
					name="files"
					render={({ field }) => (
						<FormItem>
							<FormLabel>Upload from computer</FormLabel>
							<FormControl>
								<Input
									multiple
									ref={field.ref}
									disabled={field.disabled}
									name={field.name}
									onBlur={field.onBlur}
									onChange={(event) => field.onChange(event.target.files)}
									type="file"
								/>
							</FormControl>
							<FormMessage />
						</FormItem>
					)}
				/>
				<Button type="submit" variant="secondary">
					<UploadIcon className="mr-2 h-4 w-4" />
					Upload
				</Button>
			</form>
		</Form>
	);
};

export const MultiFileEditor: React.FC<Props> = ({
	initialFiles,
	initialSelected,
	onComputer,
}: Props) => {
	const monaco = useMonaco();
	const [fileName, setFileName] = useState(Uri.file(initialSelected));
	const [fileNames, setFileNames] = useState<Uri[]>([]);
	const [program, setProgram] = useState<Program | null>(null);
	const darkMode = useMediaQuery("(prefers-color-scheme: dark)");

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

		const preprocessor = new InMemoryPreprocessor(files);
		const preprocessed = preprocessor.preprocess(fileName.path);
		const newProgram = Program.parse(preprocessed);
		setProgram(newProgram);
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
				monaco.Uri.file(path.replace(/^.*[\\/]/, "")),
			);
		}
	}

	return (
		<main className="flex h-screen bg-background">
			<div className="flex flex-col gap-4 p-4 w-96">
				<div className="flex-1 flex flex-col overflow-auto">
					{fileNames.map((name) => (
						<div
							className="flex gap-2 items-center data-[state=selected]:bg-muted"
							key={name.path}
							data-state={name.path === fileName.path ? "selected" : ""}
						>
							<button
								type="button"
								className="flex-1 p-2 text-left text-sm font-mono self-stretch hover:underline"
								onClick={() => setFileName(name)}
							>
								{name.path}
							</button>
							<Button
								variant="ghost"
								className="m-2"
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
						</div>
					))}
				</div>

				<div className="flex flex-col gap-4 p-4 border rounded">
					<NewFileForm
						onSubmit={(filename) => {
							if (!monaco) {
								return;
							}

							setFileName(monaco.Uri.file(filename));
							const uri = monaco.Uri.file(filename);
							monaco.editor.createModel("", "assembly", uri);
							setFileName(uri);
							sync();
						}}
					/>

					<Separator />

					<UploadFileForm
						onSubmit={(file) => {
							const reader = new FileReader();
							reader.onload = () => {
								if (!monaco) {
									return;
								}

								const uri = monaco.Uri.file(file.name);
								monaco.editor.createModel(
									reader.result as string,
									"assembly",
									uri,
								);
								setFileName(uri);
								sync();
							};
							reader.readAsText(file);
						}}
					/>
				</div>

				<Popover
					onOpenChange={(open) => (open ? compile() : setProgram(null))}
					open={!!program}
				>
					<PopoverTrigger asChild>
						<Button type="button">Compile</Button>
					</PopoverTrigger>
					<PopoverContent>
						{program && (
							<EntrypointSelector
								entrypoints={program.labels}
								onRun={(entrypoint) =>
									onComputer?.(program?.compile(entrypoint))
								}
							/>
						)}
					</PopoverContent>
				</Popover>
			</div>

			<div className="flex-1">
				<Editor
					className="editor"
					theme={darkMode ? "vs-dark" : "light"}
					path={fileName.path}
					beforeMount={handleEditorWillMount}
					onMount={() => sync()}
				/>
			</div>
		</main>
	);
};
