import { useState } from "react";
import { Computer } from "z33-web-bindings";
import { MultiFileEditor } from "./multi-file-editor";
import { ComputerView } from "./computer";

const initialFiles = import.meta.glob("../../samples/*.S", {
	as: "raw",
	eager: true,
});
const initialFilesMap = new Map(Object.entries(initialFiles));

const App = () => {
	const [computer, setComputer] = useState<Computer | null>(null);

	return computer === null ? (
		<MultiFileEditor
			initialFiles={initialFilesMap}
			initialSelected="fact.S"
			onComputer={setComputer}
		/>
	) : (
		<ComputerView computer={computer} />
	);
};

export default App;
