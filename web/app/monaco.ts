import * as monaco from "monaco-editor/esm/vs/editor/editor.api.js";
import { loader } from "@monaco-editor/react";

import editorWorker from "monaco-editor/esm/vs/editor/editor.worker?worker";

self.MonacoEnvironment = {
	getWorker(_workerId, _label) {
		return new editorWorker();
	},
};

loader.config({ monaco });
