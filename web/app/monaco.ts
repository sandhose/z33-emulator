import * as monaco from "monaco-editor/esm/vs/editor/editor.api.js";
import { loader } from "@monaco-editor/react";

import editorWorker from "monaco-editor/esm/vs/editor/editor.worker?worker";

self.MonacoEnvironment = {
	getWorker(_workerId, _label) {
		return new editorWorker();
	},
};

loader.config({ monaco });

const Instructions = [
	/// Add a value to a register
	"add",
	/// Bitwise `and` with a given value
	"and",
	/// Push `%pc` and go to the given address
	"call",
	/// Compare a value with a register
	"cmp",
	/// Divide a register by a value
	"div",
	/// Load a memory cell to a register and set this cell to 1
	"fas",
	/// Read a value from an I/O controller
	"in",
	/// Unconditional jump
	"jmp",
	/// Jump if equal
	"jeq",
	/// Jump if not equal
	"jne",
	/// Jump if less or equal
	"jle",
	/// Jump if strictly less
	"jlt",
	/// Jump if greater of equal
	"jge",
	/// Jump if strictly greater
	"jgt",
	/// Load a register with a value
	"ld",
	/// Multiply a value to a register
	"mul",
	"neg",
	/// No-op
	"nop",
	/// Bitwise negation of a register
	"not",
	/// Bitwise `or` with a given value
	"or",
	/// Write a value to an I/O controller
	"out",
	/// Pop a value from the stack
	"pop",
	/// Push a value into the stack
	"push",
	/// Reset the computer
	"reset",
	/// Return from an interrupt or an exception
	"rti",
	/// Return from a `call`
	"rtn",
	/// Bitshift to the left
	"shl",
	/// Bitshift to the right
	"shr",
	/// Store a register value in memory
	"st",
	/// Substract a value from a register
	"sub",
	/// Swap a value and a register
	"swap",
	/// Start a `trap` exception
	"trap",
	/// Bitwise `xor` with a given value
	"xor",
];

monaco.languages.register({ id: "z33" });

// Register a tokens provider for the language
monaco.languages.setMonarchTokensProvider("z33", {
	includeLF: true,
	tokenizer: {
		root: [
			[
				new RegExp(`\\s(${Instructions.join("|")})\\b`),
				"keyword",
				"@instruction",
			],
			[
				new RegExp(`^(${Instructions.join("|")})\\b`),
				"keyword",
				"@instruction",
			],
			[/[\w_]+:/, "type"],
			[/#\w+/, "strong", "@preprocessor"],
			[/\.\w+/, "keyword", "@directive"],
			{ include: "@constants" },
			{ include: "@comments" },
		],
		preprocessor: [
			[/\n$/, "endofline", "@pop"],
			{ include: "@constants" },
			{ include: "@string" },
		],
		directive: [
			[/\n$/, "endofline", "@pop"],
			{ include: "@constants" },
			{ include: "@string" },
			{ include: "@comments" },
		],
		instruction: [
			[/\n$/, "endofline", "@pop"],
			[/(\w|%)+[^a-zA-Z0-9_,\/]+(\w|%)+/, "invalid", "@pop"],
			[/\[[^\]]*\][^a-zA-Z0-9_,\/]+(\w|%)+/, "invalid", "@pop"],
			[/(\w|%)+[^a-zA-Z0-9_,\/]+\[[^\]]*\]/, "invalid", "@pop"],
			[/\[/, "operator", "@idx"],
			[/,/, "operator"],
			{ include: "@constants" },
			{ include: "@comments" },
			{ include: "@registers" },
		],
		string: [[/"[^"]*"/, "string"]],
		constants: [[/[0-9]+/, "constant"]],
		comments: [[/\/\/.*/, "comment"]],
		registers: [
			[/%(a|b|sp|pc)\b/, "tag"],
			[/%[^ab]\b/, "invalid"],
			[/%s[^p]/, "invalid"],
			[/%p[^c]/, "invalid"],
			[/%[^absp]/, "invalid"],
		],
		idx: [
			[/\]/, "operator", "@pop"],
			[/[^a-zA-Z0-9\-+% \t\]][^\n]*/, { token: "error", next: "@pop" }],
			{ include: "@constants" },
			{ include: "@registers" },
		],
	},
});
