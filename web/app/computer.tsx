import type * as React from "react";
import { useState, useEffect } from "react";
import { Cell, Computer, MemoryView } from "z33-web-bindings";
import {
	Table,
	TableBody,
	TableCaption,
	TableCell,
	TableHead,
	TableHeader,
	TableRow,
} from "./components/ui/table";
import { Button } from "./components/ui/button";
import { Input } from "./components/ui/input";
import { StepForm } from "./step-form";
import { Separator } from "./components/ui/separator";
import { FormControl, FormItem, FormLabel } from "./components/ui/form";

const MEMORY_SIZE = 10_000;

type Props = {
	computer: Computer;
};

const CellView: React.FC<{ cell: Cell }> = ({ cell }) =>
	cell.type === "word"
		? cell.word
		: cell.type === "instruction"
		  ? cell.instruction
		  : "0 (empty)";

const normalize = (value: number): number =>
	Math.max(0, Math.min(value, MEMORY_SIZE));

const MemoryViewer: React.FC<{
	memory: MemoryView;
	highlight: number;
	labels: Map<number, string[]>;
	description: string;
}> = ({ memory, highlight, labels, description }) => {
	return (
		<Table className="w-96">
			<TableCaption>{description}</TableCaption>
			<TableHeader>
				<TableRow>
					<TableHead className="w-8">Address</TableHead>
					<TableHead>Value</TableHead>
					<TableHead className="w-8">Labels</TableHead>
				</TableRow>
			</TableHeader>
			<TableBody className="font-mono">
				{memory.cells.map((cell, i) => {
					const address = i + memory.start;
					return (
						<TableRow
							key={address}
							data-state={address === highlight ? "selected" : ""}
						>
							<TableCell>{address}</TableCell>
							<TableCell>
								<CellView cell={cell} />
							</TableCell>
							<TableCell>{labels.get(i + memory.start)?.join(", ")}</TableCell>
						</TableRow>
					);
				})}
			</TableBody>
		</Table>
	);
};

export const ComputerView: React.FC<Props> = ({ computer }) => {
	const registers = computer.registers;
	const [, update] = useState({});
	const [viewAddress, setViewAddress] = useState(1000);
	const [stepsToRun, setStepsToRun] = useState(0);

	useEffect(() => {
		if (stepsToRun > 0) {
			const intervalId = setInterval(() => {
				computer.step();
				setStepsToRun((prevCounter) => prevCounter - 1);
			}, 20);

			return () => clearInterval(intervalId);
		}
	}, [stepsToRun, computer]);

	const spView = computer.memory_view(
		normalize(registers.sp - 5),
		normalize(registers.sp + 15),
	);

	const pcView = computer.memory_view(
		normalize(registers.pc - 10),
		normalize(registers.pc + 10),
	);

	const customView = computer.memory_view(
		normalize(viewAddress),
		normalize(viewAddress + 20),
	);

	const labels = new Map<number, string[]>();
	for (const [label, address] of computer.labels) {
		const values = labels.get(address) || [];
		labels.set(address, [...values, label]);
	}

	return (
		<div className="flex justify-center gap-4 p-2 my-4 mx-auto *:w-96">
			<div className="flex flex-col gap-4">
				{stepsToRun > 0 ? (
					<div className="border p-4 rounded flex flex-col">
						<Button variant="destructive" onClick={() => setStepsToRun(0)}>
							Stop
						</Button>
					</div>
				) : (
					<StepForm
						onStep={() => {
							computer.step();
							update({});
						}}
					/>
				)}

				<Table>
					<TableHeader>
						<TableRow>
							<TableHead className="w-8">Reg</TableHead>
							<TableHead>Value</TableHead>
						</TableRow>
					</TableHeader>
					<TableBody className="font-mono">
						<TableRow>
							<TableCell>%a</TableCell>
							<TableCell>
								<CellView cell={registers.a} />
							</TableCell>
						</TableRow>
						<TableRow>
							<TableCell>%b</TableCell>
							<TableCell>
								<CellView cell={registers.b} />
							</TableCell>
						</TableRow>
						<TableRow>
							<TableCell>%pc</TableCell>
							<TableCell>{registers.pc}</TableCell>
						</TableRow>
						<TableRow>
							<TableCell>%sp</TableCell>
							<TableCell>{registers.sp}</TableCell>
						</TableRow>
						<TableRow>
							<TableCell>%sr</TableCell>
							<TableCell>{registers.sr}</TableCell>
						</TableRow>
					</TableBody>
				</Table>
			</div>

			<div className="flex flex-col gap-4">
				<Input
					type="number"
					value={viewAddress}
					onChange={(e) => setViewAddress(+e.target.value)}
				/>

				<Separator />

				<MemoryViewer
					description="Memory viewer for the computer"
					memory={customView}
					highlight={viewAddress}
					labels={labels}
				/>
			</div>

			<MemoryViewer
				description="Stack view"
				memory={spView}
				highlight={registers.sp}
				labels={labels}
			/>
			<MemoryViewer
				description="Program counter view"
				memory={pcView}
				highlight={registers.pc}
				labels={labels}
			/>
		</div>
	);
};
