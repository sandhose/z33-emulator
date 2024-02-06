import type * as React from "react";
import { useState, useSyncExternalStore, useCallback, useMemo } from "react";
import { Cell, Computer, Registers } from "z33-web-bindings";
import {
	Table,
	TableBody,
	TableCaption,
	TableCell,
	TableHead,
	TableHeader,
	TableRow,
} from "./components/ui/table";
import { Input } from "./components/ui/input";
import { StepForm } from "./step-form";
import { Separator } from "./components/ui/separator";
import { Button } from "./components/ui/button";
import {
	DoubleArrowLeftIcon,
	DoubleArrowRightIcon,
	MinusIcon,
	PlusIcon,
} from "@radix-ui/react-icons";

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
	Math.max(0, Math.min(value, MEMORY_SIZE - 1));

const useMemoryCell = (computer: Computer, address: number): Cell => {
	const subscribe = useCallback(
		(cb: (cell: Cell) => void) => computer.subscribe_memory(address, cb),
		[computer, address],
	);
	return useSyncExternalStore(subscribe, () => computer.memory(address));
};

const MemoryCell: React.FC<{ computer: Computer; address: number }> = ({
	computer,
	address,
}) => {
	const cell = useMemoryCell(computer, address);
	return <CellView cell={cell} />;
};

const MemoryViewer: React.FC<{
	computer: Computer;
	from: number;
	count: number;
	highlight: number;
	labels: Map<number, string[]>;
	description: string;
}> = ({ computer, from, count, highlight, labels, description }) => {
	const start = normalize(from);
	const end = normalize(from + count - 1) + 1;
	const cells = Array.from({ length: end - start }, (_, i) => start + i);
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
				{cells.map((address) => (
					<TableRow
						key={address}
						data-state={address === highlight ? "selected" : undefined}
					>
						<TableCell>{address}</TableCell>
						<TableCell>
							<MemoryCell computer={computer} address={address} />
						</TableCell>
						<TableCell>{labels.get(address)?.join(", ") || ""}</TableCell>
					</TableRow>
				))}
			</TableBody>
		</Table>
	);
};

const useRegisters = (computer: Computer): Registers => {
	const subscribe = useCallback(
		(cb: (registers: Registers) => void) => computer.subscribe_registers(cb),
		[computer],
	);
	return useSyncExternalStore(subscribe, () => computer.registers());
};

const RegisterView: React.FC<{ registers: Registers }> = ({ registers }) => {
	return (
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
	);
};

export const ComputerView: React.FC<Props> = ({ computer }) => {
	const onStep = useCallback(() => computer.step(), [computer]);
	const registers = useRegisters(computer);

	const [viewAddress, setViewAddress] = useState(1000);

	const labels = useMemo(() => {
		const labels = new Map<number, string[]>();
		for (const [label, address] of computer.labels) {
			const values = labels.get(address) || [];
			labels.set(address, [...values, label]);
		}
		return labels;
	}, [computer]);

	return (
		<div className="flex justify-center gap-4 p-4 mx-auto *:w-96 h-screen">
			<div className="flex flex-col gap-4">
				<div className="border p-4 rounded">Cycles: {computer.cycles}</div>
				<div className="flex-1">
					<RegisterView registers={registers} />
				</div>
				<StepForm onStep={onStep} />
			</div>

			<div className="flex flex-col gap-4">
				<div className="flex-1 overflow-auto">
					<MemoryViewer
						description="Arbitrary memory viewer"
						computer={computer}
						from={viewAddress}
						count={20}
						highlight={viewAddress}
						labels={labels}
					/>
				</div>

				<div className="flex flex-col gap-2 border p-4 rounded">
					<h4 className="text-sm font-medium">View at label:</h4>

					{Array.from(computer.labels).map(([label, address]) => (
						<Button
							variant="secondary"
							key={label}
							onClick={() => setViewAddress(address)}
						>
							{label}
						</Button>
					))}

					<div className="flex gap-2 items-center">
						<Button
							variant="outline"
							className="aspect-square"
							size="icon"
							onClick={() => setViewAddress(viewAddress - 10)}
						>
							<DoubleArrowLeftIcon />
						</Button>
						<Button
							variant="outline"
							className="aspect-square"
							size="icon"
							onClick={() => setViewAddress(viewAddress - 1)}
						>
							<MinusIcon />
						</Button>
						<Input
							type="number"
							value={viewAddress}
							onChange={(e) => setViewAddress(+e.target.value)}
						/>
						<Button
							variant="outline"
							className="aspect-square"
							size="icon"
							onClick={() => setViewAddress(viewAddress + 1)}
						>
							<PlusIcon />
						</Button>
						<Button
							variant="outline"
							className="aspect-square"
							size="icon"
							onClick={() => setViewAddress(viewAddress + 10)}
						>
							<DoubleArrowRightIcon />
						</Button>
					</div>
				</div>
			</div>

			<MemoryViewer
				description="Stack view"
				computer={computer}
				from={registers.sp - 5}
				count={20}
				highlight={registers.sp}
				labels={labels}
			/>
			<MemoryViewer
				description="Program counter view"
				computer={computer}
				from={registers.pc - 10}
				count={20}
				highlight={registers.pc}
				labels={labels}
			/>
		</div>
	);
};
