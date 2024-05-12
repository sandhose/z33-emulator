import { useVirtualizer } from "@tanstack/react-virtual";
import {
	DoubleArrowLeftIcon,
	DoubleArrowRightIcon,
	MinusIcon,
	PlusIcon,
} from "@radix-ui/react-icons";
import * as React from "react";
import { useCallback, useMemo, useState, useSyncExternalStore } from "react";
import { Cell, Computer, Registers } from "z33-web-bindings";
import { Button } from "./components/ui/button";
import { Input } from "./components/ui/input";
import {
	Table,
	TableBody,
	TableCell,
	TableHead,
	TableHeader,
	TableRow,
} from "./components/ui/table";
import { StepForm } from "./step-form";
import { cn } from "./lib/utils";

const MEMORY_SIZE = 10_000;

type Props = {
	computer: Computer;
};

const Word: React.FC<{ word: number; labels: Map<number, string[]> }> = ({
	word,
	labels,
}) => {
	const list = labels.get(word);
	if (list) {
		return <>{word} = {...list.map((l) => <Label label={l} />)}</>;
	}

	// Find the nearest label with offset.
	let distance = 100;
	let nearest: string[] | null = null;
	for (const [address, candidates] of labels) {
		const d = word - address;
		if (d > 0 && d < distance) {
			distance = d;
			nearest = candidates;
		}
	}

	if (nearest) {
		return (
			<>
				{word} = {...nearest.map((l) => <Label label={`${l}+${distance}`} />)}
			</>
		);
	}

	return word;
};

const CellView: React.FC<{ cell: Cell; labels: Map<number, string[]> }> = ({
	cell,
	labels,
}) =>
	cell.type === "word" ? (
		<Word word={cell.word} labels={labels} />
	) : cell.type === "instruction" ? (
		cell.instruction
	) : (
		"0 (empty)"
	);

const normalize = (value: number): number =>
	Math.max(0, Math.min(value, MEMORY_SIZE - 1));

const useMemoryCell = (computer: Computer, address: number): Cell => {
	const subscribe = useCallback(
		(cb: (cell: Cell) => void) => computer.subscribe_memory(address, cb),
		[computer, address],
	);
	return useSyncExternalStore(subscribe, () => computer.memory(address));
};

const MemoryCell: React.FC<{
	computer: Computer;
	address: number;
	labels: Map<number, string[]>;
}> = ({ computer, address, labels }) => {
	const cell = useMemoryCell(computer, address);
	return <CellView cell={cell} labels={labels} />;
};

const Label: React.FC<{ label: string }> = ({ label }) => (
	<div className="px-2 inline-block bg-accent text-accent-foreground rounded text-xs">
		{label}
	</div>
);

const MemoryViewer: React.FC<{
	computer: Computer;
	highlight: number;
	labels: Map<number, string[]>;
}> = ({ computer, highlight, labels }) => {
	const parentRef = React.useRef(null);

	const rowVirtualizer = useVirtualizer({
		// Render one more element to have the "TOP OF STACK" cell
		count: MEMORY_SIZE + 1,
		initialOffset: highlight * 32,
		getScrollElement: () => parentRef.current,
		estimateSize: () => 32,
	});

	React.useEffect(() => {
		rowVirtualizer.scrollToIndex(normalize(highlight), { align: "center" });
	}, [rowVirtualizer, highlight]);

	return (
		<div className="overflow-auto" ref={parentRef}>
			<div
				className="font-mono text-sm w-full relative"
				style={{ height: `${rowVirtualizer.getTotalSize()}px` }}
			>
				{rowVirtualizer.getVirtualItems().map((virtualItem) => (
					<div
						className="flex px-2 py-1 gap-2 border-b border-b-muted items-center data-[state=selected]:bg-muted"
						key={virtualItem.key}
						style={{
							position: "absolute",
							top: 0,
							left: 0,
							width: "100%",
							height: `${virtualItem.size}px`,
							transform: `translateY(${virtualItem.start}px)`,
						}}
						data-state={
							virtualItem.index === highlight ? "selected" : undefined
						}
					>
						{virtualItem.index === MEMORY_SIZE ? (
							"TOP OF STACK"
						) : (
							<>
								<div className="w-10">{virtualItem.index}</div>
								<div className="flex-1">
									<MemoryCell
										computer={computer}
										address={virtualItem.index}
										labels={labels}
									/>
								</div>
								{...(labels
									.get(virtualItem.index)
									?.map((l) => <Label label={l} />) || [])}
							</>
						)}
					</div>
				))}
			</div>
		</div>
	);
};

const useRegisters = (computer: Computer): Registers => {
	const subscribe = useCallback(
		(cb: (registers: Registers) => void) => computer.subscribe_registers(cb),
		[computer],
	);
	return useSyncExternalStore(subscribe, () => computer.registers());
};

const RegisterView: React.FC<{
	registers: Registers;
	labels: Map<number, string[]>;
}> = ({ registers, labels }) => {
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
						<CellView cell={registers.a} labels={labels} />
					</TableCell>
				</TableRow>
				<TableRow>
					<TableCell>%b</TableCell>
					<TableCell>
						<CellView cell={registers.b} labels={labels} />
					</TableCell>
				</TableRow>
				<TableRow>
					<TableCell>%pc</TableCell>
					<TableCell>
						<Word word={registers.pc} labels={labels} />
					</TableCell>
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

export const Section: React.FC<React.ComponentProps<"section">> = ({
	className,
	children,
	...props
}) => (
	<section className={cn("flex flex-col gap-4", className)} {...props}>
		{children}
	</section>
);

export const Title: React.FC<React.ComponentProps<"h1">> = ({
	className,
	children,
	...props
}) => (
	<h1 className={cn("text-center text-xl font-bold", className)} {...props}>
		{children}
	</h1>
);

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
		<div className="flex justify-center gap-4 p-4 w-screen h-screen">
			<Section className="w-64">
				<Title>Controls</Title>
				<div className="border p-4 rounded">Cycles: {computer.cycles}</div>
				<div className="flex-1">
					<RegisterView registers={registers} labels={labels} />
				</div>
				<StepForm onStep={onStep} />
			</Section>

			<Section className="flex-1">
				<Title>Program counter</Title>
				<MemoryViewer
					computer={computer}
					highlight={registers.pc}
					labels={labels}
				/>
			</Section>

			<Section className="flex-1">
				<Title>Stack pointer</Title>
				<MemoryViewer
					computer={computer}
					highlight={registers.sp}
					labels={labels}
				/>
			</Section>

			<Section className="flex-1">
				<Title>Memory view</Title>
				<MemoryViewer
					computer={computer}
					highlight={viewAddress}
					labels={labels}
				/>

				<div className="flex flex-col gap-2 border p-4 rounded">
					<h4 className="text-sm font-medium">View at label:</h4>

					{Array.from(computer.labels).map(([label, address]) => (
						<Button
							variant="secondary"
							size="sm"
							key={label}
							onClick={() => setViewAddress(address)}
						>
							{label} ({address})
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
			</Section>
		</div>
	);
};
