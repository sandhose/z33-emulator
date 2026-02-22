import { useVirtualizer } from "@tanstack/react-virtual";
import {
  CrosshairIcon,
  SkipBackIcon,
  SkipForwardIcon,
  StepBackIcon,
  StepForwardIcon,
} from "lucide-react";
import type * as React from "react";
import {
  forwardRef,
  memo,
  useCallback,
  useDeferredValue,
  useEffect,
  useImperativeHandle,
  useMemo,
  useRef,
  useState,
  useSyncExternalStore,
} from "react";
import type {
  Cell,
  Computer,
  Cycles,
  Registers,
  SourceMap,
} from "z33-web-bindings";
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
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "./components/ui/tooltip";
import { cn } from "./lib/utils";
import { StepForm } from "./step-form";

const MEMORY_SIZE = 10_000;

type Labels = Map<number, string[]>;

const Word: React.FC<{ word: number; labels: Labels }> = ({ word, labels }) => {
  const list = labels.get(word);
  if (list) {
    return <>{word} = {...list.map((l) => <Label key={l} label={l} />)}</>;
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
        {word} ={" "}
        {...nearest.map((l) => <Label key={l} label={`${l}+${distance}`} />)}
      </>
    );
  }

  return word;
};

const CellView: React.FC<{ cell: Cell; labels: Labels }> = ({
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
  const value = useSyncExternalStore(subscribe, () => computer.memory(address));
  return useDeferredValue(value);
};

const MemoryCell: React.FC<{
  computer: Computer;
  address: number;
  labels: Labels;
}> = ({ computer, address, labels }) => {
  const cell = useMemoryCell(computer, address);
  return <CellView cell={cell} labels={labels} />;
};

const Label: React.FC<{ label: string }> = ({ label }) => (
  <div className="px-2 inline-block bg-accent text-accent-foreground rounded text-xs">
    {label}
  </div>
);

type MemoryViewerProps = {
  computer: Computer;
  highlight: number;
  labels: Labels;
};

type MemoryViewerRef = {
  recenter: () => void;
};

const MemoryViewer = memo(
  forwardRef<MemoryViewerRef, MemoryViewerProps>(
    ({ computer, highlight, labels }, ref) => {
      const parentRef = useRef(null);

      const rowVirtualizer = useVirtualizer({
        // Render one more element to have the "TOP OF STACK" cell
        count: MEMORY_SIZE + 1,
        initialOffset: highlight * 32,
        getScrollElement: () => parentRef.current,
        estimateSize: () => 32,
      });

      const recenter = useCallback(() => {
        rowVirtualizer.scrollToIndex(normalize(highlight), { align: "center" });
      }, [rowVirtualizer, highlight]);

      useEffect(() => {
        recenter();
      }, [recenter]);

      useImperativeHandle(
        ref,
        () => ({
          recenter,
        }),
        [recenter],
      );

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
                      ?.map((l) => <Label key={l} label={l} />) || [])}
                  </>
                )}
              </div>
            ))}
          </div>
        </div>
      );
    },
  ),
);
MemoryViewer.displayName = "MemoryViewer";

const useRegisters = (computer: Computer): Registers => {
  const subscribe = useCallback(
    (cb: (registers: Registers) => void) => computer.subscribe_registers(cb),
    [computer],
  );
  const registers = useSyncExternalStore(subscribe, () => computer.registers());
  return useDeferredValue(registers);
};

const useCycles = (computer: Computer): Cycles => {
  const subscribe = useCallback(
    (cb: (cycles: Cycles) => void) => computer.subscribe_cycles(cb),
    [computer],
  );
  const cycles = useSyncExternalStore(subscribe, () => computer.cycles());
  console.log(cycles);
  return useDeferredValue(cycles);
};

const RegisterView: React.FC<{
  computer: Computer;
  labels: Labels;
}> = ({ computer, labels }) => {
  const registers = useRegisters(computer);
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

export const CyclesView: React.FC<{
  computer: Computer;
  className?: string;
}> = ({ computer, className }) => {
  const cycles = useCycles(computer);
  return <div className={className}>Cycles: {cycles}</div>;
};

const SourceLocationView: React.FC<{
  computer: Computer;
  sourceMap: SourceMap;
}> = ({ computer, sourceMap }) => {
  const registers = useRegisters(computer);
  const location = sourceMap.get(registers.pc);
  if (!location) {
    return (
      <div className="border p-4 rounded text-sm text-muted-foreground">
        No source location for address {registers.pc}
      </div>
    );
  }

  return (
    <div className="border p-4 rounded text-sm font-mono">
      <div className="font-semibold">{location.file}</div>
      <div className="text-muted-foreground">
        bytes {location.span[0]}..{location.span[1]}
      </div>
    </div>
  );
};

export const ControlSection: React.FC<{
  className?: string;
  computer: Computer;
  labels: Labels;
  sourceMap: SourceMap;
}> = memo(({ className, computer, labels, sourceMap }) => {
  const onStep = useCallback(() => computer.step(), [computer]);
  return (
    <Section className={className}>
      <Title>Controls</Title>
      <CyclesView computer={computer} className="border p-4 rounded" />
      <SourceLocationView computer={computer} sourceMap={sourceMap} />
      <div className="flex-1">
        <RegisterView computer={computer} labels={labels} />
      </div>
      <StepForm onStep={onStep} />
    </Section>
  );
});
ControlSection.displayName = "ControlSection";

export const ProgramCounterSection: React.FC<{
  className?: string;
  computer: Computer;
  labels: Labels;
}> = memo(({ className, computer, labels }) => {
  const ref = useRef<MemoryViewerRef>(null);
  const registers = useRegisters(computer);
  const programCounter = registers.pc;
  return (
    <Section className={className}>
      <div className="flex justify-between items-center">
        <Title>Program counter</Title>
        <Tooltip>
          <TooltipTrigger
            render={
              <Button
                variant="secondary"
                size="icon"
                onClick={() => ref.current?.recenter()}
              >
                <CrosshairIcon />
              </Button>
            }
          />
          <TooltipContent>Recenter</TooltipContent>
        </Tooltip>
      </div>

      <MemoryViewer
        ref={ref}
        computer={computer}
        highlight={programCounter}
        labels={labels}
      />
    </Section>
  );
});
ProgramCounterSection.displayName = "ProgramCounterSection";

export const StackPointerSection: React.FC<{
  className?: string;
  computer: Computer;
  labels: Labels;
}> = memo(({ className, computer, labels }) => {
  const ref = useRef<MemoryViewerRef>(null);
  const registers = useRegisters(computer);
  const stackPointer = registers.sp;
  return (
    <Section className={className}>
      <div className="flex justify-between items-center">
        <Title>Stack pointer</Title>
        <Tooltip>
          <TooltipTrigger
            render={
              <Button
                variant="secondary"
                size="icon"
                onClick={() => ref.current?.recenter()}
              >
                <CrosshairIcon />
              </Button>
            }
          />
          <TooltipContent>Recenter</TooltipContent>
        </Tooltip>
      </div>

      <MemoryViewer
        computer={computer}
        highlight={stackPointer}
        labels={labels}
      />
    </Section>
  );
});
StackPointerSection.displayName = "StackPointerSection";

export const MemoryViewSection: React.FC<{
  className?: string;
  computer: Computer;
  labels: Labels;
}> = memo(({ className, computer, labels }) => {
  const [viewAddress, setViewAddress] = useState(1000);
  const minus10 = useCallback(() => setViewAddress((addr) => addr - 10), []);
  const minus1 = useCallback(() => setViewAddress((addr) => addr - 1), []);
  const plus1 = useCallback(() => setViewAddress((addr) => addr + 1), []);
  const plus10 = useCallback(() => setViewAddress((addr) => addr + 10), []);

  return (
    <Section className={className}>
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
            onClick={minus10}
          >
            <SkipBackIcon />
          </Button>
          <Button
            variant="outline"
            className="aspect-square"
            size="icon"
            onClick={minus1}
          >
            <StepBackIcon />
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
            onClick={plus1}
          >
            <StepForwardIcon />
          </Button>
          <Button
            variant="outline"
            className="aspect-square"
            size="icon"
            onClick={plus10}
          >
            <SkipForwardIcon />
          </Button>
        </div>
      </div>
    </Section>
  );
});
MemoryViewSection.displayName = "MemoryViewSection";

export const ComputerView: React.FC<{ computer: Computer }> = ({
  computer,
}) => {
  const labels = useMemo(() => {
    const labels: Labels = new Map();
    for (const [label, address] of computer.labels) {
      const values = labels.get(address) || [];
      labels.set(address, [...values, label]);
    }
    return labels;
  }, [computer]);

  const sourceMap = useMemo(() => computer.source_map, [computer]);

  return (
    <div className="flex justify-center gap-4 p-4 w-screen h-screen">
      <ControlSection
        className="w-96"
        computer={computer}
        labels={labels}
        sourceMap={sourceMap}
      />

      <ProgramCounterSection
        className="flex-1"
        computer={computer}
        labels={labels}
      />

      <StackPointerSection
        className="flex-1"
        computer={computer}
        labels={labels}
      />

      <MemoryViewSection
        className="flex-1"
        computer={computer}
        labels={labels}
      />
    </div>
  );
};
