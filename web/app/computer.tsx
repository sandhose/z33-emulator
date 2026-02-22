import { useVirtualizer } from "@tanstack/react-virtual";
import type * as React from "react";
import {
  forwardRef,
  memo,
  useCallback,
  useDeferredValue,
  useEffect,
  useImperativeHandle,
  useRef,
  useSyncExternalStore,
} from "react";
import type { Cell, Computer, Cycles, Registers } from "z33-web-bindings";

const MEMORY_SIZE = 10_000;

export type Labels = Map<number, string[]>;

export const Word: React.FC<{ word: number; labels: Labels }> = ({
  word,
  labels,
}) => {
  const list = labels.get(word);
  if (list) {
    return <>{word} = {...list.map((l) => <Label key={l} label={l} />)}</>;
  }

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

export const CellView: React.FC<{ cell: Cell; labels: Labels }> = ({
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

export const useMemoryCell = (computer: Computer, address: number): Cell => {
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

export type MemoryViewerRef = {
  recenter: () => void;
};

type MemoryViewerProps = {
  computer: Computer;
  highlight: number;
  labels: Labels;
};

export const MemoryViewer = memo(
  forwardRef<MemoryViewerRef, MemoryViewerProps>(
    ({ computer, highlight, labels }, ref) => {
      const parentRef = useRef(null);

      const rowVirtualizer = useVirtualizer({
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
        <div className="overflow-auto h-full" ref={parentRef}>
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

export const useRegisters = (computer: Computer): Registers => {
  const subscribe = useCallback(
    (cb: (registers: Registers) => void) => computer.subscribe_registers(cb),
    [computer],
  );
  const registers = useSyncExternalStore(subscribe, () => computer.registers());
  return useDeferredValue(registers);
};

export const useCycles = (computer: Computer): Cycles => {
  const subscribe = useCallback(
    (cb: (cycles: Cycles) => void) => computer.subscribe_cycles(cb),
    [computer],
  );
  const cycles = useSyncExternalStore(subscribe, () => computer.cycles());
  return useDeferredValue(cycles);
};
