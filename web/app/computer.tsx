import { useDebouncer } from "@tanstack/react-pacer";
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
import type { Cell, Cycles, Registers, SourceMap } from "z33-web-bindings";

const MEMORY_SIZE = 10_000;
const ROW_HEIGHT = 28;

export type Labels = Map<number, string[]>;

/** Narrow type for actual CPU registers */
export type RegisterId = "%pc" | "%sp" | "%a" | "%b";

/** Anything that can be followed in the memory viewer: a register or a label */
export type Following = RegisterId | `label:${string}`;

/** Interface satisfied by the WASM Computer class, and future worker proxies */
export interface ComputerInterface {
  step(): boolean;
  registers(): Registers;
  memory(address: number): Cell;
  cycles(): Cycles;
  subscribe_registers(cb: (r: Registers) => void): () => void;
  subscribe_memory(address: number, cb: (c: Cell) => void): () => void;
  subscribe_cycles(cb: (c: Cycles) => void): () => void;
  readonly source_map: SourceMap;
  readonly labels: Iterable<[string, number]>;
}

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
  Math.max(0, Math.min(value, MEMORY_SIZE));

export const useMemoryCell = (
  computer: ComputerInterface,
  address: number,
): Cell => {
  const subscribe = useCallback(
    (cb: (cell: Cell) => void) => computer.subscribe_memory(address, cb),
    [computer, address],
  );
  const value = useSyncExternalStore(subscribe, () => computer.memory(address));
  return useDeferredValue(value);
};

const MemoryCell: React.FC<{
  computer: ComputerInterface;
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

const RegisterBadge: React.FC<{ register: RegisterId }> = ({ register }) => {
  switch (register) {
    case "%pc":
      return (
        <div className="px-1.5 inline-block rounded text-xs font-semibold bg-blue-500 text-white">
          %pc
        </div>
      );
    case "%sp":
      return (
        <div className="px-1.5 inline-block rounded text-xs font-semibold bg-emerald-500 text-white">
          %sp
        </div>
      );
    case "%a":
      return (
        <div className="px-1.5 inline-block rounded text-xs font-semibold bg-amber-500 text-white">
          %a
        </div>
      );
    case "%b":
      return (
        <div className="px-1.5 inline-block rounded text-xs font-semibold bg-violet-500 text-white">
          %b
        </div>
      );
  }
};

export const RegisterDot: React.FC<{ register: RegisterId }> = ({
  register,
}) => {
  switch (register) {
    case "%pc":
      return (
        <span className="inline-block w-2 h-2 rounded-full shrink-0 bg-blue-500" />
      );
    case "%sp":
      return (
        <span className="inline-block w-2 h-2 rounded-full shrink-0 bg-emerald-500" />
      );
    case "%a":
      return (
        <span className="inline-block w-2 h-2 rounded-full shrink-0 bg-amber-500" />
      );
    case "%b":
      return (
        <span className="inline-block w-2 h-2 rounded-full shrink-0 bg-violet-500" />
      );
  }
};

export type MemoryViewerRef = {
  recenter: () => void;
  scrollTo: (address: number) => void;
};

/** Map from address to list of registers pointing there */
export type Pointers = Map<number, RegisterId[]>;

type MemoryViewerProps = {
  computer: ComputerInterface;
  highlight: number | null;
  labels: Labels;
  /** Registers pointing to specific addresses, shown as colored badges */
  pointers?: Pointers;
  /** Called when user scrolls highlight address out of the visible range */
  onUserScroll?: () => void;
};

export const MemoryViewer = memo(
  forwardRef<MemoryViewerRef, MemoryViewerProps>(
    ({ computer, highlight, labels, pointers, onUserScroll }, ref) => {
      const parentRef = useRef<HTMLDivElement>(null);
      const onUserScrollRef = useRef(onUserScroll);
      const highlightRef = useRef(highlight);

      useEffect(() => {
        onUserScrollRef.current = onUserScroll;
      }, [onUserScroll]);

      useEffect(() => {
        highlightRef.current = highlight;
      }, [highlight]);

      const rowVirtualizer = useVirtualizer({
        count: MEMORY_SIZE + 1,
        initialOffset: (highlight ?? 0) * ROW_HEIGHT,
        getScrollElement: () => parentRef.current,
        estimateSize: () => ROW_HEIGHT,
      });

      const recenter = useCallback(() => {
        if (highlight === null) return;
        rowVirtualizer.scrollToIndex(normalize(highlight), { align: "center" });
      }, [rowVirtualizer, highlight]);

      const scrollTo = useCallback(
        (address: number) => {
          rowVirtualizer.scrollToIndex(normalize(address), { align: "center" });
        },
        [rowVirtualizer],
      );

      // When highlight changes (e.g. followed register moves), scroll to keep it in view
      // with a buffer of rows above/below so it doesn't land flush at the edge.
      useEffect(() => {
        if (highlight === null) return;
        const el = parentRef.current;
        if (!el) return;
        const BUFFER = 4;
        const visibleStart = Math.floor(el.scrollTop / ROW_HEIGHT);
        const visibleEnd = Math.floor(
          (el.scrollTop + el.clientHeight) / ROW_HEIGHT,
        );
        if (highlight < visibleStart + BUFFER) {
          rowVirtualizer.scrollToIndex(normalize(highlight - BUFFER), {
            align: "start",
          });
        } else if (highlight > visibleEnd - BUFFER) {
          rowVirtualizer.scrollToIndex(normalize(highlight + BUFFER), {
            align: "end",
          });
        }
      }, [rowVirtualizer, highlight]);

      // Detect when user manually scrolls highlight out of view and cancel follow mode.
      // Uses refs for highlight and callback so the debounced fn is stable ([] deps).
      const scrollDebouncer = useDebouncer(
        () => {
          const el = parentRef.current;
          if (!el || !onUserScrollRef.current) return;
          const BUFFER = 3;
          const h = highlightRef.current;
          if (h === null) return;
          const visibleStart = Math.floor(el.scrollTop / ROW_HEIGHT);
          const visibleEnd = Math.floor(
            (el.scrollTop + el.clientHeight) / ROW_HEIGHT,
          );
          if (h < visibleStart - BUFFER || h > visibleEnd + BUFFER) {
            onUserScrollRef.current();
          }
        },
        { wait: 150 },
      );

      useEffect(() => {
        const el = parentRef.current;
        if (!el) return;
        el.addEventListener("scroll", scrollDebouncer.maybeExecute, {
          passive: true,
        });
        return () => {
          el.removeEventListener("scroll", scrollDebouncer.maybeExecute);
          scrollDebouncer.cancel();
        };
      }, [scrollDebouncer.maybeExecute, scrollDebouncer.cancel]);

      useImperativeHandle(
        ref,
        () => ({
          recenter,
          scrollTo,
        }),
        [recenter, scrollTo],
      );

      return (
        <div className="overflow-auto flex-1 min-h-0" ref={parentRef}>
          <div
            className="font-mono text-xs w-full relative"
            style={{ height: `${rowVirtualizer.getTotalSize()}px` }}
          >
            {rowVirtualizer.getVirtualItems().map((virtualItem) => {
              const rowPointers = pointers?.get(virtualItem.index);
              return (
                <div
                  className="flex px-1.5 py-1 gap-1.5 border-b border-b-muted items-center data-[state=selected]:bg-muted"
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
                    highlight !== null && virtualItem.index === highlight
                      ? "selected"
                      : undefined
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
                      {labels.get(virtualItem.index)?.map((l) => (
                        <Label key={l} label={l} />
                      ))}
                      {rowPointers?.map((reg) => (
                        <RegisterBadge key={reg} register={reg} />
                      ))}
                    </>
                  )}
                </div>
              );
            })}
          </div>
        </div>
      );
    },
  ),
);
MemoryViewer.displayName = "MemoryViewer";

export const useRegisters = (computer: ComputerInterface): Registers => {
  const subscribe = useCallback(
    (cb: (registers: Registers) => void) => computer.subscribe_registers(cb),
    [computer],
  );
  const registers = useSyncExternalStore(subscribe, () => computer.registers());
  return useDeferredValue(registers);
};

export const useCycles = (computer: ComputerInterface): Cycles => {
  const subscribe = useCallback(
    (cb: (cycles: Cycles) => void) => computer.subscribe_cycles(cb),
    [computer],
  );
  const cycles = useSyncExternalStore(subscribe, () => computer.cycles());
  return useDeferredValue(cycles);
};
