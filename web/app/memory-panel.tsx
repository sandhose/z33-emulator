import { memo, useCallback, useMemo, useRef } from "react";
import {
  CellView,
  type ComputerInterface,
  type Following,
  type Labels,
  MemoryViewer,
  type MemoryViewerRef,
  type Pointers,
  useMemoryCell,
  useRegisters,
} from "./computer";
import { cn } from "./lib/utils";

type MemoryPanelProps = {
  computer: ComputerInterface;
  labels: Labels;
  following: Following | null;
  onFollow: (reg: Following | null) => void;
  className?: string;
};

/** A single label row showing name, address, and live memory cell value */
const LabelRow: React.FC<{
  name: string;
  address: number;
  computer: ComputerInterface;
  labels: Labels;
  onClick: () => void;
}> = ({ name, address, computer, labels, onClick }) => {
  const cell = useMemoryCell(computer, address);
  return (
    <button
      type="button"
      className="flex items-center gap-2 w-full px-2 py-0.5 text-left hover:bg-muted/50 cursor-pointer font-mono text-xs"
      onClick={onClick}
    >
      <span className="text-muted-foreground w-10 shrink-0 text-right">
        {address}
      </span>
      <span className="text-muted-foreground shrink-0">{name}</span>
      <span className="text-foreground/80 ml-auto shrink-0">
        <CellView cell={cell} labels={labels} />
      </span>
    </button>
  );
};

const LabelList: React.FC<{
  computer: ComputerInterface;
  labels: Labels;
  onLabelClick: (address: number) => void;
}> = ({ computer, labels, onLabelClick }) => {
  const sorted = useMemo(
    () => Array.from(computer.labels).sort(([, a], [, b]) => a - b),
    [computer.labels],
  );

  if (sorted.length === 0) return null;

  return (
    <div className="border-b border-border">
      <div className="bg-muted/30 px-2 py-1 text-xs font-semibold uppercase tracking-wide text-muted-foreground">
        Labels
      </div>
      <div className="max-h-32 overflow-y-auto">
        {sorted.map(([name, address]) => (
          <LabelRow
            key={name}
            name={name}
            address={address}
            computer={computer}
            labels={labels}
            onClick={() => onLabelClick(address)}
          />
        ))}
      </div>
    </div>
  );
};

export const MemoryPanel: React.FC<MemoryPanelProps> = memo(
  ({ computer, labels, following, onFollow, className }) => {
    const viewerRef = useRef<MemoryViewerRef>(null);
    const registers = useRegisters(computer);

    // Derive the address to follow in the viewer (null = not following, no highlight)
    const highlight = useMemo((): number | null => {
      if (following === "%pc") return registers.pc;
      if (following === "%sp") return registers.sp;
      if (following === "%a" && registers.a.type === "word")
        return registers.a.word;
      if (following === "%b" && registers.b.type === "word")
        return registers.b.word;
      return null;
    }, [following, registers]);

    // Build pointers map: address → list of registers pointing there
    const pointers = useMemo<Pointers>(() => {
      const map: Pointers = new Map();
      const add = (reg: Following, addr: number) => {
        const existing = map.get(addr) ?? [];
        map.set(addr, [...existing, reg]);
      };
      add("%pc", registers.pc);
      add("%sp", registers.sp);
      if (registers.a.type === "word") add("%a", registers.a.word);
      if (registers.b.type === "word") add("%b", registers.b.word);
      return map;
    }, [registers.pc, registers.sp, registers.a, registers.b]);

    const handleLabelClick = useCallback(
      (address: number) => {
        onFollow(null);
        viewerRef.current?.scrollTo(address);
      },
      [onFollow],
    );

    const handleUserScroll = useCallback(() => {
      onFollow(null);
    }, [onFollow]);

    return (
      <div className={cn("flex flex-col h-full", className)}>
        <div className="shrink-0">
          <LabelList
            computer={computer}
            labels={labels}
            onLabelClick={handleLabelClick}
          />
        </div>
        <div className="flex-1 overflow-hidden flex flex-col">
          <div className="bg-muted/30 border-b px-2 py-1 text-xs font-semibold uppercase tracking-wide text-muted-foreground shrink-0">
            Memory
          </div>
          <MemoryViewer
            ref={viewerRef}
            computer={computer}
            highlight={highlight}
            labels={labels}
            pointers={pointers}
            {...(following !== null ? { onUserScroll: handleUserScroll } : {})}
          />
        </div>
      </div>
    );
  },
);
MemoryPanel.displayName = "MemoryPanel";
