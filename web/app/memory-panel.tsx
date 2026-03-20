import { memo, useCallback, useMemo, useRef } from "react";
import {
  ADDRESS_WIDTH,
  type ComputerInterface,
  type Following,
  formatAddress,
  type Labels,
  type Pointers,
  type RegisterId,
} from "./computer-types";
import { CellView, MemoryViewer, type MemoryViewerRef } from "./computer";
import { useMemoryCell, useRegisters } from "./hooks/use-computer";
import { cn } from "./lib/utils";
import { useDisplayStore } from "./stores/display-store";

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
  isFollowing: boolean;
  onClick: () => void;
}> = ({ name, address, computer, labels, isFollowing, onClick }) => {
  const cell = useMemoryCell(computer, address);
  const displayFormat = useDisplayStore((s) => s.format);
  return (
    <button
      type="button"
      className={cn(
        "flex items-center gap-2 w-full px-2 py-0.5 text-left hover:bg-muted/50 cursor-pointer font-mono text-xs",
        isFollowing && "bg-muted/50",
      )}
      onClick={onClick}
    >
      <span
        className={cn(
          "text-muted-foreground shrink-0 text-right",
          ADDRESS_WIDTH[displayFormat],
        )}
      >
        {formatAddress(address, displayFormat)}
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
  following: Following | null;
  onLabelClick: (name: string, address: number) => void;
}> = ({ computer, labels, following, onLabelClick }) => {
  const sorted = useMemo(
    () => Array.from(computer.labels).sort(([, a], [, b]) => a - b),
    [computer.labels],
  );

  if (sorted.length === 0) return null;

  return (
    <div role="region" aria-label="Labels" className="border-b border-border">
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
            isFollowing={following === `label:${name}`}
            onClick={() => onLabelClick(name, address)}
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
      if (following?.startsWith("label:")) {
        const name = following.slice("label:".length);
        for (const [n, addr] of computer.labels) {
          if (n === name) return addr;
        }
      }
      return null;
    }, [following, registers, computer.labels]);

    // Build pointers map: address → list of registers pointing there
    const pointers = useMemo<Pointers>(() => {
      const map: Pointers = new Map();
      const add = (reg: RegisterId, addr: number) => {
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
      (name: string, address: number) => {
        const key: Following = `label:${name}`;
        if (following === key) {
          onFollow(null);
        } else {
          onFollow(key);
          viewerRef.current?.scrollTo(address);
        }
      },
      [following, onFollow],
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
            following={following}
            onLabelClick={handleLabelClick}
          />
        </div>
        <div
          role="region"
          aria-label="Memory"
          className="flex-1 overflow-hidden flex flex-col"
        >
          <div className="bg-muted/30 border-b px-2 py-1 text-xs font-semibold uppercase tracking-wide text-muted-foreground shrink-0">
            Memory
          </div>
          <MemoryViewer
            ref={viewerRef}
            computer={computer}
            highlight={highlight}
            labels={labels}
            pointers={pointers}
            {...(following === null ? {} : { onUserScroll: handleUserScroll })}
          />
        </div>
      </div>
    );
  },
);
MemoryPanel.displayName = "MemoryPanel";
