import { XIcon } from "lucide-react";
import { memo } from "react";
import type { SourceMap } from "z33-web-bindings";
import { Button } from "./components/ui/button";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "./components/ui/table";
import { CellView, type Labels, Word, useRegisters } from "./computer";
import type { ComputerInterface } from "./computer";
import { useAppStore } from "./stores/app-store";

const SourceLocationSection: React.FC<{
  computer: ComputerInterface;
  sourceMap: SourceMap;
}> = ({ computer, sourceMap }) => {
  const registers = useRegisters(computer);
  const location = sourceMap.get(registers.pc);

  return (
    <div className="px-3 py-2">
      <h3 className="text-xs font-semibold uppercase tracking-wide text-muted-foreground mb-1">
        Source
      </h3>
      {location ? (
        <div className="text-sm font-mono">
          <div className="font-semibold">
            {location.file.replace(/^\//, "")}
          </div>
          <div className="text-muted-foreground">
            bytes {location.span[0]}..{location.span[1]}
          </div>
        </div>
      ) : (
        <div className="text-sm text-muted-foreground">
          No source for PC={registers.pc}
        </div>
      )}
    </div>
  );
};

const RegisterSection: React.FC<{
  computer: ComputerInterface;
  labels: Labels;
}> = ({ computer, labels }) => {
  const registers = useRegisters(computer);

  return (
    <div className="px-3 py-2">
      <h3 className="text-xs font-semibold uppercase tracking-wide text-muted-foreground mb-1">
        Registers
      </h3>
      <Table>
        <TableHeader>
          <TableRow>
            <TableHead className="w-12 py-1">Reg</TableHead>
            <TableHead className="py-1">Value</TableHead>
          </TableRow>
        </TableHeader>
        <TableBody className="font-mono text-sm">
          <TableRow>
            <TableCell className="py-1">%a</TableCell>
            <TableCell className="py-1">
              <CellView cell={registers.a} labels={labels} />
            </TableCell>
          </TableRow>
          <TableRow>
            <TableCell className="py-1">%b</TableCell>
            <TableCell className="py-1">
              <CellView cell={registers.b} labels={labels} />
            </TableCell>
          </TableRow>
          <TableRow>
            <TableCell className="py-1">%pc</TableCell>
            <TableCell className="py-1">
              <Word word={registers.pc} labels={labels} />
            </TableCell>
          </TableRow>
          <TableRow>
            <TableCell className="py-1">%sp</TableCell>
            <TableCell className="py-1">{registers.sp}</TableCell>
          </TableRow>
          <TableRow>
            <TableCell className="py-1">%sr</TableCell>
            <TableCell className="py-1">{registers.sr}</TableCell>
          </TableRow>
        </TableBody>
      </Table>
    </div>
  );
};

const LabelSection: React.FC<{
  computer: ComputerInterface;
}> = ({ computer }) => (
  <div className="px-3 py-2">
    <h3 className="text-xs font-semibold uppercase tracking-wide text-muted-foreground mb-1">
      Labels
    </h3>
    <div className="flex flex-col gap-0.5 font-mono text-sm">
      {Array.from(computer.labels).map(([label, address]) => (
        <div key={label} className="flex justify-between">
          <span>{label}</span>
          <span className="text-muted-foreground">{address}</span>
        </div>
      ))}
    </div>
  </div>
);

export const DebugSidebar: React.FC<{ onClose: () => void }> = memo(
  ({ onClose }) => {
    const mode = useAppStore((s) => s.mode);

    if (mode.type !== "debug") return null;
    const { computer, sourceMap, labels } = mode;

    return (
      <div className="flex flex-col h-full overflow-auto border-l border-border">
        <div className="flex items-center justify-between px-3 py-1 border-b border-border bg-muted/30">
          <span className="text-xs font-semibold uppercase tracking-wide text-muted-foreground">
            Debug
          </span>
          <Button variant="ghost" size="icon-xs" onClick={onClose}>
            <XIcon />
          </Button>
        </div>

        <div className="flex-1 overflow-auto divide-y divide-border">
          <SourceLocationSection computer={computer} sourceMap={sourceMap} />
          <RegisterSection computer={computer} labels={labels} />
          <LabelSection computer={computer} />
        </div>
      </div>
    );
  },
);
DebugSidebar.displayName = "DebugSidebar";
