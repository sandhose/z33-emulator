import { memo } from "react";
import { Table, TableBody, TableCell, TableRow } from "./components/ui/table";
import type { ComputerInterface } from "./computer";
import { CellView, type Labels, useRegisters, Word } from "./computer";

export const RegisterPanel: React.FC<{
  computer: ComputerInterface;
  labels: Labels;
}> = memo(({ computer, labels }) => {
  const registers = useRegisters(computer);

  return (
    <div className="flex flex-col h-full overflow-hidden">
      <div className="bg-muted/30 border-b px-2 py-1 text-xs font-semibold uppercase tracking-wide text-muted-foreground">
        Registers
      </div>
      <div className="px-2 py-1 overflow-auto flex-1">
        <Table>
          <TableBody className="font-mono text-xs">
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
    </div>
  );
});
RegisterPanel.displayName = "RegisterPanel";
