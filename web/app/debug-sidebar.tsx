import { memo } from "react";
import type { ComputerInterface } from "./computer";
import {
  CellView,
  type Following,
  type Labels,
  RegisterDot,
  useRegisters,
  Word,
} from "./computer";

export const RegisterPanel: React.FC<{
  computer: ComputerInterface;
  labels: Labels;
  following: Following | null;
  onFollow: (reg: Following | null) => void;
}> = memo(({ computer, labels, following, onFollow }) => {
  const registers = useRegisters(computer);

  const makeClickHandler = (reg: Following) => () => {
    onFollow(following === reg ? null : reg);
  };

  return (
    <div className="font-mono text-xs">
      <div className="bg-muted/30 px-2 py-1 text-xs font-semibold uppercase tracking-wide text-muted-foreground font-sans">
        Registers
      </div>
      {(["a", "b"] as const).map((reg) => {
        const key = `%${reg}` as Following;
        const isFollowing = following === key;
        return (
          <button
            key={reg}
            type="button"
            className="flex items-center gap-2 w-full px-2 py-0.5 text-left hover:bg-muted/50 cursor-pointer select-none"
            onClick={makeClickHandler(key)}
          >
            <span className="w-8 shrink-0 text-muted-foreground">%{reg}</span>
            <span className="flex-1 min-w-0">
              <CellView cell={registers[reg]} labels={labels} />
            </span>
            {isFollowing && <RegisterDot register={key} />}
          </button>
        );
      })}
      {(["pc", "sp"] as const).map((reg) => {
        const key = `%${reg}` as Following;
        const isFollowing = following === key;
        return (
          <button
            key={reg}
            type="button"
            className="flex items-center gap-2 w-full px-2 py-0.5 text-left hover:bg-muted/50 cursor-pointer select-none"
            onClick={makeClickHandler(key)}
          >
            <span className="w-8 shrink-0 text-muted-foreground">%{reg}</span>
            <span className="flex-1 min-w-0">
              {reg === "pc" ? (
                <Word word={registers.pc} labels={labels} />
              ) : (
                registers.sp
              )}
            </span>
            {isFollowing && <RegisterDot register={key} />}
          </button>
        );
      })}
      <div className="flex items-center gap-2 w-full px-2 py-0.5">
        <span className="w-8 shrink-0 text-muted-foreground">%sr</span>
        <span className="flex-1 min-w-0">{registers.sr}</span>
      </div>
    </div>
  );
});
RegisterPanel.displayName = "RegisterPanel";
