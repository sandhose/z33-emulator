import { memo } from "react";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "./components/ui/tooltip";
import type { ComputerInterface } from "./computer";
import {
  CellView,
  type Following,
  type Labels,
  RegisterDot,
  type RegisterId,
  useRegisters,
  Word,
} from "./computer";

const SR_FLAGS = [
  { label: "C", fullName: "Carry", mask: 0x001 },
  { label: "Z", fullName: "Zero", mask: 0x002 },
  { label: "N", fullName: "Negative", mask: 0x004 },
  { label: "V", fullName: "Overflow", mask: 0x008 },
] as const;

const SR_SYSTEM_FLAGS = [
  { label: "I", fullName: "Interrupt Enable", mask: 0x100 },
  { label: "S", fullName: "Supervisor", mask: 0x200 },
] as const;

const REGISTER_DESCRIPTIONS = {
  a: "General-purpose register",
  b: "General-purpose register",
  pc: "Program counter",
  sp: "Stack pointer",
  sr: "Status register (flags)",
} as const;

const StatusRegisterView: React.FC<{ sr: number }> = ({ sr }) => (
  <span className="flex flex-1 min-w-0 items-center gap-1.5">
    <span className="flex gap-0.5">
      {SR_FLAGS.map((flag) => (
        <Tooltip key={flag.label}>
          <TooltipTrigger
            className={`inline-flex items-center justify-center rounded px-1 py-0 text-[10px] font-bold leading-4 select-none ${
              sr & flag.mask
                ? "bg-foreground text-background"
                : "bg-muted text-muted-foreground opacity-50"
            }`}
          >
            {flag.label}
          </TooltipTrigger>
          <TooltipContent side="bottom">{flag.fullName}</TooltipContent>
        </Tooltip>
      ))}
    </span>
    <span className="flex gap-0.5">
      {SR_SYSTEM_FLAGS.map((flag) => (
        <Tooltip key={flag.label}>
          <TooltipTrigger
            className={`inline-flex items-center justify-center rounded px-1 py-0 text-[10px] font-bold leading-4 select-none ${
              sr & flag.mask
                ? "bg-foreground text-background"
                : "bg-muted text-muted-foreground opacity-50"
            }`}
          >
            {flag.label}
          </TooltipTrigger>
          <TooltipContent side="bottom">{flag.fullName}</TooltipContent>
        </Tooltip>
      ))}
    </span>
    <span className="text-muted-foreground">
      0x{sr.toString(16).toUpperCase().padStart(3, "0")}
    </span>
  </span>
);

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
        const key = `%${reg}` as RegisterId;
        const isFollowing = following === key;
        return (
          <button
            key={reg}
            type="button"
            className="flex items-center gap-2 w-full px-2 py-0.5 text-left hover:bg-muted/50 cursor-pointer select-none"
            onClick={makeClickHandler(key)}
          >
            <Tooltip>
              <TooltipTrigger className="w-8 shrink-0 text-muted-foreground text-left">
                %{reg}
              </TooltipTrigger>
              <TooltipContent side="right">
                {REGISTER_DESCRIPTIONS[reg]}
              </TooltipContent>
            </Tooltip>
            <span className="flex-1 min-w-0">
              <CellView cell={registers[reg]} labels={labels} />
            </span>
            {isFollowing && <RegisterDot register={key} />}
          </button>
        );
      })}
      {(["pc", "sp"] as const).map((reg) => {
        const key = `%${reg}` as RegisterId;
        const isFollowing = following === key;
        return (
          <button
            key={reg}
            type="button"
            className="flex items-center gap-2 w-full px-2 py-0.5 text-left hover:bg-muted/50 cursor-pointer select-none"
            onClick={makeClickHandler(key)}
          >
            <Tooltip>
              <TooltipTrigger className="w-8 shrink-0 text-muted-foreground text-left">
                %{reg}
              </TooltipTrigger>
              <TooltipContent side="right">
                {REGISTER_DESCRIPTIONS[reg]}
              </TooltipContent>
            </Tooltip>
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
        <Tooltip>
          <TooltipTrigger className="w-8 shrink-0 text-muted-foreground text-left">
            %sr
          </TooltipTrigger>
          <TooltipContent side="right">
            {REGISTER_DESCRIPTIONS.sr}
          </TooltipContent>
        </Tooltip>
        <StatusRegisterView sr={registers.sr} />
      </div>
    </div>
  );
});
RegisterPanel.displayName = "RegisterPanel";
