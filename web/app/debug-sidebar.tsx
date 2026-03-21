import { memo } from "react";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "./components/ui/tooltip";
import {
  type ComputerInterface,
  type Following,
  formatWord,
  type Labels,
  type RegisterId,
} from "./computer-types";
import { CellView, RegisterDot, Word } from "./computer";
import { useRegisters } from "./hooks/use-computer";
import { FormatSwitcher } from "./format-switcher";
import { SectionHeader } from "./section-header";
import { useDisplayStore } from "./stores/display-store";

// Listed MSB-first (rendered left-to-right)
const SR_SYSTEM_FLAGS = [
  { label: "S", fullName: "Supervisor", mask: 0x200 },
  { label: "I", fullName: "Interrupt Enable", mask: 0x100 },
] as const;

const SR_FLAGS = [
  { label: "V", fullName: "Overflow", mask: 0x008 },
  { label: "N", fullName: "Negative", mask: 0x004 },
  { label: "Z", fullName: "Zero", mask: 0x002 },
  { label: "C", fullName: "Carry", mask: 0x001 },
] as const;

const REGISTER_DESCRIPTIONS = {
  a: "General-purpose register",
  b: "General-purpose register",
  pc: "Program counter",
  sp: "Stack pointer",
  sr: "Status register (flags)",
} as const;

const FlagBit: React.FC<{
  label: string;
  fullName: string;
  active: boolean;
}> = ({ label, fullName, active }) => (
  <Tooltip>
    <TooltipTrigger
      className={`inline-flex items-center justify-center rounded px-1 py-0 text-[10px] font-bold leading-4 select-none ${
        active
          ? "bg-foreground text-background"
          : "bg-muted text-muted-foreground opacity-50"
      }`}
    >
      {label}
    </TooltipTrigger>
    <TooltipContent side="bottom">{fullName}</TooltipContent>
  </Tooltip>
);

const StatusRegisterView: React.FC<{ sr: number }> = ({ sr }) => {
  const format = useDisplayStore((s) => s.format);
  return (
    <span className="flex flex-1 min-w-0 items-center gap-1.5">
      <span className="flex gap-0.5">
        {SR_SYSTEM_FLAGS.map((flag) => (
          <FlagBit
            key={flag.label}
            label={flag.label}
            fullName={flag.fullName}
            active={Boolean(sr & flag.mask)}
          />
        ))}
      </span>
      <span className="text-muted-foreground text-[10px] leading-4 select-none">
        …
      </span>
      <span className="flex gap-0.5">
        {SR_FLAGS.map((flag) => (
          <FlagBit
            key={flag.label}
            label={flag.label}
            fullName={flag.fullName}
            active={Boolean(sr & flag.mask)}
          />
        ))}
      </span>
      <span className="text-muted-foreground">{formatWord(sr, format)}</span>
    </span>
  );
};

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
    <div role="region" aria-label="Registers" className="font-mono text-xs">
      <SectionHeader className="font-sans flex items-center justify-between">
        Registers
        <FormatSwitcher />
      </SectionHeader>
      {(["a", "b"] as const).map((reg) => {
        const key = `%${reg}` as RegisterId;
        const isFollowing = following === key;
        return (
          <button
            key={reg}
            type="button"
            aria-label={`Register %${reg}`}
            className="flex items-center gap-2 w-full px-2 py-0.5 text-left hover:bg-muted/50 cursor-pointer select-none"
            onClick={makeClickHandler(key)}
          >
            <Tooltip>
              <TooltipTrigger
                render={<span />}
                className="w-8 shrink-0 text-muted-foreground text-left"
              >
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
            aria-label={`Register %${reg}`}
            className="flex items-center gap-2 w-full px-2 py-0.5 text-left hover:bg-muted/50 cursor-pointer select-none"
            onClick={makeClickHandler(key)}
          >
            <Tooltip>
              <TooltipTrigger
                render={<span />}
                className="w-8 shrink-0 text-muted-foreground text-left"
              >
                %{reg}
              </TooltipTrigger>
              <TooltipContent side="right">
                {REGISTER_DESCRIPTIONS[reg]}
              </TooltipContent>
            </Tooltip>
            <span className="flex-1 min-w-0">
              <Word word={registers[reg]} labels={labels} />
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
