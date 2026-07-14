import {
  GaugeIcon,
  PauseIcon,
  PencilIcon,
  PlayIcon,
  StepForwardIcon,
} from "lucide-react";
import { memo, useEffect } from "react";
import { Badge } from "./components/ui/badge";
import { Button } from "./components/ui/button";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "./components/ui/select";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "./components/ui/tooltip";
import type { ComputerInterface, ExecutionControls } from "./computer-types";
import { DocsButton } from "./docs-button";
import { useCycles } from "./hooks/use-computer";
import { useStepRunner } from "./hooks/use-step-runner";
import { cn } from "./lib/utils";
import { useAppStore } from "./stores/app-store";
import { SPEED_OPTIONS, useSpeedStore } from "./stores/speed-store";
import { ThemeSwitcher } from "./theme-switcher";

/** Stable select key for a speed value ("max" or the number in cycles/s). */
const speedKey = (speed: number | null): string =>
  speed === null ? "max" : String(speed);

/** Select `items` map: stable key -> human label (rendered by SelectValue). */
const SPEED_ITEMS = Object.fromEntries(
  SPEED_OPTIONS.map((o) => [speedKey(o.speed), o.label]),
);

type DebugToolbarProps = {
  touchedFiles: string[];
  activeFile: string;
  onFileChange: (name: string) => void;
  onStop: () => void;
};

export const DebugToolbar: React.FC<DebugToolbarProps> = memo(
  ({ touchedFiles, activeFile, onFileChange, onStop }) => {
    const mode = useAppStore((s) => s.mode);

    if (mode.type !== "debug") return null;
    const { computer } = mode;

    return (
      <DebugToolbarInner
        computer={computer}
        touchedFiles={touchedFiles}
        activeFile={activeFile}
        onFileChange={onFileChange}
        onStop={onStop}
      />
    );
  },
);
DebugToolbar.displayName = "DebugToolbar";

const DebugToolbarInner: React.FC<{
  computer: ComputerInterface & ExecutionControls;
  touchedFiles: string[];
  activeFile: string;
  onFileChange: (name: string) => void;
  onStop: () => void;
}> = ({ computer, touchedFiles, activeFile, onFileChange, onStop }) => {
  const cycles = useCycles(computer);
  const { halt, panicked, running, stepOnce, run, pause } =
    useStepRunner(computer);
  const speed = useSpeedStore((s) => s.speed);
  const setSpeed = useSpeedStore((s) => s.setSpeed);

  // Seed the worker on session start and apply live speed changes mid-run.
  useEffect(() => {
    computer.setSpeed(speed);
  }, [computer, speed]);

  const disabled = halt || panicked !== null;

  return (
    <div
      role="toolbar"
      aria-label="Debug"
      className={cn(
        "flex items-center gap-1 px-2 py-1 border-b border-border bg-muted/30",
      )}
    >
      <Button
        variant="ghost"
        size="xs"
        onClick={stepOnce}
        disabled={disabled || running}
      >
        <StepForwardIcon data-icon="inline-start" />
        Step
      </Button>

      {running ? (
        <Button variant="ghost" size="xs" onClick={pause}>
          <PauseIcon data-icon="inline-start" />
          Pause
        </Button>
      ) : (
        <Button variant="ghost" size="xs" onClick={run} disabled={disabled}>
          <PlayIcon data-icon="inline-start" />
          Run
        </Button>
      )}

      <Select
        value={speedKey(speed)}
        items={SPEED_ITEMS}
        onValueChange={(v) => {
          const option = SPEED_OPTIONS.find((o) => speedKey(o.speed) === v);
          if (option) setSpeed(option.speed);
        }}
      >
        <Tooltip>
          <TooltipTrigger
            render={<SelectTrigger size="xs" className="w-24 font-mono" />}
          >
            <GaugeIcon data-icon="inline-start" />
            <SelectValue />
          </TooltipTrigger>
          <TooltipContent side="bottom">
            Clock speed in cycles per second — instructions with memory operands
            take proportionally longer
          </TooltipContent>
        </Tooltip>
        <SelectContent align="start">
          {SPEED_OPTIONS.map((o) => (
            <SelectItem
              key={speedKey(o.speed)}
              value={speedKey(o.speed)}
              className="font-mono text-xs"
            >
              {o.label}
            </SelectItem>
          ))}
        </SelectContent>
      </Select>

      <div className="mx-2 h-4 w-px bg-border" />

      <Tooltip>
        <TooltipTrigger
          render={
            <span className="text-xs font-mono cursor-default select-none flex h-6 items-center gap-1 px-1" />
          }
        >
          <span className="text-muted-foreground">Cycles count</span>
          <span className="tabular-nums" aria-label="Cycle count">
            {cycles}
          </span>
        </TooltipTrigger>
        <TooltipContent side="bottom">
          <ul className="list-disc list-inside space-y-0.5">
            <li>1 cycle per instruction</li>
            <li>+1 per memory operand</li>
            <li>registers &amp; immediates are free</li>
          </ul>
        </TooltipContent>
      </Tooltip>

      {panicked && (
        <Badge
          role="alert"
          variant="destructive"
          className="ml-2 font-semibold"
        >
          Panicked: {panicked}
        </Badge>
      )}
      {halt && !panicked && (
        <Badge
          role="alert"
          variant="destructive"
          className="ml-2 font-semibold"
        >
          Halted
        </Badge>
      )}

      <div className="ml-auto flex items-center gap-1">
        {touchedFiles.length > 1 && (
          <Select
            value={activeFile}
            onValueChange={(v) => {
              if (v !== null) onFileChange(v);
            }}
          >
            <SelectTrigger size="xs" className="font-mono w-36">
              <SelectValue />
            </SelectTrigger>
            <SelectContent align="end">
              {touchedFiles.map((name) => (
                <SelectItem
                  key={name}
                  value={name}
                  className="font-mono text-xs"
                >
                  {name}
                </SelectItem>
              ))}
            </SelectContent>
          </Select>
        )}
        <Tooltip>
          <TooltipTrigger
            render={<Button variant="outline" size="xs" onClick={onStop} />}
          >
            <PencilIcon data-icon="inline-start" />
            Edit
          </TooltipTrigger>
          <TooltipContent side="bottom">
            Stop execution and return to the editor
          </TooltipContent>
        </Tooltip>
        <DocsButton />
        <ThemeSwitcher />
      </div>
    </div>
  );
};
