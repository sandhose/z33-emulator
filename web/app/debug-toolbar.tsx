import { PauseIcon, PlayIcon, SquareIcon, StepForwardIcon } from "lucide-react";
import { memo, startTransition, useCallback, useEffect, useState } from "react";
import { Button } from "./components/ui/button";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "./components/ui/select";
import type { ComputerInterface } from "./computer";
import { useCycles } from "./computer";
import { cn } from "./lib/utils";
import { useAppStore } from "./stores/app-store";
import { ThemeSwitcher } from "./theme-switcher";

function useStepRunner(computer: ComputerInterface) {
  const [halt, setHalt] = useState(false);
  const [panicked, setPanicked] = useState<string | null>(null);
  const [running, setRunning] = useState(false);
  const [stepsToRun, setStepsToRun] = useState(0);
  const [speed] = useState(20);

  const doStep = useCallback(() => {
    try {
      if (computer.step()) {
        setHalt(true);
      }
    } catch (error) {
      setPanicked(String(error));
    }
    setStepsToRun((prev) => prev - 1);
  }, [computer]);

  useEffect(() => {
    if (halt || panicked !== null) {
      setRunning(false);
      return;
    }
    if (running !== stepsToRun > 0) {
      setRunning(stepsToRun > 0);
    }
  }, [halt, running, panicked, stepsToRun]);

  useEffect(() => {
    if (!running) return;
    const id = setInterval(doStep, 1000 / speed);
    return () => clearInterval(id);
  }, [speed, running, doStep]);

  const stepOnce = useCallback(() => {
    startTransition(() => {
      setStepsToRun(1);
      doStep();
    });
  }, [doStep]);

  const runN = useCallback(
    (n: number) => {
      startTransition(() => {
        setStepsToRun(n);
        doStep();
      });
    },
    [doStep],
  );

  const stop = useCallback(() => {
    setStepsToRun(0);
  }, []);

  return { halt, panicked, running, stepOnce, runN, stop };
}

type DebugToolbarProps = {
  className?: string;
  touchedFiles: string[];
  activeFile: string;
  onFileChange: (name: string) => void;
  onStop: () => void;
};

export const DebugToolbar: React.FC<DebugToolbarProps> = memo(
  ({ className, touchedFiles, activeFile, onFileChange, onStop }) => {
    const mode = useAppStore((s) => s.mode);

    if (mode.type !== "debug") return null;
    const { computer } = mode;

    return (
      <DebugToolbarInner
        computer={computer}
        className={className}
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
  computer: ComputerInterface;
  className?: string | undefined;
  touchedFiles: string[];
  activeFile: string;
  onFileChange: (name: string) => void;
  onStop: () => void;
}> = ({
  computer,
  className,
  touchedFiles,
  activeFile,
  onFileChange,
  onStop,
}) => {
  const cycles = useCycles(computer);
  const { halt, panicked, running, stepOnce, runN, stop } =
    useStepRunner(computer);

  const disabled = halt || panicked !== null;

  return (
    <div
      className={cn(
        "flex items-center gap-1 px-2 py-1 border-b border-border bg-muted/30",
        className,
      )}
    >
      <Button
        variant="ghost"
        size="sm"
        onClick={stepOnce}
        disabled={disabled || running}
      >
        <StepForwardIcon className="mr-1 h-3.5 w-3.5" />
        Step
      </Button>

      {running ? (
        <Button variant="ghost" size="sm" onClick={stop}>
          <PauseIcon className="mr-1 h-3.5 w-3.5" />
          Pause
        </Button>
      ) : (
        <>
          <Button
            variant="ghost"
            size="sm"
            onClick={() => runN(10)}
            disabled={disabled}
          >
            <PlayIcon className="mr-1 h-3.5 w-3.5" />
            10
          </Button>
          <Button
            variant="ghost"
            size="sm"
            onClick={() => runN(100)}
            disabled={disabled}
          >
            <PlayIcon className="mr-1 h-3.5 w-3.5" />
            100
          </Button>
          <Button
            variant="ghost"
            size="sm"
            onClick={() => runN(1000)}
            disabled={disabled}
          >
            <PlayIcon className="mr-1 h-3.5 w-3.5" />
            1k
          </Button>
        </>
      )}

      <div className="mx-2 h-4 w-px bg-border" />

      <span className="text-xs text-muted-foreground font-mono">
        C:{cycles}
      </span>

      {panicked && (
        <span className="ml-2 text-xs font-semibold text-destructive bg-destructive/10 px-2 py-0.5 rounded">
          Panicked: {panicked}
        </span>
      )}
      {halt && !panicked && (
        <span className="ml-2 text-xs font-semibold text-destructive bg-destructive/10 px-2 py-0.5 rounded">
          Halted
        </span>
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
        <Button variant="ghost" size="sm" onClick={onStop}>
          <SquareIcon className="mr-1 h-3.5 w-3.5" />
          Stop
        </Button>
        <ThemeSwitcher />
      </div>
    </div>
  );
};
