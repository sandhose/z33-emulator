import { PauseIcon, PlayIcon, StepForwardIcon } from "lucide-react";
import { memo, startTransition, useCallback, useEffect, useState } from "react";
import type { Computer } from "z33-web-bindings";
import { Button } from "./components/ui/button";
import { useCycles } from "./computer";
import { cn } from "./lib/utils";

type DebugToolbarProps = {
  computer: Computer;
  className?: string;
};

export const DebugToolbar: React.FC<DebugToolbarProps> = memo(
  ({ computer, className }) => {
    const [halt, setHalt] = useState(false);
    const [panicked, setPanicked] = useState<string | null>(null);
    const [running, setRunning] = useState(false);
    const [stepsToRun, setStepsToRun] = useState(0);
    const [speed, setSpeed] = useState(20);
    const cycles = useCycles(computer);

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

    function stepOnce() {
      startTransition(() => {
        setStepsToRun(1);
        doStep();
      });
    }

    function runN(n: number) {
      startTransition(() => {
        setSpeed(20);
        setStepsToRun(n);
        doStep();
      });
    }

    function stop() {
      setStepsToRun(0);
    }

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
      </div>
    );
  },
);
DebugToolbar.displayName = "DebugToolbar";
