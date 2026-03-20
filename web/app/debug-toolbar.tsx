import {
  BookOpenIcon,
  PauseIcon,
  PencilIcon,
  PlayIcon,
  StepForwardIcon,
} from "lucide-react";
import {
  memo,
  startTransition,
  useCallback,
  useEffect,
  useReducer,
} from "react";
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
import type { ComputerInterface } from "./computer-types";
import { useCycles } from "./hooks/use-computer";
import { cn } from "./lib/utils";
import { useAppStore } from "./stores/app-store";
import { ThemeSwitcher } from "./theme-switcher";

/** Step interval in milliseconds */
const STEP_INTERVAL_MS = 50;

type StepState =
  | { status: "idle" }
  | { status: "running"; remaining: number }
  | { status: "halted" }
  | { status: "panicked"; error: string };

type StepAction =
  | { type: "step_ok" }
  | { type: "halt" }
  | { type: "panic"; error: string }
  | { type: "schedule"; count: number }
  | { type: "stop" };

function stepReducer(state: StepState, action: StepAction): StepState {
  switch (action.type) {
    case "step_ok": {
      if (state.status !== "running") return { status: "idle" };
      const remaining = state.remaining - 1;
      if (remaining <= 0) return { status: "idle" };
      return { status: "running", remaining };
    }
    case "halt":
      return { status: "halted" };
    case "panic":
      return { status: "panicked", error: action.error };
    case "schedule":
      return { status: "running", remaining: action.count };
    case "stop":
      return { status: "idle" };
  }
}

function useStepRunner(computer: ComputerInterface) {
  const [state, dispatch] = useReducer(stepReducer, { status: "idle" });

  const doStep = useCallback(() => {
    try {
      if (computer.step()) {
        dispatch({ type: "halt" });
        return;
      }
    } catch (error) {
      dispatch({ type: "panic", error: String(error) });
      return;
    }
    dispatch({ type: "step_ok" });
  }, [computer]);

  useEffect(() => {
    if (state.status !== "running") return;
    const id = setInterval(doStep, STEP_INTERVAL_MS);
    return () => clearInterval(id);
  }, [state.status, doStep]);

  const stepOnce = useCallback(() => {
    startTransition(() => {
      dispatch({ type: "schedule", count: 1 });
      doStep();
    });
  }, [doStep]);

  const runN = useCallback(
    (n: number) => {
      startTransition(() => {
        dispatch({ type: "schedule", count: n });
        doStep();
      });
    },
    [doStep],
  );

  const stop = useCallback(() => {
    dispatch({ type: "stop" });
  }, []);

  return {
    halt: state.status === "halted",
    panicked: state.status === "panicked" ? state.error : null,
    running: state.status === "running",
    stepOnce,
    runN,
    stop,
  };
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
      role="toolbar"
      aria-label="Debug"
      className={cn(
        "flex items-center gap-1 px-2 py-1 border-b border-border bg-muted/30",
        className,
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
        <Button variant="ghost" size="xs" onClick={stop}>
          <PauseIcon data-icon="inline-start" />
          Pause
        </Button>
      ) : (
        <>
          <Button
            variant="ghost"
            size="xs"
            onClick={() => runN(10)}
            disabled={disabled}
            aria-label="Run 10 steps"
          >
            <PlayIcon data-icon="inline-start" />
            10
          </Button>
          <Button
            variant="ghost"
            size="xs"
            onClick={() => runN(100)}
            disabled={disabled}
            aria-label="Run 100 steps"
          >
            <PlayIcon data-icon="inline-start" />
            100
          </Button>
          <Button
            variant="ghost"
            size="xs"
            onClick={() => runN(1000)}
            disabled={disabled}
            aria-label="Run 1000 steps"
          >
            <PlayIcon data-icon="inline-start" />
            1k
          </Button>
        </>
      )}

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
        <Button
          variant="ghost"
          size="xs"
          nativeButton={false}
          render={
            // biome-ignore lint/a11y/useAnchorContent: Button renders its children inside this anchor
            <a
              href="https://pdagog.gitlab.io/ens/z33refcard-fr.pdf"
              target="_blank"
              rel="noopener noreferrer"
            />
          }
        >
          <BookOpenIcon data-icon="inline-start" />
          Docs
        </Button>
        <ThemeSwitcher />
      </div>
    </div>
  );
};
