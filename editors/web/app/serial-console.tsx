import { TerminalIcon, Trash2Icon } from "lucide-react";
import { lazy, Suspense, useCallback, useRef } from "react";
import { ErrorBoundary } from "./components/error-boundary";
import { Button } from "./components/ui/button";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "./components/ui/tooltip";
import type { SerialPort } from "./computer-types";
import type { SerialTerminalHandle } from "./serial-terminal";

// Lazy-load the xterm-backed terminal so xterm (~82 KB gzip) lands in its own
// chunk and stays out of the initial bundle. It is only needed once the debug
// panel renders.
const SerialTerminal = lazy(() => import("./serial-terminal"));

/**
 * Pure header + layout chrome for the serial console. The terminal body is
 * passed in as `children`, so this shell has no coupling to xterm or the
 * emulator and can be rendered standalone (stories/tests).
 */
export const SerialConsoleShell: React.FC<{
  onClear: () => void;
  children: React.ReactNode;
}> = ({ onClear, children }) => (
  <div
    role="region"
    aria-label="Serial console"
    className="flex flex-col h-full border-t bg-background"
  >
    <div className="flex shrink-0 items-center gap-1.5 border-b px-2.5 py-1">
      <TerminalIcon className="size-3.5 text-muted-foreground" />
      <span className="text-xs font-medium text-muted-foreground">
        Serial console
      </span>
      <div className="ml-auto">
        <Tooltip>
          <TooltipTrigger
            render={
              <Button
                variant="ghost"
                size="icon-xs"
                onClick={onClear}
                aria-label="Clear serial console"
              >
                <Trash2Icon />
              </Button>
            }
          />
          <TooltipContent>Clear</TooltipContent>
        </Tooltip>
      </div>
    </div>
    <div className="flex-1 min-h-0 p-1.5">{children}</div>
  </div>
);

type SerialConsoleProps = {
  computer: SerialPort;
};

/**
 * A serial terminal wired to the emulator's serial console (ports 110-111),
 * backed by xterm.js. Program output is written straight to the terminal;
 * keystrokes and pastes are translated to receive bytes and forwarded to the
 * emulator. There is **no local echo** — only what the program writes back
 * appears. Scrollback is ephemeral: it resets whenever the debug session (the
 * `computer`) changes.
 */
export const SerialConsole: React.FC<SerialConsoleProps> = ({ computer }) => {
  const terminalRef = useRef<SerialTerminalHandle>(null);

  const clear = useCallback(() => {
    terminalRef.current?.clear();
  }, []);

  return (
    <SerialConsoleShell onClear={clear}>
      <ErrorBoundary
        fallback={
          <div className="flex h-full items-center justify-center text-xs text-muted-foreground">
            The serial console crashed.
          </div>
        }
      >
        <Suspense
          fallback={
            <div className="h-full w-full animate-pulse rounded-sm bg-muted/40" />
          }
        >
          <SerialTerminal computer={computer} ref={terminalRef} />
        </Suspense>
      </ErrorBoundary>
    </SerialConsoleShell>
  );
};
