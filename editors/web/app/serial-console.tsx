import { TerminalIcon, Trash2Icon } from "lucide-react";
import { lazy, Suspense, useCallback, useRef } from "react";
import { Button } from "./components/ui/button";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "./components/ui/tooltip";
import type { ComputerProxy } from "./lib/computer-proxy";
import type { SerialTerminalHandle } from "./serial-terminal";

// Lazy-load the xterm-backed terminal so xterm (~82 KB gzip) lands in its own
// chunk and stays out of the initial bundle. It is only needed once the debug
// panel renders.
const SerialTerminal = lazy(() => import("./serial-terminal"));

type SerialConsoleProps = {
  computer: ComputerProxy;
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
    <div className="flex flex-col h-full border-t bg-background">
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
                  onClick={clear}
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
      <div className="flex-1 min-h-0 p-1.5">
        <Suspense fallback={null}>
          <SerialTerminal computer={computer} ref={terminalRef} />
        </Suspense>
      </div>
    </div>
  );
};
