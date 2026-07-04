import { TerminalIcon, Trash2Icon } from "lucide-react";
import {
  useCallback,
  useEffect,
  useLayoutEffect,
  useRef,
  useState,
} from "react";
import { Button } from "./components/ui/button";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "./components/ui/tooltip";
import type { ComputerProxy } from "./lib/computer-proxy";
import { cn } from "./lib/utils";

type SerialConsoleProps = {
  computer: ComputerProxy;
};

const encoder = new TextEncoder();

/** How close to the bottom (px) still counts as "following" the output. */
const AUTOSCROLL_THRESHOLD = 24;

/**
 * A minimal serial terminal wired to the emulator's serial console
 * (ports 110-111). Program output is decoded and appended to an ephemeral
 * scrollback; printable keystrokes, Enter, Backspace, Ctrl-<letter> and pastes
 * are forwarded to the emulator as receive bytes. There is **no local echo** —
 * only what the program writes back appears.
 *
 * Scrollback lives in component state and is reset whenever the `computer`
 * (i.e. the debug session) changes.
 */
export const SerialConsole: React.FC<SerialConsoleProps> = ({ computer }) => {
  const [text, setText] = useState("");
  const scrollRef = useRef<HTMLDivElement>(null);
  // Tracks whether the view should stick to the bottom on new output.
  const followRef = useRef(true);

  // Reset scrollback and subscribe to output whenever the session changes. A
  // fresh streaming decoder per session avoids carrying a partial multi-byte
  // sequence across sessions.
  useEffect(() => {
    setText("");
    followRef.current = true;
    const decoder = new TextDecoder();
    const unsubscribe = computer.onOutput((bytes) => {
      const chunk = decoder.decode(Uint8Array.from(bytes), { stream: true });
      if (chunk) setText((prev) => prev + chunk);
    });
    return unsubscribe;
  }, [computer]);

  // Auto-scroll to the bottom after new output, unless the user scrolled up.
  useLayoutEffect(() => {
    const el = scrollRef.current;
    if (el && followRef.current) el.scrollTop = el.scrollHeight;
  }, [text]);

  const handleScroll = useCallback(() => {
    const el = scrollRef.current;
    if (!el) return;
    const distance = el.scrollHeight - el.scrollTop - el.clientHeight;
    followRef.current = distance <= AUTOSCROLL_THRESHOLD;
  }, []);

  const handleKeyDown = useCallback(
    (event: React.KeyboardEvent<HTMLDivElement>) => {
      if (event.metaKey || event.altKey) return;

      if (event.ctrlKey) {
        // Map Ctrl-<letter> to its control code (Ctrl-D -> 4 = EOT). Leave
        // browser shortcuts like Ctrl-C/Ctrl-V (copy/paste) alone.
        const code = event.key.toUpperCase().codePointAt(0);
        if (
          event.key.length === 1 &&
          code !== undefined &&
          code >= 65 &&
          code <= 90
        ) {
          if (event.key === "c" || event.key === "v") return;
          event.preventDefault();
          computer.sendInput([code - 64]);
        }
        return;
      }

      if (event.key === "Enter") {
        event.preventDefault();
        computer.sendInput([0x0a]);
      } else if (event.key === "Backspace") {
        event.preventDefault();
        computer.sendInput([0x08]);
      } else if (event.key.length === 1) {
        event.preventDefault();
        computer.sendInput([...encoder.encode(event.key)]);
      }
    },
    [computer],
  );

  const handlePaste = useCallback(
    (event: React.ClipboardEvent<HTMLDivElement>) => {
      const pasted = event.clipboardData.getData("text");
      if (!pasted) return;
      event.preventDefault();
      // One receive-interrupt edge for the whole paste.
      computer.sendInput([...encoder.encode(pasted)]);
    },
    [computer],
  );

  const clear = useCallback(() => {
    setText("");
    followRef.current = true;
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
      <div
        ref={scrollRef}
        onScroll={handleScroll}
        onKeyDown={handleKeyDown}
        onPaste={handlePaste}
        tabIndex={0}
        role="textbox"
        aria-label="Serial console output"
        aria-multiline="true"
        className={cn(
          "flex-1 min-h-0 overflow-auto whitespace-pre-wrap break-words",
          "px-2.5 py-1.5 font-mono text-xs leading-relaxed outline-none",
          "focus-visible:ring-1 focus-visible:ring-ring focus-visible:ring-inset",
        )}
      >
        {text}
      </div>
    </div>
  );
};
