import {
  CrosshairIcon,
  SkipBackIcon,
  SkipForwardIcon,
  StepBackIcon,
  StepForwardIcon,
} from "lucide-react";
import { memo, useCallback, useRef, useState } from "react";
import { Button } from "./components/ui/button";
import { Input } from "./components/ui/input";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "./components/ui/tooltip";
import {
  type ComputerInterface,
  type Labels,
  MemoryViewer,
  type MemoryViewerRef,
  useRegisters,
} from "./computer";
import { cn } from "./lib/utils";

type Tab = "pc" | "stack" | "memory";

type MemoryPanelProps = {
  computer: ComputerInterface;
  labels: Labels;
  className?: string;
};

const TabButton: React.FC<{
  active: boolean;
  onClick: () => void;
  children: React.ReactNode;
}> = ({ active, onClick, children }) => (
  <button
    type="button"
    className={cn(
      "px-3 py-1 text-xs font-medium transition-colors",
      active
        ? "border-b-2 border-primary text-foreground"
        : "text-muted-foreground hover:text-foreground",
    )}
    onClick={onClick}
  >
    {children}
  </button>
);

const PcTab: React.FC<{ computer: ComputerInterface; labels: Labels }> = memo(
  ({ computer, labels }) => {
    const ref = useRef<MemoryViewerRef>(null);
    const registers = useRegisters(computer);

    return (
      <div className="flex flex-col h-full">
        <div className="flex items-center justify-end px-2 py-1">
          <Tooltip>
            <TooltipTrigger
              render={
                <Button
                  variant="ghost"
                  size="icon-xs"
                  onClick={() => ref.current?.recenter()}
                />
              }
            />
            <TooltipContent>Recenter on PC</TooltipContent>
          </Tooltip>
          <CrosshairIcon className="h-3.5 w-3.5" />
        </div>
        <div className="flex-1 overflow-hidden">
          <MemoryViewer
            ref={ref}
            computer={computer}
            highlight={registers.pc}
            labels={labels}
          />
        </div>
      </div>
    );
  },
);
PcTab.displayName = "PcTab";

const StackTab: React.FC<{ computer: ComputerInterface; labels: Labels }> =
  memo(({ computer, labels }) => {
    const ref = useRef<MemoryViewerRef>(null);
    const registers = useRegisters(computer);

    return (
      <div className="flex flex-col h-full">
        <div className="flex items-center justify-end px-2 py-1">
          <Tooltip>
            <TooltipTrigger
              render={
                <Button
                  variant="ghost"
                  size="icon-xs"
                  onClick={() => ref.current?.recenter()}
                />
              }
            />
            <TooltipContent>Recenter on SP</TooltipContent>
          </Tooltip>
          <CrosshairIcon className="h-3.5 w-3.5" />
        </div>
        <div className="flex-1 overflow-hidden">
          <MemoryViewer
            ref={ref}
            computer={computer}
            highlight={registers.sp}
            labels={labels}
          />
        </div>
      </div>
    );
  });
StackTab.displayName = "StackTab";

const MemoryTab: React.FC<{ computer: ComputerInterface; labels: Labels }> =
  memo(({ computer, labels }) => {
    const [viewAddress, setViewAddress] = useState(1000);
    const minus10 = useCallback(() => setViewAddress((a) => a - 10), []);
    const minus1 = useCallback(() => setViewAddress((a) => a - 1), []);
    const plus1 = useCallback(() => setViewAddress((a) => a + 1), []);
    const plus10 = useCallback(() => setViewAddress((a) => a + 10), []);

    return (
      <div className="flex flex-col h-full">
        <div className="flex items-center gap-1 px-2 py-1 flex-wrap">
          {Array.from(computer.labels).map(([label, address]) => (
            <Button
              variant="secondary"
              size="sm"
              key={label}
              className="text-xs h-6 px-2"
              onClick={() => setViewAddress(address)}
            >
              {label}
            </Button>
          ))}

          <div className="flex gap-1 items-center ml-auto">
            <Button variant="outline" size="icon-xs" onClick={minus10}>
              <SkipBackIcon />
            </Button>
            <Button variant="outline" size="icon-xs" onClick={minus1}>
              <StepBackIcon />
            </Button>
            <Input
              type="number"
              value={viewAddress}
              onChange={(e) => setViewAddress(+e.target.value)}
              className="w-20 h-6 text-xs"
            />
            <Button variant="outline" size="icon-xs" onClick={plus1}>
              <StepForwardIcon />
            </Button>
            <Button variant="outline" size="icon-xs" onClick={plus10}>
              <SkipForwardIcon />
            </Button>
          </div>
        </div>
        <div className="flex-1 overflow-hidden">
          <MemoryViewer
            computer={computer}
            highlight={viewAddress}
            labels={labels}
          />
        </div>
      </div>
    );
  });
MemoryTab.displayName = "MemoryTab";

export const MemoryPanel: React.FC<MemoryPanelProps> = memo(
  ({ computer, labels, className }) => {
    const [activeTab, setActiveTab] = useState<Tab>("pc");

    return (
      <div
        className={cn("flex flex-col h-full border-t border-border", className)}
      >
        <div className="flex border-b border-border bg-muted/30">
          <TabButton
            active={activeTab === "pc"}
            onClick={() => setActiveTab("pc")}
          >
            PC
          </TabButton>
          <TabButton
            active={activeTab === "stack"}
            onClick={() => setActiveTab("stack")}
          >
            Stack
          </TabButton>
          <TabButton
            active={activeTab === "memory"}
            onClick={() => setActiveTab("memory")}
          >
            Memory
          </TabButton>
        </div>

        <div className="flex-1 overflow-hidden">
          {activeTab === "pc" && <PcTab computer={computer} labels={labels} />}
          {activeTab === "stack" && (
            <StackTab computer={computer} labels={labels} />
          )}
          {activeTab === "memory" && (
            <MemoryTab computer={computer} labels={labels} />
          )}
        </div>
      </div>
    );
  },
);
MemoryPanel.displayName = "MemoryPanel";
