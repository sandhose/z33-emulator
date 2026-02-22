import { Separator } from "react-resizable-panels";
import { cn } from "./lib/utils";

export const ResizeHandle: React.FC<{
  direction?: "horizontal" | "vertical";
}> = ({ direction = "horizontal" }) => (
  <Separator
    className={cn(
      "group relative flex items-center justify-center bg-border",
      direction === "horizontal"
        ? "w-px data-[resize-handle-active]:w-1 hover:w-1"
        : "h-px data-[resize-handle-active]:h-1 hover:h-1",
      "transition-all duration-150 data-[resize-handle-active]:bg-ring",
    )}
  />
);
