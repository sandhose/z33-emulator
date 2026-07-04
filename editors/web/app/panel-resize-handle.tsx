import { Separator } from "react-resizable-panels";
import { cn } from "./lib/utils";

type ResizeHandleProps = {
  /** Matches the parent `Group` orientation. Defaults to horizontal. */
  orientation?: "horizontal" | "vertical";
};

export const ResizeHandle: React.FC<ResizeHandleProps> = ({
  orientation = "horizontal",
}) => (
  <Separator
    className={cn(
      "group relative flex items-center justify-center bg-border",
      "transition-all duration-150 data-[resize-handle-active]:bg-ring",
      orientation === "horizontal"
        ? "w-px data-[resize-handle-active]:w-1 hover:w-1"
        : "h-px data-[resize-handle-active]:h-1 hover:h-1",
    )}
  />
);
