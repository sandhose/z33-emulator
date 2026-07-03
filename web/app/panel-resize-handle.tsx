import { Separator } from "react-resizable-panels";
import { cn } from "./lib/utils";

export const ResizeHandle: React.FC = () => (
  <Separator
    className={cn(
      "group relative flex items-center justify-center bg-border",
      "w-px data-[resize-handle-active]:w-1 hover:w-1",
      "transition-all duration-150 data-[resize-handle-active]:bg-ring",
    )}
  />
);
