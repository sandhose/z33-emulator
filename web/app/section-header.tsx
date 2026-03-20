import type * as React from "react";
import { cn } from "./lib/utils";

export const SectionHeader: React.FC<React.ComponentProps<"div">> = ({
  className,
  children,
  ...props
}) => (
  <div
    className={cn(
      "bg-muted/30 px-2 py-1 text-xs font-semibold uppercase tracking-wide text-muted-foreground",
      className,
    )}
    {...props}
  >
    {children}
  </div>
);
