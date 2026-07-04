import { type ClassValue, clsx } from "clsx";
import { twMerge } from "tailwind-merge";

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs));
}

export function assertNever(x: never): never {
  throw new Error(`Unhandled discriminated union case: ${JSON.stringify(x)}`);
}
