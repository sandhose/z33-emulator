import type { Cell, Cycles, Registers } from "./lib/wasm";
import type { DisplayFormat } from "./stores/display-store";
import { assertNever } from "./lib/utils";

export type Labels = Map<number, string[]>;

/** Narrow type for actual CPU registers */
export type RegisterId = "%pc" | "%sp" | "%a" | "%b";

/** Anything that can be followed in the memory viewer: a register or a label */
export type Following = RegisterId | `label:${string}`;

/** Map from address to list of registers pointing there */
export type Pointers = Map<number, RegisterId[]>;

/**
 * Reactive read surface over the running computer, implemented by the
 * main-thread `ComputerProxy` backed by the emulator worker. Execution controls
 * (step / run / pause / breakpoints) live on the concrete `ComputerProxy`.
 */
export interface ComputerInterface {
  registers(): Registers;
  memory(address: number): Cell;
  cycles(): Cycles;
  subscribe_registers(cb: (r: Registers) => void): () => void;
  subscribe_memory(address: number, cb: (c: Cell) => void): () => void;
  subscribe_cycles(cb: (c: Cycles) => void): () => void;
  readonly labels: Iterable<[string, number]>;
}

export const REGISTER_COLORS: Record<RegisterId, string> = {
  "%pc": "bg-blue-500",
  "%sp": "bg-emerald-500",
  "%a": "bg-amber-500",
  "%b": "bg-violet-500",
};

export const formatWord = (word: number, format: DisplayFormat): string => {
  switch (format) {
    case "hex":
      return word < 0
        ? `-0x${(-word).toString(16).toUpperCase()}`
        : `0x${word.toString(16).toUpperCase()}`;
    case "binary":
      return word < 0 ? `-0b${(-word).toString(2)}` : `0b${word.toString(2)}`;
    case "decimal":
      return String(word);
    default:
      return assertNever(format);
  }
};

export const formatAddress = (
  address: number,
  format: DisplayFormat,
): string => {
  switch (format) {
    case "hex":
      return `0x${address.toString(16).toUpperCase()}`;
    case "binary":
      return `0b${address.toString(2)}`;
    case "decimal":
      return String(address);
    default:
      return assertNever(format);
  }
};

export const ADDRESS_WIDTH: Record<DisplayFormat, string> = {
  decimal: "w-[6ch]",
  hex: "w-[8ch]",
  binary: "w-[16ch]",
};
