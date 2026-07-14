import type { Cell, Cycles, Registers } from "./lib/wasm";
import type { RunStatus } from "./lib/emulator-protocol";
import type { DisplayFormat } from "./stores/display-store";
import { assertNever } from "./lib/utils";

/** Single import point for the execution state enum used across the UI. */
export type { RunStatus };

export type Labels = Map<number, string[]>;

type Unsubscribe = () => void;

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

/**
 * Execution controls used by the step runner and debug toolbar. Implemented by
 * the concrete `ComputerProxy`; typed as an interface here so UI props and test
 * fakes can stand in for it (`ComputerProxy` has `#private` fields, making it a
 * nominal type that a plain object could never satisfy).
 */
export interface ExecutionControls {
  step(n?: number): void;
  run(): void;
  pause(): void;
  setSpeed(speed: number | null): void;
  getStatus(): RunStatus;
  getError(): string | null;
  subscribeStatus(cb: (s: RunStatus) => void): Unsubscribe;
}

/** Serial-console I/O surface (ports 110-111), as seen by the terminal UI. */
export interface SerialPort {
  /** Subscribe to serial output; fires with each non-empty byte batch. */
  onOutput(cb: (bytes: number[]) => void): Unsubscribe;
  /** Send receive bytes to the serial console; one IRQ edge per call. */
  sendInput(bytes: number[]): void;
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
