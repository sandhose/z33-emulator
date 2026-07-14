// In-memory stand-in for `ComputerProxy`, for stories and tests. It mirrors the
// proxy's reactive contract exactly — per-address memory subscribers and
// snapshot getters that return **stable references** between changes, which
// `useSyncExternalStore` relies on to avoid infinite re-render loops. It never
// touches the wasm: only `import type` from the generated bindings.
import type { Cell, Registers } from "../lib/wasm";
import type {
  ComputerInterface,
  ExecutionControls,
  RunStatus,
  SerialPort,
} from "../computer-types";

type Unsubscribe = () => void;

/** Shared empty-cell reference, matching `ComputerProxy`'s `EMPTY_CELL`. */
const EMPTY_CELL: Cell = { type: "empty" };

const DEFAULT_REGISTERS: Registers = {
  a: { type: "empty" },
  b: { type: "empty" },
  pc: 1000,
  sp: 10_000,
  sr: 0,
};

/** Initial machine state for a fake. All fields optional. */
export type Scene = {
  registers?: Partial<Registers>;
  cells?: Iterable<[number, Cell]>;
  labels?: Iterable<[string, number]>;
  status?: RunStatus;
  cycles?: number;
  error?: string | null;
};

/** A recorded execution-control invocation, for test assertions. */
export type ControlCall =
  | { method: "step"; n: number }
  | { method: "run" }
  | { method: "pause" }
  | { method: "setSpeed"; speed: number | null }
  | { method: "sendInput"; bytes: number[] };

export class FakeComputer
  implements ComputerInterface, ExecutionControls, SerialPort
{
  readonly labels: [string, number][];
  /** Every execution-control / input call, in order. */
  readonly calls: ControlCall[] = [];

  #registers: Registers;
  #cycles: number;
  #status: RunStatus;
  #error: string | null;
  #cells = new Map<number, Cell>();

  #registerSubs = new Set<(r: Registers) => void>();
  #cycleSubs = new Set<(c: number) => void>();
  #statusSubs = new Set<(s: RunStatus) => void>();
  #cellSubs = new Map<number, Set<(c: Cell) => void>>();
  #outputSubs = new Set<(bytes: number[]) => void>();

  constructor(scene: Scene = {}) {
    this.#registers = { ...DEFAULT_REGISTERS, ...scene.registers };
    this.#cycles = scene.cycles ?? 0;
    this.#status = scene.status ?? "paused";
    this.#error = scene.error ?? null;
    this.labels = [...(scene.labels ?? [])];
    for (const [address, cell] of scene.cells ?? []) {
      this.#cells.set(address, cell);
    }
  }

  // --- reactive reads (ComputerInterface) --------------------------------

  registers(): Registers {
    return this.#registers;
  }

  cycles(): number {
    return this.#cycles;
  }

  memory(address: number): Cell {
    return this.#cells.get(address) ?? EMPTY_CELL;
  }

  subscribe_registers(cb: (r: Registers) => void): Unsubscribe {
    this.#registerSubs.add(cb);
    return () => {
      this.#registerSubs.delete(cb);
    };
  }

  subscribe_cycles(cb: (c: number) => void): Unsubscribe {
    this.#cycleSubs.add(cb);
    return () => {
      this.#cycleSubs.delete(cb);
    };
  }

  subscribe_memory(address: number, cb: (c: Cell) => void): Unsubscribe {
    let set = this.#cellSubs.get(address);
    if (!set) {
      set = new Set();
      this.#cellSubs.set(address, set);
    }
    set.add(cb);
    return () => {
      const current = this.#cellSubs.get(address);
      if (!current) return;
      current.delete(cb);
      if (current.size === 0) this.#cellSubs.delete(address);
    };
  }

  // --- execution controls (ExecutionControls) -----------------------------

  step(n = 1): void {
    this.calls.push({ method: "step", n });
    this.setCycles(this.#cycles + n);
    this.setStatus("paused");
  }

  run(): void {
    this.calls.push({ method: "run" });
    this.setStatus("running");
  }

  pause(): void {
    this.calls.push({ method: "pause" });
    this.setStatus("paused");
  }

  setSpeed(speed: number | null): void {
    this.calls.push({ method: "setSpeed", speed });
  }

  getStatus(): RunStatus {
    return this.#status;
  }

  getError(): string | null {
    return this.#error;
  }

  subscribeStatus(cb: (s: RunStatus) => void): Unsubscribe {
    this.#statusSubs.add(cb);
    return () => {
      this.#statusSubs.delete(cb);
    };
  }

  // --- serial port (SerialPort) -------------------------------------------

  sendInput(bytes: number[]): void {
    this.calls.push({ method: "sendInput", bytes });
  }

  onOutput(cb: (bytes: number[]) => void): Unsubscribe {
    this.#outputSubs.add(cb);
    return () => {
      this.#outputSubs.delete(cb);
    };
  }

  // --- imperative test API -------------------------------------------------

  /** Replace registers with a new object (fresh ref) and notify subscribers. */
  setRegisters(partial: Partial<Registers>): void {
    this.#registers = { ...this.#registers, ...partial };
    for (const cb of this.#registerSubs) cb(this.#registers);
  }

  /** Set a single memory cell and notify that address's subscribers. */
  patchCell(address: number, cell: Cell): void {
    this.#cells.set(address, cell);
    const subs = this.#cellSubs.get(address);
    if (subs) for (const cb of subs) cb(cell);
  }

  setCycles(cycles: number): void {
    if (cycles === this.#cycles) return;
    this.#cycles = cycles;
    for (const cb of this.#cycleSubs) cb(this.#cycles);
  }

  setStatus(status: RunStatus, error: string | null = null): void {
    this.#error = status === "panicked" ? (error ?? "panicked") : error;
    if (status === this.#status) return;
    this.#status = status;
    for (const cb of this.#statusSubs) cb(this.#status);
  }

  /** Emit a batch of serial output bytes to output subscribers. */
  emitOutput(bytes: number[]): void {
    for (const cb of this.#outputSubs) cb(bytes);
  }
}
