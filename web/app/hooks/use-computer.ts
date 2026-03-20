import { useCallback, useDeferredValue, useSyncExternalStore } from "react";
import type { Cell, Cycles, Registers } from "z33-web-bindings";
import type { ComputerInterface } from "../computer-types";

export const useMemoryCell = (
  computer: ComputerInterface,
  address: number,
): Cell => {
  const subscribe = useCallback(
    (cb: (cell: Cell) => void) => computer.subscribe_memory(address, cb),
    [computer, address],
  );
  const value = useSyncExternalStore(subscribe, () => computer.memory(address));
  return useDeferredValue(value);
};

export const useRegisters = (computer: ComputerInterface): Registers => {
  const subscribe = useCallback(
    (cb: (registers: Registers) => void) => computer.subscribe_registers(cb),
    [computer],
  );
  const registers = useSyncExternalStore(subscribe, () => computer.registers());
  return useDeferredValue(registers);
};

export const useCycles = (computer: ComputerInterface): Cycles => {
  const subscribe = useCallback(
    (cb: (cycles: Cycles) => void) => computer.subscribe_cycles(cb),
    [computer],
  );
  const cycles = useSyncExternalStore(subscribe, () => computer.cycles());
  return useDeferredValue(cycles);
};
