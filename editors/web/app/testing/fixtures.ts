// Ready-made scenes for the `FakeComputer`, used by stories and tests.
import type { Cell } from "../lib/wasm";
import type { Scene } from "./fake-computer";

const instruction = (text: string): Cell => ({
  type: "instruction",
  instruction: text,
});
const word = (value: number): Cell => ({ type: "word", word: value });

/** Fresh machine: nothing loaded, paused at the program start. */
export function emptyScene(): Scene {
  return {
    status: "paused",
    cycles: 0,
    registers: { pc: 1000, sp: 10_000 },
  };
}

/**
 * A factorial program part-way through execution: labels laid out from the
 * program start, a mix of instruction and word cells, and registers mid-run.
 */
export function factorialScene(): Scene {
  return {
    status: "paused",
    cycles: 42,
    labels: [
      ["main", 1000],
      ["loop", 1004],
      ["end", 1012],
    ],
    cells: [
      [1000, instruction("push 5")],
      [1001, instruction("call loop")],
      [1002, instruction("reset")],
      [1004, instruction("ld [%sp+1],%a")],
      [1005, instruction("cmp 1,%a")],
      [1006, instruction("jge end")],
      [1007, instruction("mul %b,%a")],
      [1012, instruction("rtn")],
      [9998, word(120)],
      [9999, word(5)],
    ],
    registers: {
      a: word(120),
      b: word(5),
      pc: 1007,
      sp: 9998,
      sr: 0x002,
    },
  };
}

/** A program that ran to completion. */
export function haltedScene(): Scene {
  return {
    ...factorialScene(),
    status: "halted",
    cycles: 128,
    registers: {
      a: word(120),
      b: word(5),
      pc: 1002,
      sp: 10_000,
      sr: 0x002,
    },
  };
}

/** A program that hit a fatal error. */
export function panickedScene(msg: string): Scene {
  return {
    ...factorialScene(),
    status: "panicked",
    error: msg,
  };
}
