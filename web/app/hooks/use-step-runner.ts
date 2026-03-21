import { startTransition, useCallback, useEffect, useReducer } from "react";
import type { ComputerInterface } from "../computer-types";

/** Step interval in milliseconds */
const STEP_INTERVAL_MS = 50;

type StepState =
  | { status: "idle" }
  | { status: "running"; remaining: number }
  | { status: "halted" }
  | { status: "panicked"; error: string };

type StepAction =
  | { type: "step_ok" }
  | { type: "halt" }
  | { type: "panic"; error: string }
  | { type: "schedule"; count: number }
  | { type: "stop" };

function stepReducer(state: StepState, action: StepAction): StepState {
  switch (action.type) {
    case "step_ok": {
      if (state.status !== "running") return { status: "idle" };
      const remaining = state.remaining - 1;
      if (remaining <= 0) return { status: "idle" };
      return { status: "running", remaining };
    }
    case "halt":
      return { status: "halted" };
    case "panic":
      return { status: "panicked", error: action.error };
    case "schedule":
      return { status: "running", remaining: action.count };
    case "stop":
      return { status: "idle" };
  }
}

export function useStepRunner(computer: ComputerInterface) {
  const [state, dispatch] = useReducer(stepReducer, { status: "idle" });

  const doStep = useCallback(() => {
    try {
      if (computer.step()) {
        dispatch({ type: "halt" });
        return;
      }
    } catch (error) {
      dispatch({ type: "panic", error: String(error) });
      return;
    }
    dispatch({ type: "step_ok" });
  }, [computer]);

  useEffect(() => {
    if (state.status !== "running") return;
    const id = setInterval(doStep, STEP_INTERVAL_MS);
    return () => {
      clearInterval(id);
    };
  }, [state.status, doStep]);

  const stepOnce = useCallback(() => {
    startTransition(() => {
      dispatch({ type: "schedule", count: 1 });
      doStep();
    });
  }, [doStep]);

  const runN = useCallback(
    (n: number) => {
      startTransition(() => {
        dispatch({ type: "schedule", count: n });
        doStep();
      });
    },
    [doStep],
  );

  const stop = useCallback(() => {
    dispatch({ type: "stop" });
  }, []);

  return {
    halt: state.status === "halted",
    panicked: state.status === "panicked" ? state.error : null,
    running: state.status === "running",
    stepOnce,
    runN,
    stop,
  };
}
