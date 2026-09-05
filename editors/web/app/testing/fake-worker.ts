// Stand-in for a `Worker`, for tests that drive a worker client from the main
// thread's side of `postMessage`. Install it as `globalThis.Worker` before the
// client under test constructs one; `FakeWorker.last()` then hands back the
// instance it got, to read what it was sent and to answer.
//
// Two ways this is not a worker: messages are delivered synchronously, so a
// `respond` has landed by the time it returns, and they cross by reference
// rather than being structured-cloned, so a payload a real worker would refuse
// to send goes through here and both sides share the same object.

type Listener = (event: unknown) => void;

export class FakeWorker {
  static readonly instances: FakeWorker[] = [];

  /** Every message the client has posted, in order. */
  readonly sent: unknown[] = [];
  readonly scriptUrl: string;

  // `vscode-jsonrpc`'s browser transport takes the `onmessage` property rather
  // than a listener, so both routes have to deliver.
  onmessage: Listener | null = null;
  onerror: Listener | null = null;

  #listeners = new Map<string, Set<Listener>>();

  constructor(scriptUrl: string | URL) {
    this.scriptUrl = String(scriptUrl);
    FakeWorker.instances.push(this);
  }

  /** The worker most recently constructed. */
  static last(): FakeWorker {
    const worker = FakeWorker.instances.at(-1);
    if (!worker) throw new Error("no FakeWorker was constructed");
    return worker;
  }

  /** Forget every instance, so `last()` cannot reach across tests. */
  static reset(): void {
    FakeWorker.instances.length = 0;
  }

  postMessage(message: unknown): void {
    this.sent.push(message);
  }

  addEventListener(type: string, listener: Listener): void {
    const set = this.#listeners.get(type) ?? new Set();
    set.add(listener);
    this.#listeners.set(type, set);
  }

  removeEventListener(type: string, listener: Listener): void {
    this.#listeners.get(type)?.delete(listener);
  }

  /** Deliver a worker -> main thread message. */
  respond(data: unknown): void {
    this.#emit("message", { data });
  }

  /** Fire the `error` event a hard script failure would produce. */
  crash(message: string): void {
    this.#emit("error", { message });
  }

  #emit(type: string, event: unknown): void {
    for (const listener of this.#listeners.get(type) ?? []) listener(event);
    if (type === "message") this.onmessage?.(event);
    if (type === "error") this.onerror?.(event);
  }
}

/** Point `globalThis.Worker` at `FakeWorker` and clear the instance list. */
export function installFakeWorker(): void {
  FakeWorker.reset();
  // FakeWorker covers the surface a worker client uses, not all of `Worker`.
  // oxlint-disable-next-line typescript/no-unsafe-type-assertion
  globalThis.Worker = FakeWorker as unknown as typeof Worker;
}
