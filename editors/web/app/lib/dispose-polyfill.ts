// The wasm-bindgen glue attaches `[Symbol.dispose]` to its classes only when
// `Symbol.dispose` exists, and the lowered `using` helper looks the method up
// under `Symbol.dispose || Symbol.for("Symbol.dispose")`. On WebKit, which has
// neither, the two sides would disagree and every `using` would throw "Object
// is not disposable". Defining the well-known symbol lands them both on the
// same key. `Symbol.asyncDispose` needs no equivalent: nothing here uses
// `await using`.
//
// Import this before the bindings: the glue reads the symbol as it evaluates.

// @ts-expect-error TypeScript declares `Symbol.dispose` read-only and unique.
Symbol.dispose ??= Symbol.for("Symbol.dispose");
