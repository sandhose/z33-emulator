import preview from "#.storybook/preview";
import { expect, within } from "storybook/test";

import { ErrorBoundary } from "./error-boundary";

const meta = preview.meta({
  title: "Organisms/ErrorBoundary",
  component: ErrorBoundary,
});

/** A component that throws on render, to trip the boundary. */
const Boom: React.FC = () => {
  throw new Error("kaboom");
};

export const StaticFallback = meta.story({
  render: () => (
    <ErrorBoundary
      fallback={
        <div className="rounded border border-destructive px-3 py-2 text-sm text-destructive">
          Something broke.
        </div>
      }
    >
      <Boom />
    </ErrorBoundary>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByText("Something broke.")).toBeInTheDocument();
  },
});

export const RenderPropFallback = meta.story({
  render: () => (
    <ErrorBoundary
      fallback={(error, reset) => (
        <div className="flex flex-col items-start gap-2 rounded border px-3 py-2 text-sm">
          <span className="text-destructive">Crashed: {error.message}</span>
          <button type="button" className="underline" onClick={reset}>
            Try again
          </button>
        </div>
      )}
    >
      <Boom />
    </ErrorBoundary>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByText("Crashed: kaboom")).toBeInTheDocument();
    await expect(
      canvas.getByRole("button", { name: "Try again" }),
    ).toBeInTheDocument();
  },
});
