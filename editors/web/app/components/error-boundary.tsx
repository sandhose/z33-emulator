import { Component, type ErrorInfo, type ReactNode } from "react";

type ErrorBoundaryProps = {
  children: ReactNode;
  /** Static node, or a render function given the error and a reset callback. */
  fallback: ReactNode | ((error: Error, reset: () => void) => ReactNode);
};

type ErrorBoundaryState = {
  error: Error | null;
};

/**
 * Hand-rolled error boundary: catches render/lifecycle errors in its subtree
 * and shows `fallback` instead of unmounting the whole app. `reset()` clears
 * the caught error so the subtree can re-render (e.g. after the user acts).
 */
export class ErrorBoundary extends Component<
  ErrorBoundaryProps,
  ErrorBoundaryState
> {
  override state: ErrorBoundaryState = { error: null };

  static getDerivedStateFromError(error: Error): ErrorBoundaryState {
    return { error };
  }

  override componentDidCatch(error: Error, info: ErrorInfo): void {
    console.error("[error-boundary]", error, info.componentStack);
  }

  reset = (): void => {
    this.setState({ error: null });
  };

  override render(): ReactNode {
    const { error } = this.state;
    const { children, fallback } = this.props;
    if (error !== null) {
      return typeof fallback === "function"
        ? fallback(error, this.reset)
        : fallback;
    }
    return children;
  }
}
