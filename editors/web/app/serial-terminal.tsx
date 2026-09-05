import { FitAddon } from "@xterm/addon-fit";
import { type ITheme, Terminal } from "@xterm/xterm";
import "@xterm/xterm/css/xterm.css";
import { useEffect, useImperativeHandle, useRef } from "react";
import { translateSerialInput } from "z33-editor-shared/serial-input";
import type { SerialPort } from "./computer-types";
import { useThemeStore } from "./stores/theme-store";

/** Imperative handle exposed to the header (clear button). */
export type SerialTerminalHandle = {
  clear: () => void;
};

type SerialTerminalProps = {
  computer: SerialPort;
  ref?: React.Ref<SerialTerminalHandle>;
};

const encoder = new TextEncoder();

/**
 * Static xterm palettes harmonized with the app's Tailwind theme (the neutral
 * `--background`/`--foreground` scale, converted to hex — xterm's internal
 * color math needs real hex, never CSS `var(...)`).
 */
const LIGHT_THEME: ITheme = {
  background: "#ffffff",
  foreground: "#1c1c1c",
  cursor: "#1c1c1c",
  cursorAccent: "#ffffff",
  selectionBackground: "#b4d5fe",
};

const DARK_THEME: ITheme = {
  background: "#171717",
  foreground: "#fafafa",
  cursor: "#fafafa",
  cursorAccent: "#171717",
  selectionBackground: "#3a3a3a",
};

/** Match the app's mono stack (Tailwind's default `font-mono`). */
const FONT_FAMILY =
  'ui-monospace, SFMono-Regular, "SF Mono", Menlo, Consolas, "Liberation Mono", monospace';

/**
 * The xterm.js-backed serial terminal. Lazy-loaded so xterm stays out of the
 * initial bundle. Program output is written straight through (`convertEol`
 * handles the emulator's LF-only newlines); keystrokes and pastes are
 * translated to receive bytes and forwarded to the emulator (one `sendInput`
 * call per `onData` event = one receive-interrupt edge per paste). No local
 * echo — only what the program writes back appears.
 */
const SerialTerminal: React.FC<SerialTerminalProps> = ({ computer, ref }) => {
  const containerRef = useRef<HTMLDivElement>(null);
  const terminalRef = useRef<Terminal | null>(null);

  useImperativeHandle(ref, () => ({
    clear: () => {
      terminalRef.current?.clear();
    },
  }));

  // Create the terminal once and wire up resize handling.
  useEffect(() => {
    const container = containerRef.current;
    if (!container) return () => {};

    const { effective } = useThemeStore.getState();
    const terminal = new Terminal({
      convertEol: true,
      cursorBlink: true,
      scrollback: 5000,
      fontFamily: FONT_FAMILY,
      fontSize: 12,
      theme: effective === "dark" ? DARK_THEME : LIGHT_THEME,
    });
    const fit = new FitAddon();
    terminal.loadAddon(fit);
    terminal.open(container);
    terminalRef.current = terminal;

    // The e2e tests read the scrollback through this instance: xterm's DOM
    // grid holds no text nodes they can assert on.
    if (import.meta.env.DEV) {
      (globalThis as { __z33Terminal?: Terminal }).__z33Terminal = terminal;
    }

    // Fit to the container, guarding against the collapsed-panel pitfall:
    // fitting at zero size yields NaN/Infinity dimensions and can runaway.
    const safeFit = () => {
      if (container.offsetWidth > 0 && container.offsetHeight > 0) fit.fit();
    };
    safeFit();

    let raf = 0;
    const observer = new ResizeObserver(() => {
      cancelAnimationFrame(raf);
      raf = requestAnimationFrame(safeFit);
    });
    observer.observe(container);

    return () => {
      cancelAnimationFrame(raf);
      observer.disconnect();
      terminal.dispose();
      terminalRef.current = null;
      const global = globalThis as { __z33Terminal?: Terminal };
      if (global.__z33Terminal === terminal) delete global.__z33Terminal;
    };
  }, []);

  // Live-swap the palette when the effective theme changes.
  useEffect(() => {
    const apply = (effective: "dark" | "light") => {
      const terminal = terminalRef.current;
      if (terminal) {
        terminal.options.theme =
          effective === "dark" ? DARK_THEME : LIGHT_THEME;
      }
    };
    apply(useThemeStore.getState().effective);
    return useThemeStore.subscribe((state) => {
      apply(state.effective);
    });
  }, []);

  // Reset scrollback and (re)wire I/O whenever the debug session changes.
  useEffect(() => {
    const terminal = terminalRef.current;
    if (!terminal) return () => {};

    terminal.reset();

    const onData = terminal.onData((data) => {
      const translated = translateSerialInput(data);
      if (translated) computer.sendInput([...encoder.encode(translated)]);
    });

    const unsubscribe = computer.onOutput((bytes) => {
      terminal.write(Uint8Array.from(bytes));
    });

    return () => {
      onData.dispose();
      unsubscribe();
    };
  }, [computer]);

  return <div ref={containerRef} className="h-full w-full" />;
};

export default SerialTerminal;
