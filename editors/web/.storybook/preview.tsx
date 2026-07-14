import addonA11y from "@storybook/addon-a11y";
import addonThemes, { withThemeByClassName } from "@storybook/addon-themes";
import addonVitest from "@storybook/addon-vitest";
import { definePreview } from "@storybook/react-vite";
import { useEffect } from "react";
import { useGlobals } from "storybook/preview-api";

import { TooltipProvider } from "../app/components/ui/tooltip";
import {
  type DisplayFormat,
  useDisplayStore,
} from "../app/stores/display-store";
import "../app/globals.css";

export default definePreview({
  addons: [addonA11y(), addonThemes(), addonVitest()],
  globalTypes: {
    displayFormat: {
      description: "Number display format for memory & registers",
      toolbar: {
        title: "Format",
        icon: "hash",
        items: [
          { value: "decimal", title: "Decimal" },
          { value: "hex", title: "Hexadecimal" },
          { value: "binary", title: "Binary" },
        ],
        dynamicTitle: true,
      },
    },
  },
  initialGlobals: {
    displayFormat: "decimal",
  },
  decorators: [
    withThemeByClassName({
      themes: { light: "", dark: "dark" },
      defaultTheme: "light",
    }),
    // Syncs the `displayFormat` toolbar global into the real zustand display
    // store, which leaf components (memory viewer, register panel, …) subscribe
    // to directly. Two-way: the toolbar drives the store, and an in-story
    // FormatSwitcher (which writes the store) mirrors back into the toolbar.
    // Both directions run in effects and are gated on inequality, so they
    // converge without a render loop. Mounting each story with the global's
    // value also resets the shared store, keeping stories deterministic.
    (Story) => {
      const [globals, updateGlobals] = useGlobals();
      const format = (globals["displayFormat"] as DisplayFormat) ?? "decimal";

      // Toolbar global -> store.
      useEffect(() => {
        if (useDisplayStore.getState().format !== format) {
          useDisplayStore.setState({ format });
        }
      }, [format]);

      // Store -> toolbar global (e.g. an in-story FormatSwitcher).
      useEffect(
        () =>
          useDisplayStore.subscribe((state) => {
            updateGlobals({ displayFormat: state.format });
          }),
        [updateGlobals],
      );

      return <Story />;
    },
    (Story) => (
      <TooltipProvider delay={0}>
        <Story />
      </TooltipProvider>
    ),
  ],
  parameters: {
    layout: "centered",
    a11y: {
      // a11y violations FAIL the test run (every story is checked by axe via
      // @storybook/addon-vitest). The design-token contrast audit is done
      // (globals.css light-theme muted-foreground/destructive were darkened)
      // and app landmarks/accessible-names were added. The escape hatch for
      // genuine upstream issues we don't control (e.g. base-ui focus guards
      // tripping `aria-hidden-focus` on open popups) is a per-story
      // `parameters.a11y.config.rules` disable with a comment — never a global
      // one.
      test: "error",
    },
  },
  tags: ["autodocs"],
});
