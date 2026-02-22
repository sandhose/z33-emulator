import type * as monaco from "monaco-editor";
import * as z from "zod";

const spanSchema = z.object({
  offset: z.number().min(0).int(),
  length: z.number().min(0).int(),
});

type Span = z.infer<typeof spanSchema>;

const labelSchema = z.object({
  label: z.string().optional(),
  span: spanSchema,
});

const baseReportSchema = z.object({
  message: z.string(),
  code: z.string().optional(),
  severity: z.enum(["error", "warning", "advice"]),
  causes: z.string().array().optional(),
  url: z.string().optional(),
  help: z.string().optional(),
  filename: z.string(),
  labels: labelSchema.array(),
});

export type Report = z.infer<typeof baseReportSchema> & {
  related: Report[];
};

export const reportSchema: z.ZodType<Report> = baseReportSchema.extend({
  related: z.lazy(() => reportSchema.array()),
});

// Monaco MarkerSeverity enum values (stable across versions)
const MARKER_SEVERITY: Record<Report["severity"], monaco.MarkerSeverity> = {
  error: 8 as monaco.MarkerSeverity, // MarkerSeverity.Error
  warning: 4 as monaco.MarkerSeverity, // MarkerSeverity.Warning
  advice: 2 as monaco.MarkerSeverity, // MarkerSeverity.Info
};

const SEVERITY_CLASSES: Record<
  Report["severity"],
  { line: string; glyph: string }
> = {
  error: { line: "diag-error-line", glyph: "diag-error-glyph" },
  warning: { line: "diag-warning-line", glyph: "diag-warning-glyph" },
  advice: { line: "diag-advice-line", glyph: "diag-advice-glyph" },
};

const spanToPositions = (
  model: monaco.editor.IModel,
  span: Span,
): Pick<
  monaco.editor.IMarkerData,
  "startLineNumber" | "startColumn" | "endLineNumber" | "endColumn"
> => {
  const start = model.getPositionAt(span.offset);
  // For zero-length spans, extend by 1 so the squiggly is visible
  const endOffset = span.offset + Math.max(span.length, 1);
  const end = model.getPositionAt(endOffset);

  return {
    startLineNumber: start.lineNumber,
    startColumn: start.column,
    endLineNumber: end.lineNumber,
    endColumn: end.column,
  };
};

const buildMessage = (
  report: Report,
  labelText: string | undefined,
): string => {
  const parts = [report.message];
  if (report.causes?.length) {
    for (const cause of report.causes) {
      parts.push(`Caused by: ${cause}`);
    }
  }
  if (labelText) parts.push(labelText);
  if (report.help) parts.push(`Help: ${report.help}`);
  return parts.join("\n");
};

export type DiagnosticResult = {
  markers: monaco.editor.IMarkerData[];
  decorations: monaco.editor.IModelDeltaDecoration[];
};

export const toMonacoDiagnostics = (
  model: monaco.editor.IModel,
  report: Report,
  parentReport?: Report,
): DiagnosticResult => {
  const filename = report.filename || parentReport?.filename || null;

  const children = report.related.reduce(
    (acc: DiagnosticResult, related) => {
      const child = toMonacoDiagnostics(model, related, report);
      return {
        markers: [...acc.markers, ...child.markers],
        decorations: [...acc.decorations, ...child.decorations],
      };
    },
    { markers: [], decorations: [] },
  );

  if (filename !== model.uri.path) {
    return children;
  }

  const severity = MARKER_SEVERITY[report.severity];
  const classes = SEVERITY_CLASSES[report.severity];

  const linesSeen = new Set<number>();
  const markers: monaco.editor.IMarkerData[] = [];
  const decorations: monaco.editor.IModelDeltaDecoration[] = [];

  for (const item of report.labels) {
    const positions = spanToPositions(model, item.span);

    // Squiggly underline marker
    const marker: monaco.editor.IMarkerData = {
      severity,
      message: buildMessage(report, item.label),
      ...positions,
    };
    if (report.code !== undefined) marker.source = report.code;
    markers.push(marker);

    // Whole-line background + gutter bar, one per affected line
    const lineNumber = positions.startLineNumber;
    if (!linesSeen.has(lineNumber)) {
      linesSeen.add(lineNumber);
      decorations.push({
        range: {
          startLineNumber: lineNumber,
          startColumn: 1,
          endLineNumber: lineNumber,
          endColumn: model.getLineMaxColumn(lineNumber),
        },
        options: {
          isWholeLine: true,
          className: classes.line,
          glyphMarginClassName: classes.glyph,
        },
      });
    }
  }

  return {
    markers: [...markers, ...children.markers],
    decorations: [...decorations, ...children.decorations],
  };
};
