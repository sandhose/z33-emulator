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

const spanToRange = (
  model: monaco.editor.IModel,
  span: Span,
): monaco.IRange => {
  const start = model.getPositionAt(span.offset);
  const end = model.getPositionAt(span.offset + span.length);

  return {
    startLineNumber: start.lineNumber,
    startColumn: start.column,
    endLineNumber: end.lineNumber,
    endColumn: end.column,
  };
};

export const toMonacoDecoration = (
  model: monaco.editor.IModel,
  report: Report,
  parentReport?: Report,
): monaco.editor.IModelDeltaDecoration[] => {
  const filename = report.filename || parentReport?.filename || null;

  const children = report.related.flatMap((related) =>
    toMonacoDecoration(model, related, report),
  );

  if (filename !== model.uri.path) {
    return children;
  }

  const result = report.labels.map((item) => {
    const range = spanToRange(model, item.span);
    const hoverMessage = [{ value: report.message }];
    if (item.label) {
      hoverMessage.push({ value: item.label });
    }

    if (item.span.length === 0) {
      return {
        range,
        options: {
          glyphMarginHoverMessage: hoverMessage,
          isWholeLine: true,
          linesDecorationsClassName: "bg-destructive",
        },
      };
    }

    return {
      range,
      options: {
        hoverMessage,
      },
    };
  });

  return [...result, ...children];
};
