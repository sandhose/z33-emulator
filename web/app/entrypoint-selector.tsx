import {
  Form,
  FormControl,
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from "@/components/ui/form";
import { zodResolver } from "@hookform/resolvers/zod";
import * as React from "react";
import { useForm } from "react-hook-form";
import { Button } from "./components/ui/button";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "./components/ui/select";

import { z } from "zod";

const formSchema = z.object({
  entrypoint: z.string(),
});

// List of possible default entrypoints to use
const DEFAULT_ENTRYPOINT_NAMES = ["main", "start", "run", "entry"];

type Props = {
  onRun?: (entrypoint: string) => void;
  entrypoints: string[];
  defaultEntrypoint?: string;
};

export const EntrypointSelector: React.FC<Props> = ({
  entrypoints,
  onRun,
  defaultEntrypoint,
}: Props) => {
  const form = useForm({
    resolver: zodResolver(formSchema),
  });

  React.useEffect(() => {
    // Prefer the remembered entrypoint if it's still valid
    if (defaultEntrypoint && entrypoints.includes(defaultEntrypoint)) {
      form.setValue("entrypoint", defaultEntrypoint);
      return;
    }
    // Let's find a possible candidate for a default entrypoint
    for (const candidate of DEFAULT_ENTRYPOINT_NAMES) {
      if (entrypoints.includes(candidate)) {
        form.setValue("entrypoint", candidate);
        return;
      }
    }
  }, [entrypoints, defaultEntrypoint, form.setValue]);

  function onSubmit(values: z.infer<typeof formSchema>) {
    onRun?.(values.entrypoint);
  }

  return (
    <Form {...form}>
      <form
        onSubmit={form.handleSubmit(onSubmit)}
        className="flex flex-col gap-2"
      >
        <FormField
          control={form.control}
          name="entrypoint"
          render={({ field }) => (
            <FormItem>
              <FormLabel>Entrypoint</FormLabel>
              <Select onValueChange={field.onChange} value={field.value}>
                <FormControl>
                  <SelectTrigger>
                    <SelectValue placeholder="Entrypoint" />
                  </SelectTrigger>
                </FormControl>
                <SelectContent>
                  {entrypoints.map((label) => (
                    <SelectItem value={label} key={label}>
                      {label}
                    </SelectItem>
                  ))}
                </SelectContent>
              </Select>
              <FormDescription>Label to use as entrypoint</FormDescription>
              <FormMessage />
            </FormItem>
          )}
        />

        <Button variant="outline" type="submit">
          Run
        </Button>
      </form>
    </Form>
  );
};
