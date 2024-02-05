import type * as React from "react";
import {
	Form,
	FormControl,
	FormDescription,
	FormField,
	FormItem,
	FormLabel,
	FormMessage,
} from "@/components/ui/form";
import { Button } from "./components/ui/button";
import {
	SelectContent,
	SelectItem,
	SelectTrigger,
	SelectValue,
} from "./components/ui/select";
import { Select } from "@radix-ui/react-select";
import { zodResolver } from "@hookform/resolvers/zod";
import { useForm } from "react-hook-form";

import { z } from "zod";

const formSchema = z.object({
	entrypoint: z.string(),
});

type Props = {
	onReset?: () => void;
	onRun?: (entrypoint: string) => void;
	entrypoints: string[];
};

export const EntrypointSelector: React.FC<Props> = ({
	entrypoints,
	onReset,
	onRun,
}: Props) => {
	const form = useForm<z.infer<typeof formSchema>>({
		resolver: zodResolver(formSchema),
	});

	function onSubmit(values: z.infer<typeof formSchema>) {
		onRun?.(values.entrypoint);
	}

	return (
		<div className="w-96 p-2 border rounded flex flex-col gap-2 mx-auto my-2">
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
								<Select
									onValueChange={field.onChange}
									defaultValue={field.value}
								>
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

					<Button type="submit">Run</Button>
				</form>
			</Form>

			{onReset && (
				<Button variant="destructive" onClick={() => onReset()}>
					Reset
				</Button>
			)}
		</div>
	);
};
