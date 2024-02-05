import { useState, useEffect } from "react";
import { z } from "zod";
import { zodResolver } from "@hookform/resolvers/zod";
import { useForm } from "react-hook-form";
import {
	Form,
	FormControl,
	FormDescription,
	FormField,
	FormItem,
	FormLabel,
	FormMessage,
} from "./components/ui/form";
import { Input } from "./components/ui/input";
import { Button } from "./components/ui/button";
import { Progress } from "./components/ui/progress";

const formSchema = z.object({
	steps: z.coerce.number().min(1).max(1000).default(1),
});

export const StepForm: React.FC<{ onStep: () => void }> = ({ onStep }) => {
	const [stepsToRun, setStepsToRun] = useState(0);
	const [lastStepsValue, setLastStepsValue] = useState(0);

	useEffect(() => {
		if (stepsToRun > 0) {
			const intervalId = setInterval(() => {
				setStepsToRun((prevCounter) => {
					if (prevCounter > 0) {
						setTimeout(() => onStep?.(), 0);
						return prevCounter - 1;
					}

					return 0;
				});
			}, 50);

			return () => clearInterval(intervalId);
		}
	}, [stepsToRun, onStep]);

	const form = useForm<z.infer<typeof formSchema>>({
		resolver: zodResolver(formSchema),
		defaultValues: { steps: 1 },
	});

	async function onSubmit(values: z.infer<typeof formSchema>) {
		setLastStepsValue(values.steps);
		setStepsToRun(values.steps);
	}

	return (
		<Form {...form}>
			<form
				onSubmit={form.handleSubmit(onSubmit)}
				className="flex flex-col gap-2 border p-4 rounded"
			>
				<FormField
					control={form.control}
					disabled={stepsToRun > 0}
					name="steps"
					render={({ field }) => (
						<FormItem>
							<FormLabel>Steps</FormLabel>
							<FormControl>
								<Input {...field} type="number" />
							</FormControl>
							<FormDescription>How many steps to run</FormDescription>
							<FormMessage />
						</FormItem>
					)}
				/>

				{stepsToRun > 0 ? (
					<>
						<Progress
							value={(100 * (lastStepsValue - stepsToRun)) / lastStepsValue}
						/>

						<Button
							type="button"
							variant="destructive"
							onClick={() => setStepsToRun(0)}
						>
							Stop
						</Button>
					</>
				) : (
					<Button type="submit">Run</Button>
				)}
			</form>
		</Form>
	);
};
