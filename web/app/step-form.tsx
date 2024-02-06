import { useState, useEffect, useCallback } from "react";
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
	speed: z.coerce.number().min(1).max(100).default(1),
	steps: z.coerce.number().min(1).max(100).default(1),
});

export const StepForm: React.FC<{ onStep: () => boolean }> = ({ onStep }) => {
	const [halt, setHalt] = useState(false);
	const [panicked, setPanicked] = useState<string | null>(null);
	const [running, setRunning] = useState(false);
	const [stepsToRun, setStepsToRun] = useState(0);
	const [lastStepsValue, setLastStepsValue] = useState(0);
	const [speed, setSpeed] = useState(1);

	const onStepCallback = useCallback(() => {
		try {
			if (onStep?.()) {
				setHalt(true);
			}
		} catch (error) {
			setPanicked(String(error));
		}

		setStepsToRun((prevCounter) => prevCounter - 1);
	}, [onStep]);

	useEffect(() => {
		if (halt || panicked !== null) {
			setRunning(false);
			return;
		}

		if (running !== stepsToRun > 0) {
			setRunning(stepsToRun > 0);
		}
	}, [halt, running, panicked, stepsToRun]);

	useEffect(() => {
		if (running) {
			const intervalId = setInterval(onStepCallback, 1000 / speed);
			return () => clearInterval(intervalId);
		}
	}, [speed, running, onStepCallback]);

	const form = useForm<z.infer<typeof formSchema>>({
		resolver: zodResolver(formSchema),
		defaultValues: { steps: 1, speed: 50 },
	});

	function onSubmit(values: z.infer<typeof formSchema>) {
		setSpeed(values.speed);
		setLastStepsValue(values.steps);
		setStepsToRun(values.steps);
	}

	return panicked ? (
		<div className="border p-4 rounded text-center text-xl border-destructive bg-destructive-foreground text-destructive">
			Panicked!
			<div className="text-xs font-bold">{panicked}</div>
		</div>
	) : halt ? (
		<div className="border p-4 rounded text-center text-xl border-destructive bg-destructive-foreground text-destructive">
			Halted
		</div>
	) : (
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

				<FormField
					control={form.control}
					name="speed"
					disabled={stepsToRun > 0}
					render={({ field }) => (
						<FormItem>
							<FormLabel>Speed: {field.value} op/s</FormLabel>
							<FormControl>
								<Input {...field} min="1" max="100" step="1" type="range" />
							</FormControl>
							<FormDescription>How fast to run</FormDescription>
							<FormMessage />
						</FormItem>
					)}
				/>

				{running ? (
					<>
						<Button
							type="button"
							variant="destructive"
							onClick={() => setStepsToRun(0)}
						>
							Stop
						</Button>

						{lastStepsValue > 1 ? (
							<Progress
								value={(100 * (lastStepsValue - stepsToRun)) / lastStepsValue}
							/>
						) : null}
					</>
				) : (
					<Button type="submit">Run</Button>
				)}
			</form>
		</Form>
	);
};
