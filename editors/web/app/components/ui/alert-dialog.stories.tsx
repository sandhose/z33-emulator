import preview from "#.storybook/preview";
import { expect, screen, userEvent, waitFor, within } from "storybook/test";

import {
  AlertDialog,
  AlertDialogAction,
  AlertDialogCancel,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogHeader,
  AlertDialogTitle,
  AlertDialogTrigger,
} from "./alert-dialog";
import { Button } from "./button";

const meta = preview.meta({
  title: "UI/AlertDialog",
  component: AlertDialog,
});

const Example = () => (
  <AlertDialog>
    <AlertDialogTrigger
      render={<Button variant="destructive">Delete project</Button>}
    />
    <AlertDialogContent>
      <AlertDialogHeader>
        <AlertDialogTitle>Are you absolutely sure?</AlertDialogTitle>
        <AlertDialogDescription>
          This permanently deletes the project and everything in it. This action
          cannot be undone.
        </AlertDialogDescription>
      </AlertDialogHeader>
      <AlertDialogFooter>
        <AlertDialogCancel>Cancel</AlertDialogCancel>
        <AlertDialogAction variant="destructive">Delete</AlertDialogAction>
      </AlertDialogFooter>
    </AlertDialogContent>
  </AlertDialog>
);

export const Default = meta.story({
  render: () => <Example />,
});

export const OpenAndCancel = meta.story({
  render: () => <Example />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(
      canvas.getByRole("button", { name: "Delete project" }),
    );
    // base-ui renders the dialog in a portal on document.body.
    const dialog = await screen.findByRole("alertdialog");
    await expect(dialog).toBeVisible();
    await expect(
      within(dialog).getByText("Are you absolutely sure?"),
    ).toBeVisible();
    await userEvent.click(
      within(dialog).getByRole("button", { name: "Cancel" }),
    );
    await waitFor(() =>
      expect(screen.queryByRole("alertdialog")).not.toBeInTheDocument(),
    );
  },
});
