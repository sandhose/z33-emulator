/**
 * Placeholder for the editor area while its chunk loads. Same wording and
 * layout as the editor block of the static skeleton in index.html, so the two
 * hand over without the page shifting.
 */
export const EditorSkeleton: React.FC = () => (
  <div
    className="flex h-full items-center justify-center text-sm text-muted-foreground"
    aria-busy="true"
  >
    Loading the editor…
  </div>
);
