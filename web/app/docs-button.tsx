import { BookOpenIcon } from "lucide-react";
import { Button } from "./components/ui/button";

export const DocsButton: React.FC = () => (
  <Button
    variant="ghost"
    size="xs"
    nativeButton={false}
    render={
      <a
        href="https://pdagog.gitlab.io/ens/z33refcard-fr.pdf"
        target="_blank"
        rel="noopener noreferrer"
      />
    }
  >
    <BookOpenIcon data-icon="inline-start" />
    Docs
  </Button>
);
