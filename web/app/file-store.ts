const WORKSPACE_KEY = "z33:workspace";
const ACTIVE_FILE_KEY = "z33:active-file";

export function loadWorkspace(): Map<string, string> | null {
  try {
    const raw = localStorage.getItem(WORKSPACE_KEY);
    if (!raw) return null;
    const parsed = JSON.parse(raw) as Record<string, string>;
    const map = new Map(Object.entries(parsed));
    return map.size > 0 ? map : null;
  } catch {
    return null;
  }
}

export function loadActiveFile(): string | null {
  return localStorage.getItem(ACTIVE_FILE_KEY);
}
