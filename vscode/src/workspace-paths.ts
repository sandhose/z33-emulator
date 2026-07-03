import * as vscode from "vscode";

/** Glob matching every Z33 assembly source file in the workspace. */
export const FILE_GLOB = "**/*.{s,S}";

/**
 * Whether workspace-relative paths must include the folder name to stay unique.
 * With a single root the folder prefix is noise; with several roots it prevents
 * colliding basenames (`a/main.s` vs `b/main.s`) from clobbering each other.
 * The LSP push (extension.ts) and the debug adapter must agree so their file
 * maps line up, hence this shared helper.
 */
export function includeWorkspaceFolderInPaths(): boolean {
  return (vscode.workspace.workspaceFolders?.length ?? 0) > 1;
}

/**
 * Gather every workspace `.s`/`.S` file as a workspace-relative path → content
 * map. When `program` is given (an absolute or `${file}` path), it is resolved
 * to the matching relative key so the in-memory FS lookup succeeds; the LSP
 * push ignores that field.
 *
 * Both the LSP seed (extension.ts) and the debug adapter go through this single
 * implementation so include resolution and program lookup agree on keys
 * (folder-prefixed only when there are multiple roots).
 */
export async function collectWorkspaceFiles(
  program?: string,
): Promise<{ files: Record<string, string>; program: string }> {
  const files: Record<string, string> = {};
  const uris = await vscode.workspace.findFiles(FILE_GLOB);
  const decoder = new TextDecoder();
  const includeFolder = includeWorkspaceFolderInPaths();
  let resolvedProgram = program ?? "";

  for (const uri of uris) {
    const relative = vscode.workspace.asRelativePath(uri, includeFolder);
    const bytes = await vscode.workspace.fs.readFile(uri);
    files[relative] = decoder.decode(bytes);

    // Match the configured program to its workspace-relative key.
    if (program !== undefined && program.length > 0) {
      if (uri.fsPath === program || uri.toString() === program) {
        resolvedProgram = relative;
      }
    }
  }

  return { files, program: resolvedProgram };
}
