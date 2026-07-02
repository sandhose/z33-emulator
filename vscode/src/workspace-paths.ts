import * as vscode from "vscode";

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
