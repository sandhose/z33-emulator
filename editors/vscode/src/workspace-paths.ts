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
function includeWorkspaceFolderInPaths(): boolean {
  return (vscode.workspace.workspaceFolders?.length ?? 0) > 1;
}

/** Directories a manual workspace walk must never descend into. */
const WALK_EXCLUDED_DIRS = new Set(["node_modules", ".git"]);

/**
 * Find every `.s`/`.S` file in the workspace. `findFiles` first; if it comes
 * back empty, fall back to a manual `fs.readDirectory` walk of each workspace
 * folder. On web hosts the search goes through whatever `FileSearchProvider`
 * the virtual filesystem registers, and some don't handle our brace glob (or
 * any glob) — the walk only needs the FileSystemProvider, which always exists.
 */
async function findSourceFiles(): Promise<vscode.Uri[]> {
  const found = await vscode.workspace.findFiles(FILE_GLOB);
  if (found.length > 0) {
    return found;
  }
  const walked: vscode.Uri[] = [];
  for (const folder of vscode.workspace.workspaceFolders ?? []) {
    await walkForSourceFiles(folder.uri, walked);
  }
  return walked;
}

async function walkForSourceFiles(dir: vscode.Uri, out: vscode.Uri[]): Promise<void> {
  let entries: [string, vscode.FileType][];
  try {
    entries = await vscode.workspace.fs.readDirectory(dir);
  } catch {
    return; // unreadable directory: skip, don't fail the whole collection
  }
  for (const [name, type] of entries) {
    if (type === vscode.FileType.Directory) {
      if (!WALK_EXCLUDED_DIRS.has(name) && !name.startsWith(".")) {
        await walkForSourceFiles(vscode.Uri.joinPath(dir, name), out);
      }
    } else if (type === vscode.FileType.File && (name.endsWith(".s") || name.endsWith(".S"))) {
      out.push(vscode.Uri.joinPath(dir, name));
    }
  }
}

/**
 * Resolve a server-side workspace-relative path (as carried by e.g. the run
 * code lens) back to a real workspace URI by joining it onto each workspace
 * folder and probing with `fs.stat`. Deliberately avoids `findFiles`, which is
 * unreliable on web hosts (see `findSourceFiles`). With multiple roots the
 * relative path may or may not carry the folder-name prefix (the LSP push
 * prefixes it, the server's own root-URI relativization doesn't), so both
 * spellings are probed.
 */
export async function uriForWorkspaceRelativePath(
  relativePath: string,
): Promise<vscode.Uri | undefined> {
  const folders = vscode.workspace.workspaceFolders ?? [];
  // Probe the folder the leading path segment names first: with multiple
  // roots the pushed keys are folder-prefixed, and a verbatim probe against an
  // earlier folder that coincidentally contains a same-named subtree would
  // shadow the intended file.
  for (const folder of folders) {
    const prefix = `${folder.name}/`;
    if (relativePath.startsWith(prefix)) {
      const uri = vscode.Uri.joinPath(folder.uri, relativePath.slice(prefix.length));
      if (await fileExists(uri)) {
        return uri;
      }
    }
  }
  for (const folder of folders) {
    const uri = vscode.Uri.joinPath(folder.uri, relativePath);
    if (await fileExists(uri)) {
      return uri;
    }
  }
  return undefined;
}

async function fileExists(uri: vscode.Uri): Promise<boolean> {
  try {
    await vscode.workspace.fs.stat(uri);
    return true;
  } catch {
    return false;
  }
}

/**
 * Gather every workspace `.s`/`.S` file as a workspace-relative path → content
 * map. When `program` is given (an absolute or `${file}` path), it is resolved
 * to the matching relative key so the in-memory FS lookup succeeds; the LSP
 * push ignores that field.
 *
 * `uris` maps each relative key back to its real `vscode.Uri`; the debug
 * adapter needs it to translate DAP `Source` paths between the server's
 * relative keys and the client's file/URI paths (the LSP push ignores it).
 *
 * Both the LSP seed (extension.ts) and the debug adapter go through this single
 * implementation so include resolution and program lookup agree on keys
 * (folder-prefixed only when there are multiple roots).
 */
export async function collectWorkspaceFiles(program?: string): Promise<{
  files: Record<string, string>;
  program: string;
  uris: Map<string, vscode.Uri>;
}> {
  const files: Record<string, string> = {};
  const uriByKey = new Map<string, vscode.Uri>();
  const uris = await findSourceFiles();
  const decoder = new TextDecoder();
  const includeFolder = includeWorkspaceFolderInPaths();
  let resolvedProgram = program ?? "";

  for (const uri of uris) {
    const relative = vscode.workspace.asRelativePath(uri, includeFolder);
    const bytes = await vscode.workspace.fs.readFile(uri);
    files[relative] = decoder.decode(bytes);
    uriByKey.set(relative, uri);

    // Match the configured program to its workspace-relative key.
    if (program !== undefined && program.length > 0) {
      if (uri.fsPath === program || uri.toString() === program) {
        resolvedProgram = relative;
      }
    }
  }

  return { files, program: resolvedProgram, uris: uriByKey };
}
