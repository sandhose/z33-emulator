import * as vscode from "vscode";

/** Glob matching every Z33 assembly source file in the workspace. */
export const FILE_GLOB = "**/*.{s,S}";

export const LANGUAGE_ID = "zorglub33-assembly";

/**
 * URI schemes whose documents are workspace content: on-disk files, remote
 * windows (WSL, SSH, Codespaces), virtual filesystems (vscode.dev, github.dev)
 * and the `@vscode/test-web` mount the e2e host serves from. Every other scheme
 * a `.s` document can appear under mirrors content that is not the workspace's
 * — `git:` and diff views show committed revisions, `vscode-local-history:`
 * shows past ones — so they belong neither in the file map nor in the LSP
 * document selector.
 */
export const FILE_LIKE_SCHEMES: readonly string[] = [
  "file",
  "vscode-remote",
  "vscode-vfs",
  "vscode-test-web",
];

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
  const wanted = program === undefined || program.length === 0 ? undefined : programKeys(program);
  let resolvedProgram: string | undefined;
  const collected = new Set<string>();

  for (const uri of uris) {
    const relative = normalisePath(vscode.workspace.asRelativePath(uri, includeFolder));
    const bytes = await vscode.workspace.fs.readFile(uri);
    files[relative] = decoder.decode(bytes);
    uriByKey.set(relative, uri);
    collected.add(uri.toString());

    // First match wins: a later file whose spelling coincides with the wanted
    // one must not displace the file the user actually named.
    if (
      wanted !== undefined &&
      resolvedProgram === undefined &&
      matchesProgram(uri, relative, wanted)
    ) {
      resolvedProgram = relative;
    }
  }

  // Documents the walk cannot see: unsaved buffers, and — in a window with no
  // workspace folders — a file opened on its own.
  for (const document of vscode.workspace.textDocuments) {
    if (
      document.languageId !== LANGUAGE_ID ||
      collected.has(document.uri.toString()) ||
      !isLooseDocument(document)
    ) {
      continue;
    }
    const key = documentKey(document, includeFolder);
    files[key] = document.getText();
    uriByKey.set(key, document.uri);
    if (
      wanted !== undefined &&
      resolvedProgram === undefined &&
      matchesProgram(document.uri, key, wanted)
    ) {
      resolvedProgram = key;
    }
  }

  if (wanted !== undefined && resolvedProgram === undefined) {
    const known = Object.keys(files).sort().join(", ");
    throw new Error(
      `could not find the program '${program}' among the workspace's Z33 files` +
        (known.length > 0 ? ` (${known})` : " (none found)"),
    );
  }

  return { files, program: resolvedProgram ?? "", uris: uriByKey };
}

/**
 * Whether a document with no file behind it in the walk still belongs in the
 * map. An unsaved buffer always does. A file-like one only does when the window
 * has no workspace folders at all: inside a workspace the walk is the authority
 * on what the workspace contains, and a file opened from outside the roots is
 * not part of it.
 */
function isLooseDocument(document: vscode.TextDocument): boolean {
  if (document.uri.scheme === "untitled") {
    return true;
  }
  return (
    FILE_LIKE_SCHEMES.includes(document.uri.scheme) &&
    (vscode.workspace.workspaceFolders?.length ?? 0) === 0
  );
}

/**
 * The key a document with no walked file is filed under. Unsaved buffers are
 * keyed by their URI, whose `untitled:` prefix cannot collide with a
 * workspace-relative path; a lone file in a folderless window is keyed by its
 * own path, since `asRelativePath` has no root to strip.
 */
function documentKey(document: vscode.TextDocument, includeFolder: boolean): string {
  return document.uri.scheme === "untitled"
    ? document.uri.toString()
    : normalisePath(vscode.workspace.asRelativePath(document.uri, includeFolder));
}

/**
 * Rewrite backslashes as slashes, collapse duplicate slashes and strip a
 * leading `./`. Windows spells `uri.fsPath` with backslashes, so
 * `${workspaceFolder}/file.s` substitutes to a mix of both separators; on a
 * virtual workspace root, whose `fsPath` is a bare separator, it gives
 * `\/file.s`. A path on a Windows drive is lowercased whole: those paths are
 * case-insensitive, and `uri.fsPath` lowercases the drive letter where a
 * hand-written `C:\...` does not.
 */
function normalisePath(path: string): string {
  const normalised = path
    .replace(/\\/g, "/")
    .replace(/\/{2,}/g, "/")
    .replace(/^\.\//, "");
  return /^\/?[a-z]:/i.test(normalised) ? normalised.toLowerCase() : normalised;
}

/**
 * The spellings a configured `program` may match: the raw value, its
 * normalised path, and (for URI-shaped values) the URI's path component.
 */
function programKeys(program: string): Set<string> {
  const keys = new Set<string>([program, normalisePath(program)]);
  // A scheme of two characters or more, so that a Windows path is not read as
  // a URI with a one-letter drive-letter scheme.
  if (/^[a-z][a-z0-9+.-]+:/i.test(program)) {
    try {
      const uri = vscode.Uri.parse(program);
      keys.add(uri.toString());
      keys.add(normalisePath(uri.path));
      keys.add(normalisePath(uri.fsPath));
    } catch {
      // Not a URI after all; the raw spellings above still apply.
    }
  }
  return keys;
}

/** Whether a collected file, filed under the already-normalised `key`, is the
 * one `wanted` (a `programKeys` set) names. */
function matchesProgram(uri: vscode.Uri, key: string, wanted: Set<string>): boolean {
  return (
    wanted.has(uri.toString()) ||
    wanted.has(normalisePath(uri.fsPath)) ||
    wanted.has(normalisePath(uri.path)) ||
    wanted.has(key)
  );
}
