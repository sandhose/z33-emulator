# Zorglub-33 emulator

This is an emulator for the z33 architecture used in the _"Architecture des Systèmes d'Exploitation"_ course at the University of Strasbourg.

## Getting the emulator

### Binaries

Binaries for [the latest release](https://github.com/sandhose/z33-emulator/releases/latest) are available here:

- [Linux (x86 64bit)](https://github.com/sandhose/z33-emulator/releases/latest/download/z33-cli-x86_64-linux.tar.gz)
- [Linux (ARM 64bit)](https://github.com/sandhose/z33-emulator/releases/latest/download/z33-cli-aarch64-linux.tar.gz)
- [macOS (x86 64bit)](https://github.com/sandhose/z33-emulator/releases/latest/download/z33-cli-x86_64-macos.tar.gz)
- [macOS (ARM 64bit)](https://github.com/sandhose/z33-emulator/releases/latest/download/z33-cli-aarch64-macos.tar.gz)
- [Windows (x86 64bit)](https://github.com/sandhose/z33-emulator/releases/latest/download/z33-cli-x86_64-windows.exe)
- [Windows (ARM 64bit)](https://github.com/sandhose/z33-emulator/releases/latest/download/z33-cli-aarch64-windows.exe)

#### Using the binaries on macOS

The macOS binaries are not signed, and macOS will quarantine them by default.
This can be fixed by removing the quarantine flag on the binary:

```sh
xattr -r -d com.apple.quarantine [path to]/z33-cli
```

### Compiling

Alternatively, it can be compiled from source with a recent enough Rust compiler (>= 1.40).
Check the official documentation on [how to install Rust](https://www.rust-lang.org/tools/install).

```sh
git clone https://github.com/sandhose/z33-emulator
cd z33-emulator
cargo build --release
# Binary is available under `target/release/z33-cli'
```

## Running a program

```sh
[path to]/z33-cli run samples/fact.s main
```

## Interactive mode

```sh
[path to]/z33-cli run -i samples/fact.s main
```

### Available commands:

- `help [command]`: Print the help message of a command
- `memory <address> [n]`: Show a block of memory. The address can be either a register with or without offset (e.g. `%sp-5`) or a literal (e.g. `100`). The second argument is the number of cells to show (one by default).
- `registers [register]`: Show the value of a register. If no register is specified, shows the value of all five of them.
- `list`: Show the code that will be run next.
- `step [n]`: Run `n` step of the program (one by default).
- `break <address>`: Set a breakpoint at given address.
- `unbreak <address>`: Remove a breakpoint at given address.
- `info breakpoints`: Show the list of breakpoints
- `continue`: Run the code until the next breakpoint
- `interrupt`: Trigger a hardware interrupt
- `exit`: Exit the emulator

## Releasing

Releases are cut from the GitHub Actions UI:

 1. Run the **Release** workflow on `main` with the new version number
    (e.g. `0.8.0`). It bumps every manifest (workspace crates, VS Code
    extension, Zed extension, tree-sitter grammar) through
    `scripts/set-version.sh` and opens a `Release vX.Y.Z` pull request
    with CI running on it.
 2. Merge the pull request. The merge commit is tagged automatically,
    the binaries and extensions are built, and the GitHub release is
    published with generated notes and a `SHA256SUMS` file.

# License

[MIT](./LICENSE) © [Quentin Gliech](https://sandhose.fr)
