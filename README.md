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
[path to]/z33-cli run samples/fact.S main
```

## Interactive mode

```sh
[path to]/z33-cli run -i samples/fact.S main
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

Releasing a new version is done by running doing the following steps:

 - Change the crate version in the `Cargo.toml`. This can be automated using [`cargo-edit`](https://github.com/killercup/cargo-edit):
   ```sh
   # Edit the bump flag accordingly
   cargo set-version --bump patch
   # Crate version can be extracted like that
   VERSION="v$(cargo metadata --format-version=1 | jq -r '.packages[] | select(.name == "z33-cli").version')"
   ```
 - Commit the changes
   ```sh
   git commit -m "${VERSION}" ./Cargo.lock ./Cargo.toml
   git push
   ```
 - Create a new git tag and push it
   ```sh
   git tag -s "${VERSION}"
   git push --tags
   ```
 - Wait for the CI to create the draft GitHub release
 - Finish the release from the GitHub interface

<details><summary>Full script</summary>

```sh
cargo set-version --bump patch
VERSION="v$(cargo metadata --format-version=1 | jq -r '.packages[] | select(.name == "z33-cli").version')"
git commit -m "${VERSION}" ./Cargo.lock ./Cargo.toml
git push
git tag -s "${VERSION}"
git push --tags
```

</details>

# License

[MIT](./LICENSE) © [Quentin Gliech](https://sandhose.fr)
