### Compiling

```sh
cargo build --release
```

### Running a program

```sh
./target/release/z33-emulator run samples/fact.S main
```

### Interactive mode

```sh
./target/release/z33-emulator run -i samples/fact.S main
```

#### Available commands:

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

# License

[MIT](./LICENSE) © [Quentin Gliech](https://sandhose.fr)
