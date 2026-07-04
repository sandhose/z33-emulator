# I/O Controllers

The Z33 uses **port-mapped I/O**. Controllers are accessed using the privileged `in` and `out` instructions, with port addresses identifying specific controller registers.

The keyboard and disk controllers below are **informative**: they are defined in the reference card but are not implemented by the current emulator. The **serial controller** *is* implemented.

## General Behavior

r[io.unmapped]
Reading from a port that no controller handles yields 0, and writing to such a port is ignored. No exception is raised for unmapped ports.

r[io.interrupt.delivery]
A controller may raise a **hardware interrupt** (exception code 0). The interrupt is delivered *between instructions*: after an instruction completes, if the interrupt-enable bit (`%sr.IE`) is set and a controller has a pending interrupt, the processor enters the exception handler as described in [Exceptions and Interrupts](06-exceptions.md). If `%sr.IE` is clear, the pending interrupt is held and delivered as soon as interrupts are re-enabled.

Since there is a single exception vector and all hardware interrupts report code 0, the handler must inspect each controller's status register (its I bit) to discover which device requires servicing.

## Keyboard Controller

The keyboard controller occupies ports **20–21**.

### Status Register — Port 20 (Read-only)

```
Bit:  ...  2    1    0
       ...  -    I    R
```

| Bit | Name | Description |
|---|---|---|
| 0 | R (Ready) | 1 if data is ready to be read |
| 1 | I (Interrupt) | 1 if the controller has generated an interrupt |

Reading the status register **clears the I bit**.

### Control Register — Port 20 (Write-only)

```
Bit:  ...  1    0
       ...  I    L
```

| Bit | Name | Description |
|---|---|---|
| 0 | L (LED) | 1 to switch on/off a keyboard LED |
| 1 | I (Interrupt) | 1 to generate an interrupt when data is available |

### Data Register — Port 21 (Read)

```
Bit:  ...  9    8    7..4    3..0
       ...  C    S    col     row
```

| Field | Description |
|---|---|
| row (bits 3–0) | Row number of the pressed key |
| col (bits 7–4) | Column number of the pressed key |
| S (bit 8) | 1 if Shift key is pressed |
| C (bit 9) | 1 if Ctrl key is pressed |

### Data Register — Port 21 (Write)

```
Bit:  ...  3    2..0
       ...  S    led
```

| Field | Description |
|---|---|
| led (bits 2–0) | LED number to act on |
| S (bit 3) | 1 to switch the LED on, 0 to switch it off |

## Disk Controller

The disk controller occupies ports **50–100**.

### Status Register — Port 50 (Read-only)

```
Bit:  ...  3    2    1    0
       ...  E    I    R    A
```

| Bit | Name | Description |
|---|---|---|
| 0 | A (Available) | 1 if the disk is idle and available for a new request |
| 1 | R (Ready) | 1 if data is ready to read |
| 2 | I (Interrupt) | 1 if the controller has generated an interrupt |
| 3 | E (Error) | 1 if an error has been detected |

Reading the status register **clears the I and E bits**.

### Control Register — Port 50 (Write-only)

```
Bit:  ...  3    2    1    0
       ...  I    L    R    W
```

| Bit | Name | Description |
|---|---|---|
| 0 | W (Write) | 1 to start a write request |
| 1 | R (Read) | 1 to start a read request |
| 2 | L (Location) | 1 if data registers hold C/H/S (Cylinder/Head/Sector) |
| 3 | I (Interrupt) | 1 to generate an interrupt when the transfer is done |

### Data Registers — Ports 51–100 (Read/Write)

Each port holds one byte of data representing either:
- The sector location in C/H/S format (when the L bit is set in the control register), or
- Data to be read from or written to the disk

## Serial Controller

The serial controller occupies ports **110–111**. Unlike the keyboard and disk controllers, it **is implemented** by the emulator: it models a byte-oriented console connected to the host. Received bytes are queued for the program to read; transmitted bytes are buffered for the host to display.

Transmission is **instantaneous** — writing a byte never blocks, and the transmit-ready bit is always set. Reception is host-driven: the host delivers bytes into the receive queue, which the program drains one byte at a time.

### Status Register — Port 110 (Read-only)

```
Bit:  ...  2    1    0
       ...  I    T    R
```

| Bit | Name | Description |
|---|---|---|
| 0 | R (Ready) | 1 if a received byte is waiting to be read (the receive queue is non-empty) |
| 1 | T (Transmit) | Always 1 — transmission is instantaneous, so the controller is always ready to send |
| 2 | I (Interrupt) | 1 if the controller has generated an interrupt |

r[io.serial.status.rx-ready]
The R bit is 1 exactly when the receive queue is non-empty.

r[io.serial.status.tx-ready]
The T bit is always 1: a write to the data register never blocks.

r[io.serial.status.interrupt]
The I bit is 1 when the controller has raised an interrupt that has not yet been acknowledged.

r[io.serial.status.read-clears-interrupt]
Reading the status register **clears the I bit** (the same idiom as the keyboard and disk controllers).

### Control Register — Port 110 (Write-only)

```
Bit:  ...  1    0
       ...  -    E
```

| Bit | Name | Description |
|---|---|---|
| 0 | E (Enable) | 1 to generate a hardware interrupt whenever a byte is received |

r[io.serial.control.rx-interrupt-enable]
Writing the control register with the E bit set arms receive interrupts; writing it clear disarms them.

r[io.serial.interrupt.raise]
While receive interrupts are armed (E set), each byte delivered into the receive queue raises a hardware interrupt.

r[io.serial.interrupt.enable-with-queued]
Arming receive interrupts (writing E from 0 to 1) while the receive queue is already non-empty raises a hardware interrupt immediately, so a program never misses input that arrived before it enabled interrupts.

r[io.serial.interrupt.edge]
Each raised interrupt is delivered to the processor at most once. Acknowledging the interrupt in the status register (clearing the I bit) is independent from this delivery.

### Data Register — Port 111 (Read)

r[io.serial.data.read]
Reading the data register removes and returns the byte at the front of the receive queue, as a word in the range 0–255. If the receive queue is empty, the read returns 0. **A read never blocks**, even when no byte is available; programs busy-poll the R bit to wait for input.

### Data Register — Port 111 (Write)

r[io.serial.data.write]
Writing the data register appends the low 8 bits of the value (`value & 0xFF`) to the transmit buffer for the host to display. The write never blocks.

### Line Convention

The host sends the byte `\n` (line feed, 10) for the Enter key, and programs should emit `\n` to advance to a new line.
