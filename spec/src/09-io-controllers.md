# I/O Controllers

*This chapter is informative. The I/O controllers are defined in the reference card but are not implemented in the current emulator.*

The Z33 uses **port-mapped I/O**. Controllers are accessed using the privileged `in` and `out` instructions, with port addresses identifying specific controller registers.

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
