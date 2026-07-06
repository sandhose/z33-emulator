//! Port-mapped I/O devices.
//!
//! The Z33 accesses peripherals through the privileged `in`/`out` instructions,
//! which read from and write to numbered *ports*. Each controller occupies a
//! contiguous range of ports and exposes a small set of registers (status,
//! control, data...) following the idiom described in the reference card.
//!
//! Only the serial console (ports 110–111) is implemented by the emulator. The
//! [`Device`] trait is the seam through which future controllers (keyboard,
//! disk...) can be plugged in.

use std::collections::VecDeque;

use crate::constants::{Address, Word};

/// Port range of the serial console controller (inclusive).
const SERIAL_STATUS_PORT: Address = 110;
const SERIAL_DATA_PORT: Address = 111;

/// Status register bits (port 110, read).
const STATUS_RX_READY: Word = 0b001; // R: input queue non-empty
const STATUS_TX_READY: Word = 0b010; // T: transmit ready (always 1)
const STATUS_INTERRUPT: Word = 0b100; // I: controller raised an interrupt

/// Control register bits (port 110, write).
const CONTROL_RX_INTERRUPT_ENABLE: Word = 0b001; // E: interrupt on receive

/// A port-mapped I/O device.
///
/// Devices are addressed by port number. The [`IoBus`] routes `in`/`out`
/// accesses to whichever device [`handles`](Device::handles) the port. Devices
/// may also raise hardware interrupts, which are delivered between instructions
/// (see [`Device::poll_interrupt`]).
pub trait Device {
    /// Whether this device answers to the given port.
    fn handles(&self, port: Address) -> bool;

    /// Read a word from one of the device's registers.
    fn read(&mut self, port: Address) -> Word;

    /// Write a word to one of the device's registers.
    fn write(&mut self, port: Address, value: Word);

    /// Consume and return a pending interrupt edge, if any.
    ///
    /// Returns `true` at most once per raised interrupt (edge-consumed). The
    /// device's status-register interrupt bit is independent from this edge and
    /// is only cleared by reading the status register.
    fn poll_interrupt(&mut self) -> bool {
        false
    }
}

/// A simple serial console (ports 110–111).
///
/// Transmission is instantaneous: writing to the data register appends to an
/// output buffer that the host drains, and the transmit-ready bit is always
/// set. Reception is host-driven: the host pushes bytes into an input queue,
/// which the program pops through the data register. Reads never block.
#[derive(Debug, Default, Clone)]
pub struct SerialConsole {
    /// Bytes received from the host, waiting to be read by the program.
    input: VecDeque<u8>,

    /// Bytes written by the program, waiting to be drained by the host.
    output: Vec<u8>,

    /// The E control bit: raise a hardware interrupt when a byte is received.
    irq_on_ready: bool,

    /// The I status bit: an interrupt has been raised and not yet acknowledged
    /// (cleared by reading the status register).
    interrupt_flag: bool,

    /// A pending interrupt edge, consumed once by [`Self::poll_interrupt`].
    pending_irq: bool,
}

impl SerialConsole {
    /// Deliver bytes from the host into the receive queue.
    ///
    /// If receive interrupts are enabled ([`Self::irq_on_ready`]), delivering
    /// bytes raises a hardware interrupt.
    pub fn push_input(&mut self, bytes: &[u8]) {
        if bytes.is_empty() {
            return;
        }
        self.input.extend(bytes.iter().copied());
        // r[impl io.serial.interrupt.raise]
        if self.irq_on_ready {
            self.raise_interrupt();
        }
    }

    /// Take everything the program has written, leaving the buffer empty.
    #[must_use]
    pub fn drain_output(&mut self) -> Vec<u8> {
        std::mem::take(&mut self.output)
    }

    /// Whether there is at least one byte waiting to be read.
    // r[impl io.serial.status.rx-ready]
    #[must_use]
    pub fn input_ready(&self) -> bool {
        !self.input.is_empty()
    }

    /// Raise a hardware interrupt: set the I status bit and arm the edge.
    fn raise_interrupt(&mut self) {
        self.interrupt_flag = true;
        self.pending_irq = true;
    }

    /// Read the status register, computing the R/T/I bits.
    ///
    /// Reading the status register clears the I bit (but not a pending edge
    /// that [`Self::poll_interrupt`] may still deliver).
    // r[impl io.serial.status.tx-ready]
    // r[impl io.serial.status.interrupt]
    fn read_status(&mut self) -> Word {
        let mut status = STATUS_TX_READY;
        if self.input_ready() {
            status |= STATUS_RX_READY;
        }
        if self.interrupt_flag {
            status |= STATUS_INTERRUPT;
        }
        // r[impl io.serial.status.read-clears-interrupt]
        self.interrupt_flag = false;
        status
    }

    /// Write the control register.
    // r[impl io.serial.control.rx-interrupt-enable]
    fn write_control(&mut self, value: Word) {
        let enabled = value & CONTROL_RX_INTERRUPT_ENABLE != 0;
        self.irq_on_ready = enabled;
        // Enabling receive interrupts while bytes are already queued raises the
        // interrupt straight away, so the program never misses input delivered
        // before it armed the controller.
        // r[impl io.serial.interrupt.enable-with-queued]
        if enabled && self.input_ready() {
            self.raise_interrupt();
        }
    }

    /// Read the data register: pop the front byte, or 0 if the queue is empty.
    // r[impl io.serial.data.read]
    fn read_data(&mut self) -> Word {
        self.input.pop_front().map_or(0, Word::from)
    }

    /// Write the data register: append the low byte to the output buffer.
    // r[impl io.serial.data.write]
    fn write_data(&mut self, value: Word) {
        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        let byte = (value & 0xFF) as u8;
        self.output.push(byte);
    }
}

impl Device for SerialConsole {
    fn handles(&self, port: Address) -> bool {
        port == SERIAL_STATUS_PORT || port == SERIAL_DATA_PORT
    }

    fn read(&mut self, port: Address) -> Word {
        match port {
            SERIAL_STATUS_PORT => self.read_status(),
            SERIAL_DATA_PORT => self.read_data(),
            _ => 0,
        }
    }

    fn write(&mut self, port: Address, value: Word) {
        match port {
            SERIAL_STATUS_PORT => self.write_control(value),
            SERIAL_DATA_PORT => self.write_data(value),
            _ => {}
        }
    }

    // r[impl io.serial.interrupt.edge]
    fn poll_interrupt(&mut self) -> bool {
        std::mem::take(&mut self.pending_irq)
    }
}

/// The I/O bus: routes port accesses to the attached devices.
///
/// Devices are held as concrete typed fields (not `Box<dyn Device>`) so hosts
/// can reach into them without downcasting; the [`Device`] trait is only the
/// routing seam.
#[derive(Debug, Default, Clone)]
pub struct IoBus {
    /// The serial console (ports 110–111).
    pub serial: SerialConsole,
}

impl IoBus {
    /// Read from the port, routing to the device that handles it.
    ///
    /// Unmapped ports read as 0.
    // r[impl io.unmapped]
    #[must_use]
    pub fn read(&mut self, port: Address) -> Word {
        if self.serial.handles(port) {
            self.serial.read(port)
        } else {
            0
        }
    }

    /// Write to the port, routing to the device that handles it.
    ///
    /// Writes to unmapped ports are ignored.
    // r[impl io.unmapped]
    pub fn write(&mut self, port: Address, value: Word) {
        if self.serial.handles(port) {
            self.serial.write(port, value);
        }
    }

    /// Consume a pending interrupt edge from any device.
    // r[impl io.interrupt.delivery]
    pub fn poll_interrupt(&mut self) -> bool {
        self.serial.poll_interrupt()
    }
}

#[cfg(test)]
mod tests {
    #![allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]

    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn status_reflects_rx_and_tx() {
        // r[verify io.serial.status.rx-ready]
        // r[verify io.serial.status.tx-ready]
        let mut serial = SerialConsole::default();
        // Empty queue: only T is set.
        assert_eq!(serial.read(SERIAL_STATUS_PORT), STATUS_TX_READY);
        assert!(!serial.input_ready());

        serial.push_input(b"a");
        assert!(serial.input_ready());
        // R and T are set, I is not (interrupts disabled).
        assert_eq!(
            serial.read(SERIAL_STATUS_PORT),
            STATUS_RX_READY | STATUS_TX_READY
        );
    }

    #[test]
    fn status_read_clears_interrupt_bit() {
        // r[verify io.serial.status.interrupt]
        // r[verify io.serial.status.read-clears-interrupt]
        let mut serial = SerialConsole::default();
        serial.write(SERIAL_STATUS_PORT, CONTROL_RX_INTERRUPT_ENABLE);
        serial.push_input(b"x");

        let status = serial.read(SERIAL_STATUS_PORT);
        assert_eq!(status & STATUS_INTERRUPT, STATUS_INTERRUPT);
        // A second read no longer reports the interrupt.
        let status = serial.read(SERIAL_STATUS_PORT);
        assert_eq!(status & STATUS_INTERRUPT, 0);
    }

    #[test]
    fn data_pop_and_empty_reads_zero() {
        // r[verify io.serial.data.read]
        let mut serial = SerialConsole::default();
        serial.push_input(b"hi");
        assert_eq!(serial.read(SERIAL_DATA_PORT), Word::from(b'h'));
        assert_eq!(serial.read(SERIAL_DATA_PORT), Word::from(b'i'));
        // Queue drained: reads return 0 and never block.
        assert_eq!(serial.read(SERIAL_DATA_PORT), 0);
        assert!(!serial.input_ready());
    }

    #[test]
    fn data_write_masks_to_byte_and_drains() {
        // r[verify io.serial.data.write]
        let mut serial = SerialConsole::default();
        serial.write(SERIAL_DATA_PORT, Word::from(b'A'));
        // High bits are dropped: 0x141 & 0xFF == 0x41 == 'A'.
        serial.write(SERIAL_DATA_PORT, 0x141);
        assert_eq!(serial.drain_output(), b"AA");
        // Draining leaves the buffer empty.
        assert!(serial.drain_output().is_empty());
    }

    #[test]
    fn unmapped_ports_are_lenient() {
        // r[verify io.unmapped]
        let mut bus = IoBus::default();
        assert_eq!(bus.read(0), 0);
        assert_eq!(bus.read(999), 0);
        // Writing to an unmapped port is a no-op and does not touch the serial
        // console.
        bus.write(0, 42);
        assert!(bus.serial.drain_output().is_empty());
    }

    #[test]
    fn poll_interrupt_is_edge_consumed() {
        // r[verify io.serial.interrupt.edge]
        // r[verify io.serial.interrupt.raise]
        // r[verify io.serial.control.rx-interrupt-enable]
        let mut serial = SerialConsole::default();
        serial.write(SERIAL_STATUS_PORT, CONTROL_RX_INTERRUPT_ENABLE);
        serial.push_input(b"z");
        // The edge is delivered exactly once.
        assert!(serial.poll_interrupt());
        assert!(!serial.poll_interrupt());
        // Without interrupts enabled, pushing raises no edge.
        let mut serial = SerialConsole::default();
        serial.push_input(b"z");
        assert!(!serial.poll_interrupt());
    }

    #[test]
    fn enabling_interrupt_with_queued_input_raises() {
        // r[verify io.serial.interrupt.enable-with-queued]
        let mut serial = SerialConsole::default();
        // Bytes arrive before interrupts are enabled: no edge yet.
        serial.push_input(b"q");
        assert!(!serial.poll_interrupt());
        // Enabling E while input is queued raises the interrupt.
        serial.write(SERIAL_STATUS_PORT, CONTROL_RX_INTERRUPT_ENABLE);
        assert!(serial.poll_interrupt());
        assert_eq!(
            serial.read(SERIAL_STATUS_PORT) & STATUS_INTERRUPT,
            STATUS_INTERRUPT
        );
    }

    #[test]
    fn status_read_preserves_pending_edge() {
        // The I status bit and the pending interrupt edge are independent:
        // clearing the former (by reading the status register) does not
        // consume the latter.
        // r[verify io.serial.status.read-clears-interrupt]
        // r[verify io.serial.interrupt.edge]
        let mut serial = SerialConsole::default();
        serial.write(SERIAL_STATUS_PORT, CONTROL_RX_INTERRUPT_ENABLE);
        serial.push_input(b"x");

        // Reading the status register clears the I bit...
        let status = serial.read(SERIAL_STATUS_PORT);
        assert_eq!(status & STATUS_INTERRUPT, STATUS_INTERRUPT);
        let status = serial.read(SERIAL_STATUS_PORT);
        assert_eq!(status & STATUS_INTERRUPT, 0);

        // ...but the interrupt edge is still delivered.
        assert!(serial.poll_interrupt());
    }

    #[test]
    fn push_input_empty_is_noop() {
        // An empty delivery raises no interrupt and leaves R clear.
        // r[verify io.serial.status.rx-ready]
        let mut serial = SerialConsole::default();
        serial.write(SERIAL_STATUS_PORT, CONTROL_RX_INTERRUPT_ENABLE);
        serial.push_input(&[]);
        assert!(!serial.input_ready());
        assert!(!serial.poll_interrupt());
        assert_eq!(serial.read(SERIAL_STATUS_PORT) & STATUS_RX_READY, 0);
    }

    #[test]
    fn bus_routes_serial_ports() {
        // r[verify io.interrupt.delivery]
        let mut bus = IoBus::default();
        bus.serial.push_input(b"k");
        assert_eq!(bus.read(SERIAL_DATA_PORT), Word::from(b'k'));
        bus.write(SERIAL_DATA_PORT, Word::from(b'k'));
        assert_eq!(bus.serial.drain_output(), b"k");
        // No interrupt pending by default.
        assert!(!bus.poll_interrupt());
    }
}
