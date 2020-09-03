use crate::processor::{Memory, Registers};

fn print_empty_line(address: usize) {
    println!("{:#06x} 00 00...", address);
}
fn print_line(address: usize, line: &[u8]) {
    print!("{:#06x}", address);
    for (i, ref byte) in line.iter().enumerate() {
        if i % 8 == 0 {
            print!(" ")
        }
        print!("{:02x} ", byte);
    }
    println!("");
}

pub fn display_registers(reg: Registers) {
    println!(
        " A: {a:#06x}  B: {b:#06x} SP: {sp:#06x}",
        a = reg.a,
        b = reg.b,
        sp = reg.sp
    );
    println!(
        "PC: {pc:#06x} SR: {sr:#06x} ({sr:?})",
        pc = reg.pc,
        sr = reg.sr
    );
}

pub fn display_memory(mem: Memory) {
    let mut skipping = false;
    for i in 0..0x0FFF {
        let line = &mem[(i * 0x10)..((i + 1) * 0x10)];
        if line == [0; 0x10] {
            if skipping {
                continue;
            }

            skipping = true;
            print_empty_line(i * 0x10);
        } else {
            skipping = false;
            print_line(i * 0x10, line);
        }
    }
}
