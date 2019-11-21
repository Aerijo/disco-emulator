pub mod io;
pub mod bits;

pub fn read_word(bank: &[u8], base: usize) -> u32 {
    use std::convert::TryInto;
    return u32::from_le_bytes(bank[base..base+4].try_into().expect("slice with incorrect length"));
}

pub fn read_value(bank: &[u8], base: usize, size: usize) -> u32 {
    assert!(size == 1 || size == 2 || size == 4);
    let mut result: u32 = 0;
    for i in (0..size).rev() {
        result = result << 8;
        result += bank[base + i] as u32;
    }
    return result;
}

pub fn write_value(mut value: u32, bank: &mut[u8], base: usize, size: usize) {
    assert!(size == 1 || size == 2 || size == 4);
    for i in 0..size {
        bank[base + i] = (value & 0xFF) as u8;
        value = value >> 8;
    }
}
