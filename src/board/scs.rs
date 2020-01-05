pub struct Scs {
    contents: Box<[u8; Scs::SIZE]>,
}

impl Scs {
    const SIZE: usize = 1024 * 4;

    fn new() -> Scs {
        return Scs {
            contents: Box::new([0; Scs::SIZE]),
        };
    }
}

impl Board {
    fn raise_exception() {
        panic!();
    }

    fn read_scs(&mut self, address: u32) -> Result<u32, ()> {
        assert!(0xE000_E000 <= address && address <= 0xE000_EFFF);
        if !word_aligned(address) {
            self.raise_exception();
            return Err(());
        }

        let value = match address {
            _ => {
                read_word(self.scs, address - 0xE000_E000);
            }
        };

        return Ok(value);
    }

    // TODO: Can a write fail? What happens if it does?
    fn write_scs(&mut self, address: u32, value: u32) {
        assert!(0xE000_E000 <= address && address <= 0xE000_EFFF);
        if !word_aligned(address) {
            self.raise_exception();
            return Err(());
        }

        match address {
            0xE000_ED00 => {},
            0xE000_ED04 => self.write_scs_icsr(value),
            0xE000_ED08 => self.write_scs_vtor(value),
            _ => {
                panic!("Unimplemented write address for SCS {}", address);
            }
        }
    }

    fn write_scs_icsr(&mut self, value: u32) {
        let original = read_word(self.scs, 0xD04);
        let new = (original | value) & !(0b11 << 29 | 1 << 24 | 1 << 21 | 0b11 << 9);
        let changed = new_bits_set(original, new);
        write_word(self.scs, new & !(1 << 28));
        if changed > 0 {
            self.on_did_change_iscr(changed);
        }
    }

    fn write_scs_vtor(&mut self, value: u32) {
        write_word(self.scs, value & !0x3F);
    }
}
