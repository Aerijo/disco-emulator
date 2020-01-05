pub struct Flash {
    contents: Box<[u8; Flash::SIZE]>,
}

impl Flash {
    const SIZE: usize = 1024 * 1024;

    fn new() -> Flash {
        return Flash {
            contents: Box::new([0; Flash::SIZE]),
        }
    }

    fn normalize_address(address: u32) -> u32 {
        return address & !0x0800_0000;
    }

    fn read_instruction(&self, address: u32) -> u32 {
        return self.read_word(address).rotate_right(16);
    }
}

impl Memory for Flash {
    fn read_word(&self, address: u32) -> u32 {
        let base = usize::try_from(Flash::normalize_address(address)).expect("Cannot convert u32 (address) to usize");
        return read_word(&*self.contents, base);
    }

    fn write_word(&mut self, address: u32, value: u32) {
        panic!("Cannot write to flash memory");
    }
}
