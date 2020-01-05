// General interface to 'data' locations. Includes code, sram, scs, peripherals, etc.
// Validation is to be done by the caller; these methods will simply read/write the
// value, and panic if out of range.
pub trait Memory {
    const BASE_ADDRESS: usize;
    pub fn read_word(&self, address: u32) -> u32;
    pub fn write_word(&mut self, address: u32, value: u32);
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Location {
    Flash,
    Sram,
    Peripheral, // we keep the passed address, and resolve in more detail
    Scs, // System control space
    OutOfBounds,
}

impl Location {
    fn from_address(address: u32) -> Location {
        return match address {
            0x0000_0000..=0x000F_FFFF => Location::Flash,
            0x0800_0000..=0x080F_FFFF => Location::Flash,
            0x2000_0000..=0x2001_FFFF => Location::Sram,
            0x4000_0000..=0x5FFF_FFFF => Location::Peripheral,
            0xE000_0000..=0xE000_EFFF => Location::Scs,
            _ => Location::OutOfBounds,
        }
    }
}

impl Board {
    fn read_word(&mut self, address: u32) -> Result<u32, ()> {
        let value = match Location::from_address(address) {
            Location::Flash => self.flash.read_word(address),
            Location::Sram => self.data.read_word(address),
            Location::Peripheral => self.read_peripheral(address),
            Location::Scs => self.read_scs(address),
            Location::OutOfBounds => {
                return Err(());
            }
        }
        return Ok(value);
    }

    fn write_word(&mut self, address: u32, value: u32) -> Result<(), ()> {
        match Location::from_address(address) {
            Location::Flash => self.flash.write_word(address),
            Location::Sram => self.data.write_word(address),
            Location::Peripheral => self.write_peripheral(address),
            Location::Scs => self.write_scs(address),
            Location::OutOfBounds => {
                return Err(());
            }
        }
        return Ok(());
    }
}
