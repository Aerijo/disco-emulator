use crate::peripherals::Peripheral;
use crate::utils::io::read_register;

#[derive(Debug)]
pub struct LCD {
    cr: u32,
    fcr: u32,
    sr: u32,
    clr: u32,
    ram: [u32; 8],
}

impl LCD {
    fn new() -> LCD {
        return LCD {
            cr: 0x0000_0000,
            fcr: 0x0000_0000,
            sr: 0x0000_0020,
            clr: 0x0000_0000,
            ram: [0x0000_0000; 8],
        }
    }
}

impl Default for LCD {
    fn default() -> LCD {
        return LCD::new();
    }
}

impl Peripheral for LCD {
    fn read(&self, offset: u32, size: usize) -> Result<u32, String> {
        return match offset {
            0x00..=0x03 => read_register(self.cr, offset, size),
            0x04..=0x07 => read_register(self.fcr, offset - 0x04, size),
            0x08..=0x0B => read_register(self.cr, offset - 0x08, size),
            0x0C..=0x0F => read_register(self.clr, offset - 0x0C, size),
            0x14..=0x53 => read_register(self.ram[((offset - 0x14) / 32) as usize], offset, size),
            _ => Err(format!("unimplemented LCD register")),
        }
    }

    fn write(&mut self, address: u32, _size: usize) -> Result<(), String> {
        return Err(format!("LCD write at {:#010X} is unimplemented", address));
    }

    fn reset(&mut self) {

    }
}


// Address regions that are used to access peripherals rather than memory should
// be marked as Device memory. Depending upon the processor, this may be configured
// in the Memory Protection Unit (MPU) or the Memory Management Unit (MMU). Unaligned
// accesses are not permitted to these regions even when unaligned access support is
// enabled.  If an unaligned access is attempted, the processor will take an abort.
// http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.faqs/ka15414.html
