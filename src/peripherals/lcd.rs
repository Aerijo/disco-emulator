use crate::peripherals::Peripheral;

#[derive(Debug)]
pub struct LCD {
    cr: u32,
    fcr: u32,
}

impl LCD {
    fn new() -> LCD {
        return LCD {
            cr: 0x0000_0000,
            fcr: 0x0000_0000,
        }
    }
}

impl Default for LCD {
    fn default() -> LCD {
        return LCD::new();
    }
}

impl Peripheral for LCD {
    fn read(&self, _offset: u32, _size: usize) -> Result<u32, String> {
        // return match offset {
        //     0x00..=0x03 => self.read_cr(size),
        //     0x04..=0x07 => self.read_fcr(size),
        //     _ => Err(format!("unimplemented LCD register")),
        // }

        return Ok(self.cr);
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
