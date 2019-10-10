use crate::peripherals::Peripheral;

#[derive(Debug)]
pub struct RCC {
    cr: u32,
    icscr: u32,
    cfgr: u32,
    pllcfgr: u32,
    pllsai1cfgr: u32,
    pllsai2cfgr: u32,
    cier: u32,
    cifr: u32,
    cicr: u32,
    ahb1rstr: u32,
    ahb2rstr: u32,
    ahb3rstr: u32,
    apb1rstr1: u32,
    apb1rstr2: u32,
    apb2rstr: u32,
    ahb1enr: u32,
    ahb2enr: u32,
    ahb3enr: u32,
    apb1enr1: u32,
    apb1enr2: u32,
    apb2enr: u32,
    ahb1smenr: u32,
    ahb2smenr: u32,
    ahb3smenr: u32,
    apb1smenr1: u32,
    apb1smenr2: u32,
    apb2smenr: u32,
    ccipr: u32,
    bdcr: u32,
    csr: u32,
}

impl RCC {
    fn new() -> RCC {
        return RCC {
            cr: 0x0000_0063,
            icscr: 0x1071_0096,
            cfgr: 0x0000_0000,
            pllcfgr: 0x0000_1000,
            pllsai1cfgr: 0x0000_1000,
            pllsai2cfgr: 0x0000_1000,
            cier: 0x0000_0000,
            cifr: 0x0000_0000,
            cicr: 0x0000_0000,
            ahb1rstr: 0x0000_0000,
            ahb2rstr: 0x0000_0000,
            ahb3rstr: 0x0000_0000,
            apb1rstr1: 0x0000_0000,
            apb1rstr2: 0x0000_0000,
            apb2rstr: 0x0000_0000,
            ahb1enr: 0x0000_0100,
            ahb2enr: 0x0000_0000,
            ahb3enr: 0x0000_0000,
            apb1enr1: 0x0000_0000,
            apb1enr2: 0x0000_0000,
            apb2enr: 0x0000_0000,
            ahb1smenr: 0x0001_1303,
            ahb2smenr: 0x0005_32FF,
            ahb3smenr: 0x0000_0101,
            apb1smenr1: 0xF2FE_CA3F,
            apb1smenr2: 0x0000_0025,
            apb2smenr: 0x0167_7C01,
            ccipr: 0x0000_0000,
            bdcr: 0x0000_0000,
            csr: 0x0C00_0600,
        }
    }
}

impl Default for RCC {
    fn default() -> RCC {
        return RCC::new();
    }
}

impl Peripheral for RCC {
    fn read(&self, _offset: u32, _size: usize) -> Result<u32, String> {
        // return match offset {
        //     0x00..=0x03 => self.read_cr(size),
        //     0x04..=0x07 => self.read_fcr(size),
        //     _ => Err(format!("unimplemented LCD register")),
        // }

        println!("Returning {:#010X}", self.icscr);
        return Ok(self.icscr);
    }

    fn write(&mut self, address: u32, _size: usize) -> Result<(), String> {
        return Err(format!("RCC write at {:#010X} is unimplemented", address));
    }

    fn reset(&mut self) {
        // TODO: Distinguish reset kinds
        self.cr = (self.cr & 0x0004_0000) | 0x0000_0063; // HSEBYP not affected
        self.icscr = 0x1071_0096;
        self.cfgr = 0x0000_0000;
        self.pllcfgr = 0x0000_1000;
        self.pllsai1cfgr = 0x0000_1000;
        self.pllsai2cfgr = 0x0000_1000;
        self.cier = 0x0000_0000;
        self.cifr = 0x0000_0000;
        self.cicr = 0x0000_0000;
        self.ahb1rstr = 0x0000_0000;
        self.ahb2rstr = 0x0000_0000;
        self.ahb3rstr = 0x0000_0000;
        self.apb1rstr1 = 0x0000_0000;
        self.apb1rstr2 = 0x0000_0000;
        self.apb2rstr = 0x0000_0000;
        self.ahb1enr = 0x0000_0100;
        self.ahb2enr = 0x0000_0000;
        self.ahb3enr = 0x0000_0000;
        self.apb1enr1 = 0x0000_0000;
        self.apb1enr2 = 0x0000_0000;
        self.apb2enr = 0x0000_0000;
        self.ahb1smenr = 0x0001_1303;
        self.ahb2smenr = 0x0005_32FF;
        self.ahb3smenr = 0x0000_0101;
        self.apb1smenr1 = 0xF2FE_CA3F;
        self.apb1smenr2 = 0x0000_0025;
        self.apb2smenr = 0x0167_7C01;
        self.ccipr = 0x0000_0000;
        self.bdcr = 0x0000_0000;
        self.csr = 0x0C00_0600;
    }
}
