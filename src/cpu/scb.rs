#[derive(Debug)]
pub struct Scb {
    cpu_id: CpuId,
    icsr: Icsr,
    vtor: Vtor,
    aircr: Aircr,
    scr: Scr,
    ccr: Ccr,
    shpr: Shpr,
    shcsr: Shcsr,
    cfsr: Cfsr,
    hfsr: Hsfr,
    dfsr: Dfsr,
    mmfar: Mmfar,
    bfar: Bfar,
    afsr: Afsr,
    cpacr: Cpacr,
}

impl Scb {
    fn new() -> Scb {
        return Scb {
            registers: Box<
        }
    }

    fn reset(&mut self) {
        self.cpu_id.reset();
        self.icsr.reset();
        self.vtor.reset();
        self.scr.reset();
        self.cpu_id.reset();
    }
}

#[derive(Debug)]
struct CpuId {
    value: u32,
}

impl CpuId {
    fn new() -> CpuId {
        return CpuId {
            value: 0x41 << 24 | 0x0 << 20 | 0xF << 16 | 0xC24 << 4 | 0x1;
        }
    }

    fn reset(&mut self) {
        // Do nothing
    }

    fn read(&self) -> Result<u32, ()> {
        return Ok(self.value);
    }

    fn write(&mut self, value: u32) -> Result<(), ()> {
        return Err(());
    }
}

#[derive(Debug)]
struct Icsr {
    value: u32,
}



impl Iscr {
    fn new() -> Icsr {
        return Icsr {
            value: 0,
        }
    }

    fn reset(&mut self) {
        self.value = 0;
    }
}

#[derive(Debug)]
struct Vtor {
    value: u32,
}

impl Vtor {
    fn new() -> Vtor {
        return Vtor {
            value: 0,
        }
    }

    fn reset(&mut self) {
        self.value = 0;
    }

    fn read(&self) -> Result<u32, ()> {
        return self.value;
    }

    fn write(&mut self, value: u32) -> Result<(), ()> {
        self.value = value & !0x3F;
    }
}
