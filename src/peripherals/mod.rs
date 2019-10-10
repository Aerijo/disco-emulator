mod lcd;
use lcd::LCD;
mod rcc;
use rcc::RCC;

pub trait Peripheral {
    fn read(&self, offset: u32, size: usize) -> Result<u32, String>;
    fn write(&mut self, offset: u32, size: usize) -> Result<(), String>;
    fn reset(&mut self);
}

#[derive(Default, Debug)]
struct UnimplementedPeripheral {}

impl Peripheral for UnimplementedPeripheral {
    fn read(&self, offset: u32, _size: usize) -> Result<u32, String> {
        return Err(format!("Offset at {:#010X} is unimplemented", offset));
    }

    fn write(&mut self, offset: u32, _size: usize) -> Result<(), String> {
        return Err(format!("Offset at {:#010X} is unimplemented", offset));
    }

    fn reset(&mut self) {}
}

#[derive(Default, Debug)]
pub struct Peripherals {
    tim1: UnimplementedPeripheral,
    tim2: UnimplementedPeripheral,
    tim3: UnimplementedPeripheral,
    tim4: UnimplementedPeripheral,
    tim5: UnimplementedPeripheral,
    tim6: UnimplementedPeripheral,
    tim7: UnimplementedPeripheral,
    tim8: UnimplementedPeripheral,
    tim15: UnimplementedPeripheral,
    tim16: UnimplementedPeripheral,
    tim17: UnimplementedPeripheral,

    lptim1: UnimplementedPeripheral,
    lptim2: UnimplementedPeripheral,

    lcd: LCD,
    rtc: UnimplementedPeripheral,
    wwdg: UnimplementedPeripheral,
    iwdg: UnimplementedPeripheral,

    spi1: UnimplementedPeripheral,
    spi2: UnimplementedPeripheral,
    spi3: UnimplementedPeripheral,

    sai1: UnimplementedPeripheral,
    sai2: UnimplementedPeripheral,

    usart1: UnimplementedPeripheral,
    usart2: UnimplementedPeripheral,
    usart3: UnimplementedPeripheral,

    uart4: UnimplementedPeripheral,
    uart5: UnimplementedPeripheral,

    i2c1: UnimplementedPeripheral,
    i2c2: UnimplementedPeripheral,
    i2c3: UnimplementedPeripheral,

    dma1: UnimplementedPeripheral,
    dma2: UnimplementedPeripheral,

    gpioa: UnimplementedPeripheral,
    gpiob: UnimplementedPeripheral,
    gpioc: UnimplementedPeripheral,
    gpiod: UnimplementedPeripheral,
    gpioe: UnimplementedPeripheral,
    gpiof: UnimplementedPeripheral,
    gpiog: UnimplementedPeripheral,
    gpioh: UnimplementedPeripheral,

    rcc: RCC,
    flash: UnimplementedPeripheral,
    crc: UnimplementedPeripheral,
    tsc: UnimplementedPeripheral,
    can1: UnimplementedPeripheral,
    pwr: UnimplementedPeripheral,
    dac: UnimplementedPeripheral,
    opamp: UnimplementedPeripheral,
    lpuart1: UnimplementedPeripheral,
    swpmi1: UnimplementedPeripheral,
    syscfg: UnimplementedPeripheral,
    vrefbuf: UnimplementedPeripheral,
    comp: UnimplementedPeripheral,
    exti: UnimplementedPeripheral,
    firewall: UnimplementedPeripheral,
    sdmmc1: UnimplementedPeripheral,
    dsfdm: UnimplementedPeripheral,
    otg_fs: UnimplementedPeripheral,
    adc: UnimplementedPeripheral,
    rng: UnimplementedPeripheral,
}

impl Peripherals {
    pub fn new() -> Peripherals {
        return Peripherals { ..Default::default() };
    }

    pub fn read(&self, address: u32, size: usize) -> Result<u32, String> {
        println!("Reading peripheral at {:#10X}", address);
        assert!(size == 1 || size == 2 || size == 4);
        if !(0x4000_0000..=0x5FFF_FFFF).contains(&address) {
            panic!("Unexpected address {:#010X} for peripheral read", address);
        }

        return match address {
            0x4000_0000..=0x4000_97FF => self.read_apb1(address, size),
            0x4001_0000..=0x4001_63FF => self.read_apb2(address, size),
            0x4002_0000..=0x4002_43FF => self.read_ahb1(address, size),
            0x4002_4400..=0x5006_0BFF => self.read_ahb2(address, size),
            _ => Ok(0), // reserved regions just return 0
        }
    }

    fn read_apb1(&self, address: u32, size: usize) -> Result<u32, String> {
        if !(0x4000_0000..=0x4000_97FF).contains(&address) {
            panic!("Unexpected address {:#010X} for APB1 bus", address);
        }

        return match address {
            0x4000_0000..=0x4000_03FF => self.tim2.read(address - 0x4000_0000, size),
            0x4000_0400..=0x4000_07FF => self.tim3.read(address - 0x4000_0400, size),
            0x4000_0800..=0x4000_0BFF => self.tim4.read(address - 0x4000_0800, size),
            0x4000_0C00..=0x4000_0FFF => self.tim5.read(address - 0x4000_0C00, size),
            0x4000_1000..=0x4000_13FF => self.tim6.read(address - 0x4000_1000, size),
            0x4000_1400..=0x4000_17FF => self.tim7.read(address - 0x4000_1400, size),
            0x4000_2400..=0x4000_27FF => self.lcd.read(address - 0x4000_2400, size),
            0x4000_2800..=0x4000_2BFF => self.rtc.read(address - 0x4000_2800, size),
            0x4000_2C00..=0x4000_2FFF => self.wwdg.read(address - 0x4000_2C00, size),
            0x4000_3000..=0x4000_33FF => self.iwdg.read(address - 0x4000_3000, size),
            0x4000_3800..=0x4000_3BFF => self.spi2.read(address - 0x4000_3800, size),
            0x4000_3C00..=0x4000_3FFF => self.spi3.read(address - 0x4000_3C00, size),
            0x4000_4400..=0x4000_47FF => self.usart2.read(address - 0x4000_4400, size),
            0x4000_4800..=0x4000_4BFF => self.usart3.read(address - 0x4000_4800, size),
            0x4000_4C00..=0x4000_4FFF => self.uart4.read(address - 0x4000_4C00, size),
            0x4000_5000..=0x4000_53FF => self.uart5.read(address - 0x4000_5000, size),
            0x4000_5400..=0x4000_57FF => self.i2c1.read(address - 0x4000_5400, size),
            0x4000_5800..=0x4000_5BFF => self.i2c2.read(address - 0x4000_5800, size),
            0x4000_5C00..=0x4000_5FFF => self.i2c3.read(address - 0x4000_5C00, size),
            0x4000_6400..=0x4000_67FF => self.can1.read(address - 0x4000_6400, size),
            0x4000_7000..=0x4000_73FF => self.pwr.read(address - 0x4000_7000, size),
            0x4000_7400..=0x4000_77FF => self.dac.read(address - 0x4000_7400, size),
            0x4000_7800..=0x4000_7BFF => self.opamp.read(address - 0x4000_7800, size),
            0x4000_7C00..=0x4000_7FFF => self.lptim1.read(address - 0x4000_7C00, size),
            0x4000_8000..=0x4000_83FF => self.lpuart1.read(address - 0x4000_8000, size),
            0x4000_8800..=0x4000_8BFF => self.swpmi1.read(address - 0x4000_8800, size),
            0x4000_9400..=0x4000_97FF => self.lptim2.read(address - 0x4000_9400, size),
            _ => Ok(0),
        }
    }

    fn read_apb2(&self, address: u32, size: usize) -> Result<u32, String> {
        return match address {
            0x4001_0000..=0x4001_002F => self.syscfg.read(address - 0x4001_0000, size),
            0x4001_0030..=0x4001_01FF => self.vrefbuf.read(address - 0x4001_0030, size),
            0x4001_0200..=0x4001_03FF => self.comp.read(address - 0x4001_0200, size),
            0x4001_0400..=0x4001_07FF => self.exti.read(address - 0x4001_0400, size),
            0x4001_1C00..=0x4001_1FFF => self.firewall.read(address - 0x4001_1C00, size),
            0x4001_2800..=0x4001_2BFF => self.sdmmc1.read(address - 0x4001_2800, size),
            0x4001_2C00..=0x4001_2FFF => self.tim1.read(address - 0x4001_2C00, size),
            0x4001_3000..=0x4001_33FF => self.spi1.read(address - 0x4001_3000, size),
            0x4001_3400..=0x4001_37FF => self.tim8.read(address - 0x4001_3400, size),
            0x4001_3800..=0x4001_3BFF => self.usart1.read(address - 0x4001_3800, size),
            0x4001_4000..=0x4001_43FF => self.tim15.read(address - 0x4001_4000, size),
            0x4001_4400..=0x4001_47FF => self.tim16.read(address - 0x4001_4400, size),
            0x4001_4800..=0x4001_4BFF => self.tim17.read(address - 0x4001_4800, size),
            0x4001_5400..=0x4001_57FF => self.sai1.read(address - 0x4001_5400, size),
            0x4001_5800..=0x4001_5BFF => self.sai2.read(address - 0x4001_5800, size),
            0x4001_6000..=0x4001_63FF => self.dsfdm.read(address - 0x4001_6000, size),
            _ => Ok(0),
        }
    }

    fn read_ahb1(&self, address: u32, size: usize) -> Result<u32, String> {
        return match address {
            0x4002_0000..=0x4002_03FF => self.dma1.read(address - 0x4002_0000, size),
            0x4002_0400..=0x4002_07FF => self.dma2.read(address - 0x4002_0400, size),
            0x4002_1000..=0x4002_13FF => self.rcc.read(address - 0x4002_1000, size),
            0x4002_2000..=0x4002_23FF => self.flash.read(address - 0x4002_2000, size),
            0x4002_3000..=0x4002_33FF => self.crc.read(address - 0x4002_3000, size),
            0x4002_4000..=0x4002_43FF => self.tsc.read(address - 0x4002_4000, size),
            _ => Ok(0),
        }
    }

    fn read_ahb2(&self, address: u32, size: usize) -> Result<u32, String> {
        return match address {
            0x4800_0000..=0x4800_03FF => self.gpioa.read(address - 0x4800_0000, size),
            0x4800_0400..=0x4800_07FF => self.gpiob.read(address - 0x4800_0400, size),
            0x4800_0800..=0x4800_0BFF => self.gpioc.read(address - 0x4800_0800, size),
            0x4800_0C00..=0x4800_0FFF => self.gpiod.read(address - 0x4800_0C00, size),
            0x4800_1000..=0x4800_13FF => self.gpioe.read(address - 0x4800_1000, size),
            0x4800_1400..=0x4800_17FF => self.gpiof.read(address - 0x4800_1400, size),
            0x4800_1800..=0x4800_1BFF => self.gpiog.read(address - 0x4800_1800, size),
            0x4800_1C00..=0x4800_1FFF => self.gpioh.read(address - 0x4800_1C00, size),
            0x5000_0000..=0x5003_FFFF => self.otg_fs.read(address - 0x5000_0000, size),
            0x5004_0000..=0x5004_03FF => self.adc.read(address - 0x5004_0000, size),
            0x5006_0800..=0x5006_0BFF => self.rng.read(address - 0x5006_0800, size),
            _ => Ok(0),
        }
    }

    pub fn write(&mut self, _value: u32, _address: u32, _size: usize) -> Result<(), String> {
        return Err(format!("Not implemented yet"));
    }
}
