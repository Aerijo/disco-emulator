use std::fmt;

use crate::RegFormat;
use crate::peripherals::Peripheral;
use crate::utils::io::read_register;
use crate::utils::bits::{bitset, add_with_carry, shift, shift_c, align};
use crate::instruction::{CarryChange, Condition, Instruction, ShiftType};


#[derive(Debug)]
struct APSR {
    // B1.4.2
    n: bool, // negative
    z: bool, // zero
    c: bool, // carry
    v: bool, // overflow
    q: bool, // saturation
    ge: u8,
}

impl fmt::Display for APSR {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let neg = if self.n { 'N' } else { '_' };
        let zero = if self.z { 'Z' } else { '_' };
        let carry = if self.c { 'C' } else { '_' };
        let over = if self.v { 'V' } else { '_' };
        let sat = if self.q { 'Q' } else { '_' };
        return write!(f, "APSR: {}{}{}{}{}", neg, zero, carry, over, sat);
    }
}

impl APSR {
    fn new() -> APSR {
        return APSR {
            n: false,
            z: true,
            c: true,
            v: false,
            q: false,
            ge: 0,
        };
    }
}

#[derive(Debug)]
struct IPSR {
    // B1.4.2
    exception: u32,
}

impl IPSR {
    fn new() -> IPSR {
        return IPSR {
            exception: 0,
        }
    }
}

#[derive(Debug)]
struct EPSR {
    // B1.4.2
    it_ici: u32,
    t: bool, // thumb mode
}

impl EPSR {
    fn new() -> EPSR {
        return EPSR {
            it_ici: 0,
            t: true,
        };
    }
}

#[derive(Debug)]
struct Control {
    spsel: bool,
    n_priv: bool,
    fpca: bool,
}

impl Control {
    fn new() -> Control {
        return Control {
            spsel: false,
            n_priv: false,
            fpca: false,
        };
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ExecMode {
    // B1.4.7 p521
    ModeThread,
    ModeHandler,
}

#[derive(Debug)]
pub struct CPU {
    registers: [u32; 16],
    instr_pc: u32,
    sp_unpredictable: bool,
    sp_main: u32,
    sp_process: u32,
    apsr: APSR,
    ipsr: IPSR,
    epsr: EPSR,
    control: Control,
    pub current_mode: ExecMode,
}

impl CPU {
    pub fn new() -> CPU {
        return CPU {
            registers: [0; 16],
            instr_pc: 0,
            sp_unpredictable: false, // TODO: Ensure this value is maintained
            sp_main: 0,
            sp_process: 0,
            apsr: APSR::new(),
            ipsr: IPSR::new(),
            epsr: EPSR::new(),
            control: Control::new(),
            current_mode: ExecMode::ModeThread,
        };
    }

    pub fn read_reg(&self, reg: u8) -> u32 {
        assert!(reg <= 15);
        return match reg {
            13 => self.read_sp(),
            15 => self.read_pc(),
            _ => self.registers[reg as usize],
        }
    }

    fn raise_unpredictable(&self) {
        println!("Unpredictable SP access");
    }

    pub fn read_sp(&self) -> u32 {
        if self.sp_unpredictable {
            self.raise_unpredictable();
            return 0;
        }
        return self.registers[13] & !0b11;
    }

    pub fn read_pc(&self) -> u32 {
        return self.registers[15];
    }

    pub fn read_instruction_pc(&self) -> u32 {
        return self.instr_pc;
    }

    pub fn write_instruction_pc(&mut self, address: u32) {
        self.instr_pc = address;
    }

    pub fn read_aligned_pc(&self) -> u32 {
        return self.read_pc() & !0b11;
    }

    pub fn inc_pc(&mut self, wide: bool) {
        self.instr_pc += if wide { 4 } else { 2 };
    }

    pub fn update_instruction_address(&mut self) -> u32 {
        self.registers[15] = self.instr_pc + 4;
        return self.instr_pc;
    }

    pub fn write_reg(&mut self, reg: u8, val: u32) {
        assert!(reg <= 15);
        self.registers[reg as usize] = val;
    }

    pub fn write_sp(&mut self, val: u32) {
        self.registers[13] = val & !0b11;
    }

    pub fn check_condition(&self, cond: Condition) -> bool {
        return match cond {
            Condition::Equal => self.apsr.z,
            Condition::NotEqual => !self.apsr.z,
            Condition::CarrySet => self.apsr.c,
            Condition::CarryClear => !self.apsr.c,
            Condition::Negative => self.apsr.n,
            Condition::PosOrZero => self.apsr.n,
            Condition::Overflow => self.apsr.v,
            Condition::NotOverflow => !self.apsr.v,
            Condition::UHigher => self.apsr.c && !self.apsr.z,
            Condition::ULowerSame => !self.apsr.c || self.apsr.z,
            Condition::SHigherSame => self.apsr.n == self.apsr.v,
            Condition::Slower => self.apsr.n != self.apsr.v,
            Condition::SHigher => !self.apsr.z && (self.apsr.n == self.apsr.v),
            Condition::SLowerSame => self.apsr.z || (self.apsr.n != self.apsr.v),
            Condition::Always => true,
            Condition::Never => false,
        };
    }

    pub fn set_negative_flag(&mut self, enabled: bool) {
        self.apsr.n = enabled;
    }

    pub fn set_zero_flag(&mut self, enabled: bool) {
        self.apsr.z = enabled;
    }

    pub fn set_carry_flag(&mut self, enabled: bool) {
        self.apsr.c = enabled;
    }

    pub fn read_carry_flag(&self) -> bool {
        return self.apsr.c;
    }

    pub fn set_overflow_flag(&mut self, enabled: bool) {
        self.apsr.v = enabled;
    }

    pub fn set_saturation_flag(&mut self, enabled: bool) {
        self.apsr.q = enabled;
    }

    pub fn set_thumb_mode(&mut self, enabled: bool) {
        self.epsr.t = enabled;
    }

    pub fn read_thumb_mode(&self) -> bool {
        return self.epsr.t;
    }

    pub fn get_apsr_display(&self) -> String {
        return format!("{}", self.apsr);
    }
}

impl fmt::Display for CPU {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut registers = String::new();
        let indent = "    ";

        for i in 0..4 {
            let left = self.read_reg(i);
            let right = self.read_reg(i + 8);
            let left_label = format!("r{}", i);
            let right_label = format!("r{}", i + 8);
            registers.push_str(&format!(
                "{}{: >3}: {: <34}  {: >3}: {: <34}\n",
                indent, left_label, left, right_label, right
            ));
        }
        registers.push('\n');
        for i in 4..8 {
            let left = self.read_reg(i);
            let right = self.read_reg(i + 8);
            let special = ["r12", "sp", "lr", "pc"];
            let left_label = format!("r{}", i);

            registers.push_str(&format!(
                "{}{: >3}: {: <34}  {: >3}: {: <34}\n",
                indent,
                left_label,
                left,
                special[(i - 4) as usize],
                right
            ));
        }
        registers.push('\n');
        registers.push_str(&format!("{}{}\n", indent, self.apsr));
        return write!(f, "CPU {{\n{}}}", registers);
    }
}
