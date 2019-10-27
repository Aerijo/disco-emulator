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

#[derive(Debug)]
struct IPSR {
    // B1.4.2
    exception: u32,
}

#[derive(Debug)]
struct EPSR {
    // B1.4.2
    it_ici: u32,
    t: bool, // thumb mode
}

#[derive(Debug)]
pub struct CPU {
    registers: [u32; 16],
    apsr: APSR,
    ipsr: IPSR,
    epsr: EPSR,
}

impl CPU {
    pub fn new() -> CPU {
        return CPU {
            registers: [0; 16],
            apsr: APSR {
                n: false,
                z: true,
                c: true,
                v: false,
                q: false,
                ge: 0,
            },
            ipsr: IPSR {
                exception: 0,
            },
            epsr: EPSR {
                it_ici: 0,
                t: true,
            }
        };
    }

    pub fn read_reg(&self, reg: u8) -> u32 {
        // This is a "true" assertion, as registers come
        // from the machine code, so should never be out of range.
        assert!(reg <= 15);
        return self.registers[reg as usize];
    }

    pub fn write_reg(&mut self, reg: u8, val: u32) {
        assert!(reg <= 15);
        self.registers[reg as usize] = val;
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
