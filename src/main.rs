#![allow(dead_code)]
#![allow(unused_imports)]

#[macro_use]
extern crate lazy_static;
extern crate goblin;
extern crate regex;

mod instruction;
use instruction::{bitset, bitset16, add_with_carry, shift, shift_c, align, CarryChange, Condition, Instruction, RegFormat, ShiftType};

mod peripherals;
use peripherals::Peripherals;

use goblin::elf::Elf;
use regex::Regex;
use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::io::{self, Write};
use std::num::Wrapping;
use std::option::Option;
use std::string::String;
use std::time::SystemTime;
use std::vec::Vec;

#[derive(Debug)]
enum Exception {
    Reset,
    NonMaskableInterrupt,
    HardFault,
    MemManage,
    BusFault,
    UsageFault,
    DebugMonitor,
    SupervisorCall,
    PendSV,
    SysTick,
}

#[derive(Debug)]
enum AccessType {
    Normal,
}

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
struct CPU {
    registers: [u32; 16],
    apsr: APSR,
    ipsr: IPSR,
    epsr: EPSR,
}

impl CPU {
    fn new() -> CPU {
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

    fn read_reg(&self, reg: u8) -> u32 {
        // This is a "true" assertion, as registers come
        // from the machine code, so should never be out of range.
        assert!(reg <= 15);
        return self.registers[reg as usize];
    }

    fn write_reg(&mut self, reg: u8, val: u32) {
        assert!(reg <= 15);
        self.registers[reg as usize] = val;
    }

    fn check_condition(&self, cond: Condition) -> bool {
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

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Location {
    Flash(usize),
    Ram(usize),
    Peripheral(u32), // we keep the passed address, and resolve in more detail
}

struct MemoryBus {
    flash: [u8; 1024 * 1024],
    data: [u8; 128 * 1024],
    peripherals: Peripherals
}

impl fmt::Debug for MemoryBus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "<data>");
    }
}

fn iter_print(data: &[u8], start_index: usize, amount: usize) {
    let mut c = 16;
    for i in start_index..(start_index + amount) {
        if c == 0 {
            c = 16;
            print!("\n");
        }
        c -= 1;
        print!("{:02X} ", data[i]);
    }
    print!("\n");
}

fn read_value(bank: &[u8], base: usize, size: usize) -> Result<u32, String> {
    assert!(size == 1 || size == 2 || size == 4);
    let mut result: u32 = 0;
    for i in (0..size).rev() {
        result = result << 8;
        result += bank[base + i] as u32;
    }
    return Ok(result);
}

fn write_value(mut value: u32, bank: &mut[u8], base: usize, size: usize) -> Result<(), String> {
    assert!(size == 1 || size == 2 || size == 4);
    for i in 0..size {
        bank[base + i] = (value & 0xFF) as u8;
        value = value >> 8;
    }
    return Ok(());
}

impl MemoryBus {
    fn new() -> MemoryBus {
        return MemoryBus {
            flash: [0xFF; 1024 * 1024],
            data: [0xFF; 128 * 1024],
            peripherals: Peripherals::new(),
        };
    }

    fn load_elf(&mut self, elf: Elf, bytes: &[u8]) -> Result<(), String> {
        for header in elf.program_headers.iter() {
            if header.p_type != goblin::elf::program_header::PT_LOAD {
                return Err(String::from("Unexpected program header type"));
            }

            if header.p_vaddr < 0x0800_0000 {
                continue;
            }

            let phys_adr = header.p_paddr as usize;
            let offset = header.p_offset as usize;
            let size = header.p_filesz as usize;

            let start_index = phys_adr - 0x0800_0000;

            if start_index + size > self.flash.len() {
                return Err(String::from("Flash too small to fit content"));
            }

            for i in 0..size {
                self.flash[i + start_index] = bytes[i + offset];
            }
        }
        return Ok(());
    }

    fn print_mem_dump(&self, index: usize, length: usize) {
        let mut c = 16;
        for i in (index..(index + length * 4)).step_by(4) {
            if c == 0 {
                c = 16;
                print!("\n");
            }
            c -= 1;
            let val = match self.read_word(i as u32) {
                Ok(v) => v,
                Err(e) => {
                    println!("{}", e);
                    return;
                }
            };
            print!("{:#010X} ", val);
        }
        print!("\n");
    }

    fn print_raw_mem_dump(&self, index: usize, length: usize) {
        if index >= 0x2000_0000 {
            iter_print(&self.data, index - 0x2000_0000, length);
        } else if index >= 0x0800_0000 {
            iter_print(&self.flash, index - 0x0800_0000, length);
        }
    }

    fn get_instr_word(&self, address: u32) -> Result<u32, String> {
        if 0x08000000 <= address && address <= 0x08000000 + (self.flash.len() as u32) {
            let base = (address - 0x08000000) as usize;
            let b1 = self.flash[base] as u32;
            let b2 = self.flash[base + 1] as u32;
            let b3 = self.flash[base + 2] as u32;
            let b4 = self.flash[base + 3] as u32;
            return Ok((b2 << 24) + (b1 << 16) + (b4 << 8) + b3);
        }

        return Err(String::from("Out of bounds access"));
    }

    fn read_mem_a(&self, address: u32, size: usize) -> Result<u32, String> {
        // B2.3.4 p583
        return self.read_mem_a_with_priv(address, size, &AccessType::Normal);
    }

    fn read_mem_a_with_priv(&self, address: u32, size: usize, _access_type: &AccessType) -> Result<u32, String> {
        // B2.3.4 p583
        if address != align(address, size as u32) {
            // Set UFSR.UNALIGNED = true;
            panic!("UsageFault");
        }

        // let memaddrdesc = validate_address(address, access_type, false); // TODO
        let location = self.address_to_physical(address)?;
        return match location {
            Location::Flash(i) => read_value(&self.flash, i, size),
            Location::Ram(i) => read_value(&self.data, i, size),
            Location::Peripheral(i) => self.peripherals.read(i, size),
        };
    }

    fn read_mem_u(&self, address: u32, size: usize) -> Result<u32, String> {
        // B2.3.5 p584
        return self.read_mem_u_with_priv(address, size, &AccessType::Normal);
    }

    fn read_mem_u_with_priv(&self, address: u32, size: usize, access_type: &AccessType) -> Result<u32, String> {
        // B2.3.5 p585
        if address == align(address, size as u32) {
            return self.read_mem_a_with_priv(address, size, access_type);
        } else if /* CCR.UNALIGN_TRP */ false {
            // USFR.UNALIGNED = true;
            panic!("UsageFault");
        } else {
            let mut result: u32 = 0;
            for i in 0..(size as u32) {
                result += self.read_mem_a_with_priv(address + i, 1, &access_type)? << (8 * i);
            }
            return Ok(result);
        }
    }

    fn address_to_physical(&self, address: u32) -> Result<Location, String> {
        let address = address as usize;
        let location = match address {
            0x0000_0000..=0x000F_FFFF => Location::Flash(address),
            0x0800_0000..=0x080F_FFFF => Location::Flash(address - 0x0800_0000),
            0x2000_0000..=0x2001_FFFF => Location::Ram(address - 0x2000_0000),
            0x4000_0000..=0x5FFF_FFFF => Location::Peripheral(address as u32),
            _ => {
                return Err(format!("Out of bounds memory access at {:#010X}", address));
            }
        };
        return Ok(location);
    }

    fn read_word(&self, address: u32) -> Result<u32, String> {
        return self.read_mem_u(address, 4);
    }

    fn read_halfword(&self, address: u32) -> Result<u16, String> {
        return Ok(self.read_mem_u(address, 2)? as u16);
    }

    fn read_byte(&self, address: u32) -> Result<u8, String> {
        return Ok(self.read_mem_u(address, 1)? as u8);
    }

    fn read_byte_unpriv(&self, address: u32) -> Result<u8, String> {
        return self.read_byte(address);
    }

    fn write_word(&mut self, address: u32, val: u32) -> Result<(), String> {
        let location = self.address_to_physical(address)?;
        return match location {
            Location::Flash(_) => Err(String::from("Cannot write to Flash memory")),
            Location::Ram(i) => write_value(val, &mut self.data, i, 4),
            Location::Peripheral(_) => Err(format!("Cannot write to peripherals ({:#010X}) yet", address)),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum ExecMode {
    // B1.4.7 p521
    ModeHandler,
    ModeThread,
}

#[derive(Debug)]
struct Board {
    cpu: CPU,
    memory: MemoryBus,
    current_mode: ExecMode,
    register_formats: [RegFormat; 16],
    branch_map: HashMap<u32, bool>,
}

impl Board {
    fn new() -> Board {
        return Board {
            cpu: CPU::new(),
            memory: MemoryBus::new(),
            current_mode: ExecMode::ModeThread,
            register_formats: [RegFormat::Hex; 16],
            branch_map: HashMap::new(),
        };
    }

    fn next_instruction(&mut self, update_pc: bool) -> Result<Instruction, String> {
        let pc = self.read_pc();
        let val = self.memory.get_instr_word(pc)?;

        println!("Reading: {:#010X}", val);

        // Most instructions that use the PC assume it is +4 bytes
        // from their position when calculating offsets (see page 124).
        let (instr, wide) = Instruction::from(val, pc + 4);

        println!("Instruction is {}", if wide { "wide" } else { "short" });

        if update_pc {
            self.inc_pc(wide);
        }

        return Ok(instr);
    }

    fn execute(&mut self, instr: Instruction) {
        match instr {
            Instruction::LdrLit { rt, address } => self.ldr_lit(rt, address),
            Instruction::LdrImm {
                rt,
                rn,
                offset,
                index,
                wback,
            } => self.ldr_imm(rt, rn, offset, index, wback),
            Instruction::MovImm {
                rd,
                val,
                setflags,
                carry,
            } => self.mov_imm(rd, val, setflags, carry),
            Instruction::MovReg { to, from, setflags } => {
                self.mov_reg(to, from, setflags)
            }
            Instruction::AddImm {
                dest,
                first,
                val,
                setflags,
            } => self.add_imm(dest, first, val, setflags),
            Instruction::AddReg { rd, rm, rn, shift_t, shift_n, setflags } => {
                self.add_reg(rd, rm, rn, shift_t, shift_n, setflags)
            }
            Instruction::SubReg { rd, rm, rn, shift_t, shift_n, setflags } => {
                self.sub_reg(rd, rm, rn, shift_t, shift_n, setflags)
            }
            Instruction::AndImm {
                rd,
                rn,
                val,
                setflags,
                carry,
            } => self.and_imm(rd, rn, val, setflags, carry),
            Instruction::Branch { address } => self.branch(address),
            Instruction::BranchExchange { rm } => self.branch_exchange(rm),
            Instruction::LinkedBranch { address } => self.branch_with_link(address),
            Instruction::CondBranch { address, cond } => self.branch_cond(address, cond),
            Instruction::CmpReg { rm, rn, shift_t, shift_n } => self.cmp_reg(rm, rn, shift_t, shift_n),
            Instruction::StrImm { rn, rt, offset, index, wback, } => self.str_imm(rt, rn, offset, index, wback),
            Instruction::Push { registers } => self.push(registers),
            Instruction::AddSpImm { rd, val, setflags } => self.add_sp_imm(rd, val, setflags),
            Instruction::LdrReg { rt, rn, rm, shift_t, shift_n } => self.ldr_reg(rt, rn, rm, shift_t, shift_n),
            Instruction::StrReg { rt, rn, rm, shift_t, shift_n } => self.str_reg(rt, rn, rm, shift_t, shift_n),
            Instruction::Undefined => {}
            Instruction::Unpredictable => {
                println!("Spooky");
            }
            Instruction::Unimplemented => {
                println!("Working on it");
            }
            _ => println!("Unhandled instruction"),
        }
    }

    fn load_elf_from_path(&mut self, path: &str) -> Result<(), String> {
        let bytes = match fs::read(path) {
            Ok(b) => b,
            Err(e) => {
                return Err(format!("Failed to read file \"{}\": {}", path, e));
            }
        };

        let elf = match Elf::parse(&bytes) {
            Ok(e) => e,
            Err(e) => {
                return Err(format!("Failed to parse elf file \"{}\": {}", path, e));
            }
        };

        for sym in elf.syms.iter() {
            let offset = sym.st_name;
            let name = match elf.strtab.get(offset) {
                Some(Ok(s)) => s,
                _ => return Err(String::from("missing symbols")),
            };

            if name == "SystemInit" || name == "__libc_init_array" || name == "init" {
                self.branch_map.insert((sym.st_value as u32) & !0b1, false);
            }
        }

        self.bx_write_pc(elf.entry as u32);

        match self.memory.load_elf(elf, &bytes) {
            Ok(_) => {}
            Err(e) => return Err(e),
        };

        return Ok(());
    }

    fn print_mem_dump(&mut self, index: usize, length: usize) {
        self.memory.print_mem_dump(index, length);
    }

    fn read_reg(&self, reg: u8) -> u32 {
        // TODO: Follow B1.4.7 p521
        return self.cpu.read_reg(reg);
    }

    fn write_reg(&mut self, reg: u8, val: u32) {
        // TODO: Follow B1.4.7 p521
        self.cpu.write_reg(reg, val);
    }

    fn get_register_display_value(&self, reg: u8) -> String {
        assert!(reg <= 15);
        let val = self.read_reg(reg);
        return match self.register_formats[reg as usize] {
            RegFormat::Bin => format!("{:#034b}", val),
            RegFormat::Oct => format!("{:#013o}", val),
            RegFormat::Dec => format!("{}", val),
            RegFormat::Sig => format!("{}", val as i32),
            RegFormat::Hex => format!("{:#010X}", val),
        };
    }

    fn set_display_format(&mut self, reg: u8, kind: RegFormat) {
        assert!(reg <= 15);
        self.register_formats[reg as usize] = kind;
    }

    fn read_sp(&self) -> u32 {
        return self.read_reg(13);
    }

    fn set_sp(&mut self, value: u32) {
        self.write_reg(13, value);
    }

    fn read_lr(&self) -> u32 {
        return self.read_reg(14);
    }

    fn set_lr(&mut self, value: u32) {
        self.write_reg(14, value);
    }

    fn read_pc(&self) -> u32 {
        return self.read_reg(15);
    }

    fn set_pc(&mut self, value: u32) {
        self.write_reg(15, value);
    }

    fn inc_pc(&mut self, wide: bool) {
        self.set_pc(self.read_pc() + if wide { 4 } else { 2 });
    }

    fn dec_pc(&mut self, wide: bool) {
        self.set_pc(self.read_pc() - if wide { 4 } else { 2 });
    }

    fn branch_to(&mut self, address: u32) {
        // B1.4.7 p522
        self.set_pc(address);
    }

    fn branch_write_pc(&mut self, address: u32) {
        // A2.3.1 p30
        self.branch_to(address & !0b1);
    }

    fn bx_write_pc(&mut self, address: u32) {
        // A2.3.1 p31
        if self.current_mode == ExecMode::ModeHandler && (address >> 28) == 0xF {
            panic!("TODO: ExceptionReturn(address & !(0xF << 28))");
        } else {
            self.blx_write_pc(address);
        }
    }

    fn blx_write_pc(&mut self, address: u32) {
        // A2.3.1 p31
        self.cpu.epsr.t = bitset(address, 0);
        if !self.cpu.epsr.t {
            panic!("self.raise_exception(Exception::UsageFault('Invalid State'))"); // TODO: Centralise exceptions
        }
        self.branch_to(address & !0b1);
    }

    fn load_write_pc(&mut self, address: u32) {
        // A2.3.1 p31
        self.bx_write_pc(address);
    }

    fn alu_write_pc(&mut self, address: u32) {
        // A2.3.1 p31
        self.branch_write_pc(address);
    }

    fn processor_id(&self) -> u32 {
        return 0;
    }

    fn clear_exclusive_local(&mut self, _processor_id: u32) {
        // B2.3.7 p587
        // TODO
    }

    fn set_exclusive_monitors(&mut self, _address: u32, _length: u32) {
        // TODO
    }

    /**
     * Instruction handlers
     */

    fn adc_imm(&mut self, rd: u8, rn: u8, imm32: u32, setflags: bool) {
        // A7.7.1
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), imm32, self.cpu.apsr.c as u32);
        self.write_reg(rd, result);
        if setflags {
            self.cpu.apsr.n = bitset(result, 31);
            self.cpu.apsr.z = result == 0;
            self.cpu.apsr.c = carry;
            self.cpu.apsr.v = overflow;
        }
    }

    fn adc_reg(&mut self, rd: u8, rm: u8, rn: u8, shift_t: ShiftType, shift_n: u32, setflags: bool) {
        // A7.7.2
        let shifted = shift(self.read_reg(rm), shift_t, shift_n, self.cpu.apsr.c as u32);
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), shifted, self.cpu.apsr.c as u32);
        self.write_reg(rd, result);
        if setflags {
            self.cpu.apsr.n = bitset(result, 31);
            self.cpu.apsr.z = result == 0;
            self.cpu.apsr.c = carry;
            self.cpu.apsr.v = overflow;
        }
    }

    fn add_imm(&mut self, rd: u8, rn: u8, imm32: u32, setflags: bool) {
        // A7.7.3
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), imm32, 0);
        self.write_reg(rd, result);
        if setflags {
            self.cpu.apsr.n = bitset(result, 31);
            self.cpu.apsr.z = result == 0;
            self.cpu.apsr.c = carry;
            self.cpu.apsr.v = overflow;
        }
    }

    fn add_reg(&mut self, rd: u8, rm: u8, rn: u8, shift_t: ShiftType, shift_n: u32, setflags: bool) {
        // A7.7.4
        let shifted = shift(self.read_reg(rm), shift_t, shift_n, self.cpu.apsr.c as u32);
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), shifted, 0);
        if rd == 15 {
            self.alu_write_pc(result);
        } else {
            self.write_reg(rd, result);
            if setflags {
                self.cpu.apsr.n = bitset(result, 31);
                self.cpu.apsr.z = result == 0;
                self.cpu.apsr.c = carry;
                self.cpu.apsr.v = overflow;
            }
        }
    }

    fn add_sp_imm(&mut self, rd: u8, imm32: u32, setflags: bool) {
        // A7.7.5
        let (result, carry, overflow) = add_with_carry(self.read_sp(), imm32, 0);
        self.write_reg(rd, result);
        if setflags {
            self.cpu.apsr.n = bitset(result, 31);
            self.cpu.apsr.z = result == 0;
            self.cpu.apsr.c = carry;
            self.cpu.apsr.v = overflow;
        }
    }

    fn add_sp_reg(&mut self, rd: u8, rm: u8, shift_t: ShiftType, shift_n: u32, setflags: bool) {
        // A7.7.6
        let shifted = shift(self.read_reg(rm), shift_t, shift_n, self.cpu.apsr.c as u32);
        let (result, carry, overflow) = add_with_carry(self.read_sp(), shifted, 0);
        if rd == 15 {
            self.alu_write_pc(result);
        } else {
            self.write_reg(rd, result);
            if setflags {
                self.cpu.apsr.n = bitset(result, 31);
                self.cpu.apsr.z = result == 0;
                self.cpu.apsr.c = carry;
                self.cpu.apsr.v = overflow;
            }
        }
    }

    fn adr(&mut self, rd: u8, address: u32) {
        // A7.7.7
        // NOTE: The offset calculation is determined by PC, so we precalculate it in the Instruction.
        self.write_reg(rd, address);
    }

    fn and_imm(&mut self, rd: u8, rn: u8, imm32: u32, setflags: bool, carry: CarryChange) {
        // A7.7.8
        let result = self.read_reg(rn) & imm32;
        self.write_reg(rd, result);

        if setflags {
            self.cpu.apsr.n = bitset(result, 31);
            self.cpu.apsr.z = result == 0;
            self.cpu.apsr.c = match carry {
                CarryChange::Same => self.cpu.apsr.c,
                CarryChange::Set => true,
                CarryChange::Clear => false,
            };
            // v unchanged
        }
    }

    fn and_reg(&mut self, rd: u8, rm: u8, rn: u8, shift_t: ShiftType, shift_n: u32, setflags: bool) {
        // A7.7.9
        let (shifted, carry) = shift_c(self.read_reg(rm), shift_t, shift_n, self.cpu.apsr.c as u32);
        let result = self.read_reg(rn) & shifted;
        self.write_reg(rd, result);
        if setflags {
            self.cpu.apsr.n = bitset(result, 31);
            self.cpu.apsr.z = result == 0;
            self.cpu.apsr.c = carry;
            // v unchanged
        }
    }

    fn asr_imm(&mut self, rd: u8, rm: u8, shift_n: u32, setflags: bool) {
        // A7.7.10
        let (result, carry) = shift_c(self.read_reg(rm), ShiftType::ASR, shift_n, self.cpu.apsr.c as u32);
        self.write_reg(rd, result);
        if setflags {
            self.cpu.apsr.n = bitset(result, 31);
            self.cpu.apsr.z = result == 0;
            self.cpu.apsr.c = carry;
            // v unchanged
        }
    }

    fn asr_reg(&mut self, rd: u8, rm: u8, rn: u8, setflags: bool) {
        // A7.7.11
        let shift_n = self.read_reg(rm) & 0xFF;
        let (result, carry) = shift_c(self.read_reg(rn), ShiftType::ASR, shift_n, self.cpu.apsr.c as u32);
        self.write_reg(rd, result);
        if setflags {
            self.cpu.apsr.n = bitset(result, 31);
            self.cpu.apsr.z = result == 0;
            self.cpu.apsr.c = carry;
            // v unchanged
        }
    }

    fn branch(&mut self, address: u32) {
        // A7.7.12
        self.branch_write_pc(address);
    }

    fn branch_cond(&mut self, address: u32, cond: Condition) {
        // NOTE: condition checking is defined in A7.3.1 p178
        // we will eventually need to generalise it (for IT blocks),
        // but for now we dedicate this separate function for conditional
        // branches.

        // A7.7.12 (see self.branch)
        if self.cpu.check_condition(cond) {
            self.branch_write_pc(address);
        }
    }

    fn bfc(&mut self, rd: u8, mask: u32) {
        // A7.7.13
        // NOTE: We precalculate the mask from the msbit and lsbit values.
        // TODO: Determine if UNPREDICTABLE at execution time is different
        // to treating the instruction as unpredictable.
        self.write_reg(rd, self.read_reg(rd) & mask);
    }

    fn bfi(&mut self, rd: u8, mask: u32) {
        // A7.7.14
        // NOTE: We precalculate the mask from the msbit and lsbit values.
        self.write_reg(rd, self.read_reg(rd) | mask);
    }

    fn bic_imm(&mut self, rd: u8, rn: u8, imm32: u32, setflags: bool, carry: CarryChange) {
        // A7.7.15
        let result = self.read_reg(rn) & !imm32;
        self.write_reg(rd, result);
        if setflags {
            self.cpu.apsr.n = bitset(imm32, 31);
            self.cpu.apsr.z = imm32 == 0;
            self.cpu.apsr.c = match carry {
                CarryChange::Same => self.cpu.apsr.c,
                CarryChange::Set => true,
                CarryChange::Clear => false,
            };
            // v unchanged
        }
    }

    fn bic_reg(&mut self, rd: u8, rm: u8, rn: u8, shift_t: ShiftType, shift_n: u32, setflags: bool) {
        // A7.7.16
        let (shifted, carry) = shift_c(self.read_reg(rm), shift_t, shift_n, self.cpu.apsr.c as u32);
        let result = self.read_reg(rn) & !shifted;
        self.write_reg(rd, result);
        if setflags {
            self.cpu.apsr.n = bitset(result, 31);
            self.cpu.apsr.z = result == 0;
            self.cpu.apsr.c = carry;
            // v unchanged
        }
    }

    fn bkpt(&mut self, _imm32: u32) {
        // A7.7.17
        // TODO: When return values supported, return a DebugMonitor exception with the input id
    }

    fn branch_with_link(&mut self, address: u32) {
        // A7.7.18
        // NOTE: To simplify the simulator, we allow skipping
        // certain functions. E.g., we don't need to set the clock
        // speed.
        println!("BMAP: {:?}", self.branch_map);
        match self.branch_map.get(&address) {
            Some(b) if !b => {
                println!("Skipping branch with link");
                return;
            }
            _ => {}
        }

        self.set_lr(self.read_pc() | 0b1);
        self.branch(address);
    }

    fn blx_reg(&mut self, rm: u8) {
        // A7.7.19
        let target = self.read_reg(rm);
        let next_instr_address = self.read_pc() + 2;
        self.set_lr(next_instr_address & 0b1);
        self.blx_write_pc(target);
    }

    fn branch_exchange(&mut self, rm: u8) {
        // A7.7.20
        self.bx_write_pc(self.cpu.read_reg(rm));
    }

    fn cbz(&mut self, rn: u8, address: u32, nonzero: bool) {
        // A7.7.21
        if nonzero != (self.read_reg(rn) == 0) {
            self.branch_write_pc(address);
        }
    }

    fn cpd(&mut self, _cp: u8) {
        // A7.7.22
        // TODO: Coprocessor stuff
        panic!("UsageFault");
    }

    fn clrex(&mut self) {
        // A7.7.23
        self.clear_exclusive_local(self.processor_id());
    }

    fn clz(&mut self, rd: u8, rm: u8) {
        // A7.7.24
        self.write_reg(rd, self.read_reg(rm).leading_zeros());
    }

    fn cmn_imm(&mut self, rn: u8, imm32: u32) {
        // A7.7.25
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), imm32, 0);
        self.cpu.apsr.n = bitset(result, 31);
        self.cpu.apsr.z = result == 0;
        self.cpu.apsr.c = carry;
        self.cpu.apsr.v = overflow;
    }

    fn cmn_reg(&mut self, rn: u8, rm: u8, shift_t: ShiftType, shift_n: u32) {
        // A7.7.26
        let shifted = shift(self.read_reg(rm), shift_t, shift_n, self.cpu.apsr.c as u32);
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), shifted, 0);
        self.cpu.apsr.n = bitset(result, 31);
        self.cpu.apsr.z = result == 0;
        self.cpu.apsr.c = carry;
        self.cpu.apsr.v = overflow;
    }

    fn cmp_imm(&mut self, rn: u8, imm32: u32) {
        // A7.7.27
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), !imm32, 1);
        self.cpu.apsr.n = bitset(result, 31);
        self.cpu.apsr.z = result == 0;
        self.cpu.apsr.c = carry;
        self.cpu.apsr.v = overflow;
    }

    fn cmp_reg(&mut self, rm: u8, rn: u8, shift_t: ShiftType, shift_n: u32) {
        // A7.7.28
        let shifted = shift(self.read_reg(rm), shift_t, shift_n, self.cpu.apsr.c as u32);
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), !shifted, 1);
        self.cpu.apsr.n = bitset(result, 31);
        self.cpu.apsr.z = result == 0;
        self.cpu.apsr.c = carry;
        self.cpu.apsr.v = overflow;
    }

    fn cps(&mut self, _enable: bool, _affect_pri: bool, _affect_fault: bool) {
        // A7.7.29
        // B5.2.1
        // TODO
    }

    // A7.7.30 is CPY, a deprecated alias for MOV

    fn csdb(&mut self) {
        // A7.7.31
        // TODO
    }

    fn dbg(&mut self, _option: u8) {
        // A7.7.32
        // TODO
    }

    fn dmb(&mut self, _option: u8) {
        // A7.7.33
        // TODO
    }

    fn dsb(&mut self, _option: u8) {
        // A7.7.34
        // TODO
    }

    fn eor_imm(&mut self, rd: u8, rn: u8, imm32: u32, setflags: bool, carry: CarryChange) {
        // A7.7.35
        let result = self.read_reg(rn) ^ imm32;
        self.write_reg(rd, result);
        if setflags {
            self.cpu.apsr.n = bitset(result, 31);
            self.cpu.apsr.z = result == 0;
            self.cpu.apsr.c = match carry {
                CarryChange::Same => self.cpu.apsr.c,
                CarryChange::Set => true,
                CarryChange::Clear => false,
            };
            // v unchanged
        }
    }

    fn eor_reg(&mut self, rd: u8, rm: u8, rn: u8, shift_t: ShiftType, shift_n: u32, setflags: bool) {
        // A7.7.36
        let (shifted, carry) = shift_c(self.read_reg(rm), shift_t, shift_n, self.cpu.apsr.c as u32);
        let result = self.read_reg(rn) ^ shifted;
        self.write_reg(rd, result);
        if setflags {
            self.cpu.apsr.n = bitset(result, 31);
            self.cpu.apsr.z = result == 0;
            self.cpu.apsr.c = carry;
            // v unchanged
        }
    }

    fn isb(&mut self, _option: u8) {
        // A7.7.37
        // TODO
    }

    fn it(&mut self, _firstcond: u8, _mask: u8) {
        // A7.7.38
        // TODO. Note a branch may land within an IT block, so we cannot just treat IT blocks separately.
        // we need to actually have an IT state of the board / CPU itself, and check the conditions before each
        // instruction
    }

    fn ldc_imm(&mut self) {
        // A7.7.39
        // TODO
    }

    fn ldc_lit(&mut self) {
        // A7.7.40
        // TODO
    }

    fn ldm(&mut self, rn: u8, registers: u32, wback: bool) {
        // A7.7.41
        assert!((registers >> 14) == 0);

        let mut address = self.read_reg(rn);
        for i in 0..=14u8 {
            if bitset(registers, i.into()) {
                let val = self.memory.read_word(address).unwrap();
                self.write_reg(i, val);
                address += 4;
            }
        }
        if bitset(registers, 15) {
            self.load_write_pc(self.memory.read_word(address).unwrap());
        }
        if wback && !bitset(registers, rn.into()) {
            self.write_reg(rn, address);
        }
    }

    fn ldmdb(&mut self, rn: u8, registers: u32, wback: bool) {
        // A7.7.42
        assert!((registers >> 14) == 0);

        let mut address = self.read_reg(rn) - 4 * registers.count_ones();
        let orig_address = address;
        for i in 0..=14u8 {
            if bitset(registers, i.into()) {
                self.write_reg(i, self.memory.read_word(address).unwrap());
                address += 4;
            }
        }
        if bitset(registers, 15) {
            self.load_write_pc(self.memory.read_word(address).unwrap());
        }
        if wback && !bitset(registers, rn.into()) {
            self.write_reg(rn, orig_address);
        }
    }

    fn ldr_imm(&mut self, rt: u8, rn: u8, offset: i32, index: bool, wback: bool) {
        // A7.7.43
        // NOTE: On making the Instruction, we use a signed offset instead of a separate add bool
        let offset_address = self.read_reg(rn).wrapping_add(offset as u32);
        let address = if index { offset_address } else { self.read_reg(rn) };
        let data = self.memory.read_word(address).unwrap();
        if wback { self.write_reg(rn, offset_address); }
        if rt == 15 {
            if (address & 0b11) == 0 {
                self.load_write_pc(data);
            } else {
                panic!("Unpredictable");
            }
        } else {
            self.write_reg(rt, data);
        }
    }

    fn ldr_lit(&mut self, rt: u8, address: u32) {
        // A7.7.44
        // NOTE: As PC is fixed for given instruction, we calculate the address
        // directly when building the instruction
        let data = self.memory.read_word(address).unwrap(); // TODO: Proper error handling
        if rt == 15 {
            if (address & 0b11) == 0 {
                self.load_write_pc(data);
            } else {
                panic!("Unpredictable");
            }
        } else {
            self.write_reg(rt, data);
        }
    }

    fn ldr_reg(&mut self, rt: u8, rn: u8, rm: u8, shift_t: ShiftType, shift_n: u32) {
        // A7.7.45
        let offset = shift(self.read_reg(rm), shift_t, shift_n, self.cpu.apsr.c as u32);
        let offset_address = self.read_reg(rn).wrapping_add(offset);
        let address = offset_address; // NOTE: This is supposed to be conditional on 'index', but 'index' is always true
        let data = self.memory.read_word(address).unwrap();
        // if wback { self.write_reg(rn, offset_address); } // NOTE: wback always false
        if rt == 15 {
            if (address & 0b11) == 0 {
                self.load_write_pc(data);
            } else {
                panic!("Unpredictable");
            }
        } else {
            self.write_reg(rt, data);
        }
    }

    fn ldrb_imm(&mut self, rt: u8, rn: u8, offset: i32, index: bool, wback: bool) {
        // A7.7.46
        let offset_address = self.read_reg(rn).wrapping_add(offset as u32);
        let address = if index { offset_address } else { self.read_reg(rn) };
        self.write_reg(rt, self.memory.read_byte(address).unwrap() as u32);
        if wback { self.write_reg(rn, offset_address); }
    }

    fn ldrb_lit(&mut self, rt: u8, address: u32) {
        // A7.7.47
        self.write_reg(rt, self.memory.read_byte(address).unwrap() as u32);
    }

    fn ldrb_reg(&mut self, rt: u8, rm: u8, rn: u8, shift_n: u32) {
        // A7.7.48
        // NOTE: THis has index, add, and wback, but they are always true, true, false
        let offset = shift(self.read_reg(rm), ShiftType::LSL, shift_n, self.cpu.apsr.c as u32);
        let address = self.read_reg(rn).wrapping_add(offset);
        self.write_reg(rt, self.memory.read_byte(address).unwrap() as u32);
    }

    fn ldbrt(&mut self, rt: u8, rn: u8, offset: u32) {
        // A7.7.49
        let address = self.read_reg(rn).wrapping_add(offset);
        self.write_reg(rt, self.memory.read_byte_unpriv(address).unwrap() as u32);
    }

    fn ldrd_imm(&mut self, rt: u8, rt2: u8, rn: u8, offset: i32, index: bool, wback: bool) {
        // A7.7.50
        let offset_address = self.read_reg(rn).wrapping_add(offset as u32);
        let address = if index { offset_address } else { self.read_reg(rn) };
        self.write_reg(rt, self.memory.read_word(address).unwrap());
        self.write_reg(rt2, self.memory.read_word(address + 4).unwrap());
        if wback { self.write_reg(rn, offset_address); }
    }

    fn ldrd_lit(&mut self, rt: u8, rt2: u8, offset: i32) {
        // A7.7.51
        if (self.read_pc() & 0b11) != 0 {
            panic!("Unpredictable");
        }
        let address = self.read_pc().wrapping_add(offset as u32);
        self.write_reg(rt, self.memory.read_word(address).unwrap());
        self.write_reg(rt2, self.memory.read_word(address + 4).unwrap());
    }

    fn ldrex(&mut self, rt: u8, rn: u8, imm32: u32) {
        // A7.7.52
        let address = self.read_reg(rn).wrapping_add(imm32);
        self.set_exclusive_monitors(address, 4);
        self.write_reg(rt, self.memory.read_word(address).unwrap());
    }

    fn ldrexb(&mut self, rt: u8, rn: u8) {
        // A7.7.53
        let address = self.read_reg(rn);
        self.set_exclusive_monitors(address, 1);
        self.write_reg(rt, self.memory.read_byte(address).unwrap() as u32);
    }

    fn ldrexh(&mut self, rt: u8, rn: u8) {
        // A7.7.53
        let address = self.read_reg(rn);
        self.set_exclusive_monitors(address, 2);
        self.write_reg(rt, self.memory.read_halfword(address).unwrap() as u32);
    }

    fn ldrh_imm(&mut self, rt: u8, rn: u8, offset: i32, index: bool, wback: bool) {
        // A7.7.55
        let offset_address = self.read_reg(rn).wrapping_add(offset as u32);
        let address = if index { offset_address } else { self.read_reg(rn) };
        if wback { self.write_reg(rn, offset_address); }
        self.write_reg(rt, self.memory.read_mem_u(address, 2).unwrap());
    }

    fn ldrh_lit(&mut self, rt: u8, address: u32) {
        // A7.7.56
        self.write_reg(rt, self.memory.read_mem_u(address, 2).unwrap());
    }

    fn ldrh_reg(&mut self, rt: u8, rm: u8, rn: u8, shift_n: u32) {
        // A7.7.57
        let offset = shift(self.read_reg(rm), ShiftType::LSL, shift_n, self.cpu.apsr.c as u32);
        let address = self.read_reg(rn).wrapping_add(offset);
        let data = self.memory.read_mem_u(address, 2).unwrap();
        self.write_reg(rt, data);
    }

    fn lsl_imm(&mut self, rd: u8, rm: u8, shift_n: u32, setflags: bool) {
        // A7.7.68
        let (result, carry) = shift_c(self.read_reg(rm), ShiftType::LSL, shift_n, self.cpu.apsr.c as u32);
        self.write_reg(rd, result);
        if setflags {
            self.cpu.apsr.n = bitset(result, 31);
            self.cpu.apsr.z = result == 0;
            self.cpu.apsr.c = carry;
            // v unchanged
        }
    }

    fn mov_imm(&mut self, rd: u8, imm32: u32, setflags: bool, carry: CarryChange) {
        // A7.7.76
        self.write_reg(rd, imm32);
        if setflags {
            self.cpu.apsr.n = bitset(imm32, 31);
            self.cpu.apsr.z = imm32 == 0;
            self.cpu.apsr.c = match carry {
                CarryChange::Same => self.cpu.apsr.c,
                CarryChange::Set => true,
                CarryChange::Clear => false,
            };
            // v unchanged
        }
    }

    fn mov_reg(&mut self, rd: u8, rm: u8, setflags: bool) {
        // A7.7.77
        let result = self.read_reg(rm);
        if rd == 15 {
            self.alu_write_pc(result);
        } else {
            self.write_reg(rd, result);
            if setflags {
                self.cpu.apsr.n = bitset(result, 31);
                self.cpu.apsr.z = result == 0;
                // c unchanged
                // v unchanged
            }
        }
    }

    fn push(&mut self, registers: u16) {
        // A7.7.99
        let mut sp = self.read_sp();
        for i in (0..=14u8).rev() {
            if !bitset16(registers, i.into()) {
                continue;
            }
            self.memory.write_word(sp, self.read_reg(i)).unwrap();
            sp -= 4;
        }

        self.set_sp(sp);
    }

    fn str_imm(&mut self, rt: u8, rn: u8, offset: i32, index: bool, wback: bool) {
        // A7.7.158
        // NOTE: On making the Instruction, we use a signed offset instead of a separate add bool
        let offset_address = self.read_reg(rn).wrapping_add(offset as u32);
        let address = if index { offset_address } else { self.read_reg(rn) };
        self.memory.write_word(address, self.read_reg(rt)).unwrap();
        if wback { self.write_reg(rn, offset_address); }
    }

    fn str_reg(&mut self, rt: u8, rn: u8, rm: u8, shift_t: ShiftType, shift_n: u32) {
        // A7.7.159
        let offset = shift(self.read_reg(rm), shift_t, shift_n, self.cpu.apsr.c as u32);
        let address = self.read_reg(rn).wrapping_add(offset);
        self.memory.write_word(address, self.read_reg(rt)).unwrap();
        self.memory.print_raw_mem_dump(0x2000_0000, 100);
    }

    fn sub_imm(&mut self, rd: u8, rn: u8, imm32: u32, setflags: bool) {
        // A7.7.171
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), !imm32, 1);
        self.write_reg(rd, result);
        if setflags {
            self.cpu.apsr.n = bitset(result, 31);
            self.cpu.apsr.z = result == 0;
            self.cpu.apsr.c = carry;
            self.cpu.apsr.v = overflow;
        }
    }

    fn sub_reg(&mut self, rd: u8, rm: u8, rn: u8, shift_t: ShiftType, shift_n: u32, setflags: bool) {
        // A7.7.172
        let shifted = shift(self.read_reg(rm), shift_t, shift_n, self.cpu.apsr.c as u32);
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), !shifted, 1);
        self.write_reg(rd, result);
        if setflags {
            self.cpu.apsr.n = bitset(result, 31);
            self.cpu.apsr.z = result == 0;
            self.cpu.apsr.c = carry;
            self.cpu.apsr.v = overflow;
        }
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut registers = String::new();
        let indent = "    ";

        for i in 0..4 {
            let left = self.get_register_display_value(i);
            let right = self.get_register_display_value(i + 8);
            let left_label = format!("r{}", i);
            let right_label = format!("r{}", i + 8);
            registers.push_str(&format!(
                "{}{: >3}: {: <34}  {: >3}: {: <34}\n",
                indent, left_label, left, right_label, right
            ));
        }
        for i in 4..8 {
            let left = self.get_register_display_value(i);
            let right = self.get_register_display_value(i + 8);
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
        registers.push_str(&format!("{}{}\n", indent, self.cpu.apsr));
        return write!(f, "CPU {{\n{}}}", registers);
    }
}

fn main() {
    println!("Welcome to ARM simulator");

    let mut board = Board::new();

    match board.load_elf_from_path(
        "/home/benjamin/gitlab/comp2300-2019-assignment-1-copy/.pio/build/disco_l476vg/firmware.elf",
    ) {
        Ok(_) => {}
        Err(s) => {
            println!("{}", s);
            return;
        }
    };

    println!("\n{}\n", board);
    println!("finished init");

    let mut cont = true;
    loop {
        if !cont {
            print!("press enter to continue");
            io::stdout().flush().unwrap();
            let mut input = String::new();
            io::stdin().read_line(&mut input).unwrap();
            print!("\n\n");
        } else {
            cont = board.read_pc() != 0x0800_01C8;
        }

        match board.next_instruction(true) {
            Ok(i) => {
                if !cont { println!("Ok: {:?}", i); }
                board.execute(i);
                if !cont { println!("\n{}\n", board); }
            }
            Err(e) => {
                println!("Err: {}", e);
                return;
            }
        };
    }
}
