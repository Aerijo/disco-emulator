#![allow(dead_code)]
#![allow(unused_imports)]

#[macro_use]
extern crate lazy_static;
extern crate goblin;
extern crate regex;

mod instruction;
use instruction::{CarryChange, Condition, Instruction, ShiftType};

mod peripherals;
use peripherals::Peripherals;

mod cpu;
use cpu::{CPU};

mod utils;
use utils::bits::{bitset, add_with_carry, shift, shift_c, align};

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

#[derive(Copy, Clone, Debug)]
enum RegFormat {
    Bin, // binary
    Oct, // octal
    Dec, // unsigned decimal
    Sig, // signed decimal
    Hex, // hexadecimal
}

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
pub struct Shift {
    shift_t: ShiftType,
    shift_n: u32,
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

    fn print_mem_dump(&self, address: u32, length: usize) {
        let mut c = 16;
        for i in address..(address + (length as u32) * 4) {
            if c == 0 {
                c = 16;
                print!("\n");
            }
            c -= 1;
            let val = match self.read_byte(i) {
                Ok(v) => v,
                Err(e) => {
                    println!("{}", e);
                    return;
                }
            };
            print!("{:02X} ", val);
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
    branch_map: HashMap<u32, String>,
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

        // Most instructions that use the PC assume it is +4 bytes
        // from their position when calculating offsets (see page 124).
        let (instr, wide) = Instruction::from(val, pc + 4);

        if wide {
            println!("{:#034b} -> {:?}", val, instr);
        } else {
            println!("{:#018b} -> {:?}", val >> 16, instr);
        }


        if update_pc {
            self.inc_pc(wide);
        }

        return Ok(instr);
    }

    fn execute(&mut self, instr: Instruction) {
        match instr {
            Instruction::AddImm {rd, rn, imm32, setflags} => self.add_imm(rd, rn, imm32, setflags),
            Instruction::AddReg { rd, rm, rn, shift, setflags } => self.add_reg(rd, rm, rn, shift, setflags),
            Instruction::AddSpImm { rd, imm32, setflags } => self.add_sp_imm(rd, imm32, setflags),
            Instruction::AndImm {rd, rn, imm32, setflags, carry} => self.and_imm(rd, rn, imm32, setflags, carry),
            Instruction::Branch { address } => self.branch(address),
            Instruction::CondBranch { address, cond } => self.branch_cond(address, cond),
            Instruction::LinkedBranch { address } => self.branch_with_link(address),
            Instruction::BranchExchange { rm } => self.branch_exchange(rm),
            Instruction::CmpImm { rn, imm32 } => self.cmp_imm(rn, imm32),
            Instruction::CmpReg { rm, rn, shift } => self.cmp_reg(rm, rn, shift),
            Instruction::LdrLit { rt, address } => self.ldr_lit(rt, address),
            Instruction::Ldm { rn, registers, wback } => self.ldm(rn, registers, wback),
            Instruction::LdrImm { rt, rn, offset, index, wback } => self.ldr_imm(rt, rn, offset, index, wback),
            Instruction::LdrReg { rt, rn, rm, shift } => self.ldr_reg(rt, rn, rm, shift),
            Instruction::MovImm { rd, imm32, setflags, carry } => self.mov_imm(rd, imm32, setflags, carry),
            Instruction::MovReg { rd, rm, setflags } => self.mov_reg(rd, rm, setflags),
            Instruction::Mul {rd, rn, rm, setflags} => self.mul(rd, rn, rm, setflags),
            Instruction::OrrImm {rd, rn, imm32, setflags, carry} => self.orr_imm(rd, rn, imm32, setflags, carry),
            Instruction::Pop { registers } => self.pop(registers),
            Instruction::Push { registers } => self.push(registers),
            Instruction::SubImm { rd, rn, imm32, setflags } => self.sub_imm(rd, rn, imm32, setflags),
            Instruction::SubReg { rd, rm, rn, shift, setflags } => self.sub_reg(rd, rm, rn, shift, setflags),
            Instruction::Stm { rn, registers, wback } => self.stm(rn, registers, wback),
            Instruction::StrImm { rn, rt, offset, index, wback } => self.str_imm(rt, rn, offset, index, wback),
            Instruction::StrReg { rt, rn, rm, shift } => self.str_reg(rt, rn, rm, shift),
            Instruction::Udiv {rd, rn, rm} => self.udiv(rd, rn, rm),

            Instruction::ShiftImm {rd, rm, shift, setflags} => self.shift_imm(rd, rm, shift, setflags),

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

            match name {
                "SystemInit" |
                "__libc_init_array" |
                "init" |
                "init_joystick" |
                "lcd_init" |
                "lcd_write_string" |
                "lcd_update_display" |
                "BSP_AUDIO_OUT_Play_Sample" => {
                    self.branch_map.insert((sym.st_value as u32) & !0b1, name.to_string());
                }
                _ => {}
            }
        }

        self.bx_write_pc(elf.entry as u32);

        match self.memory.load_elf(elf, &bytes) {
            Ok(_) => {}
            Err(e) => return Err(e),
        };

        return Ok(());
    }

    fn print_mem_dump(&mut self, index: u32, length: usize) {
        self.memory.print_mem_dump(index, length);
    }

    fn print_mem_area(&mut self, address: u32) {
        let padding = "   ".repeat((address & 0xF) as usize);
        println!("{}v{:#010X}", padding.to_string(), address);
        self.print_mem_dump(address & !0xF, 0x8);
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
        return self.read_reg(13) & !0b11;
    }

    fn write_sp(&mut self, value: u32) {
        self.write_reg(13, value & !0b11);
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

    fn get_shifted_register(&self, reg: u8, s: Shift) -> u32 {
        return shift(self.read_reg(reg), s, self.cpu.read_carry_flag() as u32);
    }

    fn get_shift_with_carry(&self, reg: u8, s: Shift) -> (u32, bool) {
        return shift_c(self.read_reg(reg), s, self.cpu.read_carry_flag() as u32);
    }

    fn get_add_with_carry(&self, reg: u8, imm32: u32) -> (u32, bool, bool) {
        return add_with_carry(self.read_reg(reg), imm32, self.cpu.read_carry_flag() as u32);
    }

    fn get_add_with_no_carry(&self, reg: u8, imm32: u32) -> (u32, bool, bool) {
        return add_with_carry(self.read_reg(reg), imm32, 0);
    }

    fn set_flags_nz(&mut self, result: u32) {
        self.cpu.set_negative_flag(bitset(result, 31));
        self.cpu.set_zero_flag(result == 0);
        // c unchanged
        // v unchanged
    }

    fn set_flags_nzc(&mut self, result: u32, carry: bool) {
        self.set_flags_nz(result);
        self.cpu.set_carry_flag(carry);
        // v unchanged
    }

    fn set_flags_nzcv(&mut self, result: u32, carry: bool, overflow: bool) {
        self.set_flags_nzc(result, carry);
        self.cpu.set_overflow_flag(overflow);
    }

    fn set_flags_nz_alt_c(&mut self, result: u32, carry: CarryChange) {
        self.set_flags_nz(result);
        match carry {
            CarryChange::Set => self.cpu.set_carry_flag(true),
            CarryChange::Clear => self.cpu.set_carry_flag(false),
            CarryChange::Same => {},
        };
        // v unchanged
    }

    fn shift_imm(&mut self, rd: u8, rm: u8, shift: Shift, setflags: bool) {
        // Handles LSL, LSR, ASR, etc., as their encodings (T2 at least) are all very similar
        let (result, carry) = self.get_shift_with_carry(rm, shift);
        self.write_reg(rd, result);
        if setflags {
            self.set_flags_nzc(result, carry);
        }
    }

    /**
     * Helper pseudocode functions
     */

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
        self.cpu.set_thumb_mode(bitset(address, 0));
        if !self.cpu.read_thumb_mode() {
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
        let (result, carry, overflow) = self.get_add_with_carry(rn, imm32);
        self.write_reg(rd, result);
        if setflags {
            self.set_flags_nzcv(result, carry, overflow);
        }
    }

    fn adc_reg(&mut self, rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool) {
        // A7.7.2
        let shifted = self.get_shifted_register(rm, shift);
        let (result, carry, overflow) = self.get_add_with_carry(rn, shifted);
        self.write_reg(rd, result);
        if setflags {
            self.set_flags_nzcv(result, carry, overflow);
        }
    }

    fn add_imm(&mut self, rd: u8, rn: u8, imm32: u32, setflags: bool) {
        // A7.7.3
        let (result, carry, overflow) = self.get_add_with_no_carry(rn, imm32);
        self.write_reg(rd, result);
        if setflags {
            self.set_flags_nzcv(result, carry, overflow);
        }
    }

    fn add_reg(&mut self, rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool) {
        // A7.7.4
        let shifted = self.get_shifted_register(rm, shift);
        let (result, carry, overflow) = self.get_add_with_no_carry(rn, shifted);
        if rd == 15 {
            self.alu_write_pc(result);
        } else {
            self.write_reg(rd, result);
            if setflags {
                self.set_flags_nzcv(result, carry, overflow);
            }
        }
    }

    fn add_sp_imm(&mut self, rd: u8, imm32: u32, setflags: bool) {
        // A7.7.5
        let (result, carry, overflow) = add_with_carry(self.read_sp(), imm32, 0);
        self.write_reg(rd, result);
        if setflags {
            self.set_flags_nzcv(result, carry, overflow);
        }
    }

    fn add_sp_reg(&mut self, rd: u8, rm: u8, shift: Shift, setflags: bool) {
        // A7.7.6
        let shifted = self.get_shifted_register(rm, shift);
        let (result, carry, overflow) = add_with_carry(self.read_sp(), shifted, 0);
        if rd == 15 {
            self.alu_write_pc(result);
        } else {
            self.write_reg(rd, result);
            if setflags {
                self.set_flags_nzcv(result, carry, overflow);
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
            self.set_flags_nz_alt_c(result, carry);
        }
    }

    fn and_reg(&mut self, rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool) {
        // A7.7.9
        let (shifted, carry) = self.get_shift_with_carry(rm, shift);
        let result = self.read_reg(rn) & shifted;
        self.write_reg(rd, result);
        if setflags {
            self.set_flags_nzc(result, carry);
        }
    }

    fn asr_imm(&mut self, rd: u8, rm: u8, shift: Shift, setflags: bool) {
        // A7.7.10
        assert!(shift.shift_t == ShiftType::ASR);
        let (result, carry) = self.get_shift_with_carry(rm, shift);
        self.write_reg(rd, result);
        if setflags {
            self.set_flags_nzc(result, carry);
        }
    }

    fn asr_reg(&mut self, rd: u8, rm: u8, rn: u8, setflags: bool) {
        // A7.7.11
        let shift_n = self.read_reg(rm) & 0xFF;
        let (result, carry) =  self.get_shift_with_carry(rn, Shift {shift_t: ShiftType::ASR, shift_n});
        self.write_reg(rd, result);
        if setflags {
            self.set_flags_nzc(result, carry);
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
            println!("Condition passed");
            self.branch_write_pc(address);
        } else {
            println!("Condition failed");
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
            self.set_flags_nz_alt_c(result, carry);
        }
    }

    fn bic_reg(&mut self, rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool) {
        // A7.7.16
        let (shifted, carry) = self.get_shift_with_carry(rm, shift);
        let result = self.read_reg(rn) & !shifted;
        self.write_reg(rd, result);
        if setflags {
            self.set_flags_nzc(result, carry);
        }
    }

    fn bkpt(&mut self, _imm32: u32) {
        // A7.7.17
        // TODO: When return values supported, return a DebugMonitor exception with the input id
    }

    fn branch_with_link(&mut self, address: u32) {
        // A7.7.18
        self.set_lr(self.read_pc() | 0b1);
        match self.branch_map.get(&address) {
            Some(name) => {
                println!("Skipping branch with link to {}", name);
                return;
            }
            _ => {}
        }
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
        let (result, carry, overflow) = self.get_add_with_no_carry(rn, imm32);
        self.set_flags_nzcv(result, carry, overflow);
    }

    fn cmn_reg(&mut self, rn: u8, rm: u8, shift: Shift) {
        // A7.7.26
        let shifted = self.get_shifted_register(rm, shift);
        let (result, carry, overflow) = self.get_add_with_no_carry(rn, shifted);
        self.set_flags_nzcv(result, carry, overflow);
    }

    fn cmp_imm(&mut self, rn: u8, imm32: u32) {
        // A7.7.27
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), !imm32, 1);
        self.set_flags_nzcv(result, carry, overflow);
    }

    fn cmp_reg(&mut self, rm: u8, rn: u8, shift: Shift) {
        // A7.7.28
        let shifted = self.get_shifted_register(rm, shift);
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), !shifted, 1);
        self.set_flags_nzcv(result, carry, overflow);
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
            self.set_flags_nz_alt_c(result, carry);
        }
    }

    fn eor_reg(&mut self, rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool) {
        // A7.7.36
        let (shifted, carry) = self.get_shift_with_carry(rm, shift);
        let result = self.read_reg(rn) ^ shifted;
        self.write_reg(rd, result);
        if setflags {
            self.set_flags_nzc(result, carry);
        }
    }

    fn isb(&mut self, _option: u8) {
        // A7.7.37
        // TODO
    }

    fn it(&mut self, _firstcond: u8, _mask: u8) {
        // A7.7.38
        // TODO. Note a branch may _not_ go into an IT block, so we are free to
        // pass the ITSTATE and do whatever when grabbing instructions. Yay.
    }

    fn ldc_imm(&mut self) {
        // A7.7.39
        // TODO
    }

    fn ldc_lit(&mut self) {
        // A7.7.40
        // TODO
    }

    fn ldm(&mut self, rn: u8, registers: u16, wback: bool) {
        // A7.7.41
        assert!((registers >> 14) == 0);

        let mut address = self.read_reg(rn);
        self.print_mem_area(address);
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
        self.print_mem_area(address);
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

    fn ldr_reg(&mut self, rt: u8, rn: u8, rm: u8, shift: Shift) {
        // A7.7.45
        let offset = self.get_shifted_register(rm, shift);
        let offset_address = self.read_reg(rn).wrapping_add(offset);
        let address = offset_address; // NOTE: This is supposed to be conditional on 'index', but 'index' is always true
        self.print_mem_area(address);
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
        // NOTE: This has index, add, and wback, but they are always true, true, false
        let offset = self.get_shifted_register(rm, Shift {shift_t: ShiftType::LSL, shift_n});
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
        let offset = self.get_shifted_register(rm, Shift {shift_t: ShiftType::LSL, shift_n});
        let address = self.read_reg(rn).wrapping_add(offset);
        let data = self.memory.read_mem_u(address, 2).unwrap();
        self.write_reg(rt, data);
    }

    fn lsl_imm(&mut self, rd: u8, rm: u8, shift_n: u32, setflags: bool) {
        // A7.7.68
        let (result, carry) = self.get_shift_with_carry(rm, Shift {shift_t: ShiftType::LSL, shift_n});
        self.write_reg(rd, result);
        if setflags {
            self.set_flags_nzc(result, carry);
        }
    }

    fn lsl_reg(&mut self, rd: u8, rn: u8, rm: u8, setflags: bool) {
        // A7.7.69
        let shift_n = self.read_reg(rm) & 0xF;
        let (result, carry) = self.get_shift_with_carry(rn, Shift {shift_t: ShiftType::LSL, shift_n});
        self.write_reg(rd, result);
        if setflags {
            self.set_flags_nzc(result, carry);
        }
    }

    fn mov_imm(&mut self, rd: u8, imm32: u32, setflags: bool, carry: CarryChange) {
        // A7.7.76
        self.write_reg(rd, imm32);
        if setflags {
            self.set_flags_nz_alt_c(imm32, carry);
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
                self.set_flags_nz(result);
            }
        }
    }

    fn mul(&mut self, rd: u8, rn: u8, rm: u8, setflags: bool) {
        // A7.7.84
        let op1 = self.read_reg(rn) as i32;
        let op2 = self.read_reg(rm) as i32;
        let result = op1.wrapping_mul(op2) as u32;
        self.write_reg(rd, result);
        if setflags {
            self.set_flags_nz(result);
        }
    }

    fn orr_imm(&mut self, rd: u8, rn: u8, imm32: u32, setflags: bool, carry: CarryChange) {
        let result = self.read_reg(rn) | imm32;
        self.write_reg(rd, result);
        if setflags {
            self.set_flags_nz_alt_c(result, carry);
        }
    }

    fn pop(&mut self, registers: u16) {
        let mut address = self.read_sp();
        self.print_mem_area(address);
        self.write_sp(address + 4 * registers.count_ones());

        for i in 0..=14u8 {
            if bitset(registers, i.into()) {
                self.write_reg(i, self.memory.read_mem_a(address, 4).unwrap());
                address += 4;
            }
        }

        if bitset(registers, 15) {
            self.load_write_pc(self.memory.read_mem_a(address, 4).unwrap());
        }
    }

    fn push(&mut self, registers: u16) {
        // A7.7.99
        let mut address = self.read_sp() - 4 * registers.count_ones();
        let orig_address = address;

        for i in 0..=14u8 {
            if bitset(registers, i.into()) {
                self.memory.write_word(address, self.read_reg(i)).unwrap();
                address += 4;
            }
        }

        self.write_sp(orig_address);

        self.print_mem_area(orig_address);
    }

    fn stm(&mut self, rn: u8, registers: u16, wback: bool) {
        // A7.7.159
        let mut address = self.read_reg(rn);
        for i in 0..=14u8 {
            if bitset(registers, i.into()) {
                self.memory.write_word(address, self.read_reg(i)).unwrap();
                address += 4;
            }
        }
        self.print_mem_area(address);

        if wback { self.write_reg(rn, address); }
    }

    fn str_imm(&mut self, rt: u8, rn: u8, offset: i32, index: bool, wback: bool) {
        // A7.7.158
        // NOTE: On making the Instruction, we use a signed offset instead of a separate add bool
        let offset_address = self.read_reg(rn).wrapping_add(offset as u32);
        let address = if index { offset_address } else { self.read_reg(rn) };
        self.memory.write_word(address, self.read_reg(rt)).unwrap();
        if wback { self.write_reg(rn, offset_address); }
        self.print_mem_area(address);
    }

    fn str_reg(&mut self, rt: u8, rn: u8, rm: u8, shift: Shift) {
        // A7.7.159
        let offset = self.get_shifted_register(rm, shift);
        let address = self.read_reg(rn).wrapping_add(offset);
        self.memory.write_word(address, self.read_reg(rt)).unwrap();
        self.print_mem_area(address);
    }

    fn sub_imm(&mut self, rd: u8, rn: u8, imm32: u32, setflags: bool) {
        // A7.7.171
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), !imm32, 1);
        self.write_reg(rd, result);
        if setflags {
            self.set_flags_nzcv(result, carry, overflow);
        }
    }

    fn sub_reg(&mut self, rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool) {
        // A7.7.172
        let shifted = self.get_shifted_register(rm, shift);
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), !shifted, 1);
        self.write_reg(rd, result);
        if setflags {
            self.set_flags_nzcv(result, carry, overflow);
        }
    }

    fn udiv(&mut self, rd: u8, rn: u8, rm: u8) {
        let m = self.read_reg(rm);
        let result = if m == 0 {
            if /*IntegerZeroDivideTrappingEnabled*/ true {
                panic!("GenerateIntegerZeroDivide");
            } else {
                0
            }
        } else {
            self.read_reg(rn) / m
        };
        self.write_reg(rd, result);
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
        registers.push_str(&format!("{}{}\n", indent, self.cpu.get_apsr_display()));
        return write!(f, "CPU {{\n{}}}", registers);
    }
}

fn main() {
    println!("Welcome to ARM simulator");

    let mut board = Board::new();

    match board.load_elf_from_path(
        "/home/benjamin/gitlab/2300/assignments/comp2300-2019-assignment-2-part-2/.pio/build/disco_l476vg/firmware.elf",
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
    let mut i = 0;
    loop {
        if cont {
            cont = i != 5050;
            if !cont { println!("\n{}\n", board); }
        }

        if !cont {
            print!("press enter to continue");
            io::stdout().flush().unwrap();
            let mut input = String::new();
            io::stdin().read_line(&mut input).unwrap();
            print!("\n\n");
            // cont = true;
        }

        print!("{} - ", i);
        i += 1;
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
