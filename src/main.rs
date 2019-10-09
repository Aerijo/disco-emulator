#![allow(dead_code)]
#![allow(unused_imports)]

#[macro_use]
extern crate lazy_static;
extern crate goblin;
extern crate regex;

mod instruction;

use instruction::{bitset, bitset16, add_with_carry, shift, CarryChange, Condition, Instruction, RegFormat, ShiftType};

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
struct Flags { // TODO: APSR, EPSR, etc.
    n: bool, // negative
    z: bool, // zero
    c: bool, // carry
    v: bool, // overflow
    q: bool, // saturation
    thumb: bool, // thumb mode (should always be true for discoboard)
}

impl fmt::Display for Flags {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let neg = if self.n { 'N' } else { '_' };
        let zero = if self.z { 'Z' } else { '_' };
        let carry = if self.c { 'C' } else { '_' };
        let over = if self.v { 'V' } else { '_' };
        let sat = if self.q { 'Q' } else { '_' };
        return write!(f, "xPSR: {}{}{}{}{}", neg, zero, carry, over, sat);
    }
}

#[derive(Debug)]
struct CPU {
    registers: [u32; 16],
    flags: Flags,
}

impl CPU {
    fn new() -> CPU {
        return CPU {
            registers: [0; 16],
            flags: Flags {
                n: false,
                z: true,
                c: true,
                v: false,
                q: false,
                thumb: true,
            },
        };
    }

    fn read_reg(&self, reg: u8) -> u32 {
        // This is a "true" assertion, as registers come
        // from the machine code, so should never be out of range.
        assert!(reg <= 15);
        return self.registers[reg as usize];
    }

    fn set_reg(&mut self, reg: u8, val: u32) {
        assert!(reg <= 15);
        self.registers[reg as usize] = val;
    }

    fn get_flag_state(&self) -> u32 {
        let mut state = 0;
        if self.flags.n {
            state += 1 << 0
        };
        if self.flags.z {
            state += 1 << 1
        };
        if self.flags.c {
            state += 1 << 2
        };
        if self.flags.v {
            state += 1 << 3
        };
        if self.flags.q {
            state += 1 << 4
        };
        return state;
    }

    fn check_condition(&self, cond: Condition) -> bool {
        return match cond {
            Condition::Equal => self.flags.z,
            Condition::NotEqual => !self.flags.z,
            Condition::CarrySet => self.flags.c,
            Condition::CarryClear => !self.flags.c,
            Condition::Negative => self.flags.n,
            Condition::PosOrZero => self.flags.n,
            Condition::Overflow => self.flags.v,
            Condition::NotOverflow => !self.flags.v,
            Condition::UHigher => self.flags.c && !self.flags.z,
            Condition::ULowerSame => !self.flags.c || self.flags.z,
            Condition::SHigherSame => self.flags.n == self.flags.v,
            Condition::Slower => self.flags.n != self.flags.v,
            Condition::SHigher => !self.flags.z && (self.flags.n == self.flags.v),
            Condition::SLowerSame => self.flags.z || (self.flags.n != self.flags.v),
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
        registers.push_str(&format!("{}{}\n", indent, self.flags));
        return write!(f, "CPU {{\n{}}}", registers);
    }
}

struct MemoryBus {
    flash: [u8; 1024 * 1024 + 4],
    data: [u8; 128 * 1024],
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
        let val = data[i];
        print!("{:02X} ", val);
    }
    print!("\n");
}

impl MemoryBus {
    fn new() -> MemoryBus {
        return MemoryBus {
            flash: [0xFF; 1024 * 1024 + 4],
            data: [0xFF; 128 * 1024],
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
            let val = match self.get_word(i as u32) {
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

    fn get_instr_word(&self, address: u32) -> Result<u32, &str> {
        if 0x08000000 <= address && address <= 0x08000000 + (self.flash.len() as u32 - 4) {
            let base = (address - 0x08000000) as usize;
            let b1 = self.flash[base] as u32;
            let b2 = self.flash[base + 1] as u32;
            let b3 = self.flash[base + 2] as u32;
            let b4 = self.flash[base + 3] as u32;
            return Ok((b2 << 24) + (b1 << 16) + (b4 << 8) + b3);
        }

        return Err("Out of bounds access");
    }

    fn get_flash_capacity(&self) -> u32 {
        return self.flash.len() as u32 - 4;
    }

    fn get_data_capacity(&self) -> u32 {
        return self.data.len() as u32;
    }

    fn get_word(&self, address: u32) -> Result<u32, String> {
        // TODO: Map 0x0--something to 0x0800_0000--something

        if 0x0800_0000 <= address && address <= 0x0800_0000 + self.get_flash_capacity() - 4 {
            let base = (address - 0x0800_0000) as usize;
            let b1 = self.flash[base] as u32;
            let b2 = self.flash[base + 1] as u32;
            let b3 = self.flash[base + 2] as u32;
            let b4 = self.flash[base + 3] as u32;
            return Ok((b4 << 24) + (b3 << 16) + (b2 << 8) + b1);
        } else if 0x2000_0000 <= address && address <= 0x2000_0000 + self.get_data_capacity() - 4 {
            let base = (address - 0x2000_0000) as usize;
            let b1 = self.data[base] as u32;
            let b2 = self.data[base + 1] as u32;
            let b3 = self.data[base + 2] as u32;
            let b4 = self.data[base + 3] as u32;
            return Ok((b4 << 24) + (b3 << 16) + (b2 << 8) + b1);
        } else {
            return Err(String::from("Out of bounds access"));
        };
    }

    fn write_word(&mut self, address: u32, val: u32) {
        if 0x2000_0000 <= address && address <= 0x2000_0000 + self.get_data_capacity() - 4 {
            let base = (address - 0x2000_0000) as usize;
            self.data[base] = (val & 0xFF) as u8;
            self.data[base + 1] = ((val >> 8) & 0xFF) as u8;
            self.data[base + 2] = ((val >> 16) & 0xFF) as u8;
            self.data[base + 3] = (val >> 24) as u8;
        } else {
            println!("Out of bounds memory write");
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
            current_mode: ExecMode::ModeHandler,
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

    fn set_reg(&mut self, reg: u8, val: u32) {
        // TODO: Follow B1.4.7 p521
        self.cpu.set_reg(reg, val);
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
        self.set_reg(13, value);
    }

    fn read_lr(&self) -> u32 {
        return self.read_reg(14);
    }

    fn set_lr(&mut self, value: u32) {
        self.set_reg(14, value);
    }

    fn read_pc(&self) -> u32 {
        return self.read_reg(15);
    }

    fn set_pc(&mut self, value: u32) {
        self.set_reg(15, value);
    }

    fn inc_pc(&mut self, wide: bool) {
        self.set_pc(self.read_pc() + if wide { 4 } else { 2 });
    }

    fn dec_pc(&mut self, wide: bool) {
        self.set_pc(self.read_pc() - if wide { 4 } else { 2 });
    }

    fn mov_imm(&mut self, dest: u8, imm32: u32, setflags: bool, carry: CarryChange) {
        // A7.7.76 p291
        self.set_reg(dest, imm32);
        if setflags {
            self.cpu.flags.n = bitset(imm32, 31);
            self.cpu.flags.z = imm32 == 0;
            self.cpu.flags.c = match carry {
                CarryChange::Same => self.cpu.flags.c,
                CarryChange::Set => true,
                CarryChange::Clear => false,
            };
            // v unchanged
        }
    }

    fn branch(&mut self, address: u32) {
        // A7.7.12 p205
        self.branch_write_pc(address);
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
        self.cpu.flags.thumb = bitset(address, 0);
        if !self.cpu.flags.thumb {
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

    fn branch_exchange(&mut self, rm: u8) {
        // A7.7.20 p215
        self.bx_write_pc(self.cpu.read_reg(rm));
    }

    fn branch_cond(&mut self, address: u32, cond: Condition) {
        // NOTE: condition checking is defined in A7.3.1 p178
        // we will eventually need to generalise it (for IT blocks),
        // but for now we dedicate this separate function for conditional
        // branches.

        // A7.7.12 p205 (see self.branch)
        if self.cpu.check_condition(cond) {
            self.branch_write_pc(address);
        }
    }

    fn branch_with_link(&mut self, address: u32) {
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

        // A7.7.18 p213
        self.set_lr(self.read_pc() | 0b1);
        self.branch(address);
    }

    fn and_imm(&mut self, rd: u8, rn: u8, imm32: u32, setflags: bool, carry: CarryChange) {
        // A7.7.8 p200
        let result = self.read_reg(rn) & imm32;
        self.set_reg(rd, result);

        if setflags {
            self.cpu.flags.n = bitset(result, 31);
            self.cpu.flags.z = result == 0;
            self.cpu.flags.c = match carry {
                CarryChange::Same => self.cpu.flags.c,
                CarryChange::Set => true,
                CarryChange::Clear => false,
            };
            // v unchanged
        }
    }

    fn cmp_reg(&mut self, rm: u8, rn: u8, shift_t: ShiftType, shift_n: u32) {
        // A7.7.28 p224
        let shifted = shift(self.read_reg(rm), shift_t, shift_n, self.cpu.flags.c as u32);
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), !shifted, 1);
        self.cpu.flags.n = bitset(result, 31);
        self.cpu.flags.z = result == 0;
        self.cpu.flags.c = carry;
        self.cpu.flags.v = overflow;
    }

    fn mov_reg(&mut self, rd: u8, rm: u8, setflags: bool) {
        // A7.7.77 p293
        let result = self.read_reg(rm);
        if rd == 15 {
            self.alu_write_pc(result);
        } else {
            self.set_reg(rd, result);
            if setflags {
                self.cpu.flags.n = bitset(result, 31);
                self.cpu.flags.z = result == 0;
                // c unchanged
                // v unchanged
            }
        }
    }

    fn add_imm(&mut self, rd: u8, rn: u8, imm32: u32, setflags: bool) {
        // A7.7.3 p188
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), imm32, 0);
        self.set_reg(rd, result);
        if setflags {
            self.cpu.flags.n = bitset(result, 31);
            self.cpu.flags.z = result == 0;
            self.cpu.flags.c = carry;
            self.cpu.flags.v = overflow;
        }
    }

    fn add_reg(&mut self, rd: u8, rm: u8, rn: u8, shift_t: ShiftType, shift_n: u32, setflags: bool) {
        // A7.7.4 p190
        let shifted = shift(self.read_reg(rm), shift_t, shift_n, self.cpu.flags.c as u32);
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), shifted, 0);
        if rd == 15 {
            self.alu_write_pc(result);
        } else {
            self.set_reg(rd, result);
            if setflags {
                self.cpu.flags.n = bitset(result, 31);
                self.cpu.flags.z = result == 0;
                self.cpu.flags.c = carry;
                self.cpu.flags.v = overflow;
            }
        }
    }

    fn sub_imm(&mut self, rd: u8, rn: u8, imm32: u32, setflags: bool) {
        // A7.7.171 p397
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), !imm32, 1);
        self.set_reg(rd, result);
        if setflags {
            self.cpu.flags.n = bitset(result, 31);
            self.cpu.flags.z = result == 0;
            self.cpu.flags.c = carry;
            self.cpu.flags.v = overflow;
        }
    }

    fn sub_reg(&mut self, rd: u8, rm: u8, rn: u8, shift_t: ShiftType, shift_n: u32, setflags: bool) {
        // A7.7.172 p399
        let shifted = shift(self.read_reg(rm), shift_t, shift_n, self.cpu.flags.c as u32);
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), !shifted, 1);
        self.set_reg(rd, result);
        if setflags {
            self.cpu.flags.n = bitset(result, 31);
            self.cpu.flags.z = result == 0;
            self.cpu.flags.c = carry;
            self.cpu.flags.v = overflow;
        }
    }

    fn str_reg(&mut self, rt: u8, rn: u8, rm: u8, shift_t: ShiftType, shift_n: u32) {
        // A7.7.159 p383
        let offset = shift(self.read_reg(rm), shift_t, shift_n, self.cpu.flags.c as u32);
        let address = self.read_reg(rn).wrapping_add(offset);
        self.memory.write_word(address, self.read_reg(rt));
        self.memory.print_raw_mem_dump(0x2000_0000, 100);
    }

    fn ldr_lit(&mut self, rt: u8, address: u32) {
        // A7.7.43 p245
        // NOTE: As PC is fixed for given instruction, we calculate the address
        // directly when building the instruction
        let data = self.memory.get_word(address).unwrap(); // TODO: Proper error handling
        if rt == 15 {
            if (address & 0b11) == 0 {
                self.load_write_pc(data);
            } else {
                panic!("Unpredictable");
            }
        } else {
            self.set_reg(rt, data);
        }
    }

    fn ldr_imm(&mut self, rt: u8, rn: u8, offset: i32, index: bool, wback: bool) {
        // A7.7.42 p243
        // NOTE: On making the Instruction, we use a signed offset instead of a separate add bool
        let offset_address = self.read_reg(rn).wrapping_add(offset as u32);
        let address = if index { offset_address } else { self.read_reg(rn) };
        let data = self.memory.get_word(address).unwrap();
        if wback { self.set_reg(rn, offset_address); }
        if rt == 15 {
            if (address & 0b11) == 0 {
                self.load_write_pc(data);
            } else {
                panic!("Unpredictable");
            }
        } else {
            self.set_reg(rt, data);
        }
    }

    fn ldr_reg(&mut self, rt: u8, rn: u8, rm: u8, shift_t: ShiftType, shift_n: u32) {
        // A7.7.44 p247
        let offset = shift(self.read_reg(rm), shift_t, shift_n, self.cpu.flags.c as u32);
        let offset_address = self.read_reg(rn).wrapping_add(offset);
        let address = offset_address; // NOTE: This is supposed to be conditional on 'index', but 'index' is always true
        let data = self.memory.get_word(address).unwrap();
        // if wback { self.set_reg(rn, offset_address); } // NOTE: wback always false
        if rt == 15 {
            if (address & 0b11) == 0 {
                self.load_write_pc(data);
            } else {
                panic!("Unpredictable");
            }
        } else {
            self.set_reg(rt, data);
        }
    }

    fn str_imm(&mut self, rt: u8, rn: u8, offset: i32, index: bool, wback: bool) {
        // A7.7.158 p381
        // NOTE: On making the Instruction, we use a signed offset instead of a separate add bool
        let offset_address = self.read_reg(rn).wrapping_add(offset as u32);
        let address = if index { offset_address } else { self.read_reg(rn) };
        self.memory.write_word(address, self.read_reg(rt));
        if wback { self.set_reg(rn, offset_address); }
    }

    fn push(&mut self, registers: u16) {
        // A7.7.99 p318
        let mut sp = self.read_sp();
        for i in (0..=14u8).rev() {
            if !bitset16(registers, i.into()) {
                continue;
            }
            self.memory.write_word(sp, self.read_reg(i));
            sp -= 4;
        }

        self.set_sp(sp);
    }

    fn add_sp_imm(&mut self, rd: u8, imm32: u32, setflags: bool) {
        // A7.7.5 p192
        let (result, carry, overflow) = add_with_carry(self.read_sp(), imm32, 0);
        self.set_reg(rd, result);
        if setflags {
            self.cpu.flags.n = bitset(result, 31);
            self.cpu.flags.z = result == 0;
            self.cpu.flags.c = carry;
            self.cpu.flags.v = overflow;
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
        registers.push_str(&format!("{}{}\n", indent, self.cpu.flags));
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
            cont = board.read_pc() != 0x0800_2d22;
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
