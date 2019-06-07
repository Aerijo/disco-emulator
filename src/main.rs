#![allow(dead_code)]

#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate goblin;

use std::num::Wrapping;
use std::option::Option;
use std::vec::Vec;
use std::string::String;
use std::fmt;
use std::io::{self, Write};
use regex::Regex;
use std::fs;
use goblin::elf::{Elf};


#[derive(Debug)]
enum Instruction {
    MovImm { to: usize, val: u32, flags: bool },
    MovReg { to: usize, from: usize, flags: bool },
    AddImm { dest: usize, first: usize, val: u32, flags: bool },
    AddReg { dest: usize, first: usize, second: usize, flags: bool },
    SubImm { dest: usize, first: usize, val: u32, flags: bool },
    SubReg { dest: usize, first: usize, second: usize, flags: bool },
    LdrReg { dest: usize, address: usize, flags: bool },
    StrReg { val: usize, address: usize },
    Undo { num: usize },
    Redo { num: usize },
    Format { reg: usize, kind: RegFormat },
    Memview { index: usize, length: usize },
}


impl Instruction {
    fn from_word (word: u32) -> Instruction {
        let length_check = word >> 29;
        return if length_check == 0b111 && (word >> 27 != 0b11100) {
            Instruction::get_wide_instruction(word)
        } else {
            Instruction::get_narrow_instruction((word >> 16) as u16)
        }
    }

    fn get_wide_instruction (_word: u32) -> Instruction {
        return Instruction::Undo { num: 1 };
    }

    fn get_narrow_instruction (_hword: u16) -> Instruction {
        return Instruction::Undo { num: 1 };
    }
}


#[derive(Copy, Clone, Debug)]
enum RegFormat {
    Bin, // binary
    Oct, // octal
    Dec, // unsigned decimal
    Sig, // signed decimal
    Hex, // hexadecimal
}


#[derive(Debug)]
struct Flags {
    n: bool, // negative
    z: bool, // zero
    c: bool, // carry
    v: bool, // overflow
    q: bool, // saturation
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
struct Transition {
    reg: usize,
    val: Wrapping<u32>,
    flags: u32,
}


#[derive(Debug)]
struct CPU {
    registers: [Wrapping<u32>; 16],
    flags: Flags,
    // bus: MemoryBus,
}


impl CPU {
    fn new () -> CPU {
        let mut cpu = CPU {
            registers: [Wrapping(0); 16],
            flags: Flags { n: false, z: true, c: true, v: false, q: false, },
        };

        cpu.set_reg(13, Wrapping(0x20018000));
        cpu.set_reg(15, Wrapping(0x080001C8));

        return cpu;
    }

    fn read_reg (&self, reg: usize) -> Wrapping<u32> {
        assert!(reg <= 15);
        return self.registers[reg];
    }

    fn set_reg (&mut self, reg: usize, val: Wrapping<u32>) {
        assert!(reg <= 15);
        self.registers[reg] = val;
    }

    fn get_flag_state (&self) -> u32 {
        let mut state = 0;
        if self.flags.n { state += 1 << 0};
        if self.flags.z { state += 1 << 1};
        if self.flags.c { state += 1 << 2};
        if self.flags.v { state += 1 << 3};
        if self.flags.q { state += 1 << 4};
        return state;
    }

    fn set_flags_nzcv (&mut self, v1: Wrapping<u32>, v2: Wrapping<u32>, result: Wrapping<u32>) {
        self.flags.n = (result.0 as i32) < 0;
        self.flags.z = result.0 == 0;
        self.flags.c = result < v1 || result < v2;
        self.flags.v = (((v1.0 & v2.0 & !result.0) | (!v1.0 & !v2.0 & result.0)) & (1 << 31)) > 0;
    }

    fn step (&mut self) {

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
            registers.push_str(&format!("{}{: >3}: {: <34}  {: >3}: {: <34}\n", indent, left_label, left, right_label, right));
        }
        registers.push('\n');
        for i in 4..8 {
            let left = self.read_reg(i);
            let right = self.read_reg(i + 8);
            let special = ["r12", "sp", "lr", "pc"];
            let left_label = format!("r{}", i);

            registers.push_str(&format!("{}{: >3}: {: <34}  {: >3}: {: <34}\n", indent, left_label, left, special[i - 4], right));
        }
        registers.push('\n');
        registers.push_str(&format!("{}{}\n", indent, self.flags));
        return write!(f, "CPU {{\n{}}}", registers);
    }
}


struct MemoryBus {
    flash: [u8; 1024 * 1024 + 4],
    data: [u8; 128 * 1024]
}


impl fmt::Debug for MemoryBus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "<data>");
    }
}


impl MemoryBus {
    fn new () -> MemoryBus {
        return MemoryBus {
            flash: [0xFF; 1024 * 1024 + 4],
            data: [0xFF; 128 * 1024]
        }
    }

    fn load_elf (&mut self, elf: Elf, bytes: &[u8]) -> Result<(), String> {
        for header in elf.program_headers.iter() {
            if header.p_type != goblin::elf::program_header::PT_LOAD {
                return Err(String::from("Unexpected program header type"));
            }

            if header.p_vaddr != 0x0800_0000 { continue; }

            let offset = header.p_offset as usize;
            let size = header.p_filesz as usize;

            if size > self.flash.len() {
                return Err(String::from("Flash too large"));
            }

            println!("{:#?}", header);

            for i in 0..size {
                self.flash[i] = bytes[i + offset];
                print!("{:02X} ", self.flash[i]);
            }
            print!("showing {} bytes\n", size);

        }
        return Ok(());
    }

    fn print_mem_dump (&self, mut index: usize, length: usize) {
        if index > 0x20000000 { index -= 0x20000000; }
        if index + length > self.data.len() {
            println!("Index out of range");
            return;
        }

        let mut c = 16;
        for i in index..(index + length) {
            if c == 0 {
                c = 16;
                print!("\n");
            }
            c -= 1;
            print!("{:02X} ", self.data[i]);
        }
        print!("\n");
    }

    fn get_word (&self, address: u32) -> Result<u32, &str> {
        if 0x08000000 <= address && address <= 0x08000000 + (self.flash.len() as u32) {
            let base = (address - 0x08000000) as usize;
            let b1 = self.flash[base] as u32;
            let b2 = self.flash[base + 1] as u32;
            let b3 = self.flash[base + 2] as u32;
            let b4 = self.flash[base + 3] as u32;
            return Ok((b2 << 24) + (b1 << 16) + (b4 << 8) + b3);
        }

        return Err("Out of bounds access");
    }
}


#[derive(Debug)]
struct Board {
    cpu: CPU,
    memory: MemoryBus,
    command_stack: Vec<Transition>,
    register_formats: [RegFormat; 16],

}


impl Board {
    fn new () -> Board {
        return Board {
            cpu: CPU::new(),
            memory: MemoryBus::new(),
            command_stack: Vec::new(),
            register_formats: [RegFormat::Hex; 16],
        }
    }

    fn next_instruction (&mut self, update_pc: bool) -> Result<Instruction, &str> {
        let pc = self.read_pc().0;
        if update_pc { self.inc_pc() };

        let val = match self.memory.get_word(pc) {
            Ok(v) => v,
            Err(e) => return Err(e),
        };

        println!("{:08X}", val);


        return Err("failed");
    }

    fn load_elf_from_path (&mut self, path: &str) -> Result<(), String> {
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

        self.set_reg(15, Wrapping((elf.entry - 1) as u32)); // TODO: Work out why it points to 1 byte past start

        match self.memory.load_elf(elf, &bytes) {
            Ok(_) => {}
            Err(e) => return Err(e),
        };

        return Ok(());
    }

    fn print_mem_dump (&mut self, index: usize, length: usize) {
        self.memory.print_mem_dump(index, length);
    }

    fn undo (&mut self, mut num: usize) {
        while num > 0 {
            num -= 1;

            let last = self.command_stack.pop();
            if last.is_none() {
                println!("No more instructions to undo!");
                return;
            }

            match last.unwrap() {
                Transition { reg, val, flags } => {
                    self.dec_pc();
                    self.set_reg(reg, val);
                    self.cpu.flags.n = (flags & 1 << 0) > 0;
                    self.cpu.flags.z = (flags & 1 << 1) > 0;
                    self.cpu.flags.c = (flags & 1 << 2) > 0;
                    self.cpu.flags.v = (flags & 1 << 3) > 0;
                    self.cpu.flags.q = (flags & 1 << 4) > 0;
                }
            }
        }
    }

    fn redo (&mut self, _num: usize) {
        println!("To be added");
    }

    fn read_reg (&self, reg: usize) -> Wrapping<u32> {
        return self.cpu.read_reg(reg);
    }

    fn set_reg (&mut self, reg: usize, val: Wrapping<u32>) {
        self.cpu.set_reg(reg, val);
    }

    fn get_register_display_value (&self, reg: usize) -> String {
        assert!(reg <= 15);
        let val = self.read_reg(reg).0;
        return match self.register_formats[reg] {
            RegFormat::Bin => format!("{:#034b}", val),
            RegFormat::Oct => format!("{:#013o}", val),
            RegFormat::Dec => format!("{}", val),
            RegFormat::Sig => format!("{}", val as i32),
            RegFormat::Hex => format!("{:#010X}", val),
        }
    }

    fn set_display_format (&mut self, reg: usize, kind: RegFormat) {
        assert!(reg <= 15);
        self.register_formats[reg] = kind;
    }

    fn read_pc (&self) -> Wrapping<u32> {
        return self.read_reg(15);
    }

    fn inc_pc (&mut self) {
        self.cpu.registers[15] += Wrapping(1);
    }

    fn dec_pc (&mut self) {
        self.cpu.registers[15] -= Wrapping(1);
    }

    fn set_flags_nzcv (&mut self, v1: Wrapping<u32>, v2: Wrapping<u32>, result: Wrapping<u32>) {
        self.cpu.set_flags_nzcv(v1, v2, result);
    }

    fn save_reg_state (&mut self, reg: usize) {
        self.command_stack.push(Transition {
            reg: reg,
            val: self.read_reg(reg),
            flags: self.cpu.get_flag_state()
        });
    }

    fn mov_imm (&mut self, dest: usize, val: u32, flags: bool) {
        assert!(dest <= 15);
        self.inc_pc();
        self.save_reg_state(dest);

        if flags {
            self.cpu.flags.n = (val as i32) < 0;
            self.cpu.flags.z = val == 0;
        }

        self.set_reg(dest, Wrapping(val));
    }

    fn mov_reg (&mut self, to: usize, from: usize, flags: bool) {
        assert!(to <= 15 && from <= 15);
        self.inc_pc();
        self.save_reg_state(to);

        let val = self.read_reg(from);
        if flags {
            self.cpu.flags.n = (val.0 as i32) < 0;
            self.cpu.flags.z = val.0 == 0;
        }
        self.set_reg(to, val);
    }

    fn add_imm (&mut self, dest: usize, first: usize, val: u32, flags: bool) {
        assert!(dest <= 15 && first <= 15);
        self.inc_pc();
        self.save_reg_state(dest);

        let orig = self.read_reg(first);
        let result = orig + Wrapping(val);

        if flags {
            self.set_flags_nzcv(Wrapping(val), orig, result);
        }

        self.set_reg(dest, result);
    }

    fn add_reg (&mut self, dest: usize, first: usize, second: usize, flags: bool) {
        assert!(dest <= 15 && first <= 15 && second <= 15);
        self.inc_pc();
        self.save_reg_state(dest);

        let v1 = self.read_reg(first);
        let v2 = self.read_reg(second);
        let result = v1 + v2;
        if flags {
            self.set_flags_nzcv(v1, v2, result);
        }

        self.set_reg(dest, result);
    }

    fn sub_imm (&mut self, dest: usize, first: usize, val: u32, flags: bool) {
        assert!(dest <= 15 && first <= 15);
        self.inc_pc();
        self.save_reg_state(dest);

        let orig = self.read_reg(first);
        let result = orig - Wrapping(val);

        if flags {
            self.set_flags_nzcv(Wrapping(val), orig, result);
        }

        self.set_reg(dest, result);
    }

    fn sub_reg (&mut self, dest: usize, first: usize, second: usize, flags: bool) {
        assert!(dest <= 15 && first <= 15 && second <= 15);
        self.inc_pc();
        self.save_reg_state(dest);

        let v1 = self.read_reg(first);
        let v2 = self.read_reg(second);
        let result = v1 - v2;
        if flags {
            self.set_flags_nzcv(v1, v2, result);
        }

        self.set_reg(dest, result);
    }

    fn ldr_reg (&mut self, dest: usize, address: usize, flags: bool) {
        assert!(dest <= 15 && address <= 15);

        let address_val = self.read_reg(address).0;
        if address_val < 0x20000000 || address_val > (0x20020000 - 4) {
            println!("Invalid address {:#X} in r{}", address_val, address);
            return;
        }

        let pointer = (address_val - 0x20000000) as usize;
        let mut val = self.memory.data[pointer] as u32;
        val += (self.memory.data[pointer + 1] as u32) << 8;
        val += (self.memory.data[pointer + 2] as u32) << 16;
        val += (self.memory.data[pointer + 3] as u32) << 24;

        self.save_reg_state(dest);

        if flags {
            self.cpu.flags.n = (val as i32) < 0;
            self.cpu.flags.z = val == 0;
        }

        self.set_reg(dest, Wrapping(val));
    }

    fn str_reg (&mut self, source: usize, address: usize) {
        assert!(source <= 15 && address <= 15);

        let address_val = self.read_reg(address).0;
        if address_val < 0x20000000 || address_val > (0x20020000 - 4) {
            println!("Invalid address {:#X} in r{}", address_val, address);
            return;
        }

        let mut source_val = self.read_reg(source).0;

        let pointer = (address_val - 0x20000000) as usize;

        self.memory.data[pointer] = source_val as u8;
        source_val = source_val >> 8;
        self.memory.data[pointer + 1] = source_val as u8;
        source_val = source_val >> 8;
        self.memory.data[pointer + 2] = source_val as u8;
        source_val = source_val >> 8;
        self.memory.data[pointer + 3] = source_val as u8;
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
            registers.push_str(&format!("{}{: >3}: {: <34}  {: >3}: {: <34}\n", indent, left_label, left, right_label, right));
        }
        for i in 4..8 {
            let left = self.get_register_display_value(i);
            let right = self.get_register_display_value(i + 8);
            let special = ["r12", "sp", "lr", "pc"];
            let left_label = format!("r{}", i);

            registers.push_str(&format!("{}{: >3}: {: <34}  {: >3}: {: <34}\n", indent, left_label, left, special[i - 4], right));
        }
        registers.push('\n');
        registers.push_str(&format!("{}{}\n", indent, self.cpu.flags));
        return write!(f, "CPU {{\n{}}}", registers);
    }
}


fn get_register (val: String) -> Option<usize> {
    lazy_static! {
        static ref REG_RE: Regex = Regex::new(r"^r[0-9]+$").unwrap();
    }
    if !REG_RE.is_match(&val) { return None };

    let num = val.get(1..).unwrap().parse::<usize>().unwrap();

    return if num > 15 { None } else { Some(num) };
}


fn get_number (val: String) -> Option<u32> {
    lazy_static! {
        static ref NUM_RE: Regex = Regex::new(r"^\-?(?:0([box]))?([[:xdigit:]]+)$").unwrap();
    }
    if !NUM_RE.is_match(&val) { return None };

    let capture = NUM_RE.captures(&val).unwrap();

    return match capture.get(1) {
        Some(letter) => {
            match letter.as_str() {
                "b" => u32::from_str_radix(capture.get(2).unwrap().as_str(), 2).ok(),
                "o" => u32::from_str_radix(capture.get(2).unwrap().as_str(), 8).ok(),
                "x" => u32::from_str_radix(capture.get(2).unwrap().as_str(), 16).ok(),
                _ => None,
            }
        },
        _ => {
            match val.parse::<i64>() {
                Ok(num) => Some(num as u32),
                _ => None
            }
        }
    }
}


fn parse_input (input: String) -> Option<Instruction> {
    lazy_static! {
        static ref INST_RE: Regex = Regex::new(r"^\s*([a-zA-Z0-9]+)\s+").unwrap();
        static ref ARG_RE: Regex = Regex::new(r"[\s,]+").unwrap();
        static ref NUM_RE: Regex = Regex::new(r"^\-?(?:0[box])?[[:xdigit:]]+$").unwrap();
    }

    if !INST_RE.is_match(&input) { return None };

    let instr = INST_RE.captures(&input).unwrap().get(1).unwrap().as_str();
    let args: Vec<String> = ARG_RE.split(input[instr.len()..input.len()].trim()).map(String::from).collect();

    match instr {
        "exit" => { std::process::exit(0); },
        "format" => {
            if args.len() != 2 { return None };
            let reg = get_register(args[0].clone());
            if reg.is_none() { return None };
            let reg = reg.unwrap();

            return match args[1].as_str() {
                "b" => Some(Instruction::Format { reg, kind: RegFormat::Bin }),
                "o" => Some(Instruction::Format { reg, kind: RegFormat::Oct }),
                "d" => Some(Instruction::Format { reg, kind: RegFormat::Dec }),
                "s" => Some(Instruction::Format { reg, kind: RegFormat::Sig }),
                "h" => Some(Instruction::Format { reg, kind: RegFormat::Hex }),
                _ => None,
            }
        },
        "memview" => {
            if args.len() != 2 { return None };
            let index = get_number(args[0].clone());
            let length = get_number(args[1].clone());
            if index.is_none() || length.is_none() { return None };
            let index = index.unwrap() as usize;
            let length = length.unwrap() as usize;

            return Some(Instruction::Memview { index, length })
        },
        "undo" => { return Some(Instruction::Undo { num: 1 }) },
        "redo" => { return Some(Instruction::Redo { num: 1 }) },
        "ldr" | "ldrs" => {
            if args.len() != 2 { return None };

            let dest = get_register(args[0].clone());
            if dest.is_none() { return None };

            let address = get_register(args[1].clone());
            if address.is_none() { return None };
            return Some(Instruction::LdrReg { dest: dest.unwrap(), address: address.unwrap(), flags: instr == "ldrs" })
        },
        "str" => {
            if args.len() != 2 { return None };

            let val = get_register(args[0].clone());
            if val.is_none() { return None };

            let address = get_register(args[1].clone());
            if address.is_none() { return None };
            return Some(Instruction::StrReg { val: val.unwrap(), address: address.unwrap() })
        },
        "mov" | "movs" => {
            if args.len() != 2 { return None };

            let to = get_register(args[0].clone());
            if to.is_none() { return None };

            if NUM_RE.is_match(&args[1]) {
                let from = get_number(args[1].clone());
                if from.is_none() { return None };
                return Some(Instruction::MovImm { to: to.unwrap(), val: from.unwrap(), flags: instr == "movs" })
            } else {
                let from = get_register(args[1].clone());
                if from.is_none() { return None };
                return Some(Instruction::MovReg { to: to.unwrap(), from: from.unwrap(), flags: instr == "movs" })
            }
        },
        "add" | "adds" => {
            if args.len() < 2 { return None };

            let dest = get_register(args[0].clone());
            if dest.is_none() { return None }

            if args.len() == 2 && NUM_RE.is_match(&args[1]) {
                let val = get_number(args[1].clone());
                if val.is_none() { return None }
                return Some(Instruction::AddImm { dest: dest.unwrap(), first: dest.unwrap(), val: val.unwrap(), flags: instr == "adds" });
            }

            if args.len() == 2 {
                let first = get_register(args[1].clone());
                if first.is_none() { return None }
                return Some(Instruction::AddReg { dest: dest.unwrap(), first: dest.unwrap(), second: first.unwrap(), flags: instr == "adds" });
            }

            if args.len() == 3 && NUM_RE.is_match(&args[2]) {
                let first = get_register(args[1].clone());
                let val = get_number(args[2].clone());
                if first.is_none() || val.is_none() { return None }
                return Some(Instruction::AddImm { dest: dest.unwrap(), first: first.unwrap(), val: val.unwrap(), flags: instr == "adds" });
            }

            if args.len() == 3 {
                let first = get_register(args[1].clone());
                let second = get_register(args[2].clone());
                if first.is_none() || second.is_none() { return None }
                return Some(Instruction::AddReg { dest: dest.unwrap(), first: first.unwrap(), second: second.unwrap(), flags: instr == "adds" });
            }

            return None;
        },
        "sub" | "subs" => {
            if args.len() < 2 { return None };

            let dest = get_register(args[0].clone());
            if dest.is_none() { return None }

            if args.len() == 2 && NUM_RE.is_match(&args[1]) {
                let val = get_number(args[1].clone());
                if val.is_none() { return None }
                return Some(Instruction::SubImm { dest: dest.unwrap(), first: dest.unwrap(), val: val.unwrap(), flags: instr == "subs" });
            }

            if args.len() == 2 {
                let first = get_register(args[1].clone());
                if first.is_none() { return None }
                return Some(Instruction::SubReg { dest: dest.unwrap(), first: dest.unwrap(), second: first.unwrap(), flags: instr == "subs" });
            }

            if args.len() == 3 && NUM_RE.is_match(&args[2]) {
                let first = get_register(args[1].clone());
                let val = get_number(args[2].clone());
                if first.is_none() || val.is_none() { return None }
                return Some(Instruction::SubImm { dest: dest.unwrap(), first: first.unwrap(), val: val.unwrap(), flags: instr == "subs" });
            }

            if args.len() == 3 {
                let first = get_register(args[1].clone());
                let second = get_register(args[2].clone());
                if first.is_none() || second.is_none() { return None }
                return Some(Instruction::SubReg { dest: dest.unwrap(), first: first.unwrap(), second: second.unwrap(), flags: instr == "subs" });
            }

            return None;
        },
        _ => return None,
    }
}


fn main () {
    println!("Welcome to ARM simulator");

    let mut board = Board::new();

    match board.load_elf_from_path("/home/benjamin/gitlab/2300/test/.pioenvs/disco_l476vg/firmware.elf") {
        Ok(_) => {}
        Err(s) => {
            println!("{}", s);
            return;
        }
    };

    match board.next_instruction(false) {
        Ok(i) => println!("Ok: {:?}", i),
        Err(e) => println!("Err: {:?}", e),
    };

    loop {
        print!("\n{}\n> ", board);
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        let instruction = parse_input(input);

        if instruction.is_none() {
            println!("Invalid input");
            continue;
        }

        match instruction.unwrap() {
            Instruction::MovImm { to, val, flags } => { board.mov_imm(to, val, flags); },
            Instruction::MovReg { to, from, flags } => { board.mov_reg(to, from, flags); },
            Instruction::AddImm { dest, first, val, flags } => { board.add_imm(dest, first, val, flags); },
            Instruction::AddReg { dest, first, second, flags } => { board.add_reg(dest, first, second, flags); },
            Instruction::SubImm { dest, first, val, flags } => { board.sub_imm(dest, first, val, flags); },
            Instruction::SubReg { dest, first, second, flags } => { board.sub_reg(dest, first, second, flags); },
            Instruction::LdrReg { dest, address, flags } => { board.ldr_reg(dest, address, flags); },
            Instruction::StrReg { val, address } => { board.str_reg(val, address); },
            Instruction::Undo { num } => { board.undo(num); },
            Instruction::Redo { num } => { board.redo(num); },
            Instruction::Format { reg, kind } => { board.set_display_format(reg, kind); },
            Instruction::Memview { index, length } => { board.print_mem_dump(index, length); },
        }
    }
}
