#![allow(dead_code)]
#![allow(unused_imports)]

#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate goblin;


mod instruction;

use instruction::{Instruction, RegFormat, Condition};

use std::time::{SystemTime};
use std::num::Wrapping;
use std::option::Option;
use std::vec::Vec;
use std::string::String;
use std::fmt;
use std::io::{self, Write};
use std::fs;
use std::collections::HashMap;
use regex::Regex;
use goblin::elf::{Elf};


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
}


impl CPU {
    fn new () -> CPU {
        return CPU {
            registers: [Wrapping(0xABCD1234); 16],
            flags: Flags { n: false, z: true, c: true, v: false, q: false, },
        };
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

    fn check_condition (&self, cond: Condition) -> bool {
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
            Condition::SLowerSame =>  self.flags.z || (self.flags.n != self.flags.v),
            Condition::Always => true,
        }
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
            data: [0xFF; 128 * 1024],
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

            for i in 0..size {
                self.flash[i] = bytes[i + offset];
            }
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

    fn get_instr_word (&self, address: u32) -> Result<u32, &str> {
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

    fn get_flash_capacity (&self) -> u32 {
        return self.flash.len() as u32 - 4;
    }

    fn get_data_capacity (&self) -> u32 {
        return self.data.len() as u32;
    }

    fn get_word (&self, address: u32) -> Result<u32, &str> {
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
            return Err("Out of bounds access");
        };
    }

    fn write_word (&mut self, address: u32, val: u32) {
        if 0x2000_0000 <= address && address <= 0x2000_0000 + self.get_data_capacity() - 4 {
            let base = (address - 0x2000_0000) as usize;
            self.data[base] = (val & 0xFF) as u8;
            self.data[base + 1] = ((val >> 8) & 0xFF) as u8;
            self.data[base + 2] = ((val >> 16) & 0xFF) as u8;
            self.data[base + 3] = (val >> 24) as u8;
        }
    }
}


#[derive(Debug)]
struct Board {
    cpu: CPU,
    memory: MemoryBus,
    command_stack: Vec<Transition>,
    register_formats: [RegFormat; 16],
    branch_map: HashMap<u32, bool>,
}


impl Board {
    fn new () -> Board {
        return Board {
            cpu: CPU::new(),
            memory: MemoryBus::new(),
            command_stack: Vec::new(),
            register_formats: [RegFormat::Hex; 16],
            branch_map: HashMap::new(),
        }
    }

    fn next_instruction (&mut self, update_pc: bool) -> Result<Instruction, &str> {
        let pc = self.read_pc().0;

        let val = match self.memory.get_instr_word(pc) {
            Ok(v) => v,
            Err(e) => return Err(e),
        };

        println!("Reading: {:#010X}", val);

        let (instr, wide) = Instruction::from(val, pc);

        println!("Instruction is {}", if wide { "wide" } else { "short" });

        if update_pc {
            self.cpu.registers[15] += Wrapping(if wide { 4 } else { 2 });
        }

        return Ok(instr);
    }

    fn execute (&mut self, instr: Instruction) {
        match instr {
            Instruction::LdrLit { rt, address } => self.ldr_lit(rt as usize, address),
            Instruction::LdrImm { rn, rt, offset, add, index, wback } => self.ldr_imm(rt as usize, rn as usize, offset, index, add, wback),
            Instruction::MovImm { rd, val, flags } => self.mov_imm(rd as usize, val, flags),
            Instruction::MovReg { to, from, flags } => self.mov_reg(to as usize, from as usize, flags),
            Instruction::AddReg { rd, rm, rn, flags } => self.add_reg(rd as usize, rm as usize, rn as usize, flags),
            Instruction::Branch { offset } => self.branch(offset),
            Instruction::LinkedBranch { offset } => self.branch_with_link(offset),
            Instruction::CondBranch { offset, cond } => self.branch_cond(offset, cond),
            Instruction::CmpReg { rm, rn } => self.cmp_reg(rm as usize, rn as usize),
            Instruction::StrImm { rn, rt, offset, add, index, wback } => self.str_imm(rt as usize, rn as usize, offset, index, add, wback),
            Instruction::Push { registers } => self.push(registers),
            Instruction::AddSpImm { rd, val, flags } => self.add_sp_imm(rd as usize, val, flags),
            Instruction::Undefined => {}
            Instruction::Unpredictable => {
                println!("Spooky");
            }
            Instruction::Unimplemented => {
                println!("Working on it");
            }
            _ => println!("Unhandled instruction")
        }
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

        for sym in elf.syms.iter() {
            let offset = sym.st_name;
            let name = match elf.strtab.get(offset) {
                Some(Ok(s)) => s,
                _ => return Err(String::from("missing symbols")),
            };

            if name == "SystemInit" || name == "__libc_init_array" || name == "init" {
                let address = (sym.st_value & 0xFFFFFFFE) as u32;
                self.branch_map.insert(address, false);
            }
        }

        println!("bmap: {:#?}", self.branch_map);

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
        self.cpu.registers[15] += Wrapping(2);
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

        if flags {
            self.cpu.flags.n = (val as i32) < 0;
            self.cpu.flags.z = val == 0;
        }

        self.set_reg(dest, Wrapping(val));
    }

    fn branch (&mut self, offset: i32) {
        self.cpu.registers[15] += Wrapping(offset as u32) + Wrapping(2); // TODO: Precisely determine proper PC value
    }

    fn branch_cond (&mut self, offset: i32, cond: Condition) {
        if self.cpu.check_condition(cond) {
            self.branch(offset);
        }
    }

    fn branch_with_link (&mut self, offset: i32) {
        let pc = self.read_pc();
        let new_address = (pc + Wrapping(offset as u32)).0;


        match self.branch_map.get(&new_address) {
            Some(b) if !b => {
                println!("Skipping branch with link");
                return;
            }
            _ => {}
        }

        self.cpu.registers[14] = pc | Wrapping(0b1);
        self.branch(offset - 2); // TODO: Proper PC align
    }

    fn cmp_reg (&mut self, rm: usize, rn: usize) {
        assert!(rm <= 15 && rn <= 15);
        let vm = self.read_reg(rm);
        let vn = self.read_reg(rn);
        self.cpu.set_flags_nzcv(vm, vn, vn - vm);
    }

    fn mov_reg (&mut self, to: usize, from: usize, flags: bool) {
        assert!(to <= 15 && from <= 15);
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

        let orig = self.read_reg(first);
        let result = orig + Wrapping(val);

        if flags {
            self.set_flags_nzcv(Wrapping(val), orig, result);
        }

        self.set_reg(dest, result);
    }

    fn add_reg (&mut self, rd: usize, rm: usize, rn: usize, flags: bool) {
        assert!(rd <= 15 && rm <= 15 && rn <= 15);

        let v1 = self.read_reg(rm);
        let v2 = self.read_reg(rn);
        let result = v1 + v2;
        if flags {
            self.set_flags_nzcv(v1, v2, result);
        }

        self.set_reg(rd, result);
    }

    fn sub_imm (&mut self, dest: usize, first: usize, val: u32, flags: bool) {
        assert!(dest <= 15 && first <= 15);

        let orig = self.read_reg(first);
        let result = orig - Wrapping(val);

        if flags {
            self.set_flags_nzcv(Wrapping(val), orig, result);
        }

        self.set_reg(dest, result);
    }

    fn sub_reg (&mut self, dest: usize, first: usize, second: usize, flags: bool) {
        assert!(dest <= 15 && first <= 15 && second <= 15);

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

    fn ldr_lit(&mut self, dest: usize, address: u32) {
        let val = self.memory.get_word(address).unwrap();
        self.set_reg(dest, Wrapping(val));
    }

    fn ldr_imm (&mut self, rt: usize, rn: usize, offset: u16, index: bool, add: bool, wback: bool) {
        assert!(rt <= 15 && rn <= 15);
        let offset = offset as u32;

        let address = self.read_reg(rn);
        let offset_address = if add {
            address + Wrapping(offset)
        } else {
            address - Wrapping(offset)
        };

        let address = if index { offset_address } else { address };

        let val = match self.memory.get_word(address.0) {
            Ok(w) => w,
            Err(_) => {
                println!("Unhandled access");
                return;
            } // TODO: Return error
        };

        if wback {
            self.set_reg(rn, offset_address);
        }

        self.set_reg(rt, Wrapping(val)); // TODO: Special case PC
    }

    fn str_imm (&mut self, source: usize, address_reg: usize, offset: u16, index: bool, add: bool, wback: bool) {
        assert!(source <= 15 && address_reg <= 15);
        let offset = offset as u32;

        let source = self.read_reg(source).0;
        let address = self.read_reg(address_reg).0;

        let offset_address = if add {
            address + offset
        } else {
            address - offset
        };

        let address = if index { offset_address } else { address };

        self.memory.write_word(address, source);

        if wback {
            self.set_reg(address_reg, Wrapping(offset_address));
        }
    }

    fn push (&mut self, registers: u16) {
        let mut sp = self.cpu.registers[13];

        for i in (0..16).rev() {
            if (registers & (1 << i)) == 0 { continue; }
            self.memory.write_word(sp.0, self.read_reg(i).0);
            sp -= Wrapping(4);
        }

        self.cpu.registers[13] = sp;
    }

    fn add_sp_imm (&mut self, rd: usize, val: u32, flags: bool) {
        assert!(rd <= 15);

        let orig = self.read_reg(13);
        let result = orig + Wrapping(val);

        if flags {
            self.set_flags_nzcv(Wrapping(val), orig, result);
        }

        self.set_reg(rd, result);
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


fn main () {
    println!("Welcome to ARM simulator");

    let mut board = Board::new();

    match board.load_elf_from_path("/home/benjamin/gitlab/2300/labs/comp2300-2019-lab-2/.pio/build/disco_l476vg/firmware.elf") {
        Ok(_) => {}
        Err(s) => {
            println!("{}", s);
            return;
        }
    };

    println!("\n{}\n", board);
    println!("finished init");

    let now = SystemTime::now();

    loop {
        print!("press enter to continue");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        match board.next_instruction(true) {
            Ok(i) => {
                println!("Ok: {:?}", i);
                board.execute(i);
                println!("\n{}\n", board);
            }
            Err(e) => {
                println!("Err: {}", e);
                return;
            }
        };

        if board.read_reg(0).0 == 0xFFFFFFF {
            match now.elapsed() {
                Ok(elapsed) => {
                    println!("{}", elapsed.as_millis());
                    println!("{}", board)
                }
                Err(e) => {
                    println!("Error: {:?}", e);
                }
            }
            return;
        }
    }
}
