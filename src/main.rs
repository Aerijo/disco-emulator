#![allow(dead_code)]

#[macro_use]
extern crate lazy_static;
extern crate regex;

use std::num::Wrapping;
use std::option::Option;
use std::io::{self, Write};
use regex::Regex;


#[derive(Debug)]
struct Flags {
    n: bool, // negative
    z: bool, // zero
    c: bool, // carry
    v: bool, // overflow
    q: bool, // saturation
}


#[derive(Debug)]
struct CPU {
    registers: [Wrapping<u32>; 16],
    flags: Flags,
}


impl CPU {
    fn new () -> CPU {
        return CPU {
            registers: [Wrapping(0); 16],
            flags: Flags {
                n: false,
                z: false,
                c: false,
                v: false,
                q: false,
            }
        }
    }

    fn read_reg (&self, reg: usize) -> u32 {
        assert!(reg <= 15);
        return self.registers[reg].0;
    }

    fn set_reg (&mut self, reg: usize, val: u32) {
        assert!(reg <= 15);
        self.registers[reg] = Wrapping(val);
    }

    fn read_pc (&self) -> u32 {
        return self.registers[15].0;
    }

    fn inc_pc (&mut self) {
        self.registers[15] += Wrapping(1);
    }

    fn set_flags_nzcv (&mut self, v1: Wrapping<u32>, v2: Wrapping<u32>, result: Wrapping<u32>) {
        self.flags.n = (result.0 as i32) < 0;
        self.flags.z = result.0 == 0;
        self.flags.c = result < v1 || result < v2;
        self.flags.v = (((v1.0 & v2.0 & !result.0) | (!v1.0 & !v2.0 & result.0)) & (1 << 31)) > 0;
    }

    fn mov_imm (&mut self, dest: usize, val: u32, flags: bool) {
        self.inc_pc();
        if flags {
            self.flags.n = (val as i32) < 0;
            self.flags.z = val == 0;
        }
        self.set_reg(dest, val);
    }

    fn mov_reg (&mut self, to: usize, from: usize, flags: bool) {
        assert!(to <= 15 && from <= 15);
        self.inc_pc();
        let val = self.registers[from];
        if flags {
            self.flags.n = (val.0 as i32) < 0;
            self.flags.z = val.0 == 0;
        }
        self.registers[to] = val;
    }

    fn add_imm (&mut self, dest: usize, first: usize, val: u32, flags: bool) {
        assert!(dest <= 15 && first <= 15);
        self.inc_pc();

        let orig = self.registers[first];
        let result = orig + Wrapping(val);

        if flags {
            self.set_flags_nzcv(Wrapping(val), orig, result);
        }

        self.registers[dest] = result;
    }

    fn add_reg (&mut self, dest: usize, first: usize, second: usize, flags: bool) {
        assert!(dest <= 15 && first <= 15 && second <= 15);
        self.inc_pc();

        let v1 = self.registers[first];
        let v2 = self.registers[second];
        let result = v1 + v2;
        if flags {
            self.set_flags_nzcv(v1, v2, result);
        }

        self.registers[dest] = result;
    }

    fn sub_imm (&mut self, dest: usize, first: usize, val: u32, flags: bool) {
        assert!(dest <= 15 && first <= 15);
        self.inc_pc();

        let orig = self.registers[first];
        let result = orig - Wrapping(val);

        if flags {
            self.set_flags_nzcv(Wrapping(val), orig, result);
        }

        self.registers[dest] = result;
    }

    fn sub_reg (&mut self, dest: usize, first: usize, second: usize, flags: bool) {
        assert!(dest <= 15 && first <= 15 && second <= 15);
        self.inc_pc();

        let v1 = self.registers[first];
        let v2 = self.registers[second];
        let result = v1 - v2;
        if flags {
            self.set_flags_nzcv(v1, v2, result);
        }

        self.registers[dest] = result;
    }
}


enum Instruction {
    MovImm { to: usize, val: u32, flags: bool },
    MovReg { to: usize, from: usize, flags: bool },
    AddImm { dest: usize, first: usize, val: u32, flags: bool },
    AddReg { dest: usize, first: usize, second: usize, flags: bool },
    SubImm { dest: usize, first: usize, val: u32, flags: bool },
    SubReg { dest: usize, first: usize, second: usize, flags: bool },
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
        static ref NUM_RE: Regex = Regex::new(r"^\-?[0-9]+$").unwrap();
    }
    if !NUM_RE.is_match(&val) { return None };

    let num = val.parse::<i32>().unwrap();

    return Some(num as u32);
}


fn parse_input (input: String) -> Option<Instruction> {
    lazy_static! {
        static ref INST_RE: Regex = Regex::new(r"^\s*([a-zA-Z0-9]+)\s+").unwrap();
        static ref ARG_RE: Regex = Regex::new(r"[\s,]+").unwrap();
        static ref NUM_RE: Regex = Regex::new(r"^\-?\d+$").unwrap();
    }

    if !INST_RE.is_match(&input) { return None };

    let instr = INST_RE.captures(&input).unwrap().get(1).unwrap().as_str();
    let args: Vec<String> = ARG_RE.split(input[instr.len()..input.len()].trim()).map(String::from).collect();

    match instr {
        "mov" | "movs" => {
            if args.len() != 2 || args[0].len() == 0 || args[1].len() == 0 { return None };

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
            if args.len() < 2 || args[0].len() == 0 || args[1].len() == 0 { return None };

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
            if args.len() < 2 || args[0].len() == 0 || args[1].len() == 0 { return None };

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
        }
        _ => return None,
    }
}


fn main () {
    println!("Welcome to ARM simulator");

    let mut regs = CPU::new();

    loop {
        print!("\n{:?}\n> ", regs);
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        let instruction = parse_input(input);

        if instruction.is_none() {
            println!("Invalid input");
            continue;
        }

        match instruction.unwrap() {
            Instruction::MovImm { to, val, flags } => { regs.mov_imm(to, val, flags); },
            Instruction::MovReg { to, from, flags } => { regs.mov_reg(to, from, flags); },
            Instruction::AddImm { dest, first, val, flags } => { regs.add_imm(dest, first, val, flags); },
            Instruction::AddReg { dest, first, second, flags } => { regs.add_reg(dest, first, second, flags); },
            Instruction::SubImm { dest, first, val, flags } => { regs.sub_imm(dest, first, val, flags); },
            Instruction::SubReg { dest, first, second, flags } => { regs.sub_reg(dest, first, second, flags); },
            // _ => { println!("Valid input (unhandled)"); }
        }
    }
}
