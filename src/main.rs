#[macro_use] extern crate lazy_static;
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
    flags: Flags
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
                q: false
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
        if flags {
            self.flags.n = (val as i32) < 0;
            self.flags.z = val == 0;
        }
        self.set_reg(dest, val);
    }

    fn mov_reg (&mut self, to: usize, from: usize) {
        assert!(to <= 15 && from <= 15);
        self.inc_pc();
        self.registers[to] = self.registers[from];
    }

    fn add_reg (&mut self, dest: usize, first: usize, second: usize) {
        assert!(dest <= 15 && first <= 15 && second <= 15);
        self.inc_pc();
        self.registers[dest] = self.registers[first] + self.registers[second];
    }

    fn adds_reg (&mut self, dest: usize, first: usize, second: usize) {
        assert!(dest <= 15 && first <= 15 && second <= 15);
        self.inc_pc();

        let v1 = self.registers[first];
        let v2 = self.registers[second];
        let result = v1 + v2;
        self.set_flags_nzcv(v1, v2, result);

        self.registers[dest] = result;
    }

    fn sub_reg (&mut self, dest: usize, first: usize, second: usize) {
        assert!(dest <= 15 && first <= 15 && second <= 15);
        self.inc_pc();
        self.registers[dest] = self.registers[first] - self.registers[second];
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


fn parse_input (input: String) -> Option<Instruction> {
    lazy_static! {
        static ref instr_regex: Regex = Regex::new(r"^\s*([a-zA-Z0-9]+)").unwrap();
    }

    if !instr_regex.is_match(&input) { return None };

    let instr = instr_regex.captures(&input).unwrap().get(1).unwrap().as_str();

    match instr {
        "mov" => return Some(Instruction::MovImm { to: 0, val: (-100i32 as u32), flags: false }),
        "movs" => return Some(Instruction::MovImm { to: 0, val: (-100i32 as u32), flags: true }),
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

        match instruction {
            Some (instr) => {
                match instr {
                    Instruction::MovImm { to, val, flags } => {
                        println!("Valid mov");
                        regs.mov_imm(to, val, flags);
                    },
                    _ => {println!("Valid input");}
                }
            },
            None => println!("Invalid input"),
        }
    }
}
