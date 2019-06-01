extern crate regex;

use std::num::Wrapping;
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

        self.flags.n = (result.0 as i32) < 0;
        self.flags.z = result.0 == 0;
        self.flags.c = result < v1 || result < v2;
        self.flags.v = ((!(v1.0 ^ v2.0) ^ result.0) & (1 << 31)) == 0; // highest bit was same,


        self.registers[dest] = result;
    }

    fn sub_reg (&mut self, dest: usize, first: usize, second: usize) {
        assert!(dest <= 15 && first <= 15 && second <= 15);
        self.inc_pc();
        self.registers[dest] = self.registers[first] - self.registers[second];
    }
}


fn main () {
    println!("Welcome to ARM simulator");

    let mut regs = CPU::new();

    let instruction_regex = Regex::new(r"(add|sub|mov) r([0-9]+), r([0-9]+)(?:, r([0-9]+))?").unwrap();

    loop {
        println!("{:?}", regs);
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();

        io::stdin().read_line(&mut input).unwrap();

        match instruction_regex.captures(&input) {
            Some(caps) => {
                let dest: usize = caps.get(2).unwrap().as_str().parse().unwrap();
                let first: usize = caps.get(3).unwrap().as_str().parse().unwrap();

                match caps.get(1).unwrap().as_str() {
                    "add" => {
                        let second: usize = caps.get(4).unwrap().as_str().parse().unwrap();
                        regs.adds_reg(dest, first, second);
                    },
                    "sub" => {
                        let second: usize = caps.get(4).unwrap().as_str().parse().unwrap();
                        regs.sub_reg(dest, first, second);
                    },
                    "mov" => {
                        regs.mov_reg(dest, first);
                    }
                    _ => {}
                }
            },
            None => {
                println!("Invalid input");
            }
        }
    }
}
