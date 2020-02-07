#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]

#[macro_use]
extern crate goblin;

mod instruction;
use instruction::{CarryChange, Instruction, ShiftType};

mod peripherals;
use peripherals::Peripherals;

mod audio;
use audio::{AudioHandler};

mod bytecode;
use bytecode::{InstructionCache, ItPos, InstructionContext, decode_thumb, tag, opcode::{Opcode}};

mod utils;
use utils::bits::{bitset, add_with_carry, shift, shift_c, align, word_align, sign_extend, shifted_sign_extend};

mod board;
use board::{Board};

mod gui;

use goblin::elf::Elf;

use std::env;
use std::path::{PathBuf, Path};
use std::ffi::{OsString};
use std::hint::unreachable_unchecked;
use std::collections::HashMap;
use std::{fmt, fs, string::String, option::Option};
use std::io::{self, Write};
use std::num::Wrapping;

pub type ByteInstruction = (u32, u32); // Intermediate bytecode format for more efficient decode and execution

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

// impl MemoryBus {
//     fn new() -> MemoryBus {
//         return MemoryBus {
//             flash: Box::new([0xFF; 1024 * 1024]),
//             data: Box::new([0xFF; 128 * 1024]),
//             peripherals: Peripherals::new(),
//         };
//     }
// 
//     fn load_elf(&mut self, elf: Elf, bytes: &[u8]) -> Result<(), String> {
//         for header in elf.program_headers.iter() {
//             if header.p_type != goblin::elf::program_header::PT_LOAD {
//                 return Err(String::from("Unexpected program header type"));
//             }
// 
//             if header.p_vaddr < 0x0800_0000 {
//                 continue;
//             }
// 
//             let phys_adr = header.p_paddr as usize;
//             let offset = header.p_offset as usize;
//             let size = header.p_filesz as usize;
// 
//             let start_index = phys_adr - 0x0800_0000;
// 
//             if start_index + size > self.flash.len() {
//                 return Err(String::from("Flash too small to fit content"));
//             }
// 
//             for i in 0..size {
//                 self.flash[i + start_index] = bytes[i + offset];
//             }
//         }
//         return Ok(());
//     }
// 
//     fn print_mem_dump(&self, address: u32, length: usize) {
//         // let mut c = 16;
//         // for i in address..(address + (length as u32) * 4) {
//         //     if c == 0 {
//         //         c = 16;
//         //         print!("\n");
//         //     }
//         //     c -= 1;
//         //     let val = match self.read_byte(i) {
//         //         Ok(v) => v,
//         //         Err(e) => {
//         //             println!("{}", e);
//         //             return;
//         //         }
//         //     };
//         //     print!("{:02X} ", val);
//         // }
//         // print!("\n");
//     }
// 
//     fn print_mem_area(&self, address: u32) {
//         let padding = "   ".repeat((address & 0xF) as usize);
//         // println!("{}v{:#010X}", padding.to_string(), address);
//         self.print_mem_dump(address & !0xF, 0x8);
//     }
// 
//     fn read_mem_a(&self, address: u32, size: usize) -> Result<u32, String> {
//         // B2.3.4 p583
//         return self.read_mem_a_with_priv(address, size, &AccessType::Normal);
//     }
// 
//     fn read_mem_a_with_priv(&self, address: u32, size: usize, _access_type: &AccessType) -> Result<u32, String> {
//         // B2.3.4 p583
//         // if address != align(address, size as u32) {
//         //     // Set UFSR.UNALIGNED = true;
//         //     panic!("UsageFault");
//         // }
// 
//         // let memaddrdesc = validate_address(address, access_type, false); // TODO
//         let location = self.address_to_physical(address)?;
//         return match location {
//             Location::Flash(i) => read_value(&*self.flash, i, size),
//             Location::Ram(i) => read_value(&*self.data, i, size),
//             Location::Peripheral(i) => self.peripherals.read(i, size),
//         };
//     }
// 
//     fn read_mem_u(&self, address: u32, size: usize) -> Result<u32, String> {
//         // B2.3.5 p584
//         return self.read_mem_u_with_priv(address, size, &AccessType::Normal);
//     }
// 
//     fn read_mem_u_with_priv(&self, address: u32, size: usize, access_type: &AccessType) -> Result<u32, String> {
//         // B2.3.5 p585
//         if address == align(address, size as u32) {
//             return self.read_mem_a_with_priv(address, size, access_type);
//         } else if /* CCR.UNALIGN_TRP */ false {
//             // USFR.UNALIGNED = true;
//             panic!("UsageFault");
//         } else {
//             let mut result: u32 = 0;
//             for i in 0..(size as u32) {
//                 result += self.read_mem_a_with_priv(address + i, 1, &access_type)? << (8 * i);
//             }
//             return Ok(result);
//         }
//     }
// 
//     fn read_word(&self, address: u32) -> Result<u32, String> {
//         self.print_mem_area(address);
//         return self.read_mem_u(address, 4);
//     }
// 
//     fn read_halfword(&self, address: u32) -> Result<u16, String> {
//         return Ok(self.read_mem_u(address, 2)? as u16);
//     }
// 
//     fn read_byte(&self, address: u32) -> Result<u8, String> {
//         return Ok(self.read_mem_u(address, 1)? as u8);
//     }
// 
//     fn read_byte_unpriv(&self, address: u32) -> Result<u8, String> {
//         return self.read_byte(address);
//     }
// 
//     fn write_mem_u(&mut self, address: u32, size: usize, value: u32) -> Result<(), String> {
//         let location = self.address_to_physical(address)?;
//         return match location {
//             Location::Flash(_) => Err(String::from("Cannot write to Flash memory")),
//             Location::Ram(i) => {
//                 let r = write_value(value, &mut *self.data, i, size);
//                 self.print_mem_area(address);
//                 r
//             }
//             Location::Peripheral(_) => self.peripherals.write(address, value, size),
//         }
//     }
// 
//     fn write_word(&mut self, address: u32, value: u32) -> Result<(), String> {
//         return self.write_mem_u(address, 4, value);
//     }
// }

fn locate_elf_file() -> Option<std::path::PathBuf> {
    let args: Vec<OsString> = env::args_os().collect();
    if args.len() >= 2 {
        return Some(PathBuf::from(args[1].clone()));
    }

    let mut project_kind: Option<String> = None;
    let working = std::env::current_dir().expect("cannot find or access working directory");
    for dir in working.read_dir().expect("cannot read working directory") {
        match dir {
            Ok(dir) => {
                let file_name = dir.file_name();
                if file_name == ".pio" || file_name == ".pioenvs" {
                    project_kind = Some(file_name.into_string().unwrap());
                    break;
                }
            }
            Err(_) => {}
        }
    }

    return match project_kind {
        Some(s) => {
            let elf_path: std::path::PathBuf = if s == ".pio" {
                [".pio", "build", "disco_l476vg", "firmware.elf"].iter().collect()
            } else {
                [".pioenvs", "disco_l476vg", "firmware.elf"].iter().collect()
            };
            Some(working.join(elf_path))
        }
        None => None
    }
}

use std::io::{stdin, stdout, Read};
use std::{time, thread};
fn main() {
    let path = match locate_elf_file() {
        Some(p) => p,
        None => {
            println!("Cannot detect ELF file");
            return;
        }
    };

    let mut board = Board::new();
    board.load_elf_from_path(&path).unwrap();
    println!("\n{}\n", board);
    println!("finished init");
    // board.spawn_audio();

    while board.read_instruction_pc() != 0x080f2f60 {
        board.step().unwrap();
    }

    loop {
        board.step().unwrap();
        // println!("\n{}\n", board);
        // write!(stdout, "Press enter to continue...").unwrap();
        // stdout.flush().unwrap();
        // let _ = stdin.read(&mut [0u8]).unwrap();
    }
}
