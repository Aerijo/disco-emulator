#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]

#[macro_use]
extern crate goblin;
extern crate cpal;

mod instruction;
use instruction::{CarryChange, Condition, Instruction, ShiftType};

mod peripherals;
use peripherals::Peripherals;

mod bytecode;
use bytecode::{InstructionCache, ItPos, InstructionContext, decode_thumb, tag, opcode::{Opcode}};

mod cpu;
use cpu::{CPU, ExecMode};

mod utils;
use utils::bits::{bitset, add_with_carry, shift, shift_c, align, word_align, sign_extend, shifted_sign_extend};

use goblin::elf::Elf;
use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::io::{self, Write};
use std::num::Wrapping;
use std::option::Option;
use std::string::String;
use std::time::SystemTime;
use std::vec::Vec;

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
        // let mut c = 16;
        // for i in address..(address + (length as u32) * 4) {
        //     if c == 0 {
        //         c = 16;
        //         print!("\n");
        //     }
        //     c -= 1;
        //     let val = match self.read_byte(i) {
        //         Ok(v) => v,
        //         Err(e) => {
        //             println!("{}", e);
        //             return;
        //         }
        //     };
        //     print!("{:02X} ", val);
        // }
        // print!("\n");
    }

    fn print_mem_area(&self, address: u32) {
        let padding = "   ".repeat((address & 0xF) as usize);
        // println!("{}v{:#010X}", padding.to_string(), address);
        self.print_mem_dump(address & !0xF, 0x8);
    }

    fn get_instr_word(&self, address: u32) -> Result<u32, String> {
        if 0x0800_0000 <= address && address <= 0x0800_0000 + (self.flash.len() as u32) {
            let base = (address - 0x0800_0000) as usize;
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
        // if address != align(address, size as u32) {
        //     // Set UFSR.UNALIGNED = true;
        //     panic!("UsageFault");
        // }

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
        self.print_mem_area(address);
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

    fn write_mem_u(&mut self, address: u32, size: usize, value: u32) -> Result<(), String> {
        let location = self.address_to_physical(address)?;
        return match location {
            Location::Flash(_) => Err(String::from("Cannot write to Flash memory")),
            Location::Ram(i) => {
                let r = write_value(value, &mut self.data, i, size);
                self.print_mem_area(address);
                r
            }
            Location::Peripheral(_) => self.peripherals.write(address, value, size),
        }
    }

    fn write_word(&mut self, address: u32, value: u32) -> Result<(), String> {
        return self.write_mem_u(address, 4, value);
    }
}

#[derive(Debug)]
struct Board {
    tick: u128,
    samples: u128,
    log: fs::File,
    instruction_cache: InstructionCache,
    cpu: CPU,
    memory: MemoryBus,
    register_formats: [RegFormat; 16],
    branch_map: HashMap<u32, String>,
}

/**
 * Basically a VM. Uses standard fetch-decode-execute on an intermediate bytecode format
 * better suited to detecting bad things like unpredictable instructions and jumping into
 * IT blocks, while also being more efficient to execute in software compared to the Thumb encoding
 */
impl Board {
    fn new() -> Board {
        return Board {
            tick: 0,
            samples: 0,
            log: fs::File::create("amplitudes.txt").unwrap(),
            cpu: CPU::new(),
            instruction_cache: InstructionCache::new(),
            memory: MemoryBus::new(),
            register_formats: [RegFormat::Hex; 16],
            branch_map: HashMap::new(),
        };
    }

    /**
     * Fetch: This stage
     * 1. Retrieves the address of the instruction to be executed
     * 2. Updates the PC value visible to instructions to be this + 4
     * 3. Attempts to find a cached instruction
     * 3a. If not cached, fetches direct bytes and decodes into the intermediate bytecode format
     * 3b. Caches decoded instruction
     * 4. Updates instruction pointed to by instruction PC to next instruction
     * 5. Returns fetched intermediate bytecode instruction & bool of width
     */
    fn fetch(&mut self) -> Result<(ByteInstruction, bool), String> {
        let pc = self.cpu.update_instruction_address();
        // println!("Fetching from {:#010X}", pc);
        let mut instruction = self.instruction_cache.get_cached(pc)?;
        let mut start = tag::from(instruction);
        if !tag::has_cached(start) {
            println!("No cache - translating");
            let raw = self.memory.get_instr_word(pc)?;
            let decoded = decode_thumb(raw, InstructionContext::new(pc, ItPos::None));
            instruction = decoded.0;
            start = tag::from(instruction);
            if decoded.1 {
                self.instruction_cache.write_cache_wide(pc, instruction);
            } else {
                self.instruction_cache.write_cache_narrow(pc, instruction);
            }
        }
        let wide = tag::is_wide(start);
        self.cpu.inc_pc(wide);
        return Ok((instruction, wide));
    }

    /**
     * Decode: Retrieves and returns the opcode from the instruction. Additionally
     * may raise an exception if the instruction is unpredictable, and could return
     * a modified version that is safe (consistent with the real board) to execute
     * if unpredictable behaviour is enabled.
     *
     * TODO
     */
    fn decode(&self, instruction: ByteInstruction) -> Opcode {
        return tag::get_opcode(tag::from(instruction));
    }

    /**
     * Execute: Takes the instruction and opcode, and executes
     * the instruction based on the opcode. It assumes
     */
    fn execute(&mut self, instr: ByteInstruction, wide: bool) -> Result<(), String> {
        self.tick += 1;
        let opcode = tag::get_opcode(instr.0);
        let data = instr.0 & 0xFFFF;
        let extra = instr.1 & !(0b11 << 30);
        return if wide {
            self.execute_wide(opcode, data, extra)
        } else {
            self.execute_narrow(opcode, data)
        }
    }

    fn execute_wide(&mut self, opcode: Opcode, data: u32, extra: u32) -> Result<(), String> {
        match opcode {
            Opcode::AddImm => self.w_add_imm(data, extra),
            Opcode::Bl     => self.w_bl(data, extra),
            Opcode::CmpImm => self.w_cmp_imm(data, extra),
            Opcode::Ldm    => self.w_ldm(data, extra),
            Opcode::LdrImm => self.w_ldr_imm(data, extra),
            Opcode::LdrLit => self.w_ldr_lit(data, extra),
            Opcode::LslImm => self.w_lsl_imm(data, extra),
            Opcode::MovImm => self.w_mov_imm(data, extra),
            Opcode::Mul    => self.w_mul(data, extra),
            Opcode::RsbImm => self.w_rsb_imm(data, extra),
            Opcode::Stm    => self.w_stm(data, extra),
            Opcode::StrImm => self.w_str_imm(data, extra),
            Opcode::SubImm => self.w_sub_imm(data, extra),
            Opcode::SubReg => self.w_sub_reg(data, extra),
            Opcode::Udiv   => self.w_udiv(data, extra),
            _ => {
                println!("Unimplemented wide instruction {:?} : {:#06X} + {:#010X}", opcode, data, extra);
            }
        }
        return Ok(());
    }

    fn execute_narrow(&mut self, opcode: Opcode, data: u32) -> Result<(), String> {
        match opcode {
            Opcode::AdcImm => self.n_adc_imm(data),
            Opcode::AdcReg => self.n_adc_reg(data),
            Opcode::AddImm => self.n_add_imm(data),
            Opcode::AddReg => self.n_add_reg(data),
            Opcode::AndReg => self.n_and_reg(data),
            Opcode::Branch => self.n_branch(data),
            Opcode::BranchCond => self.n_branch_cond(data),
            Opcode::Bx     => self.n_bx(data),
            Opcode::CmpImm => self.n_cmp_imm(data),
            Opcode::CmpReg => self.n_cmp_reg(data),
            Opcode::Cps    => self.n_cps(data),
            Opcode::Ldm    => self.n_ldm(data),
            Opcode::LdrLit => self.n_ldr_lit(data),
            Opcode::LdrImm => self.n_ldr_imm(data),
            Opcode::LdrReg => self.n_ldr_reg(data),
            Opcode::MovImm => self.n_mov_imm(data),
            Opcode::MovReg => self.n_mov_reg(data),
            Opcode::Pop    => self.n_pop(data),
            Opcode::Push   => self.n_push(data),
            Opcode::Stm    => self.n_stm(data),
            Opcode::StrImm => self.n_str_imm(data),
            Opcode::StrReg => self.n_str_reg(data),
            _ => {
                println!("Unimplemented narrow instruction {:?} - {:#06X}", opcode, data);
            }
        }

        return Ok(());
    }

    /**
     * Takes a path to an ELF file and initialises the board with its contents
     */
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
                "lcd_write_char" |
                "lcd_write_string" |
                "lcd_update_display" |
                "BSP_AUDIO_OUT_Play_Sample" => {
                    self.branch_map.insert((sym.st_value as u32) & !0b1, name.to_string());
                }
                _ => {}
            }
        }

        self.bx_write_pc(elf.entry as u32);
        self.cpu.write_reg(15, elf.entry as u32);

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
        self.memory.print_mem_area(address);
    }

    fn read_reg<T: Into<u32>>(&self, reg: T) -> u32 {
        let reg = reg.into();
        return self.cpu.read_reg(reg);
    }

    fn write_reg<T: Into<u32>>(&mut self, reg: T, val: u32) {
        // TODO: Follow B1.4.7 p521
        let reg = reg.into();
        self.cpu.write_reg(reg, val);
    }

    fn get_register_display_value(&self, reg: u8) -> String {
        assert!(reg <= 15);
        let val = match reg {
            15 => self.cpu.read_instruction_pc(),
            _ => self.read_reg(reg),
        };
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
        return self.cpu.read_sp();
    }

    fn write_sp(&mut self, value: u32) {
        self.cpu.write_sp(value);
    }

    fn read_lr(&self) -> u32 {
        return self.cpu.read_lr();
    }

    fn write_lr(&mut self, value: u32) {
        self.cpu.write_lr(value);
    }

    fn read_pc(&self) -> u32 {
        return self.cpu.read_pc();
    }

    fn set_pc(&mut self, address: u32) {
        self.cpu.write_instruction_pc(address);
    }

    fn inc_pc(&mut self, wide: bool) {
        self.cpu.inc_pc(wide);
    }

    fn get_shifted_register(&self, reg: u32, shift_t: u32, shift_n: u32) -> u32 {
        return shift(self.read_reg(reg), shift_t, shift_n, self.cpu.read_carry_flag() as u32);
    }

    fn get_shift_with_carry(&self, reg: u32, shift_t: u32, shift_n: u32) -> (u32, bool) {
        return shift_c(self.read_reg(reg), shift_t, shift_n, self.cpu.read_carry_flag() as u32);
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

    fn set_flags_nz_alt_c(&mut self, result: u32, spill_tag: u32) {
        self.set_flags_nz(result);
        match (spill_tag >> 2) & 0b11 {
            0b00 => {}
            0b01 => {}
            0b10 => self.cpu.set_carry_flag(false),
            0b11 => self.cpu.set_carry_flag(true),
            _ => unreachable!(),
        }
        // v unchanged
    }

    fn shift_imm(&mut self, rd: u8, rm: u8, shift: Shift, setflags: bool) {
        // Handles LSL, LSR, ASR, etc., as their encodings (T2 at least) are all very similar
        // let (result, carry) = self.get_shift_with_carry(rm, shift);
        // self.write_reg(rd, result);
        // if setflags {
        //     self.set_flags_nzc(result, carry);
        // }
    }

    fn in_it_block(&self) -> bool {
        return false;
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
        if self.cpu.current_mode == ExecMode::ModeHandler && (address >> 28) == 0xF {
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

    fn n_adc_imm(&mut self, data: u32) {
        // A7.7.1
        println!("TODO: ADC (imm) narrow");
    }

    fn n_adc_reg(&mut self, data: u32) {
        // A7.7.2
        let rd = data & 0b111;
        let rm = data >> 3;
        let (result, carry, overflow) = add_with_carry(self.read_reg(rd), self.read_reg(rm), self.cpu.carry());
        self.write_reg(rd, result);
        if !self.in_it_block() {
            self.set_flags_nzcv(result, carry, overflow);
        }
    }

    fn n_add_imm(&mut self, data: u32) {
        // A7.7.3
        let imm32 = data & 0xFF;
        let rn = data >> 11;
        let rd = (data >> 8) & 0b111;
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), imm32, 0);
        self.write_reg(rd, result);
        if !self.in_it_block() {
            self.set_flags_nzcv(result, carry, overflow);
        }
    }

    fn w_add_imm(&mut self, data: u32, extra: u32) {
        // A7.7.3
        let imm32 = data << 30 | extra;
        let rd = (data >> 4) & 0xF;
        let rn = (data >> 8) & 0xF;
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), imm32, 0);
        self.write_reg(rd, result);
        if bitset(data, 12) {
            self.set_flags_nzcv(result, carry, overflow);
        }
    }

    fn n_add_reg(&mut self, data: u32) {
        // A7.7.4
        let rd = data & 0xF;
        let rm = (data >> 4) & 0xF;
        let rn = (data >> 8) & 0xF;
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), self.read_reg(rm), 0);
        if rd == 15 {
            self.alu_write_pc(result);
        } else {
            self.write_reg(rd, result);
            if (data >> 12) > 0 {
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
        // let shifted = self.get_shifted_register(rm, shift);
        // let (result, carry, overflow) = add_with_carry(self.read_sp(), shifted, 0);
        // if rd == 15 {
        //     self.alu_write_pc(result);
        // } else {
        //     self.write_reg(rd, result);
        //     if setflags {
        //         self.set_flags_nzcv(result, carry, overflow);
        //     }
        // }
    }

    fn adr(&mut self, rd: u8, address: u32) {
        // A7.7.7
        // NOTE: The offset calculation is determined by PC, so we precalculate it in the Instruction.
        self.write_reg(rd, address);
    }

    fn w_and_imm(&mut self, data: u32, extra: u32) {
        // A7.7.8
        println!("TODO: AND (imm) wide");
    }

    fn n_and_reg(&mut self, data: u32) {
        // A7.7.9
        let rd = data & 0b111;
        let rm = data >> 3;
        let result = self.read_reg(rd) & self.read_reg(rm);
        self.write_reg(rd, result);
        if !self.in_it_block() {
            self.set_flags_nz(result);
        }
    }

    fn asr_imm(&mut self, rd: u8, rm: u8, shift: Shift, setflags: bool) {
        // A7.7.10
        // let (result, carry) = self.get_shift_with_carry(rm, shift);
        // self.write_reg(rd, result);
        // if setflags {
        //     self.set_flags_nzc(result, carry);
        // }
    }

    fn asr_reg(&mut self, rd: u8, rm: u8, rn: u8, setflags: bool) {
        // A7.7.11
        // let shift_n = self.read_reg(rm) & 0xFF;
        // let (result, carry) =  self.get_shift_with_carry(rn, Shift {shift_t: ShiftType::ASR, shift_n});
        // self.write_reg(rd, result);
        // if setflags {
        //     self.set_flags_nzc(result, carry);
        // }
    }

    fn n_branch(&mut self, data: u32) {
        // A7.7.12
        self.branch_write_pc(self.read_pc().wrapping_add(shifted_sign_extend(data, 10, 1)));
    }

    fn n_branch_cond(&mut self, data: u32) {
        // A7.7.12
        if self.cpu.check_condition(Condition::new(data >> 8)) {
            // println!("Condition passed");
            self.branch_write_pc(self.read_pc().wrapping_add(shifted_sign_extend(data, 7, 1)));
        } else {
            // println!("Condition failed");
        }
    }

    fn bfc(&mut self, rd: u8, mask: u32) {
        // A7.7.13
        // NOTE: We precalculate the mask from the msbit and lsbit values
        self.write_reg(rd, self.read_reg(rd) & mask);
    }

    fn bfi(&mut self, rd: u8, mask: u32) {
        // A7.7.14
        // NOTE: We precalculate the mask from the msbit and lsbit values.
        self.write_reg(rd, self.read_reg(rd) | mask);
    }

    fn bic_imm(&mut self, rd: u8, rn: u8, imm32: u32, setflags: bool, carry: CarryChange) {
        // A7.7.15
    }

    fn bic_reg(&mut self, rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool) {
        // A7.7.16
        // let (shifted, carry) = self.get_shift_with_carry(rm, shift);
        // let result = self.read_reg(rn) & !shifted;
        // self.write_reg(rd, result);
        // if setflags {
        //     self.set_flags_nzc(result, carry);
        // }
    }

    fn bkpt(&mut self, _imm32: u32) {
        // A7.7.17
        // TODO: When return values supported, return a DebugMonitor exception with the input id
    }

    fn w_bl(&mut self, data: u32, extra: u32) {
        // A7.7.18
        let pc = self.read_pc();
        self.write_lr(pc | 0b1);
        let address = pc.wrapping_add(shifted_sign_extend(extra, 23, 1));
        match self.branch_map.get(&address) {
            Some(name) => {
                if name == "BSP_AUDIO_OUT_Play_Sample" {
                    // println!("({}) Audio out: {:#010X}", self.tick, self.read_reg(0));
                    // write!(self.log, "{} ", self.read_reg(0u32) as i16).unwrap();
                    self.samples += 1;
                } else {
                    println!("Skipping branch to {}", name);
                }
            }
            None => {
                self.branch_write_pc(address);
            }
        }
    }

    fn blx_reg(&mut self, rm: u8) {
        // A7.7.19
        let target = self.read_reg(rm);
        let next_instr_address = self.read_pc() + 2;
        self.write_lr(next_instr_address & 0b1);
        self.blx_write_pc(target);
    }

    fn n_bx(&mut self, data: u32) {
        // A7.7.20
        let rm = data;
        self.bx_write_pc(self.read_reg(rm));
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
        // let shifted = self.get_shifted_register(rm, shift);
        // let (result, carry, overflow) = self.get_add_with_no_carry(rn, shifted);
        // self.set_flags_nzcv(result, carry, overflow);
    }

    fn n_cmp_imm(&mut self, data: u32) {
        // A7.7.27
        let rn = data >> 8;
        let imm32 = data & 0xFF;
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), !imm32, 1);
        self.set_flags_nzcv(result, carry, overflow);
    }

    fn w_cmp_imm(&mut self, data: u32, extra: u32) {
        let imm32 = data << 30 | extra;
        let rn = data >> 4;
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), !imm32, 1);
        self.set_flags_nzcv(result, carry, overflow);
    }

    fn n_cmp_reg(&mut self, data: u32) {
        // A7.7.28
        let rn = data & 0xF;
        let rm = data >> 4;
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), !self.read_reg(rm), 1);
        self.set_flags_nzcv(result, carry, overflow);
    }

    fn n_cps(&mut self, data: u32) {
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
    }

    fn eor_reg(&mut self, rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool) {
        // A7.7.36
        // let (shifted, carry) = self.get_shift_with_carry(rm, shift);
        // let result = self.read_reg(rn) ^ shifted;
        // self.write_reg(rd, result);
        // if setflags {
        //     self.set_flags_nzc(result, carry);
        // }
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

    fn n_ldm(&mut self, data: u32) {
        let rn = (data >> 8) & 0x7;
        let registers = data;
        let mut address = self.read_reg(rn);
        for i in 0..=7u32 {
            if bitset(registers, i) {
                self.write_reg(i, self.memory.read_word(address).unwrap());
                address += 4;
            }
        }
        if bitset(data, 11) {
            self.write_reg(rn, address);
        }
    }

    fn w_ldm(&mut self, data: u32, extra: u32) {
        // A7.7.41
        let rn = data;
        let registers = extra;
        let wback = bitset(extra, 16);
        let mut address = self.read_reg(rn);
        for i in 0..=14u32 { // TODO: Skip stack pointer
            if bitset(registers, i) {
                self.write_reg(i, self.memory.read_word(address).unwrap());
                address += 4;
            }
        }
        if bitset(registers, 15) {
            self.load_write_pc(self.memory.read_word(address).unwrap());
        }
        if wback && !bitset(registers, rn) {
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

    fn n_ldr_imm(&mut self, data: u32) {
        let rt = (data >> 8) & 0xF;
        let rn = data >> 12;
        let imm32 = (data & 0xFF) << 2;
        let address = self.read_reg(rn).wrapping_add(imm32);
        self.write_reg(rt, self.memory.read_word(address).unwrap());
    }

    fn w_ldr_imm(&mut self, data: u32, extra: u32) {
        // A7.7.43
        let rt = data & 0xF;
        let rn = data >> 4;
        let index = bitset(extra, 14);
        let wback = bitset(extra, 13);

        let offset_address = self.read_reg(rn).wrapping_add(sign_extend(extra, 12));
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

    fn n_ldr_lit(&mut self, data: u32) {
        let rt = data >> 10;
        let imm10 = data & 0x3FF;
        let address = word_align(self.read_pc()).wrapping_add(imm10);
        let value = self.memory.read_word(address).unwrap();
        if rt == 15 {
            if (address & 0b11) == 0 {
                self.load_write_pc(value);
            } else {
                panic!("Unpredictable");
            }
        } else {
            self.write_reg(rt, value);
        }
    }

    fn w_ldr_lit(&mut self, data: u32, extra: u32) {
        // A7.7.44
        let rt = data;
        let address = word_align(self.read_pc()).wrapping_add(sign_extend(extra, 12));
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

    fn n_ldr_reg(&mut self, data: u32) {
        // A7.7.45
        let rt = data & 0b111;
        let rn = (data >> 3) & 0b111;
        let rm = data >> 6;
        let address = self.read_reg(rn).wrapping_add(self.read_reg(rm));
        let value = self.memory.read_word(address).unwrap();
        self.write_reg(rt, value);
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
        // let offset = self.get_shifted_register(rm, Shift {shift_t: ShiftType::LSL, shift_n});
        // let address = self.read_reg(rn).wrapping_add(offset);
        // self.write_reg(rt, self.memory.read_byte(address).unwrap() as u32);
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
        // let offset = self.get_shifted_register(rm, Shift {shift_t: ShiftType::LSL, shift_n});
        // let address = self.read_reg(rn).wrapping_add(offset);
        // let data = self.memory.read_mem_u(address, 2).unwrap();
        // self.write_reg(rt, data);
    }

    fn w_lsl_imm(&mut self, data: u32, extra: u32) {
        // A7.7.68
        let rd = data & 0xF;
        let rm = (data >> 4) & 0xF;
        let setflags = bitset(data, 8);
        let shift = extra;
        let input = self.read_reg(rm);
        let result = input << shift;
        let carry_out = bitset(input, 32 - shift);
        self.write_reg(rd, result);
        if setflags {
            self.set_flags_nzc(result, carry_out);
        }
    }

    fn lsl_reg(&mut self, rd: u8, rn: u8, rm: u8, setflags: bool) {
        // A7.7.69
        // let shift_n = self.read_reg(rm) & 0xF;
        // let (result, carry) = self.get_shift_with_carry(rn, Shift {shift_t: ShiftType::LSL, shift_n});
        // self.write_reg(rd, result);
        // if setflags {
        //     self.set_flags_nzc(result, carry);
        // }
    }

    fn n_mov_imm(&mut self, data: u32) {
        // A7.7.76
        let rd = data >> 8;
        let imm8 = data & 0xFF;
        self.write_reg(rd, imm8);
        if !self.in_it_block() {
            self.set_flags_nz(imm8);
        }
    }

    fn w_mov_imm(&mut self, data: u32, extra: u32) {
        // A7.7.76
        let imm32 = data << 30 | extra;
        let rd = (data >> 4) & 0xF;
        self.write_reg(rd, imm32);
        if bitset(data, 8) {
            self.set_flags_nz_alt_c(imm32, data);
        }
    }

    fn n_mov_reg(&mut self, data: u32) {
        // A7.7.77
        let rd = data & 0xF;
        let rm = (data >> 4) & 0xF;
        let result = self.read_reg(rm);
        if rd == 15 {
            self.alu_write_pc(result);
        } else {
            self.write_reg(rd, result);
            if bitset(data, 8) {
                self.set_flags_nz(result);
            }
        }
    }

    fn w_mul(&mut self, data: u32, extra: u32) {
        // A7.7.84
        let rd = data & 0xF;
        let rn = data >> 4;
        let rm = extra;
        let op1 = self.read_reg(rn);
        let op2 = self.read_reg(rm);
        let result = op1.wrapping_mul(op2);
        self.write_reg(rd, result);
    }

    fn orr_imm(&mut self, rd: u8, rn: u8, imm32: u32, setflags: bool, carry: CarryChange) {

    }

    fn n_pop(&mut self, data: u32) {
        // println!("Popping {:0b}", data);
        let mut address = self.read_sp();
        for i in (0..8u32).rev() {
            if bitset(data, i) {
                self.print_mem_area(address);
                self.write_reg(i, self.memory.read_mem_a(address, 4).unwrap());
                address += 4;
            }
        }
        if bitset(data, 8) {
            self.print_mem_area(address);
            self.load_write_pc(self.memory.read_mem_a(address, 4).unwrap());
            address += 4;
        }
        self.write_sp(address);
    }

    fn n_push(&mut self, data: u32) {
        // A7.7.99
        let mut address = self.read_sp();
        if bitset(data, 8) {
            address -= 4;
            self.memory.write_word(address, self.read_lr()).unwrap();
        }
        for i in (0..8u32).rev() {
            if bitset(data, i) {
                address -= 4;
                self.memory.write_word(address, self.read_reg(i)).unwrap();
            }
        }
        self.write_sp(address);
    }

    fn w_rsb_imm(&mut self, data: u32, extra: u32) {
        let imm32 = data << 30 | extra;
        let rd = (data >> 4) & 0xF;
        let rn = (data >> 8) & 0xF;
        let (result, carry, overflow) = add_with_carry(!self.read_reg(rn), imm32, 1);
        self.write_reg(rd, result);
        if bitset(data, 12) {
            self.set_flags_nzcv(result, carry, overflow);
        }
    }

    fn n_stm(&mut self, data: u32) {
        // A7.7.159
        let rn = data >> 8;
        let registers = data;
        let mut address = self.read_reg(rn);
        for i in 0..=7u32 {
            if bitset(registers, i) {
                self.memory.write_word(address, self.read_reg(i)).unwrap();
                address += 4;
            }
        }
        self.write_reg(rn, address);
    }

    fn w_stm(&mut self, data: u32, extra: u32) {
        // A7.7.159
        let rn = data;
        let registers = extra;
        let mut address = self.read_reg(rn);
        for i in 0..=14u32 {
            if bitset(registers, i) {
                self.memory.write_word(address, self.read_reg(i)).unwrap();
                address += 4;
            }
        }
        if bitset(extra, 16) {
            self.write_reg(rn, address);
        }
    }

    fn n_str_imm(&mut self, data: u32) {
        let imm32 = (data & 0xFF) << 2;
        let rt = (data >> 8) & 0xF;
        let rn = data >> 12;
        let address = self.read_reg(rn).wrapping_add(imm32);
        self.memory.write_word(address, self.read_reg(rt)).unwrap();
    }

    fn w_str_imm(&mut self, data: u32, extra: u32) {
        // A7.7.158
        let rt = data & 0xF;
        let rn = data >> 4;
        let rn_val = self.read_reg(rn);
        let offset_address = rn_val.wrapping_add(sign_extend(extra, 12));
        let index = bitset(extra, 14);
        let wback = bitset(extra, 13);
        let address = if index { offset_address } else { rn_val };
        self.memory.write_word(address, self.read_reg(rt)).unwrap();
        if wback {
            self.write_reg(rn, offset_address);
        }
    }

    fn n_str_reg(&mut self, data: u32) {
        // A7.7.159
        let rt = data & 0b111;
        let rn = (data >> 3) & 0b111;
        let rm = data >> 6;
        let address = self.read_reg(rn).wrapping_add(self.read_reg(rm));
        self.memory.write_mem_u(address, 4, self.read_reg(rt)).unwrap();
    }

    fn w_sub_imm(&mut self, data: u32, extra: u32) {
        // A7.7.174
        let imm32 = data << 30 | extra;
        let rd = (data >> 4) & 0xF;
        let rn = (data >> 8) & 0xF;
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), !imm32, 1);
        self.write_reg(rd, result);
        if bitset(data, 12) {
            self.set_flags_nzcv(result, carry, overflow);
        }
    }

    fn w_sub_reg(&mut self, data: u32, extra: u32) {
        // A7.7.172
        let rd = data & 0xF;
        let rn = (data >> 4) & 0xF;
        let rm = (data >> 8) & 0xF;
        let setflags = bitset(data, 12);

        let shift_t = extra >> 6;
        let shift_n = extra & 0x3F;

        let shifted = self.get_shifted_register(rm, shift_t, shift_n);
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), !shifted, 1);
        self.write_reg(rd, result);
        if setflags {
            self.set_flags_nzcv(result, carry, overflow);
        }
    }

    fn w_udiv(&mut self, data: u32, extra: u32) {
        let rd = data & 0xF;
        let rn = data >> 4;
        let rm = extra;
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
    println!("Welcome to ARM emulator");

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
    loop {
        // cont = cont && board.samples < 48000 * 20;
        cont = cont && board.cpu.read_instruction_pc() != 0x08019756;

        if !cont {
            println!("played {} samples", board.samples);
            return;
            println!("\n{}\n", board);
            print!("press enter to continue");
            io::stdout().flush().unwrap();
            let mut input = String::new();
            io::stdin().read_line(&mut input).unwrap();
            print!("\n\n");
        }

        match board.fetch() {
            Ok((i, w)) => {
                // println!("fetched {:?} ({})", tag::get_opcode(i.0), if w { "wide" } else { "narrow" });
                match board.execute(i, w) {
                    _ => {}
                }
            }
            Err(e) => {
                println!("Err: {}", e);
                return;
            }
        };
    }
}
