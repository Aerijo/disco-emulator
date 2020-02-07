use std::path::{PathBuf, Path};
use std::collections::HashMap;

mod flash;
use flash::{Flash};

mod data;
use data::{Data};

mod scs;
use scs::{Scs};

use crate::cpu::{CoreRegisters};

// NOTE: condition checking is defined in A7.3.1 p178
#[derive(Copy, Clone, Debug)]
pub enum Condition {
    Equal = 0b0000,
    NotEqual = 0b0001,
    CarrySet = 0b0010,
    CarryClear = 0b0011,
    Negative = 0b0100,
    PosOrZero = 0b0101,
    Overflow = 0b0110,
    NotOverflow = 0b0111,
    UHigher = 0b1000,
    ULowerSame = 0b1001,
    SHigherSame = 0b1010,
    Slower = 0b1011,
    SHigher = 0b1100,
    SLowerSame = 0b1101,
    Always = 0b1110,
    Never = 0b1111,
}

impl Condition {
    pub fn new<T: Into<u32>>(code: T) -> Condition {
        let code = code.into();
        assert!(code <= 0xF);
        return match code {
            0b0000 => Condition::Equal,
            0b0001 => Condition::NotEqual,
            0b0010 => Condition::CarrySet,
            0b0011 => Condition::CarryClear,
            0b0100 => Condition::Negative,
            0b0101 => Condition::PosOrZero,
            0b0110 => Condition::Overflow,
            0b0111 => Condition::NotOverflow,
            0b1000 => Condition::UHigher,
            0b1001 => Condition::ULowerSame,
            0b1010 => Condition::SHigherSame,
            0b1011 => Condition::Slower,
            0b1100 => Condition::SHigher,
            0b1101 => Condition::SLowerSame,
            0b1110 => Condition::Always,
            0b1111 => Condition::Never,
            _ => panic!(),
        };
    }
}

#[derive(Debug)]
pub struct ItState {
    state: u32,
}

impl ItState {
    fn new() -> ItState {
        return ItState {
            state: 0,
        };
    }

    fn active(&self) -> bool {
        return self.state != 0;
    }

    fn advance(&mut self) {
        if (self.state & 0b111) == 0 {
            self.state = 0;
        } else {
            self.state = self.state & (0b111 << 5) | (self.state & 0xF) << 1;
        }
    }

    fn condition(&self) -> Condition {
        return Condition::new(self.state >> 4);
    }

    fn position(&self) -> ItPos {
        return if self.state == 0 {
            ItPos::None
        } else if (self.state & 0b111) == 0 {
            ItPos::Last
        } else {
            ItPos::Within
        }
    }

    fn num_remaining(&self) -> u32 {
        for i in 0..=3 {
            if bitset(self.state, i) {
                return 4 - i;
            }
        }
        return 0;
    }
}

#[derive(Debug)]
pub struct Shift {
    shift_t: ShiftType,
    shift_n: u32,
}

impl fmt::Display for ItState {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return match self.position() {
            ItPos::None => {
                write!(f, "IT: None")
            }
            ItPos::Within => {
                write!(f, "IT: Within ({} remaining), Cond: {:?}", self.num_remaining(), self.condition())
            }
            ItPos::Last => {
                write!(f, "IT: Last, Cond: {:?}", self.condition())
            }
        }
    }
}

#[derive(Debug)]
pub struct Board {
    tick: u128,
    audio_handler: AudioHandler,
    instruction_cache: InstructionCache,
    core_registers: CoreRegisters,
    branch_map: HashMap<u32, String>,
    itstate: ItState,

    flash: Flash,
    sram: Data,
    scs: Scs,
}

impl Board {
    fn new() -> Board {
        return Board {
            tick: 0,
            audio_handler: AudioHandler::new(),
            core_registers: CoreRegisters::new(),
            instruction_cache: InstructionCache::new(),
            branch_map: HashMap::new(),
            itstate: ItState::new(),

            flash: Flash::new(),
            sram: Data::new(),
            scs: Scs::new(),
        };
    }

    /**
     * Takes a path to an ELF file and initialises the board with its contents
     */
    fn load_elf_from_path(&mut self, path: &Path) -> Result<(), String> {
        let bytes = match fs::read(path) {
            Ok(b) => b,
            Err(e) => {
                return Err(format!("Failed to read file \"{:?}\": {}", path, e));
            }
        };

        let elf = match Elf::parse(&bytes) {
            Ok(e) => e,
            Err(e) => {
                return Err(format!("Failed to parse elf file \"{:?}\": {}", path, e));
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
                "audio_init" |
                "audio_play_sample" |
                "init_joystick" |
                "joystick_init_all" |
                "joystick_enable_interrupts_all" |
                "lcd_init" |
                "lcd_write_char" |
                "lcd_write_string" |
                "lcd_update_display" |
                "maximise_clock_speed" |
                "BSP_AUDIO_OUT_Play_Sample" => {
                    self.branch_map.insert((sym.st_value as u32) & !0b1, name.to_string());
                }
                _ => {}
            }
        }

        self.bx_write_pc(elf.entry as u32);
        self.update_instruction_address();

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

    fn spawn_audio(&mut self) {
        self.audio_handler.spawn_audio();
    }
}
