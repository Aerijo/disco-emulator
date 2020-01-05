#[derive(Debug)]
struct Board {
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
