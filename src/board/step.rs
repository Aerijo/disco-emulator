impl Board {
    fn step(&mut self) -> Result<(), String> {
        match self.fetch() {
            Ok((i, w)) => {
                // println!("fetched {:?} ({})", tag::get_opcode(i.0), if w { "wide" } else { "narrow" });
                self.execute(i, w);
                return Ok(());
            }
            Err(e) => {
                return Err(e);
            }
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
        let mut instruction = self.instruction_cache.get_cached(pc)?;
        let mut start = tag::from(instruction);
        if !tag::has_cached(start) {
            let raw = self.flash.read_instruction(pc)?;
            let decoded = decode_thumb(raw, InstructionContext::new(pc, self.itstate.position()));
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
    fn execute(&mut self, instr: ByteInstruction, wide: bool) {
        self.tick += 1;
        let opcode = tag::get_opcode(instr.0);
        let data = instr.0 & 0xFFFF;
        let extra = instr.1 & !(0b11 << 30);

        if self.itstate.active() {
            let execute = self.cpu.check_condition(self.itstate.condition());
            self.itstate.advance();
            if !execute {
                println!("IT condition failed");
                return;
            }
        }

        if wide {
            self.execute_wide(opcode, data, extra)
        } else {
            self.execute_narrow(opcode, data)
        }
    }

    fn execute_wide(&mut self, opcode: Opcode, data: u32, extra: u32) {
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
                // unsafe { unreachable_unchecked() }
                println!("Unimplemented wide instruction {:?} : {:#06X} + {:#010X}", opcode, data, extra);
            }
        }
    }

    fn execute_narrow(&mut self, opcode: Opcode, data: u32) {
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
            Opcode::It     => self.n_it(data),
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
            Opcode::SubImm => self.n_sub_imm(data),
            _ => {
                // unsafe { unreachable_unchecked() }
                println!("Unimplemented narrow instruction {:?} - {:#06X}", opcode, data);
            }
        }
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

    fn in_it_block(&self) -> bool {
        return self.itstate.active();
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
                if name == "BSP_AUDIO_OUT_Play_Sample" || name == "audio_play_sample" {
                    self.audio_handler.handle((self.read_reg(0u32) & 0xFFFF) as i16);
                } else {
                    println!("Skipping branch to {}", name);
                }
            }
            None => {
                self.branch_write_pc(address);
            }
        }
    }

    fn n_blx_reg(&mut self, data: u32) {
        // A7.7.19
        let rm = data;
        let target = self.read_reg(rm);
        let next_instr_address = self.read_pc() + 2;
        self.write_lr(next_instr_address | 0b1);
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

    fn n_it(&mut self, data: u32) {
        // A7.7.38
        self.itstate.state = data;
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

    fn n_sub_imm(&mut self, data: u32) {
        let imm32 = data & 0xFF;
        let rd = (data >> 8) & 0x7;
        let rn = data >> 11;
        let (result, carry, overflow) = add_with_carry(self.read_reg(rn), !imm32, 1);
        self.write_reg(rd, result);
        if !self.in_it_block() {
            self.set_flags_nzcv(result, carry, overflow);
        }
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
