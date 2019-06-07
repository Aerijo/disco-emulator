#[derive(Copy, Clone, Debug)]
pub enum RegFormat {
    Bin, // binary
    Oct, // octal
    Dec, // unsigned decimal
    Sig, // signed decimal
    Hex, // hexadecimal
}


#[derive(Debug)]
pub enum Instruction {
    MovImm { to: usize, val: u32, flags: bool },
    MovReg { to: usize, from: usize, flags: bool },
    AddImm { dest: usize, first: usize, val: u32, flags: bool },
    AddReg { dest: usize, first: usize, second: usize, flags: bool },
    SubImm { dest: usize, first: usize, val: u32, flags: bool },
    SubReg { dest: usize, first: usize, second: usize, flags: bool },
    LdrReg { dest: usize, address: usize, flags: bool },
    StrReg { val: usize, address: usize },

    Undefined,
    Undo { num: usize },
    Redo { num: usize },
    Format { reg: usize, kind: RegFormat },
    Memview { index: usize, length: usize },
}


impl Instruction {
    pub fn from (word: u32) -> (Instruction, bool) {
        let length_check = word >> 29;
        return if length_check == 0b111 && (word >> 27 != 0b11100) {
            (Instruction::get_wide_instruction(word), true)
        } else {
            (Instruction::get_narrow_instruction((word >> 16) as u16), false)
        }
    }

    fn get_wide_instruction (word: u32) -> Instruction {
        let op1 = (word >> 27) & 0b11;
        let op2 = (word >> 20) & 0b1111111;
        let op3 = (word >> 15) & 0b1;

        return match op1 {
            0b01 => {
                if (op2 & 0b1100100) == 0b0000000 { return id_ldr_str_multiple(word); }
                if (op2 & 0b1100100) == 0b0000100 { return id_ldr_str_dual(word); }
                if (op2 & 0b1100000) == 0b0100000 { return id_data_processing_shifted(word); }
                if (op2 & 0b1000000) == 0b1000000 { return id_coprocessor_instr(word); }
                return Instruction::Undefined
            }
            0b10 => {
                if op3 > 0 { return id_branch_and_misc(word); }
                if ((op2 >> 5) & 0b1) == 0 {
                    id_data_proc_modified_immediate(word)
                } else {
                    id_data_proc_plain_binary_immediate(word)
                }
            }
            0b11 => {
                Instruction::Undefined
            }
            _ => Instruction::Undefined // 0b00 would be a narrow instruction
        }
    }

    fn get_narrow_instruction (_hword: u16) -> Instruction {
        return Instruction::Undo { num: 1 };
    }
}



fn id_ldr_str_multiple (word: u32) -> Instruction {
    return Instruction::Undefined;
}


fn id_ldr_str_dual (word: u32) -> Instruction {
    return Instruction::Undefined;
}


fn id_data_processing_shifted (word: u32) -> Instruction {
    return Instruction::Undefined;
}


fn id_coprocessor_instr (word: u32) -> Instruction {
    return Instruction::Undefined;
}


fn id_branch_and_misc (word: u32) -> Instruction {
    return Instruction::Undefined;
}


fn id_data_proc_modified_immediate (word: u32) -> Instruction {
    return Instruction::Undefined;
}


fn id_data_proc_plain_binary_immediate (word: u32) -> Instruction {
    return Instruction::Undefined;
}
