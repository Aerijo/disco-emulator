#![allow(dead_code)]
#![allow(unused_variables)]

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
    StrReg { val: usize, address: usize },

    Stm { rn: usize, registers: u16, wback: bool },
    Stmdb { rn: usize, registers: u16, wback: bool },
    Ldm { rn: usize, registers: u16, wback: bool },
    Ldmdb { rn: usize, registers: u16, wback: bool },
    Pop { registers: u16 },
    Push { registers: u16 },

    LdrLit { rt: usize, offset: u16 /* 12 bits + sign bit max */, add: bool },
    LdrImm { rn: usize, rt: usize, offset: u16 /* 12 bits + sign bit max */, add: bool, index: bool, wback: bool },
    LdrReg { rn: usize, rt: usize, rm: usize, shift: u8 },
    Ldrt { rn: usize, rt: usize, offset: u16 },

    Undefined,
    Undo { num: usize },
    Redo { num: usize },
    Format { reg: usize, kind: RegFormat },
    Memview { index: usize, length: usize },
}


impl Instruction {
    pub fn from (word: u32) -> (Instruction, bool) {
        return if (word >> 29 == 0b111) && (word >> 27 != 0b11100) {
            (get_wide_instruction(word), true)
        } else {
            (get_narrow_instruction((word >> 16) as u16), false)
        }
    }

}


fn get_narrow_instruction (_hword: u16) -> Instruction {
    return Instruction::Undo { num: 1 };
}


fn get_wide_instruction (word: u32) -> Instruction {
    let op1 = (word >> 27) & 0b11;
    let op2 = (word >> 20) & 0b1111111;
    let op3 = (word >> 15) & 0b1;

    println!("{:02b}", op1);
    println!("{:07b}", op2);
    println!("{:b}", op3);

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
            return if ((op2 >> 5) & 0b1) == 0 {
                id_data_proc_modified_immediate(word)
            } else {
                id_data_proc_plain_binary_immediate(word)
            }
        }
        0b11 => {
            if (op2 & 0b1000000) > 0 { return id_coprocessor_instr(word); }
            if ((op2 >> 5) & 0b1) > 0 {
                return if ((op2 >> 4) & 0b1) > 0 {
                    if ((op2 >> 3) & 0b1) == 1 {
                        id_long_multiply_diff(word)
                    } else {
                        id_multiply_diff(word)
                    }
                } else {
                    id_data_proc_register(word)
                }
            }
            if (op2 & 0b1110001) == 0 { return id_store_single(word); }
            return match op2 & 0b111 {
                0b001 => return id_load_byte(word),
                0b011 => return id_load_half_word(word),
                0b101 => return id_load_word(word),
                _ => Instruction::Undefined,
            }
        }
        _ => Instruction::Undefined // 0b00 would be a narrow instruction
    }
}


fn id_ldr_str_multiple (word: u32) -> Instruction {
    let op = (word >> 23) & 0b11;
    let l = ((word >> 20) & 0b1) == 1;
    let wback = ((word >> 21) & 0b1) == 1;
    let rn = ((word >> 16) & 0b1111) as usize;
    let registers = word as u16;

    return match op {
        0b01 if !l => {
            Instruction::Stm { rn, registers, wback }
        }
        0b01 if l => {
            if wback && rn == 0b1101 {
                Instruction::Pop { registers }
            } else {
                Instruction::Ldm { rn, registers, wback }
            }
        }
        0b10 if l => {
            Instruction::Ldmdb { rn, registers, wback }
        }
        0b10 if !l => {
            if wback && rn == 0b1101 {
                Instruction::Push { registers }
            } else {
                Instruction::Stmdb { rn, registers, wback }
            }
        }
        _ => Instruction::Undefined,
    }
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


fn id_long_multiply_diff (word: u32) -> Instruction {
    return Instruction::Undefined;
}


fn id_multiply_diff (word: u32) -> Instruction {
    return Instruction::Undefined;
}


fn id_data_proc_register (word: u32) -> Instruction {
    return Instruction::Undefined;
}


fn id_store_single (word: u32) -> Instruction {
    return Instruction::Undefined;
}


fn id_load_byte (word: u32) -> Instruction {
    return Instruction::Undefined;
}


fn id_load_half_word (word: u32) -> Instruction {
    return Instruction::Undefined;
}


fn id_load_word (word: u32) -> Instruction {
    let op1 = (word >> 23) & 0b11;
    let op2 = (word >> 6) & 0b111111;
    let rn = ((word >> 16) & 0b1111) as usize;
    let rt = ((word >> 12) & 0b1111) as usize;
    let offset = (word & 0b1111_1111_1111) as u16;

    if (op1 & 0b10) > 0 { return Instruction::Undefined; }

    if rn == 0b1111 {
        return Instruction::LdrLit { rt, offset, add: (op1 & 0b1) > 0 };
    }

    if op1 == 0b01 {
        return Instruction::LdrImm { rn, rt, offset, add: true, index: true, wback: false };
    }

    if op2 == 0 {
        let rm = (word & 0b1111) as usize;
        let shift = ((word >> 4) & 0b11) as u8;
        return Instruction::LdrReg { rn, rt, rm, shift };
    }

    let op3 = op2 >> 2;
    let offset8 = (word & 0b1111_1111) as u16;

    if op3 == 0b1100 || (op3 & 0b1001) == 0b1001 {
        let index = (word & (1 << 10)) > 0;
        let add = (word & (1 << 9)) > 0;
        let wback = (word & (1 << 8)) > 0;
        return Instruction::LdrImm { rn, rt, offset: offset8, index, add, wback };
    }

    if op3 == 0b1110 {
        return Instruction::Ldrt { rn, rt, offset: offset8 };
    }

    return Instruction::Undefined;
}
