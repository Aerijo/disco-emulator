#![allow(dead_code)]
#![allow(unused_variables)]

use std::num::Wrapping;

#[derive(Copy, Clone, Debug)]
pub enum RegFormat {
    Bin, // binary
    Oct, // octal
    Dec, // unsigned decimal
    Sig, // signed decimal
    Hex, // hexadecimal
}


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
}


impl Condition {
    fn from (code: i32) -> Condition {
        assert!(code <= 0b1110);
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
            _ => panic!("Unexpected code"),
        }
    }
}


#[derive(Copy, Clone, Debug)]
pub enum CarryChange {
    Same,
    Set,
    Clear,
}


#[derive(Copy, Clone, Debug)]
pub enum ShiftType {
    LSL,
    LSR,
    ASR,
    ROR,
    RRX,
}


#[derive(Debug)]
pub enum Instruction {
    MovReg { to: u8, from: u8, flags: bool },
    AddImm { dest: u8, first: u8, val: u32, flags: bool },
    SubImm { dest: u8, first: u8, val: u32, flags: bool },
    SubReg { dest: u8, first: u8, second: u8, flags: bool },
    StrReg { val: u8, address: u8 },

    AndImm { rd: u8, rn: u8, val: u32, flags: bool, carry: CarryChange},

    Branch { address: u32 },
    LinkedBranch { address: u32 },
    BranchExchange { rm: u8 },
    CondBranch { address: u32, cond: Condition },
    AddReg { rd: u8, rm: u8, rn: u8, flags: bool },
    MovImm { rd: u8, val: u32, flags: bool, carry: CarryChange },
    Stm { rn: u8, registers: u16, wback: bool },
    Stmdb { rn: u8, registers: u16, wback: bool },
    Ldm { rn: u8, registers: u16, wback: bool },
    Ldmdb { rn: u8, registers: u16, wback: bool },
    Pop { registers: u16 },
    Push { registers: u16 },
    CmpReg { rm: u8, rn: u8 },
    AddSpImm { rd: u8, val: u32, flags: bool },

    LdrLit { rt: u8, address: u32 },
    LdrImm { rn: u8, rt: u8, offset: u16 /* 12 bits + sign bit max */, add: bool, index: bool, wback: bool },
    StrImm { rn: u8, rt: u8, offset: u16, add: bool, index: bool, wback: bool },
    LdrReg { rn: u8, rt: u8, rm: u8, shift: u8 },
    Ldrt { rn: u8, rt: u8, offset: u16 },

    TstReg { rn: u8, rm: u8, shift_type: ShiftType, shift: u16 },

    Undefined,
    Unpredictable,

    Unimplemented,
    Undo { num: usize },
    Redo { num: usize },
    Format { reg: usize, kind: RegFormat },
    Memview { index: usize, length: usize },
}


impl Instruction {
    pub fn from (word: u32, pc: u32) -> (Instruction, bool) {
        return if (word >> 29 == 0b111) && (word >> 27 != 0b11100) {
            (get_wide_instruction(word, pc), true)
        } else {
            (get_narrow_instruction((word >> 16) as u16, pc), false)
        }
    }

}


pub fn bitset (word: u32, bit: u32) -> bool {
    return (word & (1 << bit)) > 0;
}


pub fn bitset16 (word: u16, bit: u16) -> bool {
    return (word & (1 << bit)) > 0;
}


fn word_align (address: u32) -> u32 {
    return address & !0b11;
}


fn sign_extend (value: u32, from: u32) -> i32 {
    return if bitset(value, from) {
        (value | (!0 << from)) as i32
    } else {
        value as i32
    };
}


// The pseudocode definition takes the current carry flag state
// but does not use it in calculations. To make instructions stateless,
// we instead return a value representing what to do with the current
// carry flag when we execute it.
fn thumb_expand_imm_c (input: u32) -> (u32, CarryChange) { // p137
    assert!((input & !0xFFF) == 0); // only works on 12 bit numbers
    if (input >> 8) == 0 {
        return (input, CarryChange::Same); // 000X
    }

    if (input >> 10) == 0 {
        let base = input & 0xFF;
        if base == 0 {
            panic!("Unpredictable thumb imm expansion");
        }

        let val = match input >> 8 {
            0b01 => (base << 16) + base, // 0X0X
            0b10 => (base << 24) + (base << 8), // X0X0
            0b11 => (base << 24) + (base << 16) + (base << 8) + base, // XXXX
            _ => panic!("Unexpected pattern"),
        };

        return (val, CarryChange::Same);
    }

    let unrotated_value = (1 << 7) | (input & 0xFF);
    return rotate_right_32_c(unrotated_value, input >> 7);
}


fn rotate_right_32_c (input: u32, shift: u32) -> (u32, CarryChange) { // p27
    assert!(shift != 0);
    let m = shift % 32;
    let result = (input >> m) | (input << (32 - m));
    let carry_out = if bitset(result, 31) { CarryChange::Set } else { CarryChange::Clear };
    return (result, carry_out);
}


fn thumb_expand_imm (val: u32) -> u32 { // p137
    return thumb_expand_imm_c(val).0;
}


// NOTE: pc value is 4 bytes ahead of instruction start. Most instructions that use the PC
// assume this when calculating offsets (see page 124).
fn get_narrow_instruction (hword: u16, pc: u32) -> Instruction {
    let op1 = hword >> 14;

    return match op1 {
        0b00 => id_shift_add_sub_move_cmp(hword),
        0b01 => {
            if bitset16(hword, 13) || bitset16(hword, 12) {
                id_ldr_str_single(hword)
            } else if bitset16(hword, 11) {
                let rt = ((hword >> 8) & 0b111) as u8;

                // PC for offset value is based on page 124 of ARM manual.
                // When instruction is executed, the theoretical PC value is guaranteed
                // to be 4 bytes ahead of the instruction address. Ours is still pointing to
                // the current instruction, so we add 4 and then word align.
                // The PC value for LDR is then word aligned
                let address = word_align(pc) + ((hword as u32 & 0xFF) << 2);
                Instruction::LdrLit { rt, address }
            } else if bitset16(hword, 10) {
                id_special_data_branch(hword)
            } else {
                id_data_processing(hword)
            }
        }
        0b10 => {
            if (hword >> 12) == 0b1011 { return id_misc(hword); }
            if (hword >> 11) == 0b10101 {
                let val = ((hword & 0xFF) << 2) as u32;
                let rd = ((hword >> 8) & 0b111) as u8;
                return Instruction::AddSpImm { rd, val, flags: false }
            }
            Instruction::Undefined
        }
        0b11 => {
            if bitset16(hword, 13) {
                let offset = sign_extend(((hword & 0x7FF) << 1) as u32, 11);
                let address = ((pc as i32) + offset) as u32;
                Instruction::Branch { address }
            } else if bitset16(hword, 12) {
                id_conditional_branch_supc(hword, pc)
            } else {
                Instruction::Undefined
            }
        }
        _ => panic!("Unexpected pattern"),
    }
}


fn id_ldr_str_single (hword: u16) -> Instruction {
    let op_a = hword >> 12;
    assert!(op_a == 0b0101 || op_a == 0b0110 || op_a == 0b0111 || op_a == 0b1000 || op_a == 0b1001);
    let op_b = (hword >> 9) & 0b111;
    let op_c = bitset16(hword, 11);

    return match op_a {
        0b0101 => match op_b {
            0b100 => {
                Instruction::Unimplemented
            }
            _ => Instruction::Unimplemented
        }
        0b0110 if op_c => {
            let rn = ((hword >> 3) & 0b111) as u8;
            let rt = (hword & 0b111) as u8;
            let offset = ((hword >> 6) & 0b1_1111) << 2;
            Instruction::LdrImm { rn, rt, offset, add: true, index: true, wback: false }
        }
        _ => Instruction::Unimplemented
    }
}


fn id_misc (hword: u16) -> Instruction {
    assert!((hword & (0b1111 << 12)) == 0b1011 << 12);

    let op1 = (hword >> 9) & 0b111;
    return match op1 {
        0b010 => {
            let mut registers = hword & 0xFF;
            if bitset16(hword, 8) {
                registers += 1 << 14;
            }
            Instruction::Push { registers }
        }
        _ => Instruction::Unimplemented,
    }
}


fn id_conditional_branch_supc (hword: u16, pc: u32) -> Instruction {
    assert!((hword >> 12) == 0b1101);
    let op = (hword >> 8) & 0b1111;
    return match op {
        0b1110 => Instruction::Unimplemented,
        0b1111 => Instruction::Unimplemented, // SUP
        _ => {
            let offset = ((hword & 0x7F) << 1) as u32;
            let address = if bitset16(hword, 7) {
                word_align(pc) + offset
            } else {
                word_align(pc) - (!offset & 0x7F)
            };
            let cond = Condition::from(((hword >> 8) & 0b1111) as i32);
            Instruction::CondBranch { address, cond }
        }
    }
}


fn id_shift_add_sub_move_cmp (hword: u16) -> Instruction {
    let op1 = hword >> 12;
    match op1 {
        0b01 => if (hword & (1 << 11)) > 0 { id_add_sub(hword) } else { Instruction::Undefined },
        0b10 => {
            if (op1 & 0b100) > 0 {
                Instruction::Undefined
            } else {
                let rd = ((hword >> 8) & 0b111) as u8;
                let val = (hword & 0xFF) as u32;
                Instruction::MovImm { rd, val, flags: true, carry: CarryChange::Same } // TODO: No flags in IT block
            }
        }
        _ => Instruction::Undefined,
    }
}


fn id_add_sub (hword: u16) -> Instruction {
    let rm = ((hword >> 6) & 0b111) as u8;
    let rn = ((hword >> 3) & 0b111) as u8;
    let rd = (hword & 0b111) as u8;
    return match (hword >> 9) & 0b11 {
        0b00 => Instruction::AddReg { rd, rm, rn, flags: true },
        0b01 => Instruction::Unimplemented,
        0b10 => Instruction::Unimplemented,
        0b11 => Instruction::Unimplemented,
        _ => panic!("oops")
    }
}


fn id_data_processing (hword: u16) -> Instruction {
    assert!((hword >> 10) == 0b010000);
    let op = (hword >> 6) & 0b1111;
    return match op {
        0b1010 => {
            let rn = (hword & 0b111) as u8;
            let rm = ((hword >> 3) & 0b111) as u8;
            Instruction::CmpReg { rm, rn }
        }
        _ => Instruction::Unimplemented
    }
}


fn id_special_data_branch (hword: u16) -> Instruction {
    assert!((hword >> 10) == 0b010001);
    let op = (hword >> 7) & 0b111;
    let rm = ((hword >> 3) & 0b1111) as u8;
    let rn = (((hword >> 4) & (1 << 3)) + (hword & 0b111)) as u8;
    return match op {
        0b000 | 0b001 => {
            Instruction::AddReg { rd: rn, rm, rn, flags: false }
        }
        0b010 => {
            if bitset16(hword, 6) {
                Instruction::Unpredictable
            } else {
                Instruction::CmpReg { rm, rn }
            }
        }
        0b100 | 0b101 => {
            Instruction::MovReg { to: rn, from: rm, flags: false }
        }
        0b110 => {
            Instruction::BranchExchange { rm }
        }
        _ => Instruction::Unimplemented
    }
}


fn get_wide_instruction (word: u32, pc: u32) -> Instruction {
    let op1 = (word >> 27) & 0b11;
    let op2 = (word >> 20) & 0b111_1111;
    let op3 = (word >> 15) & 0b1;

    return match op1 {
        0b01 => {
            if bitset(op2, 6) { return id_coprocessor_instr(word); }
            if bitset(op2, 5) { return id_data_processing_shifted(word); }
            if bitset(op2, 2) { return id_ldr_str_dual(word); }
            id_ldr_str_multiple(word)
        }
        0b10 => {
            if op3 > 0 {
                id_branch_and_misc(word, pc)
            } else if bitset(op2, 5) {
                id_data_proc_plain_binary_immediate(word)
            } else {
                id_data_proc_modified_immediate(word)
            }
        }
        0b11 => {
            if bitset(op2, 6) {
                id_coprocessor_instr(word)
            } else if bitset(op2, 5) {
                if bitset(op2, 4) {
                    if bitset(op2, 3) {
                        id_long_multiply_diff(word)
                    } else {
                        id_multiply_diff(word)
                    }
                } else {
                    id_data_proc_register(word)
                }
            } else if (op2 & 0b1110001) == 0 {
                id_store_single(word)
            } else {
                match op2 & 0b111 {
                    0b001 => id_load_byte(word),
                    0b011 => id_load_half_word(word),
                    0b101 => id_load_word(word, pc),
                    _ => Instruction::Undefined,
                }
            }
        }
        _ => panic!("Unexpected pattern") // 0b00 would be a narrow instruction
    }
}


fn id_ldr_str_multiple (word: u32) -> Instruction {
    let op = (word >> 23) & 0b11;
    let l = ((word >> 20) & 0b1) == 1;
    let wback = ((word >> 21) & 0b1) == 1;
    let rn = ((word >> 16) & 0b1111) as u8;
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


fn id_data_processing_shifted (word: u32) -> Instruction { // p148
    assert!((word >> 25) == 0b111_0101);
    let op = (word >> 21) & 0b1111;
    let s = ((word >> 20) & 0b1) > 0;
    let rn_is_pc = ((word >> 16) & 0b1111) == 0b1111;
    let rd = ((word >> 8) & 0b1111) as u8;
    let rd_is_pc = rd == 0b1111;

    return match op {
        0b0000 => {
            if rd_is_pc {
                if s {
                    Instruction::Unpredictable
                } else {
                    Instruction::Unimplemented // TST (reg)
                }
            } else {
                Instruction::Unimplemented // ADD (reg)
            }
        }
        0b0001 => Instruction::Unimplemented, // BIC (reg)
        0b0010 => {
            if rn_is_pc {
                let det = (word >> 4) & 0b11;
                let imm5 = ((word >> 10) & 0b11100) + ((word >> 6) & 0b11);
                return match det {
                    0b00 => {
                        if imm5 == 0 {
                            let rm = (word & 0b1111) as u8;
                            Instruction::MovReg { to: rd, from: rm, flags: s }
                        } else {
                            Instruction::Unimplemented // LSL (imm)
                        }
                    }
                    _ => Instruction::Unimplemented
                }
            } else {
                Instruction::Unimplemented // ORR (reg)
            }
        }
        _ => Instruction::Unimplemented
    }
}


fn id_coprocessor_instr (word: u32) -> Instruction {
    assert!((word & (0b111011 << 26)) == 0b111011 << 26);
    return Instruction::Undefined;
}


fn id_branch_and_misc (word: u32, pc: u32) -> Instruction {
    assert!((word & (0b1111_1000_0000_0000_1 << 15)) == 0b1111_0000_0000_0000_1 << 15);
    let op1 = (word >> 12) & 0b111;

    if bitset(word, 12) {
        let s = bitset(word, 26);
        let j1 = bitset(word, 13);
        let j2 = bitset(word, 11);

        let imm10 = (word & (0x3FF << 16)) >> 4;
        let imm11 = (word & 0x7FF) << 1;
        let mut offset = imm10 + imm11;

        if !(s ^ j2) {
            offset += 1 << 22;
        }

        if !(s ^ j1) {
            offset += 1 << 23;
        }

        if s {
            offset += 0xFF << 24;
        }

        let address = (Wrapping(word_align(pc)) + Wrapping(offset)).0;

        return if bitset(word, 14) {
            Instruction::LinkedBranch { address }
        } else {
            Instruction::Branch { address } // TODO: Proper offset
        }
    }

    return Instruction::Unimplemented;
}





fn id_data_proc_modified_immediate (word: u32) -> Instruction {
    assert!((word & (0b1111_1010_0000_0000_1 << 15)) == 0b1111_0000_0000_0000_0 << 15);

    let op = (word >> 21) & 0b1111;
    let rn = ((word >> 16) & 0b1111) as u8;
    let rn_is_pc = rn == 0b1111;
    let rd = ((word >> 8) & 0b1111) as u8;

    return match op {
        0b0010 if rn_is_pc => {
            let imm12 = ((word >> 15) & (0b1 << 11)) + ((word >> 4) & (0b111 << 8)) + (word & 0xFF);
            let (imm32, carry) = thumb_expand_imm_c(imm12);
            Instruction::MovImm { rd, val: imm32, flags: bitset(word, 20), carry }
        }
        0b1000 if !rn_is_pc => {
            let imm12 = ((word >> 15) & (0b1 << 11)) + ((word >> 4) & (0b111 << 8)) + (word & 0xFF);
            let imm32 = thumb_expand_imm(imm12);
            Instruction::AddImm { dest: rd, first: rn, val: imm32, flags: bitset(word, 20) }
        }
        _ => Instruction::Unimplemented,
    }
}


fn id_data_proc_plain_binary_immediate (word: u32) -> Instruction {
    assert!((word & (0b1111_1010_0000_0000_1 << 15)) == 0b1111_0010_0000_0000_0 << 15);
    let op = (word >> 20) & 0b1_1111;
    let rn = (word >> 16) & 0b1111;
    let rn_is_pc = rn == 0b1111;

    return match op {
        0b0_0000 if !rn_is_pc => Instruction::Unimplemented, // ADD (imm)
        0b0_0000 if rn_is_pc => Instruction::Unimplemented, // ADR
        0b0_0100 => {
            let rd = ((word >> 8) & 0b1111) as u8;
            let imm32 = ((word >> 4) & (0xF << 12)) + ((word >> 15) & (0b1 << 11)) + ((word >> 4) & (0b111 << 8)) + (word & 0xFF);
            Instruction::MovImm { rd, val: imm32, flags: false, carry: CarryChange::Same }
        },
        _ => Instruction::Unimplemented,
    }
}


fn id_long_multiply_diff (word: u32) -> Instruction {
    assert!((word & (0b1111_1111_1 << 23)) == 0b1111_1011_1 << 23);
    return Instruction::Unimplemented;
}


fn id_multiply_diff (word: u32) -> Instruction {
    return Instruction::Unimplemented;
}


fn id_data_proc_register (word: u32) -> Instruction {
    return Instruction::Unimplemented;
}


fn id_store_single (word: u32) -> Instruction {
    assert!((word & (0b1111_1111_0001 << 20)) == 0b1111_1000_0000 << 20);

    let op1 = (word >> 21) & 0b111;
    let op2 = bitset(word, 11);

    let rn = ((word >> 16) & 0b1111) as u8;
    let rt = ((word >> 12) & 0b1111) as u8;

    return match op1 {
        0b110 => {
            let offset = (word & 0xFFF) as u16;
            Instruction::StrImm { rn, rt, offset, add: true, index: true, wback: false }
        },
        0b010 if op2 => {
            let offset = (word & 0xFF) as u16;
            Instruction::StrImm { rn, rt, offset, add: bitset(word, 9), index: bitset(word, 10), wback: bitset(word, 8) }
        },
        _ => Instruction::Unimplemented,
    }
}


fn id_load_byte (word: u32) -> Instruction {
    return Instruction::Unimplemented;
}


fn id_load_half_word (word: u32) -> Instruction {
    return Instruction::Unimplemented;
}


fn id_load_word (word: u32, pc: u32) -> Instruction {
    let op1 = (word >> 23) & 0b11;
    let op2 = (word >> 6) & 0b11_1111;
    let rn = ((word >> 16) & 0b1111) as u8;
    let rt = ((word >> 12) & 0b1111) as u8;
    let offset = (word & 0b1111_1111_1111) as u16;

    if (op1 & 0b10) > 0 { return Instruction::Undefined; }

    if rn == 0b1111 {
        let add = (op1 & 0b1) > 0;
        let base = word_align(pc);
        return Instruction::LdrLit { rt, address: if add { base + offset as u32 } else { base - offset as u32 } };
    }

    if op1 == 0b01 {
        return Instruction::LdrImm { rn, rt, offset, add: true, index: true, wback: false };
    }

    if op2 == 0 {
        let rm = (word & 0b1111) as u8;
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
