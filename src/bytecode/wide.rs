use crate::{ByteInstruction};
use crate::utils::bits::{bitset, matches};
use super::{ItPos, InstructionContext};
use super::opcode::{Opcode};
use super::tag;

type Context = InstructionContext;

pub fn decode_thumb_wide(word: u32, c: InstructionContext) -> ByteInstruction {
    // A5.3
    assert!(matches(word, 29, 0b111, 0b111));
    let op2 = (word >> 20) & 0b111_1111;
    return match (word >> 27) & 0b11 {
        0b01 => {
            if bitset(op2, 6) {
                id_coprocessor_instr(word, c)
            } else if bitset(op2, 5) {
                id_data_processing_shifted_register(word, c)
            } else if bitset(op2, 2) {
                id_ldr_str_dual(word, c)
            } else {
                id_ldr_str_multiple(word, c)
            }
        }
        0b10 => {
            if bitset(word, 15) {
                id_branch_and_misc(word, c)
            } else if bitset(op2, 5) {
                id_data_proc_plain_binary_immediate(word, c)
            } else {
                id_data_proc_modified_immediate(word, c)
            }
        }
        0b11 => {
            if bitset(op2, 6) {
                id_coprocessor_instr(word, c)
            } else if bitset(op2, 5) {
                if bitset(op2, 4) {
                    if bitset(op2, 3) {
                        id_long_multiply_div(word, c)
                    } else {
                        id_multiply_diff(word, c)
                    }
                } else {
                    id_data_proc_register(word, c)
                }
            } else if (op2 & 0b1110001) == 0 {
                id_store_single(word, c)
            } else {
                match op2 & 0b111 {
                    0b001 => id_load_byte(word, c),
                    0b011 => id_load_half_word(word, c),
                    0b101 => id_load_word(word, c),
                    _ => tag::get_undefined_wide(c, word),
                }
            }
        }
        _ => unreachable!(), // 0b00 would be a narrow instruction
    };
}

fn id_coprocessor_instr(word: u32, c: Context) -> ByteInstruction {
    panic!();
}

fn id_data_processing_shifted_register(word: u32, c: Context) -> ByteInstruction {
    panic!();
}

fn id_ldr_str_dual(word: u32, c: Context) -> ByteInstruction {
    panic!();
}

fn id_ldr_str_multiple(word: u32, c: Context) -> ByteInstruction {
    panic!();
}

fn id_branch_and_misc(word: u32, c: Context) -> ByteInstruction {
    panic!();
}

fn id_data_proc_plain_binary_immediate(word: u32, c: Context) -> ByteInstruction {
    panic!();
}

fn id_data_proc_modified_immediate(word: u32, c: Context) -> ByteInstruction {
    panic!();
}

fn id_long_multiply_div(word: u32, c: Context) -> ByteInstruction {
    panic!();
}

fn id_multiply_diff(word: u32, c: Context) -> ByteInstruction {
    panic!();
}

fn id_data_proc_register(word: u32, c: Context) -> ByteInstruction {
    panic!();
}

fn id_store_single(word: u32, c: Context) -> ByteInstruction {
    panic!();
}

fn id_load_byte(word: u32, c: Context) -> ByteInstruction {
    panic!();
}

fn id_load_half_word(word: u32, c: Context) -> ByteInstruction {
    panic!();
}

fn id_load_word(word: u32, c: Context) -> ByteInstruction {
    let op1 = (word >> 23) & 0b11;
    let op2 = (word >> 6) & 0x3F;
    let rn = (word >> 16) & 0xF;
    let rt = (word >> 12) & 0xF;
    let offset = word & 0xFFF;

    if op1 > 1 {
        return tag::get_undefined_wide(c, word);
    }

    if rn == 15 {
        let imm13 = if bitset(word, 23) {
            word & 0xFFF
        } else {
            (!(word & 0xFFF) + 1) & 0x1FFF
        };
        let instr = tag::get_wide(Opcode::LdrLit, c, rt, imm13);
        return if c.it_pos == ItPos::Within {
            tag::as_unpred_it_w(instr)
        } else {
            instr
        }
    }

    // if op1 == 0b01 {
    //     // p243 T3
    //     return Instruction::LdrImm {
    //         rn,
    //         rt,
    //         offset,
    //         index: true,
    //         wback: false,
    //     };
    // }
    //
    // if op2 == 0 {
    //     let rm = (word & 0b1111) as u8;
    //     let shift_n = ((word >> 4) & 0b11) as u32;
    //     return Instruction::LdrReg { rn, rt, rm, shift: Shift {shift_t: ShiftType::LSL, shift_n}};
    // }
    //
    // let op3 = op2 >> 2;
    // let mut offset8 = (word & 0b1111_1111) as i32;
    //
    // if op3 == 0b1100 || (op3 & 0b1001) == 0b1001 {
    //     // p243 T4
    //     if !bitset(word, 9) {
    //         offset8 = -offset8;
    //     }
    //     return Instruction::LdrImm {
    //         rn,
    //         rt,
    //         offset: offset8,
    //         index: bitset(word, 10),
    //         wback: bitset(word, 8),
    //     };
    // }
    //
    // if op3 == 0b1110 {
    //     return Instruction::Ldrt {
    //         rn,
    //         rt,
    //         offset: offset8,
    //     };
    // }

    return tag::get_undefined_wide(c, word);
}
