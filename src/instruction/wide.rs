use crate::instruction::{Instruction, ShiftType, CarryChange};
use crate::utils::bits::{bitset, thumb_expand_imm_c, thumb_expand_imm, matches, word_align, sign_extend, decode_imm_shift};
use crate::board::{Condition, Shift};

pub fn get_wide_instruction(word: u32, pc: u32) -> Instruction {
    // A5.3
    assert!(matches(word, 29, 0b111, 0b111));
    let op2 = (word >> 20) & 0b111_1111;
    return match (word >> 27) & 0b11 {
        0b01 => {
            if bitset(op2, 6) {
                id_coprocessor_instr(word)
            } else if bitset(op2, 5) {
                id_data_processing_shifted_register(word)
            } else if bitset(op2, 2) {
                id_ldr_str_dual(word, pc)
            } else {
                id_ldr_str_multiple(word)
            }
        }
        0b10 => {
            if bitset(word, 15) {
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
                        id_long_multiply_div(word)
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
        _ => panic!(), // 0b00 would be a narrow instruction
    };
}

fn id_data_proc_modified_immediate(word: u32) -> Instruction {
    // A5.3.1
    assert!(matches(word, 15, 0b111_11_0_1_00000_0000_1, 0b111_10_0_0_00000_0000_0));
    let rn = ((word >> 16) & 0b1111) as u8;
    let rd = ((word >> 8) & 0b1111) as u8;
    let rn_is_pc = rn == 15;
    let rd_is_pc = rd == 15;
    let setflags = bitset(word, 20);
    let imm12 = ((word >> 15) & (0b1 << 11)) + ((word >> 4) & (0b111 << 8)) + (word & 0xFF);
    let (imm32, carry) = thumb_expand_imm_c(imm12);
    return match (word >> 21) & 0b1111 {
        0b0000 => {
            if rn == 13 || rn == 15 || rd == 13 || (rd == 15 && !setflags) {
                Instruction::Unpredictable
            } else if rd_is_pc {
                Instruction::TstImm {rn, imm32, carry} // A7.7.188
            } else {
                Instruction::AndImm {rd, rn, imm32, setflags, carry} // A7.7.8 T1
            }
        }
        0b0001 => {
            if rd == 13 || rd == 15 || rn == 13 || rn == 15 {
                Instruction::Unpredictable
            } else {
                Instruction::BicImm {rd, rn, imm32, setflags, carry} // A7.7.15 T1
            }
        }
        0b0010 => {
            if rd == 13 || rd == 15 || rn == 13 {
                Instruction::Unpredictable
            } else if rn_is_pc {
                Instruction::MovImm {rd, imm32, setflags, carry} // A7.7.76 T2
            } else {
                Instruction::OrrImm {rd, rn, imm32, setflags, carry} // A7.7.91 T1
            }
        }
        0b0011 => {
            if rd == 13 || rd == 15 ||rn == 13 {
                Instruction::Unpredictable
            } else if rn_is_pc {
                Instruction::MvnImm {rd, imm32, setflags, carry} // A7.7.85 T1
            } else {
                Instruction::OrnImm {rd, rn, imm32, setflags, carry} // A7.7.89 T1
            }
        }
        0b0100 => {
            if rn == 13 || rn == 15 || rd == 13 || (rd == 15 && !setflags) {
                Instruction::Unpredictable
            } else if rd_is_pc {
                Instruction::TeqImm {rn, imm32, carry} // A7.7.186 T1
            } else {
                Instruction::EorImm {rd, rn, imm32, setflags, carry} // A7.7.35 T1
            }
        }
        0b1000 => {
            if rn == 15 || rd == 13 || (rd == 15 && !setflags) {
                Instruction::Unpredictable
            } else if rd_is_pc {
                Instruction::CmnImm {rn, imm32} // A7.7.25 T1
            } else {
                Instruction::AddImm {rd, rn, imm32, setflags} // A7.7.3 T3
            }
        }
        0b1010 => {
            if rd == 13 || rd == 15 || rn == 13 || rd == 15 {
                Instruction::Unpredictable
            } else {
                Instruction::AdcImm {rd, rn, imm32, setflags} // A7.7.1 T1
            }
        }
        0b1011 => {
            if rd == 13 || rd == 15 || rn == 13 || rd == 15 {
                Instruction::Unpredictable
            } else {
                Instruction::SbcImm {rd, rn, imm32, setflags} // A7.7.124 T1
            }
        }
        0b1101 => {
            if rn == 15 || rd == 13 || (rd == 15 && !setflags) {
                Instruction::Unpredictable
            } else if rd_is_pc {
                Instruction::CmpImm {rn, imm32} // A7.7.27 T2
            } else {
                Instruction::SubImm {rd, rn, imm32, setflags} // A7.7.174 T3
            }
        }
        0b1110 => {
            if rd == 13 || rd == 15 || rn == 13 || rn == 15 {
                Instruction::Unpredictable
            } else {
                Instruction::RsbImm {rd, rn, imm32, setflags} // A7.7.119 T2
            }
        }
        _ => Instruction::Undefined,
    };
}

fn id_ldr_str_multiple(word: u32) -> Instruction {
    // A5.3.5
    assert!(matches(word, 22, 0b111_1111_00_1, 0b111_0100_00_0));
    let l = bitset(word, 20);
    let wback = bitset(word, 21);
    let rn = ((word >> 16) & 0b1111) as u8;
    let registers = (word & 0xFFFF) as u16;
    return match (word >> 23) & 0b11 {
        0b01 => {
            if l {
                if wback && rn == 0b1101 {
                    Instruction::Pop {registers}
                } else {
                    Instruction::Ldm {rn, registers, wback} // A7.7.41 T2
                }
            } else {
                Instruction::Stm {rn, registers, wback} // A7.7.159 T2
            }
        }
        0b10 => {
            if l {
                Instruction::Ldmdb {rn, registers, wback} // A7.7.42 T1
            } else {
                if wback && rn == 0b1101 {
                    Instruction::Push {registers} // A7.7.101 T2
                } else {
                    Instruction::Stmdb {rn, registers, wback} // A7.7.160 T1
                }
            }
        }
        _ => Instruction::Undefined,
    };
}

fn id_ldr_str_dual(word: u32, pc: u32) -> Instruction {
    // A5.3.6
    assert!(matches(word, 22, 0b111_1111_00_1, 0b111_0100_00_1));
    let op_12 = (word >> 21) & (0b11 << 2) + (word >> 20) & 0b11;
    let op_3 = (word >> 4) & 0b1111;
    let rn = ((word >> 16) & 0b1111) as u8;
    let rt = ((word >> 12) & 0xF) as u8;
    let rd = ((word >> 8) & 0xF) as u8;
    let rd2 = (word & 0xF) as u8;
    let imm32 = (word & 0xFF) << 2;
    return match op_12 {
        0b0000 => {
            if rd == 13 || rd == 15 || rt == 13 || rt == 15 || rn == 15 || rd == rn || rd == rt {
                Instruction::Unpredictable
            } else {
                Instruction::Strex {rn, rt, rd, imm32} // A7.7.167 T1
            }
        }
        0b0001 => {
            if rd != 15 || rt == 13 || rt == 15 || rn == 15 {
                Instruction::Unpredictable
            } else {
                Instruction::Ldrex {rn, rt, imm32} // A7.7.52 T1
            }
        }
        0b0100 => {
            if rd != 15 || rd2 == 13 || rd2 == 15 || rt == 13 || rt == 15 || rn == 15 || rd2 == rn || rd2 == rt {
                Instruction::Unpredictable
            } else if op_3 == 0b0100 {
                Instruction::Strexb {rn, rt, rd: rd2} // A7.7.168 T1
            } else if op_3 == 0b0101 {
                Instruction::Strexh {rn, rt, rd: rd2} // A7.7.169 T1
            } else {
                Instruction::Undefined
            }
        }
        0b0101 => {
            if op_3 == 0b0000 {
                if rt != 15 || rd != 0 || rn == 13 || rd2 == 13 || rd2 == 15 {
                    Instruction::Unpredictable
                } else {
                    Instruction::Tbb {rn, rm: rd2} // A7.7.185 T1
                }
            } else if op_3 == 0b0001 {
                if rt != 15 || rd != 0 || rn == 13 || rd2 == 13 || rd2 == 15 {
                    Instruction::Unpredictable
                } else {
                    Instruction::Tbh {rn, rm: rd2} // A7.7.185 T1
                }
            } else if op_3 == 0b0100 {
                if rd != 15 || rd2 != 15 || rn == 13 || rt == 13 || rt == 15 {
                    Instruction::Unpredictable
                } else {
                    Instruction::Ldrexb {rn, rt} // A7.7.53 T1
                }
            } else if op_3 == 0b0101 {
                if rd != 15 || rd2 != 15 || rn == 13 || rt == 13 || rt == 15 {
                    Instruction::Unpredictable
                } else {
                    Instruction::Ldrexh {rn, rt} // A7.7.54 T1
                }
            } else {
                Instruction::Undefined
            }
        }
        _ => {
            let index = bitset(word, 24);
            let offset = if bitset(word, 23) { imm32 as i32 } else { - (imm32 as i32) };
            let wback = bitset(word, 21);

            if bitset(word, 20) {
                if rn == 15 {
                    if rt == 13 || rt == 15 || rd == 13 || rd == 15 || rt == rd || wback || pc != word_align(pc) {
                        Instruction::Unpredictable
                    } else {
                        Instruction::LdrdLit {rt, rt2: rd, address: pc.wrapping_add(offset as u32)} // A7.7.51 T1
                    }
                } else {
                    if (wback && (rn == rt || rn == rd)) || rt == 13 || rt == 15 || rd == 13 || rd == 15 || rt == rd {
                        Instruction::Unpredictable
                    } else {
                        Instruction::LdrdImm {rn, rt, rt2: rd, offset, index, wback} // A7.7.50 T1
                    }
                }
            } else {
                if (wback && (rn == rt || rn == rd)) || rn == 15 || rt == 13 || rt == 15 || rd == 13 || rd == 15 {
                    Instruction::Unpredictable
                } else {
                    Instruction::StrdImm {rn, rt, rt2: rd, offset, index, wback} // A7.7.166 T1
                }
            }
        }
    }
}

fn id_data_processing_shifted_register(word: u32) -> Instruction {
    // A5.3.11
    assert!(matches(word, 25, 0b111_1111, 0b111_0101));
    let rn = ((word >> 16) & 0xF) as u8;
    let rd = ((word >> 8) & 0xF) as u8;
    let rm = (word & 0xF) as u8;
    let rn_is_pc = rn == 15;
    let rd_is_pc = rd == 15;
    let setflags = bitset(word, 20);

    let shift = decode_imm_shift(((word << 1) & (0b11 << 5)) + ((word >> 6) & 0b11) + ((word >> 10) & (0b111 << 2)));

    if bitset(word, 15) {
        return Instruction::Unpredictable;
    }

    return match (word >> 21) & 0xF {
        0b0000 => {
            if rd == 13 || (rd == 15 && !setflags) || rn == 13 || rn == 15 || rm == 13 || rm == 15 {
                Instruction::Unpredictable
            } else if rd_is_pc {
                Instruction::TstReg {rn, rm, shift} // A7.7.189 T2
            } else {
                Instruction::AndReg {rn, rd, rm, shift, setflags} // A7.7.9 T2
            }
        }
        0b0001 => {
            if rd == 13 || rd == 15 || rn == 13 || rn == 15 || rm == 13 || rm == 15 {
                Instruction::Unpredictable
            } else {
                Instruction::BicReg {rd, rn, rm, shift, setflags} // A7.7.16 T2
            }
        }
        0b0010 => {
            if rn_is_pc {
                if shift.shift_t == ShiftType::LSL && shift.shift_n == 0 {
                    if rd == 15 || rm == 15 || (rd == 13 && rm == 13) || (setflags && (rd == 13 || rm == 13)) {
                        Instruction::Unpredictable
                    } else {
                        Instruction::MovReg {rd, rm, setflags} // A7.7.77 T3
                    }
                } else if rd == 13 || rd == 15 || rm == 13 || rm == 15 {
                    Instruction::Unpredictable
                } else {
                    Instruction::ShiftImm {rd, rm, shift, setflags} // LslImm, LsrImm, AsrImm, Rrx, RorImm
                }
            } else if rd == 13 || rd == 15 || rn == 13 || rm == 13 || rm == 15 {
                Instruction::Unpredictable
            } else {
                Instruction::OrrReg {rd, rn, rm, shift, setflags} // A7.7.92 T2
            }
        }
        0b0011 => {
            if rd == 13 || rd == 15 || rm == 13 || rm == 15 || rn == 13 {
                Instruction::Unpredictable
            } else if rn_is_pc {
                Instruction::MvnReg {rd, rm, shift, setflags} // A7.7.86 T2
            } else {
                Instruction::OrnReg {rd, rn, rm, shift, setflags} // A7.7.90 T2
            }
        }
        0b0100 => {
            if rd == 13 || (rd == 15 && !setflags) || rn == 13 || rn == 15 || rm == 13 || rm == 15 {
                Instruction::Unpredictable
            } else if rd_is_pc {
                Instruction::TeqReg {rn, rm, shift} // A7.7.187 T1
            } else {
                Instruction::EorReg {rd, rn, rm, shift, setflags} // A7.7.36 T2
            }
        }
        0b0110 => {
            if rd == 13 || rd == 15 || rn == 13 || rn == 15 || rm == 13 || rm == 15 {
                Instruction::Unpredictable
            } else if setflags || bitset(word, 4) {
                Instruction::Undefined
            } else if bitset(word, 5) {
                Instruction::Pkhbt {rd, rn, rm, shift} // A7.7.93 T1
            } else {
                Instruction::Pkhtb {rd, rn, rm, shift} // A7.7.93 T1
            }
        }
        0b1000 => {
            if rd == 13 || (rd == 15 && !setflags) || rn == 15 || rm == 13 || rm == 15 {
                Instruction::Unpredictable
            } else if rd_is_pc {
                Instruction::CmnReg {rn, rm, shift} // A7.7.26 T2
            } else {
                Instruction::AddReg {rd, rn, rm, shift, setflags} // A7.7.4 T3
            }
        }
        0b1010 => {
            if rd == 13 || rd == 15 || rn == 13 || rn == 15 || rm == 13 || rm == 15 {
                Instruction::Unpredictable
            } else {
                Instruction::AdcReg {rd, rn, rm, shift, setflags} // A7.7.2 T2
            }
        }
        0b1011 => {
            if rd == 13 || rd == 15 || rn == 13 || rn == 15 || rm == 13 || rm == 15 {
                Instruction::Unpredictable
            } else {
                Instruction::SbcReg {rd, rn, rm, shift, setflags} // A7.7.125 T2
            }
        }
        0b1101 => {
            if rd == 13 || (rd == 15 && !setflags) || rn == 15 || rm == 13 || rm == 15 {
                Instruction::Unpredictable
            } else if rd_is_pc {
                Instruction::CmpReg {rn, rm, shift} // A7.7.28 T3
            } else {
                Instruction::SubReg {rn, rd, rm, shift, setflags} // A7.7.175 T2
            }
        }
        0b1110 => {
            if rd == 13 || rd == 15 || rn == 13 || rn == 15 || rm == 13 || rm == 15 {
                Instruction::Unpredictable
            } else {
                Instruction::RsbReg {rd, rn, rm, shift, setflags} // A7.7.120 T1
            }
        }
        _ => Instruction::Undefined,
    };
}

fn id_long_multiply_div(word: u32) -> Instruction {
    // A5.3.17
    assert!(matches(word, 23, 0b111_1111_11, 0b111_1101_11));
    let rn = ((word >> 16) & 0xF) as u8;
    let rm = (word & 0xF) as u8;
    let op1 = (word >> 20) & 0b111;
    let op2 = (word >> 4) & 0xF;

    let rd_lo = ((word >> 12) & 0xF) as u8;
    let rd_hi = ((word >> 8) & 0xF) as u8;
    let rd = rd_hi;

    return match (op1, op2) {
        (0b000, 0b0000) => {
            if rd_lo == 13 || rd_lo == 15 || rd_hi == 13 || rd_hi == 15 || rn == 13 || rn == 15 || rm == 13 || rm == 15 || rd_hi == rd_lo {
                Instruction::Unpredictable
            } else {
                Instruction::Smull {rd_lo, rd_hi, rn, rm} // A7.7.149 T1
            }
        }
        (0b001, 0b1111) => {
            if rd_lo != 15 || rd == 13 || rd == 15 || rn == 13 || rn == 15 || rm == 13 || rm == 15 {
                Instruction::Unpredictable
            } else {
                Instruction::Sdiv {rd, rn, rm} // A7.7.127 T1
            }
        }
        (0b010, 0b0000) => {
            if rd_lo == 13 || rd_lo == 15 || rd_hi == 13 || rd_hi == 15 || rn == 13 || rn == 15 || rm == 13 || rm == 15 || rd_hi == rd_lo {
                Instruction::Unpredictable
            } else {
                Instruction::Umull {rd_lo, rd_hi, rn, rm} // A7.7.204 T1
            }
        }
        (0b011, 0b1111) => {
            if rd_lo != 15 || rd == 13 || rd == 15 || rn == 13 || rn == 15 || rm == 13 || rm == 15 {
                Instruction::Unpredictable
            } else {
                Instruction::Udiv {rd, rn, rm} // A7.7.195 T1
            }
        }
        _ => Instruction::Unimplemented
    }
}

fn id_coprocessor_instr(word: u32) -> Instruction {
    // A5.3.18
    assert!(matches(word, 26, 0b111_0_11, 0b111_0_11));
    return Instruction::Unimplemented;
}

fn id_branch_and_misc(word: u32, pc: u32) -> Instruction {
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

        let address = pc.wrapping_add(offset);

        return if bitset(word, 14) {
            Instruction::LinkedBranch { address }
        } else {
            Instruction::Branch { address } // TODO: Proper offset
        };
    }

    return Instruction::Unimplemented;
}

fn id_data_proc_plain_binary_immediate(word: u32) -> Instruction {
    assert!((word & (0b1111_1010_0000_0000_1 << 15)) == 0b1111_0010_0000_0000_0 << 15);
    let op = (word >> 20) & 0b1_1111;
    let rn = (word >> 16) & 0b1111;
    let rn_is_pc = rn == 0b1111;

    return match op {
        0b0_0000 if !rn_is_pc => Instruction::Unimplemented, // ADD (imm)
        0b0_0000 if rn_is_pc => Instruction::Unimplemented,  // ADR
        0b0_0100 => {
            let rd = ((word >> 8) & 0b1111) as u8;
            let imm32 = ((word >> 4) & (0xF << 12))
                + ((word >> 15) & (0b1 << 11))
                + ((word >> 4) & (0b111 << 8))
                + (word & 0xFF);
            Instruction::MovImm {rd, imm32, setflags: false, carry: CarryChange::Same}
        }
        _ => Instruction::Unimplemented,
    };
}

fn id_multiply_diff(word: u32) -> Instruction {
    // A5.3.16
    assert!(matches(word, 6, 0b111_1111_11_000_0000_0000_0000_11, 0b111_1101_10_000_0000_0000_0000_00));
    let op1 = (word >> 20) & 0b111;
    let op2 = (word >> 4) & 0b11;
    let ra = ((word >> 12) & 0xF) as u8;

    let rn = ((word >> 16) & 0xF) as u8;
    let rd = ((word >> 8) & 0xF) as u8;
    let rm = (word & 0xF) as u8;

    return match op1 {
        0b000 => {
            if op2 == 0b01 {
                Instruction::Unimplemented
            } else if op2 != 0b00 {
                Instruction::Undefined
            } else if ra == 15 {
                if rd == 13 || rd == 15 || rn == 13 || rn == 15 || rm == 13 || rm == 15 {
                    Instruction::Unpredictable
                } else {
                    Instruction::Mul {rd, rn, rm, setflags: false} // A7.7.84 T2
                }
            } else {
                Instruction::Unimplemented
            }
        }
        _ => Instruction::Unimplemented
    };
}

fn id_data_proc_register(word: u32) -> Instruction {
    return Instruction::Unimplemented;
}

fn id_store_single(word: u32) -> Instruction {
    assert!((word & (0b1111_1111_0001 << 20)) == 0b1111_1000_0000 << 20);

    let op1 = (word >> 21) & 0b111;
    let op2 = bitset(word, 11);

    let rn = ((word >> 16) & 0b1111) as u8;
    let rt = ((word >> 12) & 0b1111) as u8;

    return match op1 {
        0b110 => {
            // p381 T3
            let offset = (word & 0xFFF) as i32;
            Instruction::StrImm {
                rn,
                rt,
                offset,
                index: true,
                wback: false,
            }
        }
        0b010 if op2 => {
            // p381 T4
            let mut offset = (word & 0xFF) as i32;
            if !bitset(word, 9) {
                offset = -offset;
            }
            Instruction::StrImm {
                rn,
                rt,
                offset,
                index: bitset(word, 10),
                wback: bitset(word, 8),
            }
        }
        _ => Instruction::Unimplemented,
    };
}

fn id_load_byte(word: u32) -> Instruction {
    return Instruction::Unimplemented;
}

fn id_load_half_word(word: u32) -> Instruction {
    return Instruction::Unimplemented;
}

fn id_load_word(word: u32, pc: u32) -> Instruction {
    let op1 = (word >> 23) & 0b11;
    let op2 = (word >> 6) & 0b11_1111;
    let rn = ((word >> 16) & 0b1111) as u8;
    let rt = ((word >> 12) & 0b1111) as u8;
    let offset = (word & 0b1111_1111_1111) as i32;

    if (op1 & 0b10) > 0 {
        return Instruction::Undefined;
    }

    if rn == 0b1111 {
        let add = (op1 & 0b1) > 0;
        let base = word_align(pc);
        return Instruction::LdrLit {
            rt,
            address: if add {
                base + offset as u32
            } else {
                base - offset as u32
            },
        };
    }

    if op1 == 0b01 {
        // p243 T3
        return Instruction::LdrImm {
            rn,
            rt,
            offset,
            index: true,
            wback: false,
        };
    }

    if op2 == 0 {
        let rm = (word & 0b1111) as u8;
        let shift_n = ((word >> 4) & 0b11) as u32;
        return Instruction::LdrReg { rn, rt, rm, shift: Shift {shift_t: ShiftType::LSL, shift_n}};
    }

    let op3 = op2 >> 2;
    let mut offset8 = (word & 0b1111_1111) as i32;

    if op3 == 0b1100 || (op3 & 0b1001) == 0b1001 {
        // p243 T4
        if !bitset(word, 9) {
            offset8 = -offset8;
        }
        return Instruction::LdrImm {
            rn,
            rt,
            offset: offset8,
            index: bitset(word, 10),
            wback: bitset(word, 8),
        };
    }

    if op3 == 0b1110 {
        return Instruction::Ldrt {
            rn,
            rt,
            offset: offset8,
        };
    }

    return Instruction::Undefined;
}
