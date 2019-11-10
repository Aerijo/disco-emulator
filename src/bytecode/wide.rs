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
    // A5.3.11
    assert!(matches(word, 25, 0b111_1111, 0b111_0101));
    let rn = (word >> 16) & 0xF;
    let rd = (word >> 8) & 0xF;
    let rm = word & 0xF;
    let setflags = bitset(word, 20);

    let pro_data = rd | rn << 4 | rm << 8 | (setflags as u32) << 12;
    let pro_data_comp = (pro_data >> 4) & 0xFF;
    let pro_data_alt = rd | rm << 4 | (setflags as u32) << 8;

    let shift_t = (word >> 4) & 0b11;
    let mut shift_n = (word >> 6) & 0b11 | (word & (0b111 << 12)) >> 10;
    if shift_n == 0 && (shift_t == 0b01 || shift_t == 0b10) {
        shift_n = 32;
    }
    let shift_n = shift_n;
    let pro_extra = shift_t | shift_n << 2;

    let instr = match (word >> 21) & 0xF {
        0b0000 => {
            let base = if rd == 15 {
                tag::get_wide(Opcode::TstReg, c, pro_data_comp, pro_extra) // A7.7.189 T2
            } else {
                tag::get_wide(Opcode::AndReg, c, pro_data, pro_extra) // A7.7.9 T2
            };
            if rd == 13 || (rd == 15 && !setflags) || rn == 13 || rn == 15 || rm == 13 || rm == 15 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        0b0001 => {
            let base = tag::get_wide(Opcode::BicReg, c, pro_data, pro_extra); // A7.7.16 T2
            if rd == 13 || rd == 15 || rn == 13 || rn == 15 || rm == 13 || rm == 15 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        0b0010 => {
            if rn == 15 {
                if shift_t == 0 && shift_n == 0 {
                    let base = tag::get_wide(Opcode::MovReg, c, (word & (1 << 20)) >> 16 | (word >> 8) & 0xF, word & 0xF); // A7.7.77 T3
                    if rd == 15 || rm == 15 || (rd == 13 && rm == 13) || (setflags && (rd == 13 || rm == 13)) {
                        tag::as_unpred_w(base)
                    } else {
                        base
                    }
                } else {
                    let opcode = match shift_t {
                        0b00 => Opcode::LslImm,
                        0b01 => Opcode::LsrImm,
                        0b10 => Opcode::AsrImm,
                        0b11 if shift_n == 0 => Opcode::Rrx,
                        0b11 if shift_n != 0 => Opcode::RorImm,
                        _ => unreachable!(),
                    };
                    let base = tag::get_wide(opcode, c, rd | rm << 4 | (setflags as u32) << 8, shift_n);
                    if rd == 13 || rd == 15 || rm == 13 || rm == 15 {
                        tag::as_unpred_w(base)
                    } else {
                        base
                    }
                }
            } else {
                let base = tag::get_wide(Opcode::OrrReg, c, pro_data, pro_extra); // A7.7.92 T2
                if rd == 13 || rd == 15 || rn == 13 || rm == 13 || rm == 15 {
                    tag::as_unpred_w(base)
                } else {
                    base
                }
            }
        }
        0b0011 => {
            let base = if rn == 15 {
                tag::get_wide(Opcode::MvnReg, c, pro_data_alt, pro_extra) // A7.7.86 T2
            } else {
                tag::get_wide(Opcode::OrnReg, c, pro_data, pro_extra) // A7.7.90 T2
            };
            if rd == 13 || rd == 15 || rm == 13 || rm == 15 || rn == 13 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        0b0100 => {
            let base = if rd == 15 {
                tag::get_wide(Opcode::TeqReg, c, pro_data_comp, pro_extra) // A7.7.187 T1
            } else {
                tag::get_wide(Opcode::EorReg, c, pro_data, pro_extra) // A7.7.36 T2
            };
            if rd == 13 || (rd == 15 && !setflags) || rn == 13 || rn == 15 || rm == 13 || rm == 15 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        0b0110 => {
            let base = if setflags || bitset(word, 4) {
                tag::get_undefined_wide(c, word)
            } else {
                tag::get_wide(Opcode::Pkhbt, c, pro_data, pro_extra) // A7.7.93 T1
            };
            if rd == 13 || rd == 15 || rn == 13 || rn == 15 || rm == 13 || rm == 15 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        0b1000 => {
            let base = if rd == 15 {
                tag::get_wide(Opcode::CmnReg, c, pro_data_comp, pro_extra) // A7.7.26 T2
            } else {
                tag::get_wide(Opcode::AddReg, c, pro_data, pro_extra) // A7.7.4 T3
            };
            if rd == 13 || (rd == 15 && !setflags) || rn == 15 || rm == 13 || rm == 15 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        0b1010 => {
            let base = tag::get_wide(Opcode::AdcReg, c, pro_data, pro_extra); // A7.7.2 T2
            if rd == 13 || rd == 15 || rn == 13 || rn == 15 || rm == 13 || rm == 15 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        0b1011 => {
            let base = tag::get_wide(Opcode::SbcReg, c, pro_data, pro_extra); // A7.7.125 T2
            if rd == 13 || rd == 15 || rn == 13 || rn == 15 || rm == 13 || rm == 15 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        0b1101 => {
            let base = if rd == 15 {
                tag::get_wide(Opcode::CmpReg, c, pro_data_comp, pro_extra) // A7.7.28 T3
            } else {
                tag::get_wide(Opcode::SubReg, c, pro_data, pro_extra) // A7.7.175 T2
            };
            if rd == 13 || (rd == 15 && !setflags) || rn == 15 || rm == 13 || rm == 15 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        0b1110 => {
            let base = tag::get_wide(Opcode::RsbReg, c, pro_data, pro_extra); // A7.7.120 T1
            if rd == 13 || rd == 15 || rn == 13 || rn == 15 || rm == 13 || rm == 15 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        _ => tag::get_undefined_wide(c, word),
    };

    return if bitset(word, 15) {
        tag::as_unpred_w(instr)
    } else {
        instr
    }
}

fn id_ldr_str_dual(word: u32, c: Context) -> ByteInstruction {
    return tag::get_undefined_wide(c, word);
}

fn id_ldr_str_multiple(word: u32, c: Context) -> ByteInstruction {
    // A5.3.5
    assert!(matches(word, 22, 0b111_1111_00_1, 0b111_0100_00_0));
    let l = bitset(word, 20);
    let wrn = (word & (0b10_1111 << 16)) == 0b10_1101;
    return match (word >> 23) & 0b11 {
        0b01 => {
            let rn = (word >> 16) & 0xF;
            if l {
                if wrn {
                    let base = tag::get_wide(Opcode::Pop, c, 0, word & 0xFFFF); // A7.7.99 T2
                    let base = if bitset(word, 13) || (word & 0xFFFF).count_ones() < 2 || (word & (0b11 << 14)) == (0b11 << 14) {
                        tag::as_unpred_w(base)
                    } else {
                        base
                    };
                    return if bitset(word, 15) && c.it_pos == ItPos::Within {
                        tag::as_unpred_it_w(base)
                    } else {
                        base
                    }
                } else {
                    let base = tag::get_wide(Opcode::Ldm, c, rn, word & 0xFFFF | (word & (1 << 21)) >> 5); // A7.7.41 T2
                    let base = if rn == 15 || bitset(word, 13) || (word & 0xFFFF).count_ones() < 2 || (word & (0b11 << 14)) == (0b11 << 14) || (bitset(word, 21) && bitset(word, rn)) {
                        tag::as_unpred_w(base)
                    } else {
                        base
                    };
                    return if bitset(word, 15) && c.it_pos == ItPos::Within {
                        tag::as_unpred_it_w(base)
                    } else {
                        base
                    }
                }
            } else {
                let base = tag::get_wide(Opcode::Stm, c, rn, word & 0xFFFF | (word & (1 << 21)) >> 5); // A7.7.159 T2
                return if rn == 15 || bitset(word, 13) || bitset(word, 15) || (word & 0xFFFF).count_ones() < 2 || (bitset(word, 21) && bitset(word, rn)) {
                    tag::as_unpred_w(base)
                } else {
                    base
                }
            }
        }
        0b10 => {
            tag::get_undefined_wide(c, word)
            // if l {
            //     Instruction::Ldmdb {rn, registers, wback} // A7.7.42 T1
            // } else {
            //     if wrn {
            //         Instruction::Push {registers} // A7.7.101 T2
            //     } else {
            //         Instruction::Stmdb {rn, registers, wback} // A7.7.160 T1
            //     }
            // }
        }
        _ => return tag::get_undefined_wide(c, word),
    };
}

fn id_branch_and_misc(word: u32, c: Context) -> ByteInstruction {
    assert!(matches(word, 15, 0b111_11_0000000_0000_1, 0b111_10_0000000_0000_1));
    let op1 = (word >> 12) & 0b111;

    if bitset(word, 12) {
        return if bitset(word, 14) {
            let mut mid = (word & (1 << 13)) << 9 | (word & (1 << 11)) << 10;
            if !bitset(word, 26) {
                mid = !mid & (0b11 << 21);
            }
            let imm24 = word & 0x7FF | (word & (0x3FF << 16)) >> 5 | (word & (1 << 26)) >> 3 | mid;
            let instr = tag::get_wide(Opcode::Bl, c, 0, imm24);
            if c.it_pos == ItPos::Within {
                tag::as_unpred_it_w(instr)
            } else {
                instr
            }
        } else {
            tag::get_undefined_wide(c, word)
        };
    }

    return tag::get_undefined_wide(c, word);
}

fn id_data_proc_plain_binary_immediate(word: u32, c: Context) -> ByteInstruction {
    assert!(matches(word, 15, 0b111_11_0_1_00000_0000_1, 0b111_10_0_1_00000_0000_0));
    let rd = (word >> 8) & 0xF;
    let rn = (word >> 16) & 0xF;
    let imm12 = word & 0xFF | (word & (0x7 << 12)) >> 4 | (word & (1 << 25)) >> 14;
    return match (word >> 20) & 0x1F {
        0b00000 => {
            let base = if rn == 15 {
                tag::get_wide(Opcode::Adr, c, rd, imm12) // A7.7.7 T4
            } else {
                tag::get_wide(Opcode::AddImm, c, rd << 4 | rn << 8, imm12) // A7.7.3 T4
            };
            if rd == 13 || rd == 15 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        0b00100 => {
            let base = tag::get_wide(Opcode::MovImm, c, rd << 4, (word & (0xF << 16)) >> 4 | imm12); // A7.7.76 T3
            if rd == 13 || rd == 15 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        0b01010 => {
            let base = if rn == 15 {
                tag::get_wide(Opcode::Adr, c, rd, get_negated_simm13(imm12)) // A7.7.7 T3
            } else {
                tag::get_wide(Opcode::SubImm, c, rd << 4 | rn << 8, imm12) // A7.7.174 T4
            };
            if rd == 13 || rd == 15 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        0b01100 => {
            let base = tag::get_wide(Opcode::Movt, c, rd, (word & (0xF << 16)) >> 4 | imm12); // A7.7.76 T3
            if rd == 13 || rd == 15 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        v @ 0b10000 | v @ 0b10010 => {
            // if v == 0b10010 && bitset(word, 21) &&
            tag::get_undefined_wide(c, word) // TODO: SSAT / SSAT16
        }
        _ => tag::get_undefined_wide(c, word),
    }
}

fn thumb_expand_imm_c_alt(word: u32) -> (u32, u32) {
    let full;
    let mut spill = 0;
    let base = word & 0xFF;
    if !bitset(word, 26) && !bitset(word, 14) {
        full = match (word >> 12) & 0b11 {
            0b00 => base,
            0b01 => base | base << 16,
            0b10 => (base | base << 16) << 8,
            0b11 => base | base << 8 | base << 16 | base << 24,
            _ => unreachable!(),
        };
    } else {
        let encoded_shift = (word & (1 << 26)) >> 21 | (word & (0b111 << 12)) >> 11 | (word & (1 << 7)) >> 7;
        full = base << (0x20 - encoded_shift);
        spill |= 0b1000;
        if bitset(full, 31) {
            spill |= 0b0100;
        }
    };
    spill |= full >> 30;
    return (spill, full & !(0b11 << 30));
}

fn id_data_proc_modified_immediate(word: u32, c: Context) -> ByteInstruction {
    // A5.3.1
    assert!(matches(word, 15, 0b111_11_0_1_00000_0000_1, 0b111_10_0_0_00000_0000_0));
    let rn = (word >> 16) & 0xF;
    let rd = (word >> 8) & 0xF;
    let setflags = bitset(word, 20);
    let (spill, extra) = thumb_expand_imm_c_alt(word);
    let pro_spill = (word & (0xF << 8)) >> 4 | (word & (0xF << 16)) >> 8 | (word & (1 << 20)) >> 8 | spill;
    return match (word >> 21) & 0b1111 {
        0b0000 => {
            let base = if rd == 15 {
                tag::get_wide(Opcode::TstImm, c, (word & (0xF << 16)) >> 12 | spill, extra) // A7.7.188
            } else {
                tag::get_wide(Opcode::AndImm, c, pro_spill, extra) // A7.7.8 T1
            };
            return if rn == 13 || rn == 15 || rd == 13 || (rd == 15 && !setflags) {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        0b0001 => {
            let base = tag::get_wide(Opcode::BicImm, c, pro_spill, extra); // A7.7.15 T1
            if rd == 13 || rd == 15 || rn == 13 || rn == 15 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        0b0010 => {
            let base = if rn == 15 {
                tag::get_wide(Opcode::MovImm, c, (word & (1 << 20)) >> 12 | (word & (0xF << 8)) >> 4 | spill, extra) // A7.7.76 T2
            } else {
                tag::get_wide(Opcode::OrrImm, c, pro_spill, extra) // A7.7.91 T1
            };
            if rd == 13 || rd == 15 || rn == 13 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        0b0011 => {
            let base = if rn == 15 {
                tag::get_wide(Opcode::MvnImm, c, (word & (1 << 20)) >> 12 | (word & (0xF << 8)) >> 4 | spill, extra) // A7.7.85 T1
            } else {
                tag::get_wide(Opcode::OrnImm, c, pro_spill, extra) // A7.7.89 T1
            };
            if rd == 13 || rd == 15 || rn == 13 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        0b0100 => {
            let base = if rd == 15 {
                tag::get_wide(Opcode::TeqImm, c, (word & (0xF << 16)) >> 12 | spill, extra) // A7.7.186 T1
            } else {
                tag::get_wide(Opcode::EorImm, c, pro_spill, extra) // A7.7.35 T1
            };
            if rn == 13 || rn == 15 || rd == 13 || (rd == 15 && !setflags) {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        0b1000 => {
            let base = if rd == 15 {
                tag::get_wide(Opcode::CmnImm, c, (word & (0xF << 16)) >> 12 | spill, extra) // A7.7.25 T1
            } else {
                tag::get_wide(Opcode::AddImm, c, pro_spill, extra) // A7.7.3 T3
            };
            if rn == 15 || rd == 13 || (rd == 15 && !setflags) {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        0b1010 => {
            let base = tag::get_wide(Opcode::AdcImm, c, pro_spill, extra); // A7.7.1 T1
            if rd == 13 || rd == 15 || rn == 13 || rn == 15 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        0b1011 => {
            let base = tag::get_wide(Opcode::SbcImm, c, pro_spill, extra); // A7.7.124 T1
            if rd == 13 || rd == 15 || rn == 13 || rn == 15 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        0b1101 => {
            let base = if rd == 15 {
                tag::get_wide(Opcode::CmpImm, c, (word & (0xF << 16)) >> 12 | spill, extra) // A7.7.27 T2
            } else {
                tag::get_wide(Opcode::SubImm, c, pro_spill, extra) // A7.7.174 T3
            };
            if rn == 15 || rd == 13 || (rd == 15 && !setflags) {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        0b1110 => {
            let base = tag::get_wide(Opcode::RsbImm, c, pro_spill, extra); // A7.7.119 T2
            if rd == 13 || rd == 15 || rn == 13 || rn == 15 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        _ => tag::get_undefined_wide(c, word),
    };
}

fn id_long_multiply_div(word: u32, c: Context) -> ByteInstruction {
    // A5.3.17
    assert!(matches(word, 23, 0b111_1111_11, 0b111_1101_11));
    let rn = (word >> 16) & 0xF;
    let rm = word & 0xF;
    let op1 = (word >> 20) & 0b111;
    let op2 = (word >> 4) & 0xF;

    let rd_lo = (word >> 12) & 0xF;
    let rd_hi = (word >> 8) & 0xF;
    let rd = rd_hi;

    return match (op1, op2) {
        (0b000, 0b0000) => {
            let base = tag::get_wide(Opcode::Smull, c, rn | rm << 4, rd_lo | rd_hi << 4); // A7.7.149 T1
            if rd_lo == 13 || rd_lo == 15 || rd_hi == 13 || rd_hi == 15 || rn == 13 || rn == 15 || rm == 13 || rm == 15 || rd_hi == rd_lo {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        (0b001, 0b1111) => {
            let base = tag::get_wide(Opcode::Sdiv, c, rd | rn << 4, rm); // A7.7.127 T1
            if rd_lo != 15 || rd == 13 || rd == 15 || rn == 13 || rn == 15 || rm == 13 || rm == 15 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        (0b010, 0b0000) => {
            let base = tag::get_wide(Opcode::Umull, c, rn | rm << 4, rd_lo | rd_hi << 4); // A7.7.204 T1
            if rd_lo == 13 || rd_lo == 15 || rd_hi == 13 || rd_hi == 15 || rn == 13 || rn == 15 || rm == 13 || rm == 15 || rd_hi == rd_lo {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        (0b011, 0b1111) => {
            let base = tag::get_wide(Opcode::Udiv, c, rd | rn << 4, rm); // A7.7.195 T1
            if rd_lo != 15 || rd == 13 || rd == 15 || rn == 13 || rn == 15 || rm == 13 || rm == 15 {
                tag::as_unpred_w(base)
            } else {
                base
            }
        }
        _ => tag::get_undefined_wide(c, word)
    };
}

fn id_multiply_diff(word: u32, c: Context) -> ByteInstruction {
    // A5.3.16
    assert!(matches(word, 6, 0b111_1111_11_000_0000_0000_0000_11, 0b111_1101_10_000_0000_0000_0000_00));
    let op1 = (word >> 20) & 0b111;
    let op2 = (word >> 4) & 0b11;
    let ra = (word >> 12) & 0xF;

    let rn = (word >> 16) & 0xF;
    let rd = (word >> 8) & 0xF;
    let rm = word & 0xF;

    return match op1 {
        0b000 => {
            if op2 == 0b01 {
                tag::get_undefined_wide(c, word)
            } else if op2 != 0b00 {
                tag::get_undefined_wide(c, word)
            } else if ra == 15 {
                let base = tag::get_wide(Opcode::Mul, c, rd | rn << 4, rm); // A7.7.84 T2
                if rd == 13 || rd == 15 || rn == 13 || rn == 15 || rm == 13 || rm == 15 {
                    tag::as_unpred_w(base)
                } else {
                    base
                }
            } else {
                tag::get_undefined_wide(c, word)
            }
        }
        _ => tag::get_undefined_wide(c, word),
    };
}

fn id_data_proc_register(word: u32, c: Context) -> ByteInstruction {
    return tag::get_undefined_wide(c, word);
}

fn id_store_single(word: u32, c: Context) -> ByteInstruction {
    assert!(matches(word, 20, 0b111_1111_1_000_1, 0b111_1100_0_000_0));

    let op2 = bitset(word, 11);

    let rn = ((word >> 16) & 0b1111) as u8;
    let rt = ((word >> 12) & 0b1111) as u8;

    return match (word >> 21) & 0b111 {
        0b110 => tag::get_wide(Opcode::StrImm, c, (word >> 12) & 0xFF, word & 0xFFF | 1 << 14), // A7.7.161 T3
        0b010 if op2 => {
            // p381 T4
            let imm13 = if bitset(word, 9) {
                word & 0xFF
            } else {
                (!(word & 0xFF) + 1) & 0x1FFF
            };
            tag::get_wide(Opcode::StrImm, c, (word >> 12) & 0xFF, imm13 | (word & (1 << 10)) << 4 | (word & (1 << 8)) << 5)
        }
        _ => tag::get_undefined_wide(c, word),
    };
}

fn id_load_byte(word: u32, c: Context) -> ByteInstruction {
    return tag::get_undefined_wide(c, word);
}

fn id_load_half_word(word: u32, c: Context) -> ByteInstruction {
    return tag::get_undefined_wide(c, word);
}

fn get_negated_simm13(imm12: u32) -> u32 {
    return (!(imm12 & 0xFFF) + 1) & 0x1FFF;
}

fn id_load_word(word: u32, c: Context) -> ByteInstruction {
    let op1 = (word >> 23) & 0b11;
    let op2 = (word >> 6) & 0x3F;
    let rn = (word >> 16) & 0xF;
    let rt = (word >> 12) & 0xF;

    if op1 > 1 {
        return tag::get_undefined_wide(c, word);
    }

    if rn == 15 {
        let imm13 = if bitset(word, 23) {
            word & 0xFFF
        } else {
            get_negated_simm13(word)
        };
        let base = tag::get_wide(Opcode::LdrLit, c, rt, imm13);
        return if c.it_pos == ItPos::Within {
            tag::as_unpred_it_w(base)
        } else {
            base
        }
    }

    if op1 == 0b01 {
        let imm13 = word & 0xFFF;
        let base = tag::get_wide(Opcode::LdrImm, c, word & (0xFF << 12) >> 12, word & 0xFFF | 1 << 14); // A7.7.43 T3
        return if c.it_pos == ItPos::Within {
            tag::as_unpred_it_w(base)
        } else {
            base
        }
    }

    if op2 == 0 {
        return tag::get_wide(Opcode::LdrReg, c, word & (0xFF << 12) >> 12, word & 0x3F);
    }

    let op3 = op2 >> 2;

    if op3 == 0b1100 || (op3 & 0b1001) == 0b1001 {
        let imm13 = if bitset(word, 9) {
            word & 0xFF
        } else {
            get_negated_simm13(word & 0xFF)
        };
        let base = tag::get_wide(Opcode::LdrImm, c, (word & (0xFF << 12)) >> 12, imm13 | (word & (1 << 10)) << 4 | (word & (1 << 8)) << 5); // A7.7.43 T4
        let rt = (word >> 12) & 0xF;
        let rn = (word >> 16) & 0xF;
        let base = if bitset(word, 8) && rn == rt {
            tag::as_unpred_w(base)
        } else {
            base
        };
        return if rt == 15 && c.it_pos == ItPos::Within {
            tag::as_unpred_it_w(base)
        } else {
            base
        }
    }

    // if op3 == 0b1110 {
    //     return Instruction::Ldrt {
    //         rn,
    //         rt,
    //         offset: offset8,
    //     };
    // }

    return tag::get_undefined_wide(c, word);
}
