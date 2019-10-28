use crate::instruction::{Instruction, ShiftType, Condition, CarryChange};
use crate::utils::bits::{bitset, thumb_expand_imm_c, thumb_expand_imm, matches, word_align, sign_extend};
use crate::Shift;

pub fn get_wide_instruction(word: u32, pc: u32) -> Instruction {
    // A5.3
    assert!(matches(word, 29, 0b111, 0b111));
    let op2 = (word >> 20) & 0b111_1111;
    return match (word >> 27) & 0b11 {
        0b01 => {
            if bitset(op2, 6) {
                id_coprocessor_instr(word)
            } else if bitset(op2, 5) {
                id_data_processing_shifted(word)
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

fn id_data_processing_shifted(word: u32) -> Instruction {
    // A5.3.11
    assert!(matches(word, 25, 0b111_1111, 0b111_0101));
    let op = (word >> 21) & 0b1111;
    let s = bitset(word, 20);
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
                            Instruction::MovReg {
                                rd,
                                rm,
                                setflags: s,
                            }
                        } else {
                            Instruction::Unimplemented // LSL (imm)
                        }
                    }
                    _ => Instruction::Unimplemented,
                };
            } else {
                Instruction::Unimplemented // ORR (reg)
            }
        }
        _ => Instruction::Unimplemented,
    };
}

fn id_coprocessor_instr(word: u32) -> Instruction {
    assert!((word & (0b111011 << 26)) == 0b111011 << 26);
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

fn id_long_multiply_diff(word: u32) -> Instruction {
    assert!((word & (0b1111_1111_1 << 23)) == 0b1111_1011_1 << 23);
    return Instruction::Unimplemented;
}

fn id_multiply_diff(word: u32) -> Instruction {
    return Instruction::Unimplemented;
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
