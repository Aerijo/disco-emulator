use crate::{ByteInstruction};
use crate::utils::bits::{bitset, matches};
use super::{ItPos, InstructionContext};
use super::opcode::{Opcode};
use super::tag;

type Context = InstructionContext;

pub fn decode_thumb_narrow(hword: u16, c: Context) -> u32 {
    // A5.2
    return match hword >> 10 {
        0b00_0000..=0b00_1111 => id_shift_add_sub_move_cmp(hword, c),
        0b01_0000             => id_data_processing(hword, c),
        0b01_0001             => id_special_data_branch(hword, c),
        0b01_0010..=0b01_0011 => tag::get_narrow(Opcode::LdrLit, c, (hword & 0x7FF) << 2), // A7.7.44 T1
        0b01_0100..=0b01_0111 |
        0b01_1000..=0b01_1111 |
        0b10_0000..=0b10_0111 => id_ldr_str_single(hword, c),
        0b10_1000..=0b10_1001 => {
            tag::get_narrow(Opcode::Adr, c, (hword & 0x7FF) << 2) // A7.7.7 T1
        }
        0b10_1010..=0b10_1011 => {
            tag::get_narrow(Opcode::AddSpImm, c, (hword & 0x7FF) << 2) // A7.7.5 T1
        }
        0b10_1100..=0b10_1111 => id_misc(hword, c),
        0b11_0000..=0b11_0001 => {
            if hword & 0xFF == 0 {
                tag::get_unpred_narrow(Opcode::Stm, c, hword & 0x7FF)
            } else {
                tag::get_narrow(Opcode::Stm, c, hword & 0x7FF) // A7.7.159 T1
            }
        }
        0b11_0010..=0b11_0011 => {
            if hword & 0xFF == 0 {
                tag::get_unpred_narrow(Opcode::Ldm, c, hword & 0x7FF)
            } else {
                tag::get_narrow(Opcode::Ldm, c, hword & 0x7FF) // A7.7.41 T1
            }
        }
        0b11_0100..=0b11_0111 => id_conditional_branch_supc(hword, c),
        0b11_1000..=0b11_1001 => {
            if c.it_pos == ItPos::Within {
                tag::get_unpred_it_narrow(Opcode::Branch, c, hword & 0x7FF)
            } else {
                tag::get_narrow(Opcode::Branch, c, hword & 0x7FF) // A7.7.12 T2
            }
        }
        _ => unreachable!(),
    };
}

fn id_shift_add_sub_move_cmp(hword: u16, c: Context) -> u32 {
    // A5.2.1
    assert!(matches(hword, 14, 0b11, 0b00));
    return match hword >> 9 {
        0b00000..=0b00011 => {
            if (hword >> 6) == 0 {
                if c.it_pos == ItPos::None {
                    tag::get_narrow(Opcode::MovReg, c, (hword & 0b111) + (hword << 1) & (0b111 << 4) + 1 << 8) // A7.7.77 T2
                } else {
                    tag::get_unpred_it_narrow(Opcode::MovReg, c, (hword & 0b111) + (hword << 1) & (0b111 << 4) + 1 << 8)
                }
            } else {
                tag::get_narrow(Opcode::LslImm, c, hword) // A7.7.68 T1 NOTE: we will test the setflags condition when executing
            }
        }
        0b00100..=0b00111 => tag::get_narrow(Opcode::LsrImm, c, hword & 0x7FF), // A7.7.70 T1
        0b01000..=0b01011 => tag::get_narrow(Opcode::AsrImm, c, hword & 0x7FF), // A7.7.10 T1
        0b01100 => {
            let base = (hword & 0b111) + (hword & (0b111 << 3)) << 5 + (hword & (0b111 << 6)) >> 2;
            if c.it_pos == ItPos::None {
                tag::get_narrow(Opcode::AddReg, c, base + 1 << 12) // A7.7.4 T1
            } else {
                tag::get_narrow(Opcode::AddReg, c, base)
            }
        }
        0b01101 => tag::get_narrow(Opcode::SubReg, c, hword & 0x7FF),
        0b01110 => tag::get_narrow(Opcode::AddImm, c, ((hword >> 6) & 0b111) + (hword & 0x3F) << 8), // A7.7.3 T1
        0b01111 => tag::get_narrow(Opcode::SubImm, c, ((hword >> 6) & 0b111) + (hword & 0x3F) << 8), // A7.7.174 T1
        0b10000..=0b10011 => tag::get_narrow(Opcode::MovImm, c, hword & 0x7FF), // A7.7.76 T1
        0b10100..=0b10111 => tag::get_narrow(Opcode::CmpImm, c, hword & 0x7FF), // A7.7.27 T1
        0b11000..=0b11011 => tag::get_narrow(Opcode::AddImm, c, hword & 0x7FF + (hword & (0b111 << 8)) << 3), // A7.7.3 T2
        0b11100..=0b11111 => tag::get_narrow(Opcode::SubImm, c, hword & 0x7FF + (hword & (0b111 << 8)) << 3), // A7.7.174 T2
        _ => unreachable!(),
    }
}

fn id_data_processing(hword: u16, c: Context) -> u32 {
    // A5.2.2
    assert!(matches(hword, 10, 0b1111_11, 0b0100_00));
    let opcode = match (hword >> 6) & 0xF {
        0b0000 => Opcode::AndReg, // A7.7.9 T1
        0b0001 => Opcode::EorReg, // A7.7.36 T1
        0b0010 => Opcode::LslReg, // A7.7.69 T1
        0b0011 => Opcode::LsrReg, // A7.7.71 T1
        0b0100 => Opcode::AsrReg, // A7.7.11 T1
        0b0101 => Opcode::AdcReg, // A7.7.2 T1
        0b0110 => Opcode::SbcReg, // A7.7.2 T1
        0b0111 => Opcode::RorReg, // A7.7.117 T1
        0b1000 => Opcode::TstReg, // A7.7.189 T1
        0b1001 => Opcode::RsbImm, // A7.7.119 T1
        0b1010 => {
            return tag::get_narrow(Opcode::CmpReg, c, (hword & 0b111) + (hword & (0b111 << 3)) << 1); // A7.7.28 T1
        }
        0b1011 => Opcode::CmnReg, // A7.7.26 T1
        0b1100 => Opcode::OrrReg, // A7.7.92 T1
        0b1101 => Opcode::MulReg, // A7.7.84 T1
        0b1110 => Opcode::BicReg, // A7.7.16 T1
        0b1111 => Opcode::MvnReg, // A7.7.86 T1
        _ => unreachable!(),
    };
    return tag::get_narrow(opcode, c, hword & 0x3F);
}

fn id_special_data_branch(hword: u16, c: Context) -> u32 {
    // A5.2.3
    assert!(matches(hword, 10, 0b1111_11, 0b0100_01));
    let rd = ((hword >> 4) & (1 << 3)) + (hword & 0b111);
    let rm = (hword & (0xF << 3)) >> 3;
    return match (hword >> 6) & 0xF {
        0b0000..=0b0011 => {
            let mut instr = tag::get_narrow(Opcode::AddReg, c, rd | (rm << 4) | (rd << 8)); // A7.7.4 T2
            if rd == 15 {
                if rm == (15 << 4) {
                    instr = tag::as_unpred(instr);
                }
                if c.it_pos == ItPos::Within {
                    instr = tag::as_unpred_it(instr);
                }
            }
            return instr;
        }
        0b0100 => tag::as_unpred(tag::get_narrow(Opcode::Other, c, hword)),
        0b0101..=0b0111 => {
            if rd < 8 && rm < 8 || rd == 15 || rm == 15 {
                tag::get_unpred_narrow(Opcode::CmpReg, c, rd | (rm << 4))
            } else {
                tag::get_narrow(Opcode::CmpReg, c, rd | (rm << 4)) // A7.7.28 T2
            }
        }
        0b1000..=0b1011 => {
            if rd == 15 && c.it_pos == ItPos::Within {
                tag::get_unpred_it_narrow(Opcode::MovReg, c, rd | (rm << 4))
            } else {
                tag::get_narrow(Opcode::MovReg, c, rd | (rm << 4)) // A7.7.77
            }
        }
        0b1100..=0b1101 => {
            let mut instr = tag::get_narrow(Opcode::Bx, c, rm); // A7.7.20 T1
            if hword & 0b111 != 0 {
                instr = tag::as_unpred(instr);
            }
            if c.it_pos == ItPos::Within {
                instr = tag::as_unpred_it(instr);
            }
            return instr;
        }
        0b1110..=0b1111 => {
            let mut instr = tag::get_narrow(Opcode::Blx, c, rm); // A7.7.19 T1
            if hword & 0b111 != 0 {
                instr = tag::as_unpred(instr);
            }
            if c.it_pos == ItPos::Within {
                instr = tag::as_unpred_it(instr);
            }
            return instr;
        }
        _ => unreachable!(),
    }
}

fn id_ldr_str_single(hword: u16, c: Context) -> u32 {
    // A5.2.4
    let op_a = hword >> 12;
    assert!(op_a == 0b0101 || op_a == 0b0110 || op_a == 0b0111 || op_a == 0b1000 || op_a == 0b1001);
    let op_c = bitset(hword, 11);
    let rm = (hword >> 6) & 0b111;
    let rn = (hword >> 3) & 0b111;
    let rt = hword & 0b111;
    return match op_a {
        0b0101 => {
            let opcode = match (hword >> 9) & 0b111 {
                0b000 => Opcode::StrReg, // A7.7.162 T1
                0b001 => Opcode::StrhReg, // A7.7.171 T1
                0b010 => Opcode::StrbReg, // A7.7.164 T1
                0b011 => Opcode::LdrsbReg, // A7.7.61 T1
                0b100 => Opcode::LdrReg, // A7.7.45 T1
                0b101 => Opcode::LdrhReg, // A7.7.57 T1
                0b110 => Opcode::LdrbReg, // A7.7.48 T1
                0b111 => Opcode::LdrshReg, // A7.7.48 T1
                _ => unreachable!(),
            };
            return tag::get_narrow(opcode, c, hword & 0x1FF);
        },
        0b0110 => {
            let instr = (rt << 8) | (rn << 12) | ((hword >> 6) & (0x1F));
            if op_c {
                tag::get_narrow(Opcode::LdrImm, c, instr) // A7.7.43 T1
            } else {
                tag::get_narrow(Opcode::StrImm, c, instr) // A7.7.161 T1
            }
        }
        0b0111 => {
            if op_c {
                tag::get_narrow(Opcode::LdrbImm, c, hword & 0x7FF) // A7.7.46 T1
            } else {
                tag::get_narrow(Opcode::StrbImm, c, hword & 0x7FF) // A7.7.163 T1
            }
        }
        0b1000 => {
            let instr = (rt << 6) | (rn << 9) | ((hword >> 6) & (0x1F));
            if op_c {
                tag::get_narrow(Opcode::LdrhImm, c, instr) // A7.7.55 T1
            } else {
                tag::get_narrow(Opcode::StrhImm, c, instr) // A7.7.170 T1
            }
        }
        0b1001 => {
            let instr = (hword & 0x7FF) | (13 << 12);
            if op_c {
                tag::get_narrow(Opcode::LdrImm, c, instr) // A7.7.43 T2
            } else {
                tag::get_narrow(Opcode::StrImm, c, instr) // A7.7.161 T2
            }
        }
        _ => unreachable!(),
    };
}

fn id_misc(hword: u16, c: Context) -> u32 {
    panic!();
}

fn id_conditional_branch_supc(hword: u16, c: Context) -> u32 {
    panic!();
}
