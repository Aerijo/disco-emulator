use crate::instruction::{Instruction, ShiftType, Condition, CarryChange};
use crate::utils::bits::{bitset, matches, word_align, sign_extend};
use crate::Shift;

pub fn get_narrow_instruction(hword: u16, pc: u32) -> Instruction {
    // A5.2
    return match hword >> 10 {
        0b00_0000..=0b00_1111 => id_shift_add_sub_move_cmp(hword),
        0b01_0000 => id_data_processing(hword),
        0b01_0001 => id_special_data_branch(hword),
        0b01_0010..=0b01_0011 => {
            let rt = ((hword >> 8) & 0b111) as u8;
            let address = word_align(pc) + ((hword as u32 & 0xFF) << 2);
            Instruction::LdrLit { rt, address } // A7.7.44 T1
        }
        0b01_0100..=0b01_0111 |
        0b01_1000..=0b01_1111 |
        0b10_0000..=0b10_0111 => id_ldr_str_single(hword),
        0b10_1000..=0b10_1001 => {
            let rd = ((hword >> 8) & 0b111) as u8;
            let imm32 = ((hword & 0xFF) << 2) as u32;
            let address = word_align(pc) + imm32;
            Instruction::Adr {rd, address} // A7.7.7 T1
        }
        0b10_1010..=0b10_1011 => {
            let imm32 = ((hword & 0xFF) << 2) as u32;
            let rd = ((hword >> 8) & 0b111) as u8;
            Instruction::AddSpImm {rd, imm32, setflags: false} // A7.7.5 T1
        }
        0b10_1100..=0b10_1111 => id_misc(hword),
        0b11_0000..=0b11_0001 => {
            let registers = (hword & 0xFF) as u16;
            if registers == 0 {
                Instruction::Unpredictable
            } else {
                Instruction::Stm {rn: ((hword >> 8) & 0b111) as u8, registers, wback: true} // A7.7.159 T1
            }
        }
        0b11_0010..=0b11_0011 => {
            let rn = ((hword >> 8) & 0b111) as u8;
            let registers = (hword & 0xFF) as u16;
            if registers == 0 {
                Instruction::Unpredictable
            } else {
                Instruction::Ldm {rn, registers, wback: !bitset(registers, rn.into())} // A7.7.41 T1
            }
        }
        0b11_0100..=0b11_0111 => id_conditional_branch_supc(hword, pc),
        0b11_1000..=0b11_1001 => {
            let offset = sign_extend(((hword & 0x7FF) << 1) as u32, 11) as u32;
            let address = pc.wrapping_add(offset);
            Instruction::Branch { address } // A7.7.12 T2
        }
        _ => panic!(),
    };
}

fn id_shift_add_sub_move_cmp(hword: u16) -> Instruction {
    // A5.2.1
    assert!(matches(hword, 14, 0b11, 0b00));

    let op1 = hword >> 12;
    let rd = ((hword >> 8) & 0b111) as u8;
    let imm32 = (hword & 0xFF) as u32;

    return match op1 {
        0b00 => {
            let rd = (hword & 0b111) as u8;
            let rm = ((hword >> 3) & 0b111) as u8;
            let shift_n = ((hword >> 6) & 0x1F) as u32;
            if bitset(hword, 11) {
                // A7.7.70 T1
                Instruction::LsrImm {rd, rm, shift: Shift {shift_t: ShiftType::LSR, shift_n}}
            } else if (hword >> 6) == 0 {
                // A7.7.77 T2
                Instruction::MovReg {rd, rm, setflags: true}
            } else {
                // A7.7.68 T1
                Instruction::LslImm {rd, rm, shift: Shift {shift_t: ShiftType::LSL, shift_n}}
            }
        }
        0b01 => {
            if bitset(hword, 11) {
                id_add_sub(hword)
            } else {
                let rd = (hword & 0b111) as u8;
                let rm = ((hword >> 3) & 0b111) as u8;
                let shift_n = ((hword >> 6) & 0x1F) as u32;
                // A7.7.10 T1
                Instruction::AsrImm {rd, rm, shift: Shift {shift_t: ShiftType::ASR, shift_n}, setflags: true}
            }
        }
        0b10 => {
            if bitset(hword, 11) {
                // A7.7.27 T1
                Instruction::CmpImm {rn: rd, imm32}
            } else {
                // A7.7.76 T1
                Instruction::MovImm {rd, imm32, setflags: true, carry: CarryChange::Same}
            }
        }
        0b11 => {
            if !bitset(hword, 11) {
                // A7.7.3 T2
                Instruction::AddImm {rd, rn: rd, imm32, setflags: true}
            } else {
                // A7.7.175 T2
                Instruction::SubImm {rd, rn: rd, imm32, setflags: true}
            }
        }
        _ => panic!(),
    }
}

fn id_add_sub(hword: u16) -> Instruction {
    // A5.2.1
    assert!(matches(hword, 11, 0b1111_1, 0b0001_1));
    let rm = ((hword >> 6) & 0b111) as u8;
    let rn = ((hword >> 3) & 0b111) as u8;
    let rd = (hword & 0b111) as u8;
    let shift = Shift {shift_t: ShiftType::LSL, shift_n: 0};
    let imm32 = ((hword >> 6) & 0b111) as u32;
    return match (hword >> 9) & 0b11 {
        0b00 => Instruction::AddReg {rd, rm, rn, shift, setflags: true}, // A7.7.4 T1
        0b01 => Instruction::SubReg {rd, rn, rm, shift, setflags: true}, // A7.7.175 T1
        0b10 => Instruction::AddImm {rd, rn, imm32, setflags: true}, // A7.7.3 T1
        0b11 => Instruction::SubImm {rd, rn, imm32, setflags: true}, // A7.7.174 T1
        _ => panic!(),
    };
}

fn id_data_processing(hword: u16) -> Instruction {
    // A5.2.2
    assert!(matches(hword, 10, 0b1111_11, 0b0100_00));
    let rd = (hword & 0b111) as u8;
    let rm = ((hword >> 3) & 0b111) as u8;
    let shift = Shift {shift_t: ShiftType::LSL, shift_n: 0};
    return match (hword >> 6) & 0b1111 {
        0b0000 => Instruction::AndReg {rd, rm, rn: rd, shift, setflags: true}, // A7.7.9 T1
        0b0001 => Instruction::EorReg {rd, rm, rn: rd, shift, setflags: true}, // A7.7.36 T1
        0b0010 => Instruction::LslReg {rd, rm, rn: rd, shift, setflags: true}, // A7.7.69 T1
        0b0011 => Instruction::LsrReg {rd, rm, rn: rd, shift, setflags: true}, // A7.7.71 T1
        0b0100 => Instruction::AsrReg {rd, rm, rn: rd, shift, setflags: true}, // A7.7.11 T1
        0b0101 => Instruction::AdcReg {rd, rm, rn: rd, shift, setflags: true}, // A7.7.2 T1
        0b0110 => Instruction::SbcReg {rd, rm, rn: rd, shift, setflags: true}, // A7.7.125 T1
        0b0111 => Instruction::RorReg {rd, rm, rn: rd, shift, setflags: true}, // A7.7.117 T1
        0b1000 => Instruction::TstReg {rm, rn: rd, shift}, // A7.7.189 T1
        0b1001 => Instruction::RsbImm {rd, rn: rm, imm32: 0, setflags: true}, // A7.7.119 T1
        0b1010 => Instruction::CmpReg {rm, rn: rd, shift}, // A7.7.28 T1
        0b1011 => Instruction::CmnReg {rm, rn: rd, shift}, // A7.7.26 T1
        0b1100 => Instruction::OrrReg {rd, rm, rn: rd, shift, setflags: true}, // A7.7.92 T1
        0b1101 => Instruction::Mul {rd, rm: rd, rn: rm, setflags: true}, // A7.7.84 T1
        0b1110 => Instruction::BicReg {rd, rm, rn: rd, shift, setflags: true}, // A7.7.16 T1
        0b1111 => Instruction::MvnReg {rd, rm, shift, setflags: true}, // A7.7.86 T1
        _ => panic!(),
    };
}

fn id_special_data_branch(hword: u16) -> Instruction {
    // A5.2.3
    assert!(matches(hword, 10, 0b1111_11, 0b0100_01));
    let rm = ((hword >> 3) & 0b1111) as u8;
    let rn = (((hword >> 4) & (1 << 3)) + (hword & 0b111)) as u8;
    let shift = Shift {shift_t: ShiftType::LSL, shift_n: 0};
    return match (hword >> 7) & 0b111 {
        0b000 | 0b001 => {
            if rn == 15 && rm == 15 {
                Instruction::Unpredictable
            } else {
                Instruction::AddReg {rd: rn, rm, rn, shift, setflags: false} // A7.7.4 T2
            }
        }
        0b010 => {
            if (rn < 8 && rm < 8) || rn == 15 || rm == 15 {
                Instruction::Unpredictable
            } else {
                Instruction::CmpReg {rm, rn, shift} // A7.7.28 T2
            }
        }
        0b100 | 0b101 => Instruction::MovReg {rd: rn, rm, setflags: false}, // A7.7.77 T1
        0b110 => Instruction::BranchExchange {rm}, // A7.7.20 T1
        0b111 => Instruction::Blx {rm}, // A7.7.19 T1
        _ => panic!(),
    };
}

fn id_ldr_str_single(hword: u16) -> Instruction {
    // A5.2.4
    let op_a = hword >> 12;
    assert!(op_a == 0b0101 || op_a == 0b0110 || op_a == 0b0111 || op_a == 0b1000 || op_a == 0b1001);
    let op_b = (hword >> 9) & 0b111;
    let op_c = bitset(hword, 11);

    let rm = ((hword >> 6) & 0b111) as u8;
    let rn = ((hword >> 3) & 0b111) as u8;
    let rt = (hword & 0b111) as u8;

    let shift = Shift {shift_t: ShiftType::LSL, shift_n: 0};

    return match op_a {
        0b0101 => match op_b {
            0b000 => Instruction::StrReg {rt, rm, rn, shift}, // A7.7.162 T1
            0b001 => Instruction::StrhReg {rt, rm, rn, shift}, // A7.7.171 T1
            0b010 => Instruction::StrbReg {rt, rm, rn, shift}, // A7.7.164 T1
            0b011 => Instruction::LdrsbReg {rt, rm, rn, shift}, // A7.7.61 T1
            0b100 => Instruction::LdrReg {rt, rm, rn, shift}, // A7.7.45 T1
            0b101 => Instruction::LdrhReg {rt, rm, rn, shift}, // A7.7.57 T1
            0b110 => Instruction::LdrbReg {rt, rm, rn, shift}, // A7.7.48 T1
            0b111 => Instruction::LdrshReg {rt, rm, rn, shift}, // A7.7.48 T1
            _ => panic!(),
        },
        0b0110 => {
            let offset = ((hword >> 4) & (0b1_1111 << 2)) as i32;
            if op_c {
                Instruction::LdrImm {rn, rt, offset, index: true, wback: false} // A7.7.43 T1
            } else {
                Instruction::StrImm {rn, rt, offset, index: true, wback: false} // A7.7.161 T1
            }
        }
        0b0111 => {
            let offset = ((hword >> 6) & 0b1_1111) as i32;
            if op_c {
                Instruction::LdrbImm {rt, rn, offset, index: true, wback: true} // A7.7.46 T1
            } else {
                Instruction::StrbImm {rt, rn, offset, index: true, wback: true} // A7.7.163 T1
            }
        }
        0b1000 => {
            let offset = ((hword >> 5) & (0b1_1111 << 1)) as i32;
            if op_c {
                Instruction::LdrhImm {rt, rn, offset, index: true, wback: true} // A7.7.55 T1
            } else {
                Instruction::StrhImm {rt, rn, offset, index: true, wback: true} // A7.7.170 T1
            }
        }
        0b1001 => {
            let rt = ((hword >> 8) & 0b111) as u8;
            let offset = ((hword & 0xFF) << 2) as i32;
            if op_c {
                Instruction::LdrImm {rt, rn: 13, offset, index: true, wback: true} // A7.7.43 T2
            } else {
                Instruction::StrImm {rt, rn: 13, offset, index: true, wback: true} // A7.7.161 T2
            }
        }
        _ => panic!(),
    };
}

fn id_misc(hword: u16) -> Instruction {
    // A5.2.5
    assert!(matches(hword, 12, 0b1111, 0b1011));
    return match (hword >> 5) & 0x7F {
        0b011_0011 => {
            if (hword & 0b11) == 0 {
                Instruction::Unpredictable
            } else {
                Instruction::Cps {enable: bitset(hword, 4), affect_pri: bitset(hword, 1), affect_fault: bitset(hword, 0)} // B5.2.1 T1
            }
        }
        0b000_0000..=0b000_0011 => Instruction::AddSpImm {rd: 13, imm32: ((hword & 0x3F) << 2) as u32, setflags: false}, // A7.7.5 T2
        0b000_0100..=0b000_0111 => Instruction::SubSpImm {rd: 13, imm32: ((hword & 0x3F) << 2) as u32, setflags: false}, // A7.7.176 T1
        0b000_1000..=0b000_1111 |
        0b001_1000..=0b001_1111 |
        0b100_1000..=0b100_1111 |
        0b101_1000..=0b101_1111 => {
            let rn = (hword & 0b111) as u8;
            let imm32 = (((hword & (0x1F << 3)) >> 2) + ((hword & 0b1 << 9) >> 3)) as u32;
            Instruction::Cbz {rn, imm32, nonzero: bitset(hword, 11)} // A7.7.21 T1
        }
        0b001_0000..=0b001_0001 => Instruction::Sxth {rd: (hword & 0b111) as u8, rm: ((hword >> 3) & 0b111) as u8, rotation: 0}, // A7.7.184 T1
        0b001_0010..=0b001_0011 => Instruction::Sxtb {rd: (hword & 0b111) as u8, rm: ((hword >> 3) & 0b111) as u8, rotation: 0}, // A7.7.182 T1
        0b001_0100..=0b001_0101 => Instruction::Uxth {rd: (hword & 0b111) as u8, rm: ((hword >> 3) & 0b111) as u8, rotation: 0}, // A7.7.223 T1
        0b001_0110..=0b001_0111 => Instruction::Uxtb {rd: (hword & 0b111) as u8, rm: ((hword >> 3) & 0b111) as u8, rotation: 0}, // A7.7.221 T1
        0b010_0000..=0b010_1111 => Instruction::Push {registers: ((hword << 6) & (0b1 << 14)) + (hword & 0xFF)}, // A7.7.101 T1
        0b101_0000..=0b101_0001 => Instruction::Rev {rd: (hword & 0b111) as u8, rm: ((hword >> 3) & 0b111) as u8}, // A7.7.113 T1
        0b101_0010..=0b101_0011 => Instruction::Rev16 {rd: (hword & 0b111) as u8, rm: ((hword >> 3) & 0b111) as u8}, // A7.7.114 T1
        0b101_0110..=0b101_0111 => Instruction::Revsh {rd: (hword & 0b111) as u8, rm: ((hword >> 3) & 0b111) as u8}, // A7.7.115 T1
        0b110_0000..=0b110_1111 => Instruction::Pop {registers: ((hword << 7) & (0b1 << 15)) + (hword & 0xFF)}, // A7.7.99 T1
        0b111_0000..=0b111_0111 => Instruction::Bkpt {imm8: (hword & 0xFF) as u8}, // A7.7.17 T1
        0b111_1000..=0b111_1111 => id_if_then_hints(hword),
        _ => Instruction::Undefined, // intentional
    };
}

fn id_if_then_hints(hword: u16) -> Instruction {
    // A5.2.5
    assert!(matches(hword, 8, 0b1111_1111, 0b1011_1111));
    let op_a = (hword >> 4) & 0xF;
    let op_b = hword & 0xF;

    return if op_b != 0 {
        Instruction::It {firstcond: op_a as u8, mask: op_b as u8} // A7.7.38 T1
    } else {
        match op_a {
            0b0000 => Instruction::Nop, // A7.7.88 T1
            0b0001 => Instruction::Yield, // A7.7.263 T1
            0b0010 => Instruction::Wfe, // A7.7.261 T1
            0b0011 => Instruction::Wfi, // A7.7.262 T1
            0b0100 => Instruction::Sev, // A7.7.129 T1
            _ => Instruction::Nop,
        }
    }
}

fn id_conditional_branch_supc(hword: u16, pc: u32) -> Instruction {
    // A5.2.6
    assert!(matches(hword, 12, 0b1111, 0b1101));
    let op = (hword >> 8) & 0b1111;
    return match op {
        0b1110 => Instruction::Udf {imm32: (hword & 0xFF) as u32}, // A7.7.194 T1
        0b1111 => Instruction::Svc {imm8: (hword & 0xFF) as u8}, // A7.7.178 T1
        _ => {
            let imm32 = sign_extend(((hword & 0xFF) << 1) as u32, 8) as u32;
            let address = pc.wrapping_add(imm32);
            let cond = Condition::new(((hword >> 8) & 0b1111) as u32);
            Instruction::CondBranch { address, cond } // A7.7.12 T1
        }
    };
}
