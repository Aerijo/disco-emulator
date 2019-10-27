#![allow(dead_code)]
#![allow(unused_variables)]

use crate::Shift;

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
    fn from<T: Into<u32>>(code: T) -> Condition {
        let code = code.into();
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
            _ => panic!(),
        };
    }
}

#[derive(Copy, Clone, Debug)]
pub enum CarryChange {
    Same,
    Set,
    Clear,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ShiftType {
    LSL,
    LSR,
    ASR,
    ROR,
    RRX,
}

#[derive(Debug)]
pub enum Instruction {
    AdcReg {rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool},
    AddImm {rd: u8, rn: u8, imm32: u32, setflags: bool},
    AddReg {rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool},
    AddSpImm {rd: u8, imm32: u32, setflags: bool},
    Adr {rd: u8, address: u32},
    AndImm {rd: u8, rn: u8, imm32: u32, setflags: bool, carry: CarryChange},
    AndReg {rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool},
    AsrImm {rd: u8, rm: u8, shift: Shift, setflags: bool},
    AsrReg {rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool},
    Branch {address: u32},
    CondBranch {address: u32, cond: Condition},
    BicReg {rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool},
    Bkpt {imm8: u8},
    LinkedBranch {address: u32},
    Blx {rm: u8},
    BranchExchange {rm: u8},
    Cbz {rn: u8, imm32: u32, nonzero: bool},
    CmnReg {rm: u8, rn: u8, shift: Shift},
    CmpImm {rn: u8, imm32: u32},
    CmpReg {rm: u8, rn: u8, shift: Shift},
    Cps {enable: bool, affect_pri: bool, affect_fault: bool},
    EorReg {rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool},
    It {firstcond: u8, mask: u8},
    LslImm {rd: u8, rm: u8, shift: Shift},
    LslReg {rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool},
    LsrImm {rd: u8, rm: u8, shift: Shift},
    LsrReg {rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool},
    Ldm {rn: u8, registers: u16, wback: bool},
    Ldmdb {rn: u8, registers: u16, wback: bool},
    LdrImm {rn: u8, rt: u8, offset: i32, index: bool, wback: bool},
    LdrbImm {rn: u8, rt: u8, offset: i32, index: bool, wback: bool},
    LdrhImm {rn: u8, rt: u8, offset: i32, index: bool, wback: bool},
    LdrLit {rt: u8, address: u32},
    LdrReg {rn: u8, rt: u8, rm: u8, shift: Shift},
    LdrbReg {rn: u8, rt: u8, rm: u8, shift: Shift},
    LdrhReg {rn: u8, rt: u8, rm: u8, shift: Shift},
    LdrsbReg {rn: u8, rt: u8, rm: u8, shift: Shift},
    LdrshReg {rn: u8, rt: u8, rm: u8, shift: Shift},
    Ldrt {rn: u8, rt: u8, offset: i32},
    MovImm {rd: u8, imm32: u32, setflags: bool, carry: CarryChange},
    MovReg {rd: u8, rm: u8, setflags: bool},
    Mul {rd: u8, rn: u8, rm: u8, setflags: bool},
    MvnReg {rd: u8, rm: u8, shift: Shift, setflags: bool},
    Nop,
    OrrReg {rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool},
    Pop {registers: u16},
    Push {registers: u16},
    Rev {rd: u8, rm: u8},
    Rev16 {rd: u8, rm: u8},
    Revsh {rd: u8, rm: u8},
    RorReg {rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool},
    RsbImm {rd: u8, rn: u8, imm32: u32, setflags: bool},
    SbcReg {rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool},
    Sev,
    Stm {rn: u8, registers: u16, wback: bool},
    Stmdb {rn: u8, registers: u16, wback: bool},
    StrImm {rn: u8, rt: u8, offset: i32, index: bool, wback: bool},
    StrbImm {rn: u8, rt: u8, offset: i32, index: bool, wback: bool},
    StrhImm {rn: u8, rt: u8, offset: i32, index: bool, wback: bool},
    StrReg {rn: u8, rt: u8, rm: u8, shift: Shift},
    StrbReg {rn: u8, rt: u8, rm: u8, shift: Shift},
    StrhReg {rn: u8, rt: u8, rm: u8, shift: Shift},
    SubImm {rd: u8, rn: u8, imm32: u32, setflags: bool},
    SubReg {rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool},
    SubSpImm {rd: u8, imm32: u32, setflags: bool},
    Svc {imm8: u8},
    Sxtb {rd: u8, rm: u8, rotation: u32},
    Sxth {rd: u8, rm: u8, rotation: u32},
    TstReg {rn: u8, rm: u8, shift: Shift},
    Udf {imm32: u32},
    Uxtb {rd: u8, rm: u8, rotation: u32},
    Uxth {rd: u8, rm: u8, rotation: u32},
    Wfe,
    Wfi,
    Yield,

    Undefined,
    Unpredictable,

    Unimplemented,
}

impl Instruction {
    pub fn from(word: u32, pc: u32) -> (Instruction, bool) {
        return if (word >> 29 == 0b111) && (word >> 27 != 0b11100) {
            (get_wide_instruction(word, pc), true)
        } else {
            (get_narrow_instruction((word >> 16) as u16, pc), false)
        };
    }
}

pub fn bitset<T: Into<u32>>(word: T, bit: T) -> bool {
    let word = word.into();
    let bit = bit.into();
    return (word & (1 << bit)) > 0;
}

pub fn align(address: u32, size: u32) -> u32 {
    assert!(size == 1 || size == 2 || size == 4);
    return address & !(size - 1);
}

fn word_align(address: u32) -> u32 {
    return align(address, 4);
}

fn sign_extend(value: u32, from: u32) -> i32 {
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
fn thumb_expand_imm_c(input: u32) -> (u32, CarryChange) {
    // p137
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
            0b01 => (base << 16) + base,                              // 0X0X
            0b10 => (base << 24) + (base << 8),                       // X0X0
            0b11 => (base << 24) + (base << 16) + (base << 8) + base, // XXXX
            _ => panic!("Unexpected pattern"),
        };

        return (val, CarryChange::Same);
    }

    let unrotated_value = (1 << 7) | (input & 0xFF);
    return rotate_right_32_c(unrotated_value, input >> 7);
}

fn rotate_right_32_c(input: u32, shift: u32) -> (u32, CarryChange) {
    // p27
    assert!(shift != 0);
    let m = shift % 32;
    let result = (input >> m) | (input << (32 - m)); // TODO: Use u32.rotate_right
    let carry_out = if bitset(result, 31) {
        CarryChange::Set
    } else {
        CarryChange::Clear
    };
    return (result, carry_out);
}

fn ror_c(input: u32, shift: u32) -> (u32, bool) {
    let (result, change) = rotate_right_32_c(input, shift);
    return match change {
        CarryChange::Set => (result, true),
        CarryChange::Clear => (result, false),
        _ => panic!(),
    }
}

fn thumb_expand_imm(val: u32) -> u32 {
    // p137
    return thumb_expand_imm_c(val).0;
}

fn rrx_c(input: u32, carry_in: u32) -> (u32, bool) {
    // p27
    let result = (input >> 1) + (carry_in << 31);
    let carry_out = bitset(result, 0);
    return (result, carry_out);
}

fn rrx(input: u32, carry_in: u32) -> u32 {
    // p27
    return rrx_c(input, carry_in).0;
}

fn lsl_c(input: u32, shift: u32) -> (u32, bool) {
    // p26
    assert!(shift > 0 && shift <= 32);
    let result = input << shift;
    let carry_out = bitset(input, 32 - shift);
    return (result, carry_out);
}

fn lsr_c(input: u32, shift: u32) -> (u32, bool) {
    // p26
    assert!(shift > 0 && shift <= 32);
    let result = input >> shift;
    let carry_out = bitset(input, shift - 1);
    return (result, carry_out);
}

fn asr_c(input: u32, shift: u32) -> (u32, bool) {
    // p27
    assert!(shift > 0 && shift <= 32);
    let result = ((input as i32) >> shift) as u32;
    let carry_out = bitset(input, shift - 1);
    return (result, carry_out);
}

pub fn shift_c(input: u32, s: Shift, carry_in: u32) -> (u32, bool) {
    // p181
    assert!(!(s.shift_t == ShiftType::RRX && s.shift_n != 1));
    if s.shift_n == 0 {
        return (input, carry_in == 1);
    }

    return match s.shift_t {
        ShiftType::LSL => lsl_c(input, s.shift_n),
        ShiftType::LSR => lsr_c(input, s.shift_n),
        ShiftType::ASR => asr_c(input, s.shift_n),
        ShiftType::ROR => ror_c(input, s.shift_n),
        ShiftType::RRX => rrx_c(input, carry_in),
    }
}

pub fn shift(input: u32, s: Shift, carry_in: u32) -> u32 {
    // p181
    return shift_c(input, s, carry_in).0;
}

pub fn add_with_carry(x: u32, y: u32, carry_in: u32) -> (u32, bool, bool) {
    // p28
    let unsigned_sum = (x as u64) + (y as u64) + (carry_in as u64);
    let result = unsigned_sum & 0xFFFF_FFFF;
    let carry_out = result != unsigned_sum;

    let x_neg = bitset(x, 31);
    let y_neg = bitset(y, 31);
    let result_neg = bitset(result as u32, 31);
    let overflow = (x_neg == y_neg) && (x_neg != result_neg);

    return (result as u32, carry_out, overflow);
}

fn decode_imm_shift(encoded: u16) -> Shift {
    // A7.4.2
    let shift_n = (encoded & 0x1F) as u32;
    return match encoded >> 5 {
        0b00 => Shift {shift_t: ShiftType::LSL, shift_n},
        0b01 => Shift {shift_t: ShiftType::LSR, shift_n},
        0b10 => Shift {shift_t: ShiftType::ASR, shift_n},
        0b11 if shift_n == 0 => Shift {shift_t: ShiftType::RRX, shift_n: 1},
        0b11 if shift_n != 0 => Shift {shift_t: ShiftType::ROR, shift_n},
        _ => panic!(),
    }
}

fn get_narrow_instruction(hword: u16, pc: u32) -> Instruction {
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
                Instruction::Stm {rn, registers, wback: !bitset(registers, rn.into())} // A7.7.41 T1
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

fn id_ldr_str_single(hword: u16) -> Instruction {
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
    assert!((hword & (0b1111 << 12)) == 0b1011 << 12);

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
        0b110_0000..=0b110_1111 => Instruction::Pop {registers: ((hword << 6) & (0b1 << 15)) + (hword & 0xFF)}, // A7.7.99 T1
        0b111_0000..=0b111_0111 => Instruction::Bkpt {imm8: (hword & 0xFF) as u8}, // A7.7.17 T1
        0b111_1000..=0b111_1111 => id_if_then_hints(hword),
        _ => Instruction::Undefined, // intentional
    };
}

fn id_if_then_hints(hword: u16) -> Instruction {
    assert!((hword & (0b1111_1111 << 8)) == (0b1011_1111 << 8));
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
    assert!((hword & (0b1111 << 12)) == (0b1101 << 12));
    let op = (hword >> 8) & 0b1111;
    return match op {
        0b1110 => Instruction::Udf {imm32: (hword & 0xFF) as u32}, // A7.7.194 T1
        0b1111 => Instruction::Svc {imm8: (hword & 0xFF) as u8}, // A7.7.178 T1
        _ => {
            let imm32 = sign_extend(((hword & 0xFF) << 1) as u32, 8) as u32;
            let address = pc.wrapping_add(imm32);
            let cond = Condition::from((hword >> 8) & 0b1111);
            Instruction::CondBranch { address, cond } // A7.7.12 T1
        }
    };
}

fn id_shift_add_sub_move_cmp(hword: u16) -> Instruction {
    // A5.2.1
    assert!((hword & (0b11 << 14)) == 0b00 << 14);

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
    assert!(hword & (0b1111_1 << 11) == (0b0001_1 << 11));
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
    assert!(hword & (0b1111_11 << 10) == (0b0100_00 << 10));
    let op = (hword >> 6) & 0b1111;

    let rd = (hword & 0b111) as u8;
    let rm = ((hword >> 3) & 0b111) as u8;
    let shift = Shift {shift_t: ShiftType::LSL, shift_n: 0};

    return match op {
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
    assert!(hword & (0b1111_11 << 10) == (0b0100_01 << 10));
    let op = (hword >> 7) & 0b111;
    let rm = ((hword >> 3) & 0b1111) as u8;
    let rn = (((hword >> 4) & (1 << 3)) + (hword & 0b111)) as u8;
    let shift = Shift {shift_t: ShiftType::LSL, shift_n: 0};
    return match op {
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

fn get_wide_instruction(word: u32, pc: u32) -> Instruction {
    let op1 = (word >> 27) & 0b11;
    let op2 = (word >> 20) & 0b111_1111;
    let op3 = (word >> 15) & 0b1;

    return match op1 {
        0b01 => {
            if bitset(op2, 6) {
                return id_coprocessor_instr(word);
            }
            if bitset(op2, 5) {
                return id_data_processing_shifted(word);
            }
            if bitset(op2, 2) {
                return id_ldr_str_dual(word);
            }
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
        _ => panic!("Unexpected pattern"), // 0b00 would be a narrow instruction
    };
}

fn id_ldr_str_multiple(word: u32) -> Instruction {
    let op = (word >> 23) & 0b11;
    let l = ((word >> 20) & 0b1) == 1;
    let wback = ((word >> 21) & 0b1) == 1;
    let rn = ((word >> 16) & 0b1111) as u8;
    let registers = word as u16;

    return match op {
        0b01 if !l => Instruction::Stm {
            rn,
            registers,
            wback,
        },
        0b01 if l => {
            if wback && rn == 0b1101 {
                Instruction::Pop { registers }
            } else {
                Instruction::Ldm {
                    rn,
                    registers,
                    wback,
                }
            }
        }
        0b10 if l => Instruction::Ldmdb {
            rn,
            registers,
            wback,
        },
        0b10 if !l => {
            if wback && rn == 0b1101 {
                Instruction::Push { registers }
            } else {
                Instruction::Stmdb {
                    rn,
                    registers,
                    wback,
                }
            }
        }
        _ => Instruction::Undefined,
    };
}

fn id_ldr_str_dual(word: u32) -> Instruction {
    return Instruction::Undefined;
}

fn id_data_processing_shifted(word: u32) -> Instruction {
    // p148
    assert!((word >> 25) == 0b111_0101);
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
    return Instruction::Undefined;
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

fn id_data_proc_modified_immediate(word: u32) -> Instruction {
    assert!((word & (0b1111_1010_0000_0000_1 << 15)) == 0b1111_0000_0000_0000_0 << 15);

    let op = (word >> 21) & 0b1111;
    let rn = ((word >> 16) & 0b1111) as u8;
    let rn_is_pc = rn == 0b1111;
    let rd = ((word >> 8) & 0b1111) as u8;
    let rd_is_pc = rd == 0b1111;

    return match op {
        0b0000 if !rd_is_pc => {
            let imm12 = ((word >> 15) & (0b1 << 11)) + ((word >> 4) & (0b111 << 8)) + (word & 0xFF);
            let (imm32, carry) = thumb_expand_imm_c(imm12);
            Instruction::AndImm { rd, rn, imm32, setflags: bitset(word, 20), carry }
        }
        0b0010 if rn_is_pc => {
            let imm12 = ((word >> 15) & (0b1 << 11)) + ((word >> 4) & (0b111 << 8)) + (word & 0xFF);
            let (imm32, carry) = thumb_expand_imm_c(imm12);
            Instruction::MovImm {rd, imm32, setflags: bitset(word, 20), carry}
        }
        0b1000 if !rn_is_pc => {
            let imm12 = ((word >> 15) & (0b1 << 11)) + ((word >> 4) & (0b111 << 8)) + (word & 0xFF);
            let imm32 = thumb_expand_imm(imm12);
            Instruction::AddImm {rd, rn, imm32, setflags: bitset(word, 20)}
        }
        _ => Instruction::Unimplemented,
    };
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
