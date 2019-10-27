#![allow(dead_code)]
#![allow(unused_variables)]

use crate::Shift;
use crate::utils::bits::{bitset};

mod narrow;
use narrow::{get_narrow_instruction};

mod wide;
use wide::{get_wide_instruction};

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
    AdcImm {rd: u8, rn: u8, imm32: u32, setflags: bool},
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
    BicImm {rd: u8, rn: u8, imm32: u32, setflags: bool, carry: CarryChange},
    BicReg {rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool},
    Bkpt {imm8: u8},
    LinkedBranch {address: u32},
    Blx {rm: u8},
    BranchExchange {rm: u8},
    Cbz {rn: u8, imm32: u32, nonzero: bool},
    CmnImm {rn: u8, imm32: u32},
    CmnReg {rm: u8, rn: u8, shift: Shift},
    CmpImm {rn: u8, imm32: u32},
    CmpReg {rm: u8, rn: u8, shift: Shift},
    Cps {enable: bool, affect_pri: bool, affect_fault: bool},
    EorImm {rd: u8, rn: u8, imm32: u32, setflags: bool, carry: CarryChange},
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
    Ldrex {rn: u8, rt: u8, imm32: u32},
    Ldrexb {rn: u8, rt: u8},
    Ldrexh {rn: u8, rt: u8},
    LdrdImm {rn: u8, rt: u8, rt2: u8, offset: i32, index: bool, wback: bool},
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
    MvnImm {rd: u8, imm32: u32, setflags: bool, carry: CarryChange},
    MvnReg {rd: u8, rm: u8, shift: Shift, setflags: bool},
    Nop,
    OrnImm {rd: u8, rn: u8, imm32: u32, setflags: bool, carry: CarryChange},
    OrrImm {rd: u8, rn: u8, imm32: u32, setflags: bool, carry: CarryChange},
    OrrReg {rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool},
    Pop {registers: u16},
    Push {registers: u16},
    Rev {rd: u8, rm: u8},
    Rev16 {rd: u8, rm: u8},
    Revsh {rd: u8, rm: u8},
    RorReg {rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool},
    RsbImm {rd: u8, rn: u8, imm32: u32, setflags: bool},
    SbcImm {rd: u8, rn: u8, imm32: u32, setflags: bool},
    SbcReg {rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool},
    Sev,
    Stm {rn: u8, registers: u16, wback: bool},
    Stmdb {rn: u8, registers: u16, wback: bool},
    StrImm {rn: u8, rt: u8, offset: i32, index: bool, wback: bool},
    StrbImm {rn: u8, rt: u8, offset: i32, index: bool, wback: bool},
    StrdImm {rn: u8, rt: u8, rt2: u8, offset: i32, index: bool, wback: bool},
    StrhImm {rn: u8, rt: u8, offset: i32, index: bool, wback: bool},
    StrReg {rn: u8, rt: u8, rm: u8, shift: Shift},
    StrbReg {rn: u8, rt: u8, rm: u8, shift: Shift},
    Strex {rn: u8, rt: u8, rd: u8, imm32: u32},
    Strexb {rn: u8, rt: u8, rd: u8},
    Strexh {rn: u8, rt: u8, rd: u8},
    StrhReg {rn: u8, rt: u8, rm: u8, shift: Shift},
    SubImm {rd: u8, rn: u8, imm32: u32, setflags: bool},
    SubReg {rd: u8, rm: u8, rn: u8, shift: Shift, setflags: bool},
    SubSpImm {rd: u8, imm32: u32, setflags: bool},
    Svc {imm8: u8},
    Sxtb {rd: u8, rm: u8, rotation: u32},
    Sxth {rd: u8, rm: u8, rotation: u32},
    Tbb {rn: u8, rm: u8},
    Tbh {rn: u8, rm: u8},
    TeqImm {rn: u8, imm32: u32, carry: CarryChange},
    TstImm {rn: u8, imm32: u32, carry: CarryChange},
    TstReg {rn: u8, rm: u8, shift: Shift},
    Udf {imm32: u32},
    Uxtb {rd: u8, rm: u8, rotation: u32},
    Uxth {rd: u8, rm: u8, rotation: u32},
    Wfe,
    Wfi,
    Yield,

    Undefined,
    Unpredictable, // Can execute / treat as UNDEFINED

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
