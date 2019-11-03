#[repr(u8)]
pub enum Opcode {
    Unimplemented, // N: orginal thumb[16], W: blank[16] - original thumb[32]
    AdcImm,
    AdcReg, // N: blank[10]-rm[3]-rdn[3]
    AddImm, // N: blank[2]-rn[3]-rd[3]-imm8[8]
    AddReg, // N: blank[3]-setflags[1]-rn[4]-rm[4]-rd[4]
    AddSpImm, // N: blank[2]-rd[4]-offset[10]
    AddSpReg,
    Adr,    // N: blank[3]-rd[3]-offset[10]
    AsrReg, // N: blank[10]-rm[3]-rdn[3]
    AndImm,
    AndReg, // N: blank[10]-rm[3]-rdn[3]
    Branch, // N: blank[5]-imm11[11]
    BranchCond,
    BicReg, // N: blank[10]-rm[3]-rdn[3]
    Blx,     // N: blank[12]-rm[4]
    Bx,     // N: blank[12]-rm[4]
    CmnReg, // N: blank[12]-rm[3]-rn[3]
    CmpImm, // N: blank[5]-rn[3]-imm8[8]
    CmpReg, // N: blank[10]-rm[4]-rn[4]
    EorReg, // N: blank[10]-rm[3]-rdn[3]
    Ldm,    // N: blank[5]-rt[3]-registers[8]
    LdrLit, // N: blank[3]-rt[3]-offset[10]
    LdrImm, // N: rn[4]-rt[4]-imm8[8]
    LdrReg, // N: blank[7]-rm[3]-rn[3]-rt[3]
    LdrbImm, // N: blank[5]-imm5[5]-rn[3]-rt[3]
    LdrbReg, // N: blank[7]-rm[3]-rn[3]-rt[3]
    LdrhReg, // N: blank[7]-rm[3]-rn[3]-rt[3]
    LdrsbReg, // N: blank[7]-rm[3]-rn[3]-rt[3]
    LdrshReg, // N: blank[7]-rm[3]-rn[3]-rt[3]
    LslImm, // N: blank[5]-shift[5]-rm[3]-rd[3]
    LslReg, // N: blank[10]-rm[3]-rdn[3]
    LsrReg, // N: blank[10]-rm[3]-rdn[3]
    MovImm, // N: blank[5]-rd[3]-imm8[8]
    MovReg, // N: blank[7]-setflags[1]-rn[4]-rd[4]
    MulReg, // N: blank[10]-rm[3]-rdn[3]
    MvnReg, // N: blank[12]-rm[3]-rd[3]
    OrrReg, // N: blank[10]-rm[3]-rdn[3]
    RorReg, // N: blank[10]-rm[3]-rdn[3]
    RsbImm, // N: blank[12]-rn[3]-rd[3]
    SbcReg, // N: blank[10]-rm[3]-rdn[3]
    Stm,    // N: blank[5]-rt[3]-registers[8]
    StrImm, // N: rn[4]-rt[4]-imm8[8]
    StrReg, // N: blank[7]-rm[3]-rn[3]-rt[3]
    StrbImm, // N: blank[5]-imm5[5]-rn[3]-rt[3]
    StrbReg, // N: blank[7]-rm[3]-rn[3]-rt[3]
    StrhReg, // N: blank[7]-rm[3]-rn[3]-rt[3]
    SubReg, // N: blank[7]-rm[3]-rn[3]-rd[3]
    TstReg, // N: blank[12]-rm[3]-rn[3]
    Other,
    // etc.
}

pub fn to_opcode(bits: u8) -> Opcode {
    return unsafe { std::mem::transmute::<u8, Opcode>(bits) };
}

pub fn from_opcode(opcode: Opcode) -> u8 {
    return opcode as u8;
}
