#[repr(u16)]
pub enum Opcode {
    Undefined = 0x00, // N: orginal thumb[16], W: blank[16] - original thumb[32]
    AdcImm = 0x01,
    AdcReg = 0x02,
    AddImm = 0x03,
    AddReg = 0x04,
    AddSpImm = 0x05,
    AddSpReg = 0x06,
    Adr = 0x07,
    AndImm = 0x08,
    AndReg = 0x09,
    LdrLit = 0x0A, // N: blank[5]-rt[3]-offset[8]
    Other = 0xFF,
    // etc.
}

pub fn to_opcode(instruction_tag: u16) -> Opcode {
    return unsafe { std::mem::transmute::<u16, Opcode>(instruction_tag & 0x1FF) };
}

pub fn from_opcode(opcode: Opcode) -> u16 {
    return opcode as u16;
}
