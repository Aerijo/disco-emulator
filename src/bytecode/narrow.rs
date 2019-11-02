use crate::{ByteInstruction};
use crate::bytecode::{Tag, Opcode};

type Context = InstructionContext;

pub fn decode_thumb_narrow(hword: u32, context: Context) -> ByteInstruction {
    assert!((hword >> 16) == 0);
    return (id_narrow_thumb(hword, context), 0);
}

fn id_narrow_thumb(hword: u32, c: Context) -> u32 {
    // A5.2
    return match hword >> 10 {
        0b00_0000..=0b00_1111 => id_shift_add_sub_move_cmp(hword, c),
        0b01_0000             => id_data_processing(hword),
        0b01_0001             => id_special_data_branch(hword),
        0b01_0010..=0b01_0011 => {
            return Tag::get_narrow(Opcode::LdrLit, c) + ((hword & 0x7FF) << 2); // A7.7.44 T1
        }
        0b01_0100..=0b01_0111 |
        0b01_1000..=0b01_1111 |
        0b10_0000..=0b10_0111 => id_ldr_str_single(hword),
        0b10_1000..=0b10_1001 => {
            return Tag::get_narrow(Opcode::Adr, c) + (hword & 0x7FF); // A7.7.7 T1
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

fn id_shift_add_sub_move_cmp(hword: u16, c: Context) -> u32 {
    panic!();
}

fn id_data_processing(hword: u16, c: Context) -> u32 {
    panic!();
}

fn id_special_data_branch(hword: u16, c: Context) -> u32 {
    panic!();
}

fn id_ldr_str_single(hword: u16, c: Context) -> u32 {
    panic!();
}

fn id_misc(hword: u16, c: Context) -> u32 {
    panic!();
}

fn id_conditional_branch_supc(hword: u16, c: Context) -> u32 {
    panic!();
}
