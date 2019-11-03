use crate::{ByteInstruction};
use crate::utils::bits::{bitset};
use super::opcode::{Opcode, from_opcode, to_opcode};
use super::{ItPos, InstructionContext};

enum Flags {
    HasValue = 1 << 31,
    InITorWideData = 1 << 30,
    Unpredictable = 1 << 29,
    ContextualUnpred = 1 << 28,
    Wide = 1 << 27,
}

/*
 * start[0]-inIT[1]-upredictable[2]-ITupredictable[3]-wide[4]-encoding[5-6]-opcode[7:15]
 *
 * Notes: Could reduce opcode to 256 range maybe by grouping similar instructions
 * first 4 bits should be 0 for valid stored value, and nonzero handled
 * appropriately:
 * - Not start: fetch, translate, and cache instruction
 * - In IT: throw branch-into-IT exception (discoboard actually allows this though, so it's recoverable)
 * - Unpredictable: Something about the instruction is wrong, such as invalid params or not last in IT, etc.
 * - ContextualUnpred: Upredictability is due to being in an IT block. Useful for when jumping into IT block (technically illegal),
 *   but so we know to ignore the normal unpredictable mark
 *
 * - When caching an IT instruction, clear an appropriate number of subsequent instructions (e.g., length * wides worth) to ensure we don't
 *   have incorrect caches. This only happens once (in a sane program) because we can trust the cache afterwards.
 *
 * - For 2nd half of wide instruction, first bit == 1 means it is not an instruction start, second bit == 1 means this instruction is the
 *   second half of some data. Without this, we could branching into wide instruction, replace this data half, and leave in invalid state
 */

pub fn from(cached: ByteInstruction) -> u32 {
    return cached.0;
}

pub fn get_wide(opcode: Opcode, context: InstructionContext, data: u32, extra: u32) -> ByteInstruction {
    assert!((data >> 16) == 0);
    return (get_start(opcode, context, data as u16, true), tag_extra(extra));
}

pub fn get_undefined_wide(context: InstructionContext, word: u32) -> ByteInstruction {
    return get_wide(Opcode::Undefined, context, 0, (word << 2) >> 2);
}

pub fn get_unpred_wide(opcode: Opcode, context: InstructionContext, data: u32, extra: u32) -> ByteInstruction {
    return as_unpred_w(get_wide(opcode, context, data, extra));
}

pub fn get_unpred_it_wide(opcode: Opcode, context: InstructionContext, data: u32, extra: u32) -> ByteInstruction {
    return as_unpred_it_w(get_wide(opcode, context, data, extra));
}

pub fn as_unpred_w(instr: ByteInstruction) -> ByteInstruction {
    return (as_unpred(instr.0), instr.1);
}

pub fn as_unpred_it_w(instr: ByteInstruction) -> ByteInstruction {
    return (as_unpred_it(instr.0), instr.1);
}

pub fn get_narrow(opcode: Opcode, context: InstructionContext, data: u16) -> u32 {
    return get_start(opcode, context, data, false);
}

pub fn get_unpred_narrow(opcode: Opcode, context: InstructionContext, data: u16) -> u32 {
    return as_unpred(get_narrow(opcode, context, data));
}

pub fn as_unpred(instr: u32) -> u32 {
    return instr | (Flags::Unpredictable as u32);
}

pub fn get_unpred_it_narrow(opcode: Opcode, context: InstructionContext, data: u16) -> u32 {
    return as_unpred_it(get_narrow(opcode, context, data));
}

pub fn as_unpred_it(instr: u32) -> u32 {
    return instr | (Flags::ContextualUnpred as u32);
}

pub fn as_wide(instr: u32) -> u32 {
    return instr | (Flags::Wide as u32)
}

fn get_start(opcode: Opcode, context: InstructionContext, data: u16, wide: bool) -> u32 {
    let mut base = get_blank(context);
    base |= (from_opcode(opcode) as u32) << 16;
    base |= data as u32;
    if wide {
        base = as_wide(base);
    }
    return base;
}

fn tag_extra(extra: u32) -> u32 {
    assert!((extra >> 30) == 0);
    return extra | (Flags::HasValue as u32 | Flags::InITorWideData as u32) << 16;
}

pub fn get_blank(context: InstructionContext) -> u32 {
    return match context.it_pos {
        ItPos::None => 0,
        ItPos::Within | ItPos::Last => Flags::InITorWideData as u32,
    };
}

pub fn reset_value() -> u32 {
    return Flags::HasValue as u32;
}

pub fn get_opcode(instr: u32) -> Opcode {
    assert!(has_cached(instr));
    return to_opcode(((instr >> 16) & 0xFF) as u8);
}

pub fn get_opcode_num(instr: u32) -> u8 {
    return from_opcode(get_opcode(instr));
}

pub fn has_cached(instr: u32) -> bool {
    return (instr & Flags::HasValue as u32) == 0;
}

pub fn is_in_it(instr: u32) -> bool {
    assert!(has_cached(instr));
    return (instr & Flags::InITorWideData as u32) > 0;
}

pub fn is_unpredictable(instr: u32) -> bool {
    assert!(has_cached(instr));
    return (instr & Flags::Unpredictable as u32) > 0;
}

pub fn is_contextually_unpredictable(instr: u32) -> bool {
    assert!(has_cached(instr));
    return (instr & Flags::ContextualUnpred as u32) > 0;
}

pub fn is_wide(instr: u32) -> bool {
    assert!(has_cached(instr));
    return (instr & Flags::Wide as u32) > 0;
}

pub fn is_wide_data(instr: u32) -> bool {
    assert!(!has_cached(instr));
    return (instr & Flags::InITorWideData as u32) > 0;
}
