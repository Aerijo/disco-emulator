use crate::{ByteInstruction};
use opcode{Opcode, from_opcode};

enum Flags {
    HasValue = 1 << 15,
    InITorWideData = 1 << 14,
    Unpredictable = 1 << 13,
    ITUnpredictable = 1 << 12,
    Wide = 1 << 11,
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
 * - ITunpredictable: Upredictability is due to being in an IT block. Useful for when jumping into IT block (technically illegal),
 *   but so we know to ignore the normal unpredictable mark
 *
 * - When caching an IT instruction, clear an appropriate number of subsequent instructions (e.g., length * wides worth) to ensure we don't
 *   have incorrect caches. This only happens once (in a sane program) because we can trust the cache afterwards.
 *
 * - For 2nd half of wide instruction, first bit == 1 means it is not an instruction start, second bit == 1 means this instruction is the
 *   second half of some data. Without this, we could branching into wide instruction, replace this data half, and leave in invalid state
 */
pub struct Tag {
    tag: u16,
}

impl Tag {
    fn from(cached: ByteInstruction) -> Tag {
        return Tag {tag: (cached[0] >> 16) as u16};
    }

    pub fn get_wide(opcode: Opcode, context: InstructionContext) -> u32 {
        return Tag::get_tag(Flags::InITorWideData as u16, opcode, context);
    }

    pub fn get_narrow(opcode: Opcode, context: InstructionContext) -> u32 {
        return Tag::get_tag(0, opcode, context);
    }

    fn get_tag(mut prepped: u16, opcode: Opcode, context: Context) -> u32 {
        match context.it_pos {
            ItPos::None => {},
            ItPos::Within | ItPos::Last => {
                prepped |= (Flags::InITorWideData as u16);
            }
        }
        prepped |= from_opcode(opcode);
        return (prepped as u32) << 16;
    }

    fn reset_value() -> u16 {
        return Flags::HasValue as u16;
    }

    // 0b00000 is narrow instruction, 0b00001 is wide instruction,
    // anything else needs custom handling
    fn get_meta(&self) -> u16 {
        return self.tag >> 11;
    }

    fn get_opcode(&self) -> Opcode {
        assert!(self.has_cached());
        return to_opcode(self.tag);
    }

    fn get_opcode_num(&self) -> u16 {
        return from_opcode(self.get_opcode());
    }

    fn has_cached(&self) -> bool {
        return !bitset(self.tag, Flags::HasValue as u16);
    }

    fn is_in_it(&self) -> bool {
        assert!(self.has_cached());
        return bitset(self.tag, Flags::InITorWideData as u16);
    }

    fn is_unpredictable(&self) -> bool {
        assert!(self.has_cached());
        return bitset(self.tag, Flags::Unpredictable as u16);
    }

    fn is_it_unpredictable(&self) -> bool {
        assert!(self.has_cached());
        return bitset(self.tag, Flags::ITUnpredictable as u16);
    }

    fn is_wide(&self) -> bool {
        assert!(self.has_cached());
        return bitset(self.tag, Flags::Wide as u16);
    }

    fn is_wide_data(&self) -> bool {
        assert!(!self.has_cached());
        return bitset(self.tag, Flags::InITorWideData as u16);
    }
}
