use crate::{ByteInstruction};

mod narrow;
use narrow::{decode_thumb_narrow};

mod wide;
use wide::{decode_thumb_wide};

fn is_wide_instruction(value: u32) -> bool {
    return (word >> 29 == 0b111) && (word >> 27 != 0b11100);
}

pub fn decode_thumb(thumb: u32, context: InstructionContext) -> ByteInstruction {
    return if is_wide_instruction(thumb) {
        decode_thumb_wide(word, context)
    } else {
        decode_thumb_narrow(word >> 16, context)
    };
}
