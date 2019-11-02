use crate::utils::bits::{bitset};
use std::fmt;

pub mod opcode;
pub mod decode;
pub mod tag;

pub struct InstructionCache {
    cache: Box<[u32]>,
}

impl fmt::Debug for InstructionCache {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "<cache>");
    }
}

impl InstructionCache {
    pub fn new() -> InstructionCache {
        return InstructionCache {
            cache: Box::new([(BytecodeTag::reset_value() as u32) << 16; 1024 * 1024]),
        };
    }

    pub fn get_cached(&self, address: u32) -> Result<(u32, u32), String> {
        if 0x0800_0000 <= address && address <= 0x080F_FFFF {
            let base = (address - 0x0800_0000) as usize;
            return Ok((self.cache[base], self.cache[base + 1]));
        }
        return Err(String::from("Out of bounds access"));
    }

    pub fn write_cache_narrow(&mut self, address: u32, value: u32) {
        assert!(0x0800_0000 <= address && address <= 0x080F_FFFF);
        let index = (address - 0x0800_0000) as usize;
        self.cache[index] = value;
    }

    pub fn write_cache_wide(&mut self, address: u32, value: u32, extra: u32) {
        assert!(0x0800_0000 <= address && address <= 0x080F_FFFF);
        let index = (address - 0x0800_0000) as usize;
        self.cache[index] = value;
        self.cache[index + 1] = extra;
    }
}

/**
 * The position of the instruction within an IT block
 */
pub enum ItPos {
    None,
    Within,
    Last,
}

/**
 * Context relevant to deciding if an instruction is unpredictable
 */
pub struct InstructionContext {
    pc: u32,
    it_pos: ItPos,
}
