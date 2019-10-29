use crate::Shift;
use crate::instruction::{ShiftType, CarryChange};

pub fn bitset<T: Into<u32>>(word: T, bit: T) -> bool {
    let word = word.into();
    let bit = bit.into();
    return (word & (1 << bit)) > 0;
}

pub fn matches<T: Into<u32>>(word: T, shift: T, mask: T, expected: T) -> bool {
    let word = word.into();
    let shift = shift.into();
    let mask = mask.into();
    let expected = expected.into();
    return ((word >> shift) & mask) == expected;
}

pub fn decode_imm_shift<T: Into<u32>>(encoded: T) -> Shift {
    // A7.4.2
    let encoded = encoded.into();
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

pub fn align(address: u32, size: u32) -> u32 {
    assert!(size == 1 || size == 2 || size == 4);
    return address & !(size - 1);
}

pub fn word_align(address: u32) -> u32 {
    return align(address, 4);
}

pub fn sign_extend(value: u32, from: u32) -> i32 {
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
pub fn thumb_expand_imm_c(input: u32) -> (u32, CarryChange) {
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

pub fn rotate_right_32_c(input: u32, shift: u32) -> (u32, CarryChange) {
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

pub fn ror_c(input: u32, shift: u32) -> (u32, bool) {
    let (result, change) = rotate_right_32_c(input, shift);
    return match change {
        CarryChange::Set => (result, true),
        CarryChange::Clear => (result, false),
        _ => panic!(),
    }
}

pub fn thumb_expand_imm(val: u32) -> u32 {
    // p137
    return thumb_expand_imm_c(val).0;
}

pub fn rrx_c(input: u32, carry_in: u32) -> (u32, bool) {
    // p27
    let result = (input >> 1) + (carry_in << 31);
    let carry_out = bitset(result, 0);
    return (result, carry_out);
}

pub fn rrx(input: u32, carry_in: u32) -> u32 {
    // p27
    return rrx_c(input, carry_in).0;
}

pub fn lsl_c(input: u32, shift: u32) -> (u32, bool) {
    // p26
    assert!(shift > 0 && shift <= 32);
    let result = input << shift;
    let carry_out = bitset(input, 32 - shift);
    return (result, carry_out);
}

pub fn lsr_c(input: u32, shift: u32) -> (u32, bool) {
    // p26
    assert!(shift > 0 && shift <= 32);
    let result = input >> shift;
    let carry_out = bitset(input, shift - 1);
    return (result, carry_out);
}

pub fn asr_c(input: u32, shift: u32) -> (u32, bool) {
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
