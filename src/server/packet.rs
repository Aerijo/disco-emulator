use std::fs::File;
use std::vec::Vec;
use std::io::Write;
use std::str;

extern crate hex;

pub enum PacketKind {
    Qsupported,
}

pub struct Packet {
    pub data: Vec<u8>,
    checksum: u8,
}

impl Packet {
    fn new(data: &[u8], checksum: u8) -> Packet {
        return Packet {
            data: data.to_vec(),
            checksum: checksum,
        };
    }
}

pub fn hex_to_word(hex: &[u8]) -> Result<u32, ()> {
    let s = match str::from_utf8(hex) {
        Ok(s) => s,
        Err(_) => return Err(()),
    };

    match u32::from_str_radix(s, 16) {
        Ok(u) => Ok(u),
        Err(_) => Err(()),
    }
}

pub fn word_to_hex(word: u32) -> String {
    return format!("{:08x}", word);
}

fn read_checksum(check: &[u8]) -> Result<u8, hex::FromHexError> {
    if check.len() != 2 {
        return Err(hex::FromHexError::InvalidStringLength);
    }
    return Ok(*hex::decode(check)?.first().unwrap());
}

fn build_checksum(pack: &[u8]) -> String {
    let mut sum: u8 = 0;
    for c in pack {
        sum = sum.wrapping_add(*c);
    };
    return hex::encode([sum]);
}

fn decode_packet(data: &[u8]) -> Result<Vec<u8>, ()> {
    let mut out: Vec<u8> = Vec::with_capacity(data.len());

    let mut iter = data.iter();

    while let Some(&c) = iter.next() {
        if c == b'}' {
            match iter.next() {
                Some(&e) => {
                    out.push(e ^ 0x20);
                }
                None => return Err(()),
            }
        } else if c == b'*' {
            let count = match iter.next() {
                Some(&c) => c - 29,
                None => return Err(()),
            };
            let copied = match out.last() {
                Some(l) => *l,
                None => return Err(()),
            };
            for _ in 0..count {
                out.push(copied);
            }
        } else {
            out.push(c);
        }
    }

    return Ok(out);
}

fn validate_packet(data: &[u8], check: u8) -> bool {
    let mut sum: u8 = 0;
    for i in data {
        sum = sum.wrapping_add(*i);
    }
    return sum == check;
}

pub fn read_packet(mut data: &[u8], file: &mut File) -> Result<Packet, ()> {
    if data.starts_with(&[b'+']) {
        data = &data[1..];
    }

    // Packet protocol:
    // $...#cc
    // where $ is literal, # is literal, cc is checksum in ASCII hexadecimal, and ... is the data
    if data.len() < 4 || data[0] != b'$' || data[data.len() - 3] != b'#' {
        return Err(());
    }

    let checksum = match read_checksum(&data[data.len() - 2..]) {
        Ok(c) => c,
        Err(e) => {
            writeln!(file, "Error with checksum: {}", e).unwrap();
            return Err(());
        }
    };

    if !validate_packet(&data[1..data.len() - 3], checksum) {
        writeln!(file, "Error with checksum validation").unwrap();
        return Err(());
    }

    return Ok(Packet::new(&data[1..data.len() - 3], checksum));
}

pub fn build_reply(data: &[u8]) -> Vec<u8> {
    let mut out: Vec<u8> = Vec::new();

    out.extend(b"+$");
    out.extend(data);
    out.push(b'#');
    out.extend(build_checksum(data).as_bytes());

    return out;
}
