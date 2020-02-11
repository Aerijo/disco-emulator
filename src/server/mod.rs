use crate::server::packet::hex_to_word;
use crate::server::packet::word_to_hex;
use crate::server::packet::build_reply;
use std::env;
use std::ffi::{OsString};
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::io::{stdin, stdout, Read};
use std::net::{TcpStream, TcpListener, Shutdown};
use std::path::{PathBuf, Path};
use std::vec;

use crate::Board;

mod packet;
use packet::read_packet;

pub fn start_server() {
    let port: String = get_tcp_port().unwrap();

    let listener = match TcpListener::bind(format!("127.0.0.1:{}", port)) {
        Ok(s) => s,
        Err(e) => {
            return;
        }
    };

    match listener.accept() {
        Ok((socket, addr)) => {
            handle_client(socket);
        }
        Err(e) => {
            return;
        }
    }
}

fn get_tcp_port() -> Option<String> {
    let args: Vec<OsString> = env::args_os().collect();
    for arg in args {
        if arg.to_str().expect("").starts_with("tcp::") {
            let port = &arg.to_str().expect("")[5..];
            return Some(String::from(port));
        }
    };
    return None;
}

fn get_elf_file_path() -> Option<PathBuf> {
    let mut args = env::args();
    while let Some(arg) = args.next() {
        if arg == "-kernel" {
            let path = args.next()?;
            return Some(PathBuf::from(&path));
        }
    }
    return None;
}

fn parse_read_memory(mut data: &[u8]) -> Result<(u32, u32), ()> {
    data = &data[1..]; // remove "m"
    let mut parts = data.split(|c| *c == b',');

    let addr = parts.next().unwrap();
    let length = parts.next().unwrap();
    if !parts.next().is_none() {
        return Err(());
    }

    return Ok((hex_to_word(addr)?, hex_to_word(length)?));
}

fn handle_client(mut stream: TcpStream) {
    let path = Path::new("/home/benjamingray/lorem_ipsum.txt");
    let display = path.display();

    let mut board = Board::new();
    match get_elf_file_path() {
        Some(p) => {
            board.load_elf_from_path(&p).unwrap();
        },
        None => {
            return;
        }
    }

    // Open a file in write-only mode, returns `io::Result<File>`
    let mut file = match File::create(&path) {
        Err(why) => panic!("couldn't create {}: {}", display, why.description()),
        Ok(file) => file,
    };

    let args: Vec<OsString> = env::args_os().collect();
    for arg in args {
        writeln!(file, "ARG: {:?}", arg).unwrap();
    };

    let mut data = [0 as u8; 2048];
    while match stream.read(&mut data) {
        Ok(size) => {
            writeln!(file, "Receiving data...").unwrap();
            // write!(file, "{:?}", &data[0..size]).unwrap();
            file.write_all(&data[0..size]).unwrap();
            writeln!(file).unwrap();

            if size <= 1 {
                // interrupt 0x03 or + or -, etc

                // if size == 0 {
                //     let out = build_reply(b"12345678");
                //     writeln!(file, "Sending data...").unwrap();
                //     file.write_all(&out).unwrap();
                //     writeln!(file).unwrap();
                //
                //
                //     stream.write(out.as_ref()).unwrap();
                // }

            } else {
                // echo everything!


                let pack = match read_packet(&data[0..size], &mut file) {
                    Ok(p) => p,
                    Err(e) => {
                        writeln!(file, "Error reading package").unwrap();
                        return;
                    }
                };

                let out: Vec<u8> = if pack.data.starts_with(b"qSupported") {
                     build_reply(b"PacketSize=2048")
                } else if pack.data == b"!" || pack.data == b"Hg0" || pack.data.starts_with(b"Hc") || pack.data == b"qSymbol::"{
                    build_reply(b"OK")
                } else if pack.data == b"qTStatus" {
                    build_reply(b"T0")
                } else if pack.data.starts_with(b"v") || pack.data == b"qTfV" || pack.data == b"qTfP" {
                    build_reply(b"")
                } else if pack.data == b"?" {
                    build_reply(b"S05")
                } else if pack.data == b"qfThreadInfo" {
                    build_reply(b"m0")
                } else if pack.data == b"qsThreadInfo" {
                    build_reply(b"l")
                } else if pack.data == b"qC" {
                    build_reply(b"QC0")
                } else if pack.data == b"qAttached" {
                    build_reply(b"0")
                } else if pack.data == b"qOffsets" {
                    build_reply(b"Text=0;Data=0;Bss=0")
                } else if pack.data == b"g" {
                    build_reply(b"00000000")
                } else if pack.data == b"s" {
                    board.step().unwrap();
                    build_reply(b"S05")
                } else if pack.data.starts_with(b"p") {
                    // read register X where request is pX

                    let num = &pack.data[1..];
                    let k = hex_to_word(num).unwrap();
                    let rval = if k == 15 {
                        board.cpu.read_instruction_pc().swap_bytes()
                    } else if k < 15 {
                        board.read_reg(k).swap_bytes()
                    } else {
                        0
                    };

                    build_reply(word_to_hex(rval).as_bytes())
                } else if pack.data.starts_with(b"m") {
                    let (start, length) = parse_read_memory(&pack.data).unwrap();
                    let vals = board.read_memory_region(start, length).unwrap();

                    let mut strs: Vec<u8> = Vec::new();
                    for val in vals {
                        strs.extend(format!("{:02x}", val).bytes());
                    }

                    build_reply(strs.as_slice())
                } else {
                    writeln!(file, "unknown instruction").unwrap();
                    return;
                };

                writeln!(file, "Sending data...").unwrap();
                file.write_all(&out).unwrap();
                writeln!(file).unwrap();


                stream.write(out.as_ref()).unwrap();
            }
            true
        },
        Err(_) => {
            println!("An error occurred, terminating connection with {}", stream.peer_addr().unwrap());
            stream.shutdown(Shutdown::Both).unwrap();
            false
        }
    } {};
}
