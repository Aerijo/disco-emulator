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

fn handle_client(mut stream: TcpStream) {
    let path = Path::new("/home/benjamingray/lorem_ipsum.txt");
    let display = path.display();

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
                } else if pack.data == b"!" || pack.data == b"Hg0" || pack.data == b"Hc-1" {
                    build_reply(b"OK")
                } else if pack.data == b"qTStatus" {
                    build_reply(b"T1")
                } else if pack.data.starts_with(b"v") || pack.data == b"qTfV" {
                    build_reply(b"")
                } else if pack.data == b"?" {
                    build_reply(b"S 05")
                } else if pack.data == b"qfThreadInfo" {
                    build_reply(b"m 0")
                } else if pack.data == b"qsThreadInfo" {
                    build_reply(b"l")
                } else if pack.data == b"qC" {
                    build_reply(b"QC 0")
                } else if pack.data == b"qAttached" {
                    build_reply(b"0")
                } else if pack.data == b"qOffsets" {
                    build_reply(b"Text=0;Data=0;Bss=0")
                } else if pack.data == b"g" {
                    build_reply(b"0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")
                } else {
                    b"foo".to_vec()
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
