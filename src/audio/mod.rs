extern crate cpal;

use cpal::traits::{DeviceTrait, EventLoopTrait, HostTrait};
use std::thread;
use std::sync::{Mutex, mpsc::{SyncSender, sync_channel}};

#[derive(Debug)]
pub struct AudioHandler {
    sender: Option<SyncSender<i16>>,
    samples: u128,
}

impl AudioHandler {
    pub fn new() -> AudioHandler {
        return AudioHandler {
            sender: None,
            samples: 0,
        };
    }

    fn aquire(&mut self, rt: SyncSender<i16>) {
        self.sender = Some(rt);
    }

    pub fn handle(&mut self, amplitude: i16) {
        self.samples += 1;
        match &self.sender {
            Some(rt) => { rt.send(amplitude).unwrap() },
            None => {},
        }
    }

    pub fn spawn_audio(&mut self) {
        let (tx, rx) = sync_channel::<i16>(6);
        self.sender = Some(tx);
        let rx = Mutex::new(rx);

        thread::spawn(move || {
            let host = cpal::default_host();
            let device = host.default_output_device().expect("failed to find a default output device");
            let mut format = device.default_input_format().unwrap();
            format.data_type = cpal::SampleFormat::I16;
            format.sample_rate = cpal::SampleRate(48000);
            let event_loop = host.event_loop();
            let stream_id = event_loop.build_output_stream(&device, &format).unwrap();
            event_loop.play_stream(stream_id.clone()).unwrap();
            event_loop.run(move |id, result| {
                let data = match result {
                    Ok(data) => data,
                    Err(err) => {
                        eprintln!("an error occurred on stream {:?}: {}", id, err);
                        return;
                    }
                };
                match data {
                    cpal::StreamData::Output { buffer: cpal::UnknownTypeOutputBuffer::I16(mut buffer) } => {
                        let rx = rx.lock().unwrap();
                        for sample in buffer.chunks_mut(format.channels as usize) {
                            let value = rx.recv().unwrap();
                            for out in sample.iter_mut() {
                                *out = value;
                            }
                        }
                    },
                    _ => panic!(),
                }
            });
        });
    }
}
