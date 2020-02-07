pub struct Data {
    contents: Box<[u8; Data::SIZE]>,
}

impl Data {
    const SIZE: usize = 1024 * 128;

    fn new() -> Data {
        return Data {
            contents: Box::new([0; Data::SIZE]),
        }
    }
}

impl Memory for Data {
    fn read_word(&self, address: u32) -> u32 {
        panic!();
    }

    fn write_word(&mut self, address: u32, value: u32) {
        panic!();
    }
}
