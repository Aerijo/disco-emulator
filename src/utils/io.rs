pub fn read_register(register: u32, offset: u32, size: usize) -> Result<u32, String> {
    println!("OFF: {}, SIZE: {}", offset, size);
    assert!(
        size == 1 && (offset == 0 || offset == 1 || offset == 2 || offset == 3) ||
        size == 2 && (offset == 0 || offset == 2) ||
        size == 4 && offset == 0
    );
    let size = size as u32;
    return Ok((register >> (4 - (offset + size))) & (!0 >> (4 - size)));
}
