impl Board {
    fn print_mem_dump(&mut self, index: u32, length: usize) {
        self.memory.print_mem_dump(index, length);
    }

    fn print_mem_area(&mut self, address: u32) {
        self.memory.print_mem_area(address);
    }

    fn get_register_display_value(&self, reg: u8) -> String {
        assert!(reg <= 15);
        let val = match reg {
            15 => self.cpu.read_instruction_pc(),
            _ => self.read_reg(reg),
        };
        return match self.register_formats[reg as usize] {
            RegFormat::Bin => format!("{:#034b}", val),
            RegFormat::Oct => format!("{:#013o}", val),
            RegFormat::Dec => format!("{}", val),
            RegFormat::Sig => format!("{}", val as i32),
            RegFormat::Hex => format!("{:#010X}", val),
        };
    }

    fn set_display_format(&mut self, reg: u8, kind: RegFormat) {
        assert!(reg <= 15);
        self.register_formats[reg as usize] = kind;
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut registers = String::new();
        let indent = "    ";

        for i in 0..4 {
            let left = self.get_register_display_value(i);
            let right = self.get_register_display_value(i + 8);
            let left_label = format!("r{}", i);
            let right_label = format!("r{}", i + 8);
            registers.push_str(&format!(
                "{}{: >3}: {: <34}  {: >3}: {: <34}\n",
                indent, left_label, left, right_label, right
            ));
        }
        for i in 4..8 {
            let left = self.get_register_display_value(i);
            let right = self.get_register_display_value(i + 8);
            let special = ["r12", "sp", "lr", "pc"];
            let left_label = format!("r{}", i);

            registers.push_str(&format!(
                "{}{: >3}: {: <34}  {: >3}: {: <34}\n",
                indent,
                left_label,
                left,
                special[(i - 4) as usize],
                right
            ));
        }
        registers.push('\n');
        registers.push_str(&format!("{}{}\n{}{}\n", indent, self.cpu.get_apsr_display(), indent, self.itstate));
        return write!(f, "CPU {{\n{}}}", registers);
    }
}
