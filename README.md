## Design

A lot of things on the board are interconnected. E.g., clearing a pending interrupt requires writing to a peripheral, which needs to notify the exception emulator, which needs to write back to the peripheral to update certain bits. Also the RCC peripheral controls which other peripherals are enabled, so there needs to be some way for a given peripheral to check this.

The current design therefore aims to keep all execution at the Board level. It may use methods on children where possible, but it is very difficult, as many things need to be able to raise an exception.

To keep it less cluttered, methods are implemented in different files according to similar functionality.

// TODO: Write replacement startup file
- Init SRAM
- Enable usages, mem & bus faults
- Enable div by 0 trap
- Enable unaligned access trap
