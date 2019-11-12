A few programs that should be tested to ensure correct PC treatment.


Note: Everything tested so far is consistent with
- Isolated "true" instruction pointer
- Address of current instruction + 4 available in "normal" register bank
- Branch instructions affect the "true" instruction pointer
- Non specific branch instructions affect "normal" PC

### PC value
- ADC (imm) with rn=PC is Unpredictable, but will use PC+4 (no word align)
- ADC (reg) is same as ADC (imm); PC is not word aligned
- ADD (imm) is PC + 4, word aligned
- ADD (imm) writing to PC does nothing
- ADD (sp + imm) writes same as ADD (imm)
- LSL (imm) into PC does nothing

```
mov r0, PC @ 2byte
sub r0, 6  @ 4byte
mov PC, r0 @ 2byte
```


### SP value
- POP with SP is unpredictable, and does not get recognised (instruction executes as if SP bit is false)
