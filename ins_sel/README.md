**Authors:**

Siddarth Madala (smm158), Michelle Zhang (xmz), Zian Wang (zw142) 

Run Instruction Selection:

CM.make "sources.cm";

Main.main "/path/to/test.tig";

TODO:  
CH8 - BASIC BLOCKS AND TRACES (do we do this? i guess we do):
- [x] one function that "nots" a relational operator (< becomes >= etc) when you flip a CJUMP
- [ ] You also have to implement something that figures out if a statement and expression are independent of each other (can be reordering without changing the meaning). (i have no clue what this means/where this goes yet)

CH9 - INSTRUCTION SELECTION:
FRAME:
- [x] make tempmap + lists of regs
- [x] procEntryExit2 & 3

MIPSGEN:
- [x] munchexp
- [ ] munchstm
- [ ] munchargs