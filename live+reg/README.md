**Authors:**

Siddarth Madala (smm158), Michelle Zhang (xmz), Zian Wang (zw142) 

Run Instruction Selection:

CM.make "sources.cm";

Main.main "/path/to/test.tig";

TODO:  
CH. 10 - Liveness Analysis
- [ ] implement MakeGraph module that turns a list of Assem instructions into a flow graph. use the given Graph structure
- [ ] implement the Liveness module - using either set-equation algoirthm with array of bools or sorted list of temps, or one variable at a time

CH. 11 - Register Allocation
- [ ] implement Color
- [ ] implement RegAlloc