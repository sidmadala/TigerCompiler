**Authors:**

Siddarth Madala (smm158), Michelle Zhang (xmz), Zian Wang (zw142) 

Run Instruction Selection:

CM.make "sources.cm";

Main.main "/path/to/test.tig";

TODO:  
CH. 10 - Liveness Analysis
- [x] implement MakeGraph module that turns a list of Assem instructions into a flow graph. use the given Graph structure
- [x] implement the Liveness module - using either set-equation algoirthm with array of bools or sorted list of temps, or one variable at a time

CH. 11 - Register Allocation
- [x] implement Color
- [x] implement RegAlloc


note: almost all of you assumed that that array creation library function will take care of initialising all values and storing the size of the array in the 0th index etc. which is actually not the for the provided library function (your compiler needs to emit code to do that as well), but it is conceivable that you'll modify the library before turning in your compiler to do that for you. so, no points off for such "mistakes."  -> from anshu for frame analysis and IR 
* do not forget to do this eventually

malloc expects the number of bytes you want, not words, so make sure you multiply the number of words you want by word-size before you call malloc (you call "allocRecord" which doesn't exist in the runtime library but no points off for that since we're not actually running your code until the end). Otherwise, mostly correct implementation! -> from anshu for ins sel 
* then what's allocRecord on line 12 of runtime.c?
* i fixed the first point by multiplying by F.wordSize on line 268 of translate.sml

