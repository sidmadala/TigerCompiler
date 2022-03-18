**Authors:**

Siddarth Madala (smm158), Michelle Zhang (xmz), Zian Wang (zw142) 

Run FA+IR:

CM.make "sources.cm";

Main.main "/path/to/test.tig";



Frame Analysis:
- [x] modify VarEntry and FunEntry in env.sml
- [x]  mipsframe.sml
- [x]  findescape.sml
- [x]  translate.sml
- [x]  semant.sml

NOTE - I changed Tr.SEQ -> stm * stm to Tr.SEQ -> stm list 
IR:
- translate.sml

    vars:
    - [ ] simple
    - [ ] field
    - [ ] subscript

    exps:
    - [x] nilexp
    - [x] intexp
    - [x] stringexp 
    - [x] opexp
    - [ ] callexp
    - [x] seqexp
    - [x] assignexp
    - [x] ifexp
    - [x] whileexp
    - [x] forexp
    - [x] breakexp
    - [x] letexp
    - [x] arrayexp
    - [ ] recordexp

    decs:
    - [ ] vardec -> do transSimpleVar and then assign 
    - [ ] fundec -> i think this prob goes last (pg. 167-168) (procentryexit())

- [ ] semant.sml
