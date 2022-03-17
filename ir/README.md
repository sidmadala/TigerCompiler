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
- [ ]  semant.sml

NOTE - I changed Tr.SEQ -> stm * stm to Tr.SEQ -> stm list 
IR:
- [ ] translate.sml

    vars:
    - [ ] simple
    - [ ] field
    - [ ] subscript

    exps:
    - [ ] varexp, nilexp, intexp, stringexp
    - [ ] opexp
    - [ ] callexp
    - [ ] seqexp
    - [ ] assignexp
    - [ ] ifexp
    - [ ] whileexp
    - [ ] forexp
    - [ ] breakexp
    - [ ] letexp
    - [ ] arrayexp
    - [ ] recordexp

    decs:
    - [ ] fundec
    - [ ] vardec
    - [ ] typedec

    types:
    - [ ] namety
    - [ ] recordty
    - [ ] arrayty

- [ ] semant.sml
