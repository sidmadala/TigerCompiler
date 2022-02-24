**Authors:**

Siddarth Madala (smm158), Michelle Zhang (xmz), Zian Wang (zw142) 


**Run Semantic Analysis**

CM.make "sources.cm";

Main.main "/path/to/test.tig";

**Comments:**

Semantic analyzer is built via chapter 5 of Appel book.

Our group wrote main.sml, env.sml, and semant.sml.

TODO:
1. go through all of absyn.sml and write code to translate it (that's where the list below comes from)
2. go change escape = ref false in parser? (i guess deal with escapes)
3. figure out if the purely functional record EC is something we want to do  

vars:
- [x] simple
- [x] field
- [x] subscript

exps:
- [x] varexp, nilexp, intexp, stringexp
- [x] opexp
- [x] callexp
- [ ] recordexp
- [x] seqexp
- [x] assignexp
- [x] ifexp
- [x] whileexp
- [x] forexp
- [x] breakexp
- [x] letexp
- [ ] arrayexp

decs:
- [ ] fundec
- [ ] vardec
- [ ] typedec

types:
- [ ] namety
- [ ] recordty
- [ ] arrayty
