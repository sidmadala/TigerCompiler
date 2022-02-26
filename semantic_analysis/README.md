**Authors:**

Siddarth Madala (smm158), Michelle Zhang (xmz), Zian Wang (zw142) 


**Run Semantic Analysis**

CM.make "sources.cm";

Main.compile "/path/to/test.tig";

**Comments:**

Semantic analyzer is built via chapter 5 of Appel book.

Our group wrote main.sml, env.sml, and semant.sml.

**Extra Credit**

We implemented RECORDS in a purely functional manner and also added a BOTTOM type. We also changed records to take a symbol table instead of a unit since we were having trouble with our implementation in terms of extracting types. 

**Late Days** 

We used a late day for this part of the compiler due to its difficulty



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
- [ ] callexp
- [x] seqexp
- [ ] assignexp
- [ ] ifexp
- [x] whileexp
- [ ] forexp
- [X] breakexp
- [x] letexp
- [x] arrayexp
- [x] recordexp

decs:
- [x] fundec
- [x] vardec
- [x] typedec

types:
- [x] namety
- [ ] recordty
- [x] arrayty

Potential Issues
- [x] Escapes inside parser module (set to false)
