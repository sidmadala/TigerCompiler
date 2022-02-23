**Authors:**

Siddarth Madala (smm158), Michelle Zhang (xmz), Zian Wang (zw142) 


**Run Parser:**

CM.make "sources.cm";

Parse.parse "/path/to/test.tig";

**Comments:**
TODO:

1. go through all of absyn.sml and write code to translate it (that's where the list below comes from)

2. go change escape = ref false in parser? (i guess deal with escapes)

3. figure out if the purely functional record EC is something we want to do  

vars:
[x] simple
[x] field
[x] subscript

exps:
[x] varexp, nilexp, intexp, stringexp
[x] opexp
[] callexp
[] recordexp
[x] seqexp
[] assignexp
[] ifexp
[] whileexp
[] forexp
[] breakexp
[] letexp
[] arrayexp

decs:
[] fundec
[] vardec
[] typedec

types:
[] namety
[] recordty
[] arrayty
