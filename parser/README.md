**Authors:**

Siddarth Madala (smm158), Michelle Zhang (xmz), Zian Wang (zw142) 


**Run Parser:**

CM.make "sources.cm";

Parse.parse "/path/to/test.tig";


**Comments:**

Parser is built via chapters 3 and 4 of Appel book.

Shift Reduce Conflicts:

We have 3 shift-reduce conflicts in our grammar. 

1. lvalue [] versus arr [] of ID

2. funlist conflict on FUNCTION

3. tylist conflicts on TYPE

Both conflicts 2 and 3 can be resolved via the default shift-reduce conflict resolution of the parser. For conflict 1, we added an additional state lvalue_not_id which separates the logic and forces a new state instead of running into the conflict.