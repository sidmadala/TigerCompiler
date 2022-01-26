Authors:
Siddarth Madala (smm158)
Michelle Zhang (xmz)
Zian Wang (zw142)


Run Lexer:
CM.make "sources.cm";
Parse.parse "/path/to/test.tig";


**COMMENTS:**
When the start of a comment is reached (/*) while in INITIAL state, COMMENT state is entered and commentCount is incremented to include 1 level of nesting.
Further comments inside the COMMENT state increment commentCount by 1 and closed comments in the COMMENT state decrement the counter by 1.
When the end of the outermost comment is reached (*/), the counter is checked to equal 0 and the state is sent back to INITIAL.


**STRINGS:*
When the start a string is seen (") while in the INITIAL state, the STRING state is entered and the string builder (sb) is initialized to the start of the string. 
Valid escape sequences (i.e. \n, \t, \", \\, etc.) are converted to actual meaning and added to string via sb.
FMTSEQ is entered when a backslash is seen in a string (\). This feature allows for multiline strings and only entered after all other escape sequences are checked by rule precedence.
FMTSEQ throws an error if any characters asides from space, tab, newline is seen.
FMTSEQ ends and returns to STRING state when additional backslash (\) is observed.
When additional quote is encountered ("), STRING state ends, sb is reset to "" and sbStartPos is reset to 0.


**ERROR HANDLING:**
If an invalid character is seen, the appropriate error message is displayed with the position and the lexer continues.


**EOF HANDLING:*
When the end of a file is reached, open comments and strings are checked via commentCount and the value of sb. If values are invalid, the appropriate error is thrown.
