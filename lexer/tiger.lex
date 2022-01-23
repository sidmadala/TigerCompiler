type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1
val sb = ref ""
val sbStartPos = ref 0
val isSbFinished = ref true
fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


%%
digit=[0-9];
ctrlChar=\\\^[@-_ ?];
fmtChar=[ \t\^L];
%s STRING FMTSEQUENCE;
%%
\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

(* reserved words *)
<INITIAL> "while" => (Tokens.WHILE(yypos, yypos+5));
<INITIAL> "for" => (Tokens.FOR(yypos, yypos+3));
<INITIAL> "to" => (Tokens.TO(yypos, yypos+2));
<INITIAL> "break" => (Tokens.BREAK(yypos, yypos+5));
<INITIAL> "in" => (Tokens.IN(yypos, yypos+2));
<INITIAL> "end" => (Tokens.END(yypos, yypos+3);
<INITIAL> "function" => (Tokens.FUNCTION(yypos, yypos+8));
<INITIAL> "var" => (Tokens.VAR(yypos, yypos+3));
<INITIAL> "type" => (Tokens.TYPE(yypos, yypos+4));
<INITIAL> "if" => (Tokens.IF(yypos, yypos+2));
<INITIAL> "then" => (Tokens.THEN(yypos, yypos+4));
<INITIAL> "else" => (Tokens.ELSE(yypos, yypos+4));
<INITIAL> "do" => (Tokens.DO(yypos, yypos+2));
<INITIAL> "of" => (Tokens.OF(yypos, yypos+2));
<INITIAL> "nil" => (Tokens.NIL(yypos, yypos+3));
<INITIAL> "let" => (Tokens.LET(yypos, yypos+3));

(* arithmetic and punctuation *)
<INITIAL> "," => (Tokens.COMMA(yypos, yypos+1));
<INITIAL> ":" => (Tokens.COLON(yypos, yypos+1));
<INITIAL> ";" => (Tokens.SEMICOLON(yypos, yypos+1));
<INITIAL> "(" => (Tokens.LPAREN(yypos, yypos+1));
<INITIAL> ")" => (Tokens.RPAREN(yypos, yypos+1));
<INITIAL> "[" => (Tokens.LBRACK(yypos, yypos+1));
<INITIAL> "]" => (Tokens.RBRACK(yypos, yypos+1));
<INITIAL> "{" => (Tokens.LBRACE(yypos, yypos+1));
<INITIAL> "}" => (Tokens.RBRACE(yypos, yypos+1));
<INITIAL> "." => (Tokens.DOT(yypos, yypos+1));
<INITIAL> "+" => (Tokens.PLUS(yypos, yypos+1));
<INITIAL> "-" => (Tokens.MINUS(yypos, yypos+1));
<INITIAL> "*" => (Tokens.TIMES(yypos, yypos+1));
<INITIAL> "/" => (Tokens.DIVIDE(yypos, yypos+1));
<INITIAL> "=" => (Tokens.EQ(yypos, yypos+1));
<INITIAL> "<>" => (Tokens.NEQ(yypos, yypos+2));
<INITIAL> "<" => (Tokens.LT(yypos, yypos+1));
<INITIAL> "<=" => (Tokens.LE(yypos, yypos+2));
<INITIAL> ">" => (Tokens.GT(yypos, yypos+1));
<INITIAL> ">=" => (Tokens.GE(yypos, yypos+2));
<INITIAL> "&" => (Tokens.AND(yypos, yypos+1));
<INITIAL> "|" => (Tokens.OR(yypos, yypos+1));
<INITIAL> ":=" => (Tokens.ASSIGN(yypos, yypos+2));
<INITIAL> \" => (YYBEGIN STRING; sb := ""; sbStartPos := yypos; isSbFinished := false; continue());
<INITIAL> \n => (linePos := yypos :: !linePos; lineNum := !lineNum + 1; continue());
<STRING> \" => (YYBEGIN INITIAL; isSbFinished := true; Tokens.STRING(!sb, !sbStartPos, yypos + 1));
<STRING> \\n => (sb := !sb ^ "\n"; continue());
<STRING> \\t => (sb := !sb ^ "\t"; continue());
<STRING> {ctrlChar} => (sb := !sb ^ Char.toString(Char.chr(case String.substring(yytext, 2, 1) of " " => 32 | "?" => 127 | _  => Char.ord(String.sub(yytext, 2)) - 64)); continue());
<STRING> \\{digit}{digit}{digit} => (sb := !sb ^ yytext; continue());
<STRING> \\\" => (sb := !sb ^ "\""; continue());
<STRING> \\\\ => (sb := !sb ^ "\\"; continue());
<STRING> \\ => (YYBEGIN FMTSEQUENCE; continue());
<STRING> \n => (linePos := yypos :: !linePos; lineNum := !lineNum + 1; sb := !sb ^ yytext; ErrorMsg.error yypos ("illegal newline in string"); continue());
<STRING> . => (sb := !sb ^ yytext; continue());
<FMTSEQUENCE> {fmtChar} => (continue());
<FMTSEQUENCE> \n => (linePos := yypos :: !linePos; lineNum := !lineNum + 1; continue());
<FMTSEQUENCE> \\ => (YYBEGIN STRING; continue());
<FMTSEQUENCE> . => (ErrorMsg.error yypos ("illegal character(none-whitespce) in formating sequence: " ^ yytext); continue());
.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());


