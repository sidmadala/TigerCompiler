(* 
 * File: tiger.lex
 * Authors: Sid Madala, Michelle Zhang, and Zian Wang
 * Date: Jan 23, 2022
 * Description: ML-Lex configuration for Tiger lexer
 *)

(* Shortcuts *)

type pos = int
type lexresult = Tokens.token

(* Line number calculations *)
val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

(* EOF Handler *)
fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


%% 
%%
\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
","	=> (Tokens.COMMA(yypos,yypos+1));
var  	=> (Tokens.VAR(yypos,yypos+3));
"123"	=> (Tokens.INT(123,yypos,yypos+3));
.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

