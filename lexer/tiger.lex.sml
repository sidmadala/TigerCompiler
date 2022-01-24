structure Mlex  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
FMTSEQ | STRING | COMMENT | INITIAL
    structure UserDeclarations = 
      struct

(* 
 * File: tiger.lex
 * Authors: Siddarth Madala, Michelle Zhang, Zian Wang
 * Description: ML-Lex configuration file for Tiger
 *)

type pos = int
type lexresult = Tokens.token

(*  Count line numbers *)
val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1
val sb = ref ""  (* String builder *)
val sbStartPos = ref 0
val isSbFinished = ref true  (* Handle unclosed strings *)
val commentCount = ref 0  (* Handle nested/unclosed comments *)
val commentStart = ref ~1

(* End of file handler *)
fun eof() = 
    let 
        val pos = hd(!linePos) 
    in
        case (!isSbFinished, !commentCount) of
            (true, 0) => Tokens.EOF(pos, pos)
          | (true, _) => (ErrorMsg.error (pos) "Unclosed comment"; Tokens.EOF(pos, pos)) (* TODO: Add position matching *)
          | (false, _) => (ErrorMsg.error (pos) "Unclosed string"; Tokens.EOF(pos, pos)) (* TODO: Add position matching *)
    end



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN COMMENT; 
                  commentStart := yypos;
                  commentCount := !commentCount + 1;
                  continue()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (commentCount := !commentCount + 1;
                   continue()))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      (commentCount := !commentCount - 1;
                   if !commentCount = 0 then YYBEGIN INITIAL else ();
                   continue()))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lineNum := !lineNum + 1;
                   linePos := yypos + 1 :: !linePos;
                   continue()))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (linePos := yypos :: !linePos; lineNum := !lineNum + 1; continue()))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.WHILE(yypos, yypos+5)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FOR(yypos, yypos+3)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TO(yypos, yypos+2)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.BREAK(yypos, yypos+5)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.IN(yypos, yypos+2)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.END(yypos, yypos+3)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FUNCTION(yypos, yypos+8)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.VAR(yypos, yypos+3)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TYPE(yypos, yypos+4)))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.IF(yypos, yypos+2)))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.THEN(yypos, yypos+4)))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ELSE(yypos, yypos+4)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DO(yypos, yypos+2)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OF(yypos, yypos+2)))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NIL(yypos, yypos+3)))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LET(yypos, yypos+3)))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COMMA(yypos, yypos+1)))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COLON(yypos, yypos+1)))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SEMICOLON(yypos, yypos+1)))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LPAREN(yypos, yypos+1)))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RPAREN(yypos, yypos+1)))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACK(yypos, yypos+1)))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACK(yypos, yypos+1)))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACE(yypos, yypos+1)))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACE(yypos, yypos+1)))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DOT(yypos, yypos+1)))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PLUS(yypos, yypos+1)))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MINUS(yypos, yypos+1)))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TIMES(yypos, yypos+1)))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DIVIDE(yypos, yypos+1)))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EQ(yypos, yypos+1)))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NEQ(yypos, yypos+2)))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LT(yypos, yypos+1)))
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LE(yypos, yypos+2)))
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GT(yypos, yypos+1)))
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GE(yypos, yypos+2)))
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.AND(yypos, yypos+1)))
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OR(yypos, yypos+1)))
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ASSIGN(yypos, yypos+2)))
fun yyAction46 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.INT(valOf(Int.fromString(yytext)), yypos, yypos+size yytext))
      end
fun yyAction47 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ID(yytext, yypos, yypos+size yytext))
      end
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN STRING; sb := ""; sbStartPos := yypos; isSbFinished := false; continue()))
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; isSbFinished := true; Tokens.STRING(!sb, !sbStartPos, yypos + 1)))
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;
      (sb := !sb ^ "\n"; continue()))
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;
      (sb := !sb ^ "\t"; continue()))
fun yyAction52 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (sb := !sb ^ Char.toString(Char.chr(case String.substring(yytext, 2, 1) of " " => 32 | "?" => 127 | _  => Char.ord(String.sub(yytext, 2)) - 64)); continue())
      end
fun yyAction53 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (sb := !sb ^ Char.toString(Char.chr(valOf(Int.fromString(String.substring(yytext, 1, 3))))); continue())
      end
fun yyAction54 (strm, lastMatch : yymatch) = (yystrm := strm;
      (sb := !sb ^ "\""; continue()))
fun yyAction55 (strm, lastMatch : yymatch) = (yystrm := strm;
      (sb := !sb ^ "\\"; continue()))
fun yyAction56 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN FMTSEQ; continue()))
fun yyAction57 (strm, lastMatch : yymatch) = (yystrm := strm;
      (linePos := yypos :: !linePos; lineNum := !lineNum + 1; sb := !sb ^ "\n"; ErrorMsg.error yypos ("illegal newline in string"); continue()))
fun yyAction58 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (sb := !sb ^ yytext; continue())
      end
fun yyAction59 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction60 (strm, lastMatch : yymatch) = (yystrm := strm;
      (linePos := yypos :: !linePos; lineNum := !lineNum + 1; continue()))
fun yyAction61 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; ErrorMsg.error yypos ("FMTSEQ not closed"); isSbFinished := true; Tokens.STRING(!sb, !sbStartPos, yypos + 1)))
fun yyAction62 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN STRING; continue()))
fun yyAction63 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (YYBEGIN STRING; ErrorMsg.error yypos ("illegal character(none-whitespce) in formating sequence: " ^ yytext); continue())
      end
fun yyAction64 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (ErrorMsg.error yypos ("illegal character " ^ yytext); continue())
      end
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"`"
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp = #"`"
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ70(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ69(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ68(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"h"
              then yyQ67(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"h"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction14(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction14(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction14(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction14(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp = #"`"
              then yyAction14(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                  else yyAction14(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
              else yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ72(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction15(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction15(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction15(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction15(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp = #"`"
              then yyAction15(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
              else yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ77(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ76(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction9(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction9(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction9(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction9(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp = #"`"
              then yyAction9(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                  else yyAction9(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction17(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction17(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction17(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = #"`"
              then yyAction17(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ79(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ78(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction47(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"_"
                  then if inp <= #"Z"
                      then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"i"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"i"
                  then if inp = #"h"
                      then yyQ73(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"o"
                  then yyQ74(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"z"
              then if inp = #"y"
                  then yyQ75(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction20(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction20(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp = #"`"
              then yyAction20(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ80(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction21(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction21(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction21(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction21(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp = #"`"
              then yyAction21(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ82(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ81(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction22(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction22(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction22(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction22(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
            else if inp = #"`"
              then yyAction22(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                  else yyAction22(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
              else yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ84(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ83(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction11(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction11(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction11(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction11(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp = #"`"
              then yyAction11(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction16(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction16(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction16(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction16(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp = #"`"
              then yyAction16(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                  else yyAction16(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
              else yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction47(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ86(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"f"
                  then yyQ85(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ94 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction13(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction13(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction13(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction13(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp = #"`"
              then yyAction13(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                  else yyAction13(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ93 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ94(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ92 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ93(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ91 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ92(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ90 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ91(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ89 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"c"
              then yyQ90(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"c"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ89(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ95 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction8(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = #"`"
              then yyAction8(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ95(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction47(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ88(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"o"
                  then yyQ87(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ98 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction12(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction12(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction12(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction12(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp = #"`"
              then yyAction12(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ97 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"d"
              then yyQ98(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"d"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ100 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction18(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction18(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction18(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction18(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp = #"`"
              then yyAction18(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ99 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ100(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ96 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ99(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction47(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ97(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"l"
                  then yyQ96(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ101 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction19(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction19(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp = #"`"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ101(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ105 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction10(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction10(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction10(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction10(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp = #"`"
              then yyAction10(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ104 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"k"
              then yyQ105(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"k"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ103 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ104(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ102 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ103(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ102(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"`"
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ106 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ106(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
              else yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ108 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ107 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ108(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ107(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ109 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ109(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
              else yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ110 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ110(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"0"
              then yyAction46(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ110(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ110(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"0"
              then yyAction46(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ110(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ111 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ111(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction48(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction48(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\\"
              then yyQ30(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"\\"
              then if inp = #","
                  then yyQ39(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #","
                  then if inp = #"&"
                      then yyQ34(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                    else if inp < #"&"
                      then if inp = #"\v"
                          then yyQ30(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                        else if inp < #"\v"
                          then if inp = #"\t"
                              then yyQ31(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                            else if inp = #"\n"
                              then yyQ32(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                              else yyQ30(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                        else if inp = #"\""
                          then yyQ33(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                          else yyQ30(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                    else if inp = #")"
                      then yyQ36(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                    else if inp < #")"
                      then if inp = #"'"
                          then yyQ30(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                          else yyQ35(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                    else if inp = #"*"
                      then yyQ37(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                      else yyQ38(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"<"
                  then yyQ46(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"<"
                  then if inp = #"0"
                      then yyQ43(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                    else if inp < #"0"
                      then if inp = #"."
                          then yyQ41(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                        else if inp = #"-"
                          then yyQ40(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                          else yyQ42(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                    else if inp = #":"
                      then yyQ44(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                    else if inp = #";"
                      then yyQ45(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                      else yyQ43(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"?"
                  then yyQ30(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"?"
                  then if inp = #"="
                      then yyQ47(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                      else yyQ48(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ49(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"A"
                  then yyQ30(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"["
                  then yyQ50(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyQ49(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp = #"m"
              then yyQ49(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"e"
                  then yyQ54(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"e"
                  then if inp = #"b"
                      then yyQ52(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                    else if inp < #"b"
                      then if inp = #"^"
                          then yyQ30(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                        else if inp < #"^"
                          then yyQ51(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                        else if inp = #"a"
                          then yyQ49(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                          else yyQ30(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                    else if inp = #"c"
                      then yyQ49(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                      else yyQ53(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"i"
                  then yyQ56(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"i"
                  then if inp = #"f"
                      then yyQ55(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                      else yyQ49(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"l"
                  then yyQ57(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyQ49(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp = #"w"
              then yyQ62(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"w"
              then if inp = #"t"
                  then yyQ60(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"t"
                  then if inp = #"o"
                      then yyQ59(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                    else if inp = #"n"
                      then yyQ58(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                      else yyQ49(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"u"
                  then yyQ49(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyQ61(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp = #"|"
              then yyQ64(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"|"
              then if inp = #"{"
                  then yyQ63(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyQ49(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp = #"}"
              then yyQ65(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyQ30(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ28(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyQ29(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ26(strm', lastMatch)
            else if inp < #"*"
              then if inp = #"\n"
                  then yyQ25(strm', lastMatch)
                  else yyQ24(strm', lastMatch)
            else if inp = #"/"
              then yyQ27(strm', lastMatch)
              else yyQ24(strm', lastMatch)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"!"
              then yystuck(lastMatch)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ20(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"?"
              then yyQ20(strm', lastMatch)
            else if inp < #"?"
              then yystuck(lastMatch)
            else if inp <= #"_"
              then yyQ20(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction55(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction55(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ23(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"5"
              then yyQ23(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ23(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ23(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"5"
              then yyQ22(strm', lastMatch)
            else if inp < #"5"
              then if inp <= #"/"
                  then yystuck(lastMatch)
                  else yyQ21(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ21(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ21(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"]"
              then yyAction56(strm, yyNO_MATCH)
            else if inp < #"]"
              then if inp = #"0"
                  then yyQ14(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"\""
                      then yyQ13(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                      else yyAction56(strm, yyNO_MATCH)
                else if inp = #"3"
                  then yyAction56(strm, yyNO_MATCH)
                else if inp < #"3"
                  then if inp = #"2"
                      then yyQ15(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                      else yyQ14(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp = #"\\"
                  then yyQ16(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                  else yyAction56(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyAction56(strm, yyNO_MATCH)
            else if inp < #"o"
              then if inp = #"_"
                  then yyAction56(strm, yyNO_MATCH)
                else if inp < #"_"
                  then yyQ17(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp = #"n"
                  then yyQ18(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                  else yyAction56(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ19(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
              else yyAction56(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction57(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction57(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction58(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction58(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyQ11(strm', lastMatch)
            else if inp < #"\""
              then if inp = #"\n"
                  then yyQ10(strm', lastMatch)
                  else yyQ9(strm', lastMatch)
            else if inp = #"\\"
              then yyQ12(strm', lastMatch)
              else yyQ9(strm', lastMatch)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction62(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction62(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction61(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction61(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction60(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction60(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction59(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction59(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction63(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction63(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ4(strm', lastMatch)
            else if inp < #"#"
              then if inp = #"\v"
                  then yyQ4(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\t"
                      then yyQ5(strm', lastMatch)
                    else if inp = #"\n"
                      then yyQ6(strm', lastMatch)
                      else yyQ4(strm', lastMatch)
                else if inp = #"!"
                  then yyQ4(strm', lastMatch)
                else if inp < #"!"
                  then if inp = #" "
                      then yyQ5(strm', lastMatch)
                      else yyQ4(strm', lastMatch)
                  else yyQ7(strm', lastMatch)
            else if inp = #"\\"
              then yyQ8(strm', lastMatch)
            else if inp < #"\\"
              then if inp = #"L"
                  then yyQ5(strm', lastMatch)
                  else yyQ4(strm', lastMatch)
            else if inp = #"^"
              then yyQ5(strm', lastMatch)
              else yyQ4(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of FMTSEQ => yyQ0(!(yystrm), yyNO_MATCH)
    | STRING => yyQ1(!(yystrm), yyNO_MATCH)
    | COMMENT => yyQ2(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ3(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    end

  end
