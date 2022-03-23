structure Translate : TRANSLATE = 
(*manages local variables and static function nesting for semant + static link stuff*)
struct
    structure T = Tree
    structure F = MipsFrame

    datatype exp = Ex of T.exp
                 | Nx of T.stm 
                 | Cx of T.label * Temp.label -> T.stm (*true dest * false dest -> stms that jump to one of the dests*)
            
    datatype level = TOP 
                   | NESTED of {parent: level, frame: F.frame, unique: unit ref}

    type access = level * F.access (* not the same as Frame.access *)
    
    (*FRAGMENT STUFF pg 169-170*)
    type frag = F.frag
    val fraglist : frag list ref = ref []
    fun getResult() = !fraglist
    fun resetFragList() = fraglist := []

    val outermost = TOP

    (*newLevel called by transDec in Semant to create a new nesting level for each function*)
    (*this function in turn calls Frame.newFrame to make a new frame*)
    fun newLevel({parent, name, formals}) = NESTED{parent = parent, frame = F.newFrame{name=name, formals=true::formals}, unique = ref ()}

    (*Returns nothing if TOP level, get formals converted into accesses if NESTED*)
    fun formals(TOP) = []
        | formals(currentLevel as NESTED{parent, frame, unique}) = 
            let 
                fun createAccess (frameAccess, l) = (currentLevel, frameAccess)::l 
            in
                foldl createAccess [] (F.formals(frame))
            end
    
    (*Semant calls allocLocal when it processes a local variable declaration at a level to create the variable in the level*)
    (*allocLocal calls Frame.allocLocal, returns access*)
    fun allocLocal lvl esc = 
        case lvl of
        TOP => (ErrorMsg.impossible "Tried to allocate a local on top level (angery)")
        |NESTED{parent, frame, unique} => (lvl, F.allocLocal frame esc)
    
    fun makeSEQ([s]) = s
      | makeSEQ([a, b]) = T.SEQ(a,b)
      | makeSEQ(a::l) = T.SEQ(a, makeSEQ l)
      | makeSEQ([]) = T.EXP(T.CONST 0)

    (*BEGINNING OF IR TRANSLATION*)
    fun unEx (Ex e) = e
        | unEx (Nx s) = T.ESEQ(s, T.CONST 0)
        | unEx (Cx genstm) = 
            let 
                val r = Temp.newtemp()
                val t = Temp.newlabel() and f = Temp.newlabel()
            in
                T.ESEQ(makeSEQ[T.MOVE(T.TEMP r, T.CONST 1),
                        genstm(t,f),
                        T.LABEL f,
                        T.MOVE(T.TEMP r, T.CONST 0),
                        T.LABEL t],
                        T.TEMP r)
            end 
    
    fun unNx (Nx s) = s
        | unNx (Ex e) = T.EXP e
        | unNx (Cx genstm) = 
            let 
                val t = Temp.newlabel()
            in
                genstm(t, t)
                (*T.LABEL t ??? maybe???? 🤡🤫👀👨‍💻🤥*)
                (*please someone look at this later thank u idfk*)
            end
            
    fun unCx (Cx genstm) = genstm
        | unCx (Ex e) = 
            (case e of 
                T.CONST 0 => (fn (t, f) => T.JUMP(T.NAME(f), [f]))
                | T.CONST 1 => (fn (t, f) => T.JUMP(T.NAME(t), [t]))
                | exp => (fn (t, f) => T.CJUMP(T.EQ, T.CONST 1, exp, t, f)))
        | unCx (Nx s) = ErrorMsg.impossible "it should never occur in a well typed Tiger program >:("
    
    (*IF-THEN-ELSE*)
    fun transIF(test, thenBody, elseBody) = 
        let
            val genstm = unCx test
            val thenBody' = unEx thenBody 
            val elseBody' = unEx elseBody 
            val thenStart = Temp.newlabel()
            val elseStart = Temp.newlabel()
            val endLabel = Temp.newlabel()
            val ans = Temp.newtemp()
        in
            Ex(T.ESEQ(
                makeSEQ[genstm(thenStart, elseStart),
                T.LABEL thenStart, 
                T.MOVE(T.TEMP ans, thenBody'),
                T.JUMP(T.NAME endLabel, [endLabel]),
                T.LABEL elseStart,
                T.MOVE(T.TEMP ans, elseBody'),
                T.LABEL endLabel]
                , T.TEMP ans))
        end

    (*FOR*)
    fun transFOR(hi, lo, body, endLabel) = 
        let
          val hi' = unEx hi
          val lo' = unEx lo
          val body' = unNx body
          val hiTemp = Temp.newtemp()
          val i = Temp.newtemp() 
          val incrementLabel = Temp.newlabel()
          val loopBodyLabel = Temp.newlabel()
        in
            Nx(makeSEQ[
                T.MOVE(T.TEMP i, lo'),
                T.MOVE(T.TEMP hiTemp, hi'),
                T.CJUMP(T.LE, T.TEMP i, hi', loopBodyLabel, endLabel),
                T.LABEL incrementLabel,
                T.MOVE(T.TEMP i, T.BINOP(T.PLUS, T.CONST 1, T.TEMP i)),
                T.LABEL loopBodyLabel,
                body',
                T.CJUMP(T.LE, T.TEMP i, T.TEMP hiTemp, incrementLabel, endLabel),
                T.LABEL endLabel
            ])
        end
        
    (*WHILE*)
    fun transWHILE(test, body, endLabel) = 
        let 
            val testLabel = Temp.newlabel()
            val bodyLabel = Temp.newlabel()
            val genstm = unCx test
            val body' = unEx body
        in
            Nx(makeSEQ[
                T.JUMP(T.NAME testLabel, [testLabel]),
                T.LABEL bodyLabel, 
                T.EXP body',
                T.LABEL testLabel,
                genstm(bodyLabel, endLabel),
                T.LABEL endLabel
            ])
        end

    (*BREAK*)
    fun transBREAK(break) = Nx(T.JUMP(T.NAME break, [break]))

    (*ASSIGN*)
    fun transAssign(left, right) =
        let 
            val left' = unEx left 
            val right' = unEx right
        in
            Nx(T.MOVE(left',right'))
        end

    (*BINOPS*)
    fun transBinOp(oper, left, right) = 
        let
            val left' = unEx left
            val right' = unEx right
            val op' =
                (case oper of 
                    Absyn.PlusOp => T.PLUS
                   |Absyn.MinusOp => T.MINUS
                   |Absyn.TimesOp => T.MUL
                   |Absyn.DivideOp => T.DIV
                   |_ => ErrorMsg.impossible "we're using transBinOp wrong")  
        in
            Ex(T.BINOP(op', left', right'))
        end
    
    (*RELOP -> must be different to binop because string comparisons are external*)
    fun transRelOp(oper, left, right, typ) =
        let
            val left' = unEx left
            val right' = unEx right
            val oper' = case oper of
                 Absyn.EqOp => T.EQ
                |Absyn.NeqOp => T.NE
                |Absyn.LtOp => T.LT
                |Absyn.GtOp => T.GT
                |Absyn.LeOp => T.LE
                |Absyn.GeOp => T.GE
                |_ => ErrorMsg.impossible "we're using transRelOp wrong"
        in
            case (oper', typ) of
                 (T.EQ, Types.STRING) => Ex(F.externalCall("strEq", [left', right']))
                |(T.NE, Types.STRING) => Ex(F.externalCall("strNeq", [left', right']))
                |(T.LT, Types.STRING) => Ex(F.externalCall("strLt", [left', right']))
                |(T.GT, Types.STRING) => Ex(F.externalCall("strGt", [left', right']))
                |_ => Cx(fn (t, f) => T.CJUMP(oper', left', right', t, f))
        end 

    (* Helper function for frames *)
    fun findFrame (_, TOP) = (ErrorMsg.error ~1 "level not found via static links"; T.CONST 0)
      | findFrame (TOP, _) = (ErrorMsg.error ~1 "value declared in outermost level"; T.CONST 0)
      | findFrame (decLevel as NESTED{parent=parent, frame=frame, unique=unique}, callLevel as NESTED{parent=parent', frame=frame', unique=unique'}) =
            if unique = unique'
            then T.TEMP(F.FP)  (* found static link we want *)
            else T.MEM(findFrame(decLevel, parent'))  (* Follow static links *)

    (*CALL*)
    fun transCall(_, TOP, funLabel, args) = 
        let
            val argsEx = map unEx args
        in
            Ex(T.CALL(T.NAME funLabel, argsEx))
        end

      | transCall(callLevel, NESTED{parent=parent, frame=frame, unique=unique}, funLabel, args) = 
        let
            val staticLink = findFrame(parent, callLevel)
            val argsEx = map unEx args
        in
            Ex(T.CALL(T.NAME funLabel, staticLink::argsEx))
        end

    (*LET*)
    fun transLET(decs, body) = 
        let 
            val body' = unEx body
        in
            case List.length decs of 
                0 => Ex(body')
              | _ => Ex(T.ESEQ(makeSEQ (map unNx decs), body'))  
        end

    (*SEQEXP -> unclear if ... .. . . this is right? but i guess it can't hurt/we'll find out when we test*)
    fun transSEQEXP([]) = Ex(T.CONST 0) 
        |transSEQEXP([exp]) = exp
        |transSEQEXP(a::exps) = Ex(T.ESEQ(unNx(a), unEx(transSEQEXP(exps))))

    (*DATA STUCTURES*)
    fun transNIL() = Ex(T.CONST 0)
    fun transINT(n) = Ex(T.CONST n)

    (*ARRAYS*)
    fun transArray(size, init) = Ex(F.externalCall("initArray", [unEx size, unEx init]))

    (*STRINGS*)
    fun transString(string) = 
        let
            fun findStringFrag(F.PROC(_)) = false
                |findStringFrag(F.STRING(label, string')) = (String.compare(string, string') = EQUAL) 
            val foundFrag = List.find findStringFrag (!fraglist)
        in
            case foundFrag of 
                SOME(F.STRING(lab, str)) => Ex(T.NAME lab)
                |NONE =>
                    let
                    val newLabel = Temp.newlabel()
                    in
                    fraglist := (F.STRING(newLabel, string)::(!fraglist));
                    Ex(T.NAME newLabel)
                    end
                |_ => ErrorMsg.impossible "something wrong with findStringFrag"
        end

    (*RECORDS - pg 164 (@zian pass in list of expressions)*)
    fun transRecord(fieldExps) = 
        let 
            val r = Temp.newtemp()
            val flen = List.length fieldExps
            val recordInit = T.MOVE(T.TEMP r, F.externalCall("initRecord", [T.CONST flen]))
            fun initField(exp, index) = T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP r, T.CONST(F.wordSize*index))), unEx exp)
            fun genSTMS(currField, (prev, index)) =
                let 
                    val currFieldSTM = initField(currField, index)
                in
                    (currFieldSTM :: prev, index + 1)
                end  
            val (stms, index) = foldl genSTMS ([recordInit], 0) fieldExps
       in
            Ex(T.ESEQ(makeSEQ stms, T.TEMP r))
        end

    (*VARIABLES*)
    fun transSimpleVar((_, frameAccess), TOP) = (ErrorMsg.error ~1 "Variable called in outermost level"; Ex(T.CONST 0))
      | transSimpleVar((TOP, frameAccess), _) = (ErrorMsg.error ~1 "Variable declared in outermost level"; Ex(T.CONST 0))
      | transSimpleVar(frameAccess, levelFrame) = 
        let
            (* FIX: binding nonexhaustive *)
            val (NESTED{parent=parent, frame=frame, unique=testRef}, testAccess) = frameAccess
            fun followLink (NESTED{parent=parent', frame=frame', unique=currentRef}, currentAcc) =
                if testRef = currentRef then F.checkOffset(testAccess)(currentAcc)
                else
                    let 
                        val nextLink = hd (F.formals frame')
                    in
                        followLink (parent', F.checkOffset(nextLink)(currentAcc))
                    end
                |followLink (_, _) = ErrorMsg.impossible "fixing match nonexhaustive?"
        in
            Ex(followLink(levelFrame, T.TEMP(F.FP)))
        end

    fun transFieldVar(varEx, index) = Ex(T.MEM(T.BINOP(T.PLUS, unEx varEx, T.CONST(index * F.wordSize))))

    fun transSubscriptVar(arrRef, indexRef) = 
        let
            val addr = Temp.newtemp()
            val arr = unEx arrRef
            val index = unEx indexRef
        in
            Ex(T.ESEQ(
               T.MOVE(T.TEMP(addr),
                       T.BINOP(T.PLUS, arr, T.BINOP(T.MUL, index, T.CONST(F.wordSize)))),
               T.MEM(T.TEMP(addr))))
        end

    fun procEntryExit ({level = TOP, body}) = (ErrorMsg.error ~1 "function is declared in outermost level"; ())
      | procEntryExit ({level = NESTED{parent, frame, unique}, body}) =
        let 
            val body' = unEx body
            val body'' = F.PROC{body = T.MOVE(T.TEMP F.RV, body'), frame = frame}  (* Why the move? *)
        in 
            fraglist := !fraglist @ [body'']  (* What is the order here? *)
        end
end
