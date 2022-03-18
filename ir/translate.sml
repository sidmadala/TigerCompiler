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

    val outermost = TOP

    (*newLevel called by transDec in Semant to create a new nesting level for each function*)
    (*this function in turn calls Frame.newFrame to make a new frame*)
    fun newLevel({parent, name, formals}) = NESTED{parent = parent, frame = F.newFrame{name=name, formals=formals}, unique = ref ()}

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
    

    (*BEGINNING OF IR TRANSLATION*)
    fun unEx (Ex e) = e
        | unEx (Nx s) = T.ESEQ(s, T.CONST 0)
        | unEx (Cx genstm) = 
            let 
                val r = Temp.newtemp()
                val t = Temp.newlabel() and f = Temp.newlabel()
            in
                T.ESEQ(T.SEQ[T.MOVE(T.TEMP r, T.CONST 1),
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
                T.SEQ[genstm(thenStart, elseStart),
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
            Nx(T.SEQ[
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
            Nx(T.SEQ[
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
    
    (*CALL*)
    fun transCall(_, TOP, funLabel, args) = 
        let
            val argsEx = map unEx args
        in
            Ex(T.CALL(T.NAME funLabel, argsEx))
        end

        |transCall(callLevel, fnLevel, funLabel, args) = 
        let
            val staticLink = ()
            (*TODO: STATIC LINK STUFF*)
            val argsEx = map unEx args
        in
            (*this is correct, but sl not done yet so compiler will throw errors*)
            (* Ex(T.CALL(T.NAME funLabel, staticLink::argsEx)) *)
            Ex(T.CALL(T.NAME funLabel, argsEx))
        end

    (*LET*)
    fun transLET(decs, body) = 
        let 
            val body' = unEx body
        in
            case List.length decs of 
                0 => Ex(body')
              | _ => Ex(T.ESEQ(T.SEQ (map unNx decs), body'))  
        end

    (*SEQEXP -> unclear if ... .. . . this is right? but i guess it can't hurt/we'll find out when we test*)
    fun transSEQEXP([]) = Ex(T.CONST 0) 
        |transSEQEXP([exp]) = exp
        |transSEQEXP(a::exps) = Ex(T.ESEQ(unNx(a), unEx(transSEQEXP(exps))))

    (*RELOPS -> do when you understand how strings work*)

    (*DATA STUCTURES*)
    fun transNIL(_) = Ex(T.CONST 0)
    fun transINT(n) = Ex(T.CONST n)
    fun transArray(size, init) = Ex(T.CALL(T.NAME(Temp.namedlabel "initArray"), [unEx size, unEx init]))
    fun transString() = ()
    fun transRecord() = ()

    (*also have to do vars + deal with vardec here*)
    fun transVarDec(level, varLabel) = ()
    fun transSimpleVar() = () 
    fun transFieldVar() = ()
    fun transSubscriptVar() = ()
end
