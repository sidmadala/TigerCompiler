structure F = MipsFrame
structure Tr = Tree

structure Translate : TRANSLATE = 
(*manages local variables and static function nesting for semant + static link stuff*)
struct
    datatype exp = Ex of Tr.exp
                 | Nx of Tr.stm 
                 | Cx of Tr.label * Temp.label -> Tr.stm (*true dest * false dest -> stms that jump to one of the dests*)
            
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
        | unEx (Nx s) = Tr.ESEQ(s, Tr.CONST 0)
        | unEx (Cx genstm) = 
            let 
                val r = Temp.newtemp()
                val t = Temp.newlabel() and f = Temp.newlabel()
            in
                Tr.ESEQ(Tr.SEQ[Tr.MOVE(Tr.TEMP r, Tr.CONST 1),
                        genstm(t,f),
                        Tr.LABEL f,
                        Tr.MOVE(Tr.TEMP r, Tr.CONST 0),
                        Tr.LABEL t],
                        Tr.TEMP r)
            end 
    
    fun unNx (Nx s) = s
        | unNx (Ex e) = Tr.EXP e
        | unNx (Cx genstm) = 
            let 
                val t = Temp.newlabel()
            in
                genstm(t, t)
                (*Tr.LABEL t ??? maybe???? 🤡🤫👀👨‍💻🤥*)
                (*please someone look at this later thank u idfk*)
            end
            
    fun unCx (Cx genstm) = genstm
        | unCx (Ex e) = 
            (case e of 
                Tr.CONST 0 => (fn (t, f) => Tr.JUMP(Tr.NAME(f), [f]))
                | Tr.CONST 1 => (fn (t, f) => Tr.JUMP(Tr.NAME(t), [t]))
                | exp => (fn (t, f) => Tr.CJUMP(Tr.EQ, Tr.CONST 1, exp, t, f)))
        | unCx (Nx s) = ErrorMsg.impossible "it should never occur in a well typed Tiger program >:("
    
    (*IF-THEN-ELSE*)
    fun transIF(test, thenBody, elseBody) = 
        let
            val genstm = unCx test
            val thenBody' = unEx thenBody 
            val elseBody' = unEx elseBody 
            val thenStart = Temp.newlabel()
            val elseStart = Temp.newlabel()
            val end = Temp.newlabel()
            val ans = Temp.newtemp()
        in
            Ex(Tr.ESEQ[
                genstm(thenStart, elseStart),
                Tr.LABEL thenStart, 
                Tr.MOVE(Tr.TEMP ans, Tr.EXP(thenBody')),
                Tr.JUMP(Tr.NAME end, [end]),
                Tr.LABEL elseStart,
                Tr.MOVE(Tr.TEMP ans, Tr.EXP(elseBody')),
                Tr.LABEL end
            ], Tr.TEMP ans)
        end

    (*FOR*)
    fun transFOR(var, hi, lo, body, endLabel) = 
        let
          val var' = unEx var
          val hi' = unEx hi
          val lo' = unEx lo
          val body' = unNx body
        in
        end
        
    (*WHILE*)
    fun transWHILE(test, body, endLabel) = 
        let 
            val testLabel = Temp.newlabel()
            val bodyLabel = Temp.newlabel()
            val genstm = unCx test
            val body' = unEx body
        in
            Nx(Tr.SEQ[
                Tr.JUMP(Tr.NAME testLabel, [testLabel]),
                Tr.LABEL bodyLabel, 
                Ex(body'),
                Tr.LABEL testLabel
                genstm(bodyLabel, endLabel),
                Tr.LABEL endLabel
            ])
        end
    
    (*BREAK*)
    fun transBREAK(break) = Nx(Tr.JUMP(Tr.NAME break, [break]))

    (*ASSIGN*)
    fun transAssign(left, right) =
        let 
            val left' = unEx left 
            val right' = unEx right
        in
            Nx(Tr.MOVE(left',right'))
        end
    
    (*DATA STUCTURES*)
    fun transNIL = Ex(Tr.CONST 0)
    fun transINT(n) = Ex(Tr.CONST n)
    (* fun transString() = ()
    fun transRecord() = ()
    fun transArray() = () *)

end
