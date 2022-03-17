structure F = MipsFrame

structure Translate : TRANSLATE = 
(*manages local variables and static function nesting for semant + static link stuff*)
struct
    datatype exp = Ex of Tree.exp
                 | Nx of Tree.stm 
                 | Cx of Tree.label * Temp.label -> Tree.stm (*true dest * false dest -> stms that jump to one of the dests*)
            
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
        | unEx (Nx s) = Tree.ESEQ(s, Tree.CONST 0)
        | unEx (Cx genstm) = 
            let 
                val r = Temp.newtemp()
                val t = Temp.newlabel() and f = Temp.newlabel()
            in
                Tree.ESEQ(Tree.SEQ[Tree.MOVE(Tree.TEMP r, Tree.CONST 1),
                        genstm(t,f),
                        Tree.LABEL f,
                        Tree.MOVE(Tree.TEMP r, Tree.CONST 0),
                        Tree.LABEL t],
                        Tree.TEMP r)
            end 
    
    (* fun unNx (Nx s) = s
        | unNx (Ex e) = T.EXP e
        | unNx (Cx genstm) = UnNx(genstm) *)
end
