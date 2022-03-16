structure F = MipsFrames

structure Translate : TRANSLATE = 
(*manages local variables and static function nesting for semant + static link stuff*)
struct
    type exp = unit (* old *)
    datatype level = TOP 
                   | NESTED of {parent: level, frame: F.frame, unique: unit ref}
    type access = level * Frame.access (* not the same as Frame.access *)

    val outermost = TOP

    (*newLevel called by transDec in Semant to create a new nesting level for each function*)
    (*this function in turn calls Frame.newFrame to make a new frame*)
    fun newLevel({parent, name, formals}) = NESTED{parent = parent, frame = F.newFrame(name, formals), unique = ref ()}
        |newLevel(_) = ErrorMsg.impossible "wrong arguments to newLevel"

    (*Returns nothing if TOP level, get formals converted into accesses if NESTED*)
    fun formals(TOP) = []
        | formals(currentLevel as NESTED{parent, frame, unique}) = 
            let 
                fun createAccess(frameAccess, l) = (currentLevel, frameAccess)::l 
            in
                foldl createAccess [] F.formals(frame)
            end
    
    (*Semant calls allocLocal when it processes a local variable declaration at a level to create the variable in the level*)
    (*allocLocal calls Frame.allocLocal, returns access*)
    fun allocLocal lvl esc = 
        case lvl of
        TOP => (ErrorMsg.impossible "Tried to allocate a local on top level (angery)")
        |NESTED{parent, frame, unique} => (lvl, F.allocLocal frame esc)
end


(* old 
(* dummy translate module *)
structure Translate = 
struct 
    type exp = unit
end
*)
