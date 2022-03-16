structure F = MipsFrame

structure Translate : TRANSLATE = 
(*manages local variables and static function nesting for semant*)
(*static link business goes here*)
struct
    type exp = unit (* old *)
    datatype level = TOP 
                   | NESTED of {parent: level, frame: F.frame, unique: unit ref}
    type access = level * Frame.access (* not the same as Frame.access *)

    val outermost = TOP

    (*newLevel called by transDec in Semant to create a new nesting level for each function*)
    (*this function in turn calls Frame.newFrame to make a new frame*)
    val newLevel : {parent: level, name: Temp.label,
    formals: bool list} -> level
    val formals: level -> access list
    (*Semant calls allocLocal when it processes a local variable declaration at level lev to create the variable in the level*)
    (*allocLocal calls Frame.allocLocal*)
    val allocLocal: level -> bool -> access
end


(* old 
(* dummy translate module *)
structure Translate = 
struct 
    type exp = unit
end
*)
