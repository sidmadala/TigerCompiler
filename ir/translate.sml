signature TRANSLATE =
sig
type exp (* old *)
type level
type access (* not the same as Frame.access *)
val outermost : level
val newLevel : {parent: level, name: Temp.label,
formals: bool list} -> level
val formals: level -> access list
val allocLocal: level -> bool -> access
end
structure Translate : TRANSLATE = 
struct
    type exp = unit
end


(* old 
(* dummy translate module *)
structure Translate = 
struct 
    type exp = unit
end
*)
