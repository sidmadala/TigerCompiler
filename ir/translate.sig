signature TRANSLATE =

sig
type exp (* old *)
type level
type access (* not the same as Frame.access *)
val outermost : level
val newLevel : {parent: level, name: Temp.label,
formals: bool list} -> level
val formals : level -> access list
val allocLocal : level -> bool -> access
val unEx : exp -> Tree.exp
(* val unNx : exp -> Tree.stm
val unCx : exp -> (Temp.label * Temp.label -> Tree.stm) *)
end