structure F = MipsFrame

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
val unNx : exp -> Tree.stm
val unCx : exp -> (Temp.label * Temp.label -> Tree.stm)

val transIF : exp * exp * exp -> exp
val transAssign : exp * exp -> exp
val transWHILE : exp * exp * Temp.label -> exp
val transBREAK : Temp.label -> exp
val transFOR : exp * bool ref * exp * exp * exp * Temp.label -> exp
val transBinOp : Absyn.oper * exp * exp -> exp
val transRelOp : Absyn.oper * exp * exp -> exp
val transArray : exp * exp -> exp
val transRecord : exp list -> exp
val transSEQEXP : exp list -> exp
val transNIL : unit -> exp
val transINT : int -> exp
val transString : string -> exp
val transSimpleVar : access * level -> exp
val transFieldVar : exp * int -> exp
val transSubscriptVar : exp * exp -> exp

val procEntryExit : {level: level, body: exp} -> unit
val getResult : unit -> F.frag list

end