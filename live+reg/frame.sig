signature FRAME = 
sig 
    (*having these mips specific things in the signature is arguably bad code abstraction and we should 
    just make getters in mips frame.. but i will do it later - michelle*)
    val FP : Temp.temp
    val RA : Temp.temp
    val V1 : Temp.temp
    val V0 : Temp.temp
    val RV : Temp.temp 
    val SP : Temp.temp

    val callersaves : (Temp.temp * string) list
    val calleesaves : (Temp.temp * register) list
    val argregs : (Temp.temp * string) list
    val specialregs : (Temp.temp * register) list

    type frame
    type access 
    datatype frag = PROC of {body: Tree.stm, frame: frame}
		  | STRING of Temp.label * string
    val wordSize : int
    val newFrame : {name: Temp.label, formals : bool list} -> frame
    val name : frame -> Temp.label
    val formals : frame -> access list 
    val allocLocal : frame -> bool -> access
    val externalCall: string * Tree.exp list -> Tree.exp
    val checkOffset: access -> Tree.exp -> Tree.exp
    val tempMap : string Temp.Table.table
    val getRegString : Temp.temp -> string
    val string : (Temp.label * string) -> string
    val getTempList : (Temp.temp * string) list -> Temp.temp list
end
