structure MipsGen : CODEGEN =
struct

structure T = Tree
structure Frame = MipsFrame
structure A = Assem

fun codegen (frame) (stm: Tree.stm) : A.instr list = 
    let val ilist = ref (nil: A.instr list)
        fun emit x = ilist := x :: !ilist 
        fun result(gen) = let val t = Temp.newtemp() in gen t; t end
        (*use canon module to simply trees before applying codegen to them, use format function to translate assem trees to mips assembly, pass Temp.makestring to format as the trans function from temp to strings*)
        fun munchStm(stm) = stm
        and munchExp(exp) = exp
    in munchStm stm;
        rev(!ilist)
    end
end