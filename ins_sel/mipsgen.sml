structure MipsGen : CODEGEN =
struct

structure T = Tree
structure Frame = MipsFrame
structure A = Assem

fun codegen (frame) (stm: Tree.stm) : A.instr list = 
    let val ilist = ref (nil: A.instr list)
        fun emit x = ilist := x :: !ilist 
        fun result(gen) = let val t = Temp.newtemp() in gen t; t end

        fun getRelop T.EQ = "beq"
          | getRelop T.NE = "bne"
          | getRelop T.LT = "blt"
          | getRelop T.GT = "bgt"
          | getRelop T.LE = "ble"
          | getRelop T.GE = "bge"

        fun getBinop T.PLUS = "add"
          | getBinop T.MINUS = "sub"
          | getBinop T.MUL = "mul"
          | getBinop T.DIV = "div"

        (*use canon module to simply trees before applying codegen to them, use format function to translate assem trees to mips assembly, pass Temp.makestring to format as the trans function from temp to strings*)
        fun munchStm(stm) = stm
        and munchExp(exp) = exp
        and munchArgs(index, args) = index
    in munchStm stm;
        rev(!ilist)
    end
end