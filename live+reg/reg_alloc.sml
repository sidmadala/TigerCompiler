structure Reg_Alloc : REG_ALLOC = 
struct

structure F = MipsFrame
type allocation = string Temp.Table.table

val initialAllocation() = ()

val regs = 
    let fun add2list((reg, str), l) = str :: l
    in 
        foldl add2list [] (F.specialregs @ F.argregs @ F.callersaves @ F.calleesaves)
    end

fun alloc(igraph) = 
    let
      val (allocated, spillList) = Color.color{intereference=igraph, 
                                               initial=initialAllocation, 
                                               spillCost=(fn x => 1), 
                                               registers=regs}
    in
      (allocated, List.length(spillList) > 0)
    end

end 