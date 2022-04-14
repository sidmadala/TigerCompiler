structure Reg_Alloc : REG_ALLOC = 
struct

structure Frame = MipsFrame
type allocation = string Temp.Table.table

val initialAllocation : allocation = 
    let fun add2table((reg, str), table) = Temp.Table.enter(table, reg, str)
    in
        foldl add2table Temp.Table.empty Frame.allregs
    end

val regs = 
    let fun add2list((reg, str), l) = str :: l
    in 
        foldl add2list [] Frame.allregs
    end

fun spillCost node = 1 

fun alloc(igraph) = 
    let
      val (allocated, spillList) = Color.color{interference=igraph, 
                                               initial=initialAllocation, 
                                               spillCost=spillCost, 
                                               registers=regs}
    in
      (allocated, List.length(spillList) > 0)
    end

end 