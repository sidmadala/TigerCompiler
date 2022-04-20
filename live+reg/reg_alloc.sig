signature REG_ALLOC = 
sig
    structure Frame : FRAME
    type allocation = string Temp.Table.table
    (* igraph goes in, allocation table and boolean on whether it spills comes out *)
    val alloc : Liveness.igraph -> 
        	    allocation * bool
end