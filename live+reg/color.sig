signature COLOR = 
sig
    type allocation = string Temp.Table.table
    val color: {interference: Liveness.igraph,
		initial: allocation,
		spillCost: Graph.node -> int,
		registers: string list}
	       -> allocation * Temp.temp list
end