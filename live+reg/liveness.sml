structure Liveness:
sig
	datatype igraph =
	(*changed all the IGraph to Graph*)
		IGRAPH of {graph: Graph.graph,
			   	   tnode: Temp.temp -> Graph.node,
			       gtemp: Graph.node -> Temp.temp,
				   moves: (Graph.node * Graph.node) list}
	val interferenceGraph :
		Flow.flowgraph ->
			igraph * (Flow.Graph.node -> Temp.temp list)
	
	val show : outstream * igraph -> unit
end =
struct
	(* Body *)
	structure A = Assem
	structure F = Flow
	structure G = Graph
	structure S = Symbol

	fun interferenceGraph flowgraph =

	(*not implemented yet*)
	fun show (outstream, igraph) = ()
end
