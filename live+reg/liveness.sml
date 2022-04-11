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
	structure BST = IntRedBlackSet

	fun interferenceGraph(F.FGRAPH{control, def, use, isMove}) =
		let
			exception NodeNotFound
			fun node2value(s, node) = case G.Table.lookup(node) of SOME value => value | NONE => raise NodeNotFound
			fun checkEqual(table, table') = foldr (fn(a, ans) => ans andalso (BST.equal(node2value(table, a), node2value(table', a)))) true (G.nodes(control))
			fun init(control, def, use, isMove) = 
				let
					val liveIn = foldr (fn(a, ans) => G.Table.enter(ans, a, BST.empty)) G.Table.empty (G.nodes(control))
					val liveOut = foldr (fn(a, ans) => G.Table.enter(ans, a, BST.empty)) G.Table.empty (G.nodes(control))
					fun iterateToFixPoint(liveIn, liveOut) =
						let
							val updatedLiveIn = foldr (fn(a, ans) => G.Table.enter(ans, a, BST.union(BST.difference(node2value(liveOut, a), node2value(def, a)), node2value(use, a)))) G.Table.empty (G.nodes(control)) 
							val updatedLiveOut = foldr (fn(a, ans) => G.Table.enter(ans, a, (foldr (fn(a, ans) => BST.union(ans, node2value(updatedLiveIn, a))) BST.empty (G.succ(a))))) G.Table.empty (G.nodes(control)) 
						in
							if checkEqual(liveIn, updatedLiveIn) andalso checkEqual(liveOut, updatedLiveOut) then (fn(node) => BST.toList(node2value(updatedLiveOut, node))) else iterateToFixPoint(updatedLiveIn, updatedLiveOut)
						end
				in
					iterateToFixPoint(liveIn, liveOut)
				end

		in
			
		end


	(*not implemented yet*)
	fun show (outstream, igraph) = ()
end
