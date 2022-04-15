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
			igraph * (Graph.node -> Temp.temp list)
	
	val show : TextIO.outstream * igraph -> unit
end =
struct
	(* Body *)
	structure A = Assem
	structure F = Flow
	structure G = Graph
	structure S = Symbol
	structure BST = IntRedBlackSet
    datatype igraph =
	(*changed all the IGraph to Graph*)
		IGRAPH of {graph: Graph.graph,
			   	   tnode: Temp.temp -> Graph.node,
			       gtemp: Graph.node -> Temp.temp,
				   moves: (Graph.node * Graph.node) list}
	fun interferenceGraph(F.FGRAPH{control, def, use, ismove}) =
		let
			exception NodeNotFound
			fun node2value(s, node) = case G.Table.look(s, node) of SOME value => value | NONE => raise NodeNotFound
			fun checkEqual(table, table') = foldr (fn(a, ans) => ans andalso (BST.equal(node2value(table, a), node2value(table', a)))) true (G.nodes(control))
			fun init(control, def, use, ismove) = 
				let
					val liveIn = foldr (fn(a, ans) => G.Table.enter(ans, a, BST.empty)) G.Table.empty (G.nodes(control))
					val liveOut = foldr (fn(a, ans) => G.Table.enter(ans, a, BST.empty)) G.Table.empty (G.nodes(control))
					fun iterateToFixedPoint(liveIn, liveOut) =
						let
							val updatedLiveIn = foldr (fn(a, ans) =>
                            G.Table.enter(ans, a,
                            BST.union(BST.difference(node2value(liveOut, a),
                            BST.fromList(node2value(def, a))),
                            BST.fromList(node2value(use, a))))) G.Table.empty (G.nodes(control)) 
							val updatedLiveOut = foldr (fn(a, ans) => G.Table.enter(ans, a, (foldr (fn(a, ans) => BST.union(ans, node2value(updatedLiveIn, a))) BST.empty (G.succ(a))))) G.Table.empty (G.nodes(control)) 
						in
							if checkEqual(liveIn, updatedLiveIn) andalso
                            checkEqual(liveOut, updatedLiveOut) then (fn(node)
                            => BST.listItems(node2value(updatedLiveOut, node))) else iterateToFixedPoint(updatedLiveIn, updatedLiveOut)
						end
				in
					iterateToFixedPoint(liveIn, liveOut)
				end
			val node2templist = init(control, def, use, ismove)
			fun makeIGraph() = 
				let
					val tempToNodeMap = ref Temp.Table.empty
					val nodeToTempMap = ref G.Table.empty

					fun contains(map, key) = case G.Table.look(map, key) of SOME value => true | NONE => false

					exception TempNotFound

					fun temp2node temp = case Temp.Table.look(!tempToNodeMap, temp) of SOME node => node | NONE => raise TempNotFound
					fun node2temp node = case G.Table.look(!nodeToTempMap, node) of SOME temp => temp | NONE => raise NodeNotFound

					fun initHelper(graph, []) = graph
					  | initHelper(graph, temp::tail) = 
					  		case Temp.Table.look(!tempToNodeMap, temp) of
					  			NONE => 
						  			let
						  				val newNode = G.newNode(graph)
						  			in
						  				G.Table.enter(!nodeToTempMap, newNode, temp);
						  				Temp.Table.enter(!tempToNodeMap, temp, newNode);
						  				initHelper(graph, tail)
						  			end
					  		  | SOME(node) => initHelper(graph, tail)

					fun initVertex(graph, []) = graph
					  | initVertex(graph, cur::rest) = (
					  		case G.Table.look(def, cur) of NONE => graph | SOME(temps) => initHelper(graph, temps);
					  		case G.Table.look(use, cur) of NONE => graph | SOME(temps') => initHelper(graph, temps');
					  		initVertex(graph, rest)
					  	)
					  
							
					val ig = initVertex(G.newGraph(), G.nodes(control))

					fun addEdge(graph) = 
						let
							fun interfereHelper(graph, node) =
								let
									val deflist = map temp2node (case G.Table.look(def, node) of NONE => [] | SOME(temps) => temps)
									val liveoutlist = map temp2node (node2templist node)
									fun connect(a, b) = if G.eq(a, b) then ()
                             else (G.rm_edge_catch_exp{from = a, to = b};
                             G.rm_edge_catch_exp{from = b, to = a};
                             G.mk_edge{from = a, to = b})
								in
									foldl (fn(a, ans) => (foldl
                                    (fn(b,c)=>connect(a,b)) () liveoutlist)) () deflist
								end
							fun interfere(graph, []) = graph
							  | interfere(graph, cur::rest) = (interfereHelper(graph, cur); interfere(graph, rest))
						in
							interfere(graph, G.nodes(control))
						end

					val ans = addEdge(ig)
					val moves = foldl (fn(a, ans) => case G.Table.look(ismove,
                    a) of NONE => raise NodeNotFound | SOME(false) => ans |
                    SOME(true) => ans @ [(temp2node(hd (node2value(def, a))),
                    temp2node (hd (node2value(use, a))))]) [] (G.nodes(control))
				in
					(IGRAPH{graph = ans, tnode = temp2node, gtemp = node2temp, moves = moves}, node2templist)
				end
		in
			makeIGraph()	
		end

	(*not implemented yet*)
	fun show (outstream, IGRAPH{graph, tnode, gtemp, moves}) =
		let
			fun showNode(node) = TextIO.output(outstream, Temp.makestring(gtemp(node)))
			fun showAdj([]) = ()
			  | showAdj(cur::rest) = (showNode(cur); TextIO.output(outstream, ", "); showAdj(rest))
			fun showNodes([]) = ()
			  | showNodes(cur::rest) = (showNode(cur); TextIO.output(outstream, ": "); showAdj(G.adj(cur)); TextIO.output(outstream, "\n"); showNodes(rest))
		in
			showNodes(G.nodes(graph))
		end
end
