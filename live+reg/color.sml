structure Color : COLOR = 
struct 

type allocation = string Temp.Table.table

(*helper to check if there are more nodes to simplify, if yes, returns SOME node to simplify, if no returns NONE*)
fun canSimplify() = ()

(*helper to actually pick a color to .. color the node with*)
fun chooseColor() = ()

(*something like this?*)
fun color{interference as Liveness.IGRAPH{graph, tnode, gtemp, moves}, initial, spillCost, regs} = 
    case canSimplify(Graph.nodes(graph), initial, moves, List.length(regs)) of
      SOME(node) =>
        (*remove actual node to simplify, recolor (recursively removing nodes), color at the end!*)
        (let
            (*wtf graph has a remove node though is this supposed to be funcgraph?*)
            val newGraph = Graph.removeNode(graph, node)
            val (alloc, spills) = color({interference = Liveness.IGRAPH{graph = newGraph,
										 tnode = tnode,
										 gtemp = gtemp,
										 moves = moves},
                                         initial = initial,
                                         spillCost = spillCost,
						                 registers = regs})
        in
            case chooseColor(node, graph, alloc, regs) of 
                SOME(clr) => (Temp.Table.enter(alloc, node, clr), spills)
              | NONE => (alloc, node :: spills)
        end)
    (*now, if we can't actually simplify anything, we just .. delete one and see what happens basically (hope it does not spill)*)
    | NONE => (Temp.Table.empty, [])

end
