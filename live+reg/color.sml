structure Color : COLOR = 
struct 

type allocation = string Temp.Table.table
val nodes = Graph.nodes graph
val nodesOnly = map (fn(node) => Graph.exposeNode'(node)) nodes
val nodeCount = List.length(nodes)
val adjList = Array.array(nodeCount, [])
val degree = Array.array(nodeCount, 0)

val simplifyWorklist: Graph.node' list ref = ref []
val selectStack: Graph.node' list ref = ref []
val coloredNodes: Graph.node' list ref = ref []

fun setup() = map (
  fn node => (
    Array.update(adjList, Graph.getNode(node), map Graph.getNode(Graph.adj(node)));
    Array.update(degree, Graph.getNode(node), List.length(Graph.adj(node)))
  )
)(nodes)

val regCount = 24 (* number of colors *)
fun makeWorklist(node::rest) = (
    makeWorklist(rest); 
    if (Array.sub(degree, node) < regCount) 
    then simplifyWorklist := node::(!simplifyWorklist)
    else () )
  | makeWorklist([]) = ()

(*helper to check if there are more nodes to simplify, if yes, returns SOME node to simplify, if no returns NONE*)
fun canSimplify() = ()

(*helper to actually pick a color to .. color the node with*)
fun chooseColor(_, _, _, []) = NONE
  | chooseColor(id, graph, curAlloc, curColor::list) =
    let 
      fun checkNeighbors (neighbor, state) = state orelse
					       case Temp.Table.look(curAlloc, neighbor) of
						   SOME neighborColor => (curColor = neighborColor)
						 | NONE => false
    val failedColoring = foldl checkNeighbors false (Graph.adj(Graph.getNode(graph, id)))
    in
      if failedColoring
      then getColor(id, graph, curAlloc, list)
      else SOME(curColor)
  end 



(*something like this?*)
fun color{interference as Liveness.IGRAPH{graph, tnode, gtemp, moves}, initial, spillCost, regs} = 
    case canSimplify(Graph.nodes(graph), initial, moves, List.length(regs)) of
      SOME(node) =>
        (*remove actual node to simplify, recolor (recursively removing nodes), color at the end!*)
        (let
            (*wtf graph has a remove node though is this supposed to be funcgraph?*)
            val newGraph = Graph.delete(graph, node)
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
