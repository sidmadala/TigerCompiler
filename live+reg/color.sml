structure Color : COLOR = 
struct 

type allocation = string Temp.Table.table

fun color{interference as Liveness.IGRAPH{graph, tnode, gtemp, moves}, initial, spillCost, regs} = 
  let 
    val simplifyWorklist: Graph.node' list ref = ref []
    (*sigh doing this imperatively instead of functionally means a bunch of refs*)

    (*list above containing all non-precolored, nonsimplified nodes of degree less than K. 
    we are willing to look through all the nodes in the original graph for a spill
    candidate every time the simplifyWorklist becomes empty.*)

    (*stack containing removed temps from graph*)
    val selectStack: Graph.node' list ref = ref []
    (*successfully colored nodes!*)
    val coloredNodes: Graph.node' list ref = ref []

    val K = 24 (* number of colors *)
    val nodes = Graph.nodes graph
    val nodes' = map (fn(node) => Graph.getNode(node)) nodes
    val nodeCount = List.length(nodes)

    (*graph representation*)
    (*list of list of adjacent int nodes*)
    val adjList = Array.array(nodeCount, [])
    (*list of list of node degrees*)
    val degrees = Array.array(nodeCount, 0)

    (*alloc to return at the end*)
    val alloc : allocation ref = ref initial

    (*equivalent of build procedure, create degree and adjlist arrays*)
    fun setup(graph) = map (
      fn node => (
        Array.update(degrees, Graph.getNode(node), List.length(Graph.adj(node)));
        Array.update(adjList, Graph.getNode(node), map Graph.getNode(Graph.adj(node)))
      ))(nodes)

    (*create simplify worklist -> add only nodes with degree less than K*)
    fun createSimplifyWorklist(n::l) = (
       (if (Array.sub(degrees, n) < K) 
        then simplifyWorklist := node::(!simplifyWorklist)
        else ()); 
        createSimplifyWorklist(l)
      )
      | createSimplifyWorklist([]) = ()

    (*decrement given node degree, if degree is < K, then add to simplifyWorklist*)
    fun decDegree(node) = 
      let 
        val oldDegree = Array.sub(degrees, node)
      in
        Array.update(degrees, node, oldDegree-1);
        if oldDegree = K then simplifyWorklist := node :: (!simplifyWorklist) else ()
      end 

    (*TODODODOSFKSADOKFAJKWEFHASDKJFHJASDFFFFJSDJDSF*)
    (*helper to check if there are more nodes to simplify, if yes, returns SOME node to simplify, 
    if no returns NONE*)
    fun simplify([]) = ()
      | simplify(n::simpWL) = 
            let

            in
              simplifyWorklist := simpWL
              selectStack := node :: !selectStack
              map decDegree (getAdjacent(node))
              simplify(!simplifyWorklist)
            end

    (*helper to actually pick a color to .. color the node with -> this modifies alloc to return at the end*)
    fun chooseColors([]) = ()
      | chooseColors(n::selstack) =
        let 
          val availableColors = (*list of colors (registers) available, check each node in adjacency list...*)
        in
          (case Temp.Table.look((!alloc), Graph.constructNode(graph, node)) of 
            SOME color => ()
          | NONE => );
          chooseColor(selstack)
              (*need to deal with this*)
        end 
    (*etjsadjfhawejhfaTODODOODODODOODO*)
    (*runs all the helpers .. lmao*)
    fun run() = 
      ( 
        setup(graph);
        createSimplifyWorklist(nodes');
        simplify(!simplifyWorklist);
        chooseColors(!selectStack)
      )
  in
    run();
    !alloc
  end
end
