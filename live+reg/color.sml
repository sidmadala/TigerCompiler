structure Color : COLOR = 
struct 

type allocation = string Temp.Table.table

fun color{interference as Liveness.IGRAPH{graph, tnode, gtemp, moves}, initial, spillCost, registers} = 
  let 
    val simplifyWorklist: Graph.node' list ref = ref []
    (*sigh doing this imperatively instead of functionally means a bunch of refs*)

    (*list above containing all non-precolored, nonsimplified nodes of degree less than K. 
    we are willing to look through all the nodes in the original graph for a spill
    candidate every time the simplifyWorklist becomes empty.*)

    (*stack containing removed temps from graph*)
    val selectStack: Graph.node' list ref = ref []

    val K = 24 (* number of colors *)
    val nodes: Graph.node list = Graph.nodes graph
    val nodes': Graph.node' list = map (fn(n) => Graph.getNode(n)) nodes
    val nodeCount = List.length(nodes)

    (*graph representation*)
    (*list of list of adjacent int nodes*)
    val adjList = Array.array(nodeCount, [])
    (*list of list of node degrees*)
    val degrees = Array.array(nodeCount, 0)

    (*alloc to return at the end*)
    val alloc : allocation ref = ref initial

    (*equivalent of build procedure, create degree and adjlist arrays as well as simplify worklist*)
    fun setup(graph) = 
      let 
        (*create simplify worklist -> add only nodes with degree less than K*)
        fun createSimplifyWorklist(n::l) = (
          (if (Array.sub(degrees, n) < K) 
            then simplifyWorklist := n::(!simplifyWorklist)
            else ()); 
            createSimplifyWorklist(l)
          )
          | createSimplifyWorklist([]) = ()
      in 
      map (
        fn node => (
          Array.update(degrees, Graph.getNode(node), List.length(Graph.adj(node)));
          Array.update(adjList, Graph.getNode(node), (map Graph.getNode (Graph.adj(node))))
        )
      )(nodes);
      createSimplifyWorklist(nodes')
      end

    (*decrement given node degree, if degree is < K, then add to simplifyWorklist*)
    fun decDegree(node) = 
      let 
        val oldDegree = Array.sub(degrees, node)
      in
        Array.update(degrees, node, oldDegree-1);
        if oldDegree = K then simplifyWorklist := node :: (!simplifyWorklist) else ()
      end 

    (*helper to check if there are more nodes to simplify, if yes, returns SOME node to simplify, 
    if no returns NONE*)
    fun simplify([]) = ()
      | simplify(n::l) = 
            let
              fun notInSelect(node) = not (List.exists (fn(ssNode) => ssNode = node) (!selectStack))
              (*getting adjacent nodes that haven't been eliminated yet (not in selectStack)*)
              fun getValidAdjacents(node) = List.filter notInSelect (Array.sub(adjList, node))
              val adjNodes = getValidAdjacents(n)
            in
              (*decrease degree of all adjacents (to remove node)*)
              map decDegree adjNodes;
              (*add to select stack, delete from simplifyWorklist*)
              selectStack := n :: !selectStack;
              simplifyWorklist := l;
              (*recursion!*)
              simplify(!simplifyWorklist)
            end

    (*helper to actually pick a color to .. color the node with -> this modifies alloc to return at the end*)
    fun chooseColors([]) = ()
      | chooseColors(n::selstack) =
        let 
          fun notCollidingColor(colorToTry) = List.exists (
                                              fn(adjN) =>
                                                case Temp.Table.look(!alloc, gtemp(Graph.createNode(graph, adjN))) of
                                                  SOME(color) => color <> colorToTry
                                                | NONE => true
                                              ) (Array.sub(adjList, n))

          val availableColors = List.filter notCollidingColor registers
          val nodeTemp = gtemp(Graph.createNode(graph, n))
        in
          (case Temp.Table.look((!alloc), nodeTemp) of 
            (*already colored, do nothing*)
            SOME(color) => ()
            (*hasn't been colored yet, color!*)
          | NONE => 
            case (availableColors) of 
              [] => ErrorMsg.impossible "spilling"
            | color::l => (alloc := Temp.Table.enter(!alloc, nodeTemp, color))
          );
          (*recurse!*)
          chooseColors(selstack)
        end 

    (*runs all the helpers .. lmao*)
    fun run() = 
      ( 
        setup(graph);
        simplify(!simplifyWorklist);
        chooseColors(!selectStack)
      )
  in
    run();
    (*empty list placeholding for if we want to implement spill lists*)
    (!alloc, [])
  end
end
