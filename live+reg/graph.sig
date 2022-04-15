signature GRAPH =
sig
    type node' = int
    type graph
    type node

    structure Table : TABLE 
    sharing type Table.key = node
    
    val nodes: graph -> node list
    val succ: node -> node list
    val pred: node -> node list
    val adj: node -> node list   (* succ+pred *)
    val eq: node*node -> bool

    val newGraph: unit -> graph
    val newNode : graph -> node
    exception GraphEdge
    val mk_edge: {from: node, to: node} -> unit
    val rm_edge: {from: node, to: node} -> unit
    val rm_edge_catch_exp: {from: node, to: node} -> unit
    val getNode: node -> node'
    val createNode: graph * node' -> node
    
    val nodename: node->string 

end
