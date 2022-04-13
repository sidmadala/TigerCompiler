structure MakeGraph = 
struct
	structure A = Assem
	structure G = Graph
	structure F = Flow
	structure S = Symbol
	(* Body *)

	(*fun instrs2graph: Assem.instr list -> Flow.flowgraph * Flow.Graph.node list*)
	fun instrs2graph instrs = 
		let
			val labelToNodeMap = S.empty
			exception LabelNotFound
			fun label2node(lab) = case S.look(labelToNodeMap, lab) of SOME node => node | NONE => raise LabelNotFound
			fun initVertex(F.FGRAPH{control, def, use, ismove}, A.OPER{assem, dst, src, jump}, nodeList) =
					let
					 	val cur = G.newNode(control)
					 in
					 	(F.FGRAPH{control = control, def = G.Table.enter(def,
                        cur, dst), use = G.Table.enter(use, cur, src), ismove =
                        G.Table.enter(ismove, cur, false)}, nodeList @ [cur])
					 end 
			  | initVertex(F.FGRAPH{control, def, use, ismove}, A.LABEL{assem, lab}, nodeList) =
			  		let
			  			val cur = G.newNode(control)
			  		in
			  			S.enter(labelToNodeMap, lab, cur);
			  			(F.FGRAPH{control = control, def = G.Table.enter(def,
                        cur, []), use = G.Table.enter(use, cur, []), ismove =
                        G.Table.enter(ismove, cur, false)}, nodeList @ [cur])	
			  		end
			  | initVertex(F.FGRAPH{control, def, use, ismove}, A.MOVE{assem, dst, src}, nodeList) =
			  		let
			  			val cur = G.newNode(control)
			  		in
			  			(F.FGRAPH{control = control, def = G.Table.enter(def,
                        cur, [dst]), use = G.Table.enter(use, cur, [src]),
                        ismove = G.Table.enter(ismove, cur, true)}, nodeList @ [cur])
			  		end

			fun addEdge([cur], [A.OPER{assem, dst, src, jump = SOME(labelList)}])
              = (map (fn label => G.mk_edge{from = cur, to = label2node(label)}) labelList; ())
				(*Note: this list must explicitly include the next instruction if it is possible to fall through to it*)
				(*So I omitted G.mk_edge(cur, next)*)
			  | addEdge(cur::next::rest, A.OPER{assem, dst, src, jump =
              SOME(labelList)}::assemList) = (map (fn label => G.mk_edge{from =
              cur, to = label2node(label)}) labelList; addEdge(next::rest, assemList))
			  | addEdge(cur::next::rest, A.OPER{assem, dst, src, jump =
              NONE}::assemList) = (G.mk_edge{from = cur, to = next}; addEdge(next::rest, assemList))
			  | addEdge(cur::next::rest, noJump::assemList) = (G.mk_edge{from =
              cur, to = next}; addEdge(next::rest, assemList))
			  | addEdge(_, _) = ()
		in
			let 
				val ans = foldl (fn(a, ans) => initVertex(#1 ans, a, #2 ans))
                (F.FGRAPH{control = G.newGraph(), def = G.Table.empty, use =
                G.Table.empty, ismove = G.Table.empty}, []) instrs
			in
				addEdge(#2 ans, instrs);
				ans
			end
		end
end
