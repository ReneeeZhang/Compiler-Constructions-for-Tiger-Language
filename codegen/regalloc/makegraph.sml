structure MakeGraph :> 
sig
  	val instrs2graph : Assem.instr list -> Flow.flowgraph * Flow.Graph.node list
end =
struct
	structure F = Flow
	structure A = Assem
	fun instrs2graph insns =
		let fun generateFlowGraph(insns, label, (ans as F.flowgraph({control, def, use, ismove}))) = 
				let fun handleLabel({_, lab}, restInsns) = 
						if F.Graph.hasNode(control, lab) (* It means that label can't be ""; and we don't have to addNode *)
						then let val updatedControl = F.Graph.addEdge(control, {from=label, to=lab})
									val ans' = {control=updatedControl, def=def, use=use, ismove=ismove}
							 in
								generateFlowGraph(restInsns, lab, ans)
							 end
						(* If the graph doesn't have that node, the predecessor can be dummy because of the initial setting *)
						else if Symbol.name(label) = "" (* Initial dummy label, encountering the first label of the program *)
							then let val updatedControl = F.Graph.addNode(control, lab, [])
									val ans' = {control=updatedControl, def=def, use=use, ismove=ismove}
								in
									generateFlowGraph(restInsns, lab, ans')
								end
							else let val updatedControl' = F.Graph.addNode(control, lab, [])
									val updatedControl = F.Graph.addEdge(updatedControl', {from=label, to=lab})
									val ans' = {control=updatedControl, def=def, use=use, ismove=ismove}
								in
									generateFlowGraph(restInsns, lab, ans')
								end

					fun handleMove({assem, dst, src}, currInsn, restInsns) =
						if dst = src (* If they are the same, just delete this MOVE *)
						then generateFlowGraph(restInsns, label, ans)
						else let val currUseList = F.LabelMap.lookup(src, label)
								 val currDefList = F.LabelMap.lookup(def, label)
								 val updatedUseMap = if List.exists (fn temp => temp = src) currDefList
								 				      then use 
													  else F.LabelMap.insert(use, label, src::currUseList)
								 val updatedDefMap = F.LabelMap.insert(def, label, dst::currDefList)
								 val updatedIsMove = F.InsnMap.insert(ismove, assem, true)
								 val (_, l, _, _) = F.Graph.getNode(control, label)
								 val updatedControl = F.Graph.changeNodeData(control, label, l@[currInsn])
								 val ans' = F.flowgraph({control=updatedControl, def=updatedDefMap, use=updatedUseMap, ismove=updatedIsMove})
							in
								generateFlowGraph(restInsns, label, ans')
							end
				in
					case insns of
						[] => ans
					  | insn::insns' => case insn of
											A.LABEL(l) => handleLabel(l, insns')
										  | A.MOVE(m) => handleMove(m, insn, insns')
										  | A.OPER({assem, dst, src, jump}) =>
										  	let val currUseList = F.LabelMap.lookup(src, label)
												val currDefList = F.LabelMap.lookup(def, label)
												val updatedUseList = if List.exists (fn temp => temp = src) currDefList
												  					 then currSrcList
												  					 else F.LabelMap.insert(src, label, currUseList@src)
							 					val updatedDefList = F.LabelMap.insert(def, label, currDefList@dst)
												val updatedControl = F.Graph.add
									  	
end
