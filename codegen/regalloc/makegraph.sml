structure MakeGraph :> 
sig
  	val instrs2graph : Assem.instr list -> Flow.flowgraph (* * Flow.Graph.node list *)
end =
struct
	structure F = Flow
	structure A = Assem
	fun instrs2graph insns =
		let val initControl = F.Graph.empty
			val initDef = F.LabelMap.empty
			val initUse = F.LabelMap.empty
			val initIsmove = F.InsnMap.empty
			fun generateFlowGraphHelper(insns, label, (ans as F.FGRAPH({control, def, use, ismove}))) = 
				(* Because each OPER with a non-NONE jump field is responsible for setting up edges,
				   handleLabel never adds edges. 
				   It only adds node when the label that it is handling is not in the graph. 
				   Otherwise, the node associated with that label has already been set up by handleOper *)
				let fun handleLabel({assem, lab}, restInsns) = 
						if not(F.Graph.hasNode(control, lab))
						then let val updatedControl = F.Graph.addNode(control, lab, [])
								 val ans' = F.FGRAPH{control=updatedControl, def=def, use=use, ismove=ismove}
							in
								generateFlowGraphHelper(restInsns, lab, ans')
							end
						else generateFlowGraphHelper(restInsns, lab, ans)
						
						
					fun handleMove({assem, dst, src}, currInsn, restInsns) =
						if dst = src (* If they are the same, just delete this MOVE *)
						then generateFlowGraphHelper(restInsns, label, ans)
						else let (* Fetch current use and def sets *)
								val currUseSet = F.LabelMap.lookup(use, label)
								val currDefSet = F.LabelMap.lookup(def, label)
								(* Update use: if a use appears after a def to the same variable(temp), ignore it.
								Note: use currDefSet *)
								val updatedUseMap = if F.TempSet.member(currDefSet, src)
													then use 
													else F.LabelMap.insert(use, label, F.TempSet.add(currUseSet, src))
								(* Update def: directly add *)
								val updatedDefMap = F.LabelMap.insert(def, label, F.TempSet.add(currDefSet, dst))
								(* Update ismove: set to true *)
								val updatedIsMove = F.InsnMap.insert(ismove, assem, true)
								(* Update control graph: add this instruction to the node if dst <> src *)
								val l = F.Graph.nodeInfo(F.Graph.getNode(control, label))
								val updatedControl = 	if dst = src (* Delete this move if dst and src are the same *)
														then control
														else F.Graph.changeNodeData(control, label, l@[currInsn])
								val ans' = F.FGRAPH({control=updatedControl, def=updatedDefMap, use=updatedUseMap, ismove=updatedIsMove})
							in
								generateFlowGraphHelper(restInsns, label, ans')
							end (* End handleMove's else *)

					fun handleOper({assem, dst, src, jump}, currInsn, restInsns) = 
						let (* Fetch current use and def sets *)
							val currUseSet = F.LabelMap.lookup(use, label)
							val currDefSet = F.LabelMap.lookup(def, label)
							(* Update use: fold src, which is a list, to currUseSet.
								Likewise, eliminate use-after-def *)
							val updatedUseSet = foldl (fn (x, ans) => if F.TempSet.member(currDefSet, x)
																	  then ans
																	  else F.TempSet.add(ans, x)) currUseSet src
							val updatedUseMap = F.LabelMap.insert(use, label, updatedUseSet)
							(* Update def: add all from dst to currDefSet *)
							val updatedDefSet = foldl (fn (x, ans) => F.TempSet.add(ans, x)) currDefSet dst
							val updatedDefMap = F.LabelMap.insert(def, label, updatedDefSet)
							(* Update ismove: set to false *)
							val updatedIsMove = F.InsnMap.insert(ismove, assem, false)
							(* Update control graph: add this instruction to the node, which is a basic block *)
							val l = F.Graph.nodeInfo(F.Graph.getNode(control, label))
							val updatedControl' = F.Graph.changeNodeData(control, label, l@[currInsn])
							(* Build up edges based on jump. Because of this jump list, handleOper is responsible for adding edges since jump
							denotes the "to" of an edge *)
							val updatedControl = 	case jump of
														NONE => updatedControl'
													| SOME(jumptoes) => 
														let fun buildEdges(jumptoes, ans) = (* Tail recursion *)
															case jumptoes of
																[] => ans
															| jumpto::jumptoes' => 
																if F.Graph.hasNode(updatedControl', jumpto)
																then buildEdges(jumptoes', F.Graph.addEdge(ans, {from=label, to=jumpto}))
																else 	let val ans' = F.Graph.addNode(ans, jumpto, [])
																		in
																			buildEdges(jumptoes', F.Graph.addEdge(ans', {from=label, to=jumpto}))
																		end
														in
															buildEdges(jumptoes, updatedControl')
														end
							val ans' = F.FGRAPH({control=updatedControl, def=updatedDefMap, use=updatedUseMap, ismove=updatedIsMove})
						in
							generateFlowGraphHelper(restInsns, label, ans')
						end (* End handleOper *)

				in
					case insns of
						[] => ans
					| insn::insns' => case insn of
											A.LABEL(l) => handleLabel(l, insns')
										| A.MOVE(m) => handleMove(m, insn, insns')
										| A.OPER(oper) => handleOper(oper, insn, insns')
				end (* End generateGraphHelper *)

			fun generateFlowGraph(insns, (ans as F.FGRAPH({control, def, use, ismove}))) =
				generateFlowGraphHelper(insns, Symbol.symbol(""), ans)

		in
			generateFlowGraph(insns, F.FGRAPH({control=initControl, def=initDef, use=initUse, ismove=initIsmove}))
		end (* End instr2graph *)
									  	
end
