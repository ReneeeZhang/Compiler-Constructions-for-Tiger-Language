structure MakeGraph :> 
sig
  	val instrs2graph : Assem.instr list -> Flow.flowgraph
end =
struct
	structure F = Flow
	structure A = Assem
	fun instrs2graph insns =
		let val initControl = F.Graph.empty
			val initDef = F.LabelMap.empty
			val initUse = F.LabelMap.empty
			val initIsmove = F.InsnMap.empty
			fun generateFlowGraphHelper(insns, label, (ans as {control, def, use, ismove})) = 
					(*  addFallThruEdge is to deal with the removal of jump op after linearization. For instancs:
						...
						L7:
						ADDI t133, t130, 1
						ADD t130, t133, r0
						L8:
						ADDI t134, r0, 12
						BEQ t130, t134, L5
						...
						L7 block does not have a jump at the end so that no jump field can be used to build edges. 
						To address it, we have to detect such cases and add an edge from L7 to L8 *)
				let fun addFallThruEdge(currInsn, restInsns, baseControl) = 
						let fun add(restInsns, baseControl) = 
								case restInsns of
									A.LABEL({assem, lab})::_ => if F.LabelMap.inDomain(def, lab)
																then F.Graph.addEdge(baseControl, {from=label, to=lab})
																else let val updatedControl = F.Graph.addNode(baseControl, lab, [])
																	in
																		F.Graph.addEdge(updatedControl, {from=label, to=lab})
																	end
						  		  | _ => baseControl
						in
							case currInsn of
							  	A.OPER({assem, dst, src, jump}) => (case jump of
																		SOME(_) => baseControl
																	  | NONE => add(restInsns, baseControl))
							  | A.LABEL(_) => baseControl
							  | A.MOVE(_) => add(restInsns, baseControl)
						end
						
					(*  Because each OPER with a non-NONE jump field is responsible for setting up edges,
						handleLabel never adds edges. 
						It only adds node when the label that it is handling is not in the graph. 
						Otherwise, the node associated with that label has already been set up by handleOper *)
					fun handleLabel({assem, lab}, restInsns) = 
						if not(F.LabelMap.inDomain(def, lab))
						then let val updatedControl = F.Graph.addNode(control, lab, [])
								 val updatedDefMap = F.LabelMap.insert(def, lab, Temp.Set.empty)
								 val updatedUseMap = F.LabelMap.insert(use, lab, Temp.Set.empty)
								 val ans' = {control=updatedControl, def=updatedDefMap, use=updatedUseMap, ismove=ismove} (* TODO: Maybe something wrong with ismove*)
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
								val updatedUseMap = if Temp.Set.member(currDefSet, src)
													then use 
													else F.LabelMap.insert(use, label, Temp.Set.add(currUseSet, src))
								(* Update def: directly add *)
								val updatedDefMap = F.LabelMap.insert(def, label, Temp.Set.add(currDefSet, dst))
								(* Update ismove: set to true *)
								val updatedIsMove = F.InsnMap.insert(ismove, assem, true)
								(* Update control graph: add this instruction to the node if dst <> src *)
								val l = F.Graph.nodeInfo(F.Graph.getNode(control, label))
								val updatedControl' = 	if dst = src (* Delete this move if dst and src are the same *)
														then control
														else F.Graph.changeNodeData(control, label, l@[currInsn])
								val updatedControl = addFallThruEdge(currInsn, restInsns, updatedControl')
								val ans' = {control=updatedControl, def=updatedDefMap, use=updatedUseMap, ismove=updatedIsMove}
							in
								generateFlowGraphHelper(restInsns, label, ans')
							end (* End handleMove's else *)

					fun handleOper({assem, dst, src, jump}, currInsn, restInsns) = 
						let (* Fetch current use and def sets *)
							val currUseSet = F.LabelMap.lookup(use, label)
							val currDefSet = F.LabelMap.lookup(def, label)
							fun updateBasedOnJump init =
								case jump of
									NONE => init
								  | SOME(jumptoes) => foldl (fn(j, ans) => 	if F.LabelMap.inDomain(def, j)
																			then ans
																			else F.LabelMap.insert(ans, j, Temp.Set.empty))
															init jumptoes
							(* Update use: fold src, which is a list, to currUseSet.
								Likewise, eliminate use-after-def *)
							val updatedUseSet = foldl (fn (x, ans) => if Temp.Set.member(currDefSet, x)
																	  then ans
																	  else Temp.Set.add(ans, x)) currUseSet src
							val updatedUseMap' = F.LabelMap.insert(use, label, updatedUseSet)
							val updatedUseMap = updateBasedOnJump(updatedUseMap')
							(* Update def: add all from dst to currDefSet *)
							val updatedDefSet = foldl (fn (x, ans) => Temp.Set.add(ans, x)) currDefSet dst
							val updatedDefMap' = F.LabelMap.insert(def, label, updatedDefSet)
							val updatedDefMap = updateBasedOnJump(updatedDefMap')
							(* Update ismove: set to false *)
							val updatedIsMove = F.InsnMap.insert(ismove, assem, false)
							(* Update control graph: add this instruction to the node, which is a basic block *)
							val l = F.Graph.nodeInfo(F.Graph.getNode(control, label))
							val updatedControl'' = F.Graph.changeNodeData(control, label, l@[currInsn])
							val updatedControl' = addFallThruEdge(currInsn, restInsns, updatedControl'')
							(* Build up edges based on jump. Because of this jump list, handleOper is responsible for adding edges since jump
							denotes the "to" of an edge *)
							val updatedControl = 	case jump of
														NONE => updatedControl'
													  | SOME(jumptoes) => 
													  	let fun buildEdge(j, ans) = 
														  	if F.LabelMap.inDomain(def, j)
															then F.Graph.addEdge(ans, {from=label, to=j})
															else let val ans' = F.Graph.addNode(ans, j, [])
																 in
																 	F.Graph.addEdge(ans', {from=label, to=j})
																 end
														in
															foldl buildEdge updatedControl' jumptoes
														end
														
							val ans' = {control=updatedControl, def=updatedDefMap, use=updatedUseMap, ismove=updatedIsMove}
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

			fun generateFlowGraph(insns, ans) =
				generateFlowGraphHelper(insns, Symbol.symbol(""), ans)
				(* let val (fgraph as {control, def, use, ismove}) = generateFlowGraphHelper(insns, Symbol.symbol(""), ans)
				in
					(fgraph, F.Graph.nodes(control))
				end *)

		in
			generateFlowGraph(insns, {control=initControl, def=initDef, use=initUse, ismove=initIsmove})
		end (* End instr2graph *)
									  	
end
