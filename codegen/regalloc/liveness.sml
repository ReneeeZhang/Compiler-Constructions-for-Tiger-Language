structure Liveness =
struct
    structure F = Flow
    structure T = Temp
    structure A = Assem

    type liveMap = T.Set.set F.LabelMap.map
	val updatedLivenessInLastIteration = ref true
	val blocksSeenInThisIteration: Flow.LabelMap.Key.ord_key list ref = ref []

	fun makeEmptyLiveMap(labelMap) = 
		let
			val liveness = F.LabelMap.empty
		in
			foldl (fn (newLabel, currentMap) => F.LabelMap.insert(currentMap, newLabel, T.Set.empty)) liveness (F.LabelMap.listKeys(labelMap))
		end;
	
	fun getLiveInTempsForBlock(control, liveness, blockLab, use, def) =
		let
			(* val _ = print("in getLiveInTempsForBlock\n") *)
			val blockUses = F.LabelMap.lookup(use, blockLab)
			val blockDefs = F.LabelMap.lookup(def, blockLab)
			val liveOuts = if (List.exists (fn (x) => x = blockLab) (!blocksSeenInThisIteration)) then F.LabelMap.lookup(liveness, blockLab) else getLiveOutTempsForBlock(control, liveness, blockLab, use, def)
			val outMinusDefs = T.Set.difference(liveOuts, blockDefs)
			val liveIns = T.Set.union(blockUses, outMinusDefs)
		in
			liveIns (* No need to check if there's a change, bc ins can only change if outs do *)
		end
		
	and getLiveOutTempsForBlock(control, liveness, blockLab, use, def) = 
		let
			(* val _ = print("in getLiveOutTempsForBlock\n") *)
			val blockNode = F.Graph.getNode(control, blockLab)
			val succs = F.Graph.succs(blockNode) (* Returns Temp.Label list *)
			val _ = if (List.exists (fn (x) => x = blockLab) (!blocksSeenInThisIteration)) then () else blocksSeenInThisIteration := blockLab::(!blocksSeenInThisIteration)
			(* val _ = print("succs length " ^ Int.toString(List.length(succs)) ^ "\n") *)
			(* This may not be required *)
			val succNodes = foldl (fn (newSucc, nodes) => F.Graph.getNode(control, newSucc)::nodes) [] succs
			val currLiveOut = F.LabelMap.lookup(liveness, blockLab)
			val newLiveOut = foldl (fn (succ, liveOut) => T.Set.union(liveOut, getLiveInTempsForBlock(control, liveness, succ, use, def))) T.Set.empty succs
		in
			(
				if (not (T.Set.equal(currLiveOut, newLiveOut))) then updatedLivenessInLastIteration := true else updatedLivenessInLastIteration := !updatedLivenessInLastIteration;
				newLiveOut
			)
		end;

	fun computeLivenessForIter(control, ogLiveness, def, use) =
		let 
			(* val _ = print("In computeLivenessForIter\n"); *)
			val liveness = foldl (
				fn (currentBlock, currentMap) => 
					F.LabelMap.insert(
						currentMap, currentBlock,
						getLiveOutTempsForBlock(control, currentMap, currentBlock, use, def)
						)
				) ogLiveness (F.LabelMap.listKeys(def))
			(* val _ = print("In computeLivenessForIter2\n"); *)
		in
			(blocksSeenInThisIteration := []; liveness)
		end;
	
	fun recursivelyComputeLiveness(control, liveness, def, use) =
		if !updatedLivenessInLastIteration then (updatedLivenessInLastIteration := false; recursivelyComputeLiveness(control, computeLivenessForIter(control, liveness, def, use), def, use)) else (updatedLivenessInLastIteration := true; liveness)

    fun calculateLiveness(control, def, use) = (* Temp.Set.set Flow.LabelMap.map * Temp.Set.set Flow.LabelMap.map -> liveMap *)
        let
			val liveness = makeEmptyLiveMap(def) (* Trust that def and use both have same key labels *)
		in
			recursivelyComputeLiveness(control, liveness, def, use)
		end;

    structure IGraph = FuncGraph(Temp.TempOrd) (* Each node in the IGraph takes a temp as the key. Note that a graph is basically a map (adjacency list) *)
    type igraph = {
        graph: Temp.temp IGraph.graph, (* Each temp is itself a node in the interference graph *)
        moves: (Temp.temp IGraph.node * Temp.temp IGraph.node) list
    }

    fun generateIGraph {control, def, use, ismove} = (* Flow.flowgraph -> igraph *)
        let val livenessInfo = calculateLiveness(control, def, use)
            val allLabels = F.LabelMap.listKeys(def)
            fun getLiveoutOfNode l = F.LabelMap.lookup(livenessInfo, l)
            fun getDefofNode l = F.LabelMap.lookup(def, l)

            (*  Update the interference graph by a label l, which indicates a node the updating is working on.
                This function is used in fold *)
            fun updateByNode(l, baseig) = 
                let val initLiv = getLiveoutOfNode l;
                    (* val initDefset = getDefofNode l *)
                    val insns = F.Graph.nodeInfo(F.Graph.getNode(control, l)) (* Contains dst as a list of def temps *)
                    val revInsns = List.rev insns

                    (*  Build edges between elements in def list given by insn and elements in liv
                        The same element in both def list and liv will be handled properly by IGraph.doubleEdge *)
                    fun addInterferenceEdgesPerInsn({graph=baseig, moves=m}, liv, insn) =
                            (* Add edges between a def temp d to every live temp in the liv set, forming a new igraph based on ig *)
                        let fun addInterferenceEdgeFromSingleDefTemp(d, ig) =
                                    (* Helper function - add undirected edges from d to all live temps except the one the same as d *)
                                let fun addTo initig =
                                        Temp.Set.foldl (fn (li, acc) => if IGraph.hasNode(acc, li) (* Check if li is in the igraph *)
                                                                        then IGraph.doubleEdge(acc, d, li)
                                                                        else let val igraphWithNewNode = IGraph.addNode(acc, li, li)
                                                                            in 
                                                                                IGraph.doubleEdge(igraphWithNewNode, d, li)
                                                                            end) initig liv
                                in
                                    if IGraph.hasNode(ig, d)
                                    then addTo ig
                                    else let val igraphWithNewNode = IGraph.addNode(ig, d, d)
                                        in
                                            addTo igraphWithNewNode
                                        end
                                end (* End addTo *)
                        in
                            case insn of
                                A.OPER{assem, dst, src, jump} => {graph=(foldl addInterferenceEdgeFromSingleDefTemp baseig dst), moves=m}
                              | A.MOVE{assem, dst, src} => {graph=addInterferenceEdgeFromSingleDefTemp(dst, baseig), moves=(dst, src)::m}
                              | A.LABEL(_) => {graph=baseig, moves=m} (* Won't happen *)
                        end (* End addInterferenceEdges *)

                    fun updateLiveness(insn, currLiv) = 
                        case insn of
                            A.OPER{assem, dst, src, jump} => 
                            let val updatedLiv' = foldl (fn (t, ans) => Temp.Set.subtract(ans, t)) currLiv dst
                                val updatedLiv = foldl (fn (t, ans) => Temp.Set.add(ans, t)) updatedLiv' src
                            in
                                updatedLiv
                            end 
                            | A.MOVE{assem, dst, src} => 
                            let val updatedLiv' = Temp.Set.subtract(currLiv, dst)
                                val updatedLiv = Temp.Set.add(updatedLiv', src)
                            in
                                updatedLiv
                            end
                            | A.LABEL(_) => currLiv (* Won't happen *)
                    
                    (*  Given a base ig, an initial live-out at the end of the node (basic block) along with all the instructions
                        within that node, add interference edges for this node *)
                    fun addInterferenceEdgesPerNode(wholeig, initLiv, insns) = 
                        case insns of
                            [] => wholeig
                          | insn::insns' => let val updatedIG = addInterferenceEdgesPerInsn(wholeig, initLiv, insn)
                                                val updatedLiv = updateLiveness(insn, initLiv)
                                            in 
                                                addInterferenceEdgesPerNode(updatedIG, updatedLiv, insns')
                                            end

                in

					(
                    addInterferenceEdgesPerNode(baseig, initLiv, revInsns))
                end (* End updateByNode *)
        
        in
            foldl updateByNode {graph=IGraph.empty, moves=[]} allLabels
        end (* End generateIGraph *)

end
