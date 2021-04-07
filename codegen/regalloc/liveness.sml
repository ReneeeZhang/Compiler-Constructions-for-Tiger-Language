structure Liveness =
struct
    structure F = Flow
    structure T = Temp
    structure A = Assem

    type liveMap = T.Set.set F.LabelMap.map
    fun calculateLiveness(def, use) = (* Temp.Set.set Flow.LabelMap.map * Temp.Set.set Flow.LabelMap.map -> liveMap *)
        F.LabelMap.empty (* TODO: update later *)

    structure IGraph = FuncGraph(Temp.TempOrd) (* Each node in the IGraph takes a temp as the key. Note that a graph is basically a map (adjacency list) *)
    type igraph = {
        graph: Temp.temp IGraph.graph, (* Each temp is itself a node in the interference graph *)
        moves: (Temp.temp IGraph.node * Temp.temp IGraph.node) list
    }

    fun generateIGraph {control, def, use, ismove} = (* Flow.flowgraph -> igraph *)
        let val livenessInfo = calculateLiveness(def, use)
            val allLabels = F.LabelMap.listKeys(def)
            fun getLiveoutOfNode l = F.LabelMap.lookup(livenessInfo, l)
            fun getDefofNode l = F.LabelMap.lookup(def, l)
            fun updateByNode(l, baseig) = 
                let val initLiv = getLiveoutOfNode l
                    (* val initDefset = getDefofNode l *)
                    val insns = F.Graph.nodeInfo(F.Graph.getNode(control, l)) (* Contains dst as a list of def temps *)
                    val revInsns = List.rev insns

                    (* Build edges between elements in def list given by insn and in liv *)
                    fun addInterferenceEdgesPerInsn(baseig, liv, insn) =
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
                                A.OPER{assem, dst, src, jump} => foldl addInterferenceEdgeFromSingleDefTemp baseig dst
                              | A.MOVE{assem, dst, src} => addInterferenceEdgeFromSingleDefTemp(dst, baseig)
                              | A.LABEL(_) => baseig (* Won't happen *)
                        end (* End addInterferenceEdges *)

                    (*  Given a base ig, an initial live-out at the end of the node (basic block) along with all the instructions
                        within that node, add interference edges for this node *)
                    fun addInterferenceEdgesPerNode(ig, initLiv, insns) = 
                        let fun updateLiveness(insn, currLiv) = 
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
                        in
                            case insns of
                                [] => ig
                              | insn::insns' => let val updatedIG = addInterferenceEdgesPerInsn(ig, initLiv, insn)
                                                    val updatedLiv = updateLiveness(insn, initLiv)
                                                in 
                                                    addInterferenceEdgesPerNode(updatedIG, updatedLiv, insns')
                                                end
                        end (* End addInterferenceEdgesPerNode *)
                
                in
                    addInterferenceEdgesPerNode(baseig, initLiv, revInsns)
                end (* End updateByNode *)
        
        in
            foldl updateByNode IGraph.empty allLabels
        end (* End generateIGraph *)

end
