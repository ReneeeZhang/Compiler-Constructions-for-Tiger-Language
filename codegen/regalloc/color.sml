signature COLOR = 
sig
    structure Frame : FRAME
    type allocation = Temp.Map
    val color: {interference: Liveness.igraph, 
                initial: allocation, 
                spillPriority: Liveness.IGraph.node -> int, 
                registers: Frame.register list} -> allocation * Temp.temp list
end

structure Color :> COLOR = 
struct
    structure L = Liveness
    structure LIG = L.IGraph

    val numColors = 25 (* TODO: make sure the number of colors *)
    structure Frame = MipsFrame
    type allocation = Temp.Map
    fun color {interference={graph=ifgraph, moves}, initial, spillPriority, registers} = 
        let 
            (* Categorize from interference graph to have a list of nodes of trivial degrees and of non-trivial degrees *)
            fun makeWorkList ifgraph = 
                let val allNodes = LIG.nodes ifgraph
                    fun makeWorkListsFromSingleNode (node, {simplifyWorkList=simplifyL, spillWorkList=spillL}) =
                        case Temp.Map.find(initial, LIG.nodeInfo(node)) of (* Check if the node is precolored *)
                            SOME(_) => {simplifyWorkList=simplifyL, spillWorkList=spillL}
                          | NONE => if LIG.degree(node) >= numColors
                                    then {simplifyWorkList=simplifyL, spillWorkList=node::spillL}
                                    else {simplifyWorkList=node::simplifyL, spillWorkList=spillL}
                in
                    foldl makeWorkListsFromSingleNode {simplifyWorkList=[], spillWorkList=[]} allNodes
                end
            
            (* Check if all the nodes in ifgraph are procolored *)
            fun areAllPrecolored ifgraph =
                let val allNodeData = map LIG.nodeInfo (LIG.nodes ifgraph)
                in
                    List.all (fn d => Temp.Map.inDomain d) allNodeData
                end

            fun generateStack(ifgraph, stack) = 
                let val {simplifyWorkList, spillWorkList} = makeWorkList ifgraph
                    fun simplify simplifyWorkList = 
                        let val simplifiedIfgraph = foldl (fn (node, g) => LIG.remove(g, node)) ifgraph simplifyWorkList
                        in 
                            generateStack(simplifiedIfgraph, simplifyWorkList@stack)
                        end
                    
                    fun potentialSpill spillWorkList = 
                        let fun getSpillNode spills = 
                                case spills of
                                    [] => raise empty
                                  | s::[] => s
                                  | s::spills' =>   let val t = getSpills(spills')
                                                    in
                                                        if spillPriority(s) > spillPriority(t)
                                                        then s
                                                        else t
                                                    end

                            val spillNode = getSpillNode(spillWorkList)
                            val ifgraphWithSpillNodeRemoved = LIG.remove(ifgraph, spillNode)
                        in
                            generateStack(ifgraphWithSpillNodeRemoved, spillNode::stack)
                        end  
                in
                    case simplifyWorkList of
                        [] => if areAllPrecolored ifgraph
                              then stack
                              else potentialSpill spillWorkList
                      | _ => simplify simplifyWorkList 
                end
            
            (* fun assignColors () = *)




end
