(* signature COLOR = 
sig
    structure Frame : FRAME
    type allocation = Frame.register Temp.Map.map
    structure RegSet : ORD_SET sharing type RegSet.Key.ord_key = Frame.register
    val color: {interference: Liveness.igraph, 
                initial: allocation, 
                spillPriority: Temp.temp Liveness.IGraph.node -> int, 
                registers: RegSet.set} -> allocation(* * Temp.temp list *)
end *)

structure Color = 
struct
    structure L = Liveness
    structure LIG = L.IGraph
    structure TM = Temp.Map
		       
    structure Frame = MipsFrame
    type allocation = Frame.register Temp.Map.map
    structure RegSet = SplaySetFn(struct 
                                        type ord_key = Frame.register
                                        val compare = String.compare
                                  end)

    exception NotEnoughRegs

    val numColors = 25 (* TODO: make sure the number of colors *)
    fun color {interference={graph=ifgraph, moves}, initial, spillPriority, registers} = 
            (* Categorize from interference graph to have a list of nodes of trivial degrees and of non-trivial degrees 
               Note that both simplifyWorkList and spillWorkList have type (Temp.temp node * (Temp.temp.node list) list. 
               Each element in these lists indicates a node and its adj nodes *)
        let fun makeWorkList ifgraph = 
                let val allNodes = LIG.nodes ifgraph
                    fun makeWorkListsFromSingleNode (node, {simplifyWorkList=simplifyL, spillWorkList=spillL}) =
                        case Temp.Map.find(initial, LIG.nodeInfo(node)) of (* Check if the node is precolored *)
                                SOME(_) => {simplifyWorkList=simplifyL, spillWorkList=spillL}
                              | NONE => if LIG.degree(node) >= numColors
                                        then {simplifyWorkList=simplifyL, spillWorkList=(node, (LIG.adj' ifgraph node))::spillL}
                                        else {simplifyWorkList=(node, (LIG.adj' ifgraph node))::simplifyL, spillWorkList=spillL}
                in
                    foldl makeWorkListsFromSingleNode {simplifyWorkList=[], spillWorkList=[]} allNodes
                end
            
            (* Check if all the nodes in ifgraph are procolored *)
            fun areAllPrecolored ifgraph =
                let val allNodeData = map LIG.nodeInfo (LIG.nodes ifgraph)
                in
                    List.all (fn d => TM.inDomain(initial, d)) allNodeData
                end

            fun generateStack(ifgraph, stack) = 
                let val {simplifyWorkList, spillWorkList} = makeWorkList ifgraph
                    fun simplify simplifyWorkList = 
                        let val simplifiedIfgraph = foldl (fn ((node, _), g) => LIG.remove(g, node)) ifgraph simplifyWorkList
                        in 
                            generateStack(simplifiedIfgraph, simplifyWorkList@stack)
                        end
                    
                    fun potentialSpill spillWorkList = 
                        let fun getSpillNode spills = 
                                case spills of
                                    [] => raise Empty
                                  | (s as (node, _))::[] => s
                                  | (s as (node, _))::spills' =>    let val (t as (node', _)) = getSpillNode(spills')
                                                                    in
                                                                        if spillPriority(node) > spillPriority(node')
                                                                        then s
                                                                        else t
                                                                    end

                            val (sp as (spillNode, _)) = getSpillNode(spillWorkList)
                            val ifgraphWithSpillNodeRemoved = LIG.remove(ifgraph, spillNode)
                        in
                            generateStack(ifgraphWithSpillNodeRemoved, sp::stack)
                        end  
                in
                    case simplifyWorkList of
                        [] => if areAllPrecolored ifgraph
                              then stack
                              else potentialSpill spillWorkList
                      | _ => simplify simplifyWorkList 
                end
            
            fun assignColors () =
                let val stack = generateStack(ifgraph, [])
                    (* Given a stack s to rebuild, and the ans, extendedMap, return extendedMap when s is empty *)
                    fun assignColorsBasedOnStack(s, extendedMap) = 
                        case stack of
                            [] => extendedMap
                          | (node, adjs)::s' => 
                            (*  Delete a the string reg from availableRegs if the corresonding temp is found in TM *)
                            let fun delete(temp, availableRegs) = 
                                    case TM.find(extendedMap, temp) of
                                        SOME(reg) => RegSet.delete(availableRegs, reg)
                                      | NONE => availableRegs
                                val adjTemps = map LIG.nodeInfo adjs
                                val possibleRegs = foldl delete registers adjTemps (* Remove all the adj colors from registers set *)
                                val possibleRegsList = RegSet.toList(possibleRegs)
                            in
                                case possibleRegsList of
                                    [] => raise NotEnoughRegs (* Where actual spilling happens *)
                                  | reg::_ => let val extendedMap' = TM.insert(extendedMap, LIG.nodeInfo(node), reg)
                                            in 
                                                assignColorsBasedOnStack(s', extendedMap')
                                            end
                            end
                in  
                    assignColorsBasedOnStack(stack, initial)
                end (* End assignColors *)

        in
            assignColors()
        end

end (* End structure *)
