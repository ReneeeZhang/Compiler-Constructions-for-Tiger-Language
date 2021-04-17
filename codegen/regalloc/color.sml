signature COLOR = 
sig
    structure Frame : FRAME
    type allocation = Temp.Map
    val color: {interference: Liveness.igraph, 
                initial: allocation, 
                spillCost: Graph.node -> int, 
                registers: Frame.register list} -> allocation * Temp.temp list
end

structure Color :> COLOR = 
struct
    structure L = Liveness
    structure LIG = L.IGraph

    val numColors = 25 (* TODO: make sure the number of colors *)
    structure Frame = MipsFrame
    type allocation = Temp.Map
    fun color {interference={graph=ifgraph, moves}, initial, spillCost, registers} = 
        let fun makeWorkList () = 
                let val allNodes = LIG.nodes ifgraph
                    fun makeWorkListsFromSingleNode (node, {simplifyWorkList=simplifyL, spillWorkList=spillL}) =
                        case Temp.Map.find(initial, LIG.nodeInfo(node)) of
                            SOME(_) => {simplifyWorkList=simplifyL, spillWorkList=spillL}
                          | NONE => if LIG.degree(node) >= numColors
                                    then {simplifyWorkList=simplifyL, spillWorkList=node::spillL}
                                    else {simplifyWorkList=node::simplifyL, spillWorkList=spillL}
                in
                    foldl makeWorkListsFromSingleNode {simplifyWorkList=[], spillWorkList=[]} allNodes
                end


end
