structure Liveness =
struct
    structure F = Flow
    structure T = Temp

    type liveMap = T.Set.set F.LabelMap.map
    fun calculateLiveness(def, use) = (* Temp.Set.set Flow.LabelMap.map * Temp.Set.set Flow.LabelMap.map -> liveMap
                                         Or pass in a cfg directly as an arg *)
        F.LabelMap.empty (* TODO: update later *)

    structure IGraph = FuncGraph(Temp.TempOrd) (* Each node in the IGraph takes a temp as the key. Note that a graph is basically a map (adjacency list) *)
    type igraph = {
        graph: Temp.temp IGraph.graph, (* Each temp is itself a node in the interference graph *)
        moves: (Temp.temp IGraph.node * Temp.temp IGraph.node) list
    }

    (* fun generateIGraph cfg =  *)

end