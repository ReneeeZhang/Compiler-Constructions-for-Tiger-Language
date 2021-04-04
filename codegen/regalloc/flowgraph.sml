structure Flow =
struct
    structure LabelOrder =
	struct
		type ord_key = Temp.label
		fun compare(k1, k2) = 
			String.compare(Symbol.name k1, Symbol.name k2)
	end

	structure InsnStrOrder = 
	struct
		type ord_key = string
		val compare = String.compare
	end

	structure LabelMap = SplayMapFn(LabelOrder)
	structure InsnMap = SplayMapFn(InsnStrOrder)

	structure Graph = FuncGraph(LabelOrder)
	datatype flowgraph = FGRAPH of {control: Assem.instr list Graph.graph, (* a list of Assem.instr forms a basic block *)
									def: Temp.temp list LabelMap.map, (* Granularity: basic block *)
									use: Temp.temp list LabelMap.map,
									ismove: bool InsnMap.map} (* Granularity: instruction *)

  (* Note:  any "use" within the block is assumed to be BEFORE a "def" 
        of the same variable.  If there is a def(x) followed by use(x)
       in the same block, do not mention the use in this data structure,
       mention only the def.

     More generally:
       If there are any nonzero number of defs, mention def(x).
       If there are any nonzero number of uses BEFORE THE FIRST DEF,
           mention use(x).

     For any node in the graph,  
           Graph.Table.look(def,node) = SOME(def-list)
           Graph.Table.look(use,node) = SOME(use-list)
   *)

end
