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
	structure TempSet = Temp.Set  (* SplaySetFn(Temp.TempOrd) *)

	structure Graph = FuncGraph(LabelOrder) (* Key: label; Value: Assem.instr list node, i.e., basic block *)
	datatype flowgraph = FGRAPH of {control: Assem.instr list Graph.graph, (* a list of Assem.instr forms a basic block *)
									def: TempSet.set LabelMap.map, (* Granularity: basic block *)
									use: TempSet.set LabelMap.map, (* Granularity: basic block *)
									ismove: bool InsnMap.map} (* Granularity: instruction *)

	fun stringifyNodeData(nid, data) = 
	    let fun addNewLine str = str ^ "\n" 
		fun stringifyInsn(insn, ans) = 
		    case insn of
				Assem.OPER({assem, dst, src, jump}) => ans ^ assem
		      | Assem.MOVE({assem, dst, src}) => ans ^ assem
		      | Assem.LABEL({assem, lab}) => ans ^ assem (* Won't happen *)
		val initStr = addNewLine ("############# " ^ Symbol.name nid ^ " #############")
		val str = foldl stringifyInsn initStr data
	    in
			addNewLine(str)
	    end

					   
	val printControlGraph = Graph.printGraph stringifyNodeData

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
