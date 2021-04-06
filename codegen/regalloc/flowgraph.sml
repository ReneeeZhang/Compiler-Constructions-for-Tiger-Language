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
									def: TempSet.set LabelMap.map, (* Granularity: basic block. Key: label, value: set *)
									use: TempSet.set LabelMap.map, (* Granularity: basic block. Key: label, value: set *)
									ismove: bool InsnMap.map} (* Granularity: instruction *)

	fun stringifyNodeData(nid, data) = (* data here is Assem.instr list *)
	    let fun addNewLine str = str ^ "\n" 
			val fmt = Assem.format(MipsFrame.display)
			val initStr = addNewLine ("############# " ^ Symbol.name nid ^ " #############")
			val str = foldl (fn (i, ans) => ans ^ (fmt i)) initStr data
		in 
			addNewLine(str)
		end

	(* For Debugging *)
	val printControlGraph = Graph.printGraph stringifyNodeData

	fun println x = print(x ^ "\n")

	fun printLabelMapWithKey m k = (* m: LabelMap, k: label *)
		let val s = LabelMap.lookup(m, k) (* get the set s under key k *)
			val _ = println("Block: " ^ Symbol.name k)
		in
			TempSet.app (println o MipsFrame.display) s
		end

	(* print def or use *)
	fun printLabelMap(m, description) =
		let val _ = println(description ^ ": ")
		in 
			List.app (printLabelMapWithKey m) (LabelMap.listKeys(m))
		end


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
