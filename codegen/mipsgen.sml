structure MipsGen : CODEGEN =
struct
structure Frame = MipsFrame
		      
fun codegen (frame) (stm: Tree.stm) : Assem.instr list = 
    let val ilist = ref (nil: Assem.instr list)
	fun emit x= ilist := x :: !ilist
	fun result(gen) =
	    let val t = Temp.newtemp()
	    in
		gen t; t
	    end

	(* fun munchStm ...

	and munchExp ... *)

    in
	(* munchStm stm; *)
	rev(!ilist)
    end
end

