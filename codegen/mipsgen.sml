structure MipsGen : CODEGEN =
struct
structure Frame = MipsFrame
structure T = Tree
structure A = Assem
structure Symbol = Symbol

fun codegen (frame) (stm: Tree.stm) : Assem.instr list = 
    let val ilist = ref (nil: Assem.instr list)
    val calldefs = Frame.RA :: (Frame.argregs @ Frame.callersaves @ Frame.RVs)
    fun emit x= ilist := x :: !ilist
    fun isLibraryCall (funName) = List.exists(fn x => x = funName) ["print", "flush", "getchar", "ord", "chr", "size", "substring", "concat", "not", "exit"]
    fun result(gen) =
	    let val t = Temp.newtemp()
	    in
		gen t; t
	    end

	fun munchStm(T.SEQ(a,b)) = (munchStm a; munchStm b)
      | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) = 
            emit(A.OPER{assem="SW `s1, "^Int.toString(i)^"(`s0)\n", src=[munchExp
            e1, munchExp e2], dst=[], jump=NONE})
      | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) = 
            emit(A.OPER{assem="SW `s1, "^Int.toString(i)^"(`s0)\n", src=[munchExp
            e1, munchExp e2], dst=[], jump=NONE})
      | munchStm(T.MOVE(T.MEM(T.CONST i), e1)) = 
            emit(A.OPER{assem="SW `s0, 0("^Int.toString(i)^"), \n", src=[munchExp e1],
            dst=[], jump=NONE})
      | munchStm(T.MOVE(T.MEM(e1), e2)) = 
            emit(A.OPER{assem="SW `s1, 0(`s0)\n", src=[munchExp e1, munchExp e2],
            dst=[], jump=NONE})
      | munchStm(T.MOVE(T.TEMP i, T.CONST j)) = 
            emit(A.OPER{assem="ADDI `d0, r0, "^Int.toString(j)^"\n", src=[],
            dst=[i], jump=NONE})
      | munchStm(T.MOVE(T.TEMP i, e2)) = 
        emit(A.OPER{assem="ADD `d0, `s0, r0\n", src=[munchExp e2],
        dst=[i],jump=NONE})
      | munchStm(T.JUMP(e, dest)) = 
            emit(A.OPER{assem="JUMP `j0\n", src=[], dst=[],
            jump=SOME(dest)})
      | munchStm(T.CJUMP(T.LE, e1, e2, tlab, flab)) = 
            emit(A.OPER{assem="BLE `s0, `s1, `j0\n", src=[munchExp e1, munchExp
            e2], dst=[], jump=SOME([tlab])})
      | munchStm(T.CJUMP(T.GE, e1, e2, tlab, flab)) = 
            emit(A.OPER{assem="BGE `s0, `s1, `j0\n", src=[munchExp e1, munchExp
            e2], dst=[], jump=SOME([tlab])})
      | munchStm(T.CJUMP(T.LT, e1, e2, tlab, flab)) = 
            emit(A.OPER{assem="BLT `s0, `s1, `j0\n", src=[munchExp e1, munchExp
            e2], dst=[], jump=SOME([tlab])})
      | munchStm(T.CJUMP(T.GT, e1, e2, tlab, flab)) = 
            emit(A.OPER{assem="BGT `s0, `s1, `j0\n", src=[munchExp e1, munchExp
            e2], dst=[], jump=SOME([tlab])})
      | munchStm(T.CJUMP(T.EQ, e1, e2, tlab, flab)) = 
            emit(A.OPER{assem="BEQ `s0, `s1, `j0\n", src=[munchExp e1, munchExp
            e2], dst=[], jump=SOME([tlab])})
      | munchStm(T.CJUMP(T.NE, e1, e2, tlab, flab)) = 
            emit(A.OPER{assem="BNE `s0, `s1, `j0\n", src=[munchExp e1, munchExp
            e2], dst=[], jump=SOME([tlab])})
      | munchStm(T.LABEL(lab)) = emit(A.LABEL{assem=Symbol.name(lab)^":\n", lab=lab})
      | munchStm(T.EXP(T.CALL(T.NAME (fNameLabel), arg::args))) = emit(A.OPER{
        assem="JAL `j0\n",
        src=(if isLibraryCall((Symbol.name fNameLabel)) then munchArgs(0, arg::args) else (munchStaticLink(arg); munchArgs(0, args))),
        dst=calldefs,
        jump=SOME([fNameLabel])
      })
      | munchStm(T.EXP(T.CALL(_, argExps))) =  () (* Won't ever happen *)
       
    and munchStaticLink(arg) = munchStm(T.MOVE(T.MEM(T.TEMP (Frame.SP)), T.TEMP (munchExp(arg))))
    and munchArgs(argNumber, arg::[]) = if argNumber < 4 then
          (munchStm(T.MOVE((T.TEMP(List.nth(Frame.argregs, argNumber))), arg)); [List.nth(Frame.argregs, argNumber)])
        else (munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP Frame.SP, T.CONST ((argNumber + 1)*4))), T.TEMP (munchExp(arg)))); [])
      | munchArgs(argNumber, arg::args) = if argNumber < 4 then
          munchArgs(argNumber, [arg]) @ munchArgs(argNumber + 1, args)
        else (munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP Frame.SP, T.CONST ((argNumber + 1)*4))),T.TEMP (munchExp(arg)))); munchArgs(argNumber+1, args))
      | munchArgs(argNumber, []) = []

    and munchExp (T.CONST i) = 
            result(fn r => emit(A.OPER{assem="ADDI `d0, r0, "^Int.toString(i)^"\n",
            src=[], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.PLUS, e1, T.CONST i)) = 
            result(fn r => emit(A.OPER{assem="ADDI `d0, `s0, "^ 
            Int.toString(i)^"\n", src=[munchExp e1], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.PLUS, T.CONST i, e1)) = 
            result(fn r => emit(A.OPER{assem="ADDI `d0, `s0, "^ 
            Int.toString(i)^"\n", src=[munchExp e1], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.MINUS, e1, T.CONST i)) = 
            result(fn r => emit(A.OPER{assem="SUBI `d0, `s0, "^ 
            Int.toString(i)^"\n", src=[munchExp e1], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.MINUS, T.CONST i, e1)) = 
            result(fn r => emit(A.OPER{assem="SUBI `d0, `s0, "^ 
            Int.toString(i)^"\n", src=[munchExp e1], dst=[r], jump=NONE}))
      | munchExp (T.NAME l) = result(fn r => ())
      | munchExp (T.CALL (a, b)) = (munchStm(T.EXP(T.CALL(a,b))); List.nth(Frame.RVs, 0))
      | munchExp (T.BINOP(T.MUL, e1, e2)) = 
            result(fn r => emit(A.OPER{assem="MUL `d0, `s0, `s1\n",
            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.DIV, e1, e2)) = 
            result(fn r => emit(A.OPER{assem="DIV `d0, `s0, `s1\n",
            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.MINUS, e1, e2)) = 
            result(fn r => emit(A.OPER{assem="SUB `d0, `s0, `s1\n",
            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.PLUS, e1, e2)) = 
            result(fn r => emit(A.OPER{assem="ADD `d0, `s0, `s1\n",
            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
      | munchExp(T.TEMP t) = t
      | munchExp(T.ESEQ(a, b)) = (munchStm(a); munchExp(b))
      | munchExp(T.MEM(T.BINOP(T.PLUS, e, T.CONST i))) = 
        result(fn r => emit(A.OPER{assem="LW `d0, " ^ Int.toString(i) ^ "(`s0)\n",
				   src=[munchExp e],
				   dst=[r],
				   jump=NONE}))
      | munchExp(T.MEM(T.BINOP(T.PLUS, T.CONST i, e))) =
	munchExp(T.MEM(T.BINOP(T.PLUS, e, T.CONST i)))
      | munchExp(T.MEM(T.BINOP(T.MINUS, e, T.CONST i))) =
	result(fn r => emit(A.OPER{assem="LW `d0, " ^ Int.toString(~i) ^ "(`s0)\n",
				   src=[munchExp e],
				   dst=[r],
				   jump=NONE}))
      (* This case is the same as munchExp(T.MEM(e)) *)
      (* | munchExp(T.MEM(T.BINOP(T.MINUS, T.CONST i, e))) = *)
      (* 	result(fn r => emit(A.OPER{assem="LW `d0, 0(`s0)\n", *)
      (* 				   src=[munchExp(T.BINOP(T.MINUS, T.CONST i, e))], *)
      (* 				   dst=[r], *)
      (* 				   jump=NONE})) *)
      | munchExp(T.MEM(e)) =
	result(fn r => emit(A.OPER{assem="LW `d0, 0(`s0)\n",
				   src=[munchExp(e)],
				   dst=[r],
				   jump=NONE}))
    in
	munchStm stm;
	rev(!ilist)
    end
end

