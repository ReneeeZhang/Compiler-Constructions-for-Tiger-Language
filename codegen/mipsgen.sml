structure MipsGen : CODEGEN =
struct
structure Frame = MipsFrame
structure T = Tree
structure A = Assem

fun codegen (frame) (stm: Tree.stm) : Assem.instr list = 
    let val ilist = ref (nil: Assem.instr list)
    fun emit x= ilist := x :: !ilist
    fun result(gen) =
	    let val t = Temp.newtemp()
	    in
		gen t; t
	    end

	fun munchStm(T.SEQ(a,b)) = (munchStm a; munchStm b)
      | munchStm(T.MOVE(T.TEMP i, T.CONST j)) = 
            emit(A.OPER{assem="ADDI `d0 <- r0 + "^Int.toString(j)^"\n", src=[],
            dst=[i], jump=NONE})
      | munchStm(T.MOVE(T.TEMP i, e2)) = 
        emit(A.OPER{assem="ADD `d0 <- `s0 + r0\n", src=[munchExp e2],
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
       

    and munchExp (T.CONST i) = 
            result(fn r => emit(A.OPER{assem="ADDI `d0 <- r0 + "^Int.toString(i)^"\n",
            src=[], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.PLUS, e1, T.CONST i)) = 
            result(fn r => emit(A.OPER{assem="ADDI `d0 <- `s0 + "^ 
            Int.toString(i)^"\n", src=[munchExp e1], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.PLUS, T.CONST i, e1)) = 
            result(fn r => emit(A.OPER{assem="ADDI `d0 <- `s0 + "^ 
            Int.toString(i)^"\n", src=[munchExp e1], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.MINUS, e1, T.CONST i)) = 
            result(fn r => emit(A.OPER{assem="SUBI `d0 <- `s0 + "^ 
            Int.toString(i)^"\n", src=[munchExp e1], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.MINUS, T.CONST i, e1)) = 
            result(fn r => emit(A.OPER{assem="SUBI `d0 <- `s0 + "^ 
            Int.toString(i)^"\n", src=[munchExp e1], dst=[r], jump=NONE}))
      | munchExp (T.NAME l) = result(fn r =>
          emit(A.LABEL{assem=Symbol.name(l)^":\n", lab=l}))
      | munchExp (T.BINOP(T.MUL, e1, e2)) = 
            result(fn r => emit(A.OPER{assem="MUL `d0 <- `s0 + `s1\n",
            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.DIV, e1, e2)) = 
            result(fn r => emit(A.OPER{assem="DIV `d0 <- `s0 + `s1\n",
            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.MINUS, e1, e2)) = 
            result(fn r => emit(A.OPER{assem="SUB `d0 <- `s0 + `s1\n",
            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.PLUS, e1, e2)) = 
            result(fn r => emit(A.OPER{assem="ADD `d0 <- `s0 + `s1\n",
            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
      | munchExp(T.TEMP t) = t
      | munchExp(T.MEM(T.BINOP(T.PLUS, e, T.CONST i))) = 
        result(fn r => emit(A.OPER{assem="LW `d0, " ^ Int.toString(i) ^ "(`s0)\n",
				   src=[munchExp e],
				   dst=[r],
				   jump=NONE}))
      | munchExp(T.MEM(T.BINOP(T.PLUS, T.CONST i, e))) =
	munchExp(T.MEM(T.BINOP(T.PLUS, e, T.CONST i)))
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

