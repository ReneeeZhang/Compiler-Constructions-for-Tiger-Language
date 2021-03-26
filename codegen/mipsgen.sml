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
            emit(A.OPER{assem="JUMP `d0\n", src=[], dst=[munchExp e],
            jump=SOME(dest)})
      | munchStm(T.LABEL(lab)) = emit(A.LABEL{assem=Symbol.name(lab)^":\n", lab=lab})
       

    and munchExp (T.CONST i) = 
            result(fn r => emit(A.OPER{assem="ADDI `d0 <- `r0 + "^Int.toString(i)^"\n",
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
    in

	munchStm stm;
	rev(!ilist)
    end
end

