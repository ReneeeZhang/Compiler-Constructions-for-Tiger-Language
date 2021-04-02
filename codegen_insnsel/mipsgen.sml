structure MipsGen : CODEGEN =
struct
structure Frame = MipsFrame
structure T = Tree
structure A = Assem
structure Symbol = Symbol

fun codegen (frame: Frame.frame) (stm: Tree.stm) : Assem.instr list = 
    let val ilist = ref (nil: Assem.instr list)
    val calldefs = Frame.RA :: (Frame.argregs @ Frame.callersaves @ Frame.RVs)
    fun emit x= ilist := x :: !ilist

      fun printInt i = 
            if i >= 0 
            then Int.toString i
            else "-" ^ Int.toString(~i)
            
    fun isLibraryCall (funName) = List.exists(fn x => x = funName) ["tig_print", "tig_flush", "tig_getchar", "tig_ord", "tig_chr", "tig_size", "tig_substring", "tig_concat", "tig_not", "tig_exit", "tig_initArray", "malloc", "tig_stringEqual"]
    fun pushStackForCall (funName, numArgs) = if isLibraryCall(funName) andalso numArgs > 4 then emit(A.OPER{assem="ADDI $sp, $sp, -"^printInt((numArgs-4)*4)^"\n", src=[], dst=[], jump=NONE})
      else (if numArgs > 4 then emit(A.OPER{assem="ADDI $sp, $sp, -"^printInt((numArgs-4)*4)^"\n", src=[], dst=[], jump=NONE})
        else (if isLibraryCall(funName) then () else emit(A.OPER{assem="ADDI $sp, $sp, -4\n", src=[], dst=[], jump=NONE})))
    
    fun pullStackAfterCall (funName, numArgs) = if isLibraryCall(funName) andalso numArgs > 4 then emit(A.OPER{assem="ADDI $sp, $sp, "^printInt((numArgs-4)*4)^"\n", src=[], dst=[], jump=NONE})
      else (if numArgs > 4 then emit(A.OPER{assem="ADDI $sp, $sp, "^printInt((numArgs-4)*4)^"\n", src=[], dst=[], jump=NONE})
        else (if isLibraryCall(funName) then () else emit(A.OPER{assem="ADDI $sp, $sp, 4\n", src=[], dst=[], jump=NONE})))
    
    fun saveTRegs(0) = ()
      | saveTRegs(numTs) = (
      emit(A.OPER{assem="SW $t"^printInt(numTs-1)^", "^printInt(numTs*4)^"($sp)\n", src=[], dst=[], jump=NONE}); saveTRegs(numTs - 1)
    )
    fun loadTRegs(0) = ()
      | loadTRegs(numTs) = (
      emit(A.OPER{assem="LW $t"^printInt(numTs-1)^", "^printInt(numTs*4)^"($sp)\n", src=[], dst=[], jump=NONE}); loadTRegs(numTs - 1)
    )

    (* 10 $t + 1 $ra *)
    fun saveCallerSavedRegs() = (
      emit(A.OPER{assem="\n# Start function-call prologue (save caller-saved regs)\n", src=[], dst=[], jump=NONE});
      emit(A.OPER{assem="ADDI $sp, $sp, -44\n", src=[], dst=[], jump=NONE});
      emit(A.OPER{assem="SW $ra, 0($sp)\n", src=[], dst=[], jump=NONE});
      saveTRegs(10)
    )
    fun loadCallerSavedRegs() = (
      emit(A.OPER{assem="LW $ra, 0($sp)\n", src=[], dst=[], jump=NONE});
      loadTRegs(10);
      emit(A.OPER{assem="ADDI $sp, $sp, 44\n", src=[], dst=[], jump=NONE});
      emit(A.OPER{assem="# End function-call epilogue (load caller-saved regs)\n\n", src=[], dst=[], jump=NONE})
    )

    fun saveSRegs(0) = ()
      | saveSRegs(numSs) = (
      emit(A.OPER{assem="SW $s"^printInt(numSs-1)^", "^printInt((numSs-1)*4)^"($sp)\n", src=[], dst=[], jump=NONE}); saveSRegs(numSs - 1)
    )

    fun loadSRegs(0) = ()
      | loadSRegs(numSs) = (
      emit(A.OPER{assem="LW $s"^printInt(numSs-1)^", "^printInt((numSs-1)*4)^"($sp)\n", src=[], dst=[], jump=NONE}); loadSRegs(numSs - 1)
    )

    fun getCurrentFrameFormalsCount() = 
      (if ((#name frame) = Symbol.symbol("tig_main")) then 0 else (!(#numlocals frame)))
    (* 8 $s + 1 $fp *)
    fun saveCalleeSavedRegs() = (
        emit(A.OPER{assem="# Start function-body prologue (save callee-saved regs)\n", src=[], dst=[], jump=NONE});
        emit(A.OPER{assem="SW $fp, -"^printInt(4*(1+getCurrentFrameFormalsCount()))^"($sp)\n", src=[], dst=[], jump=NONE});
        emit(A.OPER{assem="MOVE $fp, $sp\n", src=[], dst=[], jump=NONE});
        emit(A.OPER{assem="ADDI $sp, $sp, -"^printInt(4*(9+getCurrentFrameFormalsCount()))^"\n", src=[], dst=[], jump=NONE});
        saveSRegs(8);
        emit(A.OPER{assem="# End function-body prologue  (save callee-saved regs)\n\n", src=[], dst=[], jump=NONE})
    )

    fun loadCalleeSavedRegs() = (
        emit(A.OPER{assem="\n# Start function-body epilogue (load callee-saved regs)\n", src=[], dst=[], jump=NONE});
        loadSRegs(8);
        emit(A.OPER{assem="ADDI $sp, $sp, "^printInt(4*(9+getCurrentFrameFormalsCount()))^"\n", src=[], dst=[], jump=NONE});
        emit(A.OPER{assem="LW $fp, -"^printInt(4*(1+getCurrentFrameFormalsCount()))^"($sp)\n", src=[], dst=[], jump=NONE});
        emit(A.OPER{assem="# End function-body epilogue (load callee-saved regs)\n", src=[], dst=[], jump=NONE})
    )
    fun emitCalleeSavedRegsCodeIfNeeded(labelName) = (
      if String.isPrefix "tig_" labelName then saveCalleeSavedRegs() else ()
    )

    fun emitCalleeSavedRegsLoadCodeIfNeeded(target) = (
      if target = Frame.RA then loadCalleeSavedRegs() else ()
    )

    fun result(gen) =
	    let val t = Temp.newtemp()
	    in
		gen t; t
	    end
	
      fun munchStm(T.SEQ(a,b)) = (munchStm a; munchStm b)
      | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) = 
            emit(A.OPER{assem="SW `s1, "^printInt(i)^"(`s0)\n", src=[munchExp
            e1, munchExp e2], dst=[], jump=NONE})
      | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) = 
            emit(A.OPER{assem="SW `s1, "^printInt(i)^"(`s0)\n", src=[munchExp
            e1, munchExp e2], dst=[], jump=NONE})
      | munchStm(T.MOVE(T.MEM(T.CONST i), e1)) = 
            emit(A.OPER{assem="SW `s0, 0("^printInt(i)^"), \n", src=[munchExp e1],
            dst=[], jump=NONE})
      | munchStm(T.MOVE(T.MEM(e1), e2)) = 
            emit(A.OPER{assem="SW `s1, 0(`s0)\n", src=[munchExp e1, munchExp e2],
            dst=[], jump=NONE})
      | munchStm(T.MOVE(T.TEMP i, T.CONST j)) = 
            emit(A.OPER{assem="ADDI `d0, r0, "^printInt(j)^"\n", src=[],
            dst=[i], jump=NONE})
      | munchStm(T.MOVE(T.TEMP i, e2)) = 
        if i = Frame.ZERO andalso (munchExp e2) = Frame.ZERO then () else
        emit(A.OPER{assem="ADD `d0, `s0, r0\n", src=[munchExp e2],
        dst=[i],jump=NONE})
      | munchStm(T.MOVE(_, _)) = () (* Won't ever happen *)
      | munchStm(T.JUMP(T.TEMP(t), dest)) = 
            emit(A.OPER{assem="JR `s0\n\n", src=[t], dst=[],
            jump=SOME(dest)})
      | munchStm(T.JUMP(_, dest)) = 
            emit(A.OPER{assem="J `j0\n", src=[], dst=[],
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
      | munchStm(T.CJUMP(_, e1, e2, tlab, flab)) = () (* Won't ever happen *)
      | munchStm(T.LABEL(lab)) = (emit(A.LABEL{assem=Symbol.name(lab)^":\n", lab=lab}))
      | munchStm(T.EXP(T.CALL(T.NAME (fNameLabel), arg::args))) = (
        pushStackForCall(Symbol.name fNameLabel, List.length(arg::args));
        let val srcArray = (if isLibraryCall((Symbol.name fNameLabel)) then munchArgs(0, arg::args) else (munchStaticLink(arg); munchArgs(0, args)))
        in
        emit(A.OPER{
          assem="JAL `j0\n",
          src=srcArray,
          dst=calldefs,
          jump=SOME([fNameLabel])
        })
        end;
      pullStackAfterCall(Symbol.name fNameLabel, List.length(arg::args)))
      | munchStm(T.EXP(T.CALL(T.NAME (fNameLabel), []))) = 
        emit(A.OPER{
          assem="JAL `j0\n",
          src=[], 
          dst=calldefs,
          jump=SOME([fNameLabel])
        }) (* this has to be a library call *)
      | munchStm(T.EXP(T.CALL(_, _))) =  () (* Won't ever happen *)
      | munchStm(T.EXP(a)) = (munchExp(a); ()) (* No side effects; can ignore *)
       
    and munchStaticLink(arg) = munchStm(T.MOVE(T.MEM(T.TEMP (Frame.SP)), T.TEMP (munchExp(arg))))
    and munchArgs(argNumber, arg::[]) = if argNumber < 4 then
          (munchStm(T.MOVE((T.TEMP(List.nth(Frame.argregs, argNumber))), arg)); [List.nth(Frame.argregs, argNumber)])
        else (munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP Frame.SP, T.CONST ((argNumber - 3)*4))), T.TEMP (munchExp(arg)))); [])
      | munchArgs(argNumber, arg::args) = if argNumber < 4 then
          munchArgs(argNumber, [arg]) @ munchArgs(argNumber + 1, args)
        else (munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP Frame.SP, T.CONST ((argNumber - 3)*4))),T.TEMP (munchExp(arg)))); munchArgs(argNumber+1, args))
      | munchArgs(argNumber, []) = []

    and munchExp (T.CONST i) = 
            result(fn r => emit(A.OPER{assem="ADDI `d0, r0, "^printInt(i)^"\n",
            src=[], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.PLUS, e1, T.CONST i)) = 
            result(fn r => emit(A.OPER{assem="ADDI `d0, `s0, "^ 
            printInt(i)^"\n", src=[munchExp e1], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.PLUS, T.CONST i, e1)) = 
            result(fn r => emit(A.OPER{assem="ADDI `d0, `s0, "^ 
            printInt(i)^"\n", src=[munchExp e1], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.MINUS, e1, T.CONST i)) = 
            result(fn r => emit(A.OPER{assem="SUBI `d0, `s0, "^ 
            printInt(i)^"\n", src=[munchExp e1], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.MINUS, T.CONST i, e1)) = 
            result(fn r => emit(A.OPER{assem="SUBI `d0, `s0, "^ 
            printInt(i)^"\n", src=[munchExp e1], dst=[r], jump=NONE}))
      | munchExp (T.NAME l) = result(fn r => emit(A.OPER{
        assem="LA `d0, " ^ Symbol.name(l) ^ "\n", src=[], dst=[r], jump=NONE
      }))
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
      | munchExp (T.BINOP(_, e1, e2)) = Temp.newtemp() (* Won't ever happen *)
      | munchExp(T.TEMP t) = t
      | munchExp(T.ESEQ(a, b)) = (munchStm(a); munchExp(b))
      | munchExp(T.MEM(T.BINOP(T.PLUS, e, T.CONST i))) = 
        result(fn r => emit(A.OPER{assem="LW `d0, " ^ printInt(i) ^ "(`s0)\n",
				   src=[munchExp e],
				   dst=[r],
				   jump=NONE}))
      | munchExp(T.MEM(T.BINOP(T.PLUS, T.CONST i, e))) =
	munchExp(T.MEM(T.BINOP(T.PLUS, e, T.CONST i)))
      | munchExp(T.MEM(T.BINOP(T.MINUS, e, T.CONST i))) =
	result(fn r => emit(A.OPER{assem="LW `d0, " ^ printInt(~i) ^ "(`s0)\n",
				   src=[munchExp e],
				   dst=[r],
				   jump=NONE}))
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

