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
    fun pushStackForCall (funName, numArgs) = if isLibraryCall(funName) andalso numArgs > 4 then emit(A.OPER{assem="addi $sp, $sp, -"^printInt((numArgs-4)*4)^"\n", src=[], dst=[], jump=NONE})
      else (if numArgs > 4 then emit(A.OPER{assem="addi $sp, $sp, -"^printInt((numArgs-4)*4)^"\n", src=[], dst=[], jump=NONE})
        else (if isLibraryCall(funName) then () else emit(A.OPER{assem="addi $sp, $sp, -4\n", src=[], dst=[], jump=NONE})))
    
    fun pullStackAfterCall (funName, numArgs) = if isLibraryCall(funName) andalso numArgs > 4 then emit(A.OPER{assem="addi $sp, $sp, "^printInt((numArgs-4)*4)^"\n", src=[], dst=[], jump=NONE})
      else (if numArgs > 4 then emit(A.OPER{assem="addi $sp, $sp, "^printInt((numArgs-4)*4)^"\n", src=[], dst=[], jump=NONE})
        else (if isLibraryCall(funName) then () else emit(A.OPER{assem="addi $sp, $sp, 4\n", src=[], dst=[], jump=NONE})))
    
    fun saveTRegs(0) = ()
      | saveTRegs(numTs) = (
      emit(A.OPER{assem="sw $t"^printInt(numTs-1)^", "^printInt(numTs*4)^"($sp)\n", src=[], dst=[], jump=NONE}); saveTRegs(numTs - 1)
    )
    fun loadTRegs(0) = ()
      | loadTRegs(numTs) = (
      emit(A.OPER{assem="lw $t"^printInt(numTs-1)^", "^printInt(numTs*4)^"($sp)\n", src=[], dst=[], jump=NONE}); loadTRegs(numTs - 1)
    )

    (* 10 $t + 1 $ra *)
    fun saveCallerSavedRegs() = (
      emit(A.OPER{assem="\n# Start function-call prologue (save caller-saved regs)\n", src=[], dst=[], jump=NONE});
      emit(A.OPER{assem="addi $sp, $sp, -44\n", src=[], dst=[], jump=NONE});
      emit(A.OPER{assem="sw $ra, 0($sp)\n", src=[], dst=[], jump=NONE});
      saveTRegs(10)
    )
    fun loadCallerSavedRegs() = (
      emit(A.OPER{assem="lw $ra, 0($sp)\n", src=[], dst=[], jump=NONE});
      loadTRegs(10);
      emit(A.OPER{assem="addi $sp, $sp, 44\n", src=[], dst=[], jump=NONE});
      emit(A.OPER{assem="# End function-call epilogue (load caller-saved regs)\n\n", src=[], dst=[], jump=NONE})
    )

    fun saveSRegs(0) = ()
      | saveSRegs(numSs) = (
      emit(A.OPER{assem="sw $s"^printInt(numSs-1)^", "^printInt((numSs-1)*4)^"($sp)\n", src=[], dst=[], jump=NONE}); saveSRegs(numSs - 1)
    )

    fun loadSRegs(0) = ()
      | loadSRegs(numSs) = (
      emit(A.OPER{assem="lw $s"^printInt(numSs-1)^", "^printInt((numSs-1)*4)^"($sp)\n", src=[], dst=[], jump=NONE}); loadSRegs(numSs - 1)
    )

    fun getCurrentFrameFormalsCount() = 
      (if ((#name frame) = Symbol.symbol("tig_main")) then 0 else (!(#numlocals frame)))
    (* 8 $s + 1 $fp *)
    fun saveCalleeSavedRegs() = (
        emit(A.OPER{assem="# Start function-body prologue (save callee-saved regs)\n", src=[], dst=[], jump=NONE});
        emit(A.OPER{assem="sw $fp, -"^printInt(4*(1+getCurrentFrameFormalsCount()))^"($sp)\n", src=[], dst=[], jump=NONE});
        emit(A.MOVE{assem="move $fp, $sp\n", src=Frame.SP, dst=Frame.FP});
        emit(A.OPER{assem="addi $sp, $sp, -"^printInt(4*(9+getCurrentFrameFormalsCount()))^"\n", src=[], dst=[], jump=NONE});
        saveSRegs(8);
        emit(A.OPER{assem="# End function-body prologue  (save callee-saved regs)\n\n", src=[], dst=[], jump=NONE})
    )

    fun loadCalleeSavedRegs() = (
        emit(A.OPER{assem="\n# Start function-body epilogue (load callee-saved regs)\n", src=[], dst=[], jump=NONE});
        loadSRegs(8);
        emit(A.OPER{assem="addi $sp, $sp, "^printInt(4*(9+getCurrentFrameFormalsCount()))^"\n", src=[], dst=[], jump=NONE});
        emit(A.OPER{assem="lw $fp, -"^printInt(4*(1+getCurrentFrameFormalsCount()))^"($sp)\n", src=[], dst=[], jump=NONE});
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
            emit(A.OPER{assem="sw `s1, "^printInt(i)^"(`s0)\n", src=[munchExp
            e1, munchExp e2], dst=[], jump=NONE})
      | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) = 
            emit(A.OPER{assem="sw `s1, "^printInt(i)^"(`s0)\n", src=[munchExp
            e1, munchExp e2], dst=[], jump=NONE})
      | munchStm(T.MOVE(T.MEM(T.CONST i), e1)) = 
            emit(A.OPER{assem="sw `s0, 0("^printInt(i)^"), \n", src=[munchExp e1],
            dst=[], jump=NONE})
      | munchStm(T.MOVE(T.MEM(e1), e2)) = 
            emit(A.OPER{assem="sw `s1, 0(`s0)\n", src=[munchExp e1, munchExp e2],
            dst=[], jump=NONE})
      | munchStm(T.MOVE(T.TEMP i, T.CONST j)) = 
            emit(A.OPER{assem="addi `d0, $0, "^printInt(j)^"\n", src=[],
            dst=[i], jump=NONE})
      | munchStm(T.MOVE(T.TEMP i, e2)) = 
        let val e2' = munchExp e2
        in
            if i = Frame.ZERO andalso (e2') = Frame.ZERO then () else
            emit(A.MOVE{assem="move `d0, `s0 \n", src=e2', dst=i})
        end
      | munchStm(T.MOVE(_, _)) = () (* Won't ever happen *)
      | munchStm(T.JUMP(T.TEMP(t), dest)) = 
            (emitCalleeSavedRegsLoadCodeIfNeeded(t);
            emit(A.OPER{assem="jr `s0\n\n", src=[t], dst=[],
            jump=SOME(dest)}))
      | munchStm(T.JUMP(_, dest)) = 
            emit(A.OPER{assem="j `j0\n", src=[], dst=[],
            jump=SOME(dest)})
      | munchStm(T.CJUMP(T.LE, e1, e2, tlab, flab)) = 
            emit(A.OPER{assem="ble `s0, `s1, `j0\n", src=[munchExp e1, munchExp
            e2], dst=[], jump=SOME([tlab, flab])})
      | munchStm(T.CJUMP(T.GE, e1, e2, tlab, flab)) = 
            emit(A.OPER{assem="bge `s0, `s1, `j0\n", src=[munchExp e1, munchExp
            e2], dst=[], jump=SOME([tlab, flab])})
      | munchStm(T.CJUMP(T.LT, e1, e2, tlab, flab)) = 
            emit(A.OPER{assem="blt `s0, `s1, `j0\n", src=[munchExp e1, munchExp
            e2], dst=[], jump=SOME([tlab, flab])})
      | munchStm(T.CJUMP(T.GT, e1, e2, tlab, flab)) = 
            emit(A.OPER{assem="bgt `s0, `s1, `j0\n", src=[munchExp e1, munchExp
            e2], dst=[], jump=SOME([tlab, flab])})
      | munchStm(T.CJUMP(T.EQ, e1, e2, tlab, flab)) = 
            emit(A.OPER{assem="beq `s0, `s1, `j0\n", src=[munchExp e1, munchExp
            e2], dst=[], jump=SOME([tlab, flab])})
      | munchStm(T.CJUMP(T.NE, e1, e2, tlab, flab)) = 
            emit(A.OPER{assem="bne `s0, `s1, `j0\n", src=[munchExp e1, munchExp
            e2], dst=[], jump=SOME([tlab, flab])})
      | munchStm(T.CJUMP(_, e1, e2, tlab, flab)) = () (* Won't ever happen *)
      | munchStm(T.LABEL(lab)) = (emit(A.LABEL{assem=Symbol.name(lab)^":\n", lab=lab}); emitCalleeSavedRegsCodeIfNeeded(Symbol.name(lab)))
      | munchStm(T.EXP(T.CALL(T.NAME (fNameLabel), arg::args))) = (saveCallerSavedRegs(); 
          emit(A.OPER{assem="# End function-call prologue (save caller-saved regs)\n", src=[], dst=[], jump=NONE});
          emit(A.OPER{assem="# Start (save static link and args on stack)\n", src=[], dst=[], jump=NONE});
          pushStackForCall(Symbol.name fNameLabel, List.length(arg::args));
          let val srcArray = (if isLibraryCall((Symbol.name fNameLabel)) then munchArgs(0, arg::args) else (munchStaticLink(arg); munchArgs(0, args)))
          in
          (emit(A.OPER{assem="# End (save static link and args on stack)\n", src=[], dst=[], jump=NONE});
          emit(A.OPER{
            assem="jal `j0\n",
            src=srcArray,
            dst=calldefs,
            jump=SOME([fNameLabel])
          }))
          end;
        emit(A.OPER{assem="# Start function-call epilogue (load caller-saved regs)\n", src=[], dst=[], jump=NONE});
        pullStackAfterCall(Symbol.name fNameLabel, List.length(arg::args));
        loadCallerSavedRegs())
        | munchStm(T.EXP(T.CALL(T.NAME (fNameLabel), []))) = (saveCallerSavedRegs();
          emit(A.OPER{assem="# End function-call prologue (save caller-saved regs)\n", src=[], dst=[], jump=NONE});
          emit(A.OPER{
            assem="jal `j0\n",
            src=[], 
            dst=calldefs,
            jump=SOME([fNameLabel])
          });
        emit(A.OPER{assem="# Start function-call epilogue (load caller-saved regs)\n\n", src=[], dst=[], jump=NONE});
        loadCallerSavedRegs()) (* this has to be a library call *)
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
            result(fn r => emit(A.OPER{assem="addi `d0, $0, "^printInt(i)^"\n",
            src=[], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.PLUS, e1, T.CONST 0)) = 
            result(fn r => emit(A.MOVE{assem="move `d0, `s0 \n",
            src=(munchExp e1), dst=r}))
      | munchExp (T.BINOP(T.PLUS, T.CONST 0, e1)) = 
            result(fn r => emit(A.MOVE{assem="move `d0, `s0 \n",
            src=(munchExp e1), dst=r}))
      | munchExp (T.BINOP(T.PLUS, e1, T.CONST i)) = 
            result(fn r => emit(A.OPER{assem="addi `d0, `s0, "^ 
            printInt(i)^"\n", src=[munchExp e1], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.PLUS, T.CONST i, e1)) = 
            result(fn r => emit(A.OPER{assem="addi `d0, `s0, "^ 
            printInt(i)^"\n", src=[munchExp e1], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.MINUS, e1, T.CONST i)) = 
            result(fn r => emit(A.OPER{assem="addi `d0, `s0, "^ 
            (if i >= 0 then "-"^printInt(i) else printInt(~1*i))^"\n", src=[munchExp e1], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.MINUS, T.CONST i, e1)) = 
            result(fn r => emit(A.OPER{assem="addi `d0, `s0, "^ 
            (if i >= 0 then "-"^printInt(i) else printInt(~1*i))^"\n", src=[munchExp e1], dst=[r], jump=NONE}))
      | munchExp (T.NAME l) = result(fn r => emit(A.OPER{
        assem="la `d0, " ^ Symbol.name(l) ^ "\n", src=[], dst=[r], jump=NONE
      }))
      | munchExp (T.CALL (a, b)) = (munchStm(T.EXP(T.CALL(a,b))); List.nth(Frame.RVs, 0))
      | munchExp (T.BINOP(T.MUL, e1, e2)) = 
            result(fn r => emit(A.OPER{assem="mul `d0, `s0, `s1\n",
            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.DIV, e1, e2)) = 
            result(fn r => emit(A.OPER{assem="div `d0, `s0, `s1\n",
            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.MINUS, e1, e2)) = 
            result(fn r => emit(A.OPER{assem="sub `d0, `s0, `s1\n",
            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(T.PLUS, e1, e2)) = 
            result(fn r => emit(A.OPER{assem="add `d0, `s0, `s1\n",
            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
      | munchExp (T.BINOP(_, e1, e2)) = Temp.newtemp() (* Won't ever happen *)
      | munchExp(T.TEMP t) = t
      | munchExp(T.ESEQ(a, b)) = (munchStm(a); munchExp(b))
      | munchExp(T.MEM(T.BINOP(T.PLUS, e, T.CONST i))) = 
        result(fn r => emit(A.OPER{assem="lw `d0, " ^ printInt(i) ^ "(`s0)\n",
				   src=[munchExp e],
				   dst=[r],
				   jump=NONE}))
      | munchExp(T.MEM(T.BINOP(T.PLUS, T.CONST i, e))) =
	munchExp(T.MEM(T.BINOP(T.PLUS, e, T.CONST i)))
      | munchExp(T.MEM(T.BINOP(T.MINUS, e, T.CONST i))) =
	result(fn r => emit(A.OPER{assem="lw `d0, " ^ printInt(~i) ^ "(`s0)\n",
				   src=[munchExp e],
				   dst=[r],
				   jump=NONE}))
      | munchExp(T.MEM(e)) =
	result(fn r => emit(A.OPER{assem="lw `d0, 0(`s0)\n",
				   src=[munchExp(e)],
				   dst=[r],
				   jump=NONE}))
    in
	munchStm stm;
	rev(!ilist)
    end
end

