structure Translate : TRANSLATE = 

struct 
  structure T = Tree
  structure A = Absyn
  structure MF = MipsFrame

  datatype level = ROOT of (MF.frame * unit ref)
    | LEVEL of (level * MF.frame * unit ref)
  type frameExtractableLevel = (MF.frame * unit ref)
  type access = level * MF.access
  val outermost = ROOT (MipsFrame.newFrame{name=Symbol.symbol("main"), formals=[]}, ref())
  fun allocLocal (LEVEL(parentLevel, fr, unique): level) esc =
      let val ac = MF.allocLocal fr esc
      in
          (LEVEL(parentLevel, fr, unique), ac)
      end
    | allocLocal (ROOT(fr, unique): level) esc = 
      let val ac = MF.allocLocal fr esc
      in
        (ROOT(fr, unique), ac)
      end

  datatype exp = Ex of T.exp
               | Nx of T.stm
               | Cx of T.label * T.label -> T.stm
               | Un of unit

  fun newLevel ({parent: level, name: T.label, formals: bool list}) =
      LEVEL(parent, MipsFrame.newFrame {name=name, formals=formals}, ref ())
  
  fun getFrameExtractableLevel (LEVEL(parentLevel, fr, unique): level) = (fr, unique)
    | getFrameExtractableLevel (ROOT(fr, unique): level) = (fr, unique)
  
  fun seq ([a,b]) = T.SEQ(a,b)
    | seq ([a]) = a
    | seq (h::t) = T.SEQ(h, seq(t))
  
  fun unEx (Ex e) = e
    | unEx(Cx genstm) = 
      let 
        val r = Temp.newtemp()
        val t = Temp.newlabel() and f = Temp.newlabel()
      in
        T.ESEQ(seq[T.MOVE(T.TEMP r, T.CONST 1),
                    genstm(t,f),
                    T.LABEL f,
                    T.MOVE(T.TEMP r, T.CONST 0),
                    T.LABEL t],
               T.TEMP r)
      end
    | unEx (Nx s) = T.ESEQ(s,T.CONST 0)

  fun unNx (Nx s) = s
    | unNx (Ex e) = T.EXP(e)
    | unNx (Cx genstm) = T.EXP(unEx(Cx genstm))

  fun unCx (Cx genstm) = genstm 
    | unCx (Ex e) = fn(t,f) => T.CJUMP(T.EQ, e, T.CONST 0, t,f)

  fun simpleVar((lev', MF.InFrame(offset)), lev) = 
      Ex(T.MEM(T.BINOP(T.PLUS, T.TEMP(MF.FP), T.CONST offset)))
    | simpleVar((lev', MF.InReg(temp)), lev) = 
      Ex(T.TEMP(temp))

  fun array_create(size,init) = Ex(MF.externalCall("tig_initArray", [unEx
    size,unEx init]))

  fun subscriptVar(array, index) = 
    let
      val good = Temp.newlabel()
      val maybe = Temp.newlabel()
      val bad = Temp.newlabel()
      val done = Temp.newlabel()
      val size = Temp.newtemp()
      val result = Temp.newtemp()
    in
      Ex(T.ESEQ(seq[T.MOVE(T.TEMP(size), T.MEM(T.BINOP(T.MINUS, unEx array,
      T.CONST MF.wordSize))), T.CJUMP(T.LT, unEx index, T.CONST 0, bad,
      maybe), T.LABEL(maybe), T.CJUMP(T.GT, unEx index, T.TEMP(size),
      bad, good), T.LABEL(bad), T.MOVE(T.TEMP(result), T.CONST
      0), T.JUMP(T.NAME(done), [done]), T.LABEL(good), T.MOVE(T.TEMP(result),  
       T.MEM(T.BINOP(T.PLUS, unEx array, unEx index))), T.JUMP(T.NAME(done),
       [done]), T.LABEL(done)], T.TEMP(result)))
    end

  fun record_creation(initlist) = (* initlist is a list of Trans.exp *)
      let val initlist' = map unEx initlist
          val r = Temp.newtemp()
          fun st(es, idx) = 
              case (es, idx) of
                  ([], _) => []
                | (e::es', i) => T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP(r), T.CONST(i))), e)::st(es', i + 4)
      in
          Ex(T.ESEQ(
            seq(
              T.MOVE(T.TEMP(r), 
                     MF.externalCall("malloc", [T.CONST(MF.wordSize * List.length(initlist))]))::st(initlist', 0)
            ),
            T.TEMP(r)
          ))
      end

  fun field_var(variable, idx) =
      let val record = unEx variable
      in
          Ex(T.MEM(T.BINOP(T.PLUS, record, T.CONST(idx * 4))))
      end


  fun assignExp(variable, value) = Nx(T.MOVE(unEx variable, unEx value))

  fun op_exp (left, right, A.PlusOp) = Ex(T.BINOP(T.PLUS, unEx left, unEx right))
    | op_exp (left, right, A.MinusOp) = Ex(T.BINOP(T.MINUS, unEx left, unEx right))
    | op_exp (left, right, A.TimesOp) = Ex(T.BINOP(T.MUL, unEx left, unEx right))
    | op_exp (left, right, A.DivideOp) = Ex(T.BINOP(T.DIV, unEx left, unEx right))

  fun int_exp (v) = Ex(T.CONST(v))

  fun cond_exp (left, right, A.EqOp) = Cx(fn (t,f) => T.CJUMP(T.EQ, unEx left, unEx right, t, f))
    | cond_exp (left, right, A.NeqOp) = Cx(fn (t,f) => T.CJUMP(T.NE, unEx left, unEx right, t, f))
    | cond_exp (left, right, A.GtOp) = Cx(fn (t,f) => T.CJUMP(T.GT, unEx left, unEx right, t, f))
    | cond_exp (left, right, A.GeOp) = Cx(fn (t,f) => T.CJUMP(T.GE, unEx left, unEx right, t, f))
    | cond_exp (left, right, A.LtOp) = Cx(fn (t,f) => T.CJUMP(T.LT, unEx left, unEx right, t, f))
    | cond_exp (left, right, A.LeOp) = Cx(fn (t,f) => T.CJUMP(T.LE, unEx left, unEx right, t, f))

  fun if_else_exp (cond, e1, e2) = 
    let 
      val cond' = unCx cond
      val e1' = unEx e1
      val e2' = unEx e2
      val r = Temp.newtemp()
      val t = Temp.newlabel()
      val f = Temp.newlabel()
      val tl = Temp.newlabel()
    in
      Ex(T.ESEQ(seq[cond'(t,f), T.LABEL t, T.MOVE(T.TEMP(r), e1'), 
      T.JUMP(T.NAME(tl), [tl]), T.LABEL f,  T.MOVE(T.TEMP(r), e2'), T.LABEL tl], T.TEMP(r)))
    end

  fun initialize_dec((lev,MF.InReg(i)), init) =  Nx(T.MOVE(T.TEMP(i), unEx init))
    | initialize_dec((lev,MF.InFrame(i)), init) = Nx(T.MOVE(T.MEM(T.BINOP(T.PLUS,
      T.TEMP(MF.FP), T.CONST i)),unEx init))

  fun if_exp (cond, e1) =
    let
      val cond' = unCx cond
      val e' = unEx e1
      val t = Temp.newlabel()
      val f = Temp.newlabel()
    in
      Nx(seq[cond'(t,f), T.LABEL t, T.EXP(e'), T.LABEL f])
    end

  fun break_exp (lab) = Nx(T.JUMP(T.NAME(lab), [lab]))

  fun seq_exp(head, tail) = 
    let 
      val head' = unEx head
      val tail' = unEx tail
    in 
      Ex(T.ESEQ(T.EXP(head'), tail'))
    end

  fun unit_exp() = Ex(T.CONST 0)

  (* Get done label for while/for loops, need to pass through transExp *)
  fun get_donelabel () = Temp.newlabel()

  fun declist(head, tail) = Nx(seq[unNx head, unNx tail])

  fun let_exp(decs, body) = Ex(T.ESEQ(unNx decs, unEx body))

  fun while_exp (cond, body, done) = 
    let
      val cond' = unCx cond
      val body' = unNx body
      val test = Temp.newlabel()
      val cont = Temp.newlabel()
    in
      Nx(seq[T.LABEL test, cond'(cont, done), T.LABEL cont, body',
      T.JUMP(T.NAME(test), [test]), T.LABEL done])
    end

  (* TODO *)
  fun procEntryExit ({level: level, body: exp}) = () 

end
