structure Translate : TRANSLATE = 

struct 
  structure T = Tree
  structure A = Absyn

  type level = int (* may not be int *)
  type access = level * MipsFrame.access

  datatype exp = Ex of T.exp
               | Nx of T.stm
               | Cx of T.label * T.label -> T.stm
               | Un of unit
  
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


  fun unCx (Cx genstm) = genstm 

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
      Ex(T.ESEQ(seq[cond'(t,f), T.LABEL t, T.MOVE(T.TEMP(r), e1'), T.LABEL f,
      T.JUMP(T.NAME(tl), [tl]), T.MOVE(T.TEMP(r), e2'), T.LABEL tl], T.TEMP(r)))
    end

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

end
