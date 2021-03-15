structure Translate : TRANSLATE = 

struct 
  structure T = Tree
  structure A = Absyn
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

end
