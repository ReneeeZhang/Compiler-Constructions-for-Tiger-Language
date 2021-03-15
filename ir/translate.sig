signature TRANSLATE = 
sig

  datatype exp = Ex of Tree.exp
               | Nx of Tree.stm
               | Cx of Tree.label * Tree.label -> Tree.stm
               | Un of unit
  val unEx : exp -> Tree.exp
  val unNx : exp -> Tree.stm
  val unCx : exp -> (Temp.label * Temp.label -> Tree.stm)
  val int_exp : int -> exp 
  val op_exp : exp * exp * Absyn.oper -> exp
  val cond_exp : exp * exp * Absyn.oper -> exp
  val if_else_exp : exp * exp * exp -> exp

end

