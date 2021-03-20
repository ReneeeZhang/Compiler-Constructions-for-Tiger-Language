signature TRANSLATE = 
sig
  type level
  type access
  (* val outermost : level
  val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
  val formals: level -> access list
  val allocLocal: level -> bool -> access *)
  datatype exp = Ex of Tree.exp
               | Nx of Tree.stm
               | Cx of Tree.label * Tree.label -> Tree.stm
               | Un of unit
  val unEx : exp -> Tree.exp
  val unNx : exp -> Tree.stm
  val unCx : exp -> (Temp.label * Temp.label -> Tree.stm)
  val int_exp : int -> exp 
  val break_exp : Tree.label -> exp
  val op_exp : exp * exp * Absyn.oper -> exp
  val cond_exp : exp * exp * Absyn.oper -> exp
  val while_exp : exp * exp * Temp.label-> exp
  val if_else_exp : exp * exp * exp -> exp
  val unit_exp : unit -> exp
  val get_donelabel : unit -> Temp.label
  val seq_exp : exp * exp -> exp
  val if_exp : exp * exp -> exp
end

