structure Semant :> 
sig 
  val transProg : Absyn.exp -> unit 
  val gettype : Types.ty -> Types.ty
end = 
struct

  type ty = Types.ty
  type venv = Env.enventry Symbol.table
  type tenv = ty Symbol.table
  type expty = {exp : Translate.exp, ty : ty}

  structure A = Absyn
  structure E = Env

  fun checkint({exp,ty}, pos) = 
    case ty of Types.INT => ()
       | _ => ErrorMsg.error pos ("Integer Required")  

  fun gettype(Types.NAME(_, ref(SOME(ty)))) = ty
    | gettype(Types.NAME(_,ref(NONE))) = Types.NIL
    | gettype(Types.INT) = Types.INT
    | gettype(Types.STRING) = Types.STRING
    | gettype(Types.UNIT) = Types.UNIT
    | gettype(Types.NIL) = Types.NIL
    | gettype(Types.RECORD(ls, un)) = Types.RECORD(ls, un)
    | gettype(Types.ARRAY(ty, un)) = Types.ARRAY(ty, un)


  fun transExp (venv, tenv, exp : Absyn.exp) =
    let
      fun trexp (A.OpExp{left,oper=A.PlusOp,right,pos}) = 
                 (checkint(trexp left, pos); checkint(trexp right, pos);
                  {exp=(), ty=Types.INT})
      | trexp (A.OpExp{left,oper=A.MinusOp,right,pos}) = 
                 (checkint(trexp left, pos); checkint(trexp right, pos);
                  {exp=(), ty=Types.INT})
      | trexp (A.OpExp{left,oper=A.TimesOp,right,pos}) = 
                 (checkint(trexp left, pos); checkint(trexp right, pos);
                  {exp=(), ty=Types.INT})
      | trexp (A.OpExp{left,oper=A.DivideOp,right,pos}) = 
                 (checkint(trexp left, pos); checkint(trexp right, pos);
                  {exp=(), ty=Types.INT})
      | trexp (A.OpExp{left,oper=A.LeOp,right,pos}) = 
                 (checkint(trexp left, pos); checkint(trexp right, pos);
                  {exp=(), ty=Types.INT})
      | trexp (A.OpExp{left,oper=A.LtOp,right,pos}) = 
                 (checkint(trexp left, pos); checkint(trexp right, pos);
                  {exp=(), ty=Types.INT})
      | trexp (A.OpExp{left,oper=A.GeOp,right,pos}) = 
                 (checkint(trexp left, pos); checkint(trexp right, pos);
                  {exp=(), ty=Types.INT})
      | trexp (A.OpExp{left,oper=A.GtOp,right,pos}) = 
                 (checkint(trexp left, pos); checkint(trexp right, pos);
                  {exp=(), ty=Types.INT})
      | trexp (A.OpExp{left,oper=A.EqOp,right,pos}) = 
                 (checkint(trexp left, pos); checkint(trexp right, pos);
                  {exp=(), ty=Types.INT})
      | trexp (A.OpExp{left,oper=A.NeqOp,right,pos}) = 
                 (checkint(trexp left, pos); checkint(trexp right, pos);
                  {exp=(), ty=Types.INT})
      | trexp (A.IntExp(intval)) = {exp=(), ty=Types.INT}
      | trexp (A.StringExp(stringval, pos)) = {exp=(), ty=Types.STRING}
      | trexp (A.NilExp) = {exp=(), ty = Types.NIL} 
      | trexp (A.AssignExp{var, exp, pos}) = 
                 (trexp(exp); trvar(var); {exp=(), ty = Types.NIL})

      and trvar (A.SimpleVar(id, pos)) = 
       (case Symbol.look(venv,id) of SOME(E.VarEntry{ty}) =>
            {exp=(), ty = gettype ty}
        | NONE => (ErrorMsg.error pos ("Undefined Variable ");
                   {exp=(), ty=Types.INT}))
    in
      trexp(exp)
    end

  fun transProg (tree : Absyn.exp) = 
    (transExp(E.base_venv, E.base_tenv, tree);())
    
end
