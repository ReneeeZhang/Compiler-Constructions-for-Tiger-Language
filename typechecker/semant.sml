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
  structure S = Symbol

  fun checkint({exp,ty}, pos) = 
    case ty of Types.INT => ()
       | _ => ErrorMsg.error pos ("Integer Required")  

  (*Fun for unwrapping Types.NAME*)
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
      (*TODO: still missing Call Record If While For Break Array exps*)
      (*Trivial stuff*)
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
      | trexp (A.VarExp(var)) = trvar var

      (*Nontrivial stuff*)
      (*Assign exps*)
      | trexp (A.AssignExp{var, exp, pos}) = 
        let 
          val {exp=exp1,ty=ty1} = trexp(exp)
          val {exp=exp2,ty=ty2} = trvar(var)
        in
         (if ty1=ty2 then () else ErrorMsg.error pos ("Assign types not equal"); {exp=(), ty = Types.NIL})
        end

        (*Let Exps*)
      | trexp (A.LetExp{decs,body,pos}) = 
        let val {venv=venv',tenv=tenv'} = transDecs(venv,tenv,decs)
        in transExp(venv',tenv',body)
        end
 
        (*Seq Exps*)
      | trexp (A.SeqExp([])) = {exp=(), ty=Types.NIL}
      | trexp (A.SeqExp([t])) = trexp (#1 t)
      | trexp (A.SeqExp(h::t)) = (trexp (#1 h); trexp(A.SeqExp(t)))

      (*TODO: Field and subscript vars*)
      and trvar (A.SimpleVar(id, pos)) = 
       (case S.look(venv,id) of SOME(E.VarEntry{ty}) =>
            {exp=(), ty = gettype ty}
        | NONE => (ErrorMsg.error pos ("Undefined Variable ");
                   {exp=(), ty=Types.INT}))
    in
      trexp(exp)
    end

  (*Dec list, venv, tenv -> venv',tenv'*)
  and transDecs (venv, tenv, []) = {venv=venv, tenv=tenv}
    | transDecs (venv, tenv, h::t) = 
        let val {venv=venv', tenv=tenv'} = transDecs(venv, tenv, t)
        in transDec (venv', tenv', h)
        end

  (*Singleton dec, venv, tenv -> venv', tenv'*)
  (*TODO: Type and fun decs*)  
  and transDec (venv,tenv,A.VarDec{escape,init,name,pos,typ=NONE}) = 
    let val {exp,ty} = transExp(venv,tenv,init)
    in {tenv=tenv, venv=S.enter(venv,name,E.VarEntry{ty=ty})}
    end

  (*TODO: write transTy (see pg118) *)

  fun transProg (tree : Absyn.exp) = 
    (transExp(E.base_venv, E.base_tenv, tree);())
    
end
