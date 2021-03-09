structure FindEscape: sig val findEscape: Absyn.exp -> unit end = 

struct
  structure A = Absyn
  structure S = Symbol
  type depth = int
  type escEnv = (depth * bool ref) Symbol.table

  fun traverseVar (env:escEnv, d:depth, s:Absyn.var) : unit = ()

  and traverseExp (env:escEnv, d:depth, s:Absyn.exp) : unit = 
    let 
      fun travexp (A.OpExp{left,oper,right,pos}) = (travexp(left);
        travexp(right); ())
      | travexp (A.IntExp(intval)) = ()
      | travexp (A.StringExp(str)) = ()
      | travexp (A.NilExp) = ()
      
      (*For*)
      | travexp (A.ForExp{var,escape,lo,hi,body,pos}) = 
        let 
          val env' = S.enter (env,var,(d, escape))
        in
          (escape := false;
           travexp(lo);
           travexp(hi);
           traverseExp(env', d, body))
        end

      (*While*)
      | travexp (A.WhileExp{test,body,pos}) = (travexp(test); travexp(body); ())
     
      (*Break*)
      | travexp (A.BreakExp(pos)) = ()

      (*Let*)
      | travexp (A.LetExp{decs,body,pos}) =
        let 
          val env' = traverseDecs (env, d, decs)
        in
          (traverseExp(env',d,body))
        end

      (*Call*)
      | travexp (A.CallExp{func, args, pos}) = 
        let 
          fun try_args([]) = ()
            | try_args(h::t) = 
            (travexp(h); try_args(t))
        in
          try_args(args)
        end

      (*Arrays*)
      | travexp (A.ArrayExp{typ, size, init, pos}) = (travexp(size);
        travexp(init))

      (*Records*)
      | travexp (A.RecordExp{fields, typ, pos}) = 
        let 
          fun rec_list ([]) = ()
            | rec_list ((name,exp,pos)::t) = (travexp(exp); rec_list(t))
        in
          rec_list(fields)
        end


      (*Expseq*)
      | travexp (A.SeqExp([])) = ()
      | travexp (A.SeqExp((exp, pos)::t)) = (travexp(exp); travexp(A.SeqExp(t)))
      
      (*Vars*)
      | travexp (A.VarExp(var)) = travvar var

      (*If*)
      | travexp (A.IfExp({test,then',else',pos})) =
      (travexp(test); travexp(then');
       if Option.isSome(else') then travexp(Option.valOf(else')) else ())
      
      (*Assign*)
      | travexp (A.AssignExp{var,exp,pos}) = (travvar(var); travexp(exp))

      (*Vars*)
      and travvar (A.SimpleVar(name,pos)) = 
        (case S.look (env, name) of SOME(depth, escape) =>
          (if depth < d then (escape := true) else (); 
           if depth = d then (escape := false) else (); 
           if depth > d then ErrorMsg.error pos ("Something has gone horribly wrong") 
           else ())
         | NONE => ErrorMsg.error pos ("Undefined variable, escape analysis fails"))
       | travvar (A.SubscriptVar(var,exp,pos)) = (travvar(var); travexp(exp))
       | travvar (A.FieldVar(var,symbol,pos)) = (travvar(var))
    in
      travexp(s)
    end

  and traverseDecs (env, d, []) : escEnv = env
        | traverseDecs (env, d, h::t) = 
          let val env' = traverseDec(env, d, h)
          in traverseDecs (env', d, t)
          end
    

  and traverseDec (env, d, A.VarDec{escape,init,name,pos,typ}) : escEnv = 
    (traverseExp(env, d, init); escape := false; S.enter(env,name,(d, escape)))
      | traverseDec (env, d, A.FunctionDec({name, params, result, body, pos}::t)) = 
        let 
          fun add_params(env, []) = env
            | add_params(env, {name, escape, typ, pos}::t) = 
              let val env' = S.enter(env,name,(d+1,escape))
              in (escape:=false; add_params(env',t))
              end
          val env' = add_params(env, params)
        in
        (traverseExp(env', d+1, body); 
         traverseDec (env, d, A.FunctionDec(t)))
        end
      | traverseDec (env, d, A.FunctionDec([])) = env 
      | traverseDec (env, d, A.TypeDec(ty)) = env

  fun findEscape (prog:Absyn.exp) : unit =  traverseExp(S.empty, 0, prog)
end
