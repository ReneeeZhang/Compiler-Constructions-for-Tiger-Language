structure Semant :> 
sig 
  val transProg : Absyn.exp -> {exp:Translate.exp, ty:Types.ty} 
end = 
struct

  type ty = Types.ty
  type venv = Env.enventry Symbol.table
  type tenv = ty Symbol.table
  type expty = {exp : Translate.exp, ty : ty}

  structure T = Types
  structure A = Absyn
  structure E = Env
  structure S = Symbol
  structure H = HashTable
  structure Trans = Translate
  structure MF = MipsFrame
  structure SymbolSet = HashSetFn (struct type hash_key = string
                                          fun hashVal s = HashString.hashString s
                                          fun sameKey(s1: hash_key, s2: hash_key) = s1 = s2
                                    end)
  structure SS = SymbolSet
  
  fun checkint({exp,ty}, pos) = 
    (case ty of Types.INT => ()
       | _ => ErrorMsg.error pos ("Invalid arithmetic operand : "^T.tostring(ty)))  

  fun check_eq_args({exp=_, ty=Types.INT}, {exp=_, ty=Types.INT},pos) = {exp=Trans.Un(), ty=Types.INT}
    | check_eq_args({exp=_, ty=Types.STRING}, {exp=_, ty=Types.STRING},pos) =
    {exp=Trans.Un(), ty=Types.INT}
    | check_eq_args({exp=_, ty=Types.RECORD(_)}, {exp=_, ty=Types.NIL},pos) =
    {exp=Trans.Un(), ty=Types.INT}
    | check_eq_args({exp=_, ty=Types.NIL}, {exp=_, ty=Types.RECORD(_)},pos) =
    {exp=Trans.Un(), ty=Types.INT}
    | check_eq_args({exp=_, ty=Types.RECORD(_)}, {exp=_, ty=Types.RECORD(_)},pos) =
    {exp=Trans.Un(), ty=Types.INT}
    | check_eq_args({exp=_, ty=Types.ARRAY(_)}, {exp=_, ty=Types.ARRAY(_)},pos) = 
    {exp=Trans.Un(), ty=Types.INT}
    | check_eq_args(a, b,pos) = (ErrorMsg.error pos 
    ("Invalid comparison operands : "^Types.tostring(#ty a)^" and "^Types.tostring(#ty b)); {exp=Trans.Un(), ty=Types.BOTTOM})

  fun transExp (venv, tenv, exp : Absyn.exp, isLoop : Temp.label option, lev:
    Translate.level) =
    let
      (*Trivial stuff*)
      fun trexp (A.OpExp{left,oper=A.PlusOp,right,pos}) = 
        let
          val lexpty = trexp left
          val rexpty = trexp right
        in 
         (checkint(lexpty, pos); checkint(rexpty, pos);
          {exp=Trans.op_exp(#exp lexpty, #exp rexpty, A.PlusOp), ty=Types.INT})
        end
      | trexp (A.OpExp{left,oper=A.MinusOp,right,pos}) = 
        let
          val lexpty = trexp left
          val rexpty = trexp right
        in 
         (checkint(lexpty, pos); checkint(rexpty, pos);
          {exp=Trans.op_exp(#exp lexpty, #exp rexpty, A.MinusOp), ty=Types.INT})
        end
      | trexp (A.OpExp{left,oper=A.TimesOp,right,pos}) = 
        let
          val lexpty = trexp left
          val rexpty = trexp right
        in 
         (checkint(lexpty, pos); checkint(rexpty, pos);
          {exp=Trans.op_exp(#exp lexpty, #exp rexpty, A.TimesOp), ty=Types.INT})
        end
      | trexp (A.OpExp{left,oper=A.DivideOp,right,pos}) = 
        let
          val lexpty = trexp left
          val rexpty = trexp right
        in 
         (checkint(lexpty, pos); checkint(rexpty, pos);
          {exp=Trans.op_exp(#exp lexpty, #exp rexpty, A.DivideOp), ty=Types.INT})
        end
      | trexp (A.OpExp{left,oper=A.LeOp,right,pos}) =
        let 
          val lexpty = trexp left
          val rexpty = trexp right
          val check = check_eq_args(lexpty, rexpty, pos)
        in
          {exp=Trans.cond_exp(#exp lexpty, #exp rexpty, A.LeOp), ty=(#ty check)}
        end 
      | trexp (A.OpExp{left,oper=A.LtOp,right,pos}) =
        let 
          val lexpty = trexp left
          val rexpty = trexp right
          val check = check_eq_args(lexpty, rexpty, pos)
        in
          {exp=Trans.cond_exp(#exp lexpty, #exp rexpty, A.LtOp), ty=(#ty check)}
        end 
      | trexp (A.OpExp{left,oper=A.GeOp,right,pos}) =
        let 
          val lexpty = trexp left
          val rexpty = trexp right
          val check = check_eq_args(lexpty, rexpty, pos)
        in
          {exp=Trans.cond_exp(#exp lexpty, #exp rexpty, A.GeOp), ty=(#ty check)}
        end 
      | trexp (A.OpExp{left,oper=A.GtOp,right,pos}) =
        let 
          val lexpty = trexp left
          val rexpty = trexp right
          val check = check_eq_args(lexpty, rexpty, pos)
        in
          {exp=Trans.cond_exp(#exp lexpty, #exp rexpty, A.GtOp), ty=(#ty check)}
        end 
      | trexp (A.OpExp{left,oper=A.EqOp,right,pos}) = 
        let 
          val lexpty = trexp left
          val rexpty = trexp right
          val check = check_eq_args(lexpty, rexpty, pos)
        in
          {exp=Trans.cond_exp(#exp lexpty, #exp rexpty, A.EqOp), ty=(#ty check)}
        end 
      | trexp (A.OpExp{left,oper=A.NeqOp,right,pos}) = 
        let 
          val lexpty = trexp left
          val rexpty = trexp right
          val check = check_eq_args(lexpty, rexpty, pos)
        in
          {exp=Trans.cond_exp(#exp lexpty, #exp rexpty, A.NeqOp), ty=(#ty check)}
        end 
      | trexp (A.IntExp(intval)) = {exp=Trans.int_exp(intval), ty=Types.INT}
      | trexp (A.StringExp(stringval, pos)) = {exp=Trans.Un(), ty=Types.STRING}
      | trexp (A.NilExp) = {exp=Trans.Un(), ty = Types.NIL} 
      | trexp (A.VarExp(var)) = trvar var

      (*Nontrivial stuff*)
      (*TODO: still missing Call Record exps, breaks within functions*)
      (*Assign exps*)
      | trexp (A.AssignExp{var, exp, pos}) = 
        let 
          val {exp=exp1,ty=ty1} = trexp(exp)
          val {exp=exp2,ty=ty2} = trvar(var)
        in
         (if Types.is_subtype_of(ty1, ty2,pos) then () else ErrorMsg.error pos
         ("Invalid assign operands : " ^ Types.tostring(ty1) ^ " and " ^
         Types.tostring(ty2)); {exp=Trans.Un(), ty = Types.UNIT})
        end

      (*While exps*)
        | trexp (A.WhileExp{test, body, pos}) = 
          let
            val {exp=exp_test, ty=ty_test} = trexp(test)
            val done_label = Trans.get_donelabel()
            val {exp=exp_body, ty=ty_body} = transExp(venv, tenv, body, SOME(done_label), lev)
          in
            (if Types.is_subtype_of(ty_test, Types.INT,pos) then () else
              ErrorMsg.error pos ("Loop condition is type "
              ^ Types.tostring(ty_test) ^ ", type int required");
              
             if Types.is_subtype_of(ty_body, Types.UNIT,pos) then () else
               ErrorMsg.error pos ("Loop body is type " ^
               Types.tostring(ty_body) ^ ", type unit required");
             {exp=Trans.while_exp(exp_test, exp_body, done_label), ty=Types.UNIT})
          end


      (*For exps*)
        | trexp (A.ForExp{var, escape, lo, hi, body, pos}) = 
          let 
            val venv' = S.enter(venv, var, {access=E.FuncAccess, ty=Types.INT}) (* TODO: access should be VarAccess *)
            val {exp=exp_lo, ty=ty_lo} = trexp(lo)
            val {exp=exp_hi, ty=ty_hi} = trexp(hi)
            val {exp=exp_body, ty=ty_body} = transExp(venv',tenv,body, (*add
            label*)
            NONE, lev)
          in
            (if Types.is_subtype_of(ty_lo, Types.INT,pos) then () else
              ErrorMsg.error pos ("Loop bound is type " ^
              Types.tostring(ty_lo) ^ ", type int required");
             if Types.is_subtype_of(ty_hi, Types.INT,pos) then () else
               ErrorMsg.error pos ("Loop bound is type " ^
               Types.tostring(ty_hi) ^ ", type int required");
             if Types.is_subtype_of(ty_body, Types.UNIT,pos) then () else
               ErrorMsg.error pos ("Loop body is type " ^
               Types.tostring(ty_body) ^ ", type unit required");
             {exp=Trans.Un(), ty=Types.UNIT})
          end

      (*Break Exps*)
        | trexp(A.BreakExp(pos)) = 
          (case isLoop of SOME(lab) => {exp=Trans.break_exp(lab), ty=Types.UNIT}
             | NONE => (ErrorMsg.error pos ("Break must be inside loop");
               {exp=Trans.Un(), ty=Types.UNIT}))

      (*If Exps*)
        | trexp (A.IfExp{test, then', else', pos}) = 
          (case else' of SOME(expression) =>  
            let 
              val {exp=expthen, ty=tythen} = trexp(then')
              val {exp=expelse, ty=tyelse} = trexp(valOf(else'))
              val {exp=exptest, ty=tytest} = trexp(test)
            in
              (if Types.is_subtype_of(tythen, tyelse,pos) then () else
                ErrorMsg.error pos ("Then type of " ^ Types.tostring(tythen) ^
                " does not match else type of " ^ Types.tostring(tyelse));
               if Types.is_subtype_of(tytest, Types.INT,pos) then () else
                 ErrorMsg.error pos ("If-then condition is type " ^
                 Types.tostring(tytest) ^ ", type int required");
               {exp=Trans.if_else_exp(exptest, expthen, expelse), ty = tythen})
            end
          | NONE =>
            let 
              val {exp=expthen, ty=tythen} = trexp(then')
              val {exp=exptest, ty=tytest} = trexp(test)
            in
              (if Types.is_subtype_of(tythen, Types.UNIT,pos) then () else
                ErrorMsg.error pos ("Then type is " ^ Types.tostring(tythen) ^", type unit required");
               if Types.is_subtype_of(tytest, Types.INT,pos) then () else
                 ErrorMsg.error pos ("If-then condition is type " ^
                 Types.tostring(tytest) ^ ", type int required");
               {exp=Trans.if_exp(exptest, expthen), ty=tythen})
            end
            )

      (*Array Exps*)
      | trexp(A.ArrayExp{typ, size, init, pos}) = 
          let
            val {exp=exp_size, ty=ty_size} = trexp(size)
            val {exp=exp_init, ty=ty_init} = trexp(init)
          in
            (if Types.is_subtype_of(ty_size, Types.INT, pos) then () else ErrorMsg.error pos ("Array size must be integer when creating an array");
            
             case S.look(tenv, typ) of 
                  SOME(Types.ARRAY(ele_type, u)) => if Types.is_subtype_of(ty_init, ele_type, pos)
                                                    then {exp=Trans.Un(), ty=Types.ARRAY(ele_type, u)}
                                                    else (ErrorMsg.error pos ("Array init, i.e., element type, does not have the same type as " 
                                                                              ^ S.name(typ) ^ "'s element type when creating an array.");
                                                          {exp=Trans.Un(), ty=Types.BOTTOM})  
                | SOME (_) => (ErrorMsg.error pos (S.name(typ) ^ "has a non-array type."); 
                               {exp=Trans.Un(), ty=Types.BOTTOM})
                | NONE => (ErrorMsg.error pos ("Undefined type for " ^ S.name(typ)); 
                           {exp=Trans.Un(), ty=Types.BOTTOM}))
          end

      (*Let Exps*)
      | trexp (A.LetExp{decs,body,pos}) = 
        let val {venv=venv',tenv=tenv'} = transDecs(venv, tenv, decs, lev)
        in transExp(venv',tenv',body,NONE, lev)
        end
 
      (*Seq Exps*)
      | trexp (A.SeqExp([])) = {exp=Trans.unit_exp(), ty=Types.UNIT}
      | trexp (A.SeqExp([t])) = trexp (#1 t)
      | trexp (A.SeqExp(h::t)) = 
        let 
          val head = trexp (#1 h)
          val tail = trexp(A.SeqExp(t))
        in
          {exp=Trans.seq_exp((#exp head), (#exp tail)), ty=(#ty tail)}
        end

	  (*Call Exps*)
	  | trexp (A.CallExp({func, args, pos})) =
    let fun check_args_and_params_match(argexplist, paramlist) =
      case (argexplist, paramlist) of
      ([], []) => true
      | ([argexp], [param]) => (
        let val {exp=_, ty=ty_field_exp} = trexp(argexp) in
          Types.is_subtype_of(param, ty_field_exp,pos) end
        )
      | (argexp::argexplist', param::paramlist') => (
        let val {exp=_, ty=ty_field_exp} = trexp(argexp) in
          Types.is_subtype_of(param, ty_field_exp, pos) end
      ) andalso check_args_and_params_match(argexplist', paramlist')
	  | _ => (*Impossible state*) false
    in
		(case S.look(venv, func) of
			SOME({access=_,ty=T.ARROW(func_param_ty_list, return_ty)}) => 
				(if List.length func_param_ty_list <> List.length args then (
                          ErrorMsg.error pos ("Function with " ^ Int.toString(List.length func_param_ty_list) ^ " parameters called with " ^ Int.toString(List.length args) ^ " arguments"); 
                          {exp=Trans.Un(), ty=Types.BOTTOM}
                          )
						  else (
							  if check_args_and_params_match(args, func_param_ty_list) then {exp=Trans.Un(), ty=return_ty}
							  else  (ErrorMsg.error pos ("Function argument list types don't match expected parameters"); {exp=Trans.Un(), ty=Types.BOTTOM})
						  )
				)
			| SOME(_) => (ErrorMsg.error pos ("Non-function symbol called: " ^
            S.name(func)); {exp=Trans.Un(), ty=Types.BOTTOM})
            | NONE => (ErrorMsg.error pos ("Undefined function name: " ^
            S.name(func)); {exp=Trans.Un(), ty=Types.BOTTOM})
		)
    end

      (* Record Exp *)
      | trexp (A.RecordExp{fields, typ, pos}) = 
          case S.look(tenv, typ) of
              SOME(Types.RECORD(thunk, u)) => 
              let val given_fields = thunk()
                  fun compare_fields(decl_fields, given_fields) = 
                      if List.length(decl_fields) <> List.length(given_fields) (* 2 field lists with inconsistent length --> error *)
                      then (
                            ErrorMsg.error pos (S.name(typ) ^ " does not have the same number of fields as declared in record exp."); 
                            {exp=Trans.Un(), ty=Types.BOTTOM}
                            )
                      else let fun aux(decl_fields, given_fields) = 
                                  case (decl_fields, given_fields) of
                                      ([], []) => {exp=Trans.Un(), ty=Types.RECORD(thunk, u)}
                                    | ((s1, e, p)::decl_fields', (s2, ty)::given_fields') => 
                                      if s1 = s2
                                      then let val {exp=_, ty=ty_field_exp} = trexp(e)
                                            in
                                              if Types.is_subtype_of(ty,
                                              ty_field_exp,pos)
                                              then aux(decl_fields',
                                              given_fields')
                                              else ( (* Field types are inconsistent *)
                                                  
                                                  ErrorMsg.error p (S.name(typ) ^ "'s field " ^ S.name(s1) ^ "'s type is not consistent with the declared in the record exp."); 
                                                  {exp=Trans.Un(), ty=Types.BOTTOM}
                                              )
                                            
                                            end
                                      else ( (* Field names are inconsistent *)
                                        ErrorMsg.error p (S.name(s1) ^ " should have the same field name as " ^ S.name(s2) ^ " in the record exp."); 
                                        {exp=Trans.Un(), ty=Types.BOTTOM}
                                      )
                                    | _ => {exp=Trans.Un(), ty=Types.BOTTOM} (* This won't happen because the length of 2 argv is promised to be the same *)
                            in
                                aux(decl_fields, given_fields)
                            end
              in
                  compare_fields(fields, given_fields)                       
              end
            | SOME(_) => (
                          ErrorMsg.error pos ("Non-record type"); 
                          {exp=Trans.Un(), ty=Types.BOTTOM}
                          )
            | NONE => (
                        ErrorMsg.error pos ("Undefined record type");
                        {exp=Trans.Un(), ty=Types.BOTTOM}
                      )

      (*Simple vars*)
      and trvar (A.SimpleVar(id, pos)) = 
       (case S.look(venv,id) of SOME({access=_, ty=ty}) =>
            {exp=Trans.Un(), ty = ty}
        | NONE => (ErrorMsg.error pos ("Undefined Variable: " ^ Symbol.name(id));
                   {exp=Trans.Un(), ty=Types.INT}))

            (* (case S.look(venv,id) of  *)
            (* SOME({access=E.VarAccess(ac), ty=ty}) => {exp=Trans.simple_var(ac, lev), ty=ty}
          | NONE => (ErrorMsg.error pos ("Undefined Variable: " ^ Symbol.name(id));
                   {exp=Trans.Un(), ty=Types.INT})) *)
          (* TODO: pattern match a function type *)

      (* Array vars *)
      | trvar (A.SubscriptVar(var, expression, pos)) = 
        (case trvar(var) of {exp=_, ty=Types.ARRAY(ty, un)} =>
          let 
            val {exp=var_exp, ty=var_ty} = trvar var
            val {exp=exp_exp, ty=exp_ty} = trexp expression
          in
            (if Types.is_subtype_of(exp_ty, Types.INT,pos) then () else ErrorMsg.error pos ("Array index must be int"); 
             {exp=Trans.Un(), ty=ty})
          end
        | {exp=_, ty=_} => (ErrorMsg.error pos ("Attempting to index non-array"); {exp=Trans.Un(), ty=Types.UNIT}))

      (* Field vars *)
      | trvar (A.FieldVar(var, name, pos)) = 
        let 
          fun findfield([]) = (ErrorMsg.error pos ("Undefined record field");
          Types.BOTTOM)
            | findfield((fname,ty)::fieldlist) = if fname=name then ty else findfield(fieldlist)
          val {exp, ty} = trvar var
        in
          (case ty of Types.RECORD(recfun,un) =>
            let
              val fieldlist = recfun()
            in
              {exp=Trans.Un(), ty=findfield(fieldlist)}
            end
             | _ => (ErrorMsg.error pos ("Not a record type: "^S.name(name)); {exp=Trans.Un(),
               ty=Types.BOTTOM}))         
        end
    in
      trexp(exp)
    end

  (*Dec list, venv, tenv -> venv',tenv'*)
  and transDecs (venv, tenv, [], lev) = {venv=venv, tenv=tenv}
    | transDecs (venv, tenv, h::t, lev) = 
        let val {venv=venv', tenv=tenv'} = transDec(venv, tenv, h, lev)
        in transDecs (venv', tenv', t, lev)
        end

  (*Singleton dec, venv, tenv -> venv', tenv'*)
  (*Var decs*)
  and transDec(venv,tenv,A.VarDec{escape,init=A.NilExp,name,pos,typ=NONE}, lev) = 
    (ErrorMsg.error pos ("Illegal nil use: record needed"); {tenv=tenv,
     venv=venv})
  | transDec (venv,tenv,A.VarDec{escape,init,name,pos,typ=NONE}, lev) = 
    let val {exp,ty} = transExp(venv, tenv, init, NONE, lev)
    in {tenv=tenv,
    venv=S.enter(venv,name,{access=E.VarAccess(Trans.allocLocal(lev) (!escape)), ty=ty})}
    end
  | transDec (venv, tenv, A.VarDec{escape,init,name,pos,typ=SOME(typ)}, lev) =
    let 
      val {exp,ty} = transExp(venv, tenv, init, NONE, lev)
      val type_lookup = case S.look(tenv, (#1 typ)) of SOME(label_ty) =>
                   if Types.is_subtype_of(label_ty,ty,pos) then label_ty else
                     (ErrorMsg.error pos ("Mismatched type in declaration");
                     Types.BOTTOM)
                    | NONE => (ErrorMsg.error pos ("Undefined type");
                      Types.BOTTOM)
    in
      {tenv=tenv,
       venv=S.enter(venv,name,{access=E.VarAccess(Trans.allocLocal(lev) (!escape)), ty=type_lookup})}
    end
        
  (*Type Decs*)
  | transDec (venv, tenv, A.TypeDec(tydec_group), lev) = 
    let exception UNIQUE_RECORDS
        val unique_ref_map : (string, Types.unique) H.hash_table =
            H.mkTable(HashString.hashString, op =) (128, UNIQUE_RECORDS) (* A type name(string) to unique ref map associated with each type dec group *)
        
        fun have_redeclarations () =
            let val redec_detector = SS.mkEmpty 64
                fun aux tydecs = 
                    case tydecs of 
                        [] => false 
                      | {name, ty, pos} :: tydecs' =>
                        let val namestr = S.name(name)
                        in
                            if SS.member(redec_detector, namestr)
                            then (
                              ErrorMsg.error pos ("Type name: " ^ namestr ^ " is an illegal redeclaration.");
                              true
                            )
                            else (
                              SS.add(redec_detector, namestr);
                              aux tydecs'
                            )
                        end
            in 
                aux tydec_group
            end 

        fun add_types (tenv, types) =
            case types of
              [] => tenv
            | {name, ty, pos} :: types' => let val tenv' = S.enter(tenv, name, 
                                                             transTy(tenv, name, unique_ref_map, tydec_group, ty))
                                           in 
                                                add_types(tenv', types')
                                           end
    in
        if have_redeclarations()
        then {venv=venv, tenv=tenv}
        else {venv=venv, tenv=add_types(tenv, tydec_group)}
    end

  (*Single-function funcdec with 0 or more params*)
  | transDec (venv,tenv,
      A.FunctionDec([{name, params, body, pos,
        result=SOME(rt,respos)}]), lev) =
      let val result_ty = case S.look(tenv,rt) of
          SOME(rty) => rty
          | NONE => (ErrorMsg.error pos ("Undefined return type in function definition"); T.UNIT)
        fun transparam(absf:Absyn.field) = 
            case S.look(tenv, #typ absf) of
              SOME t => {name = #name absf, ty=t}
              | NONE => (ErrorMsg.error pos ("Undefined type for parameter in function definition"); {name = #name absf,ty=T.UNIT})
        fun get_types([]) = []
          | get_types((h : Absyn.field)::(t : Absyn.field list)) = 
            let 
              val a = case S.look(tenv, #typ h) of 
                           SOME t => t
                         | NONE => Types.BOTTOM
            in
              a::get_types(t)
            end
        val params' = map transparam params
        val typelist = get_types(params)
        fun enterparam({name,ty},venv) = S.enter(venv, name, {access=E.FuncAccess,ty=ty}) (* TODO: it should be a VarAccess *)
        val venv' = foldl enterparam venv params' (*Pretty sure this was a typo in the book*)
        val venv'' = S.enter(venv', name, {access=E.FuncAccess, ty=Types.ARROW(typelist,
        result_ty)})
		val {exp=_,ty=bodytype} = transExp(venv'', tenv, body, NONE, lev)
		val venv''' = S.enter(venv, name, {access=E.FuncAccess, ty=Types.ARROW(typelist,
        result_ty)})
      in if Types.is_subtype_of(bodytype, result_ty,pos) then () else
        ErrorMsg.error pos ("Function body type does not match specified return type");
        {venv=venv''',tenv=tenv}
      end
  
  (*Single-procedure funcdec with 0 or more params*)
  | transDec (venv,tenv,
      A.FunctionDec([{name, params, body, pos,
        result=NONE}]), lev) =
      let
        fun transparam(absf:Absyn.field) = 
            case S.look(tenv, #typ absf) of
              SOME t => {name = #name absf, ty=t}
              | NONE => (ErrorMsg.error pos ("Undefined type for parameter in function definition at position " ^ Int.toString(pos)) ; {name = #name absf,ty=T.UNIT})
		fun get_types([]) = []
          | get_types((h : Absyn.field)::(t : Absyn.field list)) = 
            let 
              val a = case S.look(tenv, #typ h) of 
                           SOME t => t
                         | NONE => Types.BOTTOM
            in
              a::get_types(t)
            end
        val params' = map transparam params
		val typelist = get_types(params)
        fun enterparam({name,ty},venv) = S.enter(venv, name,
        {access=E.FuncAccess,ty=ty})
        val venv' = foldl enterparam venv params' (*Pretty sure this was a typo in the book*)
		val venv'' = S.enter(venv', name, {access=E.FuncAccess, ty=Types.ARROW(typelist, Types.UNIT)}) (* access might cause problem *)
		val venv''' = S.enter(venv, name, {access=E.FuncAccess, ty=Types.ARROW(typelist, Types.UNIT)}) (* access might cause problem *)
		val {exp=_,ty=bodytype} = transExp(venv'', tenv, body, NONE, lev)
      in if Types.is_subtype_of(bodytype, Types.UNIT,pos) then () else
        ErrorMsg.error pos ("Procedure body type must be UNIT, not " ^
        Types.tostring(bodytype)); {venv=venv''',tenv=tenv}
      end
  
  (*funcdec with multiple functions/procedures*)
  | transDec (venv,tenv, A.FunctionDec(otherfds), lev) =
	(checkThatNoTwoMutuallyRecursiveFunctionsHaveTheSameName(otherfds);
    processFunDecList(collectHeadersFromFunDecList(venv, tenv, A.FunctionDec(otherfds)),tenv,A.FunctionDec(otherfds), lev) 
	)
  and checkThatNoTwoMutuallyRecursiveFunctionsHaveTheSameName(fds:A.fundec list) =
	let val _ = foldl (fn (fd: A.fundec, current_list_of_names) => (
		if List.exists (fn x => x = #name fd) current_list_of_names
		then (ErrorMsg.error (#pos fd) ("Function " ^ S.name(#name fd) ^ " appears multiple times in mutually recursive segment"); current_list_of_names)
		else (#name fd)::current_list_of_names
	)) [] fds
	in () end
	
  and processFunDecList(venv,tenv, A.FunctionDec([]), lev) = {venv=venv,tenv=tenv}
  | processFunDecList(venv,tenv, A.FunctionDec([fd]), lev) = transDec(venv,tenv,A.FunctionDec([fd]), lev)
  | processFunDecList(venv,tenv, A.FunctionDec(fd::otherfds), lev) =
    (transDec(venv,tenv,A.FunctionDec([fd]), lev); processFunDecList(venv,tenv,A.FunctionDec(otherfds), lev))
  | processFunDecList(venv,tenv, _, _) = ((*Impossible state*){venv=venv,tenv=tenv})

  
  and collectHeadersFromFunDecList(venv, tenv, A.FunctionDec([])) = venv
  | collectHeadersFromFunDecList(venv, tenv, A.FunctionDec(fundeclist)) =
    foldl (fn(fundec, venv) => S.enter(venv, #name fundec, getFunDecHeader(fundec, tenv))) venv fundeclist
  | collectHeadersFromFunDecList(venv, tenv, _) = ((*Impossible state*)venv)
  
  and getFunDecHeader({name, params, body, pos,
        result=NONE}, tenv) =
      let
        fun transparam(absf:Absyn.field) = 
            case S.look(tenv, #typ absf) of
              SOME t => {name = #name absf, ty=t}
              | NONE => (ErrorMsg.error pos ("Undefined type for parameter in function definition at position " ^ Int.toString(pos)); {name = #name absf,ty=T.UNIT})
	    	fun get_types([]) = []
          | get_types((h : Absyn.field)::(t : Absyn.field list)) = 
            let 
              val a = case S.look(tenv, #typ h) of 
                           SOME t => t
                         | NONE => Types.BOTTOM
            in
              a::get_types(t)
            end
        val params' = map transparam params
		    val typelist = get_types(params)
      in {access=E.FuncAccess, ty=T.ARROW(map #ty params', T.UNIT)} (* access might cause problem *)
      end
    | getFunDecHeader({name, params, body, pos,
        result=SOME(rt,pos')}, tenv) =
        let val result_ty = case S.look(tenv,rt) of
            SOME(rty) => rty
            | NONE => (ErrorMsg.error pos ("Undefined return type in function definition"); T.UNIT)
          fun transparam(absf:Absyn.field) = 
              case S.look(tenv, #typ absf) of
                SOME t => {name = #name absf, ty=t}
                | NONE => (ErrorMsg.error pos ("Undefined type for parameter in function definition"); {name = #name absf,ty=T.UNIT})
          fun get_types([]) = []
            | get_types((h : Absyn.field)::(t : Absyn.field list)) = 
              let 
                val a = case S.look(tenv, #typ h) of 
                            SOME t => t
                          | NONE => Types.BOTTOM
              in
                a::get_types(t)
            end
          val params' = map transparam params
          val typelist = get_types(params)
        in {access=E.FuncAccess, ty=T.ARROW(map #ty params', result_ty)} (* access might cause problem *)
        end

  and transTy (tenv, type_sym, unique_ref_map, tydec_group, absyn_ty) = 
        (* function find_in_tydec_group looks for the type_sym in tydec_group (Absyn.TypeDec, a list), if it exists,
        return SOME; Otherwise, NONE *)
    let val cycle_detector = SS.mkEmpty 64 
        fun lookup_in_tydec_group type_sym = (* S.symbol -> Types.ty option *)
            let fun aux tydecs =
                case tydecs of
                    [] => NONE
                  | {name, ty, pos} :: tydecs' => if name = type_sym
                                                  then SOME(ty)
                                                  else aux tydecs'
            in
                aux tydec_group
            end

        fun lookup_in_tenv(type_sym) = (* S.symbol -> Types.ty *)
            case S.look(tenv, type_sym) of
                SOME(ty) => ty
              | NONE => (ErrorMsg.error 0 ("Undefined type: " ^ S.name(type_sym)); Types.BOTTOM) (* TODO: pos is undefined *)
        
        (* Given a type name (type_sym), return its unique ref if this name is already in the unique_ref_map;
           Otherwise, return a new unique ref *)
        fun get_uref(type_sym) = 
            let val namestr = S.name(type_sym)
                val rec_entry = H.find unique_ref_map namestr
            in
                case rec_entry of
                    SOME(uref) => uref
                  | NONE => let val new_indicator = ref ()
                            in
                              (
                                  H.insert unique_ref_map (namestr, new_indicator);
                                  new_indicator
                              )
                            end
            end

        fun check_fields_decl(type_sym, fields) = (* A.field list -> unit *)
            let fun aux fs = (* A.field list -> unit *)
                    case fs of
                        [] => ()
                      | {name, escape, typ, pos}::fs' => 
                        if has_been_declared typ
                        then aux fs'
                        else ErrorMsg.error pos ("In record type " ^ S.name(type_sym) ^ ", its field " ^ S.name(name) ^ "'s type, " 
                                                  ^ S.name(typ) ^ " is not declared.")

                and has_been_declared(type_sym) = (* A.symbol -> bool *)
                      case lookup_in_tydec_group type_sym of
                          NONE => (case S.look(tenv, type_sym) of
                                      SOME(_) => true
                                    | NONE => false)
                        | SOME(_) => true
            in
                aux fields
            end

        (* function proc basically find out type name in ty_group in case of (mutual) recersion by calling address;
           if name does occur in the ty_group, then look up in tenv *)
        fun proc(type_sym) = (* S.symbol -> Types.ty *) 
            case lookup_in_tydec_group type_sym of
                NONE => lookup_in_tenv(type_sym) (* If not in the tydec_group, search in tenv *)
              | SOME(ty) => address(type_sym, ty) (* If in the tydec_group, then recursively call proc on each ty *)

        (* function address is mutually recusive to proc, handling cases where type name is found in type dec group 
           argv: ty is the corresponding type to type_sym.
           type_sym is the lhs name of an expression *)
        and address(type_sym, ty) = 
            case ty of 
                A.NameTy(sym, pos) => if SS.member(cycle_detector, S.name(type_sym))
                                      then (ErrorMsg.error pos ("Name type: " ^ S.name(type_sym) ^ ", is in an illegal cycle of type declaration.");
                                            Types.BOTTOM)
                                      else (SS.add(cycle_detector, S.name(type_sym));
                                            proc(sym)) 
              | A.ArrayTy(sym, pos) =>  if SS.member(cycle_detector, S.name(type_sym))
                                        then (ErrorMsg.error pos ("Array type: " ^ S.name(type_sym) ^ ", is in an illegal cycle of type declaration.");
                                              Types.BOTTOM)
                                        else (SS.add(cycle_detector, S.name(type_sym));
                                              Types.ARRAY(proc(sym), get_uref(type_sym)))
              | A.RecordTy(fields) => let val thunk = fn() => ( 
                                                                SS.subtractList(cycle_detector, SS.toList(cycle_detector)); (* Clear cycle detector *)
                                                                map (fn {name, typ, ...} => (name, proc(typ))) fields
                                                              )
                                      in
                                          (
                                            check_fields_decl(type_sym, fields);
                                            Types.RECORD(thunk, get_uref(type_sym))
                                          )
                                      end
    in
        address(type_sym, absyn_ty)
    end

  fun transProg (tree : Absyn.exp) = 
    transExp(E.base_venv, E.base_tenv, tree, NONE, Trans.outermost)
    
end
