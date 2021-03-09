structure Semant :> 
sig 
  val transProg : Absyn.exp -> unit   
end = 
struct

  type ty = Types.ty
  type venv = Env.enventry Symbol.table
  type tenv = ty Symbol.table
  type expty = {exp : Translate.exp, ty : ty}

  structure A = Absyn
  structure E = Env
  structure S = Symbol
  structure H = HashTable

  structure SymbolSet = HashSetFn (struct type hash_key = string
                                          fun hashVal s = HashString.hashString s
                                          fun sameKey(s1, s2) = s1 = s2
                                    end)
  structure SS = SymbolSet
  
  fun checkint({exp,ty}, pos) = 
    case ty of Types.INT => ()
       | _ => ErrorMsg.error pos ("Integer Required")  


  fun transExp (venv, tenv, exp : Absyn.exp, isLoop : unit option) =
    let
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
      (*TODO: still missing Call Record exps, breaks within functions*)
      (*Assign exps*)
      | trexp (A.AssignExp{var, exp, pos}) = 
        let 
          val {exp=exp1,ty=ty1} = trexp(exp)
          val {exp=exp2,ty=ty2} = trvar(var)
        in
         (if Types.is_subtype_of(ty1, ty2) then () else ErrorMsg.error pos ("Assign types not equal"); {exp=(), ty = Types.UNIT})
        end

      (*While exps*)
        | trexp (A.WhileExp{test, body, pos}) = 
          let
            val {exp=exp_test, ty=ty_test} = trexp(test)
            val {exp=exp_body, ty=ty_body} = transExp(venv, tenv, body, SOME(()))
          in
            (if Types.is_subtype_of(ty_test, Types.INT) then () else ErrorMsg.error pos ("Loop condition must be int");
             if Types.is_subtype_of(ty_body, Types.UNIT) then () else ErrorMsg.error pos ("Loop body must be type unit");
             {exp=(), ty=Types.UNIT})
          end

      (*For exps*)
        | trexp (A.ForExp{var, escape, lo, hi, body, pos}) = 
          let 
            val venv' = S.enter(venv, var, {access=(), ty=Types.INT})
            val {exp=exp_lo, ty=ty_lo} = trexp(lo)
            val {exp=exp_hi, ty=ty_hi} = trexp(hi)
            val {exp=exp_body, ty=ty_body} = transExp(venv',tenv,body, SOME(()))
          in
            (if Types.is_subtype_of(ty_lo, Types.INT) then () else ErrorMsg.error pos ("Loop bounds must be int");
             if Types.is_subtype_of(ty_hi, Types.INT) then () else ErrorMsg.error pos ("Loop bounds must be int");
             if Types.is_subtype_of(ty_body, Types.UNIT) then () else ErrorMsg.error pos ("Loop body must be type unit");
             {exp=(), ty=Types.UNIT})
          end

      (*Break Exps*)
        | trexp(A.BreakExp(pos)) = 
          (case isLoop of SOME(()) => {exp=(), ty=Types.UNIT}
             | NONE => (ErrorMsg.error pos ("Break must be inside loop");
               {exp=(), ty=Types.UNIT}))

      (*If Exps*)
        | trexp (A.IfExp{test, then', else', pos}) = 
          (case else' of SOME(expression) =>  
            let 
              val {exp=expthen, ty=tythen} = trexp(then')
              val {exp=expelse, ty=tyelse} = trexp(valOf(else'))
              val {exp=exptest, ty=tytest} = trexp(test)
            in
              (if Types.is_subtype_of(tythen, tyelse) then () else ErrorMsg.error pos ("Types mismatched");
               if Types.is_subtype_of(tytest, Types.INT) then () else ErrorMsg.error pos ("Test requires type INT");
               {exp=(), ty = tythen})
            end
          | NONE =>
            let 
              val {exp=expthen, ty=tythen} = trexp(then')
              val {exp=exptest, ty=tytest} = trexp(test)
            in
              (if Types.is_subtype_of(tythen, Types.UNIT) then () else ErrorMsg.error pos ("If-then must return unit");
               if Types.is_subtype_of(tytest, Types.INT) then () else ErrorMsg.error pos ("Test requires type INT");
               {exp=(), ty=tythen})
            end
            )

      (*Array Exps*)
      | trexp(A.ArrayExp{typ, size, init, pos}) = 
          let
            val {exp=exp_size, ty=ty_size} = trexp(size)
            val {exp=exp_init, ty=ty_init} = trexp(init)
          in
            (if Types.is_subtype_of(ty_size, Types.INT) then () else ErrorMsg.error pos ("Array size must be integer when creating an array");
            
             case S.look(tenv, typ) of 
                  SOME(Types.ARRAY(ele_type, u)) => if Types.is_subtype_of(ty_init, ele_type)
                                                    then {exp=(), ty=Types.ARRAY(ele_type, u)}
                                                    else (ErrorMsg.error pos ("Array init, i.e., element type, does not have the same type as " 
                                                                              ^ S.name(typ) ^ "'s element type when creating an array.");
                                                          {exp={}, ty=Types.BOTTOM})  
                | SOME (_) => (ErrorMsg.error pos (S.name(typ) ^ "has a non-array type."); 
                               {exp=(), ty=Types.BOTTOM})
                | NONE => (ErrorMsg.error pos ("Undefined type for " ^ S.name(typ)); 
                           {exp=(), ty=Types.BOTTOM}))
          end

      (*Let Exps*)
      | trexp (A.LetExp{decs,body,pos}) = 
        let val {venv=venv',tenv=tenv'} = transDecs(venv,tenv,decs)
        in transExp(venv',tenv',body,NONE)
        end
 
      (*Seq Exps*)
      | trexp (A.SeqExp([])) = {exp=(), ty=Types.UNIT}
      | trexp (A.SeqExp([t])) = trexp (#1 t)
      | trexp (A.SeqExp(h::t)) = (trexp (#1 h); trexp(A.SeqExp(t)))

	  (*Call Exps*)
	  | trexp (A.CallExp({func, args, pos})) =
    let fun check_args_and_params_match(argexplist, paramlist) =
      case (argexplist, paramlist) of
      ([], []) => true
      | ([argexp], [param]) => (
        let val {exp=_, ty=ty_field_exp} = trexp(argexp) in Types.are_the_same_type(param, ty_field_exp) end
        )
      | (argexp::argexplist', param::paramlist') => (
        let val {exp=_, ty=ty_field_exp} = trexp(argexp) in Types.are_the_same_type(param, ty_field_exp) end
      ) andalso check_args_and_params_match(argexplist', paramlist')
    in
		(case S.look(venv, func) of
			SOME({access=_,ty=T.ARROW(func_param_ty_list, return_ty)}) => 
				(if List.length func_param_ty_list <> List.length args then (
                          ErrorMsg.error pos ("Function with " ^ Int.toString(List.length func_param_ty_list) ^ " parameters called with " ^ Int.toString(List.length args) ^ " arguments"); 
                          {exp=(), ty=Types.BOTTOM}
                          )
						  else (
							  if check_args_and_params_match(args, func_param_ty_list) then {exp=(), ty=return_ty}
							  else  (ErrorMsg.error pos ("Function argument list types don't match expected parameters"); {exp=(), ty=Types.BOTTOM})
						  )
				)
			| SOME(_) => (ErrorMsg.error pos ("Non-function symbol called"); {exp=(), ty=Types.BOTTOM})
            | NONE => (ErrorMsg.error pos ("Undefined function name"); {exp=(), ty=Types.BOTTOM})
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
                            {exp=(), ty=Types.BOTTOM}
                            )
                      else let fun aux(decl_fields, given_fields) = 
                                  case (decl_fields, given_fields) of
                                      ([], []) => {exp=(), ty=Types.RECORD(thunk, u)}
                                    | ((s1, e, p)::decl_fields', (s2, ty)::given_fields') => 
                                      if s1 = s2
                                      then let val {exp=_, ty=ty_field_exp} = trexp(e)
                                            in
                                              if Types.is_subtype_of(ty, ty_field_exp)
                                              then aux(decl_fields', given_fields')
                                              else ( (* Field types are inconsistent *)
                                                  
                                                  ErrorMsg.error p (S.name(typ) ^ "'s field " ^ S.name(s1) ^ "'s type is not consistent with the declared in the record exp."); 
                                                  {exp=(), ty=Types.BOTTOM}
                                              )
                                            
                                            end
                                      else ( (* Field names are inconsistent *)
                                        ErrorMsg.error p (S.name(s1) ^ " should have the same field name as " ^ S.name(s2) ^ " in the record exp."); 
                                        {exp=(), ty=Types.BOTTOM}
                                      )
                                    | _ => {exp=(), ty=Types.BOTTOM} (* This won't happen because the length of 2 argv is promised to be the same *)
                            in
                                aux(decl_fields, given_fields)
                            end
              in
                  compare_fields(fields, given_fields)                       
              end
            | SOME(_) => (
                          ErrorMsg.error pos ("Non-record type"); 
                          {exp=(), ty=Types.BOTTOM}
                          )
            | NONE => (
                        ErrorMsg.error pos ("Undefined record type");
                        {exp=(), ty=Types.BOTTOM}
                      )

      (*Simple vars*)
      and trvar (A.SimpleVar(id, pos)) = 
       (case S.look(venv,id) of SOME({access=(), ty=ty}) =>
            {exp=(), ty = ty}
        | NONE => (ErrorMsg.error pos ("Undefined Variable ");
                   {exp=(), ty=Types.INT}))

      (* Array vars *)
      | trvar (A.SubscriptVar(var, expression, pos)) = 
        (case trvar(var) of {exp=_, ty=Types.ARRAY(ty, un)} =>
          let 
            val {exp=var_exp, ty=var_ty} = trvar var
            val {exp=exp_exp, ty=exp_ty} = trexp expression
          in
            (if Types.is_subtype_of(exp_ty, Types.INT) then () else ErrorMsg.error pos ("Array index must be int"); 
             {exp=(), ty=ty})
          end
        | {exp=_, ty=_} => (ErrorMsg.error pos ("Attempting to index non-array"); {exp=(), ty=Types.UNIT}))

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
              {exp=(), ty=findfield(fieldlist)}
            end
             | _ => (ErrorMsg.error pos ("Not a record type"); {exp=(),
               ty=Types.BOTTOM}))         
        end
    in
      trexp(exp)
    end

  (*Dec list, venv, tenv -> venv',tenv'*)
  and transDecs (venv, tenv, []) = {venv=venv, tenv=tenv}
    | transDecs (venv, tenv, h::t) = 
        let val {venv=venv', tenv=tenv'} = transDec(venv, tenv, h)
        in transDecs (venv', tenv', t)
        end

  (*Singleton dec, venv, tenv -> venv', tenv'*)
  (*Var decs*)
  and transDec (venv,tenv,A.VarDec{escape,init,name,pos,typ=NONE}) = 
    let val {exp,ty} = transExp(venv,tenv,init,NONE)
    in {tenv=tenv, venv=S.enter(venv,name,{access=(), ty=ty})}
    end
  | transDec (venv, tenv, A.VarDec{escape,init,name,pos,typ=SOME(typ)}) =
    let 
      val {exp,ty} = transExp(venv,tenv,init,NONE)
      val test = case S.look(tenv, (#1 typ)) of SOME(label_ty) =>
                   if Types.are_the_same_type(label_ty,ty) then () else ErrorMsg.error pos ("Mismatched type")
                    | NONE => ErrorMsg.error pos ("Undefined type")
    in
      {tenv=tenv, venv=S.enter(venv,name,{access=(), ty=ty})}
    end
        
  (*Type Decs*)
  | transDec (venv,tenv,A.TypeDec(tydec_group)) = 
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
                              ErrorMsg.error pos (namestr ^ " is a redeclaration.");
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
        result=SOME(rt,respos)}])) =
      let val result_ty = case S.look(tenv,rt) of
          SOME(rty) => rty
          | NONE => (ErrorMsg.error pos ("Undefined return type in function definition"); T.UNIT)
        fun transparam(absf:Absyn.field) = 
            case S.look(tenv, #typ absf) of
              SOME t => {name = #name absf, ty=t}
              | NONE => (ErrorMsg.error pos ("Undefined type for parameter in function definition"); {name = #name absf,ty=T.UNIT})
        val params' = map transparam params
        fun enterparam({name,ty},venv) = S.enter(venv, name, {access=(),ty=ty})
        val venv' = foldl enterparam venv params' (*Pretty sure this was a typo in the book*)
        val {exp=_,ty=bodytype} = transExp(venv, tenv, body, NONE)
      in if Types.are_the_same_type(bodytype, result_ty) then () else ErrorMsg.error pos ("Function body type does not match specified return type"); {venv=venv',tenv=tenv}
      end
  
  (*Single-procedure funcdec with 0 or more params*)
  | transDec (venv,tenv,
      A.FunctionDec([{name, params, body, pos,
        result=NONE}])) =
      let
        fun transparam(absf:Absyn.field) = 
            case S.look(tenv, #typ absf) of
              SOME t => {name = #name absf, ty=t}
              | NONE => (ErrorMsg.error pos ("Undefined type for parameter in function definition at position " ^ Int.toString(pos)) ; {name = #name absf,ty=T.UNIT})
        val params' = map transparam params
        fun enterparam({name,ty},venv) = S.enter(venv, name, {access=(),ty=ty})
        val venv' = foldl enterparam venv params' (*Pretty sure this was a typo in the book*)
        val {exp=_,ty=bodytype} = transExp(venv', tenv, body, NONE)
      in if Types.are_the_same_type(bodytype, Types.UNIT) then () else
        ErrorMsg.error pos ("Procedure body type must be UNIT " ^
        Types.tostring(bodytype)); {venv=venv,tenv=tenv}
      end
  
  (*funcdec with multiple functions/procedures*)
  | transDec (venv,tenv, A.FunctionDec(otherfds)) =
    processFunDecList(collectHeadersFromFunDecList(venv, tenv, A.FunctionDec(otherfds)),tenv,A.FunctionDec(otherfds)) 
  
  and processFunDecList(venv,tenv, A.FunctionDec([])) = {venv=venv,tenv=tenv}
  | processFunDecList(venv,tenv, A.FunctionDec([fd])) = transDec(venv,tenv,A.FunctionDec([fd]))
  | processFunDecList(venv,tenv, A.FunctionDec(fd::otherfds)) =
    (transDec(venv,tenv,A.FunctionDec([fd])); processFunDecList(venv,tenv,A.FunctionDec(otherfds)))

  
  and collectHeadersFromFunDecList(venv, tenv, A.FunctionDec([])) = venv
  | collectHeadersFromFunDecList(venv, tenv, A.FunctionDec(fundeclist)) =
    foldl (fn(fundec, venv) => S.enter(venv, #name fundec, getFunDecHeader(fundec, tenv))) venv fundeclist
  
  and getFunDecHeader({name, params, body, pos,
        result=NONE}, tenv) =
      let
        fun transparam(absf:Absyn.field) = 
            case S.look(tenv, #typ absf) of
              SOME t => {name = #name absf, ty=t}
              | NONE => (ErrorMsg.error pos ("Undefined type for parameter in function definition at position " ^ Int.toString(pos)); {name = #name absf,ty=T.UNIT})
        val params' = map transparam params
      in {access=(), ty=T.ARROW(map #ty params', T.UNIT)}
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
          val params' = map transparam params
        in {access=(), ty=T.ARROW(map #ty params', result_ty)}
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
              | NONE => (ErrorMsg.error 0 ("Undefinied type: " ^ S.name(type_sym)); Types.BOTTOM) (* TODO: pos is undefined *)
        
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

        (* function proc basically find out type name in ty_group in case of (mutual) recersion by calling address;
           if name does occur in the ty_group, then look up in tenv *)
        fun proc(type_sym) = (* S.symbol -> Types.ty *) 
            case lookup_in_tydec_group type_sym of
                NONE => lookup_in_tenv(type_sym) (* If not in the tydec_group, search in tenv *)
              | SOME(ty) => address(type_sym, ty) (* If in the tydec_group, then recursively call proc on each ty *)

        (* function address is mutually recusive to proc, handling cases where type name is found in type dec group 
           argv: ty is the corresponding type to type_sym *)
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
              | A.RecordTy(fields) => Types.RECORD((fn() => map 
                                                            (fn {name, typ, ...} => (name, proc(typ)))
                                                            fields), 
                                                    get_uref(type_sym))
    in
        address(type_sym, absyn_ty)
    end

  fun transProg (tree : Absyn.exp) = 
    (transExp(E.base_venv, E.base_tenv, tree,NONE);())
    
end
