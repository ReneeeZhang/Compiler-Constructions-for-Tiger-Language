structure Semant :> 
sig 
  val transProg : Absyn.exp -> unit 
  (* val gettype : Types.ty -> Types.ty *)
  
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

  fun checkint({exp,ty}, pos) = 
    case ty of Types.INT => ()
       | _ => ErrorMsg.error pos ("Integer Required")  

  (*Fun for unwrapping Types.NAME*)
  (* fun gettype(Types.NAME(_, ref(SOME(ty)))) = ty
    | gettype(Types.NAME(_,ref(NONE))) = Types.NIL
    | gettype(Types.INT) = Types.INT
    | gettype(Types.STRING) = Types.STRING
    | gettype(Types.UNIT) = Types.UNIT
    | gettype(Types.NIL) = Types.NIL
    | gettype(Types.RECORD(ls, un)) = Types.RECORD(ls, un)
    | gettype(Types.ARRAY(ty, un)) = Types.ARRAY(ty, un) *)


  fun transExp (venv, tenv, exp : Absyn.exp) =
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
      (*TODO: still missing Call Record Break exps*)
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
            val {exp=exp_body, ty=ty_body} = trexp(body)
          in
            (if Types.is_subtype_of(ty_test, Types.INT) then () else ErrorMsg.error pos ("Loop condition must be int");
             if Types.is_subtype_of(ty_body, Types.UNIT) then () else ErrorMsg.error pos ("Loop body must be type unit");
             {exp=(), ty=Types.UNIT})
          end

          (* | trexp (A.WhileExp{test, body, pos}) = 
          let
            val {exp=exp_test, ty=ty_test} = trexp(test)
            val {exp=exp_body, ty=ty_body} = trexp(body)
          in
            if Types.is_subtype_of(ty_test, Types.INT) 
            then if Types.is_subtype_of(ty_body, Types.UNIT) 
                 then {exp=(), ty=Types.UNIT})
                 else (ErrorMsg.error pos ("Loop body must be type unit"); {exp=(), ty=Types.BOTTOM})
            else (ErrorMsg.error pos ("Loop condition must be int"); {exp=(), ty=Types.BOTTOM})
          end *)

      (*For exps*)
        | trexp (A.ForExp{var, escape, lo, hi, body, pos}) = 
          let 
            val venv' = S.enter(venv, var, {access=(), ty=Types.INT})
            val {exp=exp_lo, ty=ty_lo} = trexp(lo)
            val {exp=exp_hi, ty=ty_hi} = trexp(hi)
            val {exp=exp_body, ty=ty_body} = transExp(venv',tenv,body)
          in
            (if Types.is_subtype_of(ty_lo, Types.INT) then () else ErrorMsg.error pos ("Loop bounds must be int");
             if Types.is_subtype_of(ty_hi, Types.INT) then () else ErrorMsg.error pos ("Loop bounds must be int");
             if Types.is_subtype_of(ty_body, Types.UNIT) then () else ErrorMsg.error pos ("Loop body must be type unit");
             {exp=(), ty=Types.UNIT})
          end

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
            (if Types.is_subtype_of(ty_size, Types.INT) then () else ErrorMsg.error pos ("Array size must be integer");
             if Types.is_subtype_of(ty_init, Types.INT) then () else ErrorMsg.error pos ("Array init must be integer");
             case S.look(tenv, typ) of SOME(Types.ARRAY(fields)) => {exp=(), ty=Types.ARRAY(fields)}
                | SOME (_) => (ErrorMsg.error pos ("Non-array type"); {exp=(),
                  ty=Types.BOTTOM})
                | NONE => (ErrorMsg.error pos ("Undefined array type"); {exp=(),
                  ty=Types.BOTTOM}))
          end

      (*Let Exps*)
      | trexp (A.LetExp{decs,body,pos}) = 
        let val {venv=venv',tenv=tenv'} = transDecs(venv,tenv,decs)
        in transExp(venv',tenv',body)
        end
 
      (*Seq Exps*)
      | trexp (A.SeqExp([])) = {exp=(), ty=Types.UNIT}
      | trexp (A.SeqExp([t])) = trexp (#1 t)
      | trexp (A.SeqExp(h::t)) = (trexp (#1 h); trexp(A.SeqExp(t)))

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
        case trvar(var) of {exp=_, ty=Types.ARRAY(ty, un)} =>
          let 
            val {exp=var_exp, ty=var_ty} = trvar var
            val {exp=exp_exp, ty=exp_ty} = trexp expression
          in
            (if Types.is_subtype_of(exp_ty, Types.INT) then () else ErrorMsg.error pos ("Array index must be int"); 
             {exp=(), ty=ty})
          end
        | {exp=_, ty=_} => (ErrorMsg.error pos ("Attempting to index non-array"); {exp=(), ty=Types.UNIT})

        (* TODO: write field vars *)

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
  (*TODO: Fun decs*)  
  (*Var decs*)
  and transDec (venv,tenv,A.VarDec{escape,init,name,pos,typ=NONE}) = 
    let val {exp,ty} = transExp(venv,tenv,init)
    in {tenv=tenv, venv=S.enter(venv,name,{access=(), ty=ty})}
    end

  (*Type Decs*)
  | transDec (venv,tenv,A.TypeDec(tydec_group)) = 
    let exception UNIQUE_RECORDS
        val unique_records_map : (string, Types.unique) H.hash_table =
            H.mkTable(HashString.hashString, op =) (128, UNIQUE_RECORDS) (* A type name(string) to unique ref map associated with each type dec group *)
        fun add_types (tenv, types) =
            case types of
              [] => tenv
            | {name, ty, pos} :: types' => let val tenv' = S.enter(tenv, name, 
                                                             transTy(tenv, name, unique_records_map, tydec_group, ty))
                                           in 
                                                add_types(tenv', types')
                                           end
    in
        {venv=venv, tenv=add_types(tenv, tydec_group)}
    end

  and transTy (tenv, type_sym, unique_records_map, tydec_group, absyn_ty) = 
        (* function find_in_tydec_group looks for the type_sym in tydec_group (Absyn.TypeDec, a list), if it exists,
           return SOME; Otherwise, NONE *)
    let fun lookup_in_tydec_group type_sym = (* S.symbol -> Types.ty option *)
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
        
        (* function proc basically find out type name in ty_group in case of (mutual) recersion by calling address;
           if name does occur in the ty_group, then look up in tenv *)
        fun proc(type_sym, unique_records_map) = (* S.symbol -> Types.ty *) 
            case lookup_in_tydec_group type_sym of
                NONE => lookup_in_tenv(type_sym) (* If not in the tydec_group, search in tenv *)
              | SOME(ty) => address(type_sym, ty) (* If in the tydec_group, then recursively call proc on each ty *)

        (* function address is mutually recusive to proc, handling cases where type name is found in type dec group *)
        and address(type_sym, ty) = 
            case ty of 
                A.NameTy(sym, _) => proc(sym, unique_records_map)
              | A.ArrayTy(sym, _) => Types.ARRAY(proc(sym, unique_records_map), ref()) (* TODO: not always ref ()*)
              | A.RecordTy(fields) => 
                let val namestr = S.name(type_sym)
                    val rec_entry = H.find unique_records_map namestr
                    val indicator = case rec_entry of
                                      SOME(uref) => uref
                                    | NONE => let val new_indicator = ref ()
                                              in
                                                (
                                                    H.insert unique_records_map (namestr, new_indicator);
                                                    new_indicator
                                                )
                                              end
                in
                    Types.RECORD((fn() => map 
                                          (fn {name, typ, ...} => (name, proc(typ, unique_records_map)))
                                          fields), 
                                  indicator)
                end 
    in
        address(type_sym, absyn_ty)
        (* case absyn_ty of 
                A.NameTy(sym, _) => proc(sym, unique_records_map)
              | A.ArrayTy(sym, _) => Types.ARRAY(proc(sym, unique_records_map), ref ()) (* TODO: not always ref ()*)
              | A.RecordTy(fields) => let val indicator = ref ()
                                      in
                                          (
                                            H.insert unique_records_map (S.name(type_sym), indicator);
                                            Types.RECORD((fn() => map 
                                                        (fn {name, typ, ...} => (name, proc(typ, unique_records_map)))
                                                        fields), indicator)
                                          )
                                      end                                     *)
    end

  fun transProg (tree : Absyn.exp) = 
    (transExp(E.base_venv, E.base_tenv, tree);()) 
    
end
