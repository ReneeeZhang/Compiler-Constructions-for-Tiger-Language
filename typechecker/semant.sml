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
         (if Types.are_the_same_type(ty1, ty2) then () else ErrorMsg.error pos ("Assign types not equal"); {exp=(), ty = Types.UNIT})
        end

      (*While exps*)
        | trexp (A.WhileExp{test, body, pos}) = 
          let
            val {exp=exp_test, ty=ty_test} = trexp(test)
            val {exp=exp_body, ty=ty_body} = trexp(body)
          in
            (if Types.are_the_same_type(ty_test, Types.INT) then () else ErrorMsg.error pos ("Loop condition must be int");
             if Types.are_the_same_type(ty_body, Types.UNIT) then () else ErrorMsg.error pos ("Loop body must be type unit");
             {exp=(), ty=Types.UNIT})
          end

      (*For exps*)
        | trexp (A.ForExp{var, escape, lo, hi, body, pos}) = 
          let 
            val venv' = S.enter(venv, var, {access=(), ty=Types.INT})
            val {exp=exp_lo, ty=ty_lo} = trexp(lo)
            val {exp=exp_hi, ty=ty_hi} = trexp(hi)
            val {exp=exp_body, ty=ty_body} = transExp(venv',tenv,body)
          in
            (if Types.are_the_same_type(ty_lo, Types.INT) then () else ErrorMsg.error pos ("Loop bounds must be int");
             if Types.are_the_same_type(ty_hi, Types.INT) then () else ErrorMsg.error pos ("Loop bounds must be int");
             if Types.are_the_same_type(ty_body, Types.UNIT) then () else ErrorMsg.error pos ("Loop body must be type unit");
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
              (if Types.are_the_same_type(tythen, tyelse) then () else ErrorMsg.error pos ("Types mismatched");
               if Types.are_the_same_type(tytest, Types.INT) then () else ErrorMsg.error pos ("Test requires type INT");
               {exp=(), ty = tythen})
            end
          | NONE =>
            let 
              val {exp=expthen, ty=tythen} = trexp(then')
              val {exp=exptest, ty=tytest} = trexp(test)
            in
              (if Types.are_the_same_type(tythen, Types.UNIT) then () else ErrorMsg.error pos ("If-then must return unit");
               if Types.are_the_same_type(tytest, Types.INT) then () else ErrorMsg.error pos ("Test requires type INT");
               {exp=(), ty=tythen})
            end
            )

      (*Array Exps*)
      | trexp(A.ArrayExp{typ, size, init, pos}) = 
          let
            val {exp=exp_size, ty=ty_size} = trexp(size)
            val {exp=exp_init, ty=ty_init} = trexp(init)
          in
            (if Types.are_the_same_type(ty_size, Types.INT) then () else ErrorMsg.error pos ("Array size must be integer");
             if Types.are_the_same_type(ty_init, Types.INT) then () else ErrorMsg.error pos ("Array init must be integer");
             case S.look(tenv, typ) of SOME(Types.ARRAY(fields)) => {exp=(), ty=Types.ARRAY(fields)}
                | SOME (_) => (ErrorMsg.error pos ("Non-array type"); {exp=(),
                  ty=Types.UNIT})
                | NONE => (ErrorMsg.error pos ("Undefined array type"); {exp=(),
                  ty=Types.UNIT}))
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
            (if Types.are_the_same_type(exp_ty, Types.INT) then () else ErrorMsg.error pos ("Array index must be int"); 
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
    let fun add_types (tenv, types) =
            case types of
              [] => tenv
            | {name, ty, pos} :: types' => let val tenv' = S.enter(tenv, name, transTy(tenv, name, tydec_group, ty))
                                           in 
                                              add_types(tenv', types')
                                           end
    in
        {venv=venv, tenv=add_types(tenv, tydec_group)}
    end

  and transTy (tenv, type_sym, tydec_group, absyn_ty) = 
        (* function find_in_tydec_group looks for the type_sym in tydec_group (Absyn.TypeDec, a list), if it exists,
           return SOME; Otherwise, NONE *)
    let exception UNIQUE_RECORDS
        val unique_records_map : (string, Types.unique) H.hash_table =
            H.mkTable(HashString.hashString, op =) (128, UNIQUE_RECORDS)
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
        
        (* function proc basically find out name in ty_group in case of (mutual) recersion; if name does
           occur in the ty_group, then look up in tenv *)
        fun proc(type_sym, unique_records_map) = (* S.symbol -> Types.ty *) 
            case lookup_in_tydec_group type_sym of
                NONE => lookup_in_tenv(type_sym) (* If not in the tydec_group, search in tenv *)
              | SOME(ty) => case ty of (* If in the tydec_group, then recursively call proc on each ty *)
                                A.NameTy(sym, _) => proc(sym, unique_records_map)
                              | A.ArrayTy(sym, _) => Types.ARRAY(proc(sym, unique_records_map), ref()) (* TODO: not always ref ()*)
                              | A.RecordTy(fields) => 
                                let val name = S.name(type_sym)
                                    val rec_entry = H.find unique_records_map name
                                in
                                    Types.RECORD((fn() => map 
                                                          (fn {name, typ, ...} => (name, proc(typ, unique_records_map)))
                                                          fields), 
                                                  case rec_entry of
                                                      SOME(indicator) => indicator
                                                    | NONE => let val new_indicator = ref ()
                                                              in
                                                                (
                                                                    H.insert unique_records_map (name, new_indicator);
                                                                    new_indicator
                                                                )
                                                              end
                                                )
                                end 
    in
        case absyn_ty of 
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
                                      end                                    
    end

  (*Absyn.ty -> Types.ty*)
  (* and transTy (tenv, A.NameTy(absyn_ty)) = 
    case S.look(tenv,(#1 absyn_ty)) of SOME(ty) => ty
     | NONE => (ErrorMsg.error (#2 absyn_ty) ("Undefined type"); Types.UNIT)
  | transTy (tenv, A.ArrayTy(absyn_ty)) = 
    case S.look(tenv, (#1 absyn_ty)) of SOME(ty) => Types.ARRAY(ty, ref())
       | NONE => (ErrorMsg.error (#2 absyn_ty) ("Undefined type"); Types.UNIT)
  | transTy (tenv, A.RecordTy(fields)) = (* fields: field list, in terms of Absyn *)
    let fun iter_fields fields = (* ans: (symbol*ty) list, in terms of Types.RECORD *)
            case fields of
              [] => []
            | f :: fields' => case S.look(tenv, (#typ f)) of
                                SOME(ty) => ((#name f), ty) :: iter_fields fields'
                              | NONE => (ErrorMsg.error (#pos f) ("Undefined type in a record field"); 
                                         [])
        val fields_in_types = iter_fields fields
    in
        Types.RECORD((fn () => fields_in_types), ref())
    end 
    *)

  fun transProg (tree : Absyn.exp) = 
    (transExp(E.base_venv, E.base_tenv, tree);()) 
    
end
