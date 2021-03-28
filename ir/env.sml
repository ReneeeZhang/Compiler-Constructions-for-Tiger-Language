structure Env :> ENV = 
struct
  structure S = Symbol
  structure T = Types
  structure Trans = Translate
  structure Temp = Temp

  type ty = T.ty
  datatype accessty = VarAccess of Translate.access
                    | FuncAccess
  type enventry = {access: accessty, ty: ty}

  val base_tenv =  S.enter(S.enter(S.empty,
                   S.symbol("int"), T.INT), 
                   S.symbol("string"), T.STRING)
			  
  (* TODO: modif access and ARROW *)            
  val base_venv =
    let val venv1 = S.enter(S.empty, S.symbol("print"), {access=FuncAccess, ty=T.ARROW([T.STRING], T.UNIT, Trans.external, Temp.namedlabel "tig_print", Trans.external, 0)})
        val venv2 = S.enter(venv1, S.symbol("flush"), {access=FuncAccess, ty=T.ARROW([], T.UNIT, Trans.external, Temp.namedlabel "tig_flush", Trans.external, 0)})
        val venv3 = S.enter(venv2, S.symbol("getchar"), {access=FuncAccess, ty=T.ARROW([], T.STRING, Trans.external, Temp.namedlabel "tig_getchar", Trans.external, 0)})
        val venv4 = S.enter(venv3, S.symbol("ord"), {access=FuncAccess, ty=T.ARROW([T.STRING], T.INT, Trans.external, Temp.namedlabel "tig_ord", Trans.external, 0)})
        val venv5 = S.enter(venv4, S.symbol("chr"), {access=FuncAccess, ty=T.ARROW([T.INT], T.STRING, Trans.external, Temp.namedlabel "tig_chr", Trans.external, 0)})
        val venv6 = S.enter(venv5, S.symbol("size"), {access=FuncAccess, ty=T.ARROW([T.STRING], T.INT, Trans.external, Temp.namedlabel "tig_size", Trans.external, 0)})
        val venv7 = S.enter(venv6, S.symbol("substring"), {access=FuncAccess, ty=T.ARROW([T.STRING, T.INT, T.INT], T.STRING, Trans.external, Temp.namedlabel "tig_substring", Trans.external, 0)})
        val venv8 = S.enter(venv7, S.symbol("concat"), {access=FuncAccess, ty=T.ARROW([T.STRING, T.STRING], T.STRING, Trans.external, Temp.namedlabel "tig_concat", Trans.external, 0)})
        val venv9 = S.enter(venv8, S.symbol("not"), {access=FuncAccess, ty=T.ARROW([T.INT], T.INT, Trans.external, Temp.namedlabel "tig_not", Trans.external, 0)})
        val venv_final = S.enter(venv9, S.symbol("exit"), {access=FuncAccess, ty=T.ARROW([T.INT], T.UNIT, Trans.external, Temp.namedlabel "tig_exit", Trans.external, 0)})
    in
        venv_final
    end

end
