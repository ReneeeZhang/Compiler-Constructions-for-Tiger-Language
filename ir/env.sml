structure Env :> ENV = 
struct
  structure S = Symbol
  structure T = Types
  structure Trans = Translate

  type ty = T.ty
  datatype accessty = VarAccess of Translate.access
                    | FuncAccess
  type enventry = {access: accessty, ty: ty} (* Modify to Translate.access *)

  val base_tenv =  S.enter(S.enter(S.empty,
                   S.symbol("int"), T.INT), 
                   S.symbol("string"), T.STRING)
			  
  (* TODO: modif access and ARROW *)            
  val base_venv =
    let val venv1 = S.enter(S.empty, S.symbol("print"), {access=FuncAccess, ty=T.ARROW([T.STRING], T.UNIT)})
        val venv2 = S.enter(venv1, S.symbol("flush"), {access=FuncAccess, ty=T.ARROW([], T.UNIT)})
        val venv3 = S.enter(venv2, S.symbol("getchar"), {access=FuncAccess, ty=T.ARROW([], T.STRING)})
        val venv4 = S.enter(venv3, S.symbol("ord"), {access=FuncAccess, ty=T.ARROW([T.STRING], T.INT)})
        val venv5 = S.enter(venv4, S.symbol("chr"), {access=FuncAccess, ty=T.ARROW([T.INT], T.STRING)})
        val venv6 = S.enter(venv5, S.symbol("size"), {access=FuncAccess, ty=T.ARROW([T.STRING], T.INT)})
        val venv7 = S.enter(venv6, S.symbol("substring"), {access=FuncAccess, ty=T.ARROW([T.STRING, T.INT, T.INT], T.STRING)})
        val venv8 = S.enter(venv7, S.symbol("concat"), {access=FuncAccess, ty=T.ARROW([T.STRING, T.STRING], T.STRING)})
        val venv9 = S.enter(venv8, S.symbol("not"), {access=FuncAccess, ty=T.ARROW([T.INT], T.INT)})
        val venv_final = S.enter(venv9, S.symbol("exit"), {access=FuncAccess, ty=T.ARROW([T.INT], T.UNIT)})
    in
        venv_final
    end

end
