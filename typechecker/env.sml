structure S = Symbol
structure T = Types

structure Env :> ENV = 
struct 
  type ty = T.ty
  type access = unit
  type enventry = {access: access, ty: ty} 

  val base_tenv =  S.enter(S.enter(S.empty,
                   S.symbol("int"), T.INT), 
                   S.symbol("string"), T.STRING)
    
  (* datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result: ty} *)
			  
  val base_venv =
    let val venv1 = S.enter(S.empty, S.symbol("print"), {access=(), ty=T.ARROW([T.STRING], T.NIL)})
        val venv2 = S.enter(venv1, S.symbol("flush"), {access=(), ty=T.ARROW([], T.NIL)})
        val venv3 = S.enter(venv2, S.symbol("getchar"), {access=(), ty=T.ARROW([], T.STRING)})
        val venv4 = S.enter(venv3, S.symbol("ord"), {access=(), ty=T.ARROW([T.STRING], T.INT)})
        val venv5 = S.enter(venv4, S.symbol("chr"), {access=(), ty=T.ARROW([T.INT], T.STRING)})
        val venv6 = S.enter(venv5, S.symbol("size"), {access=(), ty=T.ARROW([T.STRING], T.INT)})
        val venv7 = S.enter(venv6, S.symbol("substring"), {access=(), ty=T.ARROW([T.STRING, T.INT, T.INT], T.STRING)})
        val venv8 = S.enter(venv7, S.symbol("concat"), {access=(), ty=T.ARROW([T.STRING, T.STRING], T.STRING)})
        val venv9 = S.enter(venv8, S.symbol("not"), {access=(), ty=T.ARROW([T.INT], T.INT)})
        val venv_final = S.enter(venv9, S.symbol("exit"), {access=(), ty=T.ARROW([T.INT], T.NIL)})
    in
        venv_final
    end

end
